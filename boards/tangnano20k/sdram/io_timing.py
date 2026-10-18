#!/usr/bin/env python3
"""Audit a sourced SDRAM I/O model and optionally emit bounded SDC.

An unknown bound is never replaced by zero or by the opposite bound. A
complete model supplies constraints; it is not routed I/O or protocol signoff.
"""
import argparse
import hashlib
import json
import math
from pathlib import Path
import re

HERE = Path(__file__).resolve().parent
DEFAULT_PROFILE = HERE / 'io_timing_gw2ar18_qn88.json'
DEVICE = 'GW2AR-LV18QN88C8/I7'
PARAMETERS = (
    't_ac_max', 't_oh_min', 't_cmd_setup', 't_cmd_hold',
    't_data_setup', 't_data_hold', 't_dqm_setup', 't_dqm_hold',
    'clock_flight_min', 'clock_flight_max',
    'read_flight_min', 'read_flight_max',
    'command_flight_min', 'command_flight_max',
    'write_flight_min', 'write_flight_max',
    'dqm_flight_min', 'dqm_flight_max',
    'setup_uncertainty', 'hold_uncertainty',
)


def number(value):
    return (isinstance(value, (int, float)) and not isinstance(value, bool)
            and math.isfinite(value) and value >= 0)


def check_static_ports(controller):
    for port, bit in (('SDRAM_CKE', 1), ('SDRAM_nCS', 0)):
        if not re.search(r'(?m)^\s*assign\s+' + port + r"\s*=\s*1'b" + str(bit) + r'\s*;', controller):
            raise ValueError(f'{port} is no longer the documented constant; update I/O coverage')


def audit(profile, freq_mhz, phase_degrees, burst_words=4):
    if not number(freq_mhz) or freq_mhz == 0:
        raise ValueError('memory frequency must be finite and positive')
    if not number(phase_degrees) or phase_degrees >= 360:
        raise ValueError('phase must be finite and in [0, 360) degrees')
    if not isinstance(profile, dict) or profile.get('schema_version') != 1 or profile.get('device') != DEVICE:
        raise ValueError(f'profile must use schema 1 for {DEVICE}')
    if profile.get('reference_plane') != 'fpga_sdram_io_boundary':
        raise ValueError('profile must specify the FPGA SDRAM I/O boundary')
    parameters = profile.get('parameters_ns', {})
    if not isinstance(parameters, dict) or set(parameters) != set(PARAMETERS):
        raise ValueError('parameter names differ from the required I/O model')
    missing, values = [], {}
    for key in PARAMETERS:
        item = parameters[key]
        if not isinstance(item, dict):
            raise ValueError(f'{key}: expected ns/status/source object')
        value = item.get('ns')
        if value is not None and not number(value):
            raise ValueError(f'{key}: ns must be finite, nonnegative, or null')
        source = item.get('source')
        if value is None or item.get('status') != 'verified' or not isinstance(source, str) or not source.strip():
            missing.append(key)
        else:
            values[key] = value
    for group in ('clock', 'read', 'command', 'write', 'dqm'):
        low, high = group + '_flight_min', group + '_flight_max'
        if low in values and high in values and values[low] > values[high]:
            raise ValueError(f'{group} flight minimum exceeds maximum')
    if 't_ac_max' in values and values['t_ac_max'] == 0:
        raise ValueError('t_ac_max must be positive')
    if {'t_ac_max', 't_oh_min'} <= values.keys() and values['t_oh_min'] > values['t_ac_max']:
        raise ValueError('read hold minimum exceeds read access maximum')
    qualification = profile.get('qualification', {})
    if not isinstance(qualification, dict):
        raise ValueError('qualification must be an object')
    for key in ('source', 'operating_conditions', 'boundary_accounting'):
        if not isinstance(qualification.get(key), str) or not qualification[key].strip():
            missing.append('qualification.' + key)
    if qualification.get('status') != 'verified':
        missing.append('qualification.status')
    if (type(qualification.get('cas_latency')) is not int or qualification['cas_latency'] != 2
            or type(qualification.get('burst_words')) is not int or qualification['burst_words'] != burst_words):
        missing.append(f'qualification.cas2_burst{burst_words}')
    low, high = qualification.get('min_mhz'), qualification.get('max_mhz')
    if not number(low) or not number(high) or high == 0 or low > high:
        missing.append('qualification.frequency_range')
    elif not low <= freq_mhz <= high:
        raise ValueError(f'{freq_mhz:g} MHz is outside qualified [{low:g}, {high:g}] MHz')
    period = 1000 / freq_mhz
    edge_delay = period * phase_degrees / 360
    result = {
        'schema_version': 1, 'device': DEVICE,
        'model_complete': not missing, 'missing': missing,
        'memory_mhz': freq_mhz, 'phase_degrees': phase_degrees,
        'cas_latency': 2, 'burst_words': burst_words,
        'period_ns': period,
        'nominal_launch_to_memory_edge_ns': edge_delay,
        'nominal_memory_edge_to_next_capture_ns': period - edge_delay,
        'signoff': False,
        'scope': 'Static I/O bounds only; routed path coverage, clock propagation, '
                 'pulse widths, bus turnaround and SDRAM protocol remain to be checked.',
        'static_ports': {'O_sdram_cs_n': 0, 'O_sdram_cke': 1},
        'profile': profile,
    }
    if not missing:
        v = values
        delays = {
            'read': {
                'min': v['clock_flight_min'] + v['t_oh_min'] + v['read_flight_min'],
                'max': v['clock_flight_max'] + v['t_ac_max'] + v['read_flight_max'],
            }
        }
        for name, prefix, flight in (
            ('command', 'cmd', 'command'), ('write', 'data', 'write'),
            ('dqm', 'dqm', 'dqm'),
        ):
            delays[name] = {
                'min': v[flight + '_flight_min'] - v['clock_flight_max'] - v['t_' + prefix + '_hold'],
                'max': v[flight + '_flight_max'] - v['clock_flight_min'] + v['t_' + prefix + '_setup'],
            }
        result['delays_ns'] = delays
    return result


def render_sdc(model, split_clock):
    if not model['model_complete']:
        raise ValueError('cannot emit SDC with missing or unverified bounds')
    source = 'pll/memory/pll/CLKOUTP' if split_clock else 'pll/pll_s2/CLKOUTP'
    memory_clock = 'clk_memory' if split_clock else 'clk_core'
    lines = [
        '# Bounded SDRAM I/O model. This file does not establish routed I/O signoff.',
        '# Source clock already contains the SDRAM phase; do not apply it twice.',
        '# Gowin propagates clk_sdram onto the output net; the named alias needs -add.',
        'create_generated_clock -name sdram_io_clk -source [get_pins {' + source +
        '}] -master_clock clk_sdram -divide_by 1 -add [get_ports {O_sdram_clk}]',
    ]
    parameters = model['profile']['parameters_ns']
    for start, end in ((memory_clock, 'sdram_io_clk'), ('sdram_io_clk', memory_clock)):
        for check in ('setup', 'hold'):
            value = parameters[check + '_uncertainty']['ns']
            lines.append(f'set_clock_uncertainty -{check} {value:.9f} '
                         f'-from [get_clocks {{{start}}}] -to [get_clocks {{{end}}}]')
    groups = (
        ('input', 'read', 'IO_sdram_dq[*]'),
        ('output', 'write', 'IO_sdram_dq[*]'),
        ('output', 'command', 'O_sdram_addr[*] O_sdram_ba[*] O_sdram_ras_n O_sdram_cas_n O_sdram_wen_n'),
        ('output', 'dqm', 'O_sdram_dqm[*]'),
    )
    for direction, group, ports in groups:
        for bound in ('min', 'max'):
            value = model['delays_ns'][group][bound]
            lines.append(f'set_{direction}_delay -clock sdram_io_clk -{bound} '
                         f'{value:.9f} [get_ports {{{ports}}}]')
    lines.append('# CKE=1 and CS_n=0 are constant in this controller, not timed outputs.')
    return '\n'.join(lines) + '\n'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--profile', type=Path, default=DEFAULT_PROFILE)
    parser.add_argument('--freq-mhz', type=float, required=True)
    parser.add_argument('--phase-degrees', type=float, required=True)
    parser.add_argument('--out', type=Path, required=True)
    parser.add_argument('--split-clock', action='store_true')
    parser.add_argument('--burst-words', type=int, choices=(1, 4), default=4)
    parser.add_argument('--emit-sdc', action='store_true',
                        help='require a complete verified model; fail before synthesis otherwise')
    args = parser.parse_args()
    sdc = args.out / 'sdram-io.sdc'
    manifest = args.out / 'sdram-io-model.json'
    if args.profile.resolve() in (sdc.resolve(), manifest.resolve()):
        parser.error('input profile must be separate from generated output files')
    args.out.mkdir(parents=True, exist_ok=True)
    # A failed/missing model must not leave constraints from an older run.
    sdc.unlink(missing_ok=True)
    manifest.unlink(missing_ok=True)
    try:
        controller = (HERE / 'sdram.v').read_bytes()
        check_static_ports(controller.decode())
        contents = args.profile.read_bytes()
        model = audit(json.loads(contents), args.freq_mhz, args.phase_degrees, args.burst_words)
        model.update(profile_path=str(args.profile.resolve()),
                     profile_sha256=hashlib.sha256(contents).hexdigest(),
                     controller_sha256=hashlib.sha256(controller).hexdigest(),
                     constraints_emitted=False)
        if args.emit_sdc and model['model_complete']:
            text = render_sdc(model, args.split_clock)
            sdc.write_text(text)
            model.update(constraints_emitted=True,
                         constraints_sha256=hashlib.sha256(text.encode()).hexdigest())
        manifest.write_text(json.dumps(model, indent=2, allow_nan=False) + '\n')
        if not model['model_complete']:
            message = 'SDRAM I/O model incomplete: ' + ', '.join(model['missing'])
            if args.emit_sdc:
                raise ValueError(message)
            print(message)
        elif args.emit_sdc:
            print(f'Emitted bounded SDRAM I/O constraints: {sdc}; routed coverage still required')
        else:
            print('SDRAM I/O model complete; audit only, constraints were not emitted')
    except (OSError, ValueError, TypeError, KeyError) as error:
        sdc.unlink(missing_ok=True)
        if not manifest.exists():
            manifest.write_text(json.dumps({'model_complete': False, 'signoff': False,
                                            'error': str(error)}, indent=2) + '\n')
        parser.exit(2, f'{error}\n')


if __name__ == '__main__':
    main()
