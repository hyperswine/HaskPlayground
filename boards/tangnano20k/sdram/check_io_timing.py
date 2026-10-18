#!/usr/bin/env python3
"""Check SDRAM bound arithmetic, refusals and build preflight without hardware.

The complete profile below contains artificial test values, never chip specs.
"""
import copy
import hashlib
import json
import os
from pathlib import Path
import subprocess

import io_timing as io

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
OUT = ROOT / 'output/tangnano20k/sdram-io-model-tests'


def fixture():
    p = json.loads(io.DEFAULT_PROFILE.read_text())
    p['qualification'].update(status='verified', source='ARTIFICIAL UNIT TEST ONLY',
        operating_conditions='Not a memory specification', boundary_accounting='Synthetic boundary',
        min_mhz=48, max_mhz=66)
    values = [5.4, 2.5, 1.5, .8, 2, 1, 1.2, .6, .1, .3,
              .2, .4, .2, .7, .4, .9, .3, .8, .25, .1]
    for key, value in zip(io.PARAMETERS, values, strict=True):
        p['parameters_ns'][key] = dict(ns=value, status='verified', source='ARTIFICIAL UNIT TEST ONLY')
    return p


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT/'results.json').write_text(json.dumps({'complete': False, 'passed': False})+'\n')
    cases = []

    def check(name, action):
        action()
        cases.append({'name': name, 'passed': True})

    def expect_error(action):
        try:
            action()
        except ValueError:
            return
        raise AssertionError('expected ValueError')

    profile = fixture()

    def arithmetic():
        m = io.audit(profile, 54, 135)
        assert m['model_complete'] and not m['signoff']
        expected = dict(read=(2.8, 6.1), write=(-.9, 2.8), command=(-.9, 2.1), dqm=(-.6, 1.9))
        for group, (low, high) in expected.items():
            assert abs(m['delays_ns'][group]['min'] - low) < 1e-9
            assert abs(m['delays_ns'][group]['max'] - high) < 1e-9
        assert abs(m['nominal_launch_to_memory_edge_ns'] - 6.944444444) < 1e-8
        assert abs(m['nominal_memory_edge_to_next_capture_ns'] - 11.574074074) < 1e-8
        sdc = io.render_sdc(m, True)
        assert sdc.count('set_input_delay') == 2 and sdc.count('set_output_delay') == 6
        assert sdc.count('set_clock_uncertainty') == 4
        assert '-min -0.900000000' in sdc and '-phase' not in sdc
        assert '-master_clock clk_sdram -divide_by 1 -add' in sdc
        assert 'pll/memory/pll/CLKOUTP' in sdc
        assert 'pll/pll_s2/CLKOUTP' in io.render_sdc(m, False)
    check('independent-arithmetic-and-sdc', arithmetic)

    for key in io.PARAMETERS:
        def missing(key=key):
            p = copy.deepcopy(profile)
            p['parameters_ns'][key]['ns'] = None
            m = io.audit(p, 54, 135)
            assert not m['model_complete'] and key in m['missing']
            expect_error(lambda: io.render_sdc(m, True))
        check('missing-' + key, missing)

    for value in (-1, True, float('nan'), float('inf'), '5.4'):
        def invalid(value=value):
            p = copy.deepcopy(profile);p['parameters_ns']['t_ac_max']['ns'] = value
            expect_error(lambda: io.audit(p, 54, 135))
        check('invalid-number-' + str(value), invalid)

    def invalid_shapes():
        for p in ([], None, {'schema_version': 1}, {**profile, 'parameters_ns': []},
                  {**profile, 'qualification': []}):
            expect_error(lambda p=p: io.audit(p, 54, 135))
    check('invalid-profile-shapes', invalid_shapes)

    def scope_and_ranges():
        expect_error(lambda: io.audit(profile, 72, 135))
        expect_error(lambda: io.audit(profile, 0, 135))
        expect_error(lambda: io.audit(profile, 54, 360))
        p = copy.deepcopy(profile)
        p['parameters_ns']['clock_flight_min']['ns'] = 1
        expect_error(lambda: io.audit(p, 54, 135))
        p = copy.deepcopy(profile)
        p['parameters_ns']['t_oh_min']['ns'] = 8
        expect_error(lambda: io.audit(p, 54, 135))
        for key, value in [('source', ''), ('status', 'assumed')]:
            p = copy.deepcopy(profile);p['parameters_ns']['t_ac_max'][key] = value
            assert not io.audit(p, 54, 135)['model_complete']
        assert not io.audit(profile, 54, 135, 1)['model_complete']
        p = copy.deepcopy(profile);p['qualification']['status'] = 'unverified'
        assert not io.audit(p, 54, 135)['model_complete']
        p = copy.deepcopy(profile);p['qualification']['burst_words'] = True
        assert not io.audit(p, 54, 135, 1)['model_complete']
    check('scope-provenance-ranges', scope_and_ranges)

    def static_ports():
        controller = (HERE/'sdram.v').read_text()
        io.check_static_ports(controller)
        expect_error(lambda: io.check_static_ports(controller.replace("assign SDRAM_CKE = 1'b1;", 'assign SDRAM_CKE = dynamic;')))
        expect_error(lambda: io.check_static_ports(controller.replace("assign SDRAM_nCS = 1'b0;", "assign SDRAM_nCS = 1'b1;")))
    check('static-port-coverage-contract', static_ports)

    def timing_guard_nonfinite():
        report = ROOT/'output/tangnano20k/sdram-pipeline-c72-m54-sd-fpr-trial3/impl/pnr/sdram_test.tr'
        # Argument validation precedes report loading for the core frequency.
        r = subprocess.run(['python3', str(HERE.parent/'check_gowin_timing.py'),
                            '/not/read', '--freq-mhz', 'nan'], capture_output=True, text=True)
        assert r.returncode == 2 and 'finite and positive' in r.stderr
        if report.exists():
            r = subprocess.run(['python3', str(HERE.parent/'check_gowin_timing.py'),
                                str(report), '--freq-mhz', '72', '--memory-freq-mhz', 'inf'],
                               capture_output=True, text=True)
            assert r.returncode == 2 and 'finite and positive' in r.stderr
    check('timing-guard-nonfinite-frequency', timing_guard_nonfinite)

    synthetic = OUT / 'ARTIFICIAL-TEST-PROFILE.json'
    synthetic.write_text(json.dumps(profile, indent=2) + '\n')

    def cli_and_stale_files():
        out = OUT / 'cli';out.mkdir(exist_ok=True)
        command = ['python3', str(HERE/'io_timing.py'), '--freq-mhz', '54',
                   '--phase-degrees', '135', '--out', str(out)]
        r = subprocess.run(command, capture_output=True, text=True)
        assert r.returncode == 0, r.stderr
        assert not (out/'sdram-io.sdc').exists()
        (out/'sdram-io.sdc').write_text('STALE CONSTRAINTS')
        r = subprocess.run(command + ['--emit-sdc'], capture_output=True, text=True)
        assert r.returncode == 2 and 'model incomplete' in r.stderr
        assert not (out/'sdram-io.sdc').exists()
        r = subprocess.run(command + ['--profile', str(synthetic), '--emit-sdc', '--split-clock'],
                           capture_output=True, text=True)
        assert r.returncode == 0, r.stderr
        assert json.loads((out/'sdram-io-model.json').read_text())['constraints_emitted']
        bad = OUT / 'invalid.json';bad.write_text('[]')
        r = subprocess.run(command + ['--profile', str(bad), '--emit-sdc'], capture_output=True, text=True)
        assert r.returncode == 2 and not (out/'sdram-io.sdc').exists()
        assert not json.loads((out/'sdram-io-model.json').read_text())['model_complete']
        retained = (out/'sdram-io-model.json').read_bytes()
        r = subprocess.run(command + ['--profile', str(out/'sdram-io-model.json'), '--emit-sdc'],
                           capture_output=True, text=True)
        assert r.returncode == 2 and (out/'sdram-io-model.json').read_bytes() == retained
    check('cli-refusals-no-stale-sdc', cli_and_stale_files)

    def build_preflight():
        env = {k: v for k, v in os.environ.items() if not k.startswith('SDRAM_')}
        env.update(SDRAM_CPU_FREQ_MHZ='72', SDRAM_FREQ_MHZ='54', SDRAM_RVC='1',
                   SDRAM_GENERATE_ONLY='1', SDRAM_REQUIRE_IO_MODEL='1',
                   SDRAM_RTL_FILE='/definitely/not/a/rtl/file',
                   SDRAM_OUT=str(OUT/'strict-build'))
        r = subprocess.run(['bash', str(HERE/'build.sh'), 'pipeline'], cwd=ROOT,
                           env=env, capture_output=True, text=True)
        assert r.returncode != 0 and 'model incomplete' in r.stderr
        assert 'SDRAM_RTL_FILE does not exist' not in r.stderr
        rtl = OUT/'ARTIFICIAL-UNUSED-RTL.v';rtl.write_text('// generate-only fixture\n')
        env.update(SDRAM_REQUIRE_IO_MODEL='0', SDRAM_RTL_FILE=str(rtl),
                   SDRAM_OUT=str(OUT/'internal-build'))
        r = subprocess.run(['bash', str(HERE/'build.sh'), 'pipeline'], cwd=ROOT,
                           env=env, capture_output=True, text=True)
        assert r.returncode == 0, r.stdout + r.stderr
        assert 'internal FPGA timing only' in r.stdout
        assert not (Path(env['SDRAM_OUT'])/'sdram-io.sdc').exists()
        env.update(SDRAM_IO_PROFILE=str(synthetic), SDRAM_OUT=str(OUT/'bounded-build'))
        r = subprocess.run(['bash', str(HERE/'build.sh'), 'pipeline'], cwd=ROOT,
                           env=env, capture_output=True, text=True)
        assert r.returncode == 0, r.stdout + r.stderr
        sdc = (Path(env['SDRAM_OUT'])/'clocks.sdc').read_text()
        assert 'create_generated_clock -name sdram_io_clk' in sdc
        assert sdc.count('set_input_delay') == 2 and sdc.count('set_output_delay') == 6
    check('build-preflight-before-clash-and-profile-integration', build_preflight)

    def build_phase_alignment():
        env = {k: v for k, v in os.environ.items() if not k.startswith('SDRAM_')}
        env.update(SDRAM_FREQ_MHZ='54', SDRAM_GENERATE_ONLY='1')
        rtl = OUT/'ARTIFICIAL-UNUSED-RTL.v'
        env.update(SDRAM_RTL_FILE=str(rtl))
        for phase, bits in [('0','0000'), ('112.5','0101'), ('135','0110'), ('157.5','0111'), ('225','1010')]:
            for mode, cpu in [('burst-test','54'), ('pipeline','72')]:
                out = OUT/f'phase-{mode}-{phase}'
                env.update(SDRAM_OUT=str(out), SDRAM_PHASE_DEGREES=phase, SDRAM_CPU_FREQ_MHZ=cpu)
                r = subprocess.run(['bash', str(HERE/'build.sh'), mode], cwd=ROOT,
                                   env=env, capture_output=True, text=True)
                assert r.returncode == 0, r.stdout+r.stderr
                assert f'PSDA_SEL("{bits}")' in (out/'pll.v').read_text()
                assert f'-phase {phase} ' in (out/'clocks.sdc').read_text()
                assert json.loads((out/'sdram-io-model.json').read_text())['phase_degrees'] == float(phase)
        for phase in ['-22.5','22.6','360','nan','inf']:
            env.update(SDRAM_PHASE_DEGREES=phase, SDRAM_RTL_FILE='/must/not/read')
            r = subprocess.run(['bash', str(HERE/'build.sh'), 'pipeline'], cwd=ROOT,
                               env=env, capture_output=True, text=True)
            assert r.returncode != 0 and '22.5-degree steps' in r.stderr
    check('single-and-split-pll-sdc-phase-alignment-and-refusals', build_phase_alignment)
    result = {'complete': True, 'passed': True, 'cases': cases,
              'source_sha256': {p.name: hashlib.sha256(p.read_bytes()).hexdigest()
                                for p in (HERE/'io_timing.py', HERE/'build.sh', Path(__file__))},
              'note': 'Artificial test bounds are not SDRAM specifications or I/O signoff.'}
    (OUT/'results.json').write_text(json.dumps(result, indent=2) + '\n')
    print(f'PASS: {len(cases)} SDRAM I/O model, refusal and build-integration cases')


if __name__ == '__main__':
    main()
