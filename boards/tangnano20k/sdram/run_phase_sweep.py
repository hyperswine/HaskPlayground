#!/usr/bin/env python3
"""Measure prebuilt 54 MHz burst-test images, then restore a verified CPU image.

Programs FPGA SRAM only. A memory mismatch is an experimental result, while
missing reports, protocol errors and failed recovery leave the run incomplete.
The tester itself halts after its first failing pattern. No electrical signoff
is implied by passing at the board's current temperature and supply voltage.
"""
import argparse
import datetime
import hashlib
import json
import os
from pathlib import Path
import re
import select
import struct
import subprocess
import sys
import time

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))
from run_program import open_port


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def collect(port, out, timeout):
    start = time.monotonic()
    result = {'status': 'incomplete', 'reports': []}
    raw = bytearray()
    buf = b''
    fd = open_port(port, round(54e6 / 868))
    try:
        time.sleep(.1)
        os.write(fd, b'S')
        while time.monotonic() - start < timeout:
            if select.select([fd], [], [], .1)[0]:
                chunk = os.read(fd, 4096)
                raw.extend(chunk)
                buf += chunk
            while len(buf) >= 25:
                packet, buf = buf[:25], buf[25:]
                magic, phase, errors, checks, address, expected, actual = struct.unpack('<4sB5I', packet)
                if magic != b'SDR1':
                    raise ValueError(f'invalid packet {packet.hex()}')
                report = dict(pattern=phase, errors=errors, comparisons=checks,
                              first_address=address, expected=expected, actual=actual,
                              elapsed_seconds=time.monotonic()-start)
                result['reports'].append(report)
                if phase == 255:
                    if len(result['reports']) != 1 or checks or errors:
                        raise ValueError('invalid start report')
                    print('  tester started', flush=True)
                    continue
                if len(result['reports']) < 2 or phase != len(result['reports'])-2:
                    raise ValueError('missing start or out-of-order pattern')
                if checks != (phase+1)*2097152 or errors > checks:
                    raise ValueError('invalid comparison/error counts')
                print(f'  pattern {phase}: {checks:,} comparisons, {errors:,} errors', flush=True)
                if errors or phase == 5:
                    result['status'] = 'memory_fail' if errors else 'pass'
                    return result
        raise TimeoutError('full-memory tester report timeout')
    except Exception as exc:
        result['error'] = f'{type(exc).__name__}: {exc}'
        return result
    finally:
        # The complete final packet precedes the tester's idle state by only a
        # few clocks. Let it stop before closing the BL616 UART bridge.
        time.sleep(.1)
        os.close(fd)
        (out/'uart.bin').write_bytes(raw)
        result['elapsed_seconds'] = time.monotonic()-start
        (out/'hardware.json').write_text(json.dumps(result, indent=2)+'\n')


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--out', type=Path, required=True, help='contains p135 etc. build directories')
    p.add_argument('--phases', nargs='+', type=float, default=[135, 90, 112.5, 157.5, 180, 225])
    p.add_argument('--port', default='/dev/cu.usbserial-20250303171')
    p.add_argument('--loader', type=Path, required=True)
    p.add_argument('--restore', type=Path, required=True)
    p.add_argument('--restore-sha256', required=True)
    p.add_argument('--probe-runner', type=Path, required=True)
    p.add_argument('--restore-freq-mhz', type=int, default=72)
    p.add_argument('--timeout', type=float, default=120)
    a = p.parse_args()
    if not a.phases or any(x not in [22.5*i for i in range(16)] for x in a.phases):
        p.error('phases must be rPLL 22.5-degree steps')
    if len(set(a.phases)) != len(a.phases) or not 0 < a.timeout <= 600:
        p.error('duplicate phases or invalid timeout')
    if sha(a.restore) != a.restore_sha256:
        p.error('restore image SHA256 mismatch')
    a.out = a.out.resolve()
    destination = a.out/'hardware-results.json'
    if destination.exists():
        p.error('hardware-results.json already exists; preserve the earlier run')
    result = {'started_utc': datetime.datetime.now(datetime.timezone.utc).isoformat(),
              'complete': False, 'frequency_mhz': 54, 'electrical_signoff': False,
              'programming': 'SRAM only', 'restore_image': str(a.restore.resolve()),
              'restore_sha256': a.restore_sha256, 'cases': []}

    def save():
        temp = destination.with_suffix('.tmp')
        temp.write_text(json.dumps(result, indent=2, allow_nan=False)+'\n')
        temp.replace(destination)

    # Validate every exact artifact before accessing the board. Keep both the
    # generated configuration and all build inputs bound to the evidence.
    for phase in a.phases:
        out = a.out/f'p{phase:g}'
        image = out/'impl/pnr/sdram_test.fs'
        report = out/'impl/pnr/sdram_test.tr'
        timing = subprocess.run([sys.executable, str(HERE.parent/'check_gowin_timing.py'),
                                 str(report), '--freq-mhz', '54'], capture_output=True, text=True, check=True)
        assert f'PSDA_SEL("{int(phase/22.5):04b}")' in (out/'pll.v').read_text()
        assert f'-phase {phase:g} ' in (out/'clocks.sdc').read_text()
        assert 'sdram_burst_word_memory #(.FREQ(54000000))' in (out/'burst-test_top.v').read_text()
        files = [Path(x) for x in re.findall(r'^add_file "([^"]+)"', (out/'build.tcl').read_text(), re.M)]
        files += [image, report, out/'build.tcl', out/'sdram-io-model.json', Path(__file__)]
        result['cases'].append({'phase_degrees': phase, 'image': str(image),
            'timing': timing.stdout.strip(), 'sha256': {str(f): sha(f) for f in files}})
    save()

    def program(image, log):
        with log.open('w') as output:
            subprocess.run([str(a.loader), '-b', 'tangnano20k', str(image)],
                           stdout=output, stderr=subprocess.STDOUT, timeout=45, check=True)
        time.sleep(.3)

    try:
        for case in result['cases']:
            phase = case['phase_degrees']
            out = a.out/f'p{phase:g}'
            print(f'PROGRAM SRAM: {phase:g} degrees', flush=True)
            program(Path(case['image']), out/'program.log')
            case['hardware'] = collect(a.port, out, a.timeout)
            print(f'RESULT {phase:g}: {case["hardware"]["status"]}', flush=True)
            save()
            if case['hardware']['status'] == 'incomplete':
                raise RuntimeError(case['hardware']['error'])
        result['measurements_complete'] = True
    finally:
        print('RESTORE tested processor image to SRAM', flush=True)
        result['restored'] = False
        save()
        # Recheck before recovery, so no changed file is programmed silently.
        if sha(a.restore) != a.restore_sha256:
            raise RuntimeError('restore image changed during sweep')
        program(a.restore, a.out/'restore-program.log')
        probe = subprocess.run([sys.executable, str(a.probe_runner), '--probe', '--port', a.port,
                                '--freq-mhz', str(a.restore_freq_mhz)],
                               capture_output=True, text=True, timeout=15)
        (a.out/'restore-probe.log').write_text(probe.stdout+probe.stderr)
        result['restored'] = probe.returncode == 0 and 'PASS: board UART diagnostic' in probe.stdout
        result['complete'] = bool(result.get('measurements_complete') and result['restored'])
        save()
        if not result['restored']:
            raise RuntimeError('recovery UART diagnostic failed; inspect restore logs')
        print('PASS: restored processor UART diagnostic', flush=True)
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
