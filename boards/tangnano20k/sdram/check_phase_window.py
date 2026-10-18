#!/usr/bin/env python3
"""Map functional CAS2 burst-read sensitivity to phase and artificial tAC.

This reuses the independent pin/command oracle and all-bank/mask/refresh
checks. No FPGA I/O routing, tOH minimum, command setup/hold, clock jitter
or metastability is modeled. These results are not chip specifications.
"""
import argparse
import hashlib
import json
import math
from pathlib import Path
import subprocess

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--freq-mhz', type=int, choices=(27, 48, 54, 66), default=54)
    p.add_argument('--phases', nargs='+', type=float, default=[90, 112.5, 135, 157.5, 180, 225])
    p.add_argument('--access-delays', nargs='+', type=float, default=[6, 12])
    p.add_argument('--out', type=Path, default=ROOT/'output/tangnano20k/sdram-phase-window')
    args = p.parse_args()
    period = 1000 / args.freq_mhz
    if any(not math.isfinite(x) or not 0 < x < 360 for x in args.phases):
        p.error('phases must be finite and strictly between 0 and 360 degrees')
    if any(not math.isfinite(x) or not 0 <= x < period for x in args.access_delays):
        p.error('access delays must be finite, nonnegative and less than one clock period')
    if any(abs(period*(1-phase/360)-tac) < .05
           for phase in args.phases for tac in args.access_delays):
        p.error('choose points at least 0.05 ns from an ideal sampling boundary')
    args.out.mkdir(parents=True, exist_ok=True)
    original = (HERE/'burst_memory_tb.v').read_text()
    assert original.count('wire phase_clk=~clk;') == 1
    source = original.replace('wire phase_clk=~clk;',
        'reg phase_clk=0;always @(clk)phase_clk <= #(2*HALF_PERIOD*PHASE_DEGREES/360.0) clk;')
    old = 'parameter real TAC_NS=0.0);'
    assert source.count(old) == 1
    source = source.replace(old, 'parameter real TAC_NS=0.0,parameter real PHASE_DEGREES=180.0);')
    # Delayed nonblocking assignments preserve clock edges above 180 degrees;
    # a continuous assign's inertial delay would suppress those pulses.
    fixture = args.out/'phase_window_tb.v';fixture.write_text(source)
    result = {'complete': False, 'passed': False, 'electrical_signoff': False,
        'frequency_mhz': args.freq_mhz, 'period_ns': period,
        'scope': 'Functional phase/access-delay sensitivity only. Artificial delays are not memory limits.',
        'unmodeled': ['FPGA I/O routing and package skew', 'tOH minimum',
                      'command/write setup and hold', 'PLL relative jitter', 'PVT'],
        'source_sha256': {str(f.relative_to(ROOT)): hashlib.sha256(f.read_bytes()).hexdigest()
            for f in (HERE/'sdram.v', HERE/'burst_memory.v', HERE/'burst_memory_tb.v', Path(__file__))},
        'cases': []}

    def save():
        (args.out/'results.json').write_text(json.dumps(result, indent=2, allow_nan=False)+'\n')

    save()
    for phase in args.phases:
        available = period * (1-phase/360)
        for tac in args.access_delays:
            label = f'p{phase:g}-tac{tac:g}'
            executable = args.out/label
            compiled = subprocess.run(['iverilog', '-g2012', '-s', 'burst_memory_tb',
                '-P', f'burst_memory_tb.FREQ={args.freq_mhz*1000000}',
                '-P', f'burst_memory_tb.PHASE_DEGREES={phase}',
                '-P', f'burst_memory_tb.TAC_NS={tac}', '-o', str(executable),
                str(HERE/'sdram.v'), str(HERE/'burst_memory.v'), str(fixture)],
                capture_output=True, text=True, timeout=60)
            if compiled.returncode:
                raise RuntimeError(compiled.stdout+compiled.stderr)
            executed = subprocess.run(['vvp', str(executable)], capture_output=True, text=True, timeout=60)
            output = executed.stdout+executed.stderr
            (args.out/(label+'.log')).write_text(output)
            expected_pass = tac < available
            observed_pass = executed.returncode == 0 and '207 reads, 98 masked writes, 828 beat comparisons' in output
            expected_read_failure = executed.returncode != 0 and 'word ' in output and 'beat ' in output
            matched = observed_pass if expected_pass else expected_read_failure
            result['cases'].append({'phase_degrees': phase, 'artificial_tac_ns': tac,
                'ideal_read_budget_ns': available, 'ideal_read_margin_ns': available-tac,
                'expected_functional_pass': expected_pass, 'observed_functional_pass': observed_pass,
                'matched_expectation': matched, 'exit_code': executed.returncode,
                'log_sha256': hashlib.sha256(output.encode()).hexdigest(), 'output': output})
            save()
            if not matched:
                raise AssertionError(f'{label}: unexpected simulator outcome\n{output}')
            print(f'{label}: expected {"pass" if expected_pass else "read failure"} observed; '
                  f'ideal read margin {available-tac:+.3f} ns', flush=True)
    result.update(complete=True, passed=True)
    save()
    print(f'PASS: {len(result["cases"])} phase/access-delay points matched; electrical timing unqualified')


if __name__ == '__main__':
    main()
