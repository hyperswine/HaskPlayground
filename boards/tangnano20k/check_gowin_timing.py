#!/usr/bin/env python3
"""Require the requested clock and zero internal setup/hold timing violations."""
import argparse
from pathlib import Path
import re


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('report', type=Path)
    parser.add_argument('--freq-mhz', type=float, required=True)
    args = parser.parse_args()
    if args.freq_mhz <= 0:
        parser.error('frequency must be positive')
    report = args.report.read_text()
    clock = re.search(r'^\s*1\s+clk_core\s+([\d.]+)\(MHz\)\s+([\d.]+)\(MHz\)', report, re.M)
    setup = re.search(r'<Numbers of Setup Violated Endpoints>:(\d+)', report)
    hold = re.search(r'<Numbers of Hold Violated Endpoints>:(\d+)', report)
    paths = re.search(r'3\.1\.1 Setup Paths Table\s*<Report Command>:(.*?)3\.1\.2 Hold Paths Table', report, re.S)
    slack = re.search(r'^\s*1\s+(-?[\d.]+)\s', paths[1], re.M) if paths else None
    if not all((clock, setup, hold, slack)):
        parser.error('missing core frequency, endpoint counts or setup paths in vendor report')
    if abs(float(clock[1]) - args.freq_mhz) > .001:
        parser.error(f'expected {args.freq_mhz:g} MHz, report constrains {clock[1]} MHz')
    passed = int(setup[1]) == 0 and int(hold[1]) == 0 and float(slack[1]) >= 0
    print(f'{"PASS" if passed else "FAIL"}: {args.freq_mhz:g} MHz constraint, '
          f'{clock[2]} MHz Fmax, {slack[1]} ns worst setup slack; '
          f'{setup[1]} setup / {hold[1]} hold violating endpoints')
    return 0 if passed else 2


if __name__ == '__main__':
    raise SystemExit(main())
