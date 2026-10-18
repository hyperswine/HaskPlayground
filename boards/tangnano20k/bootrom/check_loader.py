#!/usr/bin/env python3
"""Exercise both actual ROM C parsers with native UART and memory hooks.

This covers large uploads/bounds quickly. RTL/UART execution is a separate
check: sdram/check_pipeline_rtl.py --boot-loader --loader-command H.
"""
from pathlib import Path
import subprocess

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
OUT = ROOT / 'output/tangnano20k/bootrom/host-test'
OUT.mkdir(parents=True, exist_ok=True)
for variant in ('bram', 'sdram'):
    image = OUT / variant
    subprocess.run(['cc', '-std=c11', '-O2', '-Wall', '-Wextra', '-Werror',
                    '-DROM_HOST_TEST', *(['-DROM_SDRAM'] if variant == 'sdram' else []),
                    str(HERE / 'bootrom.c'), str(HERE / 'test_host.c'), '-o', str(image)], check=True)
    result = subprocess.run([str(image)], check=True, capture_output=True, text=True)
    (OUT / f'{variant}.log').write_text(result.stdout)
    print(result.stdout, end='')
