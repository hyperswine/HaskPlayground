#!/usr/bin/env python3
"""Gate-level simulation of a whole Gowin SDRAM pipeline build under Verilator.

Runs a build's place-and-route netlist (impl/pnr/sdram_test.vo, or --vg for the
synthesis netlist) with gowin_models.v, a preloaded raw image (such as an
FP-RISC Base image) and the netlist's boot ROM (gls_tb.v). About 15-100
thousand SDRAM clocks per second.

  run_gls.py output/tangnano20k/sdram-pipeline-s54-l11-wu image.bin

--collide picks the block RAM read-during-write model (see gowin_models.v):
word (default: write-through, as the board's 512x32 BSRAM), row, or old
(Gowin's prim_sim.v).
"""
from pathlib import Path
import argparse
import re
import subprocess
import sys

HERE = Path(__file__).resolve().parent


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument('build', type=Path, help='a build.sh output directory')
    p.add_argument('image', type=Path, help='raw binary loaded at 0x8000_0000')
    p.add_argument('--vg', action='store_true', help='use the synthesis netlist')
    p.add_argument('--collide', choices=['word', 'row', 'old'], default='word')
    p.add_argument('--limit', type=int, default=400_000_000, help='SDRAM clocks')
    p.add_argument('--out', type=Path)
    a = p.parse_args()
    build = a.build.resolve()
    netlist = build/('impl/gwsynthesis/sdram_test.vg' if a.vg else 'impl/pnr/sdram_test.vo')
    if not netlist.exists():
        raise SystemExit(f'missing {netlist}')
    tcl = (build/'build.tcl').read_text()
    cpu_mhz = int(re.search(r'set_option -frequency (\d+)', tcl)[1])
    out = (a.out or build/f'gls-{"vg" if a.vg else "vo"}-{a.collide}').resolve()
    out.mkdir(parents=True, exist_ok=True)
    # The models' rPLL serves the flat .vo and the .vg's PLL wrapper modules.
    sources = [HERE/'gowin_models.v', netlist, HERE/'gls_tb.v']
    defines = {'word': [], 'row': ['+define+GOWIN_COLLIDE_ROW'], 'old': ['+define+GOWIN_COLLIDE_OLD']}[a.collide]
    image = a.image.read_bytes()
    image += b'\0'*(-len(image) % 4)
    (out/'image.hex').write_text('\n'.join(f'{int.from_bytes(image[i:i+4], "little"):08x}'
                                           for i in range(0, len(image), 4))+'\n')
    command = ['verilator', '--binary', '--timing', '-O3', '-Wno-fatal', '-Wno-lint', '-Wno-style',
               '--timescale', '1ns/1ps', '--top-module', 'gls_tb', '--Mdir', out/'obj', '-j', '0',
               f'-GCPU_MHZ={cpu_mhz}', *defines, *sources]
    with (out/'build.log').open('w') as f:
        if subprocess.run([str(x) for x in command], stdout=f, stderr=subprocess.STDOUT).returncode:
            sys.stdout.write((out/'build.log').read_text()[-4000:])
            raise SystemExit('verilator build failed')
    run = [out/'obj'/'Vgls_tb', f'+IMAGE={out}/image.hex', f'+WORDS={len(image)//4}', f'+LIMIT={a.limit}']
    with (out/'run.log').open('w') as f:
        process = subprocess.Popen([str(x) for x in run], cwd=out, stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT, text=True)
        for line in process.stdout:
            sys.stdout.write(line)
            sys.stdout.flush()
            f.write(line)
        process.wait()
    print(f'logs: {out}')


if __name__ == '__main__':
    main()
