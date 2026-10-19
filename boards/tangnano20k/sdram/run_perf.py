#!/usr/bin/env python3
"""Measure where the SDRAM pipeline's cycles go, in simulation.

Runs a preloaded RV32 image (a raw binary at 0x8000_0000, such as an FP-RISC
Base image) on the generated CPU, the real cache, the optional clock bridge,
the real burst controller and a functional SDRAM pin model (perf_tb.v), and
prints cache-hold counters. Verilator by default; the UART divider is
shortened in a simulation copy of the generated Verilog only.
"""
from pathlib import Path
import argparse
import re
import subprocess
import sys

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]


def sv_safe(text):
    """Rename Verilog identifiers that SystemVerilog reserves.

    Clash emits legal Verilog identifiers that Verilator, parsing as
    SystemVerilog, rejects.
    """
    return re.sub(r'\b(new|byte|bit|logic|int|do|final|type|ref|this|string|unique|priority|break|continue|return|void|shortint|longint|chandle|class|package|import|export|context|bind|alias|const|static|automatic|local|virtual|extends|super|null|inside|dist|solve|before|wait_order|randomize|with|matches|tagged|interface|modport|program|property|sequence|cover|assert|assume|expect|clocking|constraint|covergroup|coverpoint|cross|bins|binsof|illegal_bins|ignore_bins|first_match|throughout|within|intersect|join_any|join_none|forkjoin|struct|union|enum|typedef|packed|var|rand|randc|pure|protected|foreach|endclass|endpackage)\b(?!\s*\()',
                  lambda m: m[1]+'_sv', text)


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('image', type=Path, help='raw binary loaded at 0x8000_0000')
    p.add_argument('--elf', type=Path, help='ELF for the text end (classifies code fills)')
    p.add_argument('--generated', type=Path,
                   default=ROOT/'output/tangnano20k/sdram-generated/SdramPipeline-1/SdramPipeline.topEntity')
    p.add_argument('--cache-rtl', type=Path, default=HERE/'pipeline_cache.v')
    p.add_argument('--bridge-rtl', type=Path, default=HERE/'clock_bridge.v')
    p.add_argument('--cpu-mhz', type=float, default=72)
    p.add_argument('--mem-mhz', type=float, default=54)
    p.add_argument('--single-clock', action='store_true', help='no bridge; memory runs on the CPU clock')
    p.add_argument('--line-bits', type=int, default=12)
    p.add_argument('--write-update', type=int, choices=[0, 1], default=0)
    p.add_argument('--start', default='DTREE BEGIN train=128 test=64 bins=8 depth=5 minLeaf=2')
    p.add_argument('--end', default='FPR EXIT 0')
    p.add_argument('--limit', type=int, default=800_000_000)
    p.add_argument('--out', type=Path)
    a = p.parse_args()
    name = a.out or ROOT/'output/tangnano20k/perf'/(
        f'{a.image.stem}-{"single" if a.single_clock else "cdc"}-c{a.cpu_mhz:g}-m{a.mem_mhz:g}'
        f'-l{a.line_bits}-w{a.write_update}-{a.cache_rtl.stem}')
    out = name.resolve()
    out.mkdir(parents=True, exist_ok=True)
    sources = sorted(a.generated.glob('*.v'))
    if not sources:
        raise SystemExit(f'no generated Verilog in {a.generated}')
    copied = []
    for source in sources:
        text = source.read_text()
        text, full = re.subn(r"10'd867\b", "10'd31", text)
        text, half = re.subn(r"10'd433\b", "10'd15", text)
        assert full >= 6 and half >= 1, (full, half)
        text = sv_safe(text)
        target = out/source.name
        target.write_text(text)
        copied.append(target)
    (out/'rom.hex').write_text('\n'.join(['800002b7', '00028067']+['00000013']*510)+'\n')
    image = a.image.read_bytes()
    image += b'\0'*(-len(image) % 4)
    (out/'image.hex').write_text('\n'.join(f'{int.from_bytes(image[i:i+4], "little"):08x}'
                                           for i in range(0, len(image), 4))+'\n')
    text_end = 0
    if a.elf:
        sections = subprocess.run(['riscv64-unknown-elf-readelf', '-SW', a.elf],
                                  capture_output=True, text=True, check=True).stdout
        found = re.search(r'\s\.text\s+PROGBITS\s+([0-9a-f]+)\s+[0-9a-f]+\s+([0-9a-f]+)', sections)
        if found:
            text_end = (int(found[1], 16)+int(found[2], 16)-0x8000_0000)//4
    cdc = 0 if a.single_clock else 1
    binary = out/'obj'/'Vperf_tb'
    build = ['verilator', '--binary', '--timing', '-O3', '-Wno-fatal', '-Wno-lint', '-Wno-style',
             '--top-module', 'perf_tb', '--Mdir', out/'obj', '-j', '0',
             f'-GROM_FILE="{out}/rom.hex"', f'-GCDC_BRIDGE={cdc}', f'-GCPU_MHZ={a.cpu_mhz}',
             f'-GMEM_MHZ={a.mem_mhz}', f'-GLINE_BITS={a.line_bits}', f'-GWRITE_UPDATE={a.write_update}',
             *copied, a.cache_rtl.resolve(), a.bridge_rtl.resolve(), HERE/'sdram.v', HERE/'burst_memory.v',
             HERE/'perf_tb.v']
    log = out/'build.log'
    with log.open('w') as f:
        result = subprocess.run([str(x) for x in build], cwd=ROOT, stdout=f, stderr=subprocess.STDOUT)
    if result.returncode:
        sys.stdout.write(log.read_text()[-4000:])
        raise SystemExit('verilator build failed')
    run = [binary, f'+IMAGE={out}/image.hex', f'+WORDS={len(image)//4}', f'+TEXT_END={text_end}',
           f'+START={a.start}', f'+END={a.end}', f'+LIMIT={a.limit}']
    with (out/'run.log').open('w') as f:
        process = subprocess.Popen([str(x) for x in run], cwd=out, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        for line in process.stdout:
            sys.stdout.write(line)
            sys.stdout.flush()
            f.write(line)
        process.wait()
    print(f'logs: {out}')


if __name__ == '__main__':
    main()
