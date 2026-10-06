#!/usr/bin/env python3
"""Check aliases/self-modifying code and report cycles for repeated cached reads."""
import argparse, re, subprocess
from pathlib import Path
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--port',default='/dev/cu.usbserial-20250303171')
p.add_argument('--freq-mhz',type=float,default=54)
a=p.parse_args()
root=Path(__file__).resolve().parents[3]
here=root/'boards/tangnano20k/sdram'
out=root/'output/tangnano20k/sdram-programs';out.mkdir(parents=True,exist_ok=True)
elf=out/'benchmark.elf';binary=elf.with_suffix('.bin')
subprocess.run(['riscv64-unknown-elf-gcc','-march=rv32im_zicsr_zifencei','-mabi=ilp32',
 '-mcmodel=medany','-O2','-Wall','-Wextra','-ffreestanding','-fno-builtin','-nostdlib',
 '-nostartfiles','-Wl,--no-warn-rwx-segments','-T',str(here/'link.ld'),
 str(here.parent/'c/crt0.S'),str(here.parent/'c/uart.c'),str(here/'benchmark.c'),'-o',str(elf)],check=True)
subprocess.run(['riscv64-unknown-elf-objcopy','-O','binary',str(elf),str(binary)],check=True)
r=subprocess.run(['python3',str(here.parent/'run_program.py'),str(binary),'--port',a.port,
 '--ram-base','0x80000000','--freq-mhz',str(a.freq_mhz),'--timeout','60'],
 capture_output=True,text=True,timeout=75)
assert r.returncode==0,(r.stdout,r.stderr)
m=re.fullmatch(r'COHERENCE PASS\nBENCH 1625600 (\d+)\nDONE\n',r.stdout)
assert m,r.stdout
cycles=int(m[1]);print(f'PASS: alias and instruction coherence; {cycles} cycles, {cycles/(a.freq_mhz*1000):.3f} ms')
