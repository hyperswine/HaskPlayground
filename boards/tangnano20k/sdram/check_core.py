#!/usr/bin/env python3
"""Build and verify a high-address program with a 1 MiB SDRAM working set."""
import argparse, subprocess
from pathlib import Path
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--port',default='/dev/cu.usbserial-20250303171')
p.add_argument('--freq-mhz',type=float,default=54)
a=p.parse_args()
root=Path(__file__).resolve().parents[3]
here=root/'boards/tangnano20k/sdram'
out=root/'output/tangnano20k/sdram-programs';out.mkdir(parents=True,exist_ok=True)
elf=out/'stress.elf';binary=elf.with_suffix('.bin')
subprocess.run(['riscv64-unknown-elf-gcc','-march=rv32im_zicsr','-mabi=ilp32','-mcmodel=medany','-O2',
 '-Wall','-Wextra','-ffreestanding','-fno-builtin','-nostdlib','-nostartfiles',
 '-ffunction-sections','-fdata-sections','-Wl,--gc-sections','-Wl,--no-warn-rwx-segments',
 '-T',str(here/'link.ld'),str(here.parent/'c/crt0.S'),str(here.parent/'c/uart.c'),str(here/'stress.c'),'-o',str(elf)],check=True)
subprocess.run(['riscv64-unknown-elf-objcopy','-O','binary',str(elf),str(binary)],check=True)
run=subprocess.run(['python3',str(here.parent/'run_program.py'),str(binary),'--port',a.port,
 '--ram-base','0x80000000','--freq-mhz',str(a.freq_mhz),'--timeout','60'],capture_output=True,text=True,timeout=75)
assert run.returncode==0,(run.stdout,run.stderr)
assert run.stdout=='SDRAM 1MiB + ALL BANKS HOLDS\nDONE\n',run.stdout
print('PASS: high-address execution, 1 MiB full working set, all banks, last word and byte lanes')
