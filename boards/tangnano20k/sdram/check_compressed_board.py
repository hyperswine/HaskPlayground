#!/usr/bin/env python3
"""Build/run focused RV32IMC integer/fetch/trap checks on the SDRAM C image.

Requires an RV32IMC FPGA image, not the default IM board build. Normal boot-ROM
P/R framing is used. --build-only creates the ELF, flat image and UART frame
without opening the serial port or changing the FPGA.
"""
import argparse
import subprocess
from pathlib import Path

p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--freq-mhz',type=float,default=48)
p.add_argument('--port',default='/dev/cu.usbserial-20250303171')
p.add_argument('--timeout',type=float,default=30)
p.add_argument('--build-only',action='store_true')
a=p.parse_args()
if a.freq_mhz<=0 or a.timeout<=0:p.error('frequency and timeout must be positive')
here=Path(__file__).resolve().parent
root=here.parents[2]
out=root/'output/tangnano20k/rv32c-board-checks'
out.mkdir(parents=True,exist_ok=True)
elf=out/'compressed.elf';binary=out/'compressed.bin';frame=out/'compressed-frame.hex'
subprocess.run(['riscv64-unknown-elf-gcc','-march=rv32imc_zicsr_zifencei','-mabi=ilp32',
    '-mcmodel=medany','-ffreestanding','-fno-builtin','-nostdlib','-nostartfiles',
    '-Wl,--no-warn-rwx-segments','-T',str(here/'link.ld'),str(here/'compressed_board.S'),'-o',str(elf)],check=True)
subprocess.run(['riscv64-unknown-elf-objcopy','-O','binary',str(elf),str(binary)],check=True)
runner=['python3',str(here.parent/'run_program.py'),str(binary),'--freq-mhz',str(a.freq_mhz)]
subprocess.run([*runner,'--frame',str(frame)],check=True)
if a.build_only:
    print(f'Built RV32IMC board fixture ({binary.stat().st_size} bytes): {binary}')
    print(f'Normal boot-ROM P/R frame: {frame}')
else:
    result=subprocess.run([*runner,'--port',a.port,'--timeout',str(a.timeout)],capture_output=True,text=True,
        timeout=a.timeout+max(30,binary.stat().st_size*8680/(a.freq_mhz*1e6)+10))
    print(result.stdout,end='')
    if result.stderr:print(result.stderr,end='')
    assert result.returncode==0,(result.returncode,result.stdout,result.stderr)
    assert 'RV32C BOARD HOLDS\n' in result.stdout and 'DONE' in result.stdout,result.stdout
    print('PASS: physical RV32IMC integer forms, pc+2/pc+4 links, straddled fetch, raw illegal traps, mret, fence.i and RAM boundary')
