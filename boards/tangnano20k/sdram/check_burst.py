#!/usr/bin/env python3
"""Check the real SDRAM burst controller against a functional SDR pin model.

This covers command sequencing and CAS2 data beats, not electrical I/O timing.
"""
import argparse
import subprocess
from pathlib import Path

p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--freq-mhz',nargs='+',type=int,choices=[27,48,54,66],default=[27,54,66])
a=p.parse_args()
here=Path(__file__).resolve().parent
root=here.parents[2]
out=root/'output/tangnano20k/sdram-burst-tests'
out.mkdir(parents=True,exist_ok=True)
for mhz in a.freq_mhz:
    executable=out/f'burst-{mhz}'
    subprocess.run(['iverilog','-g2012','-s','burst_memory_tb','-P',f'burst_memory_tb.FREQ={mhz*1000000}',
        '-o',str(executable),str(here/'sdram.v'),str(here/'burst_memory.v'),str(here/'burst_memory_tb.v')],check=True)
    result=subprocess.run(['vvp',str(executable)],capture_output=True,text=True,timeout=60)
    (out/f'burst-{mhz}.log').write_text(result.stdout+result.stderr)
    assert result.returncode==0,(result.stdout,result.stderr)
    assert 'PASS:' in result.stdout,result.stdout
    print(next(line for line in result.stdout.splitlines() if line.startswith('PASS:')))
