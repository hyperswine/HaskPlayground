#!/usr/bin/env python3
"""Check every SDRAM word; capture six cumulative hardware test reports."""
import argparse, os, select, struct, sys, time
from pathlib import Path
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from run_program import open_port
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--port',default='/dev/cu.usbserial-20250303171')
p.add_argument('--timeout',type=float,default=120)
p.add_argument('--freq-mhz',type=float,default=54)
a=p.parse_args()
fd=open_port(a.port,round(a.freq_mhz*1e6/868))
try:
    time.sleep(.1); os.write(fd,b'S');
    buf=b''; phase=0; deadline=time.monotonic()+a.timeout
    while phase<6 and time.monotonic()<deadline:
        readable,_,_=select.select([fd],[],[],.1)
        if readable: buf+=os.read(fd,4096)
        while len(buf)>=25:
            assert buf[:4]==b'SDR1',f'invalid report {buf[:25].hex()}'
            _,got,errors,checks,address,expected,actual=struct.unpack('<4sB5I',buf[:25]);buf=buf[25:]
            if got==255:
                print('SDRAM tester started',flush=True); continue
            assert got==phase,f'expected phase {phase}, received {got}'
            assert checks==(phase+1)*2097152,f'wrong coverage: {checks}'
            assert errors==0,f'{errors} mismatches; first at 0x{address:06x}: expected {expected:08x}, got {actual:08x}'
            print(f'PASS phase {phase}: {checks:,} cumulative full-word comparisons',flush=True)
            phase+=1
    assert phase==6,f'timeout after {phase}/6 reports, partial={buf.hex()}'
    print('PASS: all 8 MiB, zero/ones/address/complement, every byte lane, 250 ms retention with refresh; 12,582,912 word comparisons')
finally: os.close(fd)
