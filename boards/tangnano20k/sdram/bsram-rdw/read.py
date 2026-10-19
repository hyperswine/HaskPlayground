#!/usr/bin/env python3
"""Start a run of the loaded BSRAM read-during-write probe and print its report."""
import argparse, os, select, sys, time
sys.path.insert(0, str((os.path.dirname(os.path.abspath(__file__)))+'/../../../../../fprisc/tools'))
from run_simple_risc import open_port
p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--port', default='/dev/cu.usbserial-20250303171')
p.add_argument('--timeout', type=float, default=60)
a = p.parse_args()
fd = open_port(a.port, 115200)
time.sleep(0.5)
while select.select([fd], [], [], 0.1)[0]:
    os.read(fd, 4096)
os.write(fd, b'x')
data = b''
end = time.monotonic()+a.timeout
while time.monotonic() < end and data.count(b'\n') < 19+32:
    if select.select([fd], [], [], 0.1)[0]:
        data += os.read(fd, 4096)
sys.stdout.write(data.decode(errors='replace').replace('\r', ''))
