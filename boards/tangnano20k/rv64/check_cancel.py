#!/usr/bin/env python3
"""Exercise cancellation/reload on hardware, including invalid upload counts."""
import argparse
import os
import select
import sys
import time
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from run_program import open_port, send_frame

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--port', default='/dev/cu.usbserial-20250303171')
p.add_argument('--freq-mhz', type=float, default=27)
a = p.parse_args()
fd = open_port(a.port, round(a.freq_mhz * 1e6 / 868))
def expect(want):
    data = b''
    end = time.monotonic() + 3
    while len(data) < len(want) and time.monotonic() < end:
        if select.select([fd], [], [], .05)[0]:
            data += os.read(fd, len(want) - len(data))
    assert data == want, (want, data)
try:
    # Invalid count must return to the host and leave an empty checksum range.
    send_frame(fd, b'Q\xff\xff\xff\xffV')
    expect(b'00000000\n')
    for iteration in range(10):
        # JAL zero,0 continually fetches through the cache/memory interface.
        send_frame(fd, b'P\x01\x00\x6f\x00\x00\x00H')
        time.sleep(.001 + iteration * .00017)
        send_frame(fd, b'\x03')
        time.sleep(.02)
        send_frame(fd, b'T')
        expect(b'DONE')
    send_frame(fd, b'P\x01\x00\x73\x00\x10\x00H')
    expect(b'DONE')
    print('PASS: invalid Q count, 10 cancellations, host diagnostics and final reload')
finally:
    os.close(fd)
