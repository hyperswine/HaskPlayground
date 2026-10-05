#!/usr/bin/env python3
"""Check mixed-width bus traffic, RX sign extension and single consumption."""
import argparse
import os
from pathlib import Path
import select
import subprocess
import time
from check_rv32m import send, receive
from run_program import frame_bytes, open_port


def marker(fd, expected):
    deadline = time.monotonic() + 5
    while time.monotonic() < deadline:
        ready, _, _ = select.select([fd], [], [], .05)
        if ready:
            actual = os.read(fd, 4096)
            if actual != expected:
                raise AssertionError(f'expected marker {expected!r}, got {actual!r}')
            return
    raise TimeoutError(f'waiting for {expected!r}')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--port', default='/dev/cu.usbserial-20250303171')
    parser.add_argument('--freq-mhz', type=float, default=96)
    args = parser.parse_args()
    if args.freq_mhz <= 0:
        parser.error('frequency must be positive')
    root = Path(__file__).resolve().parents[2]
    subprocess.run([str(root / 'boards/tangnano20k/c/build.sh'), 'bus'], check=True)
    image = (root / 'output/tangnano20k/c/bus.bin').read_bytes()
    fd = open_port(args.port, round(args.freq_mhz * 1e6 / 868))
    try:
        send(fd, b'\x03')
        receive(fd, .1)
        for run in range(3):
            send(fd, frame_bytes(image, b''))
            marker(fd, b'S')
            send(fd, b'\x81')
            marker(fd, b'U')
            send(fd, b'\xfe')
            output = receive(fd, 5, done=True)
            if output != b'BUS HOLD 1284\nDONE':
                raise AssertionError(f'run {run + 1}: {output!r}')
            print(f'PASS run {run + 1}: 1284 RAM/RX bus checks', flush=True)
    finally:
        try:
            send(fd, b'\x03')
            receive(fd, .1)
        finally:
            os.close(fd)


if __name__ == '__main__':
    main()
