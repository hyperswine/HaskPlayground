#!/usr/bin/env python3
"""Run the C CSR/trap/mret test repeatedly on the connected SimpleRisc board."""
import argparse
import os
from pathlib import Path
import subprocess
from check_rv32m import send, receive
from run_program import frame_bytes, open_port


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--port', default='/dev/cu.usbserial-20250303171')
    parser.add_argument('--freq-mhz', type=float, default=96)
    args = parser.parse_args()
    if args.freq_mhz <= 0:
        parser.error('frequency must be positive')
    root = Path(__file__).resolve().parents[2]
    subprocess.run([str(root / 'boards/tangnano20k/c/build.sh'), 'traps'], check=True)
    image = (root / 'output/tangnano20k/c/traps.bin').read_bytes()
    fd = open_port(args.port, round(args.freq_mhz * 1e6 / 868))
    try:
        send(fd, b'\x03')
        receive(fd, .1)
        for run in range(3):
            send(fd, frame_bytes(image, b''))
            output = receive(fd, 5, done=True)
            if not output.endswith(b'TRAPS HOLD 89\nDONE') or output.count(b'trap ') != 15:
                raise AssertionError(f'run {run + 1}: {output!r}')
            print(output.decode(), flush=True)
            print(f'PASS run {run + 1}: 89 guest CSR/exception/mret checks', flush=True)
    finally:
        try:
            send(fd, b'\x03')
            receive(fd, .1)
        finally:
            os.close(fd)


if __name__ == '__main__':
    main()
