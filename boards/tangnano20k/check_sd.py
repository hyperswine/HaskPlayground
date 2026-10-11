#!/usr/bin/env python3
"""Check the microSD card over SimpleRisc's SPI master (c/sdtest.c).

Identifies the card, reads block 0, then writes, reads back and compares a
test block (64 MiB in) at SPI dividers 15, 7 and 3 (3, 6 and 12 MHz), and times block transfers.
The test block's previous contents are destroyed.
"""
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
    parser.add_argument('--repeat', type=int, default=3)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    subprocess.run([str(root / 'boards/tangnano20k/c/build.sh'), 'sdtest', 'sd'], check=True,
                   stdout=subprocess.DEVNULL)
    image = (root / 'output/tangnano20k/c/sdtest.bin').read_bytes()
    fd = open_port(args.port, round(args.freq_mhz * 1e6 / 868))
    try:
        send(fd, b'\x03')
        receive(fd, .1)
        for run in range(args.repeat):
            send(fd, frame_bytes(image, b''))
            output = receive(fd, 20, done=True)
            print(output.decode(errors='replace'), flush=True)
            if not output.endswith(b'SD HOLD\nDONE'):
                raise AssertionError(f'run {run + 1} failed')
            print(f'PASS run {run + 1}', flush=True)
    finally:
        try:
            send(fd, b'\x03')
            receive(fd, .1)
        finally:
            os.close(fd)


if __name__ == '__main__':
    main()
