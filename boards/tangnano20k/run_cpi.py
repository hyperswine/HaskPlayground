#!/usr/bin/env python3
"""Run c/cpi.c on the board and print cycles per instruction per kernel."""
import argparse
import os
import subprocess
from pathlib import Path
from check_rv32m import send, receive
from run_program import frame_bytes, open_port


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--port', default='/dev/cu.usbserial-20250303171')
    parser.add_argument('--freq-mhz', type=float, default=96)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    subprocess.run([str(root / 'boards/tangnano20k/c/build.sh'), 'cpi'], check=True, stdout=subprocess.DEVNULL)
    image = (root / 'output/tangnano20k/c/cpi.bin').read_bytes()
    fd = open_port(args.port, round(args.freq_mhz * 1e6 / 868))
    try:
        send(fd, b'\x03')
        receive(fd, .1)
        send(fd, frame_bytes(image, b''))
        output = receive(fd, 20, done=True)
        print(output.decode(errors='replace'))
        if not output.endswith(b'DONE'):
            raise SystemExit('no DONE')
    finally:
        try:
            send(fd, b'\x03')
            receive(fd, .1)
        finally:
            os.close(fd)


if __name__ == '__main__':
    main()
