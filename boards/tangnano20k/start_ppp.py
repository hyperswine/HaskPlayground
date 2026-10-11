#!/usr/bin/env python3
"""Load c/net.c and leave the port for pppd.

The program is passive (silent until the peer's first frame), so closing
the port here and opening it again in pppd is safe for the BL616 bridge.
Then, as root:

  sudo pppd /dev/cu.usbserial-20250303171 110599 10.0.0.1:10.0.0.2 \\
    noauth local nodetach nocrtscts novj noccp nopcomp noaccomp debug

and, in another terminal, `ping 10.0.0.2` or `nc -u 10.0.0.2 7`.
pppd's LCP terminate (Ctrl-C on pppd) ends the program.
"""
import argparse
import os
import subprocess
import time
from pathlib import Path
from check_rv32m import send, receive
from run_program import frame_bytes, open_port


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--port', default='/dev/cu.usbserial-20250303171')
    parser.add_argument('--freq-mhz', type=float, default=96)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    subprocess.run([str(root / 'boards/tangnano20k/c/build.sh'), 'net', 'sd'], check=True, stdout=subprocess.DEVNULL)
    image = (root / 'output/tangnano20k/c/net.bin').read_bytes()
    baud = round(args.freq_mhz * 1e6 / 868)
    fd = open_port(args.port, baud)
    try:
        send(fd, b'\x03')
        receive(fd, .1)
        send(fd, frame_bytes(image, b''))
        time.sleep(.3)
        stray = receive(fd, .2)
    finally:
        os.close(fd)
    if stray:
        raise SystemExit(f'the board answered unexpectedly: {stray!r}')
    print(f'net.c is running and silent; start pppd at {baud} baud (see --help)')


if __name__ == '__main__':
    main()
