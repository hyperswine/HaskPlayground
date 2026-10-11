#!/usr/bin/env python3
"""Check timer, software and PLIC/UART interrupts on the connected SimpleRisc board.

c/interrupts.c checks the CLINT timer (32 periodic ticks awaited with WFI),
mip, interrupt priority, the PLIC claim/complete protocol, and then receives a
back-to-back line from its receive interrupt while main() is busy computing,
echoing it once complete. (Echoing concurrently with full-rate input loses an
echoed byte at the host's 32-byte write boundary even with a polling echo:
that is the USB bridge, not the core.)
"""
import argparse
import os
from pathlib import Path
import re
import subprocess
from check_rv32m import send, receive
from run_program import frame_bytes, open_port

LINE = b'Interrupt-driven echo: the quick brown fox jumps over 13 lazy dogs.'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--port', default='/dev/cu.usbserial-20250303171')
    parser.add_argument('--freq-mhz', type=float, default=96)
    parser.add_argument('--repeat', type=int, default=5)
    args = parser.parse_args()
    if args.freq_mhz <= 0 or args.repeat < 1:
        parser.error('frequency and repeat must be positive')
    root = Path(__file__).resolve().parents[2]
    subprocess.run([str(root / 'boards/tangnano20k/c/build.sh'), 'interrupts'], check=True)
    image = (root / 'output/tangnano20k/c/interrupts.bin').read_bytes()
    fd = open_port(args.port, round(args.freq_mhz * 1e6 / 868))
    try:
        send(fd, b'\x03')
        receive(fd, .1)
        for run in range(args.repeat):
            send(fd, frame_bytes(image, b''))
            ready = b''
            for _ in range(50):
                ready += receive(fd, .1)
                if ready.endswith(b'ECHO READY\n'):
                    break
            if ready != b'ECHO READY\n':
                raise AssertionError(f'run {run + 1}: {ready!r}')
            send(fd, LINE)
            output = receive(fd, 5, done=True)
            match = re.fullmatch(re.escape(LINE) + rb'\nRX (\d+)\nINTERRUPTS HOLD (\d+) 0\nDONE', output)
            if not match:
                raise AssertionError(f'run {run + 1}: {output!r}')
            print(f'PASS run {run + 1}: {int(match[2])} interrupt checks, '
                  f'{int(match[1])} bytes received by interrupt while busy', flush=True)
    finally:
        try:
            send(fd, b'\x03')
            receive(fd, .1)
        finally:
            os.close(fd)


if __name__ == '__main__':
    main()
