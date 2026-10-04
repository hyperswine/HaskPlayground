#!/usr/bin/env python3
"""Check C arithmetic, UART echo, Ctrl-C recovery and RAM clearing on the board."""
import argparse
import os
import select
import struct
import subprocess
import termios
import time
from pathlib import Path
from run_program import frame_bytes, open_port


def main():
    root = Path(__file__).resolve().parents[2]
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--port', default='/dev/cu.usbserial-20250303171')
    parser.add_argument('--freq-mhz', type=float, default=96)
    args = parser.parse_args()
    if args.freq_mhz <= 0: parser.error('frequency must be positive')
    for program in ('hello', 'memory_stress'):
        subprocess.run([str(root / 'boards/tangnano20k/c/build.sh'), program], check=True)
    hello = (root / 'output/tangnano20k/c/hello.bin').read_bytes()
    expected = (b'hello from rv32im\n12345*6789=83810205\n4000000000/3=1333333333\n'
                b'-7/2=-3\n-7%2=-1\n7/0=-1\n7%0=7\nINT_MIN/-1=-2147483648\n'
                b'mulh(-7,2^31)=3\nDONE')
    fd = open_port(args.port, round(args.freq_mhz * 1e6 / 868))
    def send(data):
        while data:
            _, writable, _ = select.select([], [fd], [], 5)
            if not writable: raise TimeoutError('serial write timeout')
            data = data[os.write(fd, data):]
        termios.tcdrain(fd)
    def read_for(seconds, done=False):
        result = b''
        deadline = time.monotonic() + seconds
        while time.monotonic() < deadline:
            ready, _, _ = select.select([fd], [], [], min(.05, max(0, deadline-time.monotonic())))
            if ready: result += os.read(fd, 4096)
            if done and result.endswith(b'DONE'): break
        return result
    def check_program(name, image, want, input_bytes=b''):
        send(frame_bytes(image, b''))
        if input_bytes:
            time.sleep(.03)
            send(input_bytes)
        actual = read_for(5, True)
        print(f'{name}: {actual[:300]!r}', flush=True)
        assert actual == want, f'{name}: expected {want!r}'
        time.sleep(.01)
    try:
        send(b'\x03')
        read_for(.05)
        for i in range(3):
            check_program(f'C arithmetic run {i+1}', hello, expected)
        echo = struct.pack('<7I', 0x10000137, 0x00412183, 0x0021f193, 0xfe018ce3, 0x00812083, 0x00112023, 0x00000073)
        check_program('UART receive and echo', echo, b'ZDONE', b'Z')
        send(frame_bytes(struct.pack('<I', 0x0000006f), b''))
        assert read_for(.05) == b'', 'infinite loop unexpectedly halted'
        send(b'\x03')
        read_for(.05)
        check_program('Ctrl-C reset recovery', hello, expected)
        send(b'M')
        time.sleep(.03)
        send(b'R')
        assert read_for(.05) == b'', 'empty memory unexpectedly ran'
        check_program('Memory clear and reload', hello, expected)
        check_program('Byte/halfword/word stress', (root / 'output/tangnano20k/c/memory_stress.bin').read_bytes(), b'MEMORY HOLDS\nDONE')
        print('PASS: byte/halfword/word stress, 5 exact C arithmetic runs, UART RX/TX, Ctrl-C recovery, and memory clear/reload', flush=True)
    finally:
        try:
            send(b'\x03')
            read_for(.25)
        finally:
            os.close(fd)


if __name__ == "__main__":
    main()
