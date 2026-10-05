#!/usr/bin/env python3
"""Check finisher termination independently of traps and end-of-image halting."""
import argparse
import os
import struct
from check_rv32m import send, receive
from run_program import frame_bytes, open_port


def load_constant(reg, value):
    upper = ((value + 0x800) >> 12) & 0xfffff
    return [(upper << 12) | (reg << 7) | 0x37,
            ((value & 0xfff) << 20) | (reg << 15) | (reg << 7) | 0x13]


def image(command, halfword=False, ignored=False):
    words = [0x001000b7, 0x08000193, 0x30519073]  # x1=finisher, mtvec=128
    if ignored:
        words += load_constant(2, 0x7777) + [0x0020a023]
    # LW must read zero; otherwise loop at the branch. Emit E before exiting.
    words += [0x0000a303, 0x00031063, 0x10000237, 0x04500293, 0x00522023]
    words += load_constant(2, command)
    words += [0x00209023 if halfword else 0x0020a023, 0x0000006f]
    # A trap loops forever, as does a finisher that failed to stop execution.
    words += [0x00000013] * (32 - len(words)) + [0x0000006f]
    return struct.pack('<' + 'I' * len(words), *words)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--port', default='/dev/cu.usbserial-20250303171')
    parser.add_argument('--freq-mhz', type=float, default=96)
    args = parser.parse_args()
    if args.freq_mhz <= 0:
        parser.error('frequency must be positive')
    cases = [('word success', 0x5555, False, False),
             ('word failure code 37', (37 << 16) | 0x3333, False, False),
             ('halfword success', 0x5555, True, False),
             ('halfword failure', 0x3333, True, False),
             ('ignored command then success', 0x5555, False, True)]
    fd = open_port(args.port, round(args.freq_mhz * 1e6 / 868))
    try:
        send(fd, b'\x03')
        receive(fd, .1)
        for repeat in range(3):
            for name, command, halfword, ignored in cases:
                send(fd, frame_bytes(image(command, halfword, ignored), b''))
                output = receive(fd, 3, done=True)
                if output != b'EDONE':
                    raise AssertionError(f'run {repeat + 1}, {name}: {output!r}')
                print(f'PASS run {repeat + 1}: {name}, nonzero trap vector', flush=True)
    finally:
        try:
            send(fd, b'\x03')
            receive(fd, .1)
        finally:
            os.close(fd)


if __name__ == '__main__':
    main()
