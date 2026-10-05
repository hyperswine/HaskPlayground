#!/usr/bin/env python3
"""Check exception termination and legal access controls on the board.

This first roadmap slice has no software-visible CSRs yet. Exact cause, EPC,
trap value and absence of memory side effects are checked by Haskell properties;
this hardware test checks that faults stop before the UART failure marker.
"""
import argparse
import os
import struct
from check_rv32m import send, receive
from run_program import frame_bytes, open_port


def load(width):
    return (1 << 15) | (width << 12) | (5 << 7) | 0x03


def store(width):
    return (2 << 20) | (1 << 15) | (width << 12) | 0x23


def image(address, instruction):
    # x1 = address, x2 = 0x21, x3 = UART. Print '!' only if fault returns normally.
    upper = ((address + 0x800) >> 12) & 0xfffff
    lower = address & 0xfff
    words = [(upper << 12) | (1 << 7) | 0x37,
             (lower << 20) | (1 << 15) | (1 << 7) | 0x13,
             0x02100113, 0x100001b7, instruction, 0x0021a023, 0x00000073]
    return struct.pack('<7I', *words)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--port', default='/dev/cu.usbserial-20250303171')
    parser.add_argument('--freq-mhz', type=float, default=96)
    args = parser.parse_args()
    if args.freq_mhz <= 0:
        parser.error('frequency must be positive')
    cases = []
    for width, name in [(1, 'lh'), (2, 'lw'), (5, 'lhu')]:
        for offset in ([1, 2, 3] if width == 2 else [1, 3]):
            cases.append((f'{name} misaligned {offset}', 0x200 + offset, load(width), b'DONE'))
    for width, name in [(1, 'sh'), (2, 'sw')]:
        for offset in ([1, 2, 3] if width == 2 else [1, 3]):
            cases.append((f'{name} misaligned {offset}', 0x200 + offset, store(width), b'DONE'))
    cases += [('load access fault', 0x10000, load(2), b'DONE'),
              ('store access fault', 0x10000, store(2), b'DONE'),
              ('invalid UART load encoding', 0x10000008, load(3), b'DONE'),
              ('invalid UART store encoding', 0x10000000, store(3), b'DONE'),
              ('illegal instruction', 0, 0xffffffff, b'DONE'),
              ('ebreak', 0, 0x00100073, b'DONE'),
              ('ecall', 0, 0x00000073, b'DONE'),
              ('jal misaligned target', 0, 0x002002ef, b'DONE'),
              ('jalr misaligned target', 6, 0x000082e7, b'DONE'),
              ('taken branch misaligned target', 0, 0x00011163, b'DONE'),
              ('untaken misaligned branch control', 0, 0x00001163, b'!DONE')]
    for width, name in [(0, 'lb'), (1, 'lh'), (2, 'lw'), (4, 'lbu'), (5, 'lhu')]:
        cases.append((f'{name} legal control', 0x201 if width in (0, 4) else 0x200, load(width), b'!DONE'))
    for width, name in [(0, 'sb'), (1, 'sh'), (2, 'sw')]:
        cases.append((f'{name} legal control', 0x201 if width == 0 else 0x200, store(width), b'!DONE'))
    fd = open_port(args.port, round(args.freq_mhz * 1e6 / 868))
    try:
        send(fd, b'\x03')
        receive(fd, .1)
        for name, address, instruction, expected in cases:
            send(fd, frame_bytes(image(address, instruction), b''))
            actual = receive(fd, 3, done=True)
            if actual != expected:
                raise AssertionError(f'{name}: expected {expected!r}, got {actual!r}')
            print(f'PASS: {name}', flush=True)
        print(f'PASS: {len(cases)} exception/encoding and legal access checks at {args.freq_mhz:g} MHz')
    finally:
        try:
            send(fd, b'\x03')
            receive(fd, .1)
        finally:
            os.close(fd)


if __name__ == '__main__':
    main()
