#!/usr/bin/env python3
"""Compile/run all eight RV32M ops against host-generated reference vectors.

Uses one UART session for repeated loads. The FPGA must already hold SimpleRisc.
"""
import argparse
import os
from pathlib import Path
import random
import select
import struct
import subprocess
import tempfile
import termios
import time
from run_program import frame_bytes, open_port

OPS = ['mul', 'mulh', 'mulhsu', 'mulhu', 'div', 'divu', 'rem', 'remu']
MASK = (1 << 32) - 1


def signed(n):
    return n if n < (1 << 31) else n - (1 << 32)


def reference(a, b):
    sa, sb = signed(a), signed(b)
    q = (-1 if (sa < 0) != (sb < 0) else 1) * (abs(sa) // abs(sb)) if b else -1
    return [x & MASK for x in [
        a * b, (sa * sb) >> 32, (sa * b) >> 32, (a * b) >> 32,
        q, a // b if b else MASK, sa - q * sb if b else sa, a % b if b else a]]


def build(directory):
    edges = [0, 1, 2, MASK, MASK - 1, 0x80000000, 0x7fffffff, 0xffff]
    rng = random.Random(960051)
    pairs = [(a,b) for a in edges for b in edges] + [(rng.getrandbits(32), rng.getrandbits(32)) for _ in range(128)]
    rows = ',\n'.join('{' + ','.join(f'0x{x:08x}u' for x in [a,b,*reference(a,b)]) + '}' for a,b in pairs)
    checks = '\n'.join(f'__asm__ volatile("{op} %0,%1,%2" : "=r"(got) : "r"(a),"r"(b));\n'
                       f'if (got != cases[i][{j+2}]) {{ uart_puts("M FAIL {op} row "); uart_put_uint(i); uart_putc(10); return 1; }}'
                       for j,op in enumerate(OPS))
    source = directory / 'rv32m_stress.c'
    source.write_text('#include "uart.h"\nstatic const uint32_t cases[][10] = {\n' + rows + '\n};\n'
                      'int main(void) { for (unsigned i=0;i<sizeof(cases)/sizeof(cases[0]);i++) {\n'
                      'uint32_t a=cases[i][0], b=cases[i][1], got;\n' + checks + '\n}\n'
                      'uart_puts("M STRESS HOLDS\\n"); return 0; }\n')
    runtime = Path(__file__).resolve().parent / 'c'
    elf = directory / 'rv32m_stress.elf'
    binary = directory / 'rv32m_stress.bin'
    subprocess.run(['riscv64-unknown-elf-gcc', '-march=rv32im', '-mabi=ilp32', '-Os', '-ffreestanding',
                    '-fno-builtin', '-nostdlib', '-nostartfiles', '-I', str(runtime), '-T', str(runtime/'link.ld'),
                    str(runtime/'crt0.S'), str(runtime/'uart.c'), str(source), '-o', str(elf)], check=True)
    subprocess.run(['riscv64-unknown-elf-objcopy', '-O', 'binary', str(elf), str(binary)], check=True)
    return binary.read_bytes(), len(pairs) * len(OPS)


def send(fd, data):
    deadline = time.monotonic() + 10
    while data:
        remaining = deadline - time.monotonic()
        if remaining <= 0 or not select.select([], [fd], [], remaining)[1]:
            raise TimeoutError('UART write timeout')
        try:
            data = data[os.write(fd, data):]
        except BlockingIOError:
            pass
    termios.tcdrain(fd)


def receive(fd, seconds, done=False):
    output = b''
    deadline = time.monotonic() + seconds
    while time.monotonic() < deadline:
        if select.select([fd], [], [], .02)[0]:
            output += os.read(fd, 4096)
        if done and output.endswith(b'DONE'):
            break
    return output


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--port', default='/dev/cu.usbserial-20250303171')
    parser.add_argument('--freq-mhz', type=float, default=96)
    parser.add_argument('--repeat', type=int, default=20)
    args = parser.parse_args()
    if args.repeat < 1 or args.freq_mhz <= 0:
        parser.error('repeat and frequency must be positive')
    with tempfile.TemporaryDirectory(prefix='simplerisc-m-stress-') as temp:
        image, count = build(Path(temp))
        fd = open_port(args.port, round(args.freq_mhz * 1e6 / 868))
        try:
            send(fd, b'\x03')
            receive(fd, .1)
            for i in range(args.repeat):
                send(fd, frame_bytes(image, b''))
                output = receive(fd, 10, done=True)
                assert output == b'M STRESS HOLDS\nDONE', (i+1, output)
                print(f'PASS run {i+1}: {count} RV32M reference checks', flush=True)
        finally:
            try:
                send(fd, b'\x03')
                receive(fd, .25)
            finally:
                os.close(fd)
    print(f'PASS: {args.repeat * count} hardware RV32M checks at {args.freq_mhz:g} MHz')


if __name__ == '__main__':
    main()
