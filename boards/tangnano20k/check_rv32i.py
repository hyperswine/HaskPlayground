#!/usr/bin/env python3
"""Compare RV32I ALU operations, including every shift distance, on the board."""
import argparse
import os
from pathlib import Path
import random
import subprocess
import tempfile
from check_rv32m import MASK, signed, send, receive
from run_program import frame_bytes, open_port

OPS = ['add', 'sub', 'sll', 'srl', 'sra', 'slt', 'sltu', 'xor', 'or', 'and']


def reference(a, b):
    distance = b & 31
    return [x & MASK for x in [a+b, a-b, a << distance, a >> distance,
        signed(a) >> distance, int(signed(a) < signed(b)), int(a < b), a^b, a|b, a&b]]


def build(directory):
    edges = [0, 1, 2, MASK, MASK-1, 0x80000000, 0x7fffffff, 0x55555555]
    pairs = [(a, n) for a in edges for n in range(32)]
    rng = random.Random(108120)
    pairs += [(rng.getrandbits(32), rng.getrandbits(32)) for _ in range(128)]
    rows = ',\n'.join('{' + ','.join(f'0x{x:08x}u' for x in [a,b,*reference(a,b)]) + '}' for a,b in pairs)
    checks = '\n'.join(f'__asm__ volatile("{op} %0,%1,%2" : "=r"(got) : "r"(a),"r"(b));\n'
        f'if (got != cases[i][{j+2}]) {{ uart_puts("I FAIL {op} row "); uart_put_uint(i); uart_puts(" got "); uart_put_uint(got); uart_putc(10); return 1; }}'
        for j,op in enumerate(OPS))
    source = directory / 'rv32i_stress.c'
    source.write_text('#include "uart.h"\nstatic const uint32_t cases[][12] = {\n' + rows + '\n};\n'
        'int main(void) { for (unsigned i=0;i<sizeof(cases)/sizeof(cases[0]);i++) {\n'
        'uint32_t a=cases[i][0], b=cases[i][1], got;\n' + checks + '\n}\n'
        'uart_puts("I STRESS HOLDS\\n"); return 0; }\n')
    runtime = Path(__file__).resolve().parent / 'c'
    elf = directory / 'rv32i_stress.elf'
    binary = elf.with_suffix('.bin')
    subprocess.run(['riscv64-unknown-elf-gcc', '-march=rv32im', '-mabi=ilp32', '-Os', '-ffreestanding',
        '-fno-builtin', '-nostdlib', '-nostartfiles', '-I', str(runtime), '-T', str(runtime/'link.ld'),
        str(runtime/'crt0.S'), str(runtime/'uart.c'), str(source), '-o', str(elf)], check=True)
    subprocess.run(['riscv64-unknown-elf-objcopy', '-O', 'binary', str(elf), str(binary)], check=True)
    return binary.read_bytes(), len(pairs)*len(OPS)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--port', default='/dev/cu.usbserial-20250303171')
    parser.add_argument('--freq-mhz', type=float, default=96)
    parser.add_argument('--repeat', type=int, default=10)
    args = parser.parse_args()
    if args.repeat < 1 or args.freq_mhz <= 0: parser.error('repeat and frequency must be positive')
    with tempfile.TemporaryDirectory(prefix='simplerisc-i-stress-') as temp:
        image, count = build(Path(temp))
        fd = open_port(args.port, round(args.freq_mhz*1e6/868))
        try:
            send(fd, b'\x03')
            receive(fd, .1)
            for i in range(args.repeat):
                send(fd, frame_bytes(image, b''))
                output = receive(fd, 10, done=True)
                assert output == b'I STRESS HOLDS\nDONE', (i+1, output)
                print(f'PASS run {i+1}: {count} RV32I reference checks', flush=True)
        finally:
            try:
                send(fd, b'\x03')
                receive(fd, .25)
            finally:
                os.close(fd)
    print(f'PASS: {args.repeat*count} hardware RV32I checks at {args.freq_mhz:g} MHz')


if __name__ == '__main__':
    main()
