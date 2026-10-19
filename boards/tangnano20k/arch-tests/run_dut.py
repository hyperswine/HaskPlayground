#!/usr/bin/env python3
"""Run one compiled arch test on a Verilator model of SimpleRisc.

Converts the ELF to a raw image at 0x8000_0000, looks up begin_signature and
end_signature, and runs the model, which writes the signature words. An
image that does not fit the model's RAM (less the marker word) or a run that
never halts leaves no signature, so RISCOF reports the test as failed.
"""
from pathlib import Path
import argparse
import subprocess

RAM = 0x8000_0000


def symbols(elf):
    out = subprocess.run(['riscv64-unknown-elf-nm', elf], capture_output=True, text=True, check=True).stdout
    return {name: int(value, 16) for value, _, name in (line.split() for line in out.splitlines() if len(line.split()) == 3)}


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('elf', type=Path)
    p.add_argument('signature', type=Path)
    p.add_argument('--sim', required=True)
    p.add_argument('--ram-bytes', type=lambda s: int(s, 0), required=True)
    p.add_argument('--limit', type=int, default=50_000_000)
    a = p.parse_args()
    a.signature.unlink(missing_ok=True)
    binary = a.elf.with_suffix('.bin')
    subprocess.run(['riscv64-unknown-elf-objcopy', '-O', 'binary', a.elf, binary], check=True)
    image = binary.read_bytes()
    image += b'\0' * (-len(image) % 4)
    if len(image) > a.ram_bytes - 4:
        raise SystemExit(f'ARCH TOO BIG image {len(image)} bytes, RAM {a.ram_bytes} bytes')
    hexfile = a.elf.with_suffix('.hex')
    hexfile.write_text('\n'.join(f'{int.from_bytes(image[i:i+4], "little"):08x}' for i in range(0, len(image), 4)) + '\n')
    s = symbols(a.elf)
    begin, end = ((s[name] - RAM) // 4 for name in ('begin_signature', 'end_signature'))
    run = subprocess.run([a.sim, f'+IMAGE={hexfile.resolve()}', f'+WORDS={len(image)//4}', f'+SIG_BEGIN={begin}',
                          f'+SIG_END={end}', f'+SIGNATURE={a.signature.resolve()}', f'+LIMIT={a.limit}'],
                         capture_output=True, text=True)
    print(run.stdout + run.stderr, end='')
    if run.returncode:
        raise SystemExit(f'simulator exit {run.returncode}')


if __name__ == '__main__':
    main()
