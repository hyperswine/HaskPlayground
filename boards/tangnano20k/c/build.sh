#!/usr/bin/env bash
# Build a C program for SimpleRisc (RV32IM, loaded at address 0).
#
#   boards/tangnano20k/c/build.sh hello      -> output/tangnano20k/c/hello.{elf,bin,lst}
#
# Uses the riscv64-unknown-elf-gcc toolchain's rv32im/ilp32 support; no libc,
# no libgcc (the CPU has the M extension, so there are no multiply/divide calls).
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../../.." && pwd)"
prog="${1:?usage: build.sh PROGRAM (a .c file in $here, without .c)}"
out="$root/output/tangnano20k/c"
mkdir -p "$out"
cc="${CROSS:-riscv64-unknown-elf-}gcc"
"$cc" -march=rv32im_zicsr -mabi=ilp32 -O2 -Wall -Wextra -ffreestanding -fno-builtin -nostdlib -nostartfiles \
  -ffunction-sections -fdata-sections -Wl,--gc-sections -Wl,--no-warn-rwx-segments -T "$here/link.ld" \
  "$here/crt0.S" "$here/uart.c" "$here/$prog.c" -o "$out/$prog.elf"
"${CROSS:-riscv64-unknown-elf-}objcopy" -O binary "$out/$prog.elf" "$out/$prog.bin"
"${CROSS:-riscv64-unknown-elf-}objdump" -d "$out/$prog.elf" > "$out/$prog.lst"
"${CROSS:-riscv64-unknown-elf-}size" "$out/$prog.elf"
echo "Image: $out/$prog.bin ($(wc -c < "$out/$prog.bin" | tr -d ' ') bytes)"
