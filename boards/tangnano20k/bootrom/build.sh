#!/usr/bin/env bash
# Build the SimpleRisc boot ROM and write bootrom.hex: exactly 512 32-bit
# words, one per line in hex, which src/SimpleRisc.hs embeds at compile time.
# The generated bootrom.hex is committed so building the core needs no gcc.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../../.." && pwd)"
out="$root/output/tangnano20k/bootrom"
mkdir -p "$out"
cross="${CROSS:-riscv64-unknown-elf-}"
"${cross}gcc" -march=rv32i_zicsr -mabi=ilp32 -Os -Wall -Wextra -ffreestanding -fno-builtin \
  -nostdlib -nostartfiles -msmall-data-limit=0 -fno-pic \
  -ffunction-sections -Wl,--gc-sections -Wl,--no-warn-rwx-segments -T "$here/link.ld" \
  "$here/crt0.S" "$here/bootrom.c" -o "$out/bootrom.elf"
"${cross}objcopy" -O binary "$out/bootrom.elf" "$out/bootrom.bin"
"${cross}objdump" -d "$out/bootrom.elf" > "$out/bootrom.lst"
python3 - "$out/bootrom.bin" "$here/bootrom.hex" <<'PY'
import struct, sys
image = open(sys.argv[1], "rb").read()
image += b"\0" * (-len(image) % 4)
words = struct.unpack(f"<{len(image) // 4}I", image)
if len(words) > 512:
    sys.exit(f"boot ROM is {len(words)} words; the ROM holds 512")
with open(sys.argv[2], "w") as f:
    f.writelines(f"{w:08x}\n" for w in list(words) + [0] * (512 - len(words)))
print(f"boot ROM: {len(image)} bytes of 2048")
PY
