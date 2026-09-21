#!/usr/bin/env bash
# Synthesise, place and route PsramRegs for the Tang Nano 20K.
#
#   boards/tangnano20k/build_psram.sh        build output/tangnano20k/psram/psram_regs.fs
#   boards/tangnano20k/build_psram.sh load   also load it into FPGA SRAM (not flash)
#
# The core runs at the board's 27 MHz; nextpnr places and routes for 27 MHz
# x MARGIN (default 1.25, see build.sh for why).
# OSS_CAD_SUITE if the suite is not at ~/Documents/Libs/oss-cad-suite.
set -euo pipefail

margin="${MARGIN:-1.25}"
target=$(python3 -c "print(round(27 * $margin, 2))")

here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
suite="${OSS_CAD_SUITE:-$HOME/Documents/Libs/oss-cad-suite}"
bin="$suite/bin"
out="$root/output/tangnano20k/psram"
core="$out/clash/PsramRegs.topEntity/psram_regs.v"

mkdir -p "$out"
cd "$root"

stack exec --package clash-ghc -- clash src/PsramRegs.hs --verilog -outputdir "$out/clash"

"$bin/yosys" -q -l "$out/yosys.log" -p \
  "read_verilog $core $here/psram_top.v; synth_gowin -top top -json $out/psram_regs.json"

"$bin/nextpnr-himbaechel" \
  --json "$out/psram_regs.json" \
  --write "$out/psram_regs_pnr.json" \
  --device GW2AR-LV18QN88C8/I7 \
  --vopt family=GW2A-18C \
  --vopt cst="$here/tangnano20k.cst" \
  --freq "$target" \
  -l "$out/nextpnr.log" >/dev/null 2>&1 || {
  echo "place and route failed; see $out/nextpnr.log" >&2
  exit 1
}
echo "$(grep 'Max frequency' "$out/nextpnr.log" | tail -1 | sed 's/.*Info: *//')"
grep -A12 'Device utilisation' "$out/nextpnr.log" | grep -E 'LUT4|DFF|BSRAM|ALU' | sed 's/.*Info: *//'

"$bin/gowin_pack" -d GW2A-18C -o "$out/psram_regs.fs" "$out/psram_regs_pnr.json"
echo "Bitstream: $out/psram_regs.fs"

if [[ "${1:-}" == "load" ]]; then
  "$bin/openFPGALoader" -b tangnano20k "$out/psram_regs.fs"
fi
