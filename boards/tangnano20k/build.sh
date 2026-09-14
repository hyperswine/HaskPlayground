#!/usr/bin/env bash
# Synthesise, place and route SimpleRisc for the Tang Nano 20K.
#
#   boards/tangnano20k/build.sh            build output/tangnano20k/simple_risc.fs
#   boards/tangnano20k/build.sh load       also load it into FPGA SRAM (not flash)
#
# Set OSS_CAD_SUITE if the suite is not at ~/Documents/Libs/oss-cad-suite.
set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
suite="${OSS_CAD_SUITE:-$HOME/Documents/Libs/oss-cad-suite}"
bin="$suite/bin"
out="$root/output/tangnano20k"
core="$out/clash/SimpleRisc.topEntity/simple_risc.v"

mkdir -p "$out"
cd "$root"

stack exec --package clash-ghc -- clash src/SimpleRisc.hs --verilog -outputdir "$out/clash"

"$bin/yosys" -q -l "$out/yosys.log" -p \
  "read_verilog $core $here/top.v; synth_gowin -top top -json $out/simple_risc.json"

# The design meets 51 MHz with only a few percent to spare, so placement luck
# matters: try a handful of seeds and keep the first that passes timing.
placed=""
for seed in 1 2 3 4 5 6 7 8; do
  if "$bin/nextpnr-himbaechel" \
    --json "$out/simple_risc.json" \
    --write "$out/simple_risc_pnr.json" \
    --device GW2AR-LV18QN88C8/I7 \
    --vopt family=GW2A-18C \
    --vopt cst="$here/tangnano20k.cst" \
    --freq 51 \
    --seed "$seed" \
    -l "$out/nextpnr.log" >/dev/null 2>&1; then
    placed="$seed"
    break
  fi
  echo "seed $seed: $(grep 'Max frequency' "$out/nextpnr.log" | tail -1 | sed 's/.*: //')"
done
if [[ -z "$placed" ]]; then
  echo "No seed met timing; see $out/nextpnr.log" >&2
  exit 1
fi
echo "seed $placed: $(grep 'Max frequency' "$out/nextpnr.log" | tail -1 | sed 's/.*: //')"

"$bin/gowin_pack" -d GW2A-18C -o "$out/simple_risc.fs" "$out/simple_risc_pnr.json"
echo "Bitstream: $out/simple_risc.fs"

if [[ "${1:-}" == "load" ]]; then
  "$bin/openFPGALoader" -b tangnano20k "$out/simple_risc.fs"
fi
