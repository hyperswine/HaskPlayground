#!/usr/bin/env bash
# Synthesise, place and route SimpleRisc for the Tang Nano 20K.
#
#   boards/tangnano20k/build.sh            build output/tangnano20k/simple_risc.fs
#   boards/tangnano20k/build.sh load       also load it into FPGA SRAM (not flash)
#
# Environment:
#   FREQ_MHZ  core clock, a multiple of 3 (PLL: 3 MHz x integer).  Default 96,
#             which passes every board test; 102 works with some placements
#             but not others, and 108 fails.
#             The UART then runs at FREQ_MHZ * 1e6 / 868 baud.
#   MARGIN    place and route for FREQ_MHZ * MARGIN (default 1.2).  nextpnr's
#             Gowin timing model is optimistic: designs it passed at 52 and
#             126 MHz failed on the chip at 51 and 108 MHz.
#   SEEDS     placement seeds to try, first passing one wins.  Default 1..8.
#   ALLOW_FAIL=1  write a bitstream even if timing fails (for overclocking
#             experiments on the board).
#   OSS_CAD_SUITE if the suite is not at ~/Documents/Libs/oss-cad-suite.
set -euo pipefail

freq="${FREQ_MHZ:-96}"
margin="${MARGIN:-1.2}"
seeds="${SEEDS:-1 2 3 4 5 6 7 8}"
allow_fail=()
if [[ "${ALLOW_FAIL:-}" == "1" ]]; then allow_fail=(--timing-allow-fail); fi
if (( freq % 3 != 0 )); then
  echo "FREQ_MHZ must be a multiple of 3" >&2
  exit 1
fi
fbdiv=$(( freq / 3 - 1 ))
odiv=""
for candidate in 128 112 96 80 64 48 32 16 8 4 2; do
  vco=$(( freq * candidate ))
  if (( vco >= 500 && vco <= 1250 )); then odiv=$candidate; break; fi
done
if [[ -z "$odiv" ]]; then
  echo "No PLL output divider keeps the VCO within 500-1250 MHz for $freq MHz" >&2
  exit 1
fi
target=$(python3 -c "print(round($freq * $margin, 2))")
echo "core clock $freq MHz (FBDIV_SEL=$fbdiv ODIV_SEL=$odiv), place and route for $target MHz, UART $(( freq * 1000000 / 868 )) baud"

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
  "read_verilog $core $here/top.v; chparam -set FBDIV_SEL $fbdiv -set ODIV_SEL $odiv top; synth_gowin -top top -json $out/simple_risc.json"

# Placement luck matters near the limit: keep the first seed that passes.
placed=""
for seed in $seeds; do
  if "$bin/nextpnr-himbaechel" \
    --json "$out/simple_risc.json" \
    --write "$out/simple_risc_pnr.json" \
    --device GW2AR-LV18QN88C8/I7 \
    --vopt family=GW2A-18C \
    --vopt cst="$here/tangnano20k.cst" \
    --freq "$target" \
    --seed "$seed" \
    ${allow_fail[@]+"${allow_fail[@]}"} \
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
