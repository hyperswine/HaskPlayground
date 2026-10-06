#!/usr/bin/env bash
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../../.." && pwd)"
mode="${1:-test}"
case "$mode" in
 test) top="$here/test_top.v"; rtl="";;
 core) top="$here/core_top.v";
   cd "$root"
   stack exec --package clash-ghc -- clash -isrc -hide-package haskplayground src/SdramSimpleRisc.hs --verilog -outputdir "$root/output/tangnano20k/sdram-generated/clash"
   rtl="$root/output/tangnano20k/sdram-generated/clash/SdramSimpleRisc.topEntity/sdram_simple_risc.v";;
 *) echo "usage: build.sh [test|core]" >&2; exit 1;;
esac
out="${SDRAM_OUT:-$root/output/tangnano20k/sdram-$mode}"
ide="/Applications/GowinIDE.app/Contents/Resources/Gowin_EDA/IDE"
mkdir -p "$out"
cat > "$out/build.tcl" <<TCL
set_device -name GW2AR-18C GW2AR-LV18QN88C8/I7
set_option -top_module top
set_option -output_base_name sdram_test
set_option -opt_goal timing
set_option -frequency 54
set_option -global_freq 54
set_option -num_critical_paths 20
add_file "$top"
add_file "$here/memory.v"
add_file "$here/sdram.v"
add_file "$here/pll.v"
add_file "$here/../tangnano20k.cst"
add_file "$out/clocks.sdc"
run all
exit
TCL
if [[ -n "$rtl" ]]; then
  python3 - "$out/build.tcl" "$rtl" <<'PYTHON'
from pathlib import Path
import sys
p=Path(sys.argv[1]);p.write_text(p.read_text().replace('run all', 'add_file "'+sys.argv[2]+'"\nrun all'))
PYTHON
fi
cat > "$out/clocks.sdc" <<'SDC'
create_clock -name clk_27m -period 37.037037 [get_ports {clk_27m}]
create_generated_clock -name clk_core -source [get_ports {clk_27m}] -multiply_by 2 [get_pins {pll/pll_s2/CLKOUT}]
create_generated_clock -name clk_sdram -source [get_ports {clk_27m}] -multiply_by 2 [get_pins {pll/pll_s2/CLKOUTP}]
SDC
export DYLD_LIBRARY_PATH="$ide/lib"
export DYLD_FRAMEWORK_PATH="$ide/lib"
cd "$out"
rm -f "$out/impl/pnr/sdram_test.tr"
"$ide/bin/gw_sh" build.tcl
python3 "$here/../check_gowin_timing.py" "$out/impl/pnr/sdram_test.tr" --freq-mhz 54
