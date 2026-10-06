#!/usr/bin/env bash
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../../.." && pwd)"
freq="${RV64_FREQ_MHZ:-27}"
case "$freq" in 27|36|54) ;; *) echo "RV64 clocks: 27, 36, 54 MHz" >&2;exit 1;; esac
out="${RV64_OUT:-$root/output/tangnano20k/rv64-$freq}"
mkdir -p "$out"
python3 - "$here/../sdram" "$out" "$freq" <<'PY'
from pathlib import Path
import sys
src,out=map(Path,sys.argv[1:3]);f=int(sys.argv[3])
(out/'pll.v').write_text((src/'pll.v').read_text().replace('FBDIV_SEL=17',f'FBDIV_SEL={f//3-1}').replace('ODIV_SEL=16','ODIV_SEL=32' if f==27 else 'ODIV_SEL=16'))
(out/'memory.v').write_text((src/'memory.v').read_text().replace('FREQ=54000000',f'FREQ={f*1000000}'))
PY
cat > "$out/clocks.sdc" <<SDC
create_clock -name clk_27m -period 37.037037 [get_ports {clk_27m}]
create_generated_clock -name clk_core -source [get_ports {clk_27m}] -multiply_by $((freq/3)) -divide_by 9 [get_pins {pll/pll_s2/CLKOUT}]
create_generated_clock -name clk_sdram -source [get_ports {clk_27m}] -multiply_by $((freq/3)) -divide_by 9 [get_pins {pll/pll_s2/CLKOUTP}]
SDC
cat > "$out/build.tcl" <<TCL
set_device -name GW2AR-18C GW2AR-LV18QN88C8/I7
set_option -top_module top
set_option -output_base_name rv64
set_option -opt_goal timing
set_option -frequency $freq
set_option -global_freq $freq
set_option -num_critical_paths 20
add_file "$here/top.v"
add_file "$here/core.v"
add_file "$here/uart.v"
add_file "$out/memory.v"
add_file "$out/pll.v"
add_file "$here/../sdram/sdram.v"
add_file "$here/../sdram/cache.v"
add_file "$here/../tangnano20k.cst"
add_file "$out/clocks.sdc"
run all
exit
TCL
ide=/Applications/GowinIDE.app/Contents/Resources/Gowin_EDA/IDE
export DYLD_LIBRARY_PATH="$ide/lib" DYLD_FRAMEWORK_PATH="$ide/lib"
cd "$out"
rm -f impl/pnr/rv64.tr
"$ide/bin/gw_sh" build.tcl
python3 "$here/../check_gowin_timing.py" impl/pnr/rv64.tr --freq-mhz "$freq"
