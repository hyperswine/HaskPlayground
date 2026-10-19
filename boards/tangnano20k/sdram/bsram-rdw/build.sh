#!/usr/bin/env bash
# Build the BSRAM read-during-write probe: build.sh [DEPTH_BITS] (13 or 14).
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../../../.." && pwd)"
depth="${1:-13}"
out="${BSRAM_RDW_OUT:-$root/output/tangnano20k/bsram-rdw-d$depth}"
mkdir -p "$out"
grep -E '"(clk_27m|btn_s1|uart_rx|uart_tx)"' "$here/../../tangnano20k.cst" > "$out/pins.cst"
freq="${BSRAM_RDW_MHZ:-54}"
baud=$((freq*1000000/115200-1))
case "$freq" in 27) fb=8;od=32;; 54) fb=17;od=16;; *) echo "BSRAM_RDW_MHZ must be 27 or 54" >&2;exit 1;; esac
sed "s/PSDA_SEL(\"1010\")/PSDA_SEL(\"0110\")/; s/FBDIV_SEL=17/FBDIV_SEL=$fb/; s/ODIV_SEL=16/ODIV_SEL=$od/" "$here/../pll.v" > "$out/pll.v"
sed "s/parameter DEPTH_BITS=13/parameter DEPTH_BITS=$depth/; s/parameter GAP=0/parameter GAP=${BSRAM_RDW_GAP:-0}/; s/BAUD_DIV=468/BAUD_DIV=$baud/" "$here/bsram_rdw_top.v" > "$out/top.v"
cat > "$out/clocks.sdc" <<SDC
create_clock -name clk_27m -period 37.037037 [get_ports {clk_27m}]
create_generated_clock -name clk_core -source [get_ports {clk_27m}] -multiply_by $((freq/3)) -divide_by 9 [get_pins {pll/pll_s2/CLKOUT}]
SDC
cat > "$out/build.tcl" <<TCL
set_device -name GW2AR-18C GW2AR-LV18QN88C8/I7
set_option -top_module top
set_option -output_base_name bsram_rdw
set_option -gen_verilog_sim_netlist 1
add_file "$out/top.v"
add_file "$out/pll.v"
add_file "$out/pins.cst"
add_file "$out/clocks.sdc"
run all
exit
TCL
ide="/Applications/GowinIDE.app/Contents/Resources/Gowin_EDA/IDE"
export DYLD_LIBRARY_PATH="$ide/lib" DYLD_FRAMEWORK_PATH="$ide/lib"
cd "$out" && "$ide/bin/gw_sh" build.tcl
