#!/usr/bin/env bash
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../../.." && pwd)"
mode="${1:-test}"
freq="${SDRAM_FREQ_MHZ:-54}"
cache="${SDRAM_CACHE:-1}"
case "$cache" in 0|1) ;; *) echo "SDRAM_CACHE must be 0 or 1" >&2;exit 1;; esac
case "$freq" in 27|54|60|66) ;; *) echo "supported SDRAM clocks: 27, 54, 60, 66 MHz" >&2;exit 1;; esac
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
python3 - "$here" "$out" "$freq" "$mode" "$cache" <<'PYTHON'
from pathlib import Path
import sys
src,out=map(Path,sys.argv[1:3]);freq=int(sys.argv[3])
if sys.argv[4]=='core':
    top=(src/'core_top.v').read_text()
    if sys.argv[5]=='0':
        start=top.index(' sdram_cache cache(');end=top.index(' sdram_memory mem(')
        top=top[:start]+''' assign mem_req=req;assign mem_write=write;assign mem_address=address;
 assign mem_wdata=wdata;assign ready=mem_ready;assign done=mem_done;assign rdata=mem_rdata;
'''+top[end:]
    (out/'core_top.v').write_text(top)
if sys.argv[4]=='test':
    (out/'test_top.v').write_text((src/'test_top.v').read_text().replace("24'd2700000",f"24'd{freq*50000}").replace("25'd13500000",f"25'd{freq*250000}"))
(out/'pll.v').write_text((src/'pll.v').read_text().replace('FBDIV_SEL=17',f'FBDIV_SEL={freq//3-1}').replace('ODIV_SEL=16','ODIV_SEL=32' if freq==27 else 'ODIV_SEL=16'))
(out/'memory.v').write_text((src/'memory.v').read_text().replace('FREQ=54000000',f'FREQ={freq*1000000}'))
PYTHON
top="$out/${mode}_top.v"
cat > "$out/build.tcl" <<TCL
set_device -name GW2AR-18C GW2AR-LV18QN88C8/I7
set_option -top_module top
set_option -output_base_name sdram_test
set_option -opt_goal timing
set_option -frequency $freq
set_option -global_freq $freq
set_option -num_critical_paths 20
add_file "$top"
add_file "$out/memory.v"
add_file "$here/cache.v"
add_file "$here/sdram.v"
add_file "$out/pll.v"
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
cat > "$out/clocks.sdc" <<SDC
create_clock -name clk_27m -period 37.037037 [get_ports {clk_27m}]
create_generated_clock -name clk_core -source [get_ports {clk_27m}] -multiply_by $((freq/3)) -divide_by 9 [get_pins {pll/pll_s2/CLKOUT}]
create_generated_clock -name clk_sdram -source [get_ports {clk_27m}] -multiply_by $((freq/3)) -divide_by 9 [get_pins {pll/pll_s2/CLKOUTP}]
SDC
export DYLD_LIBRARY_PATH="$ide/lib"
export DYLD_FRAMEWORK_PATH="$ide/lib"
cd "$out"
rm -f "$out/impl/pnr/sdram_test.tr"
"$ide/bin/gw_sh" build.tcl
python3 "$here/../check_gowin_timing.py" "$out/impl/pnr/sdram_test.tr" --freq-mhz "$freq"
