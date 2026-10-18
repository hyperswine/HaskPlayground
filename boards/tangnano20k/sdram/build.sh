#!/usr/bin/env bash
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../../.." && pwd)"
mode="${1:-test}"
freq="${SDRAM_FREQ_MHZ:-54}"
cpu_freq="${SDRAM_CPU_FREQ_MHZ:-$freq}"
cache="${SDRAM_CACHE:-1}"
rvc="${SDRAM_RVC:-0}"
placement="${SDRAM_PLACE_OPTION:-2}"
case "$placement" in 0|1|2|3|4) ;; *) echo "SDRAM_PLACE_OPTION must be 0 through 4" >&2;exit 1;; esac
case "$rvc" in 0|1) ;; *) echo "SDRAM_RVC must be 0 or 1" >&2;exit 1;; esac
case "$cache" in 0|1) ;; *) echo "SDRAM_CACHE must be 0 or 1" >&2;exit 1;; esac
case "$freq" in 27|48|54|60|66) ;; *) echo "supported SDRAM clocks: 27, 48, 54, 60, 66 MHz" >&2;exit 1;; esac
if [[ "$cpu_freq" != "$freq" ]];then
  if [[ ( "$cpu_freq" != 108 && "$cpu_freq" != 72 ) || "$freq" != 54 || ( "$mode" != core && "$mode" != pipeline ) ]];then
    echo "separate clocks require pipeline/core, SDRAM_CPU_FREQ_MHZ=72 or 108 and SDRAM_FREQ_MHZ=54" >&2;exit 1
  fi
fi
# Audit the memory specification before expensive CPU generation. Ordinary
# builds still check internal STA only; an explicit I/O profile is fail-closed.
case "$mode" in test|serial) phase=225;burst_words=1;; core|pipeline|burst-test) phase=135;burst_words=4;;
 *) echo "usage: build.sh [test|burst-test|core|pipeline|serial]" >&2;exit 1;;esac
phase="${SDRAM_PHASE_DEGREES:-$phase}"
phase="$(python3 - "$phase" <<'PYTHON'
from decimal import Decimal, InvalidOperation
import sys
try:
    value = Decimal(sys.argv[1])
    if not value.is_finite() or not 0 <= value < 360 or value % Decimal('22.5'):
        raise ValueError()
except (InvalidOperation, ValueError):
    raise SystemExit('SDRAM_PHASE_DEGREES must be 0 through 337.5 in 22.5-degree steps')
print(format(value.normalize(), 'f'))
PYTHON
)"
out="${SDRAM_OUT:-$root/output/tangnano20k/sdram-$mode}"
if [[ "$out" != /* ]];then out="$PWD/$out";fi
case "${SDRAM_REQUIRE_IO_MODEL:-0}" in 0|1) ;; *) echo "SDRAM_REQUIRE_IO_MODEL must be 0 or 1" >&2;exit 1;;esac
io_args=(--freq-mhz "$freq" --phase-degrees "$phase" --burst-words "$burst_words" --out "$out")
if [[ "$cpu_freq" != "$freq" ]];then io_args+=(--split-clock);fi
if [[ -n "${SDRAM_IO_PROFILE:-}" ]];then
  io_profile="$SDRAM_IO_PROFILE"
  if [[ "$io_profile" != /* ]];then io_profile="$root/$io_profile";fi
  io_args+=(--profile "$io_profile" --emit-sdc)
elif [[ "${SDRAM_REQUIRE_IO_MODEL:-0}" == 1 ]];then
  io_args+=(--emit-sdc)
fi
python3 "$here/io_timing.py" "${io_args[@]}"
case "$mode" in
 test|burst-test) top="$here/test_top.v"; rtl="";;
 core|pipeline|serial)
   cd "$root"
   if [[ "$mode" == serial ]];then
     module=SdramSimpleRisc;entity=sdram_simple_risc;top="$here/core_top.v"
   else
     module=SdramPipeline;entity=sdram_pipeline_risc;top="$here/pipeline_top.v"
   fi
   generated="$root/output/tangnano20k/sdram-generated/$module-$rvc"
   options=(-isrc -hide-package haskplayground);if [[ "$rvc" == 1 ]];then options+=(-DRISCV_COMPRESSED);fi
   if [[ -n "${SDRAM_RTL_FILE:-}" ]];then
     rtl="$SDRAM_RTL_FILE"
     if [[ "$rtl" != /* ]];then rtl="$root/$rtl";fi
     if [[ ! -f "$rtl" ]];then echo "SDRAM_RTL_FILE does not exist: $rtl" >&2;exit 1;fi
   else
     cabal run -v0 clash -- "${options[@]}" "src/$module.hs" --verilog -fclash-clear -outputdir "$generated"
     rtl="$generated/$module.topEntity/$entity.v"
   fi;;
 *) echo "usage: build.sh [test|burst-test|core|pipeline|serial]" >&2; exit 1;;
esac
ide="/Applications/GowinIDE.app/Contents/Resources/Gowin_EDA/IDE"
mkdir -p "$out"
python3 - "$here" "$out" "$freq" "$mode" "$cache" "$phase" "$cpu_freq" <<'PYTHON'
from pathlib import Path
import sys
src,out=map(Path,sys.argv[1:3]);freq=int(sys.argv[3])
cpu_freq=int(sys.argv[7])
# Only the pipeline top exposes the SD-card slot; the other images retain
# their original port list.
sd_slot=sys.argv[4] in ('core','pipeline')
(out/'pins.cst').write_text(''.join(line for line in (src.parent/'tangnano20k.cst').read_text().splitlines(True) if sd_slot or '"sd_' not in line))
if sys.argv[4] in ('core','pipeline','serial'):
    pipelined=sys.argv[4]!='serial'
    top=(src/('pipeline_top.v' if pipelined else 'core_top.v')).read_text()
    top=top.replace('BOOT_ROM_FILE',str(src.parent/'bootrom/bootrom-sdram.hex')).replace('CORE_FREQUENCY',str(freq*1000000)).replace('CPU_FREQUENCY',str(cpu_freq*1000000))
    if cpu_freq!=freq:
        top=top.replace('sdram_pll pll(.clk_27m(clk_27m),.clk_core(clk),.clk_sdram(clk_sdram),.pll_lock(lock));',
                        f'sdram_dual_pll #(.CPU_MHZ({cpu_freq})) pll(.clk_27m(clk_27m),.clk_core(clk),.clk_memory(clk_memory),.clk_sdram(clk_sdram),.pll_lock(lock));')
        top=top.replace(' assign clk_memory=clk; // SINGLE_CLOCK_MEMORY\n','')
    if pipelined and sys.argv[5]=='0':raise SystemExit('the pipeline requires its fixed-latency cache; use serial with SDRAM_CACHE=0')
    if not pipelined and sys.argv[5]=='0':
        start=top.index(' sdram_cache cache(');end=top.index(' sdram_memory mem(')
        top=top[:start]+''' assign mem_req=req;assign mem_write=write;assign mem_address=address;
 assign mem_wdata=wdata;assign ready=mem_ready;assign done=mem_done;assign rdata=mem_rdata;
'''+top[end:]
    (out/(sys.argv[4]+'_top.v')).write_text(top)
if sys.argv[4] in ('test','burst-test'):
    top=(src/'test_top.v').read_text().replace("24'd2700000",f"24'd{freq*50000}").replace("25'd13500000",f"25'd{freq*250000}")
    if sys.argv[4]=='burst-test':top=top.replace('sdram_memory mem(',f'sdram_burst_word_memory #(.FREQ({freq*1000000})) mem(')
    (out/(sys.argv[4]+'_top.v')).write_text(top)
pll=(src/'pll.v').read_text().replace('FBDIV_SEL=17',f'FBDIV_SEL={freq//3-1}').replace('ODIV_SEL=16','ODIV_SEL=32' if freq==27 else 'ODIV_SEL=16')
phase_bits=format(int(float(sys.argv[6])/22.5),'04b')
pll=pll.replace('PSDA_SEL("1010")',f'PSDA_SEL("{phase_bits}")')
(out/'pll.v').write_text(pll)
(out/'memory.v').write_text((src/'memory.v').read_text().replace('FREQ=54000000',f'FREQ={freq*1000000}'))
PYTHON
top="$out/${mode}_top.v"
cat > "$out/build.tcl" <<TCL
set_device -name GW2AR-18C GW2AR-LV18QN88C8/I7
set_option -top_module top
set_option -output_base_name sdram_test
set_option -opt_goal timing
set_option -place_option $placement
set_option -frequency $cpu_freq
set_option -global_freq $cpu_freq
set_option -num_critical_paths 20
set_option -gen_verilog_sim_netlist 1
add_file "$top"
add_file "$out/memory.v"
add_file "$here/cache.v"
add_file "$here/pipeline_cache.v"
add_file "$here/burst_memory.v"
add_file "$here/clock_bridge.v"
add_file "$here/sdram.v"
add_file "$out/pll.v"
add_file "$here/dual_pll.v"
add_file "$out/pins.cst"
add_file "$out/clocks.sdc"
file delete -force "$out/impl/pnr/sdram_test.tr"
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
if [[ "$cpu_freq" == "$freq" ]];then
cat > "$out/clocks.sdc" <<SDC
create_clock -name clk_27m -period 37.037037 [get_ports {clk_27m}]
create_generated_clock -name clk_core -source [get_ports {clk_27m}] -multiply_by $((freq/3)) -divide_by 9 [get_pins {pll/pll_s2/CLKOUT}]
create_generated_clock -name clk_sdram -source [get_ports {clk_27m}] -multiply_by $((freq/3)) -divide_by 9 -phase $phase [get_pins {pll/pll_s2/CLKOUTP}]
SDC
else
cat > "$out/clocks.sdc" <<SDC
create_clock -name clk_27m -period 37.037037 [get_ports {clk_27m}]
create_generated_clock -name clk_core -source [get_ports {clk_27m}] -multiply_by $((cpu_freq/3)) -divide_by 9 [get_pins {pll/cpu/pll/CLKOUT}]
create_generated_clock -name clk_memory -source [get_ports {clk_27m}] -multiply_by 2 [get_pins {pll/memory/pll/CLKOUT}]
create_generated_clock -name clk_sdram -source [get_ports {clk_27m}] -multiply_by 2 -phase $phase [get_pins {pll/memory/pll/CLKOUTP}]
# Only the first metastability stages are asynchronous endpoints. Bundled
# payload buses retain normal setup/hold analysis; no clock groups or blanket
# cross-domain false paths conceal their physical routing delays.
set_false_path -to [get_regs {dual_clock.bridge/request_meta_s0}]
set_false_path -to [get_regs {dual_clock.bridge/response_meta_s0}]
SDC
fi
if [[ -f "$out/sdram-io.sdc" ]];then
  cat "$out/sdram-io.sdc" >> "$out/clocks.sdc"
  echo "SDRAM I/O bounds supplied; routed path coverage and protocol checks are still required."
else
  echo "SDRAM I/O remains unqualified: this build checks internal FPGA timing only."
fi
if [[ "${SDRAM_GENERATE_ONLY:-0}" == 1 ]];then
  echo "Generated $out: CPU/cache $cpu_freq MHz, SDRAM $freq MHz, phase $phase degrees"
  exit 0
fi
export DYLD_LIBRARY_PATH="$ide/lib"
export DYLD_FRAMEWORK_PATH="$ide/lib"
cd "$out"
rm -f "$out/impl/pnr/sdram_test.tr"
"$ide/bin/gw_sh" build.tcl
timing_args=(--freq-mhz "$cpu_freq")
if [[ "$cpu_freq" != "$freq" ]];then timing_args+=(--memory-freq-mhz "$freq");fi
python3 "$here/../check_gowin_timing.py" "$out/impl/pnr/sdram_test.tr" "${timing_args[@]}"
