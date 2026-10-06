#!/usr/bin/env bash
# Vendor analysis; FREQ_MHZ is a multiple of 3 (default 96). Does not program the board.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
ide="${GOWIN_IDE:-/Applications/GowinIDE.app/Contents/Resources/Gowin_EDA/IDE}"
freq="${FREQ_MHZ:-96}"
placement="${GOWIN_PLACE_OPTION:-0}"
if ! [[ "$placement" =~ ^[0-4]$ ]]; then
  echo "GOWIN_PLACE_OPTION must be 0, 1, 2, 3 or 4" >&2; exit 1
fi
if ! [[ "$freq" =~ ^[0-9]+$ ]] || (( freq < 3 || freq % 3 != 0 )); then
  echo "FREQ_MHZ must be a positive integer multiple of 3" >&2; exit 1
fi
odiv=""
for candidate in 128 112 96 80 64 48 32 16 8 4 2; do
  vco=$(( freq * candidate ))
  if (( vco >= 500 && vco <= 1250 )); then odiv=$candidate; break; fi
done
if [[ -z "$odiv" ]]; then echo "No supported PLL VCO for $freq MHz" >&2; exit 1; fi
out="${GOWIN_OUT:-$root/output/tangnano20k/gowin-$freq}"
echo "Gowin vendor build: core $freq MHz, VCO $((freq * odiv)) MHz, output $out"
mkdir -p "$out"
cd "$root"
stack exec --package clash-ghc -- clash src/SimpleRisc.hs --verilog -outputdir "$root/output/tangnano20k/clash"
# The vendor PLL library uses GW2AR-18C for this device.
python3 - "$here/top.v" "$out/top_vendor.v" "$freq" "$odiv" <<'PY'
from pathlib import Path
import sys
s = Path(sys.argv[1]).read_text().replace('.DEVICE("GW2A-18C")', '.DEVICE("GW2AR-18C")')
s = s.replace('FBDIV_SEL = 31', f'FBDIV_SEL = {int(sys.argv[3]) // 3 - 1}')
s = s.replace('ODIV_SEL  = 8', f'ODIV_SEL  = {sys.argv[4]}')
Path(sys.argv[2]).write_text(s)
PY
python3 - "$out/clocks.sdc" "$freq" <<'PY'
from pathlib import Path
import sys
Path(sys.argv[1]).write_text('create_clock -name clk_27m -period 37.037037 [get_ports {clk_27m}]\n'
    f'create_generated_clock -name clk_core -source [get_ports {{clk_27m}}] -multiply_by {int(sys.argv[2]) // 3} -divide_by 9 [get_pins {{pll/CLKOUT}}]\n')
PY
cat > "$out/build.tcl" <<'TCL'
set_device -name GW2AR-18C GW2AR-LV18QN88C8/I7
set_option -top_module top
set_option -output_base_name simple_risc
set_option -looplimit 20000
set_option -frequency 96
set_option -global_freq 96
set_option -opt_goal timing
set_option -max_fanout 1000
set_option -place_option 0
set_option -num_critical_paths 20
add_file ../clash/SimpleRisc.topEntity/simple_risc.v
add_file top_vendor.v
add_file ../../../boards/tangnano20k/tangnano20k.cst
add_file clocks.sdc
run all
exit
TCL
python3 - "$out/build.tcl" "$freq" "$placement" "$root" <<'PY'
from pathlib import Path
import sys
p = Path(sys.argv[1]); text = p.read_text()
def tcl_quote(value):
    return '"' + str(value).replace('\\', '\\\\').replace('"', '\\"').replace('$', '\\$').replace('[', '\\[') + '"'
text = text.replace('add_file ../clash/SimpleRisc.topEntity/simple_risc.v',
    'add_file ' + tcl_quote(Path(sys.argv[4]) / 'output/tangnano20k/clash/SimpleRisc.topEntity/simple_risc.v'))
text = text.replace('add_file ../../../boards/tangnano20k/tangnano20k.cst',
    'add_file ' + tcl_quote(Path(sys.argv[4]) / 'boards/tangnano20k/tangnano20k.cst'))
p.write_text(text
    .replace('set_option -frequency 96', f'set_option -frequency {sys.argv[2]}')
    .replace('set_option -global_freq 96', f'set_option -global_freq {sys.argv[2]}')
    .replace('set_option -place_option 0', f'set_option -place_option {sys.argv[3]}'))
PY
cd "$out"
# A failed build must not leave an older timing report looking current.
python3 - "$out/impl/pnr/simple_risc.tr" <<'PY'
from pathlib import Path
import sys
Path(sys.argv[1]).unlink(missing_ok=True)
PY
export DYLD_LIBRARY_PATH="$ide/lib${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}"
export DYLD_FRAMEWORK_PATH="$ide/lib${DYLD_FRAMEWORK_PATH:+:$DYLD_FRAMEWORK_PATH}"
"$ide/bin/gw_sh" build.tcl
python3 "$here/check_gowin_timing.py" "$out/impl/pnr/simple_risc.tr" --freq-mhz "$freq"
