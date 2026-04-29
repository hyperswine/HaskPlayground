#!/usr/bin/env bash
# timing-report.sh — Full build + timing report for the RV32I CPU (Tang Nano 20K)
#
# Runs:
#   1. yosys      — synthesise top.v + pll_wrap.v → config.json
#   2. nextpnr    — place-and-route → config_pnr.json + timing.sdf + timing_report.json
#   3. gowin_pack — generate bitstream → config.fs  (optional, --no-pack to skip)
#   4. report     — print fmax, slack, utilisation summary from timing_report.json
#
# Usage:
#   ./timing-report.sh [--no-pack] [--seed <N>]

set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# ── Tool paths ──────────────────────────────────────────────────────────────
OSS="$HOME/Documents/Libs/oss-cad-suite/bin"
YOSYS="$OSS/yosys"
NEXTPNR="$OSS/nextpnr-himbaechel"
GOWIN_PACK="$OSS/gowin_pack"
CHIPDB="$HOME/Documents/Libs/oss-cad-suite/share/nextpnr/himbaechel/gowin/chipdb-GW2A-18C.bin"

# ── Defaults ────────────────────────────────────────────────────────────────
DO_PACK=1
SEED=1

while [[ $# -gt 0 ]]; do
    case "$1" in
        --no-pack) DO_PACK=0 ;;
        --seed)    SEED="$2"; shift ;;
        *) echo "Unknown flag: $1"; exit 1 ;;
    esac
    shift
done

# ── 1. Synthesis ─────────────────────────────────────────────────────────────
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "  [1/4] Synthesis (yosys)"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
"$YOSYS" -p "
    read_verilog top.v pll_wrap.v
    synth_gowin -top pll_top -json config.json
" 2>&1 | grep -E "^(Warning|Error|\$|   \$| -- |\[|Chip:|Number of)" || true

echo ""

# ── 2. Place-and-route + timing ──────────────────────────────────────────────
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "  [2/4] Place-and-Route (nextpnr-himbaechel)"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
"$NEXTPNR" \
    --json config.json \
    --write config_pnr.json \
    --freq 81 \
    --seed "$SEED" \
    --device GW2AR-LV18QN88C8/I7 \
    --vopt family=GW2A-18C \
    --vopt cst=tangnano-20k.cst \
    --chipdb "$CHIPDB" \
    --sdc top.sdc \
    --sdf timing.sdf \
    --report timing_report.json \
    --detailed-timing-report \
    2>&1 | grep -E "^(Info|Warning|Error|CRITICAL)" | grep -vE "^Info: \[" | tail -30

echo ""

# ── 3. Bitstream pack ────────────────────────────────────────────────────────
if [[ $DO_PACK -eq 1 ]]; then
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "  [3/4] Bitstream (gowin_pack)"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    "$GOWIN_PACK" --device GW2AR-LV18QN88C8/I7 --cst tangnano-20k.cst \
        config_pnr.json config.fs 2>&1 | tail -5
    echo "  Bitstream written: config.fs"
    echo ""
else
    echo "  [3/4] Bitstream pack skipped (--no-pack)"
    echo ""
fi

# ── 4. Timing summary ────────────────────────────────────────────────────────
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "  [4/4] Timing & Utilisation Report"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
python3 - <<'PYEOF'
import json, sys

with open("timing_report.json") as f:
    d = json.load(f)

# ── Fmax ──────────────────────────────────────────────────────────────────
print("\n  Fmax (achieved vs constraint):")
for clk, info in d.get("fmax", {}).items():
    achieved    = info.get("achieved", 0)
    constraint  = info.get("constraint", 0)
    period_ns   = 1000.0 / achieved if achieved else 0
    slack_mhz   = achieved - constraint
    slack_mark  = "✓ PASS" if achieved >= constraint else "✗ FAIL"
    print(f"    {clk}")
    print(f"      Achieved  : {achieved:.2f} MHz  ({period_ns:.3f} ns)")
    print(f"      Constraint: {constraint:.2f} MHz")
    print(f"      Slack     : {slack_mhz:+.2f} MHz  [{slack_mark}]")

# ── Critical path ─────────────────────────────────────────────────────────
print("\n  Critical path(s):")
for cp in d.get("critical_paths", []):
    frm  = cp.get("from", "?")
    to   = cp.get("to",   "?")
    path = cp.get("path", [])

    # Delays are in nanoseconds
    total_ns = sum(seg.get("delay", 0) for seg in path)

    print(f"    {frm} → {to}")
    print(f"      Total delay: {total_ns:.3f} ns  ({len(path)} segments)")

    # Print the 5 slowest segments
    segs_sorted = sorted(path, key=lambda s: s.get("delay", 0), reverse=True)
    print("      Top 5 slowest segments:")
    for seg in segs_sorted[:5]:
        dly  = seg.get("delay", 0)
        typ  = seg.get("type", "")
        # net name: from-cell → to-cell for logic, net name for routing
        net  = seg.get("net") or (
            seg.get("from", {}).get("cell", "?").split(".")[-1]
            + " → "
            + seg.get("to", {}).get("cell", "?").split(".")[-1]
        )
        # Truncate long names
        if len(net) > 60:
            net = net[:57] + "..."
        print(f"        {dly:6.3f} ns  [{typ:7s}]  {net}")

# ── Utilisation ───────────────────────────────────────────────────────────
# GW2A-18C totals (nextpnr doesn't report these in the JSON)
KNOWN_TOTALS = {"LUT4": 20736, "DFF": 20736, "BSRAM": 46, "ALU": 10368}

print("\n  Utilisation:")
util = d.get("utilization", {})
for name, info in util.items():
    used  = info.get("used",  0)
    total = info.get("total", 0) or KNOWN_TOTALS.get(name, 0)
    if used == 0:
        continue
    pct = 100.0 * used / total if total else 0
    bar_len = int(pct / 5)
    bar = "█" * bar_len + "░" * (20 - bar_len)
    pct_str = f"{pct:5.1f}%" if total else "     "
    print(f"    {name:<12} {used:>5}{(' / ' + str(total)) if total else '':>8}  {pct_str}  {bar}")

print()
PYEOF
