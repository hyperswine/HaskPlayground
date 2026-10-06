# Gowin vendor timing baseline (2026-10-06)

Gowin V1.9.12.04 successfully synthesizes and routes the registered-data-bus
SimpleRisc source at HaskPlayground commit `82411df` for
`GW2AR-LV18QN88C8/I7`. The installed license works for the command-line tools.
This is a vendor-generated netlist and placement, not a vendor reanalysis of
the nextpnr placement already tested on the board.

The initial version of `boards/tangnano20k/build_gowin.sh` ran this baseline on macOS. It regenerates Clash RTL,
uses the existing pins and 96 MHz PLL settings, raises the synthesis loop
limit to 20,000 for the 16,384-word RAM initialization, and sets the bundled
library/framework search paths. The vendor-only wrapper copy changes the
PLL DEVICE string to `GW2AR-18C`; the open-source wrapper is unchanged.
The generated `clocks.sdc` defines the 27 MHz input and the 32/9 generated core clock.

## Post-route results

| Metric | Result |
|---|---|
| Core clock recognized | 96 MHz / 10.417 ns |
| Setup corner | Slow, 0.95 V, 85 C, C8/I7 |
| Hold corner | Fast, 1.05 V, 0 C, C8/I7 |
| Vendor maximum frequency | 84.444 MHz |
| Worst setup slack | -1.426 ns |
| Setup violating endpoints | 271 |
| Setup total negative slack | -160.517 ns |
| Hold violating endpoints | 0 |
| Logic | 7,583 (7,284 LUTs + 299 ALUs) |
| Registers | 2,665 |
| BSRAM | 32 |

The worst reported path launches from `core/c$ds2_app_arg_1519_s0`
(`cpuInstruction[28]`) to `core/c$ds2_app_arg_1133_s0`
(a bit in `cpuComputed`). Its detailed path passes through ALU selection,
`c$$j_case_alt` add/subtract carry logic and result selection. Data delay is
11.807 ns across 15 logic levels. Registering ALU operation selection or
splitting execution is a concrete next timing experiment; this report alone
does not prove every failing endpoint has the same cause.

The initial run used the open-source PLL DEVICE string and warned that it
was invalid. Repeating with `GW2AR-18C` removes that warning and produces the
same timing numbers. The remaining PR1014 warning says the oscillator input
net `clk_27m_d` uses generic routing. UART/button external timing is not fully
constrained here; this is an internal-clock baseline, not full I/O signoff.

The previous nextpnr image reported 144.61 MHz and passed physical tests at
96 MHz. Different synthesis and placement prevent interpreting 84.444 versus
144.61 as a direct measurement of timing-model error. The vendor result
shows that this vendor placement does **not** meet the 96 MHz slow-corner
constraint, even though bitstream generation succeeds. No higher clock is
validated, and this vendor image has not been loaded into the board.

Reports are under `output/tangnano20k/gowin/impl/pnr/`:
`simple_risc.tr` (text timing), `simple_risc.tr.html` (HTML timing), and
`simple_risc.rpt.txt` (resources). The existing board SRAM/flash is unchanged
by this analysis.

The current script builds the newer execution experiment with timing optimization.
See [EXECUTE-TIMING-2026-10-06.md](EXECUTE-TIMING-2026-10-06.md) for subsequent
source changes and physical validation; the results above describe only the
original baseline.
