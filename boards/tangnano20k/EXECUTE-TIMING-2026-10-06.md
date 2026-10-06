# Registered execution experiment (2026-10-06)

The licensed Gowin build of the staged SimpleRisc core meets 108 MHz internal
setup/hold timing and passes the complete board and FP-RISC regressions there.
The original registered-bus core failed the vendor 96 MHz constraint. This
experiment adds one ordinary instruction clock; it does not overlap instructions.

## Changes

OperandSelect now registers the second ALU operand and decoded operation
controls alongside the register operands. Execute registers candidate ALU
results, address/link calculations and comparison flags. ExecuteFinish selects
the result and combines comparison flags. Signed and unsigned comparisons use
parallel 16-bit high/low comparisons, with the registered high-equality flag
choosing the low comparison. Branch comparisons use the same split.

CSR address selection is registered at Commit. CsrRead combines one-hot masked
values through a balanced OR tree; invalid/busy reads can capture a temporary
value but cannot retire it. Host reset clears architectural state and pending
requests while preserving pipeline data that is overwritten before reuse.
Hardware reset still initializes the whole machine. These changes remove
address decode and broad reset gates from timing-sensitive data paths.

Ordinary instructions take nine clocks, shifts ten and CSR reads eleven.
Iterative multiply/divide bypasses ExecuteFinish and retains its unit latency.
The exact C counter fixture consequently changes 10/18 clocks to 11/20; its
expectations remain exact. RAM remains 64 KiB at zero. Traps, finisher, loader,
UART protocol and FP-RISC runtime ABI retain their contracts.

## Vendor trials

Gowin V1.9.12.04, GW2AR-LV18QN88C8/I7; setup Slow 0.95 V / 85 C, hold Fast
1.05 V / 0 C. Placement mode 0 unless noted. Each row is its own placement;
reported maximum frequency is not a promise that another target will route
as well. All rows have zero hold violations.

| Source variant | Target MHz | Reported maximum MHz | Worst setup ns | Setup endpoints |
|---|---:|---:|---:|---:|
| Original eight-clock bus core | 96 | 84.444 | -1.426 | 271 |
| Two additional execution stages, original CSR mux | 96 | 86.654 | -1.123 | 218 |
| Two additional stages plus CSR/reset changes | 96 | 95.981 | -0.002 | 3 |
| One additional stage, full-width comparisons | 99 | 99.061 | +0.006 | 0 |
| One additional stage, full-width comparisons | 108 | 101.646 | -0.579 | 146 |
| Final split comparisons | 96 | 96.252 | +0.027 | 0 |
| Final split comparisons | 108 | 108.038 | +0.003 | 0 |
| Final split comparisons | 111 | 96.881 | -1.313 | 159 |
| Final split comparisons | 114 | 99.683 | -1.260 | 261 |
| Final split comparisons | 120 | 113.709 | -0.461 | 143 |

Timing optimization and default maximum fanout 1000 are used in the final
builds. Placement mode 1 and maximum fanout 32 did not improve the earlier
variant. Higher targets did not reliably produce faster placements. The
120 MHz report is a useful direction for further optimization, but it fails
its own constraint and was not programmed.

The final 108 MHz image uses 9,158 logic cells (8,821 LUTs + 337 ALUs), 2,929
registers and 32 BSRAM blocks. Its SHA-256 is
`77bcc3dcc58aa8a3f9134c2341564090a98b3535d304b70b1dcbbb488c0b2e7a`.
Reports are in `output/tangnano20k/gowin-compare-108/impl/pnr/`; the other
final trials replace the directory suffix with 96, 111, 114 or 120.
The final 96 MHz image has the same resource counts; SHA-256:
`e8394da4ec3b4c92bfa338d3900d173591afae765230a6e7a79399c786890d88`.

## Validation

All Haskell suites pass, including 33 SimpleRisc properties. New properties
compare registered execution against the original computation, CSR selection
against the reference read function, and split comparisons/branches against
32-bit results, including signed boundary values. Generated RTL returns
`HiDONE` under Icarus.

At both 96 and 108 MHz the physical board passes 3,852 mixed-width RAM/RX assertions,
15 finisher cases, 270 counter assertions, 267 trap assertions, CPU arithmetic,
UART RX/TX, Ctrl-C, clear/reload and memory stress. RV32I contributes 3,840
reference comparisons and RV32M 30,720. The complete FP-RISC Tang Nano harness
passes host/link/ISA/loader checks, three smoke runs, three CSR runs, seven
panic/refusal cases and recovery at both frequencies. The board is left on the
fully tested 108 MHz SRAM image; flash is unchanged.

For ordinary instructions, 108 MHz / nine clocks equals 12 million instructions
per second, matching the earlier eight-clock core at 96 MHz. This is a cycle
calculation, not a workload benchmark; memory, shifts, CSR and M-unit code have
different costs. Timing margins remain very narrow. The oscillator input
still has the PR1014 generic-routing warning, and UART/button external timing
is not fully constrained. These results do not establish temperature/voltage
stress coverage or an ultimate frequency limit.

## Reproduction and next experiment

```bash
FREQ_MHZ=108 GOWIN_OUT="$PWD/output/tangnano20k/gowin-compare-108" boards/tangnano20k/build_gowin.sh
openFPGALoader -b tangnano20k output/tangnano20k/gowin-compare-108/impl/pnr/simple_risc.fs
python3 boards/tangnano20k/check_processor.py --freq-mhz 108
python3 boards/tangnano20k/check_rv32i.py --freq-mhz 108 --repeat 1
python3 boards/tangnano20k/check_rv32m.py --freq-mhz 108
```

The build now generates matching PLL and clock constraints and returns failure
if any internal setup/hold endpoint violates timing, even when Gowin emitted
a bitstream. Run the remaining bus, finisher, counter and trap checks plus
FP-RISC's `tests/check_tangnano20k.py --freq-mhz 108` for the complete gate.

The 108 MHz critical path now includes ALU result selection controlled by
registered funct3 bits. Registering a one-hot result selector is a concrete
next experiment. Improving placement stability is also needed before relying
on the estimated maximum from a different frequency build.
