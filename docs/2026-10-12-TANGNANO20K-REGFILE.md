# The register file in RAM, and main at 93 MHz (2026-10-12)

## What happened

- **A fix that broke timing.** The MISC-MEM fix (reserved opcode-0x0f
  encodings are illegal; FENCE.I fetches from memory) was a tiny decode
  change, yet `main` no longer met 96 MHz in any placement: 78-91.5 MHz.
  `main` had met 96 MHz in one placement mode with +0.001 ns, so any change
  could reshuffle placement off that cliff.
- **The register file moves to RAM.** x1-x31 live in two RAMs
  (`asyncRam`), written together, one per read port, read at the decoding
  instruction's rs1/rs2. Decode registers their outputs, so the 4-bank split
  and the bank select are gone. A 32-bit valid mask replaces clearing the
  registers on reset: a register whose bit is clear reads as 0, and x0's is
  never set. Simulation and the SDRAM image keep `cpuRegs` as a mirror of
  values. The BRAM circuit leaves the mirror constant, so synthesis removes
  it.
- **Mapping.** Gowin maps the two RAMs to two SDPB block RAMs (35 BSRAM in
  all), not to distributed SSRAM. On the board the reads behave correctly:
  RV32I passes all 38,400 checks.
- **Effect.** 1,009 fewer registers (2,682) and 1,441 fewer logic cells
  (7,865). Fmax did not move (about 91 MHz best): the critical path is now
  the main state machine's multiplexers (pc and fetch fields).
- **Combined with decode-ahead.** Decode-ahead's limit was the
  register-bank multiplexers, which the RAM removes. Together they pass
  93 MHz (93.042 MHz, +0.005 ns, placement mode 2). At a 96 MHz target the
  best placement reaches 85.9 MHz.

## Results at 93 MHz

| Build | CPI (four kernels) | Clock | Million instructions/s |
|---|---:|---:|---:|
| previous `main` | 9.56 | 96 MHz | 10.04 |
| decode-ahead + RAM register file | 8.82 | 93 MHz | 10.54 (+5%) |

All board suites pass at 93 MHz: interrupts, processor, RV32I, RV32M,
exceptions, finisher, traps, counters, bus, PPP and SD. So does FP-RISC's
harness (12 of 12). All 50 SimpleRisc properties pass, including 900 random
programs and the new MISC-MEM property. SHA-256 of the image begins
`b436e3757a5a50fe`.

## Next

The timing limit is now the single state machine itself: one `case` over
19 phases muxing every field. The real pipeline splits it into per-stage
registers with their own small next-state logic. That is the structural
change that should recover 96 MHz and beyond, and it is where forwarding
belongs.
