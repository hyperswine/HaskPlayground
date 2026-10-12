# The memory port from registers

Branch `memport`. The CPU's read address and store write are now functions
of registered state, outside the phase case. Cycle behaviour is unchanged
(CPI 8.82); the gain is all clock: 99 to 117 MHz.

## Why

With every constraint met, Gowin's report shows near-zero slack everywhere
and ranks nothing. Building `main` against an unreachable 108 MHz target
gave a ranking (Fmax 101.6 MHz): the worst endpoints started at single
phase bits (Decode, Fetch) and ended in registers those phases do not even
write (the pc, the instruction, the prefetch word), and in the port's write
register. Each was a priority mux over the phase case: the memory port's
address and write data were chosen per phase inside it, with an "address 0,
no read" case for most phases.

## What changed

- `portAddress`: Fetch reads the pc, Decode pc + 4, ExecuteFinish a load or
  store's word, a branch or jump's target, or pc + 8 (decode-ahead). Every
  other phase reads the pc too: a read nobody waits for is harmless (the
  prefetch and target buffers capture only reads they issued, and the SDRAM
  image ignores the read address), so no phase needs its own case.
- `portWrite`: stores are prepared at ExecuteFinish into `cpuPortIndex` (the
  RAM word index), `cpuStoreData` (the data in its byte lanes) and
  `cpuStoreMask` (the lanes it replaces); `cpuStoreNow` marks an aligned
  fast word store. A word store writes at Commit from these registers; a
  byte or halfword store merges at MemoryCapture as
  `(word .&. complement mask) .|. data`, two levels of logic.
- The prefetch word is captured whenever its read arrives (`count == 1`, a
  register); only its valid bit and count wait on whether this cycle issued
  a new read.
- A fetch fault only chooses Decode's next phase; Decode's other writes are
  harmless for a trap.

Tests that asserted an idle cycle's read address is 0 now assert only "no
write and no UART byte" (`quiet`).

## Results

Gowin, placement 2 unless stated:

| Target | Before (`main`) | After |
|---:|---:|---:|
| 99 MHz | 99.01 (0.001 ns) | 99.40 (0.040 ns); placement 3 99.39 (was 96.7); placement 0 92.7 (was 84.6) |
| 108 MHz | 101.6 | **108.05**; placement 3 108.01; placement 4 84.6 |
| 111 MHz | | 111.05 |
| 114 MHz | | **114.03**; placement 3 114.08 |
| 117 MHz | | **117.09** |
| 120 MHz | | 115.9 (fail) |

Verified build: `GOWIN_PLACE_OPTION=2 FREQ_MHZ=117` (image hash begins
0e0a028b1d4061ee; UART at 117 MHz / 868 = 134,793 baud). On the board at
117 MHz every suite passes: interrupts, processor, RV32I (38,400 checks),
RV32M (30,720), exceptions, finisher, traps, counters, bus, SD and PPP, and
FP-RISC's harness (12 of 12). The 108 and 114 MHz images pass the same
suites. All 50 SimpleRisc properties pass, including the differential test
against the reference model.

| Build | CPI | Clock | Million instructions/s |
|---|---:|---:|---:|
| serial core | 12.98 | 96 MHz | 7.40 |
| `main` before | 8.82 | 99 MHz | 11.22 |
| `memport` | 8.82 | 117 MHz | **13.27 (+18%)** |

## Next

At 120 MHz the failing endpoints spread across the phase case again: a
phase bit (MulDiv's) into the pc, the exception value and mscratch, plus
register-file and bus paths. No single structure dominates. The next step
is the real split: per-stage registers with their own valid bits and
next-state logic, several instructions in flight, and forwarding. That is
where CPI falls, not just the clock.
