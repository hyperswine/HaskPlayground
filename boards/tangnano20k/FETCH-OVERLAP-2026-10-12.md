# Fetch overlap: the first pipeline step (2026-10-12)

Decode now reads the word at pc + 4 while the CPU's RAM port is otherwise
idle: during Decode, OperandSelect and Execute. The word arrives three
cycles later, before the current instruction can issue a load or store, and
is kept in a prefetch buffer. A Fetch whose pc matches the buffer goes
straight to Decode, skipping FetchDelay, FetchCapture and FetchWait.

- **Redirects.** A taken branch, a jump, a trap, an interrupt or MRET
  changes the pc, so the next Fetch misses and fetches normally. Nothing
  needs flushing.
- **Stores into the next instruction.** A RAM write to the buffered word
  invalidates it, so self-modifying code still sees the new instruction
  (`prop_store_into_next_instruction_is_fetched_fresh`). A DMA write to code
  is not snooped; code loaded by DMA must not be the next instruction.
- **Reset.** A system reset drops the buffer.
- **The SDRAM image** keeps prefetch off (`cpuPrefetchEnable = False`): its
  wrapper drives memory by the CPU's phase.

The capture point is fixed: the buffer takes the memory word on the third
cycle after Decode. No load can be on the bus then, because loads issue only
after Commit.

## Results

**Haskell.** All 47 SimpleRisc properties pass. That includes 900 random
programs against the reference model (now with loops, software interrupts
and MRET) and the full-circuit UART test. Two tests that pinned instruction
timing were updated: simple instructions now take 9 cycles for the first and
6 after it, and the final trap takes 7 cycles instead of 10.

**Gowin.** 96.393 MHz, +0.042 ns worst setup slack (placement mode 1).

**Board, `run_cpi.py`** (`c/cpi.c`, `mcycle`/`minstret`):

| Kernel | CPI before | CPI after |
|---|---:|---:|
| arithmetic loop | 9.25 | 6.62 |
| memory copy | 11.49 | 8.99 |
| branch-heavy | 16.65 | 14.07 |
| multiply/divide | 22.34 | 19.93 |
| all four | 12.98 | 10.42 |

That is 1.25× faster overall at the same clock.

All board suites pass: interrupts, processor, RV32I, RV32M, exceptions,
finisher, traps, counters, bus, PPP and SD. So does FP-RISC's harness (12 of
12). `c/counters.c` pinned a CSR read at 11 cycles and a NOP at 9; both are
3 cycles shorter now.

## Where the remaining cycles go

- **Taken branches and jumps** miss the prefetch, and the branch kernel is
  still at 14 CPI. Prefetching the branch target as soon as it is computed
  (at Execute) would recover most of that.
- **Loads and stores** spend about 9 cycles in the registered bus: Idle,
  Decode, Dispatch, Issue, Delay, Capture, Wait, Align, Reply. A RAM-only
  fast path would recover most of that.
- **The instruction itself.** Decode, OperandSelect, Execute,
  ExecuteFinish and Commit run in sequence. Overlapping them with the next
  instruction is the real pipeline, and needs forwarding.
