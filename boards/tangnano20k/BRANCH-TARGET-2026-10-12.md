# Branch-target prefetch (2026-10-12)

**How it works.** At ExecuteFinish, a branch, JAL or JALR already holds its
target in a register, and the RAM port is free there. Loads and stores, the
port's other user at that stage, are never jumps. So the core reads the
target word into a second buffer, whether or not the branch will be taken.
If the jump is taken, the next Fetch finds the target either captured, or
on the memory bus a cycle later; the new phase FetchTargetWait takes it
straight off the bus. A taken jump's target then costs 2 fetch cycles
instead of 4. `prop_taken_jump_fetches_its_target_early` pins it: `jal`,
then `addi`, then the trap takes 23 cycles, against 25 without the read.

## Making it fit 96 MHz

The first version fell to 74-85 MHz. Three changes brought it back.

1. **ExecuteFinish's read address** came from opcode compares and a RAM
   check in front of the address mux. It now comes from two kind bits
   (`execMemory`, `execJump`) registered at OperandSelect. The RAM check
   only gates the fast path at Commit; reading a non-RAM address is
   harmless.
2. **Fetch compared the pc against both buffer addresses**, so the issued
   address and the instruction mux waited on 32-bit compares. Now the
   instruction that sets the pc records where its successor is:
   `cpuFetchSource` is FromPrefetch (pc + 4), FromTarget (a taken jump), or
   FromMemory. Fetch consumes it, so a trap, MRET, an interrupt or a reset
   always fetches normally. Fetch always issues the pc's read, which a hit
   makes harmless.
3. **Stores into a buffered word.** The check snooped every RAM write
   through the CPU's output mux into the valid bits. Now ExecuteFinish
   compares the store's word address with both buffer addresses, all three
   registers, and registers `cpuStoreHitsPrefetch`/`cpuStoreHitsTarget`.
   Commit drops the matching buffer; every write path finishes before the
   next Fetch.
   
   The compare uses the word address (bits 31-2), not `memoryIndex`.
   Clash shared `memoryIndex` with the memory-address output and pulled
   its mux into the path. A store that writes is always to RAM, so the word
   address is exact.

On the board, `check_counters.py` caught one regression from step 2: CSR
instructions retire through `csrWriteStep`, which did not set the fetch
source, so the instruction after every CSR access missed the prefetch. It
now sets FromPrefetch.

## Results

**Haskell.** All 49 SimpleRisc properties pass, including 900 random
programs (loops, interrupts, MRET) against the reference model.

**Gowin.** 96.008 MHz, +0.001 ns (placement mode 2). Placement now swings
widely between modes (80-96 MHz), and Gowin's license server refused
roughly half the builds.

**Board CPI** (`run_cpi.py`):

| Kernel | Serial | + fetch overlap | + fast RAM | + branch target |
|---|---:|---:|---:|---:|
| arithmetic | 9.25 | 6.62 | 6.62 | 6.37 |
| memory copy | 11.49 | 8.99 | 6.83 | 6.49 |
| branches | 16.65 | 14.07 | 14.07 | 13.79 |
| multiply/divide | 22.34 | 19.93 | 19.93 | 19.53 |
| all four | 12.98 | 10.42 | 9.85 | 9.56 |

Every kernel's loop branch benefits. The "branches" kernel spends most of
its time in its multiply (an LCG), not in branches.

All board suites pass, and so do PPP, SD and FP-RISC's harness (12 of 12).

## Where this leaves the serial core

The total is 1.36x the serial core at the same clock. What remains is
mostly the five sequential stages of each instruction (Decode,
OperandSelect, Execute, ExecuteFinish, Commit), plus the iterative
multiply/divide. Overlapping those is the real pipeline: forwarding,
hazards, flushes. With the timing margin at zero, it will need a clock
step down, or deeper retiming of the stages.
