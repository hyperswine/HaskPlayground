# The RAM load/store fast path (2026-10-12)

Loads and stores to the local block RAM no longer go through the registered
bus. That path took 11 cycles after Commit for a load and about 5 for a word
store.

- **ExecuteFinish** already holds the address in a register, and the CPU's
  RAM port is free: the prefetch read went out at Decode. So for an aligned
  load or store whose address is in RAM (a single 16-bit compare), it issues
  the word's read and records `cpuFastMemory`.
- **Commit** still checks the encoding and alignment first, and a fault
  still wins. Then:
  - a **word store** writes at once and retires;
  - a **load** or a **byte/halfword store** waits in the new phases
    MemoryWait and MemoryCapture for the word, three cycles after the
    read. A load aligns it and writes back; a sub-word store merges into it
    and writes.
- **Unchanged:** devices, ROM data loads, faults, and the SDRAM image (where
  `cpuLocalMemory`, formerly `cpuPrefetchEnable`, is off) all keep the bus
  path.
- **Stores into code.** A fast store into the prefetched next instruction
  still invalidates the prefetch: the check watches every RAM write the CPU
  makes.

As the first instruction, after a full four-cycle fetch, a RAM load takes
11 cycles, a word store 9 and a byte store 11. Mid-program, after a
prefetch hit, those are 8, 6 and 8. `prop_ram_accesses_take_the_fast_path`
pins this.

## Results

**Haskell.** 48 SimpleRisc properties pass, including 900 random programs
against the reference model.

**Gowin.** 96.099 MHz, +0.011 ns (placement mode 1). The first version
decoded the whole memory map at ExecuteFinish and passed only with
placement mode 3 (+0.015 ns). Mode 0 reached 90.5 MHz. The RAM-only compare
is enough for this path. Slack at 96 MHz is now very small.

**Board CPI** (`run_cpi.py`):

| Kernel | Serial | Fetch overlap | Plus fast RAM |
|---|---:|---:|---:|
| arithmetic | 9.25 | 6.62 | 6.62 |
| memory copy | 11.49 | 8.99 | 6.83 |
| branches | 16.65 | 14.07 | 14.07 |
| multiply/divide | 22.34 | 19.93 | 19.93 |
| all four | 12.98 | 10.42 | 9.85 |

All board suites pass, and so do PPP, SD (DMA and byte transfers) and
FP-RISC's harness (12 of 12).

## Next

- **Taken branches** are now the largest single cost: 14 CPI on the branch
  kernel. Prefetching the target as soon as it is known would cut it.
- **Overlapping instruction stages** (the real pipeline) needs forwarding.
  The 96 MHz slack suggests either retiming or accepting a lower clock.
