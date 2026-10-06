# Tang Nano 20K SDRAM experiment

The board's 64-Mbit, 32-bit SDR SDRAM is now usable as 8 MiB of main memory.
This is a separate processor image with a unified 1 KiB word cache and
54/60/66 MHz clock options. Code, data and loader writes share the cache. The established BRAM image
and its 96/108 MHz build remain available.

## Cached 66 MHz experiment (2026-10-06)

A 256-entry direct-mapped cache stores one physical word per entry, using two
block RAMs. It has synchronous lookup, a valid bit and full physical tags.
Writes invalidate the indexed entry, go through to SDRAM and complete only
after the backing write. Reset sweeps all entries before accepting requests.
There are no dirty lines or separate instruction/data copies. Both RAM aliases
and loader writes use physical word addresses, keeping code uploads and modified
instructions coherent. Ctrl-C leaves the cache transaction to drain with the
existing wrapper. Refresh continues independently during hits.

Measured repeated reads of a 512-byte array (25,600 reads, checksum 1,625,600):

| Image | Measured cycles | Time |
| --- | ---: | ---: |
| Uncached 54 MHz | 2,933,382 | 54.322 ms |
| Cached 54 MHz | 2,389,660 | 44.253 ms |
| Cached 66 MHz | 2,389,648 | 36.207 ms |

Counts vary by a few clocks when refresh overlaps the timed section. The
uncached reference was rebuilt with `SDRAM_CACHE=0` and rechecked on the board.
Cache alone saves 18.5% of cycles; cache plus clock gives about 1.50x throughput
on this workload. This is a small repeated-read workload, not a general FP-RISC
speedup claim; streaming misses and write-heavy workloads can pay lookup overhead.
`benchmark.c` also checks stores/reads through both aliases and executes code
modified through the low alias with `fence.i`.

Cached 54 MHz STA reports Fmax 70.692 MHz, +4.373 ns setup slack. The tested
66 MHz placement reports Fmax 66.165 MHz, +0.038 ns, zero setup/hold violations.
Its limiting paths are routed logic inside the processor, not the cache RAM.
The margin is small and full SDRAM I/O signoff is still outstanding. 66 MHz is
an experimental board-tested setting, not completion of the 96 MHz roadmap gate.
The controller retains its existing <=66.7 MHz timing parameters; higher SDRAM
clocks need revised CAS/delay parameters and a wider initialization cycle counter,
or a separate SDRAM clock with a verified clock-domain bridge.

At 66 MHz the full-capacity tester passes all six phases, 12,582,912 comparisons,
including masked writes and the frequency-adjusted 250 ms retention pause.
The cached CPU passes 38,400 RV32I and 30,720 RV32M reference comparisons,
3,852 bus checks, 270 counter checks, 267 trap checks, 15 finisher cases,
arithmetic/UART/Ctrl-C/reload tests, alias/instruction coherence and the full
1 MiB working set/all-bank/end-of-memory checks. Cache simulation additionally
checks hits, conflicts, write completion, reset invalidation and memory backpressure.

The complete FP-RISC builtin smoke/CSR/refusal/recovery harness passes at
66 MHz on the zero alias. The high-linked 2 MiB allocation fixture also passes,
including its 32 sampled words, final byte, free and successful exit.

Tested cached processor SHA-256:
`0d4e7e54fe6e70ea7f268aa8f3ce8337ffa0c8065c3caec4cbdd2365ffc06565`.

## Original uncached validation (2026-10-06)

The standalone tester passes all 2,097,152 words in six phases: zero, ones,
address hash, complemented hash, zero plus four independent byte-mask writes,
and reverse-order retention readback after a 250 ms pause. Refresh continues
during that pause. Total: 12,582,912 word comparisons, zero mismatches. The
address hash is an odd multiply/XOR, so each word address has a distinct value
and bank/row/column aliases cannot pass the hash phase. Unselected byte lanes
carry poisoned values during masked writes, so ignoring the mask cannot pass. Host checking requires
all six reports and exact cumulative coverage; silence/partial output fails.
A behavioral protocol simulation exercises readiness dropping between request
preparation and acceptance; injected ignored masks fail on all 32 test words.
This simulation checks the tester, not SDRAM electrical timing.

Vendor STA at 54 MHz reports 99.307 MHz estimated maximum, +8.449 ns setup
slack and zero setup/hold violations for the final standalone tester. The
processor placement reports 61.175 MHz, +2.172 ns and zero setup/hold violations.
These are internal logic checks; SDRAM I/O setup/hold constraints and shifted
clock phase still require full timing signoff. The physical tests above are
board evidence under the current conditions, not PVT stress qualification.
The oscillator generic-routing warning remains.

The processor passes 3,852 bus/RX checks, 15 finisher cases, 270 counter checks,
267 traps, arithmetic/UART/reset/reload/memory tests and 34,560 RV32IM reference
comparisons at 54 MHz. Counter tests use the SDRAM mode: elapsed cycles must
include memory waits, while retirement and rollover checks retain exact values.
All Haskell suites pass, including 36 SimpleRisc properties.
A high-address C image passes a full 1 MiB working-set write/verify, all four
bank ends and mixed byte writes near the last RAM word. It saves/restores
stack words when testing those addresses.
The existing complete FP-RISC builtin harness also passes on the zero alias.
High-linked FP-RISC allocates 2 MiB, writes/verifies 32 word samples at 64 KiB
intervals, checks the final byte, frees the allocation and exits successfully.
An earlier 2,048-sample FP-RISC fixture did not finish within the 30/60 second
budgets; instrumentation confirmed allocation completed. That denser loop is
not a passed test and its performance/cause has not been fully profiled.

Bitstream SHA-256:

- Standalone: `9cf37316ed26515007261bec14741ee318422820bc8bd834c80598bfb8609436`
- Processor: `815d54bf81454c37809e1445f31490990ff1c881f566c15b7c247d6809219b5a`

## Memory and host contract

- `0x80000000..0x807fffff`: full 8 MiB SDRAM, word indices retain 21 bits.
- `0x00000000..0x0000ffff`: compatibility alias of the first 64 KiB.
- Finisher and UART addresses retain their existing meanings.
- `P` still loads at physical offset zero, with the existing 16-bit count and
  64 KiB image limit. Data/BSS/heap/stack can use the rest of SDRAM.
- `R` runs at zero; new `H` runs at `0x80000000`. Use `--ram-base 0x80000000`
  in the host runners for high-linked images. Byte framing is otherwise unchanged.
- `M` clears the compatibility 64 KiB, not all SDRAM; startup code initializes
  BSS. Ctrl-C cancels CPU work and drains an outstanding memory operation.
- Cycle counters count physical memory wait clocks, including cache lookup.

`SdramSimpleRisc` wraps the existing serial core. It captures full addresses
before the old 14-bit memory index, stops controller advancement during an
outstanding memory transaction, buffers received UART data and continues the
cycle counter. One outstanding transaction and registered data keep instruction
fetch, byte-store read/modify/write and loader accesses ordered. Refresh gets
priority before accepting work and runs at least every roughly 10 us.

## Build and test

```bash
boards/tangnano20k/sdram/build.sh test
openFPGALoader -b tangnano20k output/tangnano20k/sdram-test/impl/pnr/sdram_test.fs
python3 boards/tangnano20k/sdram/test_protocol.py
python3 boards/tangnano20k/sdram/check.py
boards/tangnano20k/sdram/build.sh core
openFPGALoader -b tangnano20k output/tangnano20k/sdram-core/impl/pnr/sdram_test.fs
python3 boards/tangnano20k/sdram/check_core.py
python3 boards/tangnano20k/check_counters.py --freq-mhz 54 --sdram
```

The host starts the standalone test after opening the UART, preventing lost
initial reports. Build uses the licensed macOS Gowin flow and rejects internal
timing violations. Programming is SRAM-only. Both clocks run at the selected frequency. The default remains 54 MHz.
`SDRAM_CACHE=0` builds a comparison image that bypasses the cache.
`SDRAM_FREQ_MHZ=66` selects the experimentally validated higher clock.
The boot ROM, removal of legacy halt rules and larger loader frames remain
roadmap work; this is not completion of the full system-separation step.

For the faster cached image:

```bash
SDRAM_FREQ_MHZ=66 SDRAM_OUT="$PWD/output/tangnano20k/sdram-cache66" \
  boards/tangnano20k/sdram/build.sh core
openFPGALoader -b tangnano20k output/tangnano20k/sdram-cache66/impl/pnr/sdram_test.fs
python3 boards/tangnano20k/sdram/check_cache.py --freq-mhz 66
python3 boards/tangnano20k/sdram/check_core.py --freq-mhz 66
```

Use the same frequency for `build.sh test`, `check.py --freq-mhz 66`, all
processor check scripts and the FP-RISC runner. `SDRAM_OUT` must be absolute.
The standalone tester bypasses the cache to exercise every SDRAM word.

## Controller provenance

`sdram.v` derives from nand2mario's Apache-2.0 controller at
https://github.com/nand2mario/sdram-tang-nano-20k,
commit `918ae4143eed676d29b706df6ec7ebcb61e257c1`. `LICENSE` retains its license.
Changes here provide 32-bit writes with four byte enables and registered word
readback. The PLL uses the reference's shifted SDRAM clock phase. `memory.v`
adds request/completion handling and autonomous refresh. The embedded-memory
port names let Gowin connect the dedicated SDRAM pins; the pin report confirms
those connections.

The cache's standalone simulation can be reproduced with:

```bash
iverilog -g2012 -s cache_tb -o /tmp/sdram-cache-tb \
  boards/tangnano20k/sdram/cache.v boards/tangnano20k/sdram/cache_tb.v
vvp /tmp/sdram-cache-tb
```
