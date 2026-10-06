# Tang Nano 20K SDRAM experiment

The board's 64-Mbit, 32-bit SDR SDRAM is now usable as 8 MiB of main memory.
This is a separate 54 MHz processor image, with uncached instruction fetch,
data and loader writes all going through SDRAM. The established BRAM image
and its 96/108 MHz build remain available.

## Physical validation (2026-10-06)

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
- Cycle counters count physical memory wait clocks. No cache exists yet.

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
timing violations. Programming is SRAM-only. Both clocks currently run at
54 MHz; higher CPU frequency needs a faster controller or a clock-domain bridge,
then repeated timing and physical tests. Caches are the next throughput step.
The boot ROM, removal of legacy halt rules and larger loader frames remain
roadmap work; this is not completion of the full system-separation step.

## Controller provenance

`sdram.v` derives from nand2mario's Apache-2.0 controller at
https://github.com/nand2mario/sdram-tang-nano-20k,
commit `918ae4143eed676d29b706df6ec7ebcb61e257c1`. `LICENSE` retains its license.
Changes here provide 32-bit writes with four byte enables and registered word
readback. The PLL uses the reference's shifted SDRAM clock phase. `memory.v`
adds request/completion handling and autonomous refresh. The embedded-memory
port names let Gowin connect the dedicated SDRAM pins; the pin report confirms
those connections.
