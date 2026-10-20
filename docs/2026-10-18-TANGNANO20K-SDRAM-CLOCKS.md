# Split CPU/SDRAM clock experiment, 2026-10-18

The subsequent [SDRAM I/O audit](2026-10-18-TANGNANO20K-SDRAM-IO.md) adds explicit
model-completeness checks. The timing results below are historical internal
STA/physical acceptance results, not complete SDRAM I/O signoff.

RV32IMC at 108 MHz with 54 MHz SDRAM still fails vendor timing and the
physical compressed-instruction test. The same final RTL at **72 MHz CPU /
54 MHz SDRAM passes timing and all 12 physical FP-RISC/SD acceptance cases**.
Its classifier takes 36.1% longer than the previous 48 MHz image, so this is
a clock-domain and correctness checkpoint, not a performance improvement.
The board currently contains the 72/54 MHz image in SRAM; flash is unchanged.
The known working 48 MHz image is retained separately. The results below
distinguish simulation, vendor timing, and physical tests.

## Changes

The frontend now registers raw instruction assembly before compressed
expansion, then registers the expanded instruction and source metadata before
register-file addressing. Redirects, traps and FENCE.I clear both stages.
This adds two frontend cycles; steady-state issue throughput is unchanged.
Expanded instructions use a two-entry FIFO whose payload writes depend on
its registered count, separating decode hazards from frontend data enables.
Cross-word assembly waits for the second word even at an unmapped boundary;
decode remains responsible for the precise access fault.

The wrapper uses the CPU's registered memory addresses and byte lanes, with a
registered request slot. The cache retains the next two logical stages, so
the CPU still sees a three-cycle port. Holds freeze the request and cache
stages together. The address mapper checks the RAM mask and ROM range
directly instead of decoding every peripheral.

Cache tags use a dedicated synchronous BSRAM read and one combined write
port for clearing, invalidation and refill. A registered collision flag
forces a miss when a lookup overlaps tag invalidation, so correctness does
not depend on device-specific read-during-write results. An isolated Gowin
synthesis confirmed the tag BSRAM, and a model returning unknown data on
every same-address read/write still passed the cache acceptance suite.

A one-request mailbox crosses from the CPU/cache clock to the memory clock.
It holds each request until completion, collects all four read words in the
memory domain, then replays the completed line to the cache. Request and
response toggles each use two synchronizer registers. Accepted writes remain
ordered and execute once. CPU Ctrl-C restart leaves the mailbox and memory
controller running; the board reset asserts both domains and synchronizes
release locally.

Two PLLs derive CPU 108 MHz and memory 54 MHz from the board's 27 MHz input.
The SDRAM output clock retains its 135-degree phase relative to the controller.
Only first-stage toggle synchronizers have false-path exceptions. Payload
buses retain setup/hold analysis; there are no blanket asynchronous clock
groups. Mailbox payloads are held across the synchronizer delay and capture.

## Reproduction

```sh
SDRAM_CPU_FREQ_MHZ=108 SDRAM_FREQ_MHZ=54 SDRAM_RVC=1 \
  SDRAM_OUT="$PWD/output/tangnano20k/sdram-pipeline-c108-m54-sd-fpr" \
  boards/tangnano20k/sdram/build.sh pipeline
```

Equal CPU/memory frequencies use the direct connection and existing single
PLL. The separate-clock option accepts 108/54 MHz and a 72/54 MHz checkpoint. The build
guard requires the explicit CPU and memory/pin clock constraints and zero
internal setup/hold violations.

```sh
python3 boards/tangnano20k/sdram/check_clock_bridge.py
python3 boards/tangnano20k/sdram/check_pipeline_cache.py
python3 boards/tangnano20k/sdram/check_pipeline_rtl.py --compressed --cdc-bridge
python3 boards/tangnano20k/sdram/check_pipeline_rtl.py --compressed --cdc-bridge \
  --boot-loader --loader-command H --pin-model --pin-freq-mhz 54 \
  --pin-phase-degrees 135 --pin-tac-ns 5.4 --uart-bit-cycles 32
```

## Completed functional checks

- Full Haskell suite, including all 64 SimpleRisc properties: passed.
  This covers compressed differential programs, cross-word assembly, precise
  traps, interrupts, arithmetic and independent-instruction issue throughput.
  Trial three additionally checks 2,000 real pipelined multiply/dependent-ADDI
  cases and 200 random streams of 700 cycles for the registered hazard contract.
- Cache model with BSRAM tags: 201 reads, 121 masked writes, 73 fills and
  2,555 held clocks. All 82 overlapping tag read/write cases also pass when
  the memory returns unknown data; removing the collision guard makes that
  test fail as intended. Conservatively rejecting these collisions adds
  eight fills relative to the original model run.
- Generated CPU/cache: passed with direct memory and with the actual mailbox
  at half-speed memory and an offset phase. Includes high RAM, byte lanes,
  coherence, branches/traps, counters, UART Ctrl-C restart and SPI loopback.
  The final CDC test exercised 72 fills, 18 writes and 32,102 held CPU clocks.
  Hash-bound evidence is in
  `output/pipeline-trial3-integrated/results.json`.
- Mailbox: four clock-ratio/phase scenarios, 1,233 transactions per scenario,
  all 16 write masks, stalls, out-of-order beat indices and reset at four
  transaction stages. Actual controller pin-model runs at 108/54 MHz and
  135 degrees passed with 0 and 6 ns DQ access delay: 207 reads, 98 masked
  writes, 828 beat comparisons and 13 refreshes each.
- Generated CPU plus actual SDRAM pin controller: extended bootloader
  upload, checksum, execution and `DONE` passed at 108/54 MHz, 135 degrees
  and 5.4 ns DQ access delay. This integration run accelerates UART to
  32 clocks/bit; physical tests use the production 868-clock divider.

The fresh physical reference run at 48 MHz completed the FP-RISC decision
tree (128 training rows, 64 test rows, 8 bins, depth 5, minimum leaf 2) in
4,856,153 microseconds. Its 23 nodes, depth, both prediction digests,
124/128 training hits and 61/64 test hits matched an independent Python
reference. It exited with `FPR EXIT 0` and `DONE`.

## First vendor trial

Gowin V1.9.12.04, placement mode 2, slow C8/I7 setup model:

| Item | Result |
| --- | --- |
| CPU target / reported Fmax | 108 / 79.514 MHz |
| Worst setup slack | -3.317 ns |
| Setup / hold violating endpoints | 1,955 / 0 |
| Memory target / reported Fmax | 54 / 141.538 MHz |
| Logic / registers / BSRAM | 17,256 (84%) / 6,558 (42%) / 4 |
| Placement / routing elapsed | 2m58s / 9m13s |

This image was **not programmed**. Its reports, bitstream and netlists are
retained in `output/tangnano20k/sdram-pipeline-c108-m54-sd-fpr-trial1` with a
SHA-256 manifest. The build's timing guard rejects it.

The worst CPU path is instruction-PC range decoding during cross-word
assembly to the outstanding-word counter (12.541 ns). The next group is the
registered write flag through address selection and the cache's distributed
tag-memory read mux (12.538 ns). Further paths involve decode backpressure
and E2 result selection. Both former 20 ns paths were removed; the next
changes target these newly exposed cones.

The report explicitly lists CPU 108 MHz, memory 54 MHz, SDRAM 54 MHz with
6.944 ns/135-degree phase, and both narrow false-path constraints as active.
Cross-clock payloads appear in the normal hold analysis (worst +0.272 ns),
and the request synchronizer's first-to-second stage remains timed
(+0.307 ns hold). Memory-clock setup TNS is zero. The separate exception
detail section says none; the active constraints are recorded in section
3.8. Positive reset recovery/removal timing is reported.

Gowin warns that a branch of the 27 MHz input feeding both PLLs uses generic
routing. The CPU and memory clocks use global clock routing. The mailbox
tolerates their relative phase, and the SDRAM phase is generated locally by
the memory PLL; internal STA does not establish PLL reference jitter or
physical lock reliability.

## Second vendor trial

The cache BSRAM and frontend FIFO changes reduce final PNR logic to
14,617 (71%), registers to 5,036 (32%), and BSRAM use to 5/46. This saves
2,639 logic cells and 1,522 registers across the whole design. Placement
and routing each take 1m39s, compared with 2m58s and 9m13s in trial one.

Timing still fails 108 MHz: 79.757 MHz CPU Fmax, -3.279 ns worst setup,
1,549 setup violations, zero hold violations. Memory Fmax is 133.621 MHz
with zero memory-clock TNS. All 25 reported worst setup paths are now CPU
internal; the cache tag paths have disappeared. Minimum hold is +0.077 ns
from the bridge's CPU-side data register to the cache data BSRAM.

At this point the critical path recombined multiply partial products and selected the
pending register-file write value in the same cycle. Other paths run through
memory-target selection and execute-result forwarding into decode. Trial
three below registers the multiply result, simplifies target selection and
forwards from registered results.

This image was also **not programmed**. The complete build is retained at
`output/tangnano20k/sdram-pipeline-c108-m54-sd-fpr-trial2`.

## Third vendor and physical trial

The multiply result now has an extra M cycle before writeback, and operand
forwarding comes from registered M results. Dependent ALU operations wait
one extra cycle compared with E2 forwarding. The decode hazard is calculated
from the next pipeline state and registered; a cycle-by-cycle invariant
checks it against the original dependency rule. RAM/ROM predicates use
one-hot target bits, and iterative multiply/divide registers the result-half
selector. Constructor-based target equality works around the installed
Clash backend's unsupported `DataTag` operation on a custom representation.

Final 108/54 MHz PNR uses 12,933 logic cells (63%), 5,036 registers (32%),
5 BSRAM and 3 DSP. It still fails timing: **82.359 MHz Fmax, -2.883 ns,
2,367 setup violations and zero hold violations**.

| Remaining path | Data delay | Implication |
| --- | --- | --- |
| M wait counter through stage readiness to D payload | 12.107 ns | Wide stall enables; 79% routing |
| Pending interrupt through handover enables to E1 payload | 12.105 ns | Global control fanout |
| D PC through second-half fetch check to next hazard | 12.084 ns | Move fetch-fault metadata before D |
| Multiply partial products to registered result | 11.979 ns | Split the wide sum further |

The SRAM-only physical 108 MHz trial passed the UART `T` diagnostic but
failed the 584-byte compressed-instruction fixture: it returned repeated
ROM `DONE`/zero-diagnostic output instead of `RV32C BOARD HOLDS`. The suite
stopped at this first failure, before any FP-RISC guest or SD write. The
48 MHz recovery image was restored and its UART diagnostic passed.

The failed 108 MHz bitstream SHA-256 is
`3f23060d1edb5cb5cdb4dd045c1b0ad28da39db71c340a2386210c2a5b364c8d`.
Frozen final CPU RTL SHA-256 is
`3b2a9fe155b15806517c575a91c285aa5060f5171861d0ccfd28d270d980fcc0`.
Physical evidence is in sibling FP-RISC's
`build/tangnano20k-clock108/board/results.json` and `rv32c.log`.

## 72/54 MHz checkpoint

The final frozen CPU RTL was rebuilt with CPU/cache at 72 MHz and the same
54 MHz memory controller and 135-degree SDRAM clock. Gowin reports:

| Item | Result |
| --- | --- |
| CPU target / reported Fmax | 72 / 72.018 MHz |
| Worst setup slack | +0.003 ns |
| Setup / hold violating endpoints | 0 / 0 |
| Minimum hold slack | +0.225 ns |
| Memory target / reported Fmax | 54 / 121.169 MHz |
| Logic / registers / BSRAM / DSP | 12,933 (63%) / 5,036 (32%) / 5 / 3 |

All three clock constraints and both narrow synchronizer exceptions are
active. The build's timing guard passes, including cross-clock payload
setup/hold checks. The **3 ps setup margin is very small**. The existing
27 MHz PLL-reference routing warning remains; neither this report nor the
bounded physical tests establish long-term voltage/temperature margin.

The SRAM-only physical suite passed all 12 cases:

- UART diagnostics, compressed instructions and precise traps.
- 1 MiB RAM patterns, all SDRAM banks, last-word accesses and byte lanes.
- FP-RISC RV32 numeric/software-float cases, the complete I32 fixture and
  UART virtual-host services with independently checked host-file contents.
- Two decision-tree runs matching independent Python shape, prediction
  digests, root split and accuracy checks. Both exited with `FPR EXIT 0`
  and `DONE`.
- FP-RISC created `CLKTEST.BIN`, containing 512 deterministic binary bytes,
  in the existing FAT16 test partition at a 3 MHz SD SPI clock. It verified
  nine exact reads and refused an overwrite, then repeated the checks in a
  fresh uploaded guest. The earlier 60-byte `FPRTEST.TXT` remained readable.
- A separate read-only guest captured nine physical card sectors. An
  independent Python FAT/root decoder verified both file payloads, both
  inspected FAT copies and the new root entry. The MBR, FAT boot sector
  and saved Linux superblock samples at LBA 8194/8195 were unchanged. This
  is sampled Linux preservation, not a complete reread of that volume.

The card was not reformatted or repartitioned during this experiment.
The clock-specific SD fixtures use the existing FP-RISC driver and C HAL;
no production driver change was needed. The 72 MHz image remains loaded.

| Same 128/64-row classifier | Time |
| --- | --- |
| Previous 48 MHz image, fresh reference run | 4,856,153 microseconds |
| New 72/54 MHz image, run one | 6,608,712 microseconds |
| New 72/54 MHz image, run two | 6,608,709 microseconds |

The new image takes **36.1% longer**. Registered-only operand forwarding
adds dependency stalls, multiply assembly adds a cycle, and the mailbox
adds refill latency. These are source-based candidates for the regression;
the relative contribution of each was not measured on the board.

A larger 512-training/128-test-row classifier, 16 bins, depth 6 and minimum
leaf 5, reached `DTREE DATA READY` then failed with
`Base panic: Builtin: out of memory`, `FPR EXIT 1`, and `DONE`. It did not
complete classification. This is an explicit guest-heap failure, not
evidence of a clock fault. A subsequent UART diagnostic passed.

The working bitstream is
`output/tangnano20k/sdram-pipeline-c72-m54-sd-fpr-trial3/impl/pnr/sdram_test.fs`,
SHA-256 `49bf20aa59d07b6b480fa7ba80885a1468b4a52a7118b8767c0c0b026301ecf8`.
Its reports are in the same directory. Physical evidence is in sibling
FP-RISC's `build/tangnano20k-clock72/board/results.json`,
`build/tangnano20k-clock72/large-dtree.log` and
`build/tangnano20k-clock72/post-large-uart.log`; a durable summary with
embedded logs and hashes is in
`fprisc/docs/evidence/2026-10-18-tangnano20k-clocks.json`.

To reproduce this checkpoint, set `SDRAM_CPU_FREQ_MHZ=72` in the build
command above. FP-RISC guests must use `FPR_CPU_MHZ=72`; board upload tools
must use `--freq-mhz 72`. The clock-specific SD fixture uses
`SD_DIVIDER=11` to hold SPI at 3 MHz.
