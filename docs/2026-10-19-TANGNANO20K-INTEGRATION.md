# SimpleRisc branch integration, timing and regressions

The banked write-update cache and architecture-test branches are integrated
on `main`. The selected SDRAM RV32IMC build passes internal vendor timing at
CPU72/SDRAM54 MHz and the physical processor/FP-RISC regression. Its worst
setup margin is only **+0.008 ns**. This is an experimental board result;
full SDRAM I/O qualification and the roadmap's 96 MHz target remain open.

## Integration and selected implementation

- `write-update-debug` (`c4914ac`) was fast-forwarded from `f2789ac`.
- `arch-tests` (`992f638`) was merged as `50c744d`.
- The cache now defaults to write-update with 512x32 data banks. The
  architecture testbench now uses that same write policy.
- `SimpleRisc.pipeStepDecodedWith` compares the two possible decode-stage
  consumers before selecting the next hazard bit. The late `dFree`
  backpressure signal selects a one-bit result instead of passing through
  a wide instruction mux and then the source-register comparators.
- The existing experimental E2 action/fast-path retiming is retained:
  action, exception and memory fast-path decisions are registered at the
  E1-to-E2 edge. A smaller variant omitting this retiming passed STA and
  shorter board checks but failed the FP-RISC I32 fixture. No instruction
  latency or ISA change is intended.

The pre-integration patch is preserved locally as
`/tmp/haskplayground-pre-integration-20261019.patch`; the full E2 experiment
is also saved as `output/tangnano20k/integration-20261019/e2-retiming-before-selection.hs`.
Unrelated Google Chat, package, top-level README and ignore-file edits were
kept outside this work.

## Timing and area

Gowin V1.9.12.04, GW2AR-LV18QN88C8/I7, CPU72/SDRAM54 MHz, 135-degree SDRAM
phase, RVC enabled, 64 KiB cache, write-update enabled:

| Candidate | Placement | CPU Fmax | Worst setup | Setup / hold violations | Logic |
|---|---:|---:|---:|---:|---:|
| E2 action retiming | 2 | 69.781 MHz | -0.442 ns | 6 / 0 | — |
| E2 action retiming | 4 | 72.049 MHz | +0.009 ns | 0 / 0 | — |
| **E2 retiming plus early hazard comparisons, selected** | **2** | **72.040 MHz** | **+0.008 ns** | **0 / 0** | **15,827** |
| Early hazard comparisons only, rejected on hardware | 2 | 72.114 MHz | +0.022 ns | 0 / 0 | 14,153 |

The selected image uses 5,113 registers, 37/46 BSRAMs and 3/24 DSP resources.
The previously committed banked-cache image was already timing-closed;
these measurements select among the pending timing experiments, not a
claim of a higher clock than that image. The original failing path passed
through the producer/backpressure logic and the next-decode instruction
mux into `pDBlocked`.

The smaller candidate's I32 failure repeated after reprogramming: the UART
loader's image XOR readback passed, but execution returned only `DONE`
without `FPR EXIT 0`. The harness restored the known-good image. The cause
has not been isolated to core logic, placement or the unqualified memory
interface, so this result rejects that bitstream rather than proving the
E2 retiming is architecturally necessary. The selected larger build passed
the complete board suite and additional I32 repeats.

The timing checker confirms the supplied internal setup/hold constraints.
The report still has no paths for the external `clk_sdram` domain; the
QN88 SDRAM I/O model lacks required bounds. The 33 I/O-model/refusal tests
pass and the strict qualification gate remains in place. The physical
tests below are ambient functional evidence, not electrical/PVT signoff.
See [the I/O audit](2026-10-18-TANGNANO20K-SDRAM-IO.md).

## Regression coverage

The selected source passes the full Cabal test suite, including all
64 SimpleRisc properties. The registered-hazard invariant now runs with
both values of `cpuCompressed`.

Fresh Clash-generated Verilog passes the pinned riscv-arch-test 3.9.1 suite
against Spike through RISCOF:

| Image | Passed | RAM-size exclusions | Failed |
|---|---:|---:|---:|
| SDRAM IMC | 92 | 0 | 0 |
| SDRAM IM | 63 | 0 | 0 |
| BRAM IMC | 85 | 7 | 0 |
| BRAM IM | 56 | 7 | 0 |
| Total | **296** | **14** | **0** |

Only `beq`, `bge`, `bgeu`, `blt`, `bltu`, `bne` and `jal` are allowed to
exceed BRAM capacity. The runner now exits unsuccessfully for incomplete
discovery, missing references, empty/mismatched signatures, unexpected
skips or failed tests. Its five unit tests include a real command-entry
negative control and source/RTL-cache invalidation. Generated RTL is reused
only when source and output hashes agree; use `--regenerate` after changing
installed compiler binaries or dependency builds, which are not hashed.

Additional generated-RTL tests pass for IMC with a 64 KiB cache and CDC,
IMC with a 2 KiB cache, and IM with a 64 KiB cache and CDC. They force eight
long cache misses while timer deadlines expire, check exact resumption of
load-use dependencies, execute a straddling 32-bit instruction in C mode,
and verify that a store on a flushed branch path cannot change a sentinel.
They also cover precise faults, `fence.i`, counters, high RAM, masked
stores, memory backpressure and Ctrl-C during a hold. Corrupting a backend
beat must fail at the expected guest stage in each configuration.

The actual boot ROM passes T/Q/V/H upload, XOR readback and execution using
the real SDRAM controller and functional pin model at 54 MHz, phase 135
degrees, 5.4 ns read-output delay and the normal 868-cycle UART divider.
This pin-model loader test uses one core/memory clock; the separate CDC
tests cover the clock crossing.

The cache regression passes ten scenarios: both write policies, all 32
physical banks with boundary/partial-store preservation under both policies,
tag/data collision poisoning and four deliberate faulty variants. The new
missing-bank-write-select fault is detected. The bridge passes four clock
ratio/phase scenarios and two actual-controller/pin-model scenarios, including
unrelated clocks, posted writes, refresh and reset during a read. Digital
CDC simulation does not model metastability.

## Physical board evidence

The selected bitstream was loaded into FPGA SRAM only. The board passes:

- UART probe, compressed integer instructions, straddled fetches, raw
  illegal-instruction traps, MRET, `fence.i` and RAM-boundary cases.
- Processor arithmetic, byte/halfword/word operations, UART input/output,
  Ctrl-C recovery, memory clearing and reload.
- High-address execution, a 1 MiB working set, all SDRAM banks, last word
  and byte lanes; instruction coherence at 590,208 cycles (8.197 ms).
- 270 counter/retirement/trap assertions, five sets of 27 interrupt checks
  with 67 bytes of interrupt-driven UART traffic per set.
- 38,400 RV32I and 30,720 RV32M comparisons.
- FP-RISC numeric, I32 and host-service fixtures, including verified host
  file output, and three independently checked classifier executions.

The classifier remains approximately **2.059 seconds**: training accuracy
124/128 with digest 21562, test accuracy 61/64 with digest 909572. Exact times
and frozen binary hashes are in the evidence manifest. This retains the
banked write-update improvement (about 1.38x against the preceding 2.837 s
write-invalidate image) on this workload. It is not a general application
speedup claim.

No SD-card cases or writes were performed. BRAM variants were freshly
simulated, not newly placed or tested physically. The selected SRAM image
remains on the board; a power cycle removes it.

## Evidence and reproduction

Compact evidence is in
[`evidence/2026-10-19-integration/`](evidence/2026-10-19-integration/manifest.json).
It includes source/RTL/bitstream hashes, the architecture result matrix,
cache results, physical case commands/results, and selected logs. Full local
builds and logs are under `output/tangnano20k/integration-20261019/`;
the selected vendor build is `timing-precompare-p2/`. The rejected smaller
experiment is under `compare-only/`; its board failure is under
`board-selected/` (the directory name predates its rejection).

Run from the repository root, with the existing Cabal/Clash, RISC-V GCC,
RISCOF/Spike, Icarus, Verilator and Gowin installations:

```sh
cabal test haskplayground-test --test-show-details=direct
python3 -m unittest discover -s boards/tangnano20k/arch-tests -p 'test_*.py'
python3 boards/tangnano20k/sdram/check_pipeline_cache.py
python3 boards/tangnano20k/sdram/check_clock_bridge.py
python3 boards/tangnano20k/sdram/check_io_timing.py
python3 boards/tangnano20k/arch-tests/run_arch_tests.py --out output/arch-tests --jobs 4

python3 boards/tangnano20k/sdram/check_pipeline_rtl.py --compressed --cdc-bridge \
  --simulator verilator --generated output/arch-tests/gen/SdramPipeline-1/SdramPipeline.topEntity
python3 boards/tangnano20k/sdram/check_pipeline_rtl.py --compressed --cache-line-bits 7 \
  --simulator verilator --generated output/arch-tests/gen/SdramPipeline-1/SdramPipeline.topEntity
python3 boards/tangnano20k/sdram/check_pipeline_rtl.py --cdc-bridge \
  --simulator verilator --generated output/arch-tests/gen/SdramPipeline-0/SdramPipeline.topEntity
python3 boards/tangnano20k/sdram/check_pipeline_rtl.py --compressed --simulator verilator \
  --generated output/arch-tests/gen/SdramPipeline-1/SdramPipeline.topEntity \
  --boot-loader --loader-command H --pin-model --pin-freq-mhz 54 \
  --pin-phase-degrees 135 --pin-tac-ns 5.4

SDRAM_CPU_FREQ_MHZ=72 SDRAM_FREQ_MHZ=54 SDRAM_RVC=1 \
  SDRAM_CACHE_LINE_BITS=12 SDRAM_PLACE_OPTION=2 SDRAM_PHASE_DEGREES=135 \
  SDRAM_RTL_FILE="$PWD/output/arch-tests/gen/SdramPipeline-1/SdramPipeline.topEntity/sdram_pipeline_risc.v" \
  SDRAM_OUT="$PWD/output/tangnano20k/integration-rebuild" \
  boards/tangnano20k/sdram/build.sh pipeline
```

Physical case commands and binary hashes are recorded in `board-results.json`
and `manifest.json`. The archived generated RTL and the final current-source
RTL differ only in source-location comments, which is verified by a
comment-stripped byte comparison in the manifest. The bitstream hash ties
the physical run to the measured vendor timing report.

## Follow-up: larger FP-RISC regression, 2026-10-20

The 72/54 MHz acceptance above applies to the recorded smaller workload.
A new 512-row classifier repeated ten times with FP-RISC's opt-in collector
exposes intermittent wrong trees/prediction digests on this same bitstream,
despite a successful guest exit. Do not extend its acceptance to this larger
workload. The exact failing ELF passes all ten iterations in QEMU after only
its UART output instructions are adapted, preserving every other byte/address.

A fresh build of the same E2-retimed RTL at CPU/SDRAM 48/48 MHz, 135-degree
SDRAM phase, RVC, 64 KiB banked write-update cache and placement option 2
passes all ten physical iterations. Reported internal Fmax is 55.269 MHz,
worst setup slack +2.740 ns, zero setup/hold violating endpoints. External
SDRAM I/O/PVT qualification is still incomplete. This narrows the failure to
an execution-profile difference; timing, CDC ratio, placement and memory
behavior have not been isolated as causes.

The accepted larger-workload image is
`output/tangnano20k/gc-validation-c48-20261020/impl/pnr/sdram_test.fs`, SHA256
`032849eeb6fe291eb8b73038ed0ef88f9ffacfe24e1e834342672683ac539f40`.
The board is left on this 48/48 MHz volatile SRAM image. No flash/SD writes
were performed. See the sibling FP-RISC
[reclamation report](../../fprisc/docs/2026-10-20-TANGNANO20K-GC.md)
and its tracked evidence for both the passing run and rejected 72 MHz run.

Rebuild from HaskPlayground:

```sh
SDRAM_CPU_FREQ_MHZ=48 SDRAM_FREQ_MHZ=48 SDRAM_RVC=1 \
  SDRAM_CACHE_LINE_BITS=12 SDRAM_PLACE_OPTION=2 SDRAM_PHASE_DEGREES=135 \
  SDRAM_RTL_FILE="$PWD/output/tangnano20k/sdram-generated/SdramPipeline-1/SdramPipeline.topEntity/sdram_pipeline_risc.v" \
  SDRAM_OUT="$PWD/output/tangnano20k/gc-validation-c48-20261020" \
  boards/tangnano20k/sdram/build.sh pipeline
```
