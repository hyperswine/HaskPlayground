# Physical SDRAM phase sweep, 2026-10-18

At **54 MHz, CAS2/burst4**, the standalone full-capacity tester passes at
**90, 112.5, 135 and 157.5 degrees**. All twelve other available PLL steps
fail. The four passing points each completed two separate SRAM programming
and test runs, with **12,582,912 comparisons per run and zero mismatches**.
The measured window supports retaining the current 135-degree setting.
It does not qualify a higher memory or processor frequency.

## Method and results

The first run tested 135, 90, 112.5, 157.5, 180 and 225 degrees. The second
tested every step from 0 through 337.5 degrees in ascending order. The six
repeated points agreed on pass/fail. There were 22 physical runs in total;
the eight passing runs accumulated **100,663,296 zero-error comparisons**.

Each passing run covers all 2,097,152 words / 8 MiB with zeros, ones,
address hash, complemented hash, four independently masked byte lanes and
reverse-order readback after 250 ms retention with refresh. The hardware
halts after its first failing pattern; every failing phase here stopped
after the zero pattern's 2,097,152 comparisons.

| Clock phase | Full-range run | Mismatches at stop | Initial run |
| ---: | --- | ---: | --- |
| 0 | Fail, zero pattern | 2,097,152 | — |
| 22.5 | Fail, zero pattern | 2,064,385 | — |
| 45 | Fail, zero pattern | 2,097,152 | — |
| 67.5 | Fail, zero pattern | 789 | — |
| 90 | Pass, all six patterns | 0 | Pass |
| 112.5 | Pass, all six patterns | 0 | Pass |
| 135 | Pass, all six patterns | 0 | Pass |
| 157.5 | Pass, all six patterns | 0 | Pass |
| 180 | Fail, zero pattern | 1 | Fail, 19,419 |
| 202.5 | Fail, zero pattern | 300,406 | — |
| 225 | Fail, zero pattern | 1 | Fail, 19,419 |
| 247.5 | Fail, zero pattern | 1 | — |
| 270 | Fail, zero pattern | 19,419 | — |
| 292.5 | Fail, zero pattern | 1 | — |
| 315 | Fail, zero pattern | 19,419 | — |
| 337.5 | Fail, zero pattern | 19,419 | — |

The first failing word was byte address zero in each failing run. Expected
was zero; actual values and raw UART packets are in the evidence. Error
counts differ between repeats. SRAM reconfiguration does not erase the
SDRAM, and a faulty write/read path can expose preceding contents. These
counts are therefore diagnostic observations, not a normalized bit-error
rate or proof of which read/write edge failed. Even one mismatch rejects
that phase. Later patterns were not attempted at failing points.

The observed transitions are between 67.5 and 90 degrees, and between
157.5 and 180 degrees. The PLL step is 22.5 degrees, approximately 1.157 ns
at 54 MHz. The exact electrical boundaries remain unknown. Of the tested
steps, 112.5 and 135 degrees sit nearest the observed window's midpoint;
the previously accepted CPU image already uses 135 degrees.

## Why every image still passes internal STA

All sixteen independently generated images pass Gowin V1.9.12.04 internal
STA: 54 MHz constraint, **107.153 MHz reported Fmax, +9.186 ns worst setup
slack, zero setup and hold violating endpoints**. The report uses the
Slow 0.95 V / 85 C setup model and Fast 1.05 V / 0 C hold model.
All sixteen bitstreams have distinct hashes. A separate check confirms the
requested phase in each routed netlist's rPLL `PSDA_SEL` and in the timing
report's generated SDRAM clock edge.

The report explicitly says `No timing paths to get frequency of clk_sdram!`.
The incomplete default I/O profile supplies no guessed input/output delays.
Consequently the common 107.153 MHz result describes the tester's internal
logic; it does not describe the SDRAM's maximum working clock. Changing
the external memory phase moves the physical read/write windows without
changing that internal result. This experiment directly demonstrates why
internal timing closure alone cannot establish SDRAM operation.

Read access/hold, command/write setup/hold, package delays, relative PLL
jitter and output-enable turnaround still need the applicable Gowin limits
and complete coverage described in the [I/O audit](2026-10-18-TANGNANO20K-SDRAM-IO.md).
The vendor request remains pending. No supply or temperature measurements
were made; this is one board at ambient conditions, without PVT qualification.
The CPU image also has a different placement, so this standalone window is
not a quantified I/O margin for that image.

## Recovery and retained evidence

Only FPGA **SRAM** was programmed. FPGA flash was unchanged. The SD card
was not accessed. After each run set, the runner restored the tested
CPU72 / SDRAM54 / 135-degree processor image and passed the UART diagnostic.
After the final restoration, the existing FP-RISC RV32 numeric fixture
also passed I64, software-float and Base numeric checks, exited zero and
returned `DONE`.

Restored processor image SHA256:
`49bf20aa59d07b6b480fa7ba80885a1468b4a52a7118b8767c0c0b026301ecf8`.

The [committable evidence](evidence/2026-10-18-sdram-phase-sweep.json) retains
all 22 decoded report sets, raw UART bytes, bitstream/build-input/constraint/
timing-report hashes, routed phase checks, programmer and recovery logs,
and the final FP-RISC output. Full build artifacts and logs remain under
`output/tangnano20k/sdram-phase-sweep-2026-10-18/` (ignored by Git).
Three builds initially hit the intermittent license-server failure;
their retained failed-attempt logs are followed by successful rebuilds.
Only images passing the timing guard were programmed.

## Repeating the experiment

Build each phase in its own new output directory, for example:

```sh
SDRAM_FREQ_MHZ=54 SDRAM_PHASE_DEGREES=112.5 \
  SDRAM_OUT="$PWD/output/tangnano20k/new-sweep/p112.5" \
  boards/tangnano20k/sdram/build.sh burst-test
```

Repeat for all requested phases. `run_phase_sweep.py` consumes these prebuilt
`p<degrees>` directories, checks their timing and configuration before USB
access, saves each result and restores the supplied hash-verified CPU image
in a `finally` block. It refuses to overwrite an existing results file.
For the full grid:

```sh
python3 boards/tangnano20k/sdram/run_phase_sweep.py \
  --out output/tangnano20k/new-sweep \
  --phases 0 22.5 45 67.5 90 112.5 135 157.5 180 202.5 225 247.5 270 292.5 315 337.5 \
  --loader /Users/jasonqin/Documents/Libs/oss-cad-suite/bin/openFPGALoader \
  --restore output/tangnano20k/sdram-pipeline-c72-m54-sd-fpr-trial3/impl/pnr/sdram_test.fs \
  --restore-sha256 49bf20aa59d07b6b480fa7ba80885a1468b4a52a7118b8767c0c0b026301ecf8 \
  --probe-runner /Users/jasonqin/Documents/GitHub/fprisc/tools/run_tangnano20k_base.py
```

A `memory_fail` is a completed experimental measurement. Missing, malformed
or incomplete reports interrupt the sweep and still trigger recovery.
`complete: true` records that all requested measurements finished and CPU
recovery passed; inspect each case's status for memory pass/fail.

Before increasing clocks, apply the vendor I/O bounds to the exact CPU image
and repeat the memory/application checks there. This result alone does not
resolve the separate 108 MHz CPU timing failures.
