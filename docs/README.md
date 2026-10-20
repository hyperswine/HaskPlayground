# docs/: a chronological record

Kind: index. Every document in this directory is named
`YYYY-MM-DD-NAME.md`, dated by the day it was first written, so a listing of
the directory is the timeline of the project's thinking. Older and newer pages
are not reconciled with each other: a page says what was true or intended when
it was written, and a later page may supersede it without the earlier one
being rewritten. When two pages disagree, the later date wins.

Raw board logs, manifests and hashes that a page cites live under
`evidence/`, in a directory named after the page's date and topic.

Component guides stay beside the code they describe: the repository
[README](../README.md), the [Tang Nano 20K build guide](../boards/tangnano20k/README.md)
with its [SDRAM](../boards/tangnano20k/sdram/README.md) and
[RV64](../boards/tangnano20k/rv64/README.md) notes, the
[Google Chat client](../app/google-chat/README.md) and
[plugin](../plugins/google-chat-readonly/README.md), and the
[terra-bgm](../app/terra-bgm/README.md) score.

## Adding a page

Name it with today's date, add one line to the end of the list below, and say
near the top what kind of page it is and what state of the code it applies to.
Do not rename a page when it changes: its date is when it began, and the record
of what changed goes in dated sections inside it. Do not fold an older page
into a newer one to make them agree; write the newer one and point back.

Pages moved here on 2026-10-20 from the repository root, `examples/` and
`boards/tangnano20k/` keep the date already in their name, or else the date of
their first commit.

## The record, oldest first

| date | page | what it is |
|---|---|---|
| 2026-03-15 | [SPEC-EXAMPLE](2026-03-15-SPEC-EXAMPLE.md) | an example spec: a Haskell program embedded in a markdown page |
| 2026-09-10 | [NEW-EDITOR](2026-09-10-NEW-EDITOR.md) | design note for the NewML cell-based editor: notebooks, version cells, keyboard-driven navigation |
| 2026-09-12 | [NEOMUSIC-DESIGN](2026-09-12-NEOMUSIC-DESIGN.md) | Neo Music Notation and Neo Keyboard design document, draft v0.1 |
| 2026-09-12 | [NEOMUSIC-V1](2026-09-12-NEOMUSIC-V1.md) | the first playable NeoMusic implementation (`Seq`, `Part`, `Piece`); historical since 2026-10-12 |
| 2026-09-16 | [NEWML-DESIGN](2026-09-16-NEWML-DESIGN.md) | NewML design document (written January 2026, committed 2026-09-16) |
| 2026-09-16 | [NEOMUSIC-V2](2026-09-16-NEOMUSIC-V2.md) | NeoMusic v2: velocity, synth voices, stereo mixing and reverb; historical since 2026-10-12 |
| 2026-09-22 | [NEOMUSIC-REVIEW](2026-09-22-NEOMUSIC-REVIEW.md) | review of v0.1, v1 and v2, and the proposal that led to `.neomusic` files and piano sheets |
| 2026-09-22 | [NEOMUSIC-LANGUAGE](2026-09-22-NEOMUSIC-LANGUAGE.md) | the current NeoMusic score language and `.neomusic` file guide: authoring, audio, MIDI, LilyPond |
| 2026-10-05 | [SIMPLE-RISC-ROADMAP](2026-10-05-SIMPLE-RISC-ROADMAP.md) | SimpleRisc roadmap from tested RV32IM to a real RISC-V core, with progress against each step |
| 2026-10-05 | [TANGNANO20K-CLOCK-SWEEP](2026-10-05-TANGNANO20K-CLOCK-SWEEP.md) | clock sweep on the board: 114 MHz passes with the registered two-stage shifter |
| 2026-10-05 | [TANGNANO20K-MACHINE-MODE](2026-10-05-TANGNANO20K-MACHINE-MODE.md) | roadmap step 1: precise exception entry, CSRs and trap return |
| 2026-10-05 | [TANGNANO20K-SYSTEM-BUS](2026-10-05-TANGNANO20K-SYSTEM-BUS.md) | system separation, first slice: an explicit exit device |
| 2026-10-06 | [TANGNANO20K-EXECUTE-TIMING](2026-10-06-TANGNANO20K-EXECUTE-TIMING.md) | registered execution experiment: 108 MHz vendor timing with one extra instruction clock |
| 2026-10-06 | [TANGNANO20K-GOWIN-TIMING](2026-10-06-TANGNANO20K-GOWIN-TIMING.md) | Gowin vendor timing baseline for the registered-data-bus core |
| 2026-10-11 | [TANGNANO20K-BOOT-ROM](2026-10-11-TANGNANO20K-BOOT-ROM.md) | roadmap step 3: boot ROM, RAM at `0x8000_0000` |
| 2026-10-11 | [TANGNANO20K-INTERRUPTS](2026-10-11-TANGNANO20K-INTERRUPTS.md) | roadmap step 2: CLINT, minimal PLIC, machine interrupts, WFI |
| 2026-10-11 | [TANGNANO20K-SD-PPP](2026-10-11-TANGNANO20K-SD-PPP.md) | microSD and PPP over the USB UART on the 96 MHz BRAM image |
| 2026-10-12 | [NEOMUSIC-CURVES](2026-10-12-NEOMUSIC-CURVES.md) | curves within a note; superseded 2026-10-13 by signals |
| 2026-10-12 | [NEOMUSIC-TIMEMAP](2026-10-12-NEOMUSIC-TIMEMAP.md) | time maps: one shared clock, tempo shapes and views |
| 2026-10-12 | [TANGNANO20K-BRANCH-TARGET](2026-10-12-TANGNANO20K-BRANCH-TARGET.md) | branch-target prefetch at ExecuteFinish |
| 2026-10-12 | [TANGNANO20K-DECODE-AHEAD](2026-10-12-TANGNANO20K-DECODE-AHEAD.md) | decode-ahead: the first overlap of two instructions' stages (`pipeline` branch) |
| 2026-10-12 | [TANGNANO20K-FAST-MEMORY](2026-10-12-TANGNANO20K-FAST-MEMORY.md) | the block-RAM load/store fast path |
| 2026-10-12 | [TANGNANO20K-FETCH-OVERLAP](2026-10-12-TANGNANO20K-FETCH-OVERLAP.md) | fetch overlap: prefetching pc + 4 while the RAM port is idle |
| 2026-10-12 | [TANGNANO20K-IO-COMPLEX](2026-10-12-TANGNANO20K-IO-COMPLEX.md) | the I/O complex: UART FIFOs, an I/O port and SPI DMA |
| 2026-10-12 | [TANGNANO20K-MEMORY-PORT](2026-10-12-TANGNANO20K-MEMORY-PORT.md) | the memory port driven from registers: 99 to 117 MHz (`memport` branch) |
| 2026-10-12 | [TANGNANO20K-PIPELINE](2026-10-12-TANGNANO20K-PIPELINE.md) | a real pipeline, first version (`real-pipeline` branch) |
| 2026-10-12 | [TANGNANO20K-REFERENCE-MODEL](2026-10-12-TANGNANO20K-REFERENCE-MODEL.md) | the RV32IM + Zicsr reference model for differential testing |
| 2026-10-12 | [TANGNANO20K-REGFILE](2026-10-12-TANGNANO20K-REGFILE.md) | the register file in RAM, and `main` at 93 MHz |
| 2026-10-12 | [TANGNANO20K-STAGES](2026-10-12-TANGNANO20K-STAGES.md) | stage registers, step 1: decoded control travels with the instruction (`stages` branch) |
| 2026-10-13 | [NEOMUSIC-PRELUDE](2026-10-13-NEOMUSIC-PRELUDE.md) | number parameters and a prelude: conveniences as definitions, not keywords |
| 2026-10-13 | [NEOMUSIC-SIGNALS](2026-10-13-NEOMUSIC-SIGNALS.md) | signals: one algebra for everything that varies over a note |
| 2026-10-14 | [NEOMUSIC-CYCLE](2026-10-14-NEOMUSIC-CYCLE.md) | tempo shapes as Ramp and Cycle |
| 2026-10-14 | [NEOMUSIC-HUMANIZE](2026-10-14-NEOMUSIC-HUMANIZE.md) | humanizing: timing and level variation per part |
| 2026-10-14 | [NEOMUSIC-PATCH](2026-10-14-NEOMUSIC-PATCH.md) | patches as modulation signals |
| 2026-10-14 | [NEOMUSIC-PHRASE](2026-10-14-NEOMUSIC-PHRASE.md) | phrase-level signals across notes |
| 2026-10-14 | [NEOMUSIC-SAMPLES](2026-10-14-NEOMUSIC-SAMPLES.md) | recorded sources (WAV, SFZ) and real rooms |
| 2026-10-14 | [NEOMUSIC-SOUNDS](2026-10-14-NEOMUSIC-SOUNDS.md) | sounds and mix defined in `.neomusic` files |
| 2026-10-14 | [NEOMUSIC-VOICE](2026-10-14-NEOMUSIC-VOICE.md) | voices as signal graphs: sources, shapers, mixing |
| 2026-10-16 | [TANGNANO20K-SDRAM-PIPELINE](2026-10-16-TANGNANO20K-SDRAM-PIPELINE.md) | the pipelined RV32 core over all 8 MiB of SDRAM through a fixed-latency cache port |
| 2026-10-18 | [TANGNANO20K-BASE-LOADER](2026-10-18-TANGNANO20K-BASE-LOADER.md) | the RV32 SDRAM ROM accepts the FP-RISC Base uploader protocol |
| 2026-10-18 | [TANGNANO20K-SDRAM-CLOCKS](2026-10-18-TANGNANO20K-SDRAM-CLOCKS.md) | split CPU/SDRAM clock experiment: 72 MHz CPU over 54 MHz SDRAM |
| 2026-10-18 | [TANGNANO20K-SDRAM-IO](2026-10-18-TANGNANO20K-SDRAM-IO.md) | SDRAM I/O timing model audit: the QN88 profile is incomplete |
| 2026-10-18 | [TANGNANO20K-SDRAM-IO-VENDOR-REQUEST](2026-10-18-TANGNANO20K-SDRAM-IO-VENDOR-REQUEST.md) | the support request sent to Gowin for the missing SDRAM AC timing |
| 2026-10-18 | [TANGNANO20K-SDRAM-PHASE-SWEEP](2026-10-18-TANGNANO20K-SDRAM-PHASE-SWEEP.md) | physical phase sweep at 54 MHz: four passing PLL steps |
| 2026-10-18 | [TANGNANO20K-SDRAM-SD-SPI](2026-10-18-TANGNANO20K-SDRAM-SD-SPI.md) | the microSD SPI byte port on the SDRAM pipeline |
| 2026-10-19 | [TANGNANO20K-ARCH-TESTS](2026-10-19-TANGNANO20K-ARCH-TESTS.md) | roadmap step 4: riscv-arch-test under RISCOF against Spike |
| 2026-10-19 | [TANGNANO20K-INTEGRATION](2026-10-19-TANGNANO20K-INTEGRATION.md) | branch integration, timing and regressions at CPU72/SDRAM54 MHz; evidence in `evidence/2026-10-19-integration/` |
| 2026-10-19 | [TANGNANO20K-SDRAM-CACHE](2026-10-19-TANGNANO20K-SDRAM-CACHE.md) | the 64 KiB cache and asynchronous-FIFO clock bridge: classifier 2.33 times faster |
| 2026-10-19 | [TANGNANO20K-SDRAM-WRITE-UPDATE](2026-10-19-TANGNANO20K-SDRAM-WRITE-UPDATE.md) | write-update on the board: narrow BSRAM slices; evidence in `evidence/2026-10-19-bsram-rdw/` and `evidence/2026-10-19-write-update-board/` |
