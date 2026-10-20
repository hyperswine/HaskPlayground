# SDRAM I/O timing model audit, 2026-10-18

The build now records a sourced SDRAM timing model before synthesis and can
emit explicit minimum/maximum I/O delays. The supplied QN88 profile remains
**incomplete**. No missing value is replaced by zero, the opposite bound, or
a value from a different memory. This work does not qualify a faster clock.

## Source investigation

- Installed Gowin V1.9.12.04 includes a GW2AR embedded-SDRAM testbench, but no
  memory model. Its simulation readme refers to a generic Micron
  MT48LC8M16A2 model; the testbench instantiates two 16-bit instances. This
  does not identify or characterize the actual QN88 embedded 32-bit SDRAM.
- Downloaded the [official GW2AR reference design](https://www.gowinsemi.com/upload/database_doc/2058/document/64f5457822ba5.rar).
  Its documented target is `GW2AR-18LV7LQ144C`, not this board's QN88 C8/I7.
  Its SDC has three clock definitions and no input delay, output delay or
  clock uncertainty constraints. The archive supplies no memory AC timing
  table or standalone memory simulation model.
- [IPUG279-1.4E](https://www.gowinsemi.com/upload/database_doc/2057/document/6820d99fcc4d1.pdf),
  section 6.2, recommends adjusting SDRAM clock phase to meet the read/write
  windows. Its note about reserved embedded-SDRAM port names removes the
  need for `.cst` pin assignments; it does not supply an SDC I/O timing model.
- [DS226-2.7E](https://cdn.gowinsemi.com.cn/DS226E.pdf), section 2.2.1,
  lists a 5.4 ns access-time feature. It does not establish all of the
  mode-specific min/max bounds needed below. That feature is retained as
  metadata, not substituted for a qualified `t_ac_max`.

Download and inspected-file hashes are retained in
[the investigation record](evidence/2026-10-18-sdram-io-audit.json).
The [vendor request](2026-10-18-TANGNANO20K-SDRAM-IO-VENDOR-REQUEST.md) specifies the missing
information and exact board/controller configuration. It was sent to Gowin
support on 2026-10-18; the Sent label, recipient and subject were verified.

## Model and constraints

`io_timing_gw2ar18_qn88.json` contains 20 timing bounds plus qualification
metadata. A bound needs a finite nonnegative value, `status: verified`, and
a source reference. Qualification must name the operating conditions,
frequency range, CAS/burst mode and what the STA boundary already includes.
All 20 numerical bounds currently remain unknown.

The reference plane is the FPGA SDRAM I/O boundary. A generated clock alias
at `O_sdram_clk` derives from the already shifted PLL clock; the 135-degree
phase must not be added twice. Gowin already propagates `clk_sdram` onto that
net, so the additional named alias requires `-add` and an explicit master.
Routed clock propagation must still be inspected in a real qualified build.

Let C be the unmodeled clock flight delay to the memory, R the unmodeled
read-data return delay, and W the unmodeled output flight delay. The
generator derives:

| SDC bound | Expression |
| --- | --- |
| DQ input maximum | Cmax + tACmax + Rmax |
| DQ input minimum | Cmin + tOHmin + Rmin |
| Output maximum | tSU + Wmax - Cmin |
| Output minimum | Wmin - Cmax - tH |

Command/address, write-data and DQM outputs use separate setup/hold and
flight-delay values. Negative output minima are retained. Setup and hold
uncertainty are applied in both memory-clock/I/O-clock directions, using
the supplied relative uncertainty budget rather than guessed defaults.
Both min and max are always emitted: Gowin uses the specified value for
the omitted bound, which is inappropriate for this interface.

There are 32 read inputs and 52 active output bits: 32 DQ, 11 address,
2 bank, 3 command and 4 DQM. In this RTL CKE is tied high and CS_n low;
they are documented static outputs. These counts are intended coverage,
not proof that the routed analyzer timed each bit.

## Build use

Ordinary builds audit the pending profile and explicitly report that they
check internal FPGA timing only. They write `sdram-io-model.json` beside the
bitstream build. This leaves the existing internal STA guard in operation.
Its output now explicitly excludes full SDRAM I/O signoff.

Require a complete verified model before CPU generation or synthesis:

```sh
SDRAM_REQUIRE_IO_MODEL=1 SDRAM_CPU_FREQ_MHZ=72 SDRAM_FREQ_MHZ=54 \
  SDRAM_RVC=1 boards/tangnano20k/sdram/build.sh pipeline
```

This currently **fails deliberately** and lists the unknown bounds. Once
Gowin supplies applicable limits, populate a separate sourced profile:

```sh
SDRAM_IO_PROFILE=/absolute/path/to/verified-qn88-profile.json \
  SDRAM_CPU_FREQ_MHZ=72 SDRAM_FREQ_MHZ=54 SDRAM_RVC=1 \
  boards/tangnano20k/sdram/build.sh pipeline
```

Supplying a profile requires it to be complete. The build appends its
`sdram-io.sdc` to the clock constraints. Invalid or incomplete runs remove
stale generated I/O constraints. A complete numerical model still records
`signoff: false`: numerical completeness alone does not establish coverage.

The profile's CAS/burst qualification must match the selected image:
pipeline/core/burst-test use CAS2/burst4, test/serial use CAS2/burst1.

## Verification and remaining work

```sh
python3 boards/tangnano20k/sdram/check_io_timing.py
```

Thirty-three focused cases pass, including independent flight-delay arithmetic,
negative output minima, each missing bound, invalid numbers and provenance,
frequency/mode refusals, stale-file removal, static-port coverage changes,
non-finite frequency rejection, build preflight before Clash and matching
PLL/SDC phases in the single-clock and split-clock configurations.
Artificial test values exist only under ignored `output/` and are explicitly
not memory specifications.

The standalone Gowin syntax probe uses the actual SDRAM tester with these
artificial values, without programming the board. Its result is recorded in
the investigation evidence. The initial probe reached SDC analysis and showed
that the existing propagated clock makes an alias without `-add` get ignored.
The generator now supplies `-add` and an explicit master. Attempts to validate
that correction, including a retry after sending the vendor request, stopped
at license verification with the server not responding. **The corrected SDC
has not yet passed Gowin validation.** Unit
and build-generation tests pass; this remains a provisional generator until
the vendor check can run. Parsing a constraint will still be insufficient:
all intended I/O paths and applied constraints must be checked in the report.

After the vendor response, a qualified run must also check clock-output
propagation, all active input/output bits, ignored or unconstrained objects,
setup/hold at every corner, clock pulse widths and relative phase uncertainty.
The controller/pin model must separately cover output-enable tLZ/tHZ and bus
turnaround, first/last burst beats, command spacing, refresh and initialization.
Then sweep clock phase on the physical full-capacity tester before repeating
FP-RISC and SD acceptance. The previous 72/54 MHz image is unchanged by this
audit. That initial audit did not program the board or card. The subsequent
[physical phase sweep](2026-10-18-TANGNANO20K-SDRAM-PHASE-SWEEP.md) used FPGA SRAM only,
found the passing 54 MHz steps and restored the tested processor image;
the SD card was not accessed.

## Local phase experiments while waiting for the vendor

`SDRAM_PHASE_DEGREES` now accepts the rPLL's 22.5-degree steps from 0 through
337.5 degrees. The generated PLL phase bits, SDC phase and model-audit
metadata agree. Defaults remain 135 degrees for burst modes and 225 degrees
for the legacy single-word modes. Both the single-clock and split-clock
builders use the same requested memory phase.

Two additional simulation checks need no Gowin synthesis license:

```sh
python3 boards/tangnano20k/sdram/check_pll_phases.py
python3 boards/tangnano20k/sdram/check_phase_window.py
```

The vendor `gw2a/prim_sim.v` PLL model passes 12 cases: six phases from
0 through 337.5 degrees, each at single-clock 54 MHz and split CPU72/memory54.
Each checks eight locked samples, CPU and memory periods, and memory-to-SDRAM
phase separation. This checks configuration and functional clock generation;
the model does not establish physical jitter or phase error.

The phase/access-delay suite reuses the independent command/pin oracle and
all-bank, masking and refresh checks. It covers six phases with artificial
6 and 12 ns access delays. Expected read failures are required to be actual
word/beat mismatches; initialization, compilation and timeout failures cannot
count as successful negative tests. Delayed nonblocking clock assignments
preserve edges above 180 degrees; continuous-assignment inertial delays can
otherwise suppress those clock pulses in simulation.

| Phase at 54 MHz | Nominal command/write edge spacing | Nominal read edge spacing |
| --- | ---: | ---: |
| 90 degrees | 4.630 ns | 13.889 ns |
| 112.5 degrees | 5.787 ns | 12.731 ns |
| 135 degrees | 6.944 ns | 11.574 ns |
| 157.5 degrees | 8.102 ns | 10.417 ns |
| 180 degrees | 9.259 ns | 9.259 ns |
| 225 degrees | 11.574 ns | 6.944 ns |

An earlier phase extends the read budget while reducing command/write setup
time. The artificial 12 ns case is intended to cross the ideal read boundary;
it is not an SDRAM limit. Command/write setup/hold, tOH minimum, routed I/O,
package skew and jitter are still absent from this functional model. In
particular a simulated 225-degree/6 ns pass cannot override the historical
physical 225-degree failure.

After license access is restored, a candidate standalone full-memory build is:

```sh
SDRAM_FREQ_MHZ=54 SDRAM_PHASE_DEGREES=112.5 \
  SDRAM_OUT="$PWD/output/tangnano20k/sdram-phase-p112.5" \
  boards/tangnano20k/sdram/build.sh burst-test
```

This is an experiment, not a supported phase selection. Check each exact
placement's STA before programming SRAM, test all six full-memory patterns,
and restore the known working processor image after the sweep. No physical
phase sweep was performed in this follow-up. Raw simulation results are under
`output/tangnano20k/sdram-{pll-phases,phase-window}/results.json`; the durable
investigation record embeds both results and source hashes.
