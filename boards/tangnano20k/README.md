# Tang Nano 20K

Build and run the Clash designs on a Sipeed Tang Nano 20K (Gowin
GW2AR-LV18QN88C8/I7) with the open-source toolchain (oss-cad-suite: yosys,
nextpnr-himbaechel, gowin_pack, openFPGALoader).

| Design | Source | Build script | Clock |
|---|---|---|---|
| SimpleRisc (RV32IM, 64 KiB RAM) | `src/SimpleRisc.hs` | `build.sh` | PLL, default 96 MHz |
| SimpleRisc SDRAM (8 MiB, experimental) | `src/SdramSimpleRisc.hs` | `sdram/build.sh core` (Gowin) | 54 MHz |
| PsramRegs | `src/PsramRegs.hs` | `build_psram.sh` | 27 MHz oscillator |

```bash
boards/tangnano20k/build.sh
```

```bash
boards/tangnano20k/build.sh load
```

`load` writes FPGA SRAM only; the design is gone after a power cycle. Set
`OSS_CAD_SUITE` if the suite is not at `~/Documents/Libs/oss-cad-suite`.

## Pins

From `tangnano20k.cst`: clock 4 (27 MHz), S1 button 88 (active high, resets
the core), UART RX 70 and UART TX 69 (both wired to the on-board BL616
USB bridge).

## Talking to the board

The BL616 shows up as two serial ports, for example on macOS:

| Port | Function |
|---|---|
| `/dev/cu.usbserial-XXXXXXXX0` | JTAG (used by openFPGALoader) |
| `/dev/cu.usbserial-XXXXXXXX1` | UART to the FPGA |

SimpleRisc's UART divider is 868 clocks per bit, so the baud rate follows the
core clock: `FREQ_MHZ * 1e6 / 868`. That is 115200 at 100 MHz but, for example,
110599 at the 96 MHz default, which standard terminals do not offer. On macOS
set an arbitrary rate by configuring the port with `tcsetattr` at a standard
rate and then calling the `IOSSIOSPEED` ioctl (`0x80085402`) with the exact
rate. Setting a non-standard speed through `tcsetattr` alone fails with
`EINVAL`.

## Running C programs

`c/` holds a minimal C runtime for SimpleRisc: `crt0.S` (sets `sp` and `gp`,
zeroes `.bss`, calls `main`, then writes the finisher at `0x00100000`,
which stops execution and sends "DONE"), `link.ld` (64 KiB at address 0, stack at the top) and a UART driver
(`uart.h`, `uart.c`). Programs build with `rv32im_zicsr/ilp32` support in
`riscv64-unknown-elf-gcc`, with no libc or libgcc. Exit preserves `mtvec`; installed guest trap handlers cannot intercept the
finisher. Older ECALL-based binaries retain temporary compatibility.

SimpleRisc supports the six Zicsr instructions, direct-mode machine traps,
`mret`, and `wfi` as a no-op until interrupts are implemented. The trap/status,
scratch and identification CSRs are available, together with writable 64-bit
`mcycle`/`minstret` counters and their read-only `cycle`/`instret` aliases.
Unknown CSRs and writes to read-only CSRs trap. See
[MACHINE-MODE-2026-10-05.md](MACHINE-MODE-2026-10-05.md) for implementation
boundaries and measured board results. `check_traps.py` runs a guest C handler
that checks CSR operations, trap cause/PC/value and return status.
`check_counters.py` checks counter rollover, half writes, aliases, exact
retirement and faulting instructions that must not retire.
`check_finisher.py` verifies explicit success/failure exits with a nonzero
trap vector and an infinite loop after the store; see
[SYSTEM-BUS-2026-10-05.md](SYSTEM-BUS-2026-10-05.md).
`check_bus.py --freq-mhz 96` checks mixed-width RAM traffic, signed UART RX
loads and single consumption through the registered data bus on three runs.

```bash
boards/tangnano20k/c/build.sh hello
```

```bash
boards/tangnano20k/run_program.py output/tangnano20k/c/hello.bin
```

`run_program.py` loads the image with the `P`/`R` host protocol, sends
`--input` text, and prints the output until "DONE". It keeps one port session
open throughout (see the bridge issue below); `--freq-mhz` sets the baud rate
for a non-default clock.

To run the same program in simulation instead, write the host byte stream to
a file and replay it through the core's real UART loader with `tb_program.v`:

```bash
boards/tangnano20k/run_program.py output/tangnano20k/c/hello.bin --frame /tmp/hello.hex
```

```bash
iverilog -o /tmp/tb.vvp boards/tangnano20k/tb_program.v output/tangnano20k/clash/SimpleRisc.topEntity/simple_risc.v
```

```bash
vvp -n /tmp/tb.vvp +frame=/tmp/hello.hex
```

Loading runs at UART speed, so a 750-byte image takes about 3 minutes to
simulate. `build.sh` writes the Verilog; to generate only the Verilog, run
`stack exec --package clash-ghc -- clash src/SimpleRisc.hs --verilog -outputdir output/tangnano20k/clash`.

## Clock speed: nextpnr is optimistic

nextpnr's timing model for Gowin parts is only an estimate, and it has been
consistently optimistic for this design. The board is the only reliable
judge:

| Design | nextpnr fmax | Works on the board | Failures / limits |
|---|---|---|---|
| 1 cycle per instruction (early version) | 52 MHz | 27 MHz | 51 MHz |
| 2 cycles per instruction | 63 MHz | 51 MHz | not tested higher |
| 5 cycles per instruction (previous; retested 2026-10-05) | ~126 MHz | 51 MHz | 96 MHz |
| 8 cycles per instruction (earlier 2026-10-05) | 149.08 MHz, seed 2 | 96 MHz with the earlier suite; expanded ALU test passes at 51 MHz | expanded ALU test fails at 96, 102 and 108 MHz |
| 8 cycles ordinary / 9 cycles shifts (clock sweep, 2026-10-05) | 150.58 MHz, seed 2 | 108 and 114 MHz | 117 MHz: incorrect sieve; 120 MHz: subtraction failure |
| machine traps, Zicsr and split counters (2026-10-05) | 147.19 MHz, seed 2, route target 144 MHz | 96 MHz: counter, trap, CPU, RV32IM and FP-RISC suites | seed 1 at target 115.2 MHz loses string characters at 96 MHz despite a reported 142.90 MHz maximum |
| finisher and registered device target (2026-10-05) | 144.61 MHz, seed 3, route target 144 MHz | 96 MHz: finisher, counter, trap, CPU, RV32IM and FP-RISC suites | seed 2 misses the route margin target at 137.84 MHz; not loaded |
| registered data bus (2026-10-05) | 144.61 MHz, seed 7, route target 144 MHz | 96 MHz: bus, finisher, counter, trap, CPU, RV32IM and FP-RISC suites | seeds 3, 2, 1, 4, 5 and 6 miss the margin; not loaded |

`build.sh` therefore places and routes for `FREQ_MHZ * MARGIN`
(`MARGIN=1.5` by default, seed 3 tried first). To test above what timing allows, build with
`ALLOW_FAIL=1 MARGIN=1` and check on the board.

The paths that turned out to be slow on the real chip, well beyond what
nextpnr reported, were long carry chains, the 32-way register-file mux
(Gowin's `MUX2_LUT5`–`MUX2_LUT8` cells), and logic driving the block RAM
directly. `src/SimpleRisc.hs` now uses one-hot CPU stages, two-stage register
reads (four banks of eight), registered writeback, registered RAM commands and
readback, separate load/store alignment registers, and a queued UART request.
The iterative multiply/divide unit registers its 33-bit arithmetic before
committing each step. The barrel shifter now registers its result after the first two shift levels
and finishes the remaining three levels in a separate stage. Ordinary
instructions take nine clocks, shifts take ten; nonzero-divisor
M operations add 68 unit clocks. These changes trade cycles for shorter paths.

Run the hardware regressions after loading a bitstream:

```bash
python3 boards/tangnano20k/check_rv32i.py --freq-mhz 114 --repeat 10
python3 boards/tangnano20k/check_processor.py --freq-mhz 114
python3 boards/tangnano20k/check_rv32m.py --freq-mhz 114 --repeat 20
```

The ALU test checks all ten RV32I register operations, every shift distance,
signed edge cases and randomized operands against Python references (38,400
comparisons across ten loads). The processor test checks five exact C arithmetic
runs, UART echo, Ctrl-C recovery,
memory clear/reload, and 64 passes of byte/halfword/word memory checks. The
second compares all eight RV32M operations with host-generated results for
192 operand pairs per load, including division by zero and signed overflow.
Both keep the UART session open and stop the CPU before closing it.

Verified on 2026-10-05: both hardware commands passed at 96 MHz, including
30,720 RV32M checks across 20 loads. The FP-RISC Tang Nano builtin suite also
passed three smoke runs, seven refusal cases, and recovery on this bitstream.
`stack test --fast` and the generated-Verilog `HiDONE` UART test passed. An
earlier full-suite run exposed an intermittent failure in the unchanged
`test/PsramRegs.hs` snapshot-model property; the processor properties passed
on that run too. The verified SRAM image SHA-256 is
`2563be842bfbabcaf77c6a0e784feff27fc3077d2a28d2b01a4f423ecf51211e`.

The 96 MHz result above is historical coverage, not a pass of the expanded
ALU regression. The clock sweep found shift corruption at 96 MHz on that
image and corrected it with the registered shifter. See
[CLOCK-SWEEP-2026-10-05.md](CLOCK-SWEEP-2026-10-05.md) for the current
114 MHz validation, failed higher clocks, bitstream hashes and replay commands.
The build and host-tool defaults remain 96 MHz; use an explicit `--freq-mhz`
matching the image loaded into the board.

## Known issue: the USB-UART bridge stops working

After some runs the UART port stops returning any data and only unplugging
the board brings it back.

### Symptoms

- The UART port opens normally but never returns a byte, even for a design
  that previously passed every test (for example at 27 MHz).
- Loading a new bitstream does not help.
- The JTAG side keeps working: `openFPGALoader --detect` still finds the
  GW2A and loading still succeeds.
- Unplugging and replugging the board fixes it immediately.

### Cause: reopening the port while the FPGA is sending

**Closing and reopening the UART port while the FPGA is transmitting wedges
the bridge.** It was reproduced on purpose with a SimpleRisc program that
transmits nonstop (`sw` to TXDATA in a loop) at about 110.6k baud:

| Run | Result |
|---|---|
| 3 min flood, port open and read throughout (2 MB at full line rate), then the port closed mid-stream and reopened about 0.1 s later to send Ctrl-C | wedged |
| 5 s flood, port open and read throughout, Ctrl-C sent in the same session | fine: the flood stopped and the next test passed |
| Flood, port closed mid-stream, reopened 5 s later to send Ctrl-C | wedged: the Ctrl-C never reached the FPGA, so the flood kept going, and then all traffic stopped |
| Flood, line settings and baud rate reapplied mid-stream (`tcsetattr` + `IOSSIOSPEED`), port kept open | fine |
| Flood, buffers flushed mid-stream (`tcflush`), port kept open | fine |
| Flood, port closed mid-stream and reopened without changing any settings | wedged |

So data volume is not the trigger, and neither is a long time with nobody
reading: the first run's only gap was about 0.1 s, but it included a
close and reopen. Changing the baud rate or flushing while data arrives is
harmless. The trigger is the close and reopen itself. After the reopen, the
host-to-FPGA direction dies first, and FPGA-to-host traffic stops shortly
after. The JTAG side is a separate USB interface and keeps working. A stuck
macOS driver seems unlikely, because the JTAG port uses the same driver and
was unaffected.

Which part of the close/open sequence the BL616 firmware mishandles is not
known. On macOS, closing the port normally drops the DTR/RTS handshake
lines, and reopening it resets the port to defaults (it read back as
9600 baud).

This also explains the wedges during overclocking sweeps: the test script
opened and closed the port for every test, and a CPU failing at too high a
clock was stuck re-sending one byte (`YNNNN...`), so the next test's open
hit a live stream.

### How to avoid it

- Never close or reopen the UART port while the FPGA may be sending. Stop the
  stream first, from the same open session: send Ctrl-C (`0x03`, which resets
  SimpleRisc's CPU), wait until no more bytes arrive, then close. Changing
  the baud rate within a session is fine.
- Better, keep one session open for a whole test run instead of reopening
  the port per test.
- Before reopening after a failure, load a harmless bitstream first. A
  reload stops the FPGA sending, and reopening after a reload has been
  tested to work. It cannot undo a wedge that already happened, though.
- For long overclocking sweeps, use a separate USB-serial adapter on spare
  FPGA pins instead of the BL616.

### Checking the setup

When a test suddenly returns nothing at all, check the setup before blaming
the design: build the design for 27 MHz, which is known to pass, and run it.
If that also returns nothing, the bridge is stuck; replug the board.

## Gowin vendor analysis

`boards/tangnano20k/build_gowin.sh` runs the licensed macOS vendor flow
without programming the board. The default is 96 MHz; select another PLL
clock with `FREQ_MHZ` (an integer multiple of 3). Reports and the SRAM image
are written to `output/tangnano20k/gowin-$FREQ_MHZ/impl/pnr/`.
`GOWIN_OUT` overrides the output directory and `GOWIN_PLACE_OPTION` selects
vendor placement mode 0 through 4 (default 0).

```bash
FREQ_MHZ=108 boards/tangnano20k/build_gowin.sh
```

The build checks the requested clock and rejects any internal setup or hold
violation. Gowin can generate a bitstream even when timing fails; the separate
`check_gowin_timing.py` guard makes that failure visible to callers.

The historical [vendor baseline](GOWIN-TIMING-2026-10-06.md) failed at
96 MHz (84.444 MHz maximum). The
[execution experiment](EXECUTE-TIMING-2026-10-06.md) records the staged ALU,
split comparisons, registered CSR selection, timing trials and board results.
Ordinary instructions now take nine clocks; this is still a serial core.

## SDRAM main-memory experiment

[The SDRAM report and build instructions](sdram/README.md) describe the
standalone full-capacity test and the separate uncached processor image.
All 8 MiB passed 12,582,912 word comparisons. The processor runs at 54 MHz
with RAM at `0x80000000` and a zero-address compatibility alias; C uses a
1 MiB working set and FP-RISC uses a 2 MiB allocation on this image.
Use `--freq-mhz 54` with board tools and `--ram-base 0x80000000` for high-linked
programs. The legacy loader still limits images to 64 KiB, and `M` clears
only the compatibility 64 KiB. SRAM programming leaves flash unchanged.

## Separate RV64IM / Base host experiment

[rv64/README.md](rv64/README.md) describes the serial 64-bit variant verified at
27 MHz with cached 8 MiB SDRAM. It runs FP-RISC's experimental single-threaded
Base host with UART virtual devices and software floating point, including a
small guest in the existing WASM VM. Use its matching FPGA image and host
clock; the RV32 configurations documented above remain separate.
