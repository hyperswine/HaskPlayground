# Tang Nano 20K

Build and run the Clash designs on a Sipeed Tang Nano 20K (Gowin
GW2AR-LV18QN88C8/I7) with the open-source toolchain (oss-cad-suite: yosys,
nextpnr-himbaechel, gowin_pack, openFPGALoader).

| Design | Source | Build script | Clock |
|---|---|---|---|
| SimpleRisc (RV32I) | `src/SimpleRisc.hs` | `build.sh` | PLL, default 96 MHz |
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

## Clock speed: nextpnr is optimistic

nextpnr's timing model for Gowin parts is only an estimate, and it has been
consistently optimistic for this design. The board is the only reliable
judge:

| Design | nextpnr fmax | Works on the board | Fails on the board |
|---|---|---|---|
| 1 cycle per instruction (early version) | 52 MHz | 27 MHz | 51 MHz |
| 2 cycles per instruction | 63 MHz | 51 MHz | not tested higher |
| 5 cycles per instruction (current) | ~126 MHz | 96 MHz; 102 MHz with some placements | 108 MHz |

`build.sh` therefore places and routes for `FREQ_MHZ * MARGIN`
(`MARGIN=1.2` by default). To test above what timing allows, build with
`ALLOW_FAIL=1 MARGIN=1` and check on the board.

The paths that turned out to be slow on the real chip, well beyond what
nextpnr reported, were long carry chains, the 32-way register-file mux
(Gowin's `MUX2_LUT5`–`MUX2_LUT8` cells), and logic driving the block RAM
directly. `src/SimpleRisc.hs` keeps each of these in a pipeline stage of its
own.

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
