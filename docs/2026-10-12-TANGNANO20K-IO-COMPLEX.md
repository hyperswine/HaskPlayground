# The I/O complex: UART FIFOs, an I/O port and SPI DMA (2026-10-12)

The first two steps toward a pipelined core whose I/O is a black box. No
device access blocks the CPU for long any more, and the SD card's data moves
without it. BRAM image only; the SDRAM image maps the I/O window as
unmapped for now.

## Changes

**UART FIFOs.**
- A 16-byte transmit FIFO sits between the CPU and the transmitter. A TX
  store completes as soon as there is room; it no longer waits for the line.
  STATUS bit 0 now means "the FIFO has room".
- A 32-byte receive FIFO feeds the CPU's one-byte holding register. A byte is
  offered only while the register is empty and no other offer is in flight.
- A Ctrl-C that breaks a program flushes everything unread, as the reset
  itself does to the holding register. In the boot ROM, 0x03 is still data.

**An I/O port.**
- Word accesses to `0x1000_1000`–`0x1000_1fff` go to a new bus phase,
  `BusIo`.
- The I/O complex, outside the CPU, reads the request from the CPU's
  registered bus state. It takes the request once and answers from a
  register the next cycle: a valid/ready handshake where the CPU and the
  devices meet only at flip-flops.
- The SPI master moved there from the CPU's device state. New devices plug
  in here without touching the core.

**SPI DMA.**
- The RAM is now a true dual-port block RAM: port A for the CPU, port B for
  the I/O complex.
- The DMA engine moves a byte count between the SPI master and RAM. On
  receive, it can first clock 0xff until the SD start token, which must be
  0xfe; a wrong token is an error. It assembles bytes into little-endian
  words for port B. On send, it reads words from port B and shifts out their
  bytes.
- While it runs, the engine owns the SPI data register. The CPU sends the
  short command bytes itself, starts the DMA, and polls STATUS (or does other
  work).
- `c/sd.c` uses DMA for the 512-byte data phase of word-aligned buffers and
  falls back to byte transfers otherwise.

**Problems found.**
- The engine read RAM a cycle early: its read command leaves the state
  register a cycle after it is decided, and the block RAM answers the cycle
  after that. A Haskell property caught it.
- STATUS showed idle one cycle before the last word's RAM write had left.
  "Not busy" now includes that write.
- Clash emits the dual-port RAM as its own Verilog module. Both build
  scripts now add every generated `.v` file.

## Results

**Haskell.** 46 SimpleRisc properties pass. The new ones cover:
- the FIFO as a bounded queue;
- a program reaching the SPI master through the I/O handshake;
- SPI DMA against a modelled SD card and RAM: receive after a token at 0-20
  waiting bytes, send, and a wrong token.

The unrelated `PsramRegs` model property failed once and then passed three
times in a row; it is flaky and was not touched here.

**Gowin.** With placement mode 1, the image meets 96 MHz: 96.415 MHz, +0.045
ns. That is more margin than the previous image's +0.001 ns. It uses 9,306
logic cells and 3,691 registers.

**Board, at 96 MHz:**

| Check | Result |
|---|---|
| `check_sd.py` (2 runs) | pass; DMA block read 45,437 cycles (0.47 ms, ≈1.08 MB/s) against 96,908 byte by byte; write 72,907 against 123,647; DMA and byte reads identical |
| interrupts, processor, RV32I, RV32M, exceptions, finisher, traps, counters, bus | all pass |
| `check_ppp.py` | PPP HOLDS |
| FP-RISC `tests/check_tangnano20k.py --port` | 12 of 12 |

A DMA block read is now at the SPI line rate: 512 bytes at 8 clocks per bit
is 32,768 cycles, plus the command and the token wait.

## Next

- A completion interrupt for the DMA. This needs a second PLIC source.
- The same I/O port on the SDRAM image.
- The reference model and differential tests, then the pipeline.
