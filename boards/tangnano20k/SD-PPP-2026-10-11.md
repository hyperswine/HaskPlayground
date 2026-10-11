# microSD and PPP on SimpleRisc (2026-10-11)

An experiment: can the Tang Nano 20K's SimpleRisc core use the board's
microSD slot, and reach a host over IP through the existing USB UART? Yes,
on the 96 MHz BRAM image. The SDRAM image does not have the SPI master yet.

## microSD

**Hardware.** A mode-0 SPI master in the core, at `0x1000_1000` (DATA,
STATUS, CTRL). Its pins come from Sipeed's NESTang constraints:

| Signal | Pin | SPI role |
|---|---|---|
| `sd_clk` | 83 | SCK |
| `sd_cmd` | 82 | MOSI |
| `sd_dat0` | 84 | MISO |
| `sd_dat3` | 81 | chip select |

`sd_dat1` and `sd_dat2` are held high. MISO passes through two synchroniser
flip-flops. Every SPI output is a register of the machine state.

**Driver.** `c/sd.c` does the standard SPI-mode card initialisation: CMD0,
CMD8, ACMD41 with high capacity, CMD58, then CSD and CID. It handles v1, v2
and high-capacity cards, with single-block read (CMD17) and write (CMD24).

**On the board.** `check_sd.py` passes 3 of 3 runs on a 64 GB SDXC card
(product `SDABC`, 122,152,960 blocks). Block 0 is an MBR with a Linux
partition (type 0x83), and it is read but not written. A test block 64 MiB
into the card is written, read back and compared at SPI 3, 6 and 12 MHz
(dividers 15, 7, 3).

At 12 MHz, a block read takes about 91,600 cycles (0.95 ms, ≈540 KB/s) and a
write about 118,300 (1.23 ms, ≈415 KB/s). Software byte handling dominates.
24 MHz (divider 1) fails: the synchroniser's two-cycle delay leaves too
little time before the sampling edge. Sampling later would fix it.

The checks overwrite blocks 131,072, 131,073 and 131,080-131,087 of the card.

## PPP

**Board.** `c/net.c` runs IPv4 over PPP on the UART:

- RFC 1662 HDLC-like framing with FCS-16; the UART receive interrupt fills a
  4 KiB ring;
- LCP and IPCP, with the board at 10.0.0.2 and the host at 10.0.0.1;
- ICMP echo;
- UDP echo on port 7;
- a UDP block service on port 5000 that reads and writes SD blocks.

The UART is the only console, so the program prints nothing. It is passive
until the peer's first frame, so a host can reopen the port safely.

**Ctrl-C.** Byte 0x03 resets a running SimpleRisc program, and every PPP
header contains it (the control field). It is safe only because the board
never negotiates the ACCM down: the peer keeps escaping every control
character.

**Test.** `check_ppp.py` is a user-space PPP peer in Python: no root, no
pppd. It passes 3 of 3 runs:

- LCP and IPCP up in 0.02 s;
- ICMP echo at 0-1,400 bytes (RTT 12 ms at 0 B, 309 ms at 1,400 B);
- 20 pings of 1,400 B at 9.3 kB/s each way, on a 110,599-baud line;
- UDP echo;
- the block service: card info, block 0's `55aa` signature, and a
  write/read-back over UDP at about 70 ms per 512-byte read (≈7.5 kB/s, the
  line's limit);
- LCP echo, Protocol-Reject for IPv6CP, and LCP terminate: the program exits
  and the ROM sends DONE;
- zero bad FCS, zero receive overruns.

**With macOS pppd.** pppd needs root, so it is not tested here.
`start_ppp.py` loads the program and leaves the port; its `--help` gives the
pppd command. Whether macOS pppd accepts the non-standard 110,599 baud is
untested. If it does not, the UART divider needs a build with a standard
rate (for example 833 clocks per bit gives 115,246 baud at 96 MHz).

## Build

```bash
GOWIN_PLACE_OPTION=1 FREQ_MHZ=96 boards/tangnano20k/build_gowin.sh
```

Gowin: 96.005 MHz, +0.001 ns worst setup slack, 0 violations. The margin is
now essentially zero. The full BRAM regression passes on this image
(interrupts, processor, RV32I, RV32M, exceptions, finisher, traps,
counters, bus). All 43 SimpleRisc properties pass, including a new SPI
mode-0 property.
