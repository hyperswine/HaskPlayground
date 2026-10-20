# Timer and interrupts (2026-10-11)

This completes roadmap step 2. SimpleRisc now has a CLINT, a minimal PLIC
with the UART as its one source, machine software/timer/external interrupts,
a `mip` CSR backed by real pending bits, and a WFI that waits. Both the BRAM
and the SDRAM images have them.

## Devices

| Address | Register |
|---|---|
| `0x0200_0000` | CLINT `msip` (bit 0) |
| `0x0200_4000` / `0x0200_4004` | CLINT `mtimecmp` low / high |
| `0x0200_bff8` / `0x0200_bffc` | CLINT `mtime` low / high (writable) |
| `0x0c00_0028` | PLIC priority of source 10 (3 bits) |
| `0x0c00_1000` | PLIC pending (bit 10, read-only) |
| `0x0c00_2000` | PLIC enable, context 0 (bit 10) |
| `0x0c20_0000` | PLIC threshold (3 bits) |
| `0x0c20_0004` | PLIC claim (read) / complete (write 10) |
| `0x1000_000c` | UART CTRL: bit 0 interrupt while a byte is waiting, bit 1 while the transmitter is ready |

The layout is QEMU `virt`'s, and the UART is source 10 as on `virt`. These
registers are word-only; byte and halfword accesses fault.

Decisions (roadmap open decisions 2 and 3): `mtime` ticks once per core
clock, like `mcycle`, so programs scale by the clock frequency; the UART goes
through a PLIC so later devices need no new software interface.

**`mtime`** is a split 64-bit counter like `mcycle`. The SDRAM wrapper ticks
it during memory waits too, so it counts every clock on both images.
`mtime >= mtimecmp` is computed in two registered stages: the halves are
compared, then combined into MTIP.

**PLIC.** The gateway level is `(RXIE and a byte waiting) or (TXIE and the
transmitter ready)`. A level sets pending unless the source is already in
service. A claim returns 10 when the source is pending, enabled and its
priority exceeds the threshold (otherwise 0); it clears pending and marks the
source in service. Writing 10 to complete ends service, and a level still
high then sets pending again. MEIP is the registered claimable condition.

## Core

- **Taking interrupts.** In Fetch, at instruction boundaries only, the
  highest-priority interrupt with its `mip` and `mie` bits set is taken if
  `mstatus.MIE` is set; the priority order is external, software, timer. The
  trap has `mcause` bit 31 set, `mepc` = the instruction about to be fetched
  and `mtval` 0. A multiply/divide or bus wait in progress always finishes
  first.
- **WFI** waits in Commit until `mie & mip` is nonzero, whether or not MIE is
  set, so the usual "clear MIE, check, WFI, set MIE" sequence has no lost
  wakeups.
- **`mip`** reads MEIP/MTIP/MSIP; writes are ignored (all three bits are
  read-only views of the devices).
- **Reset.** Every system reset (power-on, Ctrl-C, finisher) quietens the
  devices (`msip` 0, `mtimecmp` all ones, PLIC and UART interrupt enables,
  priority, threshold, pending and in-service 0) but keeps `mtime`. A trap
  back into the boot ROM is not a reset, so the ROM's `R` command now does the
  same in software (`reset_devices()`, draining a stale pending request with
  one claim/complete). The ROM grew from 572 to 660 bytes.

## Problems found on the way

**Timing.** The first version stepped the devices from the post-reset machine
state, so the `mtime` adder and the `mtimecmp` compares sat behind the reset
select (a mux with a fanout of 155). It reached 89.5 MHz. Now the devices step
from their own registers and the reset mux comes after the arithmetic. With
Gowin placement mode 1, the BRAM image meets 96 MHz.

**Interrupt latency after enabling.** To keep Fetch shallow, the second
version registered "which interrupt to take" from the previous cycle's state.
On the board this was one cycle stale after `mret` or `csrs mstatus` set MIE.
The next instruction ran before the pending interrupt was taken. With
`wait_for_interrupt()` inlined, the `lw ticks` right after `csrs` read the old
count, and the timer loop waited one period too many (33 ticks instead of 32).
Every input to the choice (the registered pending bits, `mie` and MIE) is
already a flip-flop, so Fetch now computes it directly, with no lag. A new
property checks that a pending interrupt is taken at the first Fetch after
`mret`.

**WFI was a no-op in two tests.** `traps.c` executed WFI with `mie` = 0x888
and nothing pending, which now waits forever. It now arms the timer first and
checks that WFI returns with MIE clear and without a trap (90 checks, up from
89). A Haskell counter property had used WFI as a no-op too.

**Echo at full rate loses a byte, but not in the core.** Echoing a 67-byte
line sent back-to-back always lost the echoed byte 32. The program had
received all 67 bytes, and a plain polling echo (no interrupts) loses the same
byte, while input paced at 2 ms per byte echoes intact. The loss is in the
host-bound direction while both directions are busy, at the host's write
boundary, so it is most likely the BL616 USB bridge. `check_interrupts.py`
therefore receives the full-rate line while `main()` is busy computing (never
touching the UART) and echoes it afterwards.

**`bootrom.hex` was never committed.** `.gitignore`'s `*.hex` matched it, so
commit 64d567f cannot build from a fresh clone. `.gitignore` now makes an
exception for it, and the file is committed with this step.

## Results

Haskell: all 42 SimpleRisc properties pass. The new ones cover:

- 64-bit MTIP across the halves;
- a timer interrupt waking WFI, with `mepc` and the deadline;
- a received byte claimed through the PLIC: claim returns 10, with nothing
  left after completion;
- MRET taking a pending interrupt at once, and not at all while `mie` masks it;
- resets quietening the devices and keeping `mtime`;
- the boot ROM quietening armed devices before `R`.

Gowin V1.9.12.04:

| Image | Target | Reported maximum | Worst setup | Notes |
|---|---:|---:|---:|---|
| BRAM, placement mode 1 | 96 MHz | 96.474 MHz | +0.051 ns, 0 violations | mode 0 misses by 2 ps (95.98 MHz) |
| SDRAM, cached | 66 MHz | 72.073 MHz | +1.277 ns, 0 violations | |

Build them with:

```bash
GOWIN_PLACE_OPTION=1 FREQ_MHZ=96 boards/tangnano20k/build_gowin.sh
```

```bash
SDRAM_FREQ_MHZ=66 boards/tangnano20k/sdram/build.sh core
```

On the board:

| Check | BRAM at 96 MHz | SDRAM at 66 MHz |
|---|---|---|
| `check_interrupts.py` (27 checks, 67-byte interrupt receive) | 5 runs pass | 5 runs pass |
| `check_processor.py` | pass | pass |
| `check_rv32i.py` | 38,400 checks | 38,400 checks |
| `check_rv32m.py` | 30,720 checks | 30,720 checks |
| `check_exceptions.py` | 31 checks | 31 checks |
| `check_finisher.py` | 5 cases × 3 | 5 cases × 3 |
| `check_traps.py` | 90 checks × 3 | 90 checks × 3 |
| `check_counters.py` | 27 checks × 10 | 27 checks × 10 (`--sdram`) |
| `check_bus.py` | 1,284 checks × 3 | 1,284 checks × 3 |
| `sdram/check_cache.py`, `sdram/check_core.py` | n/a | pass; benchmark 2,389,652 cycles |
| FP-RISC `tests/check_tangnano20k.py --port` | pass | pass |

`interrupts.c` checks:

- `mtime` against `mcycle`;
- 32 periodic 2,000-clock timer ticks awaited with WFI, within one period of
  the ideal time;
- `mip` with MIE clear;
- software before timer when both are pending;
- the PLIC's threshold, claim, in-service hold and complete-with-level-high
  behaviour, using the transmit-ready source;
- one receive interrupt per byte for a back-to-back line.

FP-RISC's builtin machine layer still refuses IRQ operations
(`machine/builtin/tangnano20k/machine.c`); its board harness passes unchanged.
Making those calls real is follow-up work in fprisc.

Bitstream SHA-256:

- BRAM 96 MHz: `6abec77353755440a1954f6a99aa67f477282f0c8d29cac65ba7e45526fc5510`
- SDRAM 66 MHz: `ae184efa4c887a5e1a1dddb844086ad2528121ebef508402ff6c2e9f96f52395`
