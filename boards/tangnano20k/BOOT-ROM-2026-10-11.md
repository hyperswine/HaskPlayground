# Boot ROM and RAM at 0x8000_0000 (2026-10-11)

This completes roadmap step 3: the CPU is only a CPU. Loading, exiting and
resetting moved out of its state machine into a boot ROM, and RAM moved to
`0x8000_0000`, as on QEMU `virt`. Both the BRAM image and the SDRAM image use
the same ROM and memory map.

## Changes

**Memory map.** The 2 KiB boot ROM sits at address 0 (the reset vector),
RAM at `0x8000_0000` (64 KiB BRAM, or all 8 MiB of SDRAM), with the finisher
at `0x0010_0000`, a new read-only BOOTCAUSE register at `0x0010_0004`, and
the UART at `0x1000_0000`. Everything else faults. Fetching outside the ROM
and RAM raises an instruction access fault (cause 1). That check replaced the
old end-of-image halt and reuses its registered flag at Fetch. The SDRAM
image's zero-address alias is gone.

**Boot ROM** (`bootrom/`, 572 bytes). It implements the unchanged host
protocol (`P`, `R`, `M`, `X`) in C and starts programs with `mret` and every
register cleared. On entry it reads BOOTCAUSE: after a finisher exit, or a
trap taken with `mtvec` 0, it sends `DONE`. Power-on and Ctrl-C resets stay
silent. `P` writes a zero word after the image, so a program that runs off its
end still traps and ends with `DONE`. The core embeds `bootrom/bootrom.hex`
at compile time through Template Haskell. The BRAM image reads the ROM from
its own block RAM; the select bit is delayed to align with the registered
outputs, so the final mux sits between flip-flops. The SDRAM wrapper reads it
from a block RAM in two wait phases, without an SDRAM request.

**Removed from the core:** the hardware host controller (`stoppedStep`,
`ClearMemory`, `ProgramBytes`), the DONE sender, `cpuRunning`, `programEnd`,
the end-of-image halt and the temporary `mtvec` = 0 halt rule. A finisher
write and a Ctrl-C are now system resets into the ROM with a recorded cause;
RAM is kept.

## Problems found on the way

**Ctrl-C bytes inside program images.** With the CPU always running, the
first version reset on any received `0x03`, including bytes of an image being
loaded. `0x03` is common in RV32 code: any load whose destination register is
even has it as its low byte, and `hello.bin` contains 11. The randomized
Haskell load property caught it. Ctrl-C now breaks only programs: a
registered `cpuInRom` bit, set at Fetch, gates it, and in the ROM `0x03` is
an ordinary byte. Both gate inputs are flip-flops, so the reset fan-out still
starts from a single LUT.

**Absolute low addresses in tests.** `c/bus.c` used RAM at `0x7000`, and
`check_exceptions.py` and `check_finisher.py` used `0x200` and an `mtvec` of
128. These moved into RAM at `0x8000_0000`.

## Host-visible differences

- `R` with nothing loaded (for example after `M`) runs the cleared RAM, which
  traps straight back to the ROM: the host sees `DONE` rather than silence.
  `check_processor.py` now expects this.
- `X` does nothing; every run starts from zeroed registers anyway.
- `run_program.py --ram-base`/`H` remain only for the separate RV64 core,
  which still has its own hardware loader.

## Results

Haskell: all 36 SimpleRisc properties pass, including new ones for the ROM
loader with a guard word (random words, so `0x03` bytes included), ROM
clearing, unhandled traps returning to the ROM, Ctrl-C as data inside the ROM,
BOOTCAUSE reads, finisher resets, and SDRAM boot ROM reads. The generated
Verilog passes `tb_simple_risc.v` (`HiDONE`) in iverilog.

Gowin V1.9.12.04 (setup slow 0.95 V / 85 C, hold fast 1.05 V / 0 C):

| Image | Target | Reported maximum | Worst setup | Resources |
|---|---:|---:|---:|---|
| BRAM, placement mode 0 | 96 MHz | 96.480 MHz | +0.052 ns, 0 setup/hold violations | 7,608 LUT/ALU, 2,888 registers, 33 BSRAM (RAM 32 + ROM 1) |
| SDRAM, cached | 66 MHz | 72.429 MHz | +1.345 ns, 0 setup/hold violations | 7,733 LUT/ALU, 3,122 registers, 3 BSRAM (cache 2 + ROM 1) |

On the board, everything was loaded through the boot ROM:

| Check | BRAM at 96 MHz | SDRAM at 66 MHz |
|---|---|---|
| `check_processor.py` (C arithmetic, echo, Ctrl-C, `M`/`R`, memory stress) | pass | pass |
| `check_rv32i.py` | 7,680 checks | 11,520 checks |
| `check_rv32m.py` | 30,720 checks | 7,680 checks |
| `check_exceptions.py` | 31 checks | 31 checks |
| `check_finisher.py` | 5 cases × 3 | 5 cases × 3 |
| `check_traps.py` | 89 checks × 3 | 89 checks × 3 |
| `check_counters.py` | 27 checks × 10 | 27 checks × 10 (`--sdram`) |
| `check_bus.py` | 1,284 checks × 3 | 1,284 checks × 3 |
| `sdram/check_cache.py`, `sdram/check_core.py` | n/a | pass; benchmark 2,389,649 cycles |
| FP-RISC `tests/check_tangnano20k.py` | pass | pass |
| FP-RISC stress (7 workloads) | pass | not run |
| FP-RISC factorial example | pass | not run |
| FP-RISC 2 MiB SDRAM allocation fixture | n/a | pass |

The FP-RISC runs used fprisc's step-3 board files. Since fprisc commit
`84e1f05` (2026-10-10), `fpr_panic` had referenced actor-plane symbols that
builtin images do not link, so every builtin build failed, the QEMU reference
included. Guarding that branch with `#ifndef FPR_BUILTIN` fixes it; fprisc
`4f621a3` contains the guard and the board files. With it, the QEMU builtin
program boots, `tests/check_builtin.py` passes, and the harness and factorial
pass again on the 96 MHz image.

Bitstream SHA-256:

- BRAM 96 MHz: `d6fc097640c8e128eec5ade1ee25dc634bee50d076196339033545c823c0150d`
- SDRAM 66 MHz: `b8b9d6ed6843eaccd445f536a6ef040902c808867f2e57eb8362ee3fc85f57e4`

Build them with:

```bash
FREQ_MHZ=96 boards/tangnano20k/build_gowin.sh
```

```bash
SDRAM_FREQ_MHZ=66 boards/tangnano20k/sdram/build.sh core
```

Simulation note: iverilog runs the current core at about 3,000 clocks per
second. The pre-step-3 core was slightly slower on the same testbench (74 s
against 63 s), so the slowdown since the M2 measurement comes from the
earlier pipeline work, not from the ROM.
