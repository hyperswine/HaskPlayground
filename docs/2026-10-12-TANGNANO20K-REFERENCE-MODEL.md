# A reference model for differential testing (2026-10-12)

The safety net for pipelining SimpleRisc. `test/Rv32Model.hs` is an
independent RV32IM + Zicsr interpreter over plain Haskell values, written
from the RISC-V specification rather than from the core. It models:

- RAM at `0x8000_0000` and the boot ROM at 0, readable by loads;
- synchronous traps into a direct-mode `mtvec`;
- the machine CSRs SimpleRisc implements, except the counters.

Where the specification leaves a choice, the model follows SimpleRisc and
says so:

- `mtval` holds the instruction for an illegal instruction and the pc for
  EBREAK;
- a misaligned address traps before an unmapped one.

`prop_core_matches_reference_model` in `test/SimpleRisc.hs` generates random
programs and runs each one on the core (cycle-accurate, through the real bus)
and on the model:

- up to 64 instructions, every control transfer forward, ending in ECALL;
- operands from `x0`-`x8`, so instructions depend on each other back to
  back;
- loads and stores through a data pointer, occasionally misaligned or at an
  unmapped or ROM base;
- forward branches, JAL, and AUIPC/JALR pairs, occasionally to misaligned
  targets;
- CSR reads and writes, including illegal ones;
- FENCE;
- occasionally ECALL, EBREAK or a random word.

It then compares:

- every register;
- `mepc`, `mcause` and `mtval`;
- MIE and MPIE;
- `mscratch` and `mie`;
- every RAM word either side touched;
- the number of instructions retired.

Programs that loop are discarded: a branch can land on a JALR and skip its
AUIPC.

## What it found

Three mismatches while it was being written, all in the model or the
harness, none in the core:

- **ROM loads.** A load from address 0 reads the boot ROM on SimpleRisc; the
  model first faulted it.
- **Deferred register writes.** The core's last register write lands a
  cycle after its pc enters the ROM, so the comparison now runs five cycles
  later. That is too few to commit a ROM instruction.
- **Generator loops.** The generator could build an infinite loop.

## Coverage, three runs of 300 programs

| Program ends in | Share |
|---|---:|
| ECALL | 77-82% |
| an illegal instruction | 7-11% |
| a misaligned access | 5% |
| a misaligned jump | 4-5% |
| an access fault | 1-3% |
| a jump into the ROM | 1-2% |

About 40% of programs retire 10 or more instructions.

## Not yet covered

- Backward branches (loops) and interrupts.
- MRET and WFI.
- Self-modifying code (a store into the next instruction has its own
  property).

Interrupts need the CLINT and PLIC in the model; loops need a generator
that bounds them. Both matter for the pipeline's flush logic, and should be
added before it.
