# Machine-mode roadmap: precise exception entry, 2026-10-05

This is the first independent slice of `simple-risc-roadmap.md` step 1.
It does not complete machine mode. Software-visible CSR instructions,
`mret`, `wfi`, identification CSRs and counters remain to be implemented.

## Implemented

- A dedicated one-hot `Trap` stage, after registered exception cause/value
  capture in Commit. Trap entry records the faulting PC, cause and trap value,
  pushes MIE into MPIE, disables MIE, and selects the direct trap vector.
- Misaligned JAL/JALR and taken-branch targets raise cause 0 without link
  writeback. An untaken branch does not fault for its unused target.
- Illegal instructions raise cause 2 with instruction bits in the trap value;
  ECALL raises cause 11 with value zero; EBREAK raises cause 3 with its PC.
- Misaligned halfword/word loads and stores raise causes 4/6 before any RAM
  command, UART transmission or receive-latch consumption. Invalid widths
  are illegal instructions, including at UART addresses.
- Out-of-map loads and stores raise causes 5/7. Misalignment takes priority
  over access fault for this implementation.
- Trap vector zero retains the existing `DONE` termination protocol. RUN and
  RESET clear trap state. The memory map and software ABI are unchanged.

Trap vector and trap registers are currently Haskell state, not accessible
through guest CSR instructions. Handler redirection, cause/EPC/value and
MIE/MPIE transitions are verified in simulation. Synthesis may remove these
unobservable register fields until CSR reads/writes are connected; hardware
checks in this slice validate fault termination and legal-access controls,
not software-visible trap register contents.

## Verification

`stack test --fast` passes all suites, including 18 SimpleRisc properties.
The new random-PC/status property covers 21 exception cases on each of 100
trials. Additional end-to-end simulation checks show that a faulting store
never modifies RAM and that vector-zero traps emit exactly `DONE`.

The generated RTL also passes the Icarus Verilog UART smoke test with exact
`HiDONE` output (`iverilog -g2005`; SystemVerilog mode treats the generated
identifier `byte` as a reserved keyword).

On the connected Tang Nano 20K, loaded into SRAM at 96 MHz:

- `python3 boards/tangnano20k/check_exceptions.py --freq-mhz 96`: all 31
  exception/encoding and legal access checks pass, including misaligned
  JAL/JALR, taken branch and untaken-branch control.
- `python3 boards/tangnano20k/check_processor.py --freq-mhz 96`: five exact C
  arithmetic runs, UART echo, Ctrl-C recovery, clear/reload and
  byte/halfword/word memory stress pass.
- `python3 boards/tangnano20k/check_rv32m.py --freq-mhz 96`: all 30,720
  RV32M reference comparisons pass (20 loads of 1,536 checks).

Build: seed 1, route target 115.2 MHz, reported post-route maximum 163.67 MHz.
This model result does not establish operation above the tested 96 MHz.
Resources: 2,218 flip-flops and 32 of 46 BSRAM blocks.
The board is left running this 96 MHz SRAM image; flash is not modified.

Image: `output/tangnano20k/simple_risc.fs`.
SHA-256: `476718c48f0c3962d9ffc325234a4b4d0c50b54aff8b711d99a67dd17a0a4223`.

## Next slice

Add registered `CsrRead`/`CsrWrite` stages and the trap/status/scratch CSRs,
then `mret` and a real guest trap handler. Add read-only identification CSRs,
interrupt-enable placeholders and split counters to finish step 1. Update
both C and FP-RISC CSR interfaces only once guest CSR accesses exist.

The exception semantics follow the [RISC-V machine-mode specification](https://docs.riscv.org/reference/isa/priv/machine.html).
