# Machine-mode roadmap: precise exception entry, 2026-10-05

This records two independent slices of `simple-risc-roadmap.md` step 1.
The exception foundation was committed as `0b03fb8`; the CSR/trap-return
continuation below makes the handler state software-visible. Machine mode
remains incomplete until split cycle and retirement counters are implemented.

## First slice: exception foundation

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

In the first slice, trap vector/registers were Haskell state without guest CSR
access. Handler redirection and cause/EPC/value/status transitions were checked
in simulation; hardware validated fault termination and legal-access controls.
The continuation below connects CSR reads/writes and validates this state
on the physical board.

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

## Second slice: registered CSR access and machine trap return

`CsrRead` selects one CSR into a register and checks access legality;
`CsrWrite` performs the registered read/modify/write and requests rd writeback.
All six CSRRW/CSRRS/CSRRC and immediate forms are implemented. Write
suppression uses the encoded rs1/zimm: a nonzero source register containing
zero still requests a write. These CSRs have no read side effects.

Supported CSRs: `mstatus`, `mstatush`, `misa`, `mie`, `mip`, `mtvec`,
`mscratch`, `mepc`, `mcause`, `mtval`, and the four read-only machine IDs.
`misa` reads RV32IM; IDs read zero. `mstatus` exposes MIE/MPIE and fixed
MPP=M. `mtvec` and `mepc` mask the low two bits. `mie` retains MSIE/MTIE/MEIE;
`mip` and `mstatush` are currently WARL zero. Unknown CSRs and writes to
read-only CSRs raise cause 2 with the instruction in `mtval`.

`TrapReturn` restores PC/MIE/MPIE for `mret`. `wfi` is a no-op until step 2.
Ordinary instructions retain their existing staging; CSR instructions take
ten cycles and MRET nine. RUN/RESET clear trap, scratch and enable state.

C now builds with `rv32im_zicsr`; the FP-RISC target builds with
`rv32im_zicsr_zifencei`. FP-RISC CSR helpers dispatch real instructions for
implemented addresses and panic for unknown runtime CSR numbers. IRQ/wait and
atomic runtime APIs remain unsupported. Both runtimes clear `mtvec` on exit
so a guest handler cannot intercept the legacy host ECALL termination.
These updated runtimes require the matching CSR-enabled processor image.

### Validation of the continuation

- `stack test --fast`: all suites pass, including 22 SimpleRisc properties.
  New properties cover all six CSR forms, rd=x0/rs1=x0/immediate forms,
  read-only/unknown CSR failures, WARL masks, MRET status restoration and an
  end-to-end guest handler that advances `mepc` and resumes.
- Generated RTL Icarus smoke: exact `HiDONE`.
- `check_traps.py --freq-mhz 96`: three complete runs of 89 assertions each.
  The C guest checks CSR read/modify/write, every planned synchronous exception
  class, precise PC/cause/value and status after MRET. It leaves its handler
  installed to verify startup's exit compatibility.
- `check_processor.py --freq-mhz 96`: all C arithmetic, UART, reset, clear/reload
  and byte/halfword/word stress checks pass.
- `check_rv32m.py --freq-mhz 96`: all 30,720 comparisons pass.
- `check_rv32i.py --freq-mhz 96 --repeat 1`: all 3,840 comparisons pass; both
  comparison harnesses now enable Zicsr for their shared C startup.
- FP-RISC `tests/check_tangnano20k.py --port /dev/cu.usbserial-20250303171
  --freq-mhz 96`: host/link/ISA/loader checks, three builtin smoke runs,
  three CSR fixture runs, seven failure/refusal cases and recovery all pass.
- The final FP-RISC CSR fixture deliberately leaves `mtvec` nonzero;
  `tests/check_tangnano20k_csr.py --port /dev/cu.usbserial-20250303171
  --freq-mhz 96` passes all three board runs, testing `hal_poweroff` cleanup.

Build: seed 1, requested route target 115.2 MHz, estimated post-route maximum
131.86 MHz. Verified operation is 96 MHz; this is not a higher-clock claim.
Resources: 7,639 LUT4s, 2,511 flip-flops, 32 BSRAM blocks.
Image SHA-256: `a2e89522be688958329a7f96be16d72840813ad5dd1820c2bcc5cac70a17f81c`.
The matching image is loaded in board SRAM; flash was not modified.

### Counter continuation

`mcycle`/`mcycleh` and `minstret`/`minstreth` now provide writable 64-bit
counters. `cycle`/`cycleh` and `instret`/`instreth` are read-only aliases;
attempted writes trap with cause 2. Unknown counter addresses still trap.

Each counter uses two 32-bit words and a registered overflow bit. The high
word consumes the carry on the next clock. A CSR read waits for that carry
to settle before selecting its word into the existing CSR read register.
There is no 64-bit increment or carry adjustment in the CSR read mux.
Software still needs the usual high/low/high retry when sampling a running
64-bit counter across separate RV32 instructions.

Retirement is a registered pulse from successful instruction completion.
Loads, stores, CSR instructions, MRET and the temporary no-op WFI retire once;
UART and multiply/divide stalls add no extra retirements. Trapping instructions,
including ECALL and illegal CSR writes, do not retire. Explicit counter writes
override that clock's implicit increment and affect only the selected half.
A low-half write drains an earlier carry; a high-half write replaces it.

Physical reset initializes both counters to zero. The cycle counter counts
every core clock, including host programming and stopped time. Host RUN,
RESET and Ctrl-C preserve counters; guests can reset the writable halves.
The instruction counter counts guest execution only. Counter write requests
and retirement pulses are applied on the following clock, before a subsequent
instruction can read the counter.

The FP-RISC target dispatches these eight CSR addresses. Its CSR fixture now
checks writable halves, rollover, high aliases and increasing retirement.
TIME and interrupt sources belong to step 2, so this does not claim the full
Zicntr extension. Official architecture tests remain step 4.

The initial counter image (seed 1, route target 115.2 MHz, reported maximum
142.90 MHz, SHA-256
`9b7ea5c9610b7db3be2f29c243feaa782c64dfebdfd8732ffafd05383861547f`)
failed at 96 MHz: the counter fixture returned truncated `C27\nDONE`, and
the existing C arithmetic regression also lost string characters. The same
routed design with only the PLL changed to 72 MHz passed ten runs of all 27
counter assertions and the complete processor regression. These diagnostics
show why the modeled maximum is insufficient for the roadmap's 96 MHz gate.


Exception and CSR semantics follow the
[RISC-V machine-mode specification](https://docs.riscv.org/reference/isa/priv/machine.html)
and [Zicsr specification](https://docs.riscv.org/reference/isa/unpriv/zicsr.html).

### Final counter validation at 96 MHz

The replacement placement uses seed 2 and a 144 MHz route target; its modeled
maximum is 147.19 MHz. `build.sh` now defaults to margin 1.5 and tries seed 2
first, matching this build. Reproduce with `FREQ_MHZ=96 MARGIN=1.5 SEEDS=2
boards/tangnano20k/build.sh`; each new build still needs physical checks.
Resources: 7,840 LUT4s, 2,687 flip-flops and 32 BSRAM blocks.
Final image SHA-256:
`3c29163eb1375600f2b5e22ac953f68c6af0c487ac39d12e202e603275b58958`.
Verified frequency is 96 MHz; this is not a higher-clock claim.

- `stack test --fast --test-arguments='-p SimpleRisc'`: all suites pass,
  including 26 SimpleRisc properties. Counter properties compare randomized
  increments/writes against a 64-bit reference, force carry, check exact
  retirement during UART stalls and traps, and check explicit-write priority.
- Generated RTL Icarus UART smoke: exact `HiDONE`.
- `check_counters.py --freq-mhz 96`: ten runs of 27 assertions each pass.
- `check_traps.py --freq-mhz 96`: three runs of 89 assertions each pass.
- `check_processor.py --freq-mhz 96`: all five exact arithmetic runs, UART,
  Ctrl-C, memory clear/reload and byte/halfword/word stress checks pass.
- `check_rv32m.py --freq-mhz 96`: 30,720 reference comparisons pass.
- `check_rv32i.py --freq-mhz 96 --repeat 1`: 3,840 comparisons pass.
- FP-RISC `tests/check_tangnano20k.py --port /dev/cu.usbserial-20250303171
  --freq-mhz 96`: host/link/ISA/loader checks, three builtin smoke runs,
  three updated counter CSR fixture runs, all seven refusal cases and recovery
  pass. The fixture leaves `mtvec` nonzero to check runtime exit cleanup.

The board is left running the replacement 96 MHz image in SRAM; flash is
unchanged. Step 1 is complete within the roadmap's scope. Step 3 removes
end-of-image halting, the vector-zero trap compatibility and the hardware
host loader, then step 4 supplies the official conformance baseline.
