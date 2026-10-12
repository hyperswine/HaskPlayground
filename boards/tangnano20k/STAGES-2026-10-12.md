# Stage registers, step 1: decoded control travels with the instruction

Branch `stages`. The first step of splitting the core into per-stage
registers: later stages stop decoding the raw instruction word and instead
select from control decoded once and carried in a stage register. Cycle
behaviour is unchanged (CPI 8.82 on the four `run_cpi.py` kernels), so the
gain is all clock.

## Why

On `main` (decode-ahead plus the register file in RAM) no 96 MHz placement
closed: 85.9, 82.3, 80.0 and 82.2 MHz. The worst paths started at opcode and
funct3 bits of `cpuInstruction` (fanout 63-93) and went through 12 logic
levels into the pc, the pending register write and the exception value.
Commit compared the opcode, funct3 and whole SYSTEM encodings every cycle to
pick its action.

## What changed

- `CommitKind`, one-hot (13 bits): Alu, Jump, Branch, Load, Store, Fence,
  FenceI, Ecall, Ebreak, Mret, Wfi, Csr, Invalid. `decodeCommitKind` runs at
  OperandSelect, as part of `ExecuteControl`, the decoded half of the
  OperandSelect/Execute stage register; reserved funct3 values decode to
  Invalid there. Commit dispatches on it.
- ExecuteFinish picks its result from registered `execLui`, `execAuipc`,
  `execAluOp` and `execStore` flags instead of comparing a registered
  32-bit opcode (which is gone).
- CsrWrite writes the CSR chosen by the one-hot selection registered at
  Commit (`writeSelectedCsr`), with the write-request bit registered beside
  it (`cpuCsrWrite`), instead of decoding the address bits again.
- Fetch writes the instruction access fault's cause and value
  unconditionally; Decode only chooses the phase.

## Results

Gowin, 96 MHz constraint unless stated:

| Build | place 0 | place 2 | place 3 | place 4 |
|---|---:|---:|---:|---:|
| `main` | 82.2 | 85.9 | 82.3 | 80.0 |
| step 1 (commit kind) | **96.0** | **96.2** | **96.1** | 94.3 |
| step 2 (+ CSR, fetch fault) | 83.9 | **96.2** | | **96.0** |
| step 2 at 99 MHz | 84.6 | **99.01** | 96.7 | |

Step 2 in placement 0 is placement noise: about 70% of its worst path is
routing, through an instruction bit with fanout 111.

Verified build: `GOWIN_PLACE_OPTION=2 FREQ_MHZ=99` (image hash begins
64ff8cd82a28712a; UART at 99 MHz / 868 = 114,055 baud). At 99 MHz on the
board every suite passes: interrupts, processor, RV32I (38,400 checks),
RV32M (30,720), exceptions, finisher, traps, counters, bus, SD and PPP, and
FP-RISC's harness (12 of 12). The step-2 image at 96 MHz passes the same
suites. All 50 SimpleRisc properties pass, including the differential test
against the reference model.

| Build | CPI | Clock | Million instructions/s |
|---|---:|---:|---:|
| `main` | 8.82 | 93 MHz | 10.54 |
| `stages` | 8.82 | 99 MHz | **11.22 (+6.5%)** |

## Next

The remaining near-critical paths are an instruction bit (the register-file
read address and the operand decode) into the prefetch, operand and
memory-port registers. The next steps in the same direction: give the
memory port its own registered source select instead of a mux across the
phases and the bus; then make the stages real (each with its own valid bit
and next-state logic, several instructions in flight) and add forwarding.
