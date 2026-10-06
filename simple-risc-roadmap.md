# SimpleRisc roadmap: from tested RV32IM to a real RISC-V core

SimpleRisc (`src/SimpleRisc.hs`) runs RV32IM user-level code correctly on
the Tang Nano 20K at 96 MHz: unmodified `gcc -march=rv32im` output and
FP-RISC's builtin runtime both work. It is still far from a real core in
five ways, and this plan addresses them in turn:

1. The planned machine-mode slice is implemented: CSRs, synchronous traps,
   `mret`, and cycle/retirement counters work. Traps with `mtvec` zero retain the host
   halt convention until step 3.
2. No timer or interrupts.
3. Non-RISC-V behaviour inside the CPU: halting when the PC reaches the end of
   the loaded image, and the program loader, `DONE` reply and Ctrl-C living in
   the CPU's state machine. Data accesses now use a registered bus; instruction
   fetch and the hardware host loader still access RAM directly.
4. Never checked against the official architecture tests.
5. Slow: one instruction at a time, 9 cycles each, about 75 for
   multiply/divide.

Steps 1 to 4 make it a small but genuine RV32IM microcontroller core; step 5
makes it competitive.

## Ground rules for every step

- **The board decides.** nextpnr's Gowin timing has been 15-30% optimistic
  (`boards/tangnano20k/README.md`). Each step ends with `check_processor.py`
  and `check_rv32m.py` passing at 96 MHz on the board, plus the step's own
  hardware test. A step that cannot hold 96 MHz is not done.
- **New logic gets its own stage.** On this FPGA, wide muxes, long carry
  chains and anything driving the block RAM directly fail before nextpnr says
  they will. A new CSR file, comparator or device is read into a register
  before anything else uses it.
- **Keep the host protocol.** `P`/`R`/`X`/`M`, `DONE` and Ctrl-C stay
  byte-for-byte the same from the host's side, so `run_program.py`, the check
  scripts and fprisc's `tools/run_simple_risc.py` keep working.
- **Keep software in step.** `boards/tangnano20k/c/` and fprisc's
  `machine/builtin/tangnano20k/` change in the same step as the hardware they
  depend on.

## Execution timing experiment (2026-10-06)

The serial core now prepares operands/control in OperandSelect, registers ALU
candidates and half-width comparisons in Execute, then selects/composes results
in ExecuteFinish. Ordinary instructions take nine clocks, shifts ten and CSR
reads eleven; iterative multiply/divide bypasses ExecuteFinish. CSR selection
is registered and host reset preserves overwritten pipeline temporaries.
See [the experiment report](boards/tangnano20k/EXECUTE-TIMING-2026-10-06.md)
for vendor timing and physical tests. This improves timing without completing
step 5's instruction overlap. RAM relocation and boot ROM remain next in step 3.

## SDRAM experiment (2026-10-06)

A separate `SdramSimpleRisc` image now uses all 8 MiB of SDRAM at `0x80000000`
for code, data, BSS, heap and stack, with the first 64 KiB aliased at zero.
The controller passes a full-capacity test and physical C/FP-RISC programs.
The cached prototype now passes hardware checks at 66 MHz, with a 1 KiB
unified write-through cache. A repeated-read workload is about 1.50x faster
than the original uncached 54 MHz image. It does not yet satisfy this roadmap's
96 MHz completion gate. It retains the hardware loader and legacy halt rules;
`H` selects high-address entry and loader images remain limited to 64 KiB.
Next: recover CPU clock/throughput, then replace the loader with boot ROM.
See [the measured results](boards/tangnano20k/sdram/README.md).

## Implementation status (2026-10-05)

Step 1 is complete within this roadmap's M-mode scope: precise exceptions,
registered CSR stages, trap/status/scratch/identification registers, `mret`,
a temporary no-op `wfi`, and split 64-bit cycle/retirement counters with
read-only aliases. The final counter image passes at 96 MHz: 270 counter
assertions, 267 guest trap assertions, the processor regression, 30,720 RV32M
and 3,840 RV32I comparisons. All 26 SimpleRisc properties and the generated
RTL smoke test pass. FP-RISC uses the matching CSR adapter and counter fixture.
See [the implementation and measured results](boards/tangnano20k/MACHINE-MODE-2026-10-05.md).

The RAM map and host protocol remain in place. Step 3 now starts with an
explicit finisher at `0x00100000` and registered device selection; C and
FP-RISC terminate through the finisher while
preserving `mtvec`. The temporary ECALL/end-of-image halt rules remain for
older binaries until the boot ROM replaces the hardware host controller.
The finisher/device-selection slice passes its 15 exit cases and all existing
CPU, counter, trap, RV32IM and FP-RISC checks at 96 MHz. No timer/interrupt
sources or official architecture-test claim yet. The registered data bus also
passes 3,852 mixed-width RAM/RX checks and
the existing CPU, counter, trap and RV32IM regressions at 96 MHz. Its
request/response interface preserves precise faults and waits for completion
before changing the PC or destination register. The rest of step 3 is
RAM relocation and boot ROM, followed by step 4's conformance
baseline. See [the system separation slice](boards/tangnano20k/SYSTEM-BUS-2026-10-05.md).

## Step 1: machine mode (Zicsr and traps)

**Goal:** M-mode only, enough for trap handlers, `riscv-arch-test` and an
RTOS-style runtime. No S or U mode.

**CSRs**

| CSR | Behaviour |
|---|---|
| `misa` | read-only: RV32IM (`0x4000_1100`) |
| `mvendorid`, `marchid`, `mimpid`, `mhartid` | read-only zero |
| `mstatus` (and `mstatush`) | `MIE`, `MPIE`; `MPP` fixed at M (`11`); other fields zero |
| `mtvec` | direct mode only (`MODE` reads 0) |
| `mepc`, `mcause`, `mtval`, `mscratch` | read/write |
| `mie`, `mip` | `MSIE`/`MTIE`/`MEIE` and pending bits; pending bits come from step 2 |
| `mcycle`/`mcycleh`, `minstret`/`minstreth` | 64-bit counters; `cycle`/`instret` (`h`) as read-only user aliases |

An unknown CSR, or a write to a read-only one, raises an illegal-instruction
exception.

**Instructions:** `csrrw`, `csrrs`, `csrrc` and their immediate forms;
`ecall`, `ebreak`, `mret`; `wfi` (stalls until an interrupt is pending once
step 2 exists; a no-op before that).

**Exceptions**

| Cause | When |
|---|---|
| 0 instruction address misaligned | a jump or taken branch to an address with bit 1 set (checked in Commit, with `mtval` = target) |
| 2 illegal instruction | unknown opcode/funct, bad CSR access; `mtval` = the instruction |
| 3 breakpoint | `ebreak` |
| 4 / 6 load / store address misaligned | `lw`/`sw` not 4-aligned, `lh`/`lhu`/`sh` not 2-aligned; `mtval` = address. These accesses trap |
| 5 / 7 load / store access fault | address outside RAM and the device map |
| 11 environment call from M-mode | `ecall` |

**Hardware**
- **CSR access stages.** A CSR file as plain registers, with dedicated stages
  `CsrRead` (select the one CSR into a register) and `CsrWrite` (apply the
  read-modify-write and write `rd`). Only CSR instructions pay for them.
- **A `Trap` stage** that writes `mepc` (the faulting PC), `mcause` and
  `mtval`, sets `MPIE` = `MIE` and `MIE` = 0, and sets `pc` = `mtvec`, then
  goes to Fetch. `mret` reverses it (`pc` = `mepc`, `MIE` = `MPIE`,
  `MPIE` = 1).
- **Counters.** `mcycle` and `minstret` are split into two 32-bit halves with a
  registered carry between them. A 64-bit increment in one cycle is exactly the
  kind of carry chain that fails on this chip.

**Compatibility rule until step 3:** while `mtvec` is 0, a trap halts the CPU
and sends `DONE`, so older binaries that terminate with `ecall` keep
working. Current C and FP-RISC startup use the step 3 finisher. The legacy
trap convention is non-standard and is removed with the boot ROM in step 3.

**Software:** build with `-march=rv32im_zicsr`. fprisc's Tang Nano
`machine.c` can implement `csr_read`/`csr_write` for real instead of
panicking.

**Tests**
- Haskell properties: CSR read/modify/write semantics, every exception's
  `mepc`/`mcause`/`mtval`, `mret` restoring `mstatus`, and counters carrying
  across 32 bits.
- On the board: a C program that installs a trap handler and deliberately
  triggers each exception, printing `mcause`/`mtval` and resuming.

**Size:** medium. Mostly new stages; the existing ones barely change.

## Step 2: timer and interrupts

**Goal:** a periodic timer interrupt and an interrupt-driven UART.

**Devices**
- **A CLINT** at `0x0200_0000` with the SiFive/QEMU `virt` layout: `msip` at
  `+0x0`, `mtimecmp` at `+0x4000`, `mtime` at `+0xBFF8`. This is the layout
  FP-RISC's `machine/virt/clint.fpr` already uses.
- **`mtime`** ticks at a fixed 1 MHz from a clock divider, so programs don't
  depend on `FREQ_MHZ`. (The alternative is one tick per clock; that's an open
  decision.)
- **UART interrupts.** A new UART control register enables "byte received"
  and "transmitter ready" interrupts. The UART drives `MEIP` directly (no PLIC
  for a single source).

**Hardware**
- **Pending bits.** `MTIP` = `mtime >= mtimecmp`, computed as a registered
  64-bit compare split over two cycles. `MSIP` comes from `msip`; `MEIP` from
  the UART.
- **Taking interrupts.** Interrupts are taken only at instruction boundaries,
  in Fetch: if `MIE` is set and `mie & mip` is nonzero, go to `Trap` with the
  interrupt cause (bit 31 set) and `mepc` = the next instruction. A
  multiply/divide or a UART wait in progress always completes first.
- **`wfi`** waits in its own stage until `mie & mip` is nonzero.

**Tests**
- Haskell properties: interrupt priority, `MIE` masking, `mepc` on interrupts.
- On the board: a 1 kHz timer tick counter; an interrupt-driven UART echo
  that keeps up with back-to-back input. Today's one-byte receiver drops
  bytes while the program is busy.

**Size:** medium.

## Step 3: take the special cases out of the CPU

**Goal:** the CPU is just a CPU; loading, exiting and resetting are separate
parts of the system.

**Memory map** (an open decision: this moves RAM)

| Address | Device |
|---|---|
| `0x0000_0000` | boot ROM, 2 KiB (block RAM initialised at build time) |
| `0x0010_0000` | exit device (QEMU `virt` "finisher": `0x5555` = exit 0, `code << 16 \| 0x3333` = exit `code`) |
| `0x0200_0000` | CLINT (step 2) |
| `0x1000_0000` | UART (TXDATA, STATUS, RXDATA, plus the step 2 control register) |
| `0x8000_0000` | RAM, 64 KiB |

RAM at `0x8000_0000` and the finisher match QEMU `virt`, the reference board
for FP-RISC's builtin profile. That makes linker scripts and exit code
portable between QEMU and the board.

**Changes**
- **A small bus.** Load/store stages issue a registered request (address,
  width, data, write) to an address decoder; each device answers with a
  registered response. This replaces the hard-wired `isUartAddress` check, and
  out-of-map addresses raise access faults (step 1).
- **The exit device.** Writing it stops the CPU and makes the host side send
  `DONE` (a failure code could also be reported). The end-of-image halt
  (`cpuPastEnd`, `programEnd`) goes, and so does the `mtvec` = 0 compatibility
  rule from step 1.
- **The boot ROM.** The `P`/`R`/`X`/`M` host protocol moves out of the CPU's
  state machine into a boot ROM written in C. It runs from reset, implements
  the same protocol over the UART, writes RAM and jumps to `0x8000_0000`. The
  hardware host controller (`stoppedStep`, `ClearMemory`, `ProgramBytes`)
  goes away.
- **Ctrl-C** becomes a system reset: the UART receiver resets the CPU, which
  restarts in the boot ROM with RAM preserved. A runaway program can still
  always be stopped, without a special CPU state.

**Software:** update `c/link.ld`, fprisc's `tangnano20k/link.ld` and both
`crt0.S` files for RAM at `0x8000_0000`; exit through the finisher instead of
`ecall`. The host scripts need no protocol change.

**Tests**
- The Haskell simulator runs the boot ROM image. The existing load/run
  properties hold unchanged, since the protocol is the same.
- `tb_program.v` runs as today.
- On the board: all existing checks, plus a program that writes code into RAM
  and jumps to it (impossible today).

**Size:** large. The boot ROM is new software, and the bus touches every memory
stage.

## Step 4: the official architecture tests

**Goal:** pass `riscv-arch-test` for RV32I, M, Zicsr and the M-mode
privileged tests that apply, with Spike as the reference.

**Setup**
- **Framework.** The `riscv-arch-test` suites run under RISCOF, with Spike
  (installed at `/opt/homebrew/bin/spike`) as the reference model. A
  SimpleRisc plugin supplies the `RVMODEL_*` macros: boot (step 3's RAM map),
  halt (the step 3 finisher), and a signature dump.
- **Where tests run.** Fast runs happen on the board: the halt macro prints
  the signature region in hex over the UART before exiting, and the plugin
  collects it with one port session. CI-style runs happen in simulation: give
  the testbench a backdoor that preloads RAM from a hex file, because loading
  through the UART takes about 3 minutes per image in iverilog. Verilator is
  worth trying for the RTL; it failed only on the post-synthesis netlist.

**Dependencies:** step 1 (traps: many tests expect them) and step 3 (halt and
RAM map). RV32I/M tests that need no traps could run earlier with a temporary
halt macro, as an early baseline.

**Deliverable:** a script and a results table in the README. Every failure is
fixed or documented as a deliberate deviation (for example "misaligned
accesses trap rather than being emulated").

**Size:** medium. Mostly harness work; then whatever the tests find.

## Step 5: a real pipeline

**Goal:** overlap instructions. Get from 9 cycles per instruction to under 2,
at the same verified 96 MHz.

**Shape.** An in-order pipeline, deeper than the classic 5 stages because of
this FPGA's constraints:
- **Fetch** takes 3 stages, because block RAM needs registered addresses and
  outputs.
- **Decode** takes 2, for the split register read.
- **Then** execute, memory and writeback.

**Hazards**
- **Forwarding** from execute, memory and writeback into operand select.
- **Load-use:** stall one or two cycles.
- **Branches:** predict not taken. A taken branch costs roughly the fetch
  depth (about 4-5 cycles); a small branch target buffer can come later.
- **Multiply/divide:** stall the pipeline while the iterative unit is busy (a
  pipelined multiplier on the DSP blocks can come later).
- **Precise traps (step 1):** exceptions are recorded with the instruction and
  taken at writeback, flushing everything younger. Interrupts are taken
  between instructions at writeback.

**An intermediate step if the full pipeline is too big a jump:** fetch the
next instruction while the current one executes, which alone takes about 8
cycles per instruction down to about 4.

**Verification** (the main risk is hazard bugs):
- **A pure Haskell RV32IM+Zicsr reference model** (an instruction-set
  interpreter: register file, PC, CSRs and memory as values), checked against
  Spike on random programs.
- **Differential properties:** random instruction sequences, weighted towards
  back-to-back dependencies, loads followed by uses, and branches. Run on the
  pipelined core and the model, and compare register files, memory and CSRs.
- **Unchanged** property tests (results, not cycle counts), the architecture
  tests and the board checks.

**Size:** large.

## Order and dependencies

```
1 machine mode ──► 2 timer and interrupts
      │
      ├──► 3 bus, exit device, boot ROM ──► 4 architecture tests ──► 5 pipeline
```

Recommended order: 1, then 3, then 4 (a conformance baseline before the risky
change), then 2, then 5. The architecture tests then guard steps 2 and 5. Step
2 can move before 4 if interrupts are wanted sooner. Small, independent pieces
can land early to reduce risk: the exit device from step 3, and misaligned
accesses trapping from step 1.

## Open decisions

1. **The memory map change in step 3:** RAM moves to `0x8000_0000` (matching
   QEMU `virt`), or stays at 0 with the boot ROM elsewhere. This plan assumes
   the move.
2. **`mtime` rate:** a fixed 1 MHz (assumed here) or one tick per clock.
3. **UART interrupts directly on `MEIP`** (assumed), or a minimal PLIC for
   future devices.
4. **Step 5's ambition:** the full pipeline, or the fetch-overlap step first.
