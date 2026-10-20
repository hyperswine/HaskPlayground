# A real pipeline, first version

Branch `real-pipeline`. Several instructions are now in flight at once. The
BRAM image runs ordinary instructions in a pipeline and hands everything
else to the existing phases (the multi-cycle state machine), one
instruction at a time.

## Structure

`pipeStep` in `src/SimpleRisc.hs`; the state is `cpuPipe` (`Pipe`).

- **Fetch**: issues a word read per cycle while fewer than four words are
  queued or in flight (a registered count). Reads arrive three cycles later
  into a four-entry ring, or straight into decode when it is empty. Words
  are predecoded on arrival with the source registers they read.
- **Decode / register read**: the register file is read the cycle before,
  at the sources of the word that will enter (the queue head, else the
  arriving word), so no stall decision reaches its read address. Decode
  keeps its operands, refreshed by forwarding while it waits. Forwarding
  comes from the commit stage (finished results) and from the pending
  register write; a dependence on either execute stage stalls.
- **Execute one**: the adders, comparisons and ALU candidates (the
  existing Execute logic).
- **Execute two**: selects the result, resolves branches and jumps, checks
  memory addresses, and starts a load's (or sub-word store's) RAM read. A
  taken branch sets a registered redirect: the next cycle flushes the
  younger stages and fetches the target (predict not taken).
- **Commit**: the oldest instruction. Word stores write here; loads and
  sub-word stores wait for their word (two cycles), then align or merge.
  Results go to the register file through the pending write.

Fast instructions: ALU (not multiply/divide), LUI, AUIPC, JAL, JALR,
branches, FENCE, and aligned RAM loads and stores. Anything else (CSRs,
ECALL, EBREAK, MRET, WFI, FENCE.I, multiply and divide, device, ROM or
misaligned accesses, illegal encodings, misaligned targets, fetch faults)
and any interrupt **hands over**: when it reaches the commit stage the
pipeline is flushed and the phases run it from Fetch, then the pipeline
takes over again. The phases run without their own overlaps
(`cpuLocalMemory = False`) on this image; the SDRAM image still runs the
phases alone.

Every stall and port decision reads registers (valid bits, kinds, wait
counts, queue pointers): the first version, with stalls chained from the
commit stage to fetch, closed at only 64 MHz.

## Verification

- A second differential test, `Pipeline matches the reference model on
  random programs` (300 random programs with loops, CSRs, traps and
  software interrupts), runs the pipelined machine. In it the simulation
  reads the register file at the registered indices, as the circuit does.
- On the board at 96 MHz every suite passes: interrupts, processor, RV32I
  (38,400 checks), RV32M (30,720), exceptions, finisher, traps, counters,
  bus, SD, PPP, and FP-RISC's harness (12 of 12).
- `check_counters`' exact-cycle checks changed: a CSR read now hands over,
  so two reads are 20 clocks apart (was 8), and 21 across a NOP (was 12).
  The simulation predicted both, and reproduces the old 8 and 12 for the
  phases alone.

## Results

CPI on the board (`run_cpi.py`):

| Kernel | Phases (`main`) | Pipeline |
|---|---:|---:|
| arithmetic | 5.62 | **2.75** |
| memcpy | 5.83 | **2.83** |
| branches | 13.05 | 11.99 |
| muldiv | 18.34 | 19.13 |
| total | 8.82 | **6.71** |

The branches kernel does a 32-bit multiply every iteration, and multiply
and divide still run in the phases. Simulation (instructions in brackets):

| Program | Phases | Pipeline |
|---|---:|---:|
| 80 independent ADDI | 330 | 99 (1.2 CPI) |
| 80 dependent ADDI | 488 | 257 (3.2) |
| 4-instruction loop | 1063 | 614 (3.1) |
| store, load, use | 370 | 199 (3.3) |

Timing: 96 MHz passes in placements 2 and 3 (0.020 ns spare in placement
2: `GOWIN_PLACE_OPTION=2 FREQ_MHZ=96`). Above that, placements reach 90-106
MHz. `main` closes 117 MHz with the phases alone.

| Build | Clock | CPI | Million instructions/s |
|---|---:|---:|---:|
| `main` | 117 MHz | 8.82 | 13.3 |
| pipeline | 96 MHz | 6.71 | 14.3 (+8%) |
| pipeline, arithmetic kernel | 96 MHz | 2.75 | 34.9 (vs 20.8) |

## Next

In order of expected gain:

1. Multiply in the pipeline on the GW2A's DSP multipliers (the branches and
   muldiv kernels); division can stay in the phases.
2. Forward from the second execute stage, so a dependent instruction
   waits one cycle instead of two.
3. A small branch target buffer at fetch: loops and calls without the
   redirect bubbles.
4. Clock: the worst paths now are decode's hazard compare into its hold,
   and the phases' own logic, which no longer closes as tightly beside the
   pipeline.

## Update (2026-10-13): multiply, forwarding, branch prediction

Three of the next steps, on branch `pipe-forward`:

- **Multiply in the pipeline.** The first execute stage forms four 17 x 17
  partial products of the operands extended to 33 bits (four DSP blocks);
  the second sums them and picks MUL's low word or the high word for MULH,
  MULHSU and MULHU. Division stays in the phases. A property checks the
  multiplier against exact arithmetic (2,000 cases). The differential test
  found a real bug on the way: MULH shares SLL's funct3, so a multiply was
  first treated as a shift.
- **Forwarding from the second execute stage**: a plain ALU result, LUI,
  AUIPC or a jump's link is selected there and decode takes it, so a
  dependent instruction waits one cycle instead of two.
- **A branch target buffer**: eight entries indexed by pc bits 4..2 with an
  8-bit tag; fetch follows a hit, each instruction carries its predicted
  successor, and the second execute stage redirects on a mismatch. A taken
  branch or jump writes its entry; a branch not taken clears it.

Simulation (cycles; instructions in brackets):

| Program | Phases | First pipeline | Now |
|---|---:|---:|---:|
| 80 independent ADDI | 330 | 99 | 99 |
| 80 dependent ADDI | 488 | 257 | 178 (2.2 CPI) |
| 4-instruction loop | 1063 | 614 | 282 (1.4) |
| store, load, use | 370 | 199 | 199 |

CPI on the board (`run_cpi.py`):

| Kernel | Phases | First pipeline | Now |
|---|---:|---:|---:|
| arithmetic | 5.62 | 2.75 | **2.50** |
| memcpy | 5.83 | 2.83 | **1.67** |
| branches | 13.05 | 11.99 | **1.94** |
| muldiv | 18.34 | 19.13 | 17.75 |
| total | 8.82 | 6.71 | **2.56** |

Clock: the extra logic fills the device (79% of LUTs, 86% of CLS) and
96 MHz no longer closes (77-81 MHz). Verified build:
`GOWIN_PLACE_OPTION=2 FREQ_MHZ=81` (image hash begins f22c0f984b69ee48); every
board suite passes at 81 MHz, including FP-RISC's harness (12 of 12).

| Build | Clock | CPI | Million instructions/s |
|---|---:|---:|---:|
| phases alone | 117 MHz | 8.82 | 13.3 |
| first pipeline | 96 MHz | 6.71 | 14.3 |
| now | 81 MHz | 2.56 | **31.6** |

Next: area. The phases are a second CPU (fetch, decode, operand
registers, ALU, shifter, commit) kept for the rare slow instructions.
Making the pipeline the only datapath, with a small sequential unit in the
commit stage for bus accesses, CSRs, traps, MRET, WFI and division, should
free thousands of LUTs and the clock with them.

## Update (2026-10-13): the pipeline as the only datapath

Branch `single-datapath`. The phases were a second CPU (fetch, decode,
operand registers, ALU, shifter, commit) kept for the rare slow
instructions. Now every instruction runs through the pipeline to its
commit stage. The second execute stage classifies it (`Action`, in the
order the phases check: fetch fault, illegal encoding, system
instructions, misaligned target, misaligned access, access outside RAM),
and the commit stage either commits it (Plain; FENCE.I then refetches) or
flushes the pipeline and starts the matching phase straight from its own
registers: Trap (an exception, or an interrupt, taken before the oldest
instruction), CsrRead and CsrWrite, TrapReturn (MRET), Commit (WFI's
wait), MulDiv (division), BusWait (a load or store outside RAM). When the
phase finishes, the pipeline restarts at the pc. The circuit uses
`pipelinedCpuStep` directly, so the phases' fetch, decode and execute
stages are not part of it; the SDRAM image keeps them.

- Logic: 52% of the LUTs (was 79%), 74% of CLS (was 86%).
- A CSR read takes 11 clocks between reads (was 20): `check_counters`
  updated.
- The multiply's partial products now come from the second execute
  stage's operand registers and are summed at commit: Gowin had placed the
  first stage's operand registers inside the DSP blocks, so decode's
  forwarding crossed the die.
- Branch prediction no longer waits on the commit action.

New tests: `Interrupts are transparent to the phases` and `... to the
pipeline` run the same random program undisturbed and with software
interrupts raised at random cycles, and require the same registers and
memory (100 programs each, all of them interrupted).

Verified build: `GOWIN_PLACE_OPTION=2 FREQ_MHZ=78` (image hash begins
4d4f46cb47aa5212): every board suite passes (PPP four times), FP-RISC's
harness 12 of 12. CPI 2.55 (muldiv 16.15, was 17.75): 30.6 million
instructions per second, about the same as at 81 MHz with the phases.

One open problem: the placement-3 build at 78 MHz also met timing, but
failed PPP twice in four runs (an ICMP checksum); every other suite passed
on it. The placement-2 image passes PPP reliably. A path the timing
analysis does not cover, or one that closes too tightly, is likely; the
simulation, including the new interrupt tests, finds nothing.

The clock did not follow the area: the worst paths are decode's hazard
check into its own register and stage-advance enables, dominated by
routing (about 9 ns of wire against 2 ns of logic).
