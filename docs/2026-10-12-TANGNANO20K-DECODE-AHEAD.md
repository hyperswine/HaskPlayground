# Decode-ahead: first instruction overlap (2026-10-12)

The first step that overlaps two instructions' stages, not only memory
reads. It is on the `pipeline` branch, not on `main`, because at the clock
it currently reaches it is barely faster (below).

## Design

At ExecuteFinish of a plain ALU instruction (OP, OP-IMM, LUI, AUIPC; not a
shift or multiply/divide), the next instruction's word is on the memory bus
(the sequential prefetch lands there) or already in the prefetch buffer.

1. **ExecuteFinish** keeps the word (`cpuNextInstruction`) and reissues the
   prefetch for pc + 8. It also registers `cpuAhead`: the next instruction
   does not read this one's destination register. It stays within the same
   2 KiB block, so it is just as fetchable.
2. **Commit** retires this instruction. If `cpuAhead` holds and no
   interrupt is due, Commit also does the next instruction's Decode (the
   register-bank read), and it continues at OperandSelect, skipping Fetch
   and Decode.
3. **Otherwise** Fetch takes the captured word (`FromNext`), without a
   memory read.

There is no forwarding: a dependent instruction just falls back.
Independent ALU instructions now take 4 cycles (6 before); dependent ones
still take 6.

## What the differential test found

A real, older core bug: every MISC-MEM encoding (opcode 0x0f) was a FENCE
no-op. Reserved encodings such as `0x58cf520f` (funct3 5) must be illegal.
Now:

- FENCE (funct3 0) is a no-op;
- FENCE.I (funct3 1) retires and makes the next fetch read memory, never a
  prefetch buffer;
- everything else is an illegal instruction.

The model agrees. Random words rarely hit this opcode; this run found it.
This fix is worth having on `main` regardless of decode-ahead.

## Timing: the register file is the wall

- **First version** (bank read at ExecuteFinish, straight from the memory
  word): 76-86 MHz. Two 32x32-bit bank readers, the word routed from the
  block RAM's output register to every mux select.
- **One shared reader:** still 84-86 MHz.
- **Bank read moved to Commit,** from the captured word in a core
  register (this version): 94.8 MHz best at a 96 MHz target. At 93 MHz,
  89-91.6 MHz depending on placement; **90 MHz passes** (90.160 MHz, +0.020
  ns).

Every worst path ends in the register-bank readers: 32 registers of
flip-flops behind wide multiplexers, with long routes. Placement swings by
15 MHz between modes. The register file in flip-flops is what limits
overlapping stages now.

## Results at 90 MHz

| Kernel | `main` (96 MHz) CPI | decode-ahead CPI |
|---|---:|---:|
| arithmetic | 6.37 | 5.62 |
| memory copy | 6.49 | 5.83 |
| branches | 13.79 | 13.05 |
| multiply/divide | 19.53 | 18.34 |
| all four | 9.56 | 8.82 |

That is 7.7% fewer cycles. Instructions per second:

| Clock | Million instructions per second | Against `main` |
|---|---:|---:|
| `main` at 96 MHz | 10.04 | - |
| decode-ahead at 90 MHz | 10.20 | +1.6% |
| decode-ahead at 93 MHz (if it met timing) | 10.5 | +5% |
| decode-ahead at 96 MHz (if it met timing) | 10.9 | +8% |

All board suites pass at 90 MHz, and so do PPP, SD and FP-RISC's harness
(12 of 12). `c/counters.c` now expects a CSR read after a NOP to take 12
cycles, not 14. All 49 SimpleRisc properties pass, including 900 random
programs.

## Recommendation

Move the register file into Gowin's distributed RAM (LUT RAM): two read
ports by duplication, one write port. That replaces 32x32 flip-flops and
their 32:1 multiplexers with a read of one or two levels. This is what a
real pipeline needs anyway: a register read every cycle in Decode, with
forwarding from Execute and Commit. It should recover the clock and make
the next overlaps (Execute while the next instruction decodes, then
forwarding) feasible.
