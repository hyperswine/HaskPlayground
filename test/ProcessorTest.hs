{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Pure simulation tests for the Processor CPU core.
--
-- The CPU is a 2-stage pipeline (IF → ID/EX) implemented as a Mealy machine
-- via 'stepCpu3'. Tests run the machine in pure Haskell (no clock domain),
-- driving it with a hand-crafted instruction memory.
module ProcessorTest where

import Clash.Prelude hiding (take, (++))
import Data.String (fromString)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Processor
import qualified Prelude as P

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Run the CPU for @n@ cycles against a fixed instruction memory.
-- Returns the sequence of (nextPC, outValid, outByte, halted) produced each cycle.
runCpu :: InstrMem -> Int -> [(PC, Bool, BitVector 8, Bool)]
runCpu mem n = P.take n (go initCpuState3)
  where
    go s =
      let addr = cpu3PC s
          instr = mem !! addr
          (s', (nextPC, ov, od, halted, _, _)) = stepCpu3 s (instr, True)
       in (nextPC, ov, od, halted) : go s'

-- | Run until halt (or a safety limit of 512 cycles). Returns:
--   (final register file, list of output bytes, halted flag)
runUntilHalt :: InstrMem -> (RegFile, [BitVector 8], Bool)
runUntilHalt mem = go initCpuState3 512
  where
    go s 0 = (cpu3Regs s, [], False)
    go s budget
      | cpu3Halt s = (cpu3Regs s, [], True)
      | otherwise =
          let addr = cpu3PC s
              instr = mem !! addr
              (s', (_, ov, od, _, _, _)) = stepCpu3 s (instr, True)
              (finalRegs, bytes, halted) = go s' (budget - 1)
           in (finalRegs, (if ov then (od :) else P.id) bytes, halted)

-- | Build an instruction memory: fill with NOPs and patch in given (address, instruction) pairs.
buildMem :: [(Unsigned 10, Instr32)] -> InstrMem
buildMem patches = P.foldl (\m (i, v) -> replace i v m) (repeat nop) patches
  where
    nop = mkInstr3NoReg 0x0 0

-- | Convenience: read register @r@ from a register file.
reg :: RegFile -> RegIdx -> Unsigned 32
reg rf r = rf !! r

-- ---------------------------------------------------------------------------
-- mkInstr3 re-exports (for readability in tests)
-- op encodings:
--   0x0 = NOP
--   0x1 = LOAD_IMM  rd ← imm24
--   0x2 = ADD       rd ← rs + imm24
--   0x3 = SUB       rd ← rs - imm24
--   0x4 = AND       rd ← rs & imm24
--   0x5 = OR        rd ← rs | imm24
--   0x6 = MOV       rd ← rs
--   0x7 = OUTPUT    emit rs[7:0]
--   0x8 = HALT
--   0x9 = JMP       PC ← imm24
--   0xA = BEZ       if rs==0 then PC ← imm24
--   0xB = BNZ       if rs!=0 then PC ← imm24
-- ---------------------------------------------------------------------------

li :: RegIdx -> BitVector 24 -> Instr32
li rd imm = mkInstr3 0x1 rd 0 imm

addI :: RegIdx -> RegIdx -> BitVector 24 -> Instr32
addI rd rs imm = mkInstr3 0x2 rd rs imm

subI :: RegIdx -> RegIdx -> BitVector 24 -> Instr32
subI rd rs imm = mkInstr3 0x3 rd rs imm

andI :: RegIdx -> RegIdx -> BitVector 24 -> Instr32
andI rd rs imm = mkInstr3 0x4 rd rs imm

orI :: RegIdx -> RegIdx -> BitVector 24 -> Instr32
orI rd rs imm = mkInstr3 0x5 rd rs imm

mov :: RegIdx -> RegIdx -> Instr32
mov rd rs = mkInstr3 0x6 rd rs 0

out :: RegIdx -> Instr32
out rs = mkInstr3 0x7 0 rs 0

halt :: Instr32
halt = mkInstr3NoReg 0x8 0

jmp :: BitVector 24 -> Instr32
jmp target = mkInstr3NoReg 0x9 target

bez :: RegIdx -> BitVector 24 -> Instr32
bez rs target = mkInstr3 0xA 0 rs target

bnz :: RegIdx -> BitVector 24 -> Instr32
bnz rs target = mkInstr3 0xB 0 rs target

-- ---------------------------------------------------------------------------
-- Unit-style tests using Hedgehog 'property' with concrete assertions
-- ---------------------------------------------------------------------------

-- | LOAD_IMM: r0 ← 42, then HALT.  r0 should equal 42.
prop_li_basic :: Property
prop_li_basic = property $ do
  let mem =
        buildMem
          [ (0, li 0 42),
            (1, halt)
          ]
      (regs, _, halted) = runUntilHalt mem
  halted === True
  reg regs 0 === 42

-- | ADD: r0 ← 10; r1 ← r0 + 5; HALT.  r1 should be 15.
prop_add_basic :: Property
prop_add_basic = property $ do
  let mem =
        buildMem
          [ (0, li 0 10),
            (1, addI 1 0 5),
            (2, halt)
          ]
      (regs, _, halted) = runUntilHalt mem
  halted === True
  reg regs 1 === 15

-- | SUB: r0 ← 20; r1 ← r0 - 7; HALT.  r1 should be 13.
prop_sub_basic :: Property
prop_sub_basic = property $ do
  let mem =
        buildMem
          [ (0, li 0 20),
            (1, subI 1 0 7),
            (2, halt)
          ]
      (regs, _, halted) = runUntilHalt mem
  halted === True
  reg regs 1 === 13

-- | AND: r0 ← 0xFF; r1 ← r0 & 0x0F; HALT.  r1 should be 0x0F.
prop_and_basic :: Property
prop_and_basic = property $ do
  let mem =
        buildMem
          [ (0, li 0 0xFF),
            (1, andI 1 0 0x0F),
            (2, halt)
          ]
      (regs, _, halted) = runUntilHalt mem
  halted === True
  reg regs 1 === 0x0F

-- | OR: r0 ← 0xF0; r1 ← r0 | 0x0F; HALT.  r1 should be 0xFF.
prop_or_basic :: Property
prop_or_basic = property $ do
  let mem =
        buildMem
          [ (0, li 0 0xF0),
            (1, orI 1 0 0x0F),
            (2, halt)
          ]
      (regs, _, halted) = runUntilHalt mem
  halted === True
  reg regs 1 === 0xFF

-- | MOV: r0 ← 77; r1 ← r0; HALT.  r1 should be 77.
prop_mov_basic :: Property
prop_mov_basic = property $ do
  let mem =
        buildMem
          [ (0, li 0 77),
            (1, mov 1 0),
            (2, halt)
          ]
      (regs, _, halted) = runUntilHalt mem
  halted === True
  reg regs 1 === 77

-- | OUTPUT: r0 ← 0xAB; emit r0; HALT.  Output byte should be 0xAB.
prop_output_basic :: Property
prop_output_basic = property $ do
  let mem =
        buildMem
          [ (0, li 0 0xAB),
            (1, out 0),
            (2, halt)
          ]
      (_, bytes, halted) = runUntilHalt mem
  halted === True
  bytes === [0xAB]

-- | JMP: load r0←1, jump to pc=5, load r0←2 (skipped), halt.
--   r0 should still be 1 after the jump skips pc=3..4 which set r0←2.
prop_jmp_skips_instructions :: Property
prop_jmp_skips_instructions = property $ do
  let mem =
        buildMem
          [ (0, li 0 1),
            (1, jmp 5), -- jump to pc 5, skipping (2) li r0 2
            (2, li 0 2), -- should be skipped
            (3, halt), -- should be skipped
            (4, halt), -- should be skipped
            (5, halt)
          ]
      (regs, _, halted) = runUntilHalt mem
  halted === True
  reg regs 0 === 1

-- | BEZ taken: r0 ← 0; bez r0, target; skipped li; halt at target.
prop_bez_taken :: Property
prop_bez_taken = property $ do
  let mem =
        buildMem
          [ (0, li 0 0), -- r0 = 0
            (1, bez 0 4), -- branch if r0==0 → pc=4
            (2, li 1 99), -- should be squashed
            (3, halt), -- should be squashed
            (4, halt)
          ]
      (regs, _, halted) = runUntilHalt mem
  halted === True
  reg regs 1 === 0 -- the li at pc=2 should have been squashed

-- | BEZ not taken: r0 ← 5; bez r0, target; execute li r1←7; halt.
prop_bez_not_taken :: Property
prop_bez_not_taken = property $ do
  let mem =
        buildMem
          [ (0, li 0 5), -- r0 = 5 (nonzero)
            (1, bez 0 4), -- branch NOT taken
            (2, li 1 7), -- should execute
            (3, halt),
            (4, halt) -- never reached
          ]
      (regs, _, halted) = runUntilHalt mem
  halted === True
  reg regs 1 === 7

-- | BNZ taken: r0 ← 3; bnz r0, target; skipped li; halt at target.
prop_bnz_taken :: Property
prop_bnz_taken = property $ do
  let mem =
        buildMem
          [ (0, li 0 3), -- r0 = 3
            (1, bnz 0 4), -- branch if r0!=0 → pc=4
            (2, li 1 99), -- should be squashed
            (3, halt), -- should be squashed
            (4, halt)
          ]
      (regs, _, halted) = runUntilHalt mem
  halted === True
  reg regs 1 === 0

-- | BNZ not taken: r0 ← 0; bnz r0, target; execute li r1←8; halt.
prop_bnz_not_taken :: Property
prop_bnz_not_taken = property $ do
  let mem =
        buildMem
          [ (0, li 0 0), -- r0 = 0
            (1, bnz 0 4), -- NOT taken
            (2, li 1 8), -- should execute
            (3, halt),
            (4, halt)
          ]
      (regs, _, halted) = runUntilHalt mem
  halted === True
  reg regs 1 === 8

-- | Data hazard / forwarding: two back-to-back ALU instructions that produce
--   then consume a result.  The pipeline must forward the EX result so that
--   the second instruction sees the correct value.
--
--   r0 ← 10
--   r1 ← r0 + 1     (r1 should be 11; r0 produced one cycle earlier)
--   r2 ← r1 + 1     (r2 should be 12; r1 produced one cycle earlier)
--   HALT
prop_forwarding_chain :: Property
prop_forwarding_chain = property $ do
  let mem =
        buildMem
          [ (0, li 0 10),
            (1, addI 1 0 1), -- r1 = r0 + 1 = 11
            (2, addI 2 1 1), -- r2 = r1 + 1 = 12  (needs forwarding)
            (3, halt)
          ]
      (regs, _, halted) = runUntilHalt mem
  halted === True
  reg regs 1 === 11
  reg regs 2 === 12

-- | Multiple outputs: emit three bytes and check they arrive in order.
prop_multiple_outputs :: Property
prop_multiple_outputs = property $ do
  let mem =
        buildMem
          [ (0, li 0 0x41), -- 'A'
            (1, out 0),
            (2, li 0 0x42), -- 'B'
            (3, out 0),
            (4, li 0 0x43), -- 'C'
            (5, out 0),
            (6, halt)
          ]
      (_, bytes, halted) = runUntilHalt mem
  halted === True
  bytes === [0x41, 0x42, 0x43]

-- | HALT freezes the machine: the register file must not change after HALT.
prop_halt_freezes :: Property
prop_halt_freezes = property $ do
  let mem =
        buildMem
          [ (0, li 0 55),
            (1, halt),
            (2, li 0 99) -- should never execute
          ]
      (regs, _, halted) = runUntilHalt mem
  halted === True
  reg regs 0 === 55

-- | NOP: executing pure NOPs should not modify registers.
prop_nop_no_side_effects :: Property
prop_nop_no_side_effects = property $ do
  let mem =
        buildMem
          [ (5, halt) -- run 5 NOPs (pc 0..4) then halt
          ]
      (regs, bytes, halted) = runUntilHalt mem
  halted === True
  bytes === []
  reg regs 0 === 0
  reg regs 1 === 0
  reg regs 2 === 0
  reg regs 3 === 0

-- | Property test: LOAD_IMM with random immediates correctly stores the value.
prop_li_roundtrip :: Property
prop_li_roundtrip = property $ do
  val <- forAll $ Gen.integral (Range.linear 0 0xFFFFFF)
  let imm = fromIntegral val :: BitVector 24
      mem =
        buildMem
          [ (0, li 0 imm),
            (1, halt)
          ]
      (regs, _, _) = runUntilHalt mem
  reg regs 0 === fromIntegral val

-- | Property test: ADD with random operands.
prop_add_random :: Property
prop_add_random = property $ do
  a <- forAll $ Gen.integral (Range.linear 0 0xFFFF)
  b <- forAll $ Gen.integral (Range.linear 0 0xFFFF)
  let mem =
        buildMem
          [ (0, li 0 (fromIntegral a)),
            (1, addI 1 0 (fromIntegral b)),
            (2, halt)
          ]
      (regs, _, _) = runUntilHalt mem
  reg regs 1 === fromIntegral (a + b :: P.Integer)

-- | Property test: SUB with a ≥ b (no underflow intended).
prop_sub_random :: Property
prop_sub_random = property $ do
  b <- forAll $ Gen.integral (Range.linear 0 0xFFFF)
  a <- forAll $ Gen.integral (Range.linear b 0x1FFFF)
  let mem =
        buildMem
          [ (0, li 0 (fromIntegral a)),
            (1, subI 1 0 (fromIntegral b)),
            (2, halt)
          ]
      (regs, _, _) = runUntilHalt mem
  reg regs 1 === fromIntegral (a - b :: P.Integer)

-- | The existing testProgram3 should produce the expected output bytes.
--   Trace (with pipeline latency):
--     pc0: li r0, 5
--     pc1: li r1, 3
--     pc2: add r2, r0, 0    → r2 = 5
--     pc3: add r2, r2, 3    → r2 = 8
--     pc4: out r2            → emit 8
--     pc5: sub r3, r2, 2    → r3 = 6
--     pc6: and r3, r3, 7    → r3 = 6 & 7 = 6
--     pc7: or  r3, r3, 1    → r3 = 6 | 1 = 7
--     pc8: out r3            → emit 7
--     pc9: li r0, 0
--     pc10: bez r0, 12      → taken (r0==0) → pc=12
--     pc11: halt             → skipped
--     pc12: out r2           → emit 8
--     pc13: halt
prop_testProgram3_outputs :: Property
prop_testProgram3_outputs = property $ do
  let (_, bytes, halted) = runUntilHalt testProgram3
  halted === True
  bytes === [8, 7, 8]

-- ---------------------------------------------------------------------------
-- Group
-- ---------------------------------------------------------------------------

processorGroup :: Group
processorGroup =
  Group
    (fromString "Processor")
    [ (fromString "LOAD_IMM basic", prop_li_basic),
      (fromString "ADD basic", prop_add_basic),
      (fromString "SUB basic", prop_sub_basic),
      (fromString "AND basic", prop_and_basic),
      (fromString "OR basic", prop_or_basic),
      (fromString "MOV basic", prop_mov_basic),
      (fromString "OUTPUT basic", prop_output_basic),
      (fromString "JMP skips instructions", prop_jmp_skips_instructions),
      (fromString "BEZ taken", prop_bez_taken),
      (fromString "BEZ not taken", prop_bez_not_taken),
      (fromString "BNZ taken", prop_bnz_taken),
      (fromString "BNZ not taken", prop_bnz_not_taken),
      (fromString "Forwarding chain", prop_forwarding_chain),
      (fromString "Multiple outputs", prop_multiple_outputs),
      (fromString "HALT freezes state", prop_halt_freezes),
      (fromString "NOP has no side effects", prop_nop_no_side_effects),
      (fromString "LOAD_IMM roundtrip", prop_li_roundtrip),
      (fromString "ADD random", prop_add_random),
      (fromString "SUB random", prop_sub_random),
      (fromString "testProgram3 output bytes", prop_testProgram3_outputs)
    ]
