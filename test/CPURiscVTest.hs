{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Simulation tests for CPURiscV (RV32I base ISA).
--
-- Each 'prop_' runs a concrete program on the pure 'stepCpuRV' mealy machine,
-- without any clock domain or UART.  Tests cover:
--
--   * Immediate arithmetic (ADDI, XORI, ORI, ANDI, LUI, AUIPC)
--   * Register-register ALU (ADD, SUB, AND, OR, XOR, SLL, SRL, SRA, SLT, SLTU)
--   * Shift instructions (SLLI, SRLI, SRAI)
--   * Comparison / set instructions (SLT, SLTU, SLTI, SLTIU)
--   * x0 is always zero
--   * Load / store (SW/LW, SB/LB/LBU, SH/LH/LHU)
--   * Load-use hazard stall (one-cycle bubble after a load)
--   * Branches taken / not-taken (BEQ, BNE, BLT, BGE, BLTU, BGEU)
--   * JAL / JALR (link register + jump)
--   * WB→EX forwarding for back-to-back ALU
--   * A small "sum 1..10" loop program
module CPURiscVTest where

import CPURiscV
import Clash.Prelude hiding (take, (++))
import Hedgehog
import qualified Prelude as P

-- ===========================================================================
-- Simulation helpers
-- ===========================================================================

-- | Run the CPU until the PC remains stationary for 4 consecutive cycles (no instruction changes it), or until a cycle budget is exhausted. Returns the final register file and memory-controller state.
runUntilDone :: InstrMem -> Int -> (RegFile, MemCtrl)
runUntilDone iMem budget = go initSimState budget (rvPC initCpuStateRV) 0
  where
    go st 0 _ _ = (rvRegs (simCpu st), simData st)
    go st n lastPC sameCount
      | sameCount >= 4 = (rvRegs (simCpu st), simData st)
      | otherwise =
          let cpu      = simCpu st
              mc       = simData st
              ram      = simRam st
              pcW      = truncateB (rvPC cpu `shiftR` 2) :: Unsigned 10
              instr    = iMem !! pcW
              dataAddr = truncateB (unpack (exmemAluOut (rvExMem cpu)) `shiftR` 2) :: Unsigned 10
              dataWord = ram !! dataAddr
              (cpu', mc', wc, pc, _, _, _) = stepCpuRV cpu (instr, dataWord, mc, True)
              ram'  = case wc of
                Just (idx, w) -> replace idx w ram
                Nothing       -> ram
              st'  = st { simCpu = cpu', simData = mc', simRam = ram' }
              same = if pc == lastPC then sameCount + 1 else 0
           in go st' (n - 1) pc same

-- | Read a register from the result register file (syntactic sugar).
reg :: RegFile -> RegIdx -> Word32
reg rf r = rf !! r

-- | Read a word from the simulation data RAM (word-addressed).
dmem :: SimState -> Unsigned 10 -> Word32
dmem st idx = simRam st !! idx

-- | Build an instruction memory: fill with NOPs then patch in (wordAddr, instr) pairs.
buildMem :: [(Unsigned 10, Word32)] -> InstrMem
buildMem patches = P.foldl (\m (i, v) -> replace i v m) (repeat iNOP) patches

-- ===========================================================================
-- Properties
-- ===========================================================================

-- ---------------------------------------------------------------------------
-- Arithmetic / immediate
-- ---------------------------------------------------------------------------

-- | ADDI: x1 = x0 + 42 = 42
prop_addi_basic :: Property
prop_addi_basic = property $ do
  let mem = buildMem [(0, iADDI 1 0 42)]
      (rf, _) = runUntilDone mem 20
  reg rf 1 === 42

-- | ADDI negative immediate: x1 = 100 + (-7) = 93
prop_addi_negative_imm :: Property
prop_addi_negative_imm = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 100), -- x1 = 100
            (1, iADDI 1 1 (-7)) -- x1 = 93
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 1 === 93

-- | ADD: x3 = x1 + x2
prop_add_reg_reg :: Property
prop_add_reg_reg = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 30), -- x1 = 30
            (1, iADDI 2 0 12), -- x2 = 12
            (2, iADD 3 1 2) -- x3 = 42
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 3 === 42

-- | SUB: x3 = x1 - x2
prop_sub_reg_reg :: Property
prop_sub_reg_reg = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 50), -- x1 = 50
            (1, iADDI 2 0 8), -- x2 = 8
            (2, iSUB 3 1 2) -- x3 = 42
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 3 === 42

-- | AND: x3 = x1 & x2
prop_and_reg_reg :: Property
prop_and_reg_reg = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 0xFF), -- x1 = 0xFF
            (1, iADDI 2 0 0x0F), -- x2 = 0x0F
            (2, iAND 3 1 2) -- x3 = 0x0F
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 3 === 0x0F

-- | OR: x3 = x1 | x2
prop_or_reg_reg :: Property
prop_or_reg_reg = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 0xF0), -- x1 = 0xF0
            (1, iADDI 2 0 0x0F), -- x2 = 0x0F
            (2, iOR 3 1 2) -- x3 = 0xFF
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 3 === 0xFF

-- | XOR / toggling via XORI
prop_xori :: Property
prop_xori = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 0b1010), -- x1 = 0b1010
            (1, iXORI 2 1 0b1111) -- x2 = 0b0101
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 2 === 0b0101

-- | XOR register-register
prop_xor_reg_reg :: Property
prop_xor_reg_reg = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 0xFF),
            (1, iADDI 2 0 0xAA),
            (2, iXOR 3 1 2) -- x3 = 0xFF ^ 0xAA = 0x55
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 3 === 0x55

-- | ANDI
prop_andi :: Property
prop_andi = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 0xFF),
            (1, iANDI 2 1 0x0F) -- x2 = 0x0F
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 2 === 0x0F

-- | ORI
prop_ori :: Property
prop_ori = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 0xF0),
            (1, iORI 2 1 0x0F) -- x2 = 0xFF
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 2 === 0xFF

-- | LUI: load upper immediate
prop_lui :: Property
prop_lui = property $ do
  -- LUI x1, 1  →  x1 = 1 << 12 = 0x1000
  let mem = buildMem [(0, iLUI 1 0x00001)]
      (rf, _) = runUntilDone mem 20
  reg rf 1 === 0x1000

-- | AUIPC: result = PC + (imm << 12)
--   At word address 0 the byte PC is 0x00, so result = 0 + (2 << 12) = 0x2000
prop_auipc :: Property
prop_auipc = property $ do
  let mem = buildMem [(0, iAUIPC 1 0x00002)] -- x1 = PC + 0x2000
      (rf, _) = runUntilDone mem 20
  reg rf 1 === 0x2000

-- ---------------------------------------------------------------------------
-- Shift instructions
-- ---------------------------------------------------------------------------

-- | SLLI: x2 = x1 << 3
prop_slli :: Property
prop_slli = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 1),
            (1, iSLLI 2 1 3) -- x2 = 1 << 3 = 8
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 2 === 8

-- | SRLI: x2 = x1 >> 4 (logical)
prop_srli :: Property
prop_srli = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 0x80),
            (1, iSRLI 2 1 4) -- x2 = 0x08
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 2 === 0x08

-- | SRAI: arithmetic right shift, sign bit propagated
prop_srai :: Property
prop_srai = property $ do
  -- Load -8 (= 0xFFFF_FFF8) into x1 via LUI+ADDI, then shift right by 2. -8 >> 2 (arithmetic) = -2 = 0xFFFF_FFFE
  let mem =
        buildMem
          [ (0, iADDI 1 0 (-8)), -- x1 = 0xFFFF_FFF8 (sign-extended -8)
            (1, iSRAI 2 1 2) -- x2 = -8 >> 2 = -2
          ]
      (rf, _) = runUntilDone mem 20
  -- 0xFFFFFFFE as BitVector 32
  reg rf 2 === 0xFFFFFFFE

-- | SLL / SRL register-register
prop_sll_srl :: Property
prop_sll_srl = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 0x10), -- x1 = 16
            (1, iADDI 2 0 2), -- x2 = 2
            (2, iSLL 3 1 2), -- x3 = 16 << 2 = 64
            (3, iSRL 4 1 2) -- x4 = 16 >> 2 = 4
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 3 === 64
  reg rf 4 === 4

-- | SRA register-register: arithmetic shift
prop_sra :: Property
prop_sra = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 (-16)), -- x1 = -16 (0xFFFFFFF0)
            (1, iADDI 2 0 3), -- x2 = 3
            (2, iSRA 3 1 2) -- x3 = -16 >> 3 = -2 (0xFFFFFFFE)
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 3 === 0xFFFFFFFE

-- ---------------------------------------------------------------------------
-- Comparison / set instructions
-- ---------------------------------------------------------------------------

-- | SLT: signed less-than
prop_slt_true :: Property
prop_slt_true = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 (-1)), -- x1 = -1 (signed)
            (1, iADDI 2 0 1), -- x2 = 1
            (2, iSLT 3 1 2) -- x3 = (-1 < 1) = 1
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 3 === 1

prop_slt_false :: Property
prop_slt_false = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 5),
            (1, iADDI 2 0 3),
            (2, iSLT 3 1 2) -- x3 = (5 < 3) = 0
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 3 === 0

-- | SLTU: unsigned; -1 = 0xFFFFFFFF > 1 unsigned → SLTU x3, x2, x1 = 1
prop_sltu :: Property
prop_sltu = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 (-1)), -- x1 = 0xFFFFFFFF (large unsigned)
            (1, iADDI 2 0 1), -- x2 = 1
            (2, iSLTU 3 2 1) -- x3 = (1 <u 0xFFFFFFFF) = 1
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 3 === 1

-- | SLTI / SLTIU immediate forms
prop_slti :: Property
prop_slti = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 3),
            (1, iSLTI 2 1 10) -- x2 = (3 < 10) = 1
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 2 === 1

prop_sltiu :: Property
prop_sltiu = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 1),
            (1, iSLTIU 2 1 5) -- x2 = (1 <u 5) = 1
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 2 === 1

-- ---------------------------------------------------------------------------
-- x0 is always zero
-- ---------------------------------------------------------------------------

-- | Writing to x0 must have no effect.
prop_x0_always_zero :: Property
prop_x0_always_zero = property $ do
  let mem =
        buildMem
          [ (0, iADDI 0 0 99), -- try to write 99 to x0 — should be discarded
            (1, iADDI 1 0 (-1)) -- x1 = -1 (uses x0 which should still be 0)
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 0 === 0 -- x0 must remain 0
  reg rf 1 === 0xFFFFFFFF

-- ---------------------------------------------------------------------------
-- WB→EX forwarding (back-to-back ALU)
-- ---------------------------------------------------------------------------

-- | Forwarding: result of one instruction available to the next.
prop_forwarding_chain :: Property
prop_forwarding_chain = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 10), -- x1 = 10
            (1, iADDI 2 1 1), -- x2 = x1 + 1 = 11  (needs forwarding)
            (2, iADDI 3 2 1) -- x3 = x2 + 1 = 12  (needs forwarding)
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 1 === 10
  reg rf 2 === 11
  reg rf 3 === 12

-- ---------------------------------------------------------------------------
-- Load / store
-- ---------------------------------------------------------------------------

-- | SW + LW round-trip: store a word, load it back.
prop_sw_lw :: Property
prop_sw_lw = property $ do
  -- Store 42 to data address 0x100 (within RAM), load it back into x4.
  let mem =
        buildMem
          [ (0, iADDI 1 0 0x100), -- x1 = 0x100 (base address)
            (1, iADDI 3 0 42), -- x3 = 42
            (2, iSW 1 3 0), -- mem[x1 + 0] = x3 = 42
            (3, iLW 4 1 0) -- x4 = mem[x1 + 0]
          ]
      (rf, _) = runUntilDone mem 30
  reg rf 4 === 42

-- | SB + LB: store a byte, sign-extend load.
prop_sb_lb :: Property
prop_sb_lb = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 0x100), -- x1 = 0x100 (base address)
            (1, iADDI 2 0 (-1)), -- x2 = 0xFF (low byte = 0xFF)
            (2, iSB 1 2 0), -- mem[0x100] = 0xFF
            (3, iLB 3 1 0) -- x3 = sign_ext(mem[0x100]) = -1 = 0xFFFFFFFF
          ]
      (rf, _) = runUntilDone mem 30
  reg rf 3 === 0xFFFFFFFF

-- | LBU: zero-extended byte load.
prop_sb_lbu :: Property
prop_sb_lbu = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 0x100),
            (1, iADDI 2 0 0xFF),
            (2, iSB 1 2 0),
            (3, iLBU 3 1 0) -- x3 = 0x000000FF
          ]
      (rf, _) = runUntilDone mem 30
  reg rf 3 === 0xFF

-- | SH + LH: store a halfword, sign-extend load.
prop_sh_lh :: Property
prop_sh_lh = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 0x100),
            (1, iADDI 2 0 (-1)), -- x2 = 0xFFFFFFFF; low 16 bits = 0xFFFF
            (2, iSH 1 2 0), -- store halfword 0xFFFF
            (3, iLH 3 1 0) -- x3 = sign_ext(0xFFFF) = -1 = 0xFFFFFFFF
          ]
      (rf, _) = runUntilDone mem 30
  reg rf 3 === 0xFFFFFFFF

-- | LHU: zero-extended halfword.
prop_sh_lhu :: Property
prop_sh_lhu = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 0x100),
            (1, iADDI 2 0 (-1)),
            (2, iSH 1 2 0),
            (3, iLHU 3 1 0) -- x3 = 0x0000FFFF
          ]
      (rf, _) = runUntilDone mem 30
  reg rf 3 === 0xFFFF

-- | Load-use hazard: LW followed immediately by an ADD using the loaded value.
--   The pipeline must stall one cycle so the loaded value is available.
prop_load_use_hazard :: Property
prop_load_use_hazard = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 0x100), -- x1 = base
            (1, iADDI 2 0 7), -- x2 = 7
            (2, iSW 1 2 0), -- mem[base] = 7
            (3, iLW 3 1 0), -- x3 = 7  (load)
            (4, iADD 4 3 2) -- x4 = x3 + x2 = 14  (load-use stall)
          ]
      (rf, _) = runUntilDone mem 40
  reg rf 4 === 14

-- ---------------------------------------------------------------------------
-- Branch instructions
-- ---------------------------------------------------------------------------

-- | BEQ taken: x1==x2 so jump over a write, x3 stays 0.
prop_beq_taken :: Property
prop_beq_taken = property $ do
  --  word  byte
  --  0     0x00  ADDI x1, x0, 5
  --  1     0x04  ADDI x2, x0, 5
  --  2     0x08  BEQ  x1, x2, +8   → branch to byte 0x10 (word 4)
  --  3     0x0C  ADDI x3, x0, 99   ← should be skipped
  --  4     0x10  NOP
  let mem =
        buildMem
          [ (0, iADDI 1 0 5),
            (1, iADDI 2 0 5),
            (2, iBEQ 1 2 8), -- offset +8 bytes → word 4
            (3, iADDI 3 0 99) -- skipped
          ]
      (rf, _) = runUntilDone mem 30
  reg rf 3 === 0

-- | BEQ not taken: x1 /= x2, so the instruction after the branch executes.
prop_beq_not_taken :: Property
prop_beq_not_taken = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 3),
            (1, iADDI 2 0 7),
            (2, iBEQ 1 2 8), -- not taken
            (3, iADDI 3 0 55) -- executes
          ]
      (rf, _) = runUntilDone mem 30
  reg rf 3 === 55

-- | BNE taken
prop_bne_taken :: Property
prop_bne_taken = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 1),
            (1, iADDI 2 0 2),
            (2, iBNE 1 2 8), -- 1 /= 2 → taken, skip word 3
            (3, iADDI 3 0 99) -- skipped
          ]
      (rf, _) = runUntilDone mem 30
  reg rf 3 === 0

-- | BLT taken (signed)
prop_blt_taken :: Property
prop_blt_taken = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 (-1)), -- x1 = -1
            (1, iADDI 2 0 1), -- x2 =  1
            (2, iBLT 1 2 8), -- -1 < 1 → taken
            (3, iADDI 3 0 99) -- skipped
          ]
      (rf, _) = runUntilDone mem 30
  reg rf 3 === 0

-- | BGE taken (signed >=)
prop_bge_taken :: Property
prop_bge_taken = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 5),
            (1, iADDI 2 0 5),
            (2, iBGE 1 2 8), -- 5 >= 5 → taken
            (3, iADDI 3 0 99) -- skipped
          ]
      (rf, _) = runUntilDone mem 30
  reg rf 3 === 0

-- | BLTU taken (unsigned): -1 as unsigned > 1, so 1 <u 0xFFFFFFFF
prop_bltu_taken :: Property
prop_bltu_taken = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 1), -- x1 = 1
            (1, iADDI 2 0 (-1)), -- x2 = 0xFFFFFFFF
            (2, iBLTU 1 2 8), -- 1 <u 0xFFFFFFFF → taken
            (3, iADDI 3 0 99) -- skipped
          ]
      (rf, _) = runUntilDone mem 30
  reg rf 3 === 0

-- | BGEU taken (unsigned >=)
prop_bgeu_taken :: Property
prop_bgeu_taken = property $ do
  let mem =
        buildMem
          [ (0, iADDI 1 0 (-1)), -- x1 = 0xFFFFFFFF (large unsigned)
            (1, iADDI 2 0 1), -- x2 = 1
            (2, iBGEU 1 2 8), -- 0xFFFFFFFF >=u 1 → taken
            (3, iADDI 3 0 99) -- skipped
          ]
      (rf, _) = runUntilDone mem 30
  reg rf 3 === 0

-- | Backward branch: loop runs 3 times.
--   x1 = 3; loop: x1 -= 1; if x1 != 0 goto loop; x2 = x1
--   Expected: x1 = 0, x2 = 0
prop_backward_branch_loop :: Property
prop_backward_branch_loop = property $ do
  --  word  byte
  --  0     0x00  ADDI x1, x0, 3
  --  1     0x04  ADDI x1, x1, -1   ← loop top
  --  2     0x08  BNE  x1, x0, -4   → back to byte 0x04 (word 1) when x1 != 0
  --  3     0x0C  ADD  x2, x1, x0   ← x2 = x1 after loop = 0
  let mem =
        buildMem
          [ (0, iADDI 1 0 3),
            (1, iADDI 1 1 (-1)), -- x1 -= 1
            (2, iBNE 1 0 (-4)), -- if x1 != 0 jump to byte offset -4 (→ word 1)
            (3, iADD 2 1 0) -- x2 = x1 = 0
          ]
      (rf, _) = runUntilDone mem 60
  reg rf 1 === 0
  reg rf 2 === 0

-- ---------------------------------------------------------------------------
-- JAL / JALR
-- ---------------------------------------------------------------------------

-- | JAL: link register gets PC+4, PC jumps.
prop_jal_link :: Property
prop_jal_link = property $ do
  --  word  byte
  --  0     0x00  JAL x1, +8    → jump to byte 0x08; x1 = 0x04
  --  1     0x04  ADDI x2, x0, 99  ← skipped
  --  2     0x08  NOP            ← lands here
  let mem =
        buildMem
          [ (0, iJAL 1 8), -- jump +8 bytes; x1 = 0x04
            (1, iADDI 2 0 99) -- skipped
          ]
      (rf, _) = runUntilDone mem 30
  reg rf 1 === 4 -- return address = PC+4 of JAL = 0x00+4 = 0x04
  reg rf 2 === 0 -- was skipped

-- | JALR: indirect jump, link register saved.
prop_jalr :: Property
prop_jalr = property $ do
  --  word  byte
  --  0     0x00  ADDI x2, x0, 12    -- x2 = 12 (target byte addr)
  --  1     0x04  JALR x1, x2, 0    -- jump to x2+0=12; x1 = 0x08
  --  2     0x08  ADDI x3, x0, 99   -- skipped
  --  3     0x0C  NOP               -- lands here
  let mem =
        buildMem
          [ (0, iADDI 2 0 12), -- x2 = 12 (byte address of word 3)
            (1, iJALR 1 2 0), -- x1 = 0x08, PC = 12
            (2, iADDI 3 0 99) -- skipped
          ]
      (rf, _) = runUntilDone mem 30
  reg rf 1 === 8 -- return address = byte addr of next instr after JALR = 4+4 = 8
  reg rf 3 === 0 -- was skipped

-- ---------------------------------------------------------------------------
-- Larger simulation: sum 1..10 using the built-in testProgRV
-- ---------------------------------------------------------------------------

-- | The built-in test program computes sum(1..10) = 55 into x2.
prop_sum1to10 :: Property
prop_sum1to10 = property $ do
  let (rf, _) = runUntilDone testProgRV 200
  reg rf 2 === 55

-- ---------------------------------------------------------------------------
-- UART memory-mapped I/O
-- ---------------------------------------------------------------------------

-- | Helper: collect all UART bytes that were emitted during a simulation run.
--   Drains 'uartPop' in a loop until the state is exhausted.
--   NOTE: because the pipeline retires at most one store per cycle, and
--   'runUntilDone' stops 4 cycles after the PC freezes, we inspect the
--   final memory-controller state directly rather than collecting mid-run.
uartByte :: MemCtrl -> Maybe (BitVector 8)
uartByte mc
  | uartTxValid (mcUart mc) = Just (uartTxByte (mcUart mc))
  | otherwise = Nothing

-- | SW to UART_TX (0x0001_0000) latches the low byte into UartState.
--   x1 = 0x0001_0000 (upper half via LUI); SW x2, x1, 0
prop_uart_sw_sets_valid :: Property
prop_uart_sw_sets_valid = property $ do
  -- LUI x1, 0x10 → x1 = 0x0001_0000
  -- ADDI x2, x0, 0x41  → x2 = 0x41 ('A')
  -- SW x1, x2, 0       → mem[0x0001_0000] = 0x41
  let mem =
        buildMem
          [ (0, iLUI 1 0x10), -- x1 = 0x0001_0000
            (1, iADDI 2 0 0x41), -- x2 = 'A' = 0x41
            (2, iSW 1 2 0) -- UART_TX ← 0x41
          ]
      (_, mc) = runUntilDone mem 30
  uartByte mc === Just 0x41

-- | SB to UART_TX: only the low 8 bits are captured.
prop_uart_sb_low_byte :: Property
prop_uart_sb_low_byte = property $ do
  let mem =
        buildMem
          [ (0, iLUI 1 0x10), -- x1 = 0x0001_0000
            (1, iADDI 2 0 0x42), -- x2 = 'B' = 0x42
            (2, iSB 1 2 0) -- SB: low byte of x2 → UART_TX
          ]
      (_, mc) = runUntilDone mem 30
  uartByte mc === Just 0x42

-- | SH to UART_TX: low 8 bits of the halfword are stored as the UART byte.
prop_uart_sh_low_byte :: Property
prop_uart_sh_low_byte = property $ do
  let mem =
        buildMem
          [ (0, iLUI 1 0x10), -- x1 = 0x0001_0000
            (1, iADDI 2 0 0x43), -- x2 = 'C' = 0x43
            (2, iSH 1 2 0) -- SH: low halfword of x2 → UART_TX
          ]
      (_, mc) = runUntilDone mem 30
  uartByte mc === Just 0x43

-- | LW from UART_TX (write-only) returns 0.
prop_uart_tx_read_returns_zero :: Property
prop_uart_tx_read_returns_zero = property $ do
  let mem =
        buildMem
          [ (0, iLUI 1 0x10), -- x1 = 0x0001_0000
            (1, iLW 2 1 0) -- x2 = mem[UART_TX] — should be 0
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 2 === 0

-- | LW from UART_STATUS (0x0001_0004) always returns 1 (tx_ready).
prop_uart_status_always_ready :: Property
prop_uart_status_always_ready = property $ do
  let mem =
        buildMem
          [ (0, iLUI 1 0x10), -- x1 = 0x0001_0000
            (1, iLW 2 1 4) -- x2 = mem[0x0001_0004] = UART_STATUS
          ]
      (rf, _) = runUntilDone mem 20
  reg rf 2 === 1

-- | uartPop clears the valid flag after draining one byte.
prop_uart_pop_clears_valid :: Property
prop_uart_pop_clears_valid = property $ do
  let mem =
        buildMem
          [ (0, iLUI 1 0x10),
            (1, iADDI 2 0 0x5A), -- x2 = 'Z'
            (2, iSW 1 2 0)
          ]
      (_, mc) = runUntilDone mem 30
  -- before pop, valid must be set
  uartTxValid (mcUart mc) === True
  -- after pop, valid must be cleared
  let (mc', byte) = uartPop mc
  byte === Just 0x5A
  uartTxValid (mcUart mc') === False

-- | Writing to UART_TX while UART is already valid overwrites the old byte.
prop_uart_overwrite :: Property
prop_uart_overwrite = property $ do
  -- Write 0x41 then 0x42; only 0x42 will be in the valid latch at the end (the pipeline retires one instruction at a time so the second SW wins).
  let mem =
        buildMem
          [ (0, iLUI 1 0x10), -- x1 = UART_TX base
            (1, iADDI 2 0 0x41), -- x2 = 'A'
            (2, iSW 1 2 0), -- UART_TX ← 'A'
            (3, iADDI 3 0 0x42), -- x3 = 'B'
            (4, iSW 1 3 0) -- UART_TX ← 'B' (overwrites 'A')
          ]
      (_, mc) = runUntilDone mem 40
  uartByte mc === Just 0x42

-- | A loop that writes bytes 1, 2, 3 to UART_TX sequentially.
--   Only the last written byte (3) is visible in the final UartState.
prop_uart_loop_last_byte :: Property
prop_uart_loop_last_byte = property $ do
  --  word  byte
  --  0     0x00  LUI  x1, 0x10        -- x1 = 0x0001_0000 (UART_TX)
  --  1     0x04  ADDI x2, x0, 1       -- x2 = counter = 1
  --  2     0x08  SW   x1, x2, 0       -- UART_TX ← x2              (loop top)
  --  3     0x0C  ADDI x2, x2, 1       -- x2 += 1
  --  4     0x10  ADDI x3, x0, 4       -- x3 = 4 (loop limit)
  --  5     0x14  BNE  x2, x3, -12     -- if x2 != 4 jump back to word 2
  --  -- after loop x2 = 4, last UART_TX byte written = 3
  let mem =
        buildMem
          [ (0, iLUI 1 0x10),
            (1, iADDI 2 0 1),
            (2, iSW 1 2 0),
            (3, iADDI 2 2 1),
            (4, iADDI 3 0 4),
            (5, iBNE 2 3 (-12)) -- BNE x2, x3, -12 bytes → word 2
          ]
      (_, mc) = runUntilDone mem 100
  uartByte mc === Just 3

-- ---------------------------------------------------------------------------
-- Property group
-- ---------------------------------------------------------------------------

cpuRiscVGroup :: [(PropertyName, Property)]
cpuRiscVGroup =
  [ ("addi basic", prop_addi_basic),
    ("addi negative imm", prop_addi_negative_imm),
    ("add reg-reg", prop_add_reg_reg),
    ("sub reg-reg", prop_sub_reg_reg),
    ("and reg-reg", prop_and_reg_reg),
    ("or reg-reg", prop_or_reg_reg),
    ("xori", prop_xori),
    ("xor reg-reg", prop_xor_reg_reg),
    ("andi", prop_andi),
    ("ori", prop_ori),
    ("lui", prop_lui),
    ("auipc", prop_auipc),
    ("slli", prop_slli),
    ("srli", prop_srli),
    ("srai", prop_srai),
    ("sll/srl reg-reg", prop_sll_srl),
    ("sra reg-reg", prop_sra),
    ("slt true", prop_slt_true),
    ("slt false", prop_slt_false),
    ("sltu", prop_sltu),
    ("slti", prop_slti),
    ("sltiu", prop_sltiu),
    ("x0 always zero", prop_x0_always_zero),
    ("forwarding chain", prop_forwarding_chain),
    ("sw/lw round-trip", prop_sw_lw),
    ("sb/lb sign-extend", prop_sb_lb),
    ("sb/lbu zero-extend", prop_sb_lbu),
    ("sh/lh sign-extend", prop_sh_lh),
    ("sh/lhu zero-extend", prop_sh_lhu),
    ("load-use hazard", prop_load_use_hazard),
    ("beq taken", prop_beq_taken),
    ("beq not taken", prop_beq_not_taken),
    ("bne taken", prop_bne_taken),
    ("blt taken", prop_blt_taken),
    ("bge taken", prop_bge_taken),
    ("bltu taken", prop_bltu_taken),
    ("bgeu taken", prop_bgeu_taken),
    ("backward branch loop", prop_backward_branch_loop),
    ("jal link register", prop_jal_link),
    ("jalr", prop_jalr),
    ("sum 1..10 loop", prop_sum1to10),
    ("uart sw sets valid", prop_uart_sw_sets_valid),
    ("uart sb low byte", prop_uart_sb_low_byte),
    ("uart sh low byte", prop_uart_sh_low_byte),
    ("uart tx read zero", prop_uart_tx_read_returns_zero),
    ("uart status ready", prop_uart_status_always_ready),
    ("uart pop clears valid", prop_uart_pop_clears_valid),
    ("uart overwrite", prop_uart_overwrite),
    ("uart loop last byte", prop_uart_loop_last_byte),
    -- Integration tests (full program simulations)
    ("count.S: uart output 0-9 newline", prop_count_s_uart),
    ("sum 1..10: uart byte = 55",        prop_sum_s_uart)
  ]

-- ===========================================================================
-- Integration tests: full program simulations
-- ===========================================================================

-- | Run the CPU collecting every UART byte as it is emitted, draining the UART latch after each step so back-to-back writes are all captured. Stops when the PC has been stationary for 4 consecutive cycles or the cycle budget is exhausted.
runCollectUart :: InstrMem -> Int -> [BitVector 8]
runCollectUart iMem budget = go initSimState budget (rvPC initCpuStateRV) 0 []
  where
    go _ 0 _ _ acc = P.reverse acc
    go st n lastPC sameCount acc
      | sameCount >= 4 = P.reverse acc
      | otherwise =
          let cpu      = simCpu st
              mc       = simData st
              ram      = simRam st
              pcW      = truncateB (rvPC cpu `shiftR` 2) :: Unsigned 10
              instr    = iMem !! pcW
              dataAddr = truncateB (unpack (exmemAluOut (rvExMem cpu)) `shiftR` 2) :: Unsigned 10
              dataWord = ram !! dataAddr
              (cpu', mc1, wc, pc, _, _, _) = stepCpuRV cpu (instr, dataWord, mc, True)
              ram'     = case wc of
                           Just (idx, w) -> replace idx w ram
                           Nothing       -> ram
              (mc2, mByte) = uartPop mc1
              acc'     = case mByte of { Just b -> b : acc; Nothing -> acc }
              st'      = st { simCpu = cpu', simData = mc2, simRam = ram' }
              same     = if pc == lastPC then sameCount + 1 else 0
           in go st' (n - 1) pc same acc'

-- ---------------------------------------------------------------------------
-- count.S (hand-assembled)
-- ---------------------------------------------------------------------------
--
-- Register map:  s0=x8 (UART_TX addr), t0=x5 (current char), t1=x6 ('9')
--
-- Layout:
--   word 0  (byte  0):  LUI  s0,  0x10        s0 = 0x00010000
--   word 1  (byte  4):  ADDI t0,  x0,  48     t0 = '0'
--   word 2  (byte  8):  ADDI t1,  x0,  57     t1 = '9'
--   word 3  (byte 12):  SB   t0,  0(s0)       .Lloop – UART_TX ← t0
--   word 4  (byte 16):  BEQ  t0,  t1, +12     if t0=='9' goto .Ldone (byte 28)
--   word 5  (byte 20):  ADDI t0,  t0,  1      t0 += 1
--   word 6  (byte 24):  JAL  x0, -12          j .Lloop  (byte 12)
--   word 7  (byte 28):  ADDI a0,  x0,  10     .Ldone – a0 = '\n'
--   word 8  (byte 32):  SB   a0,  0(s0)       UART_TX ← '\n'
--   word 9  (byte 36):  JAL  x0,   0          .Lhalt – spin

countSProg :: InstrMem
countSProg = buildMem
  [ (0, iLUI 8 0x10)           -- s0 = 0x00010000 (UART_TX)
  , (1, iADDI 5 0 48)          -- t0 = '0'
  , (2, iADDI 6 0 57)          -- t1 = '9'
  , (3, iSB 8 5 0)             -- .Lloop: UART_TX ← t0
  , (4, iBEQ 5 6 12)           -- if t0 == '9' goto .Ldone (offset +12 from byte 16)
  , (5, iADDI 5 5 1)           -- t0++
  , (6, iJAL 0 (-12))          -- j .Lloop (offset -12 from byte 24 → byte 12)
  , (7, iADDI 10 0 10)         -- .Ldone: a0 = '\n'
  , (8, iSB 8 10 0)            -- UART_TX ← '\n'
  , (9, iJAL 0 0)              -- .Lhalt: spin
  ]

-- | count.S simulation: the full UART output must be "0123456789\n".
prop_count_s_uart :: Property
prop_count_s_uart = property $ do
  let bytes    = runCollectUart countSProg 300
      expected = P.map (fromIntegral . fromEnum) "0123456789\n" :: [BitVector 8]
  bytes === expected

-- ---------------------------------------------------------------------------
-- Minimal sum 1..10 → UART (inline, no subroutine)
-- ---------------------------------------------------------------------------
--
-- Register map:  x1=i, x2=acc, x3=limit, x4=UART_TX
--
-- Layout:
--   word 0  (byte  0):  ADDI x1, x0, 1       i = 1
--   word 1  (byte  4):  ADDI x2, x0, 0       acc = 0
--   word 2  (byte  8):  ADD  x2, x2, x1      .Lloop: acc += i
--   word 3  (byte 12):  ADDI x1, x1, 1       i++
--   word 4  (byte 16):  ADDI x3, x0, 11      limit = 11
--   word 5  (byte 20):  BNE  x1, x3, -12     if i != 11 goto .Lloop (byte 8)
--   word 6  (byte 24):  LUI  x4, 0x10        x4 = UART_TX
--   word 7  (byte 28):  SB   x4, x2, 0       UART_TX ← acc (raw byte, = 55)
--   word 8  (byte 32):  JAL  x0, 0           spin

sumSProg :: InstrMem
sumSProg = buildMem
  [ (0, iADDI 1 0 1)           -- i = 1
  , (1, iADDI 2 0 0)           -- acc = 0
  , (2, iADD 2 2 1)            -- .Lloop: acc += i
  , (3, iADDI 1 1 1)           -- i++
  , (4, iADDI 3 0 11)          -- limit = 11
  , (5, iBNE 1 3 (-12))        -- if i != 11 goto .Lloop (offset -12 from byte 20)
  , (6, iLUI 4 0x10)           -- x4 = UART_TX
  , (7, iSB 4 2 0)             -- UART_TX ← acc = 55
  , (8, iJAL 0 0)              -- spin
  ]

-- | sum 1..10 simulation: the single emitted UART byte must be 55 (= 0x37).
prop_sum_s_uart :: Property
prop_sum_s_uart = property $ do
  let bytes = runCollectUart sumSProg 200
  bytes === [55]
