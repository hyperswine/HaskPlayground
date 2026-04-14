{-# OPTIONS_GHC -fexpose-all-unfoldings -fno-worker-wrapper #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

-- | RISC-V RV32I base integer instruction set CPU
--
-- ── Instruction formats ──────────────────────────────────────────────────
--   R-type:  [31:25] funct7 | [24:20] rs2 | [19:15] rs1 | [14:12] funct3 | [11:7] rd | [6:0] opcode
--   I-type:  [31:20] imm[11:0]             | [19:15] rs1 | [14:12] funct3 | [11:7] rd | [6:0] opcode
--   S-type:  [31:25] imm[11:5]| [24:20] rs2| [19:15] rs1 | [14:12] funct3 |[11:7] imm[4:0]| [6:0] opcode
--   B-type:  [31:25] imm[12|10:5]|[24:20]rs2|[19:15]rs1 | [14:12] funct3 |[11:7] imm[4:1|11]| [6:0] opcode
--   U-type:  [31:12] imm[31:12]                                             | [11:7] rd | [6:0] opcode
--   J-type:  [31:12] imm[20|10:1|11|19:12]                                 | [11:7] rd | [6:0] opcode
--
-- ── Opcodes (bits [6:0]) ─────────────────────────────────────────────────
--   0110011  R-type  (OP)         ADD SUB SLL SLT SLTU XOR SRL SRA OR AND
--   0010011  I-type  (OP-IMM)     ADDI SLTI SLTIU XORI ORI ANDI SLLI SRLI SRAI
--   0000011  I-type  (LOAD)       LB LH LW LBU LHU
--   0100011  S-type  (STORE)      SB SH SW
--   1100011  B-type  (BRANCH)     BEQ BNE BLT BGE BLTU BGEU
--   1101111  J-type  (JAL)
--   1100111  I-type  (JALR)
--   0110111  U-type  (LUI)
--   0010111  U-type  (AUIPC)
--   1110011  (SYSTEM – not implemented, treated as NOP)
--   0001111  (FENCE  – treated as NOP)
--
-- ── Pipeline ─────────────────────────────────────────────────────────────
--   3-stage: IF → ID/EX → MEM/WB
--   Simple in-order, stall-on-load (one bubble after a load before a use).
--   Forwarding from WB to EX for non-load results.
--
-- ── Memory ───────────────────────────────────────────────────────────────
--   Separate 32-bit instruction memory (1024 words, block RAM).
--   Byte-addressed data memory (1 KiB, synchronous read/write).
--   Data memory is word-granular internally; byte/halfword accesses are
--   handled by masking in software (sub-word reads sign-/zero-extend).

module CPURiscV where

import Clash.Prelude hiding (take)
import Data.Proxy (Proxy (..))
import qualified Prelude as P

-- ===========================================================================
-- Basic types
-- ===========================================================================

type Word32 = BitVector 32
type Addr   = Unsigned 32

-- | 32 general-purpose registers (x0..x31).  x0 is always 0.
type RegFile = Vec 32 Word32

-- | PC is 32 bits, byte-addressed.
type PC = Unsigned 32

-- | Register index (5 bits → 0..31).
type RegIdx = Unsigned 5

-- | Instruction memory: 1024 × 32-bit words (4 KiB), word-addressed.
type InstrMem = Vec 1024 Word32

-- | Data memory: 1024 × 32-bit words (4 KiB), word-addressed.
type DataMem = Vec 1024 Word32

-- | RAM pool backing the controller's RAM region (same layout as DataMem).
type RamPool = Vec 1024 Word32

-- ===========================================================================
-- Memory controller
-- ===========================================================================
--
-- Address map
-- ┌──────────────────────────────┬─────────────────────────────────────────┐
-- │ 0x0000_0000 – 0x0000_0FFF   │ Block RAM  (4 KiB, 1024 × 32-bit words) │
-- │ 0x0001_0000 – 0x0001_0003   │ UART_TX    (W: send byte; R: 0x00)      │
-- │ 0x0001_0004 – 0x0001_0007   │ UART_STATUS (R: bit 0 = tx_ready; W: -) │
-- └──────────────────────────────┴─────────────────────────────────────────┘

ramTop       :: Addr; ramTop       = 0x0000_0FFF
uartTxAddr   :: Addr; uartTxAddr   = 0x0001_0000
uartStatAddr :: Addr; uartStatAddr = 0x0001_0004

-- | Minimal simulated UART: captures the last byte written to UART_TX.
data UartState = UartState
  { uartTxByte  :: BitVector 8  -- last byte written to UART_TX
  , uartTxValid :: Bool         -- True when a new byte is available
  } deriving (Generic, NFDataX, Show)

initUartState :: UartState
initUartState = UartState 0 False

-- | Memory-controller state: UART peripheral only.
--   The 4 KiB data RAM is now a separate blockRam at the top level.
data MemCtrl = MemCtrl
  { mcUart :: UartState
  } deriving (Generic, NFDataX, Show)

initMemCtrl :: MemCtrl
initMemCtrl = MemCtrl initUartState

-- | Consume the pending UART TX byte (if any) and clear the valid flag.
uartPop :: MemCtrl -> (MemCtrl, Maybe (BitVector 8))
uartPop mc
  | uartTxValid (mcUart mc) =
      (mc { mcUart = initUartState }, Just (uartTxByte (mcUart mc)))
  | otherwise = (mc, Nothing)

-- ===========================================================================
-- Instruction decode helpers
-- ===========================================================================

-- | Extract the 7-bit opcode field.
opcode :: Word32 -> BitVector 7
opcode w = slice d6 d0 w

-- | Extract rd (bits [11:7]).
rdOf :: Word32 -> RegIdx
rdOf w = unpack (slice d11 d7 w)

-- | Extract funct3 (bits [14:12]).
funct3Of :: Word32 -> BitVector 3
funct3Of w = slice d14 d12 w

-- | Extract rs1 (bits [19:15]).
rs1Of :: Word32 -> RegIdx
rs1Of w = unpack (slice d19 d15 w)

-- | Extract rs2 (bits [24:20]).
rs2Of :: Word32 -> RegIdx
rs2Of w = unpack (slice d24 d20 w)

-- | Extract funct7 (bits [31:25]).
funct7Of :: Word32 -> BitVector 7
funct7Of w = slice d31 d25 w

-- | Sign-extend a 12-bit value to 32 bits (I-type immediate).
immI :: Word32 -> Signed 32
immI w = signExtend (unpack (slice d31 d20 w) :: Signed 12)

-- | Sign-extend S-type immediate (bits [31:25] and [11:7]).
immS :: Word32 -> Signed 32
immS w =
  let hi = slice d31 d25 w  -- imm[11:5]
      lo = slice d11 d7  w  -- imm[4:0]
      raw = hi ++# lo       -- 12-bit signed
  in signExtend (unpack raw :: Signed 12)

-- | Sign-extend B-type immediate (bits encode imm[12|10:5|4:1|11]).
--   Result is a signed byte offset; bit 0 is always 0.
immB :: Word32 -> Signed 32
immB w =
  let b12  = slice d31 d31 w
      b11  = slice d7  d7  w
      b10_5 = slice d30 d25 w
      b4_1  = slice d11 d8 w
      raw  = b12 ++# b11 ++# b10_5 ++# b4_1 ++# (0 :: BitVector 1)
  in signExtend (unpack raw :: Signed 13)

-- | U-type immediate: bits[31:12] shifted to upper 20 bits, lower 12 zeroed.
immU :: Word32 -> Signed 32
immU w = unpack (slice d31 d12 w ++# (0 :: BitVector 12))

-- | J-type immediate: imm[20|10:1|11|19:12], bit 0 always 0.
immJ :: Word32 -> Signed 32
immJ w =
  let b20    = slice d31 d31 w
      b19_12 = slice d19 d12 w
      b11    = slice d20 d20 w
      b10_1  = slice d30 d21 w
      raw    = b20 ++# b19_12 ++# b11 ++# b10_1 ++# (0 :: BitVector 1)
  in signExtend (unpack raw :: Signed 21)

-- ===========================================================================
-- Decoded instruction (micro-op)
-- ===========================================================================

data AluOp
  = AluAdd
  | AluSub
  | AluAnd
  | AluOr
  | AluXor
  | AluSll   -- shift left logical
  | AluSrl   -- shift right logical
  | AluSra   -- shift right arithmetic
  | AluSlt   -- set-less-than (signed)
  | AluSltu  -- set-less-than (unsigned)
  | AluLui   -- pass immediate as result (for LUI/AUIPC/JAL/JALR)
  deriving (Generic, NFDataX, Show, Eq)

-- | How to choose the ALU's second operand.
data AluSrc2 = Src2Reg | Src2Imm
  deriving (Generic, NFDataX, Show, Eq)

-- | What to write to rd.
data WbSrc
  = WbAlu    -- ALU result
  | WbPc4    -- PC+4 (for JAL/JALR link)
  | WbMem    -- data memory load
  deriving (Generic, NFDataX, Show, Eq)

-- | Memory access kind.
data MemOp
  = MemNone
  | MemLb | MemLh | MemLw | MemLbu | MemLhu  -- loads
  | MemSb | MemSh | MemSw                     -- stores
  deriving (Generic, NFDataX, Show, Eq)

-- | Branch/jump condition.
data BranchOp
  = BrNone
  | BrEq | BrNe | BrLt | BrGe | BrLtu | BrGeu  -- conditional branches
  | BrJal   -- unconditional (JAL / JALR always jump)
  deriving (Generic, NFDataX, Show, Eq)

-- | Decoded micro-op passed through the pipeline.
data MicroOp = MicroOp
  { uAluOp    :: AluOp
  , uAluSrc2  :: AluSrc2
  , uWbSrc    :: WbSrc
  , uWbEn     :: Bool       -- whether to write rd at all
  , uRd       :: RegIdx
  , uRs1      :: RegIdx
  , uRs2      :: RegIdx
  , uImm      :: Signed 32
  , uMemOp    :: MemOp
  , uBranchOp :: BranchOp
  , uAuipc    :: Bool       -- AUIPC: add PC to upper-imm before ALU
  } deriving (Generic, NFDataX, Show)

-- ===========================================================================
-- Instruction decode
-- ===========================================================================

-- Opcode constants
opOP     :: BitVector 7; opOP     = 0b0110011
opOPIMM  :: BitVector 7; opOPIMM  = 0b0010011
opLOAD   :: BitVector 7; opLOAD   = 0b0000011
opSTORE  :: BitVector 7; opSTORE  = 0b0100011
opBRANCH :: BitVector 7; opBRANCH = 0b1100011
opJAL    :: BitVector 7; opJAL    = 0b1101111
opJALR   :: BitVector 7; opJALR   = 0b1100111
opLUI    :: BitVector 7; opLUI    = 0b0110111
opAUIPC  :: BitVector 7; opAUIPC  = 0b0010111

nopMicroOp :: MicroOp
nopMicroOp = MicroOp
  { uAluOp    = AluAdd
  , uAluSrc2  = Src2Imm
  , uWbSrc    = WbAlu
  , uWbEn     = False
  , uRd       = 0
  , uRs1      = 0
  , uRs2      = 0
  , uImm      = 0
  , uMemOp    = MemNone
  , uBranchOp = BrNone
  , uAuipc    = False
  }

decode :: Word32 -> MicroOp
decode instr =
  let op  = opcode instr
      f3  = funct3Of instr
      f7  = funct7Of instr
      rd  = rdOf  instr
      rs1 = rs1Of instr
      rs2 = rs2Of instr
  in case op of

    -- ── R-type ───────────────────────────────────────────────────────────
    _ | op == opOP ->
      let aluOp = case (f3, f7) of
            (0b000, 0b0000000) -> AluAdd
            (0b000, 0b0100000) -> AluSub
            (0b001, _)         -> AluSll
            (0b010, _)         -> AluSlt
            (0b011, _)         -> AluSltu
            (0b100, _)         -> AluXor
            (0b101, 0b0000000) -> AluSrl
            (0b101, 0b0100000) -> AluSra
            (0b110, _)         -> AluOr
            (0b111, _)         -> AluAnd
            _                  -> AluAdd
      in nopMicroOp { uAluOp = aluOp, uAluSrc2 = Src2Reg
                    , uWbSrc = WbAlu, uWbEn = True
                    , uRd = rd, uRs1 = rs1, uRs2 = rs2 }

    -- ── I-type arithmetic ─────────────────────────────────────────────────
    _ | op == opOPIMM ->
      let shamt  = unpack (slice d24 d20 instr) :: Unsigned 5
          -- For SLLI/SRLI/SRAI imm field carries the shift amount in [4:0]
          aluOp = case f3 of
            0b000 -> AluAdd
            0b001 -> AluSll
            0b010 -> AluSlt
            0b011 -> AluSltu
            0b100 -> AluXor
            0b101 -> if f7 == 0b0100000 then AluSra else AluSrl
            0b110 -> AluOr
            0b111 -> AluAnd
            _     -> AluAdd
          -- SLLI/SRLI/SRAI use shamt, others use sign-extended 12-bit imm
          imm' = case f3 of
            0b001 -> fromIntegral shamt
            0b101 -> fromIntegral shamt
            _     -> immI instr
      in nopMicroOp { uAluOp = aluOp, uAluSrc2 = Src2Imm
                    , uWbSrc = WbAlu, uWbEn = True
                    , uRd = rd, uRs1 = rs1, uImm = imm' }

    -- ── LUI ──────────────────────────────────────────────────────────────
    _ | op == opLUI ->
      nopMicroOp { uAluOp = AluLui, uAluSrc2 = Src2Imm
                 , uWbSrc = WbAlu, uWbEn = True
                 , uRd = rd, uImm = immU instr }

    -- ── AUIPC ────────────────────────────────────────────────────────────
    _ | op == opAUIPC ->
      nopMicroOp { uAluOp = AluAdd, uAluSrc2 = Src2Imm
                 , uWbSrc = WbAlu, uWbEn = True
                 , uRd = rd, uImm = immU instr
                 , uAuipc = True }

    -- ── JAL ──────────────────────────────────────────────────────────────
    _ | op == opJAL ->
      nopMicroOp { uAluOp = AluLui, uAluSrc2 = Src2Imm
                 , uWbSrc = WbPc4, uWbEn = True
                 , uRd = rd, uImm = immJ instr
                 , uBranchOp = BrJal }

    -- ── JALR ─────────────────────────────────────────────────────────────
    _ | op == opJALR ->
      nopMicroOp { uAluOp = AluAdd, uAluSrc2 = Src2Imm
                 , uWbSrc = WbPc4, uWbEn = True
                 , uRd = rd, uRs1 = rs1, uImm = immI instr
                 , uBranchOp = BrJal }

    -- ── LOAD ─────────────────────────────────────────────────────────────
    _ | op == opLOAD ->
      let memOp = case f3 of
            0b000 -> MemLb
            0b001 -> MemLh
            0b010 -> MemLw
            0b100 -> MemLbu
            0b101 -> MemLhu
            _     -> MemLw
      in nopMicroOp { uAluOp = AluAdd, uAluSrc2 = Src2Imm
                    , uWbSrc = WbMem, uWbEn = True
                    , uRd = rd, uRs1 = rs1, uImm = immI instr
                    , uMemOp = memOp }

    -- ── STORE ────────────────────────────────────────────────────────────
    _ | op == opSTORE ->
      let memOp = case f3 of
            0b000 -> MemSb
            0b001 -> MemSh
            0b010 -> MemSw
            _     -> MemSw
      in nopMicroOp { uAluOp = AluAdd, uAluSrc2 = Src2Imm
                    , uWbSrc = WbAlu, uWbEn = False
                    , uRs1 = rs1, uRs2 = rs2, uImm = immS instr
                    , uMemOp = memOp }

    -- ── BRANCH ───────────────────────────────────────────────────────────
    _ | op == opBRANCH ->
      let brOp = case f3 of
            0b000 -> BrEq
            0b001 -> BrNe
            0b100 -> BrLt
            0b101 -> BrGe
            0b110 -> BrLtu
            0b111 -> BrGeu
            _     -> BrNone
      in nopMicroOp { uAluOp = AluAdd, uAluSrc2 = Src2Imm
                    , uWbSrc = WbAlu, uWbEn = False
                    , uRs1 = rs1, uRs2 = rs2, uImm = immB instr
                    , uBranchOp = brOp }

    -- ── FENCE / SYSTEM → NOP ─────────────────────────────────────────────
    _ -> nopMicroOp

-- ===========================================================================
-- ALU
-- ===========================================================================

alu :: AluOp -> Word32 -> Word32 -> Word32
alu op a b =
  let sa  = unpack a :: Signed 32
      sb  = unpack b :: Signed 32
      shamt = unpack (slice d4 d0 b) :: Unsigned 5
  in case op of
    AluAdd  -> pack (sa + sb)
    AluSub  -> pack (sa - sb)
    AluAnd  -> a .&. b
    AluOr   -> a .|. b
    AluXor  -> a `xor` b
    AluSll  -> pack (unpack a :: Unsigned 32) `shiftL` fromIntegral shamt
    AluSrl  -> pack (unpack a :: Unsigned 32) `shiftR` fromIntegral shamt
    AluSra  -> pack (sa `shiftR` fromIntegral shamt)
    AluSlt  -> if sa < sb then 1 else 0
    AluSltu -> if (unpack a :: Unsigned 32) < (unpack b :: Unsigned 32) then 1 else 0
    AluLui  -> b   -- pass-through (used for LUI and JAL which just write the imm)

-- | Evaluate a branch condition given two register values.
evalBranch :: BranchOp -> Word32 -> Word32 -> Bool
evalBranch brOp a b =
  let sa = unpack a :: Signed 32
      sb = unpack b :: Signed 32
      ua = unpack a :: Unsigned 32
      ub = unpack b :: Unsigned 32
  in case brOp of
    BrNone -> False
    BrEq   -> a == b
    BrNe   -> a /= b
    BrLt   -> sa < sb
    BrGe   -> sa >= sb
    BrLtu  -> ua < ub
    BrGeu  -> ua >= ub
    BrJal  -> True   -- unconditional

-- ===========================================================================
-- Data memory helpers (byte/halfword sub-word access)
-- ===========================================================================

-- | Extract a sub-word from a pre-fetched 32-bit word.
--   byteAddr supplies the byte offset; 'word' is the full aligned word.
memLoad :: MemOp -> Addr -> Word32 -> Word32
memLoad memOp byteAddr word =
  let byteOff = resize (byteAddr .&. 3) :: Unsigned 2
      -- extract byte / halfword based on byte offset
      byte0   = slice d7  d0  word
      byte1   = slice d15 d8  word
      byte2   = slice d23 d16 word
      byte3   = slice d31 d24 word
      selByte = case byteOff of
        0 -> byte0; 1 -> byte1; 2 -> byte2; _ -> byte3
      half0   = slice d15 d0  word
      half1   = slice d31 d16 word
      selHalf = if byteOff < 2 then half0 else half1
  in case memOp of
    MemLb  -> pack (signExtend (unpack selByte  :: Signed 8)  :: Signed 32)
    MemLbu -> zeroExtend selByte
    MemLh  -> pack (signExtend (unpack selHalf  :: Signed 16) :: Signed 32)
    MemLhu -> zeroExtend selHalf
    MemLw  -> word
    _      -> 0  -- non-load: unused

-- | Produce the (wordIdx, newWord) pair for a data memory write.
--   'oldWord' is the current word at the target address (pre-fetched from BRAM);
--   it is used for the read-modify-write of sub-word (byte / halfword) stores.
--   Returns Nothing when memOp is not a store.
memStore :: MemOp -> Addr -> Word32 -> Word32 -> Maybe (Unsigned 10, Word32)
memStore memOp byteAddr storeVal oldWord =
  let wordIdx = truncateB (byteAddr `shiftR` 2) :: Unsigned 10
      byteOff = resize (byteAddr .&. 3) :: Unsigned 2
      b8  = zeroExtend (slice d7  d0  storeVal) :: BitVector 32
      h16 = zeroExtend (slice d15 d0  storeVal) :: BitVector 32
      newWord = case memOp of
        MemSb -> case byteOff of
          0 -> (oldWord .&. 0xFFFFFF00) .|. b8
          1 -> (oldWord .&. 0xFFFF00FF) .|. (b8  `shiftL` 8)
          2 -> (oldWord .&. 0xFF00FFFF) .|. (b8  `shiftL` 16)
          _ -> (oldWord .&. 0x00FFFFFF) .|. (b8  `shiftL` 24)
        MemSh ->
          if byteOff < 2
            then (oldWord .&. 0xFFFF0000) .|. h16
            else (oldWord .&. 0x0000FFFF) .|. (h16 `shiftL` 16)
        MemSw -> storeVal
        _     -> oldWord
  in case memOp of
    MemNone -> Nothing
    MemLb   -> Nothing
    MemLh   -> Nothing
    MemLw   -> Nothing
    MemLbu  -> Nothing
    MemLhu  -> Nothing
    _       -> Just (wordIdx, newWord)

-- ===========================================================================
-- Memory controller dispatch
-- ===========================================================================

-- | Route a load through the memory controller.
--   'ramWord' is the word pre-fetched from the data blockRam this cycle.
memCtrlLoad :: MemCtrl -> MemOp -> Addr -> Word32 -> Word32
memCtrlLoad _mc memOp byteAddr ramWord
  | byteAddr <= ramTop       = memLoad memOp byteAddr ramWord
  | byteAddr == uartTxAddr   = 0  -- TX is write-only; reads return 0
  | byteAddr == uartStatAddr = 1  -- STATUS bit 0 = tx_ready, always 1 in sim
  | otherwise                = 0

-- | Route a store through the memory controller.
--   Returns the updated controller state and an optional data-BRAM write command.
--   'oldWord' is the word pre-fetched from the data blockRam (needed for RMW).
memCtrlStore :: MemCtrl -> MemOp -> Addr -> Word32 -> Word32
            -> (MemCtrl, Maybe (Unsigned 10, Word32))
memCtrlStore mc memOp byteAddr storeVal oldWord
  | byteAddr <= ramTop    = (mc, memStore memOp byteAddr storeVal oldWord)
  | byteAddr == uartTxAddr =
      case memOp of
        MemSb -> (mc { mcUart = UartState (truncateB (pack storeVal)) True }, Nothing)
        MemSh -> (mc { mcUart = UartState (truncateB (pack storeVal)) True }, Nothing)
        MemSw -> (mc { mcUart = UartState (truncateB (pack storeVal)) True }, Nothing)
        _     -> (mc, Nothing)
  | otherwise = (mc, Nothing)

-- ===========================================================================
-- Pipeline state
-- ===========================================================================

-- ---------------------------------------------------------------------------
-- IF/ID pipeline register
-- ---------------------------------------------------------------------------

data IfIdReg = IfIdReg
  { ifidValid :: Bool
  , ifidPC    :: PC
  , ifidInstr :: Word32
  } deriving (Generic, NFDataX, Show)

emptyIfId :: IfIdReg
emptyIfId = IfIdReg False 0 0

-- ---------------------------------------------------------------------------
-- ID/EX pipeline register
-- Note: we merge decode + register-read into one combined ID/EX stage.
-- ---------------------------------------------------------------------------

data IdExReg = IdExReg
  { idexValid  :: Bool
  , idexPC     :: PC
  , idexUop    :: MicroOp
  , idexRs1Val :: Word32
  , idexRs2Val :: Word32
  } deriving (Generic, NFDataX, Show)

emptyIdEx :: IdExReg
emptyIdEx = IdExReg False 0 nopMicroOp 0 0

-- ---------------------------------------------------------------------------
-- EX/MEM/WB pipeline register (single combined stage)
-- ---------------------------------------------------------------------------

data ExWbReg = ExWbReg
  { exwbValid   :: Bool
  , exwbPC      :: PC
  , exwbRd      :: RegIdx
  , exwbWbEn    :: Bool
  , exwbWbSrc   :: WbSrc
  , exwbAluOut  :: Word32   -- ALU result / effective address
  , exwbRs2Val  :: Word32   -- original rs2 (for stores)
  , exwbMemOp   :: MemOp
  } deriving (Generic, NFDataX, Show)

emptyExWb :: ExWbReg
emptyExWb = ExWbReg False 0 0 False WbAlu 0 0 MemNone

-- ---------------------------------------------------------------------------
-- Overall CPU state
-- ---------------------------------------------------------------------------

data CpuStateRV = CpuStateRV
  { rvPC      :: PC
  , rvRegs    :: RegFile
  , rvIfId    :: IfIdReg
  , rvIdEx    :: IdExReg
  , rvExWb    :: ExWbReg
  , rvHalt    :: Bool
  } deriving (Generic, NFDataX, Show)

initCpuStateRV :: CpuStateRV
initCpuStateRV = CpuStateRV
  { rvPC   = 0
  , rvRegs = repeat 0
  , rvIfId = emptyIfId
  , rvIdEx = emptyIdEx
  , rvExWb = emptyExWb
  , rvHalt = False
  }

-- ===========================================================================
-- CPU step function
-- ===========================================================================

-- | One-cycle step of the RV32I pipeline.
--
--   Inputs:
--     instrWord    – instruction from instruction memory at rvPC
--     dataBramWord – word pre-fetched from the data blockRam
--                    (the aligned word at the current MEM/WB effective address).
--                    For synthesis this comes from a blockRam with 1-cycle
--                    latency; for simulation it is read combinatorially.
--     memCtrl      – peripheral state (UART only; data RAM is external)
--     en           – clock enable
--
--   Outputs:
--     (newState, newMemCtrl, dataWrCmd, newPC, wbRd, wbVal, wbValid)
--     dataWrCmd – optional write to the data blockRam produced by stores.
stepCpuRV
  :: CpuStateRV
  -> (Word32, Word32, MemCtrl, Bool)
  -> (CpuStateRV, MemCtrl, Maybe (Unsigned 10, Word32), PC, RegIdx, Word32, Bool)
stepCpuRV s@CpuStateRV{..} (instrWord, dataBramWord, memCtrl, en)
  | rvHalt  = (s, memCtrl, Nothing, rvPC, 0, 0, False)
  | not en  = (s, memCtrl, Nothing, rvPC, 0, 0, False)
  | otherwise =
      -- ── Stage 3: MEM / WB ────────────────────────────────────────────
      let exwb = rvExWb

          -- Data memory access for loads/stores
          (memCtrl', dataWrCmd, wbVal) =
            if exwbValid exwb
              then
                let addr   = unpack (exwbAluOut exwb) :: Addr
                    storeV = exwbRs2Val exwb
                    mop    = exwbMemOp exwb
                in case mop of
                     -- loads: use the pre-fetched word from the data blockRam
                     MemLb   -> (memCtrl, Nothing, memCtrlLoad memCtrl mop addr dataBramWord)
                     MemLbu  -> (memCtrl, Nothing, memCtrlLoad memCtrl mop addr dataBramWord)
                     MemLh   -> (memCtrl, Nothing, memCtrlLoad memCtrl mop addr dataBramWord)
                     MemLhu  -> (memCtrl, Nothing, memCtrlLoad memCtrl mop addr dataBramWord)
                     MemLw   -> (memCtrl, Nothing, memCtrlLoad memCtrl mop addr dataBramWord)
                     -- stores: return updated peripheral state + BRAM write command
                     MemSb   -> let (mc', wc) = memCtrlStore memCtrl mop addr storeV dataBramWord in (mc', wc, 0)
                     MemSh   -> let (mc', wc) = memCtrlStore memCtrl mop addr storeV dataBramWord in (mc', wc, 0)
                     MemSw   -> let (mc', wc) = memCtrlStore memCtrl mop addr storeV dataBramWord in (mc', wc, 0)
                     MemNone -> (memCtrl, Nothing, exwbAluOut exwb)
              else (memCtrl, Nothing, 0)

          -- Choose write-back value
          wbResult = case exwbWbSrc exwb of
            WbAlu -> exwbAluOut exwb
            WbPc4 -> pack (exwbPC exwb + 4)
            WbMem -> wbVal

          -- Write to register file; x0 is always zero
          wbEn   = exwbValid exwb && exwbWbEn exwb && exwbRd exwb /= 0
          regs1  = if wbEn then replace (exwbRd exwb) wbResult rvRegs else rvRegs

          -- ── Stage 2: EX ──────────────────────────────────────────────
          idex = rvIdEx

          -- Forward WB→EX: if the EX/WB stage writes a register that ID/EX reads
          fwdRs1 =
            if wbEn && exwbRd exwb == uRs1 (idexUop idex)
              then wbResult
              else idexRs1Val idex

          fwdRs2 =
            if wbEn && exwbRd exwb == uRs2 (idexUop idex)
              then wbResult
              else idexRs2Val idex

          uop = idexUop idex

          -- ALU operand B: register or immediate
          aluB = case uAluSrc2 uop of
            Src2Reg -> fwdRs2
            Src2Imm -> pack (uImm uop)

          -- AUIPC: ALU input A = PC instead of rs1
          aluA = if uAuipc uop
                   then pack (idexPC idex)
                   else fwdRs1

          aluResult = alu (uAluOp uop) aluA aluB

          -- Branch / jump target computation
          brTaken = evalBranch (uBranchOp uop) fwdRs1 fwdRs2
          brTarget = case uBranchOp uop of
            BrJal -> -- JAL: PC + imm; JALR: (rs1 + imm) & ~1
              if opcode (ifidInstr rvIfId) == opJALR
                -- JALR target is ALU result (rs1+imm) with bit 0 cleared
                then unpack (aluResult .&. complement 1) :: PC
                else unpack (pack (fromIntegral (idexPC idex) + fromIntegral (uImm uop) :: Signed 32)) :: PC
            _ ->
              unpack (pack (fromIntegral (idexPC idex) + fromIntegral (uImm uop) :: Signed 32)) :: PC

          squash = idexValid idex && (brTaken || uBranchOp uop == BrJal)

          exwb' = if idexValid idex
                    then ExWbReg
                           { exwbValid  = True
                           , exwbPC     = idexPC idex
                           , exwbRd     = uRd uop
                           , exwbWbEn   = uWbEn uop
                           , exwbWbSrc  = uWbSrc uop
                           , exwbAluOut = aluResult
                           , exwbRs2Val = fwdRs2
                           , exwbMemOp  = uMemOp uop
                           }
                    else emptyExWb

          -- ── Load-use hazard detection ─────────────────────────────────
          -- If the ID/EX stage is a load and its rd matches one of the
          -- IF/ID stage's source registers, we must stall one cycle.
          isLoad op = case op of
            MemLb  -> True
            MemLbu -> True
            MemLh  -> True
            MemLhu -> True
            MemLw  -> True
            _      -> False
          loadUseHazard =
            idexValid idex
            && isLoad (uMemOp uop)
            && uWbEn uop
            && uRd uop /= 0
            && let decRs1 = rs1Of (ifidInstr rvIfId)
                   decRs2 = rs2Of (ifidInstr rvIfId)
               in uRd uop == decRs1 || uRd uop == decRs2

          -- ── Stage 1: ID / register read ──────────────────────────────
          ifid = rvIfId

          -- Decode the instruction sitting in IF/ID
          uop' = decode (ifidInstr ifid)

          -- Read registers (using the updated register file from WB)
          rs1v = regs1 !! uRs1 uop'
          rs2v = regs1 !! uRs2 uop'

          idex' =
            if squash || not (ifidValid ifid) || loadUseHazard
              then emptyIdEx
              else IdExReg
                     { idexValid  = True
                     , idexPC     = ifidPC ifid
                     , idexUop    = uop'
                     , idexRs1Val = rs1v
                     , idexRs2Val = rs2v
                     }

          -- ── Stage 0: IF (advance PC) ──────────────────────────────────
          -- When stalling (load-use): hold PC and IF/ID, insert bubble into ID/EX
          nextPC
            | loadUseHazard = rvPC
            | squash = brTarget
            | otherwise = rvPC + 4

          ifid' =
            if loadUseHazard
              then ifid  -- hold
              else IfIdReg
                     { ifidValid = True
                     , ifidPC    = rvPC
                     , ifidInstr = instrWord
                     }

          s' = s
            { rvPC   = nextPC
            , rvRegs = regs1
            , rvIfId = ifid'
            , rvIdEx = idex'
            , rvExWb = exwb'
            , rvHalt = False
            }

      in (s', memCtrl', dataWrCmd, nextPC, exwbRd exwb, wbResult, wbEn)

-- ===========================================================================
-- Simple simulation helper (no UART)
-- ===========================================================================

data SimState = SimState
  { simCpu  :: CpuStateRV
  , simData :: MemCtrl
  , simRam  :: Vec 1024 Word32   -- data RAM (separate from MemCtrl for synthesis)
  } deriving (Generic, NFDataX, Show)

initSimState :: SimState
initSimState = SimState initCpuStateRV initMemCtrl (repeat 0)

-- | Pre-fetch the aligned word the MEM/WB stage will read this cycle.
--   In hardware this is provided by the data blockRam (1-cycle latency,
--   address driven from the previous cycle's ExWb output).
simDataWord :: SimState -> Word32
simDataWord st =
  let addr = truncateB (unpack (exwbAluOut (rvExWb (simCpu st))) `shiftR` 2) :: Unsigned 10
  in simRam st !! addr

-- | Run the CPU for n cycles on a fixed instruction memory image.
--   Returns a list of (pc, wbRd, wbVal, wbEn) per cycle.
simulateRV :: InstrMem -> Int -> [(PC, RegIdx, Word32, Bool)]
simulateRV iMem n = P.take n $ go initSimState
  where
    go st =
      let cpu   = simCpu st
          mc    = simData st
          pcW   = truncateB (rvPC cpu `shiftR` 2) :: Unsigned 10
          instr = iMem !! pcW
          dword = simDataWord st
          (cpu', mc', wc, pc, rd, val, wbEn) = stepCpuRV cpu (instr, dword, mc, True)
          ram'  = case wc of
            Just (idx, w) -> replace idx w (simRam st)
            Nothing       -> simRam st
          st'   = st { simCpu = cpu', simData = mc', simRam = ram' }
      in (pc, rd, val, wbEn) : go st'

-- ===========================================================================
-- Instruction assembly helpers
-- ===========================================================================

-- | Build an R-type instruction word.
mkR :: BitVector 7 -> RegIdx -> RegIdx -> RegIdx -> BitVector 3 -> BitVector 7 -> Word32
mkR op rd rs1 rs2 f3 f7 =
  f7 ++# pack rs2 ++# pack rs1 ++# f3 ++# pack rd ++# op

-- | Build an I-type instruction word.
mkI :: BitVector 7 -> RegIdx -> BitVector 3 -> RegIdx -> Signed 12 -> Word32
mkI op rd f3 rs1 imm =
  pack imm ++# pack rs1 ++# f3 ++# pack rd ++# op

-- | Build an S-type instruction word.
mkS :: BitVector 7 -> RegIdx -> RegIdx -> BitVector 3 -> Signed 12 -> Word32
mkS op rs1 rs2 f3 imm =
  let immBV = pack imm
      hi    = slice d11 d5 immBV
      lo    = slice d4  d0 immBV
  in hi ++# pack rs2 ++# pack rs1 ++# f3 ++# lo ++# op

-- | Build a B-type instruction word.
mkB :: BitVector 7 -> RegIdx -> RegIdx -> BitVector 3 -> Signed 13 -> Word32
mkB op rs1 rs2 f3 imm =
  let immBV = pack imm   -- 13 bits: [12:0]
      b12   = slice d12 d12 immBV
      b11   = slice d11 d11 immBV
      b10_5 = slice d10 d5  immBV
      b4_1  = slice d4  d1  immBV
  in (b12 ++# b10_5) ++# pack rs2 ++# pack rs1 ++# f3 ++# (b4_1 ++# b11) ++# op

-- | Build a U-type instruction word (imm is the upper 20 bits).
mkU :: BitVector 7 -> RegIdx -> BitVector 20 -> Word32
mkU op rd imm20 = imm20 ++# pack rd ++# op

-- | Build a J-type instruction word.
mkJ :: BitVector 7 -> RegIdx -> Signed 21 -> Word32
mkJ op rd imm =
  let immBV = pack imm   -- 21 bits
      b20    = slice d20 d20 immBV
      b19_12 = slice d19 d12 immBV
      b11    = slice d11 d11 immBV
      b10_1  = slice d10 d1  immBV
  in (b20 ++# b10_1 ++# b11 ++# b19_12) ++# pack rd ++# op

-- Convenience constructors ---------------------------------------------------

-- R-type
iADD  rd rs1 rs2 = mkR opOP rd rs1 rs2 0b000 0b0000000
iSUB  rd rs1 rs2 = mkR opOP rd rs1 rs2 0b000 0b0100000
iSLL  rd rs1 rs2 = mkR opOP rd rs1 rs2 0b001 0b0000000
iSLT  rd rs1 rs2 = mkR opOP rd rs1 rs2 0b010 0b0000000
iSLTU rd rs1 rs2 = mkR opOP rd rs1 rs2 0b011 0b0000000
iXOR  rd rs1 rs2 = mkR opOP rd rs1 rs2 0b100 0b0000000
iSRL  rd rs1 rs2 = mkR opOP rd rs1 rs2 0b101 0b0000000
iSRA  rd rs1 rs2 = mkR opOP rd rs1 rs2 0b101 0b0100000
iOR   rd rs1 rs2 = mkR opOP rd rs1 rs2 0b110 0b0000000
iAND  rd rs1 rs2 = mkR opOP rd rs1 rs2 0b111 0b0000000

-- I-type arithmetic
iADDI  rd rs1 imm = mkI opOPIMM rd 0b000 rs1 imm
iSLTI  rd rs1 imm = mkI opOPIMM rd 0b010 rs1 imm
iSLTIU rd rs1 imm = mkI opOPIMM rd 0b011 rs1 imm
iXORI  rd rs1 imm = mkI opOPIMM rd 0b100 rs1 imm
iORI   rd rs1 imm = mkI opOPIMM rd 0b110 rs1 imm
iANDI  rd rs1 imm = mkI opOPIMM rd 0b111 rs1 imm

-- Shifts with shamt encoded in imm[4:0]
iSLLI rd rs1 sh = mkI opOPIMM rd 0b001 rs1 (fromIntegral (sh :: Unsigned 5) :: Signed 12)
iSRLI rd rs1 sh = mkI opOPIMM rd 0b101 rs1 (fromIntegral (sh :: Unsigned 5) :: Signed 12)
iSRAI rd rs1 sh = mkI opOPIMM rd 0b101 rs1 (0b0100000_00000 .|. fromIntegral (sh :: Unsigned 5) :: Signed 12)

-- Loads
iLB  rd rs1 imm = mkI opLOAD rd 0b000 rs1 imm
iLH  rd rs1 imm = mkI opLOAD rd 0b001 rs1 imm
iLW  rd rs1 imm = mkI opLOAD rd 0b010 rs1 imm
iLBU rd rs1 imm = mkI opLOAD rd 0b100 rs1 imm
iLHU rd rs1 imm = mkI opLOAD rd 0b101 rs1 imm

-- Stores
iSB rs1 rs2 imm = mkS opSTORE rs1 rs2 0b000 imm
iSH rs1 rs2 imm = mkS opSTORE rs1 rs2 0b001 imm
iSW rs1 rs2 imm = mkS opSTORE rs1 rs2 0b010 imm

-- Branches (imm is byte offset, must be even)
iBEQ  rs1 rs2 imm = mkB opBRANCH rs1 rs2 0b000 imm
iBNE  rs1 rs2 imm = mkB opBRANCH rs1 rs2 0b001 imm
iBLT  rs1 rs2 imm = mkB opBRANCH rs1 rs2 0b100 imm
iBGE  rs1 rs2 imm = mkB opBRANCH rs1 rs2 0b101 imm
iBLTU rs1 rs2 imm = mkB opBRANCH rs1 rs2 0b110 imm
iBGEU rs1 rs2 imm = mkB opBRANCH rs1 rs2 0b111 imm

-- Upper-immediate / jumps
iLUI   rd imm20 = mkU opLUI   rd imm20
iAUIPC rd imm20 = mkU opAUIPC rd imm20
iJAL   rd imm   = mkJ opJAL   rd imm
iJALR  rd rs1 imm = mkI opJALR rd 0b000 rs1 imm

iNOP :: Word32
iNOP = iADDI 0 0 0  -- ADDI x0, x0, 0

-- ===========================================================================
-- Example test program
-- ===========================================================================

-- | Compute 1+2+3+...+10 (= 55) and store result in x1.
--   x1=0, x2=10 (counter), x3=1 (step), x4=accumulator
--   loop: if x2 == 0 goto done; x4 += x2; x2 -= 1; goto loop
testProgRV :: InstrMem
testProgRV =
  let nop = iNOP
      base = repeat nop :: InstrMem
      -- x0 is always 0
      -- x1 = 10  (loop counter)
      -- x2 = 0   (accumulator, starts 0)
  in     replace ( 0 :: Unsigned 10) (iADDI 1 0 10)     -- x1 = 10
       $ replace ( 1 :: Unsigned 10) (iADDI 2 0 0)      -- x2 = 0
       -- loop (byte addr 8, word addr 2):
       $ replace ( 2 :: Unsigned 10) (iBEQ  1 0 16)     -- if x1==0 goto +16 (word 6 = done)
       $ replace ( 3 :: Unsigned 10) (iADD  2 2 1)      -- x2 += x1
       $ replace ( 4 :: Unsigned 10) (iADDI 1 1 (-1))   -- x1 -= 1
       $ replace ( 5 :: Unsigned 10) (iBEQ  0 0 (-12))  -- goto -12 (back to word 2, byte 8)
       -- done (word 6, byte 24):
       $ replace ( 6 :: Unsigned 10) nop
       $ base
