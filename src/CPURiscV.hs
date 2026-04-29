{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings -fno-worker-wrapper #-}
{-# HLINT ignore "Use newtype instead of data" #-}

-- | RISC-V RV32I base integer instruction set CPU
--
-- ── Optimisations vs v1 ──────────────────────────────────────────────────
--
--   CP1 – Two-level forwarding.
--         The MEM/WB bypass is pre-applied at the END of the ID stage so
--         the forwarded values are stored in IdExReg.  EX therefore only
--         needs to check EX/MEM (one mux level), cutting the ALU-input
--         combinatorial cone roughly in half.
--
--   CP2 – Registered BHT.
--         IF reads rvBHT, the BHT committed at the end of the previous
--         cycle.  The in-cycle update (bht') is only stored into s'; it
--         does NOT feed back into IF in the same cycle.  This breaks the
--         EX-branch-resolve → Vec-replace → BHT-lookup → ifPredTarget loop
--         which was the single longest combinatorial path.
--
--   CP3 – Registered load-use hazard.
--         stallNext' is computed at end of cycle N (ID/EX load instruction
--         vs IF/ID register indices) and stored in rvStallNext.  Cycle N+1
--         reads rvStallNext instead of recomputing the hazard from scratch,
--         removing the decode-and-compare cone from the nextPC/idex' path.
--
--   CP4 – External blockRam register file.
--         The register file is promoted from Vec 32 Word32 (32-way mux on
--         every write) to an external synchronous blockRam managed by the
--         top level.  stepCpuRV receives two pre-fetched read words
--         (regRdA for rs1, regRdB for rs2) and emits a Maybe write command.
--         ID pre-applies the WB bypass (already needed for CP1) so the
--         single cycle of blockRam read latency is fully hidden.
--
-- ── Pipeline ─────────────────────────────────────────────────────────────
--   5-stage: IF → ID → EX → MEM → WB
--   In-order scalar.
--   Branch resolution in EX; 2-bit saturating BHT (64 entries, registered).
--   JAL predicted taken in ID; JALR always flushes.
--
module CPURiscV where

import Clash.Prelude hiding (take)
import qualified Prelude as P

-- ===========================================================================
-- Basic types
-- ===========================================================================

type Word32 = BitVector 32
type Addr   = Unsigned 32
type RegFile = Vec 32 Word32
type PC      = Unsigned 32
type RegIdx  = Unsigned 5
type InstrMem = Vec 1024 Word32
type DataMem  = Vec 1024 Word32
type RamPool  = Vec 1024 Word32

-- ===========================================================================
-- Memory controller
-- ===========================================================================

ramTop       :: Addr; ramTop       = 0x0000_0FFF
uartTxAddr   :: Addr; uartTxAddr   = 0x0001_0000
uartStatAddr :: Addr; uartStatAddr = 0x0001_0004

data UartState = UartState
  { uartTxByte  :: BitVector 8
  , uartTxValid :: Bool
  } deriving (Generic, NFDataX, Show)

initUartState :: UartState
initUartState = UartState 0 False

data MemCtrl = MemCtrl { mcUart :: UartState }
  deriving (Generic, NFDataX, Show)

initMemCtrl :: MemCtrl
initMemCtrl = MemCtrl initUartState

uartPop :: MemCtrl -> (MemCtrl, Maybe (BitVector 8))
uartPop mc
  | uartTxValid (mcUart mc) = (mc { mcUart = initUartState }, Just (uartTxByte (mcUart mc)))
  | otherwise               = (mc, Nothing)

-- ===========================================================================
-- Instruction decode helpers
-- ===========================================================================

opcode  :: Word32 -> BitVector 7; opcode  w = slice d6  d0  w
rdOf    :: Word32 -> RegIdx;      rdOf    w = unpack (slice d11 d7  w)
funct3Of :: Word32 -> BitVector 3; funct3Of w = slice d14 d12 w
rs1Of   :: Word32 -> RegIdx;      rs1Of   w = unpack (slice d19 d15 w)
rs2Of   :: Word32 -> RegIdx;      rs2Of   w = unpack (slice d24 d20 w)
funct7Of :: Word32 -> BitVector 7; funct7Of w = slice d31 d25 w

immI :: Word32 -> Signed 32
immI w = signExtend (unpack (slice d31 d20 w) :: Signed 12)

immS :: Word32 -> Signed 32
immS w = signExtend (unpack (slice d31 d25 w ++# slice d11 d7 w) :: Signed 12)

immB :: Word32 -> Signed 32
immB w = signExtend (unpack ( slice d31 d31 w ++# slice d7  d7  w
                           ++# slice d30 d25 w ++# slice d11 d8  w
                           ++# (0 :: BitVector 1)) :: Signed 13)

immU :: Word32 -> Signed 32
immU w = unpack (slice d31 d12 w ++# (0 :: BitVector 12))

immJ :: Word32 -> Signed 32
immJ w = signExtend (unpack ( slice d31 d31 w ++# slice d19 d12 w
                           ++# slice d20 d20 w ++# slice d30 d21 w
                           ++# (0 :: BitVector 1)) :: Signed 21)

-- ===========================================================================
-- Decoded instruction (micro-op)
-- ===========================================================================

data AluOp
  = AluAdd | AluSub | AluAnd | AluOr  | AluXor
  | AluSll | AluSrl | AluSra
  | AluSlt | AluSltu
  | AluLui
  deriving (Generic, NFDataX, Show, Eq)

data AluSrc2 = Src2Reg | Src2Imm
  deriving (Generic, NFDataX, Show, Eq)

data WbSrc = WbAlu | WbPc4 | WbMem
  deriving (Generic, NFDataX, Show, Eq)

data MemOp
  = MemNone
  | MemLb | MemLh | MemLw | MemLbu | MemLhu
  | MemSb | MemSh | MemSw
  deriving (Generic, NFDataX, Show, Eq)

data BranchOp
  = BrNone
  | BrEq | BrNe | BrLt | BrGe | BrLtu | BrGeu
  | BrJal
  deriving (Generic, NFDataX, Show, Eq)

data MicroOp = MicroOp
  { uAluOp    :: AluOp
  , uAluSrc2  :: AluSrc2
  , uWbSrc    :: WbSrc
  , uWbEn     :: Bool
  , uRd       :: RegIdx
  , uRs1      :: RegIdx
  , uRs2      :: RegIdx
  , uImm      :: Signed 32
  , uMemOp    :: MemOp
  , uBranchOp :: BranchOp
  , uAuipc    :: Bool
  } deriving (Generic, NFDataX, Show)

-- ===========================================================================
-- BHT (2-bit saturating, 64 entries)
-- ===========================================================================

data BhtCounter = SNT | WNT | WT | ST
  deriving (Generic, NFDataX, Show, Eq)

bhtStrengthenTaken    :: BhtCounter -> BhtCounter
bhtStrengthenTaken    SNT = WNT; bhtStrengthenTaken    WNT = WT
bhtStrengthenTaken    WT  = ST;  bhtStrengthenTaken    ST  = ST

bhtStrengthenNotTaken :: BhtCounter -> BhtCounter
bhtStrengthenNotTaken ST  = WT;  bhtStrengthenNotTaken WT  = WNT
bhtStrengthenNotTaken WNT = SNT; bhtStrengthenNotTaken SNT = SNT

bhtPredTaken :: BhtCounter -> Bool
bhtPredTaken WT = True; bhtPredTaken ST = True; bhtPredTaken _ = False

type BHT = Vec 64 BhtCounter

initBHT :: BHT
initBHT = repeat WNT

bhtIdx :: PC -> Unsigned 6
bhtIdx pc = truncateB (pc `shiftR` 2)

-- ===========================================================================
-- Instruction decode
-- ===========================================================================

opOP :: BitVector 7; opOP = 0b0110011
opOPIMM :: BitVector 7; opOPIMM = 0b0010011
opLOAD :: BitVector 7; opLOAD = 0b0000011
opSTORE :: BitVector 7; opSTORE = 0b0100011
opBRANCH :: BitVector 7; opBRANCH = 0b1100011
opJAL :: BitVector 7; opJAL = 0b1101111
opJALR :: BitVector 7; opJALR = 0b1100111
opLUI :: BitVector 7; opLUI = 0b0110111
opAUIPC :: BitVector 7; opAUIPC = 0b0010111

nopMicroOp :: MicroOp
nopMicroOp = MicroOp
  { uAluOp = AluAdd, uAluSrc2 = Src2Imm, uWbSrc = WbAlu, uWbEn = False
  , uRd = 0, uRs1 = 0, uRs2 = 0, uImm = 0
  , uMemOp = MemNone, uBranchOp = BrNone, uAuipc = False }

decode :: Word32 -> MicroOp
decode instr =
  let op  = opcode   instr
      f3  = funct3Of instr
      f7  = funct7Of instr
      rd  = rdOf     instr
      rs1 = rs1Of    instr
      rs2 = rs2Of    instr
   in case op of
        _ | op == opOP ->
              let aluOp = case (f3, f7) of
                    (0b000, 0b0000000) -> AluAdd;  (0b000, 0b0100000) -> AluSub
                    (0b001, _)         -> AluSll;  (0b010, _)         -> AluSlt
                    (0b011, _)         -> AluSltu; (0b100, _)         -> AluXor
                    (0b101, 0b0000000) -> AluSrl;  (0b101, 0b0100000) -> AluSra
                    (0b110, _)         -> AluOr;   (0b111, _)         -> AluAnd
                    _                  -> AluAdd
               in nopMicroOp { uAluOp = aluOp, uAluSrc2 = Src2Reg
                              , uWbSrc = WbAlu, uWbEn = True
                              , uRd = rd, uRs1 = rs1, uRs2 = rs2 }

          | op == opOPIMM ->
              let shamt = unpack (slice d24 d20 instr) :: Unsigned 5
                  aluOp = case f3 of
                    0b000 -> AluAdd;  0b001 -> AluSll;  0b010 -> AluSlt
                    0b011 -> AluSltu; 0b100 -> AluXor
                    0b101 -> if f7 == 0b0100000 then AluSra else AluSrl
                    0b110 -> AluOr;   0b111 -> AluAnd;  _     -> AluAdd
                  imm' = case f3 of
                    0b001 -> fromIntegral shamt
                    0b101 -> fromIntegral shamt
                    _     -> immI instr
               in nopMicroOp { uAluOp = aluOp, uAluSrc2 = Src2Imm
                              , uWbSrc = WbAlu, uWbEn = True
                              , uRd = rd, uRs1 = rs1, uImm = imm' }

          | op == opLUI ->
              nopMicroOp { uAluOp = AluLui, uAluSrc2 = Src2Imm
                         , uWbSrc = WbAlu, uWbEn = True, uRd = rd, uImm = immU instr }

          | op == opAUIPC ->
              nopMicroOp { uAluOp = AluAdd, uAluSrc2 = Src2Imm
                         , uWbSrc = WbAlu, uWbEn = True
                         , uRd = rd, uImm = immU instr, uAuipc = True }

          | op == opJAL ->
              nopMicroOp { uAluOp = AluLui, uAluSrc2 = Src2Imm
                         , uWbSrc = WbPc4, uWbEn = True
                         , uRd = rd, uImm = immJ instr, uBranchOp = BrJal }

          | op == opJALR ->
              nopMicroOp { uAluOp = AluAdd, uAluSrc2 = Src2Imm
                         , uWbSrc = WbPc4, uWbEn = True
                         , uRd = rd, uRs1 = rs1, uImm = immI instr, uBranchOp = BrJal }

          | op == opLOAD ->
              let memOp = case f3 of
                    0b000 -> MemLb; 0b001 -> MemLh; 0b010 -> MemLw
                    0b100 -> MemLbu; 0b101 -> MemLhu; _ -> MemLw
               in nopMicroOp { uAluOp = AluAdd, uAluSrc2 = Src2Imm
                              , uWbSrc = WbMem, uWbEn = True
                              , uRd = rd, uRs1 = rs1, uImm = immI instr, uMemOp = memOp }

          | op == opSTORE ->
              let memOp = case f3 of
                    0b000 -> MemSb; 0b001 -> MemSh; _ -> MemSw
               in nopMicroOp { uAluOp = AluAdd, uAluSrc2 = Src2Imm
                              , uWbSrc = WbAlu, uWbEn = False
                              , uRs1 = rs1, uRs2 = rs2
                              , uImm = immS instr, uMemOp = memOp }

          | op == opBRANCH ->
              let brOp = case f3 of
                    0b000 -> BrEq;  0b001 -> BrNe;  0b100 -> BrLt
                    0b101 -> BrGe;  0b110 -> BrLtu; 0b111 -> BrGeu; _ -> BrNone
               in nopMicroOp { uAluOp = AluAdd, uAluSrc2 = Src2Imm
                              , uWbSrc = WbAlu, uWbEn = False
                              , uRs1 = rs1, uRs2 = rs2
                              , uImm = immB instr, uBranchOp = brOp }

          | otherwise -> nopMicroOp

-- ===========================================================================
-- ALU
-- ===========================================================================

alu :: AluOp -> Word32 -> Word32 -> Word32
alu op a b =
  let sa    = unpack a :: Signed 32
      sb    = unpack b :: Signed 32
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
        AluLui  -> b

evalBranch :: BranchOp -> Word32 -> Word32 -> Bool
evalBranch brOp a b =
  let sa = unpack a :: Signed 32; sb = unpack b :: Signed 32
      ua = unpack a :: Unsigned 32; ub = unpack b :: Unsigned 32
   in case brOp of
        BrNone -> False; BrEq  -> a == b; BrNe  -> a /= b
        BrLt   -> sa < sb; BrGe  -> sa >= sb
        BrLtu  -> ua < ub; BrGeu -> ua >= ub; BrJal -> True

-- ===========================================================================
-- Data memory helpers
-- ===========================================================================

memLoad :: MemOp -> Addr -> Word32 -> Word32
memLoad memOp byteAddr word =
  let byteOff = resize (byteAddr .&. 3) :: Unsigned 2
      selByte = case byteOff of
        0 -> slice d7  d0  word; 1 -> slice d15 d8  word
        2 -> slice d23 d16 word; _ -> slice d31 d24 word
      selHalf = if byteOff < 2 then slice d15 d0 word else slice d31 d16 word
   in case memOp of
        MemLb  -> pack (signExtend (unpack selByte :: Signed 8)  :: Signed 32)
        MemLbu -> zeroExtend selByte
        MemLh  -> pack (signExtend (unpack selHalf :: Signed 16) :: Signed 32)
        MemLhu -> zeroExtend selHalf
        MemLw  -> word
        _      -> 0

memStore :: MemOp -> Addr -> Word32 -> Word32 -> Maybe (Unsigned 10, Word32)
memStore memOp byteAddr storeVal oldWord =
  let wordIdx = truncateB (byteAddr `shiftR` 2) :: Unsigned 10
      byteOff = resize (byteAddr .&. 3) :: Unsigned 2
      b8  = zeroExtend (slice d7  d0 storeVal) :: BitVector 32
      h16 = zeroExtend (slice d15 d0 storeVal) :: BitVector 32
      newWord = case memOp of
        MemSb -> case byteOff of
          0 -> (oldWord .&. 0xFFFFFF00) .|. b8
          1 -> (oldWord .&. 0xFFFF00FF) .|. (b8  `shiftL` 8)
          2 -> (oldWord .&. 0xFF00FFFF) .|. (b8  `shiftL` 16)
          _ -> (oldWord .&. 0x00FFFFFF) .|. (b8  `shiftL` 24)
        MemSh -> if byteOff < 2
                   then (oldWord .&. 0xFFFF0000) .|. h16
                   else (oldWord .&. 0x0000FFFF) .|. (h16 `shiftL` 16)
        MemSw -> storeVal
        _     -> oldWord
   in case memOp of
        MemNone -> Nothing; MemLb -> Nothing; MemLh  -> Nothing
        MemLw   -> Nothing; MemLbu -> Nothing; MemLhu -> Nothing
        _       -> Just (wordIdx, newWord)

memCtrlLoad :: MemCtrl -> MemOp -> Addr -> Word32 -> Word32
memCtrlLoad _mc memOp byteAddr ramWord
  | byteAddr <= ramTop       = memLoad memOp byteAddr ramWord
  | byteAddr == uartTxAddr   = 0
  | byteAddr == uartStatAddr = 1
  | otherwise                = 0

memCtrlStore :: MemCtrl -> MemOp -> Addr -> Word32 -> Word32
             -> (MemCtrl, Maybe (Unsigned 10, Word32))
memCtrlStore mc memOp byteAddr storeVal oldWord
  | byteAddr <= ramTop     = (mc, memStore memOp byteAddr storeVal oldWord)
  | byteAddr == uartTxAddr = case memOp of
      MemSb -> (mc { mcUart = UartState (truncateB (pack storeVal)) True }, Nothing)
      MemSh -> (mc { mcUart = UartState (truncateB (pack storeVal)) True }, Nothing)
      MemSw -> (mc { mcUart = UartState (truncateB (pack storeVal)) True }, Nothing)
      _     -> (mc, Nothing)
  | otherwise = (mc, Nothing)

-- ===========================================================================
-- Pipeline registers
-- ===========================================================================

-- IF/ID -----------------------------------------------------------------------

data IfIdReg = IfIdReg
  { ifidValid     :: Bool
  , ifidPC        :: PC
  , ifidInstr     :: Word32
  , ifidPredPC    :: PC
  , ifidPredTaken :: Bool
  } deriving (Generic, NFDataX, Show)

emptyIfId :: IfIdReg
emptyIfId = IfIdReg False 0 0 0 False

-- ID/EX -----------------------------------------------------------------------
-- CP1: rs1Val/rs2Val have the MEM/WB bypass pre-applied.

data IdExReg = IdExReg
  { idexValid     :: Bool
  , idexPC        :: PC
  , idexUop       :: MicroOp
  , idexRs1Val    :: Word32
  , idexRs2Val    :: Word32
  , idexPredPC    :: PC
  , idexPredTaken :: Bool
  } deriving (Generic, NFDataX, Show)

emptyIdEx :: IdExReg
emptyIdEx = IdExReg False 0 nopMicroOp 0 0 0 False

-- EX/MEM ----------------------------------------------------------------------

data ExMemReg = ExMemReg
  { exmemValid  :: Bool
  , exmemPC     :: PC
  , exmemRd     :: RegIdx
  , exmemWbEn   :: Bool
  , exmemWbSrc  :: WbSrc
  , exmemAluOut :: Word32
  , exmemRs2Val :: Word32
  , exmemMemOp  :: MemOp
  } deriving (Generic, NFDataX, Show)

emptyExMem :: ExMemReg
emptyExMem = ExMemReg False 0 0 False WbAlu 0 0 MemNone

-- MEM/WB ----------------------------------------------------------------------

data MemWbReg = MemWbReg
  { memwbValid  :: Bool
  , memwbRd     :: RegIdx
  , memwbWbEn   :: Bool
  , memwbWbSrc  :: WbSrc
  , memwbAluOut :: Word32
  , memwbMemVal :: Word32
  , memwbPc4    :: Word32
  } deriving (Generic, NFDataX, Show)

emptyMemWb :: MemWbReg
emptyMemWb = MemWbReg False 0 False WbAlu 0 0 0

-- ===========================================================================
-- CPU state
-- ===========================================================================

data CpuStateRV = CpuStateRV
  { rvPC        :: PC
  , rvIfId      :: IfIdReg
  , rvIdEx      :: IdExReg
  , rvExMem     :: ExMemReg
  , rvMemWb     :: MemWbReg
  , rvBHT       :: BHT    -- ^ CP2: committed BHT, one cycle behind the update
  , rvStallNext :: Bool   -- ^ CP3: registered load-use stall signal
  , rvHalt      :: Bool
  } deriving (Generic, NFDataX, Show)

initCpuStateRV :: CpuStateRV
initCpuStateRV = CpuStateRV
  { rvPC        = 0
  , rvIfId      = emptyIfId
  , rvIdEx      = emptyIdEx
  , rvExMem     = emptyExMem
  , rvMemWb     = emptyMemWb
  , rvBHT       = initBHT
  , rvStallNext = False
  , rvHalt      = False
  }

-- ===========================================================================
-- CPU step (Mealy body)
-- ===========================================================================
--
-- Inputs:
--   instrWord    – instruction from instruction blockRam at rvPC
--   dataBramWord – data blockRam word for EX/MEM address (previous cycle)
--   regRdA       – CP4: register file port A output (rs1 of last cycle's ID instr)
--   regRdB       – CP4: register file port B output (rs2 of last cycle's ID instr)
--   memCtrl      – peripheral state
--   en           – clock enable
--
-- Outputs:
--   (newState, newMemCtrl, dataWrCmd, regWrCmd, nextFetchPC, wbRd, wbVal, wbValid)
--   regWrCmd   – CP4: optional (RegIdx, Word32) write to register blockRam
--   nextFetchPC – address for the next instrWord fetch

stepCpuRV
  :: CpuStateRV
  -> (Word32, Word32, Word32, Word32, MemCtrl, Bool)
  -> ( CpuStateRV
     , MemCtrl
     , Maybe (Unsigned 10, Word32)
     , Maybe (RegIdx, Word32)
     , PC
     , RegIdx, Word32, Bool
     )
stepCpuRV s@CpuStateRV{..} (instrWord, dataBramWord, regRdA, regRdB, memCtrl, en)
  | rvHalt = (s, memCtrl, Nothing, Nothing, rvPC, 0, 0, False)
  | not en = (s, memCtrl, Nothing, Nothing, rvPC, 0, 0, False)
  | otherwise =

  -- ── WB ──────────────────────────────────────────────────────────────────
  let mwb = rvMemWb

      wbResult = case memwbWbSrc mwb of
        WbAlu -> memwbAluOut mwb
        WbPc4 -> memwbPc4    mwb
        WbMem -> memwbMemVal mwb

      wbEn = memwbValid mwb && memwbWbEn mwb && memwbRd mwb /= 0

      -- CP4: drive external register blockRam write port
      regWrCmd :: Maybe (RegIdx, Word32)
      regWrCmd = if wbEn then Just (memwbRd mwb, wbResult) else Nothing

  -- ── MEM ─────────────────────────────────────────────────────────────────
      exmem = rvExMem

      memAddr  = unpack (exmemAluOut exmem) :: Addr
      storeVal = exmemRs2Val exmem
      mop      = exmemMemOp  exmem

      (memCtrl', dataWrCmd, memReadVal) =
        if exmemValid exmem
          then case mop of
            MemLb  -> (memCtrl, Nothing, memCtrlLoad memCtrl mop memAddr dataBramWord)
            MemLbu -> (memCtrl, Nothing, memCtrlLoad memCtrl mop memAddr dataBramWord)
            MemLh  -> (memCtrl, Nothing, memCtrlLoad memCtrl mop memAddr dataBramWord)
            MemLhu -> (memCtrl, Nothing, memCtrlLoad memCtrl mop memAddr dataBramWord)
            MemLw  -> (memCtrl, Nothing, memCtrlLoad memCtrl mop memAddr dataBramWord)
            MemSb  -> let (mc', wc) = memCtrlStore memCtrl mop memAddr storeVal dataBramWord
                       in (mc', wc, 0)
            MemSh  -> let (mc', wc) = memCtrlStore memCtrl mop memAddr storeVal dataBramWord
                       in (mc', wc, 0)
            MemSw  -> let (mc', wc) = memCtrlStore memCtrl mop memAddr storeVal dataBramWord
                       in (mc', wc, 0)
            MemNone -> (memCtrl, Nothing, exmemAluOut exmem)
          else (memCtrl, Nothing, 0)

      memwb' = if exmemValid exmem
                 then MemWbReg
                        { memwbValid  = True
                        , memwbRd     = exmemRd    exmem
                        , memwbWbEn   = exmemWbEn  exmem
                        , memwbWbSrc  = exmemWbSrc exmem
                        , memwbAluOut = exmemAluOut exmem
                        , memwbMemVal = memReadVal
                        , memwbPc4    = pack (exmemPC exmem + 4)
                        }
                 else emptyMemWb

  -- ── EX ──────────────────────────────────────────────────────────────────
      idex = rvIdEx
      uop  = idexUop idex

      -- CP1: EX only checks EX/MEM. MEM/WB was pre-applied in ID.
      fwdRs1 =
        let fromExMem = exmemWbEn exmem && exmemRd exmem /= 0
                          && exmemRd exmem == uRs1 uop
                          && exmemWbSrc exmem /= WbMem
        in if fromExMem then exmemAluOut exmem else idexRs1Val idex

      fwdRs2 =
        let fromExMem = exmemWbEn exmem && exmemRd exmem /= 0
                          && exmemRd exmem == uRs2 uop
                          && exmemWbSrc exmem /= WbMem
        in if fromExMem then exmemAluOut exmem else idexRs2Val idex

      aluA = if uAuipc uop then pack (idexPC idex) else fwdRs1
      aluB = case uAluSrc2 uop of
               Src2Reg -> fwdRs2
               Src2Imm -> pack (uImm uop)

      aluResult = alu (uAluOp uop) aluA aluB

      brActualTaken = evalBranch (uBranchOp uop) fwdRs1 fwdRs2

      brTarget = case uBranchOp uop of
        BrJal ->
          if opcode (ifidInstr rvIfId) == opJALR
            then unpack (aluResult .&. complement 1) :: PC
            else unpack (pack (fromIntegral (idexPC idex) + uImm uop :: Signed 32)) :: PC
        _ -> unpack (pack (fromIntegral (idexPC idex) + uImm uop :: Signed 32)) :: PC

      actualNextPC = if brActualTaken then brTarget else idexPC idex + 4

      mispredicted =
        idexValid idex
          && uBranchOp uop /= BrNone
          && idexPredPC idex /= actualNextPC

      -- CP2: compute bht' for storage; IF reads rvBHT (last committed value)
      bhtUpdateIdx = bhtIdx (idexPC idex)
      bhtOld       = rvBHT !! bhtUpdateIdx
      bhtNew       = if brActualTaken then bhtStrengthenTaken    bhtOld
                                      else bhtStrengthenNotTaken bhtOld
      bht' = if idexValid idex && uBranchOp uop /= BrNone
               then replace bhtUpdateIdx bhtNew rvBHT
               else rvBHT

      exmem' = if idexValid idex
                 then ExMemReg
                        { exmemValid  = True
                        , exmemPC     = idexPC  idex
                        , exmemRd     = uRd     uop
                        , exmemWbEn   = uWbEn   uop
                        , exmemWbSrc  = uWbSrc  uop
                        , exmemAluOut = aluResult
                        , exmemRs2Val = fwdRs2
                        , exmemMemOp  = uMemOp  uop
                        }
                 else emptyExMem

  -- ── CP3: Load-use hazard (registered) ───────────────────────────────────
  --
  --   loadUseHazard for THIS cycle = rvStallNext computed LAST cycle.
  --   stallNext' for NEXT cycle is computed from the current IF/ID and ID/EX.

      isLoad op = case op of
        MemLb -> True; MemLbu -> True; MemLh  -> True
        MemLhu -> True; MemLw  -> True; _      -> False

      loadUseHazard = rvStallNext  -- registered; off the critical path

      ifidDecRs1 = rs1Of (ifidInstr rvIfId)
      ifidDecRs2 = rs2Of (ifidInstr rvIfId)

      stallNext' =
        idexValid idex
          && isLoad (uMemOp uop)
          && uWbEn uop
          && uRd uop /= 0
          && (uRd uop == ifidDecRs1 || uRd uop == ifidDecRs2)

  -- ── ID ──────────────────────────────────────────────────────────────────
      ifid = rvIfId
      uop' = decode (ifidInstr ifid)

      -- CP4: use blockRam read ports for register values.
      -- CP1: pre-apply MEM/WB bypass so EX needs only one forwarding mux.
      rs1vRaw = if uRs1 uop' == 0 then 0 else regRdA
      rs2vRaw = if uRs2 uop' == 0 then 0 else regRdB

      rs1v = if wbEn && memwbRd mwb /= 0 && memwbRd mwb == uRs1 uop'
               then wbResult else rs1vRaw
      rs2v = if wbEn && memwbRd mwb /= 0 && memwbRd mwb == uRs2 uop'
               then wbResult else rs2vRaw

      idex' =
        if mispredicted || not (ifidValid ifid) || loadUseHazard
          then emptyIdEx
          else IdExReg
                 { idexValid     = True
                 , idexPC        = ifidPC       ifid
                 , idexUop       = uop'
                 , idexRs1Val    = rs1v
                 , idexRs2Val    = rs2v
                 , idexPredPC    = ifidPredPC   ifid
                 , idexPredTaken = ifidPredTaken ifid
                 }

  -- ── IF ──────────────────────────────────────────────────────────────────

      -- CP2: read rvBHT (registered, not bht') to break the EX→BHT→IF loop
      bhtPred = bhtPredTaken (rvBHT !! bhtIdx rvPC)
      fetchOp = opcode instrWord

      (ifPredTaken, ifPredTarget) =
        if fetchOp == opJAL
          then ( True
               , unpack (pack (fromIntegral rvPC + immJ instrWord :: Signed 32)) :: PC )
          else if fetchOp == opBRANCH && bhtPred
                 then ( True
                      , unpack (pack (fromIntegral rvPC + immB instrWord :: Signed 32)) :: PC )
                 else (False, rvPC + 4)

      nextPC
        | loadUseHazard = rvPC
        | mispredicted  = actualNextPC
        | otherwise     = ifPredTarget

      ifid' =
        if loadUseHazard
          then ifid
          else if mispredicted
                 then emptyIfId
                 else IfIdReg
                        { ifidValid     = True
                        , ifidPC        = rvPC
                        , ifidInstr     = instrWord
                        , ifidPredPC    = ifPredTarget
                        , ifidPredTaken = ifPredTaken
                        }

      s' = s { rvPC        = nextPC
             , rvIfId      = ifid'
             , rvIdEx      = idex'
             , rvExMem     = exmem'
             , rvMemWb     = memwb'
             , rvBHT       = bht'        -- CP2: committed next cycle
             , rvStallNext = stallNext'  -- CP3: consumed next cycle
             , rvHalt      = False
             }

   in (s', memCtrl', dataWrCmd, regWrCmd, nextPC, memwbRd mwb, wbResult, wbEn)

-- ===========================================================================
-- Simulation helper
-- ===========================================================================

data SimState = SimState
  { simCpu  :: CpuStateRV
  , simData :: MemCtrl
  , simRam  :: Vec 1024 Word32
  , simRegs :: Vec 32  Word32   -- CP4: register file modelled as synchronous RAM
  } deriving (Generic, NFDataX, Show)

initSimState :: SimState
initSimState = SimState initCpuStateRV initMemCtrl (repeat 0) (repeat 0)

simDataWord :: SimState -> Word32
simDataWord st =
  let addr = truncateB (unpack (exmemAluOut (rvExMem (simCpu st))) `shiftR` 2) :: Unsigned 10
   in simRam st !! addr

-- | CP4 sim: read register file for the rs1/rs2 of the IF/ID instruction.
--   Models the one-cycle blockRam latency: we read from last cycle's simRegs.
simRegRead :: SimState -> (Word32, Word32)
simRegRead st =
  let instr = ifidInstr (rvIfId (simCpu st))
   in (simRegs st !! rs1Of instr, simRegs st !! rs2Of instr)

simulateRV :: InstrMem -> Int -> [(PC, RegIdx, Word32, Bool)]
simulateRV iMem n = P.take n $ go initSimState
  where
    go st =
      let cpu   = simCpu  st
          mc    = simData st
          pcW   = truncateB (rvPC cpu `shiftR` 2) :: Unsigned 10
          instr = iMem !! pcW
          dword = simDataWord st
          (rA, rB) = simRegRead st
          (cpu', mc', dataWc, regWc, pc, rd, val, en)
            = stepCpuRV cpu (instr, dword, rA, rB, mc, True)
          ram'  = case dataWc of
            Just (i, w) -> replace i w (simRam  st); Nothing -> simRam  st
          regs' = case regWc of
            Just (i, w) | i /= 0 -> replace i w (simRegs st); _ -> simRegs st
          st' = st { simCpu  = cpu', simData = mc'
                   , simRam  = ram', simRegs = regs' }
       in (pc, rd, val, en) : go st'

-- ===========================================================================
-- Instruction assembly helpers
-- ===========================================================================

mkR :: BitVector 7 -> RegIdx -> RegIdx -> RegIdx -> BitVector 3 -> BitVector 7 -> Word32
mkR op rd rs1 rs2 f3 f7 = f7 ++# pack rs2 ++# pack rs1 ++# f3 ++# pack rd ++# op

mkI :: BitVector 7 -> RegIdx -> BitVector 3 -> RegIdx -> Signed 12 -> Word32
mkI op rd f3 rs1 imm = pack imm ++# pack rs1 ++# f3 ++# pack rd ++# op

mkS :: BitVector 7 -> RegIdx -> RegIdx -> BitVector 3 -> Signed 12 -> Word32
mkS op rs1 rs2 f3 imm =
  let immBV = pack imm
   in slice d11 d5 immBV ++# pack rs2 ++# pack rs1 ++# f3 ++# slice d4 d0 immBV ++# op

mkB :: BitVector 7 -> RegIdx -> RegIdx -> BitVector 3 -> Signed 13 -> Word32
mkB op rs1 rs2 f3 imm =
  let immBV = pack imm
      b12   = slice d12 d12 immBV; b11   = slice d11 d11 immBV
      b10_5 = slice d10 d5  immBV; b4_1  = slice d4  d1  immBV
   in (b12 ++# b10_5) ++# pack rs2 ++# pack rs1 ++# f3 ++# (b4_1 ++# b11) ++# op

mkU :: BitVector 7 -> RegIdx -> BitVector 20 -> Word32
mkU op rd imm20 = imm20 ++# pack rd ++# op

mkJ :: BitVector 7 -> RegIdx -> Signed 21 -> Word32
mkJ op rd imm =
  let immBV  = pack imm
      b20    = slice d20 d20 immBV; b19_12 = slice d19 d12 immBV
      b11    = slice d11 d11 immBV; b10_1  = slice d10 d1  immBV
   in (b20 ++# b10_1 ++# b11 ++# b19_12) ++# pack rd ++# op

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

iADDI  rd rs1 imm = mkI opOPIMM rd 0b000 rs1 imm
iSLTI  rd rs1 imm = mkI opOPIMM rd 0b010 rs1 imm
iSLTIU rd rs1 imm = mkI opOPIMM rd 0b011 rs1 imm
iXORI  rd rs1 imm = mkI opOPIMM rd 0b100 rs1 imm
iORI   rd rs1 imm = mkI opOPIMM rd 0b110 rs1 imm
iANDI  rd rs1 imm = mkI opOPIMM rd 0b111 rs1 imm

iSLLI rd rs1 sh = mkI opOPIMM rd 0b001 rs1 (fromIntegral (sh :: Unsigned 5) :: Signed 12)
iSRLI rd rs1 sh = mkI opOPIMM rd 0b101 rs1 (fromIntegral (sh :: Unsigned 5) :: Signed 12)
iSRAI rd rs1 sh = mkI opOPIMM rd 0b101 rs1 (0b0100000_00000 .|. fromIntegral (sh :: Unsigned 5) :: Signed 12)

iLB  rd rs1 imm = mkI opLOAD rd 0b000 rs1 imm
iLH  rd rs1 imm = mkI opLOAD rd 0b001 rs1 imm
iLW  rd rs1 imm = mkI opLOAD rd 0b010 rs1 imm
iLBU rd rs1 imm = mkI opLOAD rd 0b100 rs1 imm
iLHU rd rs1 imm = mkI opLOAD rd 0b101 rs1 imm

iSB rs1 rs2 imm = mkS opSTORE rs1 rs2 0b000 imm
iSH rs1 rs2 imm = mkS opSTORE rs1 rs2 0b001 imm
iSW rs1 rs2 imm = mkS opSTORE rs1 rs2 0b010 imm

iBEQ  rs1 rs2 imm = mkB opBRANCH rs1 rs2 0b000 imm
iBNE  rs1 rs2 imm = mkB opBRANCH rs1 rs2 0b001 imm
iBLT  rs1 rs2 imm = mkB opBRANCH rs1 rs2 0b100 imm
iBGE  rs1 rs2 imm = mkB opBRANCH rs1 rs2 0b101 imm
iBLTU rs1 rs2 imm = mkB opBRANCH rs1 rs2 0b110 imm
iBGEU rs1 rs2 imm = mkB opBRANCH rs1 rs2 0b111 imm

iLUI   rd imm20   = mkU opLUI   rd imm20
iAUIPC rd imm20   = mkU opAUIPC rd imm20
iJAL   rd imm     = mkJ opJAL   rd imm
iJALR  rd rs1 imm = mkI opJALR  rd 0b000 rs1 imm

iNOP :: Word32
iNOP = iADDI 0 0 0

-- ===========================================================================
-- Test program
-- ===========================================================================

testProgRV :: InstrMem
testProgRV =
  let nop  = iNOP
      base = repeat nop :: InstrMem
   in replace (0 :: Unsigned 10) (iADDI 1 0 10)
    $ replace (1 :: Unsigned 10) (iADDI 2 0 0)
    $ replace (2 :: Unsigned 10) (iBEQ  1 0 16)
    $ replace (3 :: Unsigned 10) (iADD  2 2 1)
    $ replace (4 :: Unsigned 10) (iADDI 1 1 (-1))
    $ replace (5 :: Unsigned 10) (iBEQ  0 0 (-12))
    $ replace (6 :: Unsigned 10) nop
    $ base
