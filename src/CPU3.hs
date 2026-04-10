{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | A 32-bit register-file CPU with a 3-stage pipeline, large BRAM instruction
--   memory, and a clean branch-resolution interface ready for a branch predictor.
--
-- ── Core changes from CPU (8-bit) ────────────────────────────────────────
--   * 32-bit datapath: all registers, immediates, ALU results are 32 bits.
--   * 4 general-purpose registers: R0..R3 (no implicit accumulator).
--   * Instruction memory: 1 KiB (1024 × 32-bit words) mapped to FPGA block RAM.
--     Host loads the whole image over UART before running.
--   * 3-stage pipeline: IF → ID → EX, with a dedicated branch-resolution
--     handshake that a predictor can hook into later.
--   * Branch predictor interface (predict-not-taken stub):
--       - Predict signal emitted at end of IF (when insn enters ID).
--       - Actual outcome signalled at end of EX.
--       - Misprediction squashes the IF+ID stages (2-cycle penalty).
--
-- ── Instruction encoding (32 bits) ───────────────────────────────────────
--   [31:28]  opcode    (4 bits)
--   [27:26]  rd        destination register (2 bits → R0..R3)
--   [25:24]  rs        source register      (2 bits → R0..R3)
--   [23:0]   imm24     24-bit immediate / branch offset / unused
--
-- ── Opcodes ──────────────────────────────────────────────────────────────
--   0x0  NOP
--   0x1  LOAD rd, imm24        rd  = imm24          (zero-extended)
--   0x2  ADD  rd, rs, imm24    rd  = rs + imm24
--   0x3  SUB  rd, rs, imm24    rd  = rs - imm24
--   0x4  AND  rd, rs, imm24    rd  = rs .&. imm24
--   0x5  OR   rd, rs, imm24    rd  = rs .|. imm24
--   0x6  MOV  rd, rs            rd  = rs
--   0x7  OUT  rs                emit rs over UART (low 8 bits)
--   0x8  HALT
--   0x9  JMP  imm24             PC  = imm24          (absolute)
--   0xA  JZ   rs, imm24         PC  = imm24 when rs == 0
--   0xB  JNZ  rs, imm24         PC  = imm24 when rs /= 0
--
-- ── Serial protocol (115200 8N1) ──────────────────────────────────────────
--   PROGRAM mode: host sends 'P' then 4096 bytes (1024 × 32-bit words, MSB first).
--                 FPGA replies 'K' when loaded.
--   RUN    mode: host sends 'R'. FPGA resets CPU, executes.
--                Each OUT emits one byte. After HALT sends 'D'.
--   RESET  mode: host sends 'X'. FPGA soft-resets and replies 'K'.
--
-- ── Pipeline stages ──────────────────────────────────────────────────────
--   Stage 1  IF   — issue PC to BRAM (registered-output block RAM = 1 cycle
--                   read latency); PC advances speculatively (predict-not-taken).
--   Stage 2  ID   — BRAM result available; decode fields, read registers.
--   Stage 3  EX   — ALU, branch resolution, register write-back, output.
--
--   Branch predictor interface:
--     At end of IF  : predictTaken (Bool) + predictTarget (Unsigned 10).
--                     Currently always False / 0 (predict-not-taken).
--     At end of EX  : branchActual (Bool) + branchTarget (Unsigned 10).
--     If predictTaken /= branchActual: squash IF and ID, redirect PC.
--     Misprediction penalty: 2 cycles.
--
module CPU3 where

import Clash.Annotations.TopEntity ()
import Clash.Prelude hiding (take)
import Data.Proxy (Proxy (..))
import qualified Prelude as P

-- ===========================================================================
-- Synthesis annotation
-- ===========================================================================

{-# ANN
  topEntity3
  ( Synthesize
      { t_name = "top3",
        t_inputs =
          [ PortName "clk",
            PortName "uart_rx_pin"
          ],
        t_output =
          PortProduct
            ""
            [ PortName "uart_tx_pin",
              PortName "led"
            ]
      }
  )
  #-}

-- ===========================================================================
-- Shared constants (identical UART params to CPU.hs)
-- ===========================================================================

type ClkFreq3  = 27_000_000
type BaudRate3 = 115_200
type ClksPerBit3 = Div ClkFreq3 BaudRate3   -- 234
type HalfBit3    = Div ClksPerBit3 2         -- 117

-- ===========================================================================
-- Types
-- ===========================================================================

-- | 32-bit instruction word.
type Instr32 = BitVector 32

-- | Instruction memory: 1024 words of 32-bit instructions.
--   Clash will infer block RAM when indexed with a registered address.
type InstrMem = Vec 1024 Instr32

-- | Register file: 4 × 32-bit registers.
type RegFile = Vec 4 (Unsigned 32)

-- | PC is 10 bits wide (addresses 0..1023).
type PC = Unsigned 10

-- | Register index: 0..3.
type RegIdx = Unsigned 2

-- ===========================================================================
-- UART RX / TX / FIFO — identical logic, widened baud constants
-- ===========================================================================

data RxFSM3 = RxIdle3 | RxStart3 | RxData3 | RxStop3
  deriving (Generic, NFDataX, Show, Eq)

data UartRxState3 = UartRxState3
  { rxFSM3    :: RxFSM3
  , rxCnt3    :: Unsigned 9
  , rxBitIdx3 :: Unsigned 3
  , rxShift3  :: BitVector 8
  , rxSync13  :: Bit
  , rxSync23  :: Bit
  , rxData3   :: BitVector 8
  , rxValid3  :: Bool
  } deriving (Generic, NFDataX, Show)

initRxState3 :: UartRxState3
initRxState3 = UartRxState3 RxIdle3 0 0 0 1 1 0 False

data TxFSM3 = TxIdle3 | TxStart3 | TxData3 | TxStop3
  deriving (Generic, NFDataX, Show, Eq)

data UartTxState3 = UartTxState3
  { txFSM3    :: TxFSM3
  , txCnt3    :: Unsigned 9
  , txBitIdx3 :: Unsigned 3
  , txShift3  :: BitVector 8
  , txPinLvl3 :: Bit
  } deriving (Generic, NFDataX, Show)

initTxState3 :: UartTxState3
initTxState3 = UartTxState3 TxIdle3 0 0 0 1

data FifoState3 = FifoState3
  { fifoBuf3 :: Vec 16 (BitVector 8)
  , fifoWr3  :: Unsigned 4
  , fifoRd3  :: Unsigned 4
  } deriving (Generic, NFDataX, Show)

initFifoState3 :: FifoState3
initFifoState3 = FifoState3 (repeat 0) 0 0

-- ===========================================================================
-- UART RX
-- ===========================================================================

uartRxT3 :: UartRxState3 -> Bit -> (UartRxState3, (BitVector 8, Bool))
uartRxT3 s@UartRxState3{..} rxPin =
  let sync1    = rxPin
      sync2    = rxSync13
      sampled  = rxSync23
      clksPB   = fromIntegral (natVal (Proxy :: Proxy ClksPerBit3)) :: Unsigned 9
      half     = fromIntegral (natVal (Proxy :: Proxy HalfBit3))    :: Unsigned 9

      (fsm', cnt', bitIdx', shift', valid') = case rxFSM3 of
        RxIdle3  -> if sampled == 0
                      then (RxStart3, 0, 0, rxShift3, False)
                      else (RxIdle3,  0, 0, rxShift3, False)
        RxStart3 -> if rxCnt3 == half - 1
                      then if sampled == 0
                             then (RxData3, 0, 0, rxShift3, False)
                             else (RxIdle3, 0, 0, rxShift3, False)
                      else (RxStart3, rxCnt3 + 1, rxBitIdx3, rxShift3, False)
        RxData3  -> if rxCnt3 == clksPB - 1
                      then let shifted = pack sampled ++# slice d7 d1 rxShift3
                            in if rxBitIdx3 == 7
                                 then (RxStop3, 0, rxBitIdx3, shifted, False)
                                 else (RxData3, 0, rxBitIdx3 + 1, shifted, False)
                      else (RxData3, rxCnt3 + 1, rxBitIdx3, rxShift3, False)
        RxStop3  -> if rxCnt3 == clksPB - 1
                      then (RxIdle3, 0, 0, rxShift3, sampled == 1)
                      else (RxStop3, rxCnt3 + 1, rxBitIdx3, rxShift3, False)

      dataOut = if valid' then shift' else rxData3
      s' = s { rxFSM3    = fsm'
             , rxCnt3    = cnt'
             , rxBitIdx3 = bitIdx'
             , rxShift3  = shift'
             , rxSync13  = sync1
             , rxSync23  = sync2
             , rxData3   = dataOut
             , rxValid3  = valid' }
   in (s', (dataOut, valid'))

uartRx3 :: (HiddenClockResetEnable dom) => Signal dom Bit -> Signal dom (BitVector 8, Bool)
uartRx3 = mealy uartRxT3 initRxState3

-- ===========================================================================
-- UART TX
-- ===========================================================================

uartTxT3 :: UartTxState3 -> (BitVector 8, Bool) -> (UartTxState3, (Bit, Bool))
uartTxT3 s@UartTxState3{..} (dat, send) =
  let clksPB = fromIntegral (natVal (Proxy :: Proxy ClksPerBit3)) :: Unsigned 9
      busy   = txFSM3 /= TxIdle3
      (fsm', cnt', bitIdx', shift', pin') = case txFSM3 of
        TxIdle3  -> if send && not busy
                      then (TxStart3, 0, 0, dat, 1)
                      else (TxIdle3, txCnt3, txBitIdx3, txShift3, 1)
        TxStart3 -> if txCnt3 == clksPB - 1
                      then (TxData3, 0, 0, txShift3, 0)
                      else (TxStart3, txCnt3 + 1, txBitIdx3, txShift3, 0)
        TxData3  -> let bit0    = lsb txShift3
                        shifted = 0 ++# slice d7 d1 txShift3
                     in if txCnt3 == clksPB - 1
                          then if txBitIdx3 == 7
                                 then (TxStop3,  0,           txBitIdx3, shifted, bit0)
                                 else (TxData3,  0,           txBitIdx3 + 1, shifted, bit0)
                          else (TxData3, txCnt3 + 1, txBitIdx3, txShift3, bit0)
        TxStop3  -> if txCnt3 == clksPB - 1
                      then (TxIdle3, 0, txBitIdx3, txShift3, 1)
                      else (TxStop3, txCnt3 + 1, txBitIdx3, txShift3, 1)
      s' = s { txFSM3    = fsm'
             , txCnt3    = cnt'
             , txBitIdx3 = bitIdx'
             , txShift3  = shift'
             , txPinLvl3 = pin' }
   in (s', (pin', busy))

uartTx3 :: (HiddenClockResetEnable dom) => Signal dom (BitVector 8, Bool) -> Signal dom (Bit, Bool)
uartTx3 = mealy uartTxT3 initTxState3

-- ===========================================================================
-- TX FIFO helpers
-- ===========================================================================

fifoEmpty3 :: FifoState3 -> Bool
fifoEmpty3 FifoState3{..} = fifoWr3 == fifoRd3

fifoFull3 :: FifoState3 -> Bool
fifoFull3 FifoState3{..} = (fifoWr3 + 1) == fifoRd3

fifoPush3 :: FifoState3 -> BitVector 8 -> FifoState3
fifoPush3 st@FifoState3{..} b = st { fifoBuf3 = replace fifoWr3 b fifoBuf3
                                    , fifoWr3  = fifoWr3 + 1 }

fifoPop3 :: FifoState3 -> (FifoState3, BitVector 8)
fifoPop3 st@FifoState3{..} = (st { fifoRd3 = fifoRd3 + 1 }, fifoBuf3 !! fifoRd3)

-- ===========================================================================
-- 3-Stage Pipeline CPU state
-- ===========================================================================

-- ---------------------------------------------------------------------------
-- IF/ID pipeline register
-- ---------------------------------------------------------------------------

-- | Contents of the IF→ID latch.
--   Also carries the branch-predictor prediction made at the end of IF so
--   that EX can compare against the actual outcome.
data IfIdReg = IfIdReg
  { ifidValid      :: Bool
  , ifidPC         :: PC           -- PC of the instruction in ID
  , ifidInstr      :: Instr32
    -- Branch predictor snapshot (set by predictor in IF, checked in EX)
  , ifidPredTaken  :: Bool         -- predictor said "branch taken"
  , ifidPredTarget :: PC           -- predicted target PC
  } deriving (Generic, NFDataX, Show)

emptyIfId :: IfIdReg
emptyIfId = IfIdReg False 0 0 False 0

-- ---------------------------------------------------------------------------
-- ID/EX pipeline register
-- ---------------------------------------------------------------------------

data IdExReg = IdExReg
  { idexValid      :: Bool
  , idexPC         :: PC
  , idexOpcode     :: BitVector 4
  , idexRd         :: RegIdx
  , idexRsVal      :: Unsigned 32  -- register-file read result
  , idexImm24      :: Unsigned 32  -- zero-extended 24-bit immediate
    -- Forward prediction snapshot (needed to compare in EX)
  , idexPredTaken  :: Bool
  , idexPredTarget :: PC
  } deriving (Generic, NFDataX, Show)

emptyIdEx :: IdExReg
emptyIdEx = IdExReg False 0 0 0 0 0 False 0

-- ---------------------------------------------------------------------------
-- Full CPU state
-- ---------------------------------------------------------------------------

data CpuState3 = CpuState3
  { cpu3PC       :: PC
  , cpu3Regs     :: RegFile       -- R0..R3
  , cpu3Halt     :: Bool
    -- Pipeline latches
  , cpu3IfId     :: IfIdReg
  , cpu3IdEx     :: IdExReg
    -- Output port (one-cycle pulse)
  , cpu3OutValid :: Bool
  , cpu3OutData  :: BitVector 8
    -- Branch predictor interface (resolved in EX, consumed externally)
    -- These are combinational outputs latched here for one cycle so the
    -- top-level can observe them without extra plumbing.
  , cpu3BrActual :: Bool          -- did a branch/jump actually fire?
  , cpu3BrTarget :: PC            -- where it went
  } deriving (Generic, NFDataX, Show)

initCpuState3 :: CpuState3
initCpuState3 = CpuState3
  { cpu3PC       = 0
  , cpu3Regs     = repeat 0
  , cpu3Halt     = False
  , cpu3IfId     = emptyIfId
  , cpu3IdEx     = emptyIdEx
  , cpu3OutValid = False
  , cpu3OutData  = 0
  , cpu3BrActual = False
  , cpu3BrTarget = 0
  }

-- ===========================================================================
-- Branch-predictor stub
-- ===========================================================================
--
-- A real predictor would look at the decoded instruction and historical
-- information to decide whether to predict taken and what target to use.
-- This stub is the "predict-not-taken" baseline: it never predicts a branch
-- as taken, so the fetch always continues sequentially.  Replace the body
-- of `branchPredict` to plug in a real predictor later.
--
-- Inputs:
--   instrWord  — the raw 32-bit word just fetched (available at end of IF)
--   fetchPC    — the PC of that instruction
-- Outputs:
--   (predictTaken, predictTarget)

branchPredict :: Instr32 -> PC -> (Bool, PC)
branchPredict _instr _fetchPC = (False, 0)   -- stub: always predict not-taken

-- ===========================================================================
-- Single-cycle CPU step (pure, for mealy machine)
-- ===========================================================================
--
-- Pipeline structure
-- ──────────────────
--   Cycle N  :  IF  issues PC to BRAM.
--   Cycle N+1:  ID  sees BRAM output (one-cycle read latency).
--               IF  issues next PC.
--   Cycle N+2:  EX  operates.
--               ID  decodes the following instruction.
--               IF  issues the one after.
--
-- The BRAM read latency is modelled by threading `romData` (the output of
-- the block RAM registered in the previous cycle) as an explicit input.
-- Clash will synthesize the array as block RAM automatically when the
-- top-level wraps it with `blockRamPow2` or embeds it in a `Vec` indexed
-- with a registered address.
--
-- Branch resolution
-- ─────────────────
--   Branch outcome is known at the end of EX.  If the actual direction
--   disagrees with the prediction captured in the ID/EX latch:
--     • Squash the IF/ID and ID/EX latches (turn them into bubbles).
--     • Redirect PC to the actual target.
--   The penalty is exactly 2 cycles (the two squashed stages).
--   A perfect predictor reduces this to 0 cycles.
--
-- Hazards
-- ───────
--   Load-use hazard: there are no load instructions in this ISA (all
--   immediates are inlined), so no stall logic is needed.
--   RAW hazard: EX → ID forwarding is provided.  An instruction in EX
--   that writes rd can forward its result to the rs read done in ID.

stepCpu3 ::
  CpuState3        ->
  (Instr32, Bool)  -> -- (bram_out_registered, cpu_enable)
  ( CpuState3
  , ( PC            -- next BRAM address to issue (= next IF PC)
    , Bool          -- output valid (one cycle pulse)
    , BitVector 8   -- output byte
    , Bool          -- halted
    , Bool          -- branch actually taken (for predictor training)
    , PC            -- actual branch target   (for predictor training)
    )
  )
stepCpu3 s@CpuState3{..} (bramOut, en)
  -- Halted: freeze everything
  | cpu3Halt =
      ( s { cpu3OutValid = False, cpu3BrActual = False }
      , (cpu3PC, False, 0, True, False, 0) )
  -- Clock-enable low: freeze all state
  | not en =
      ( s { cpu3OutValid = False, cpu3BrActual = False }
      , (cpu3PC, False, 0, False, False, 0) )
  | otherwise =
      let
        -- ── EX stage ──────────────────────────────────────────────────────
        idex = cpu3IdEx

        (acc_rd, _acc_pc, acc_halt, acc_ov, acc_od, brActual, brTarget, squash) =
          if idexValid idex
            then
              let op     = idexOpcode idex
                  rd     = idexRd     idex
                  rsVal  = idexRsVal  idex
                  imm    = idexImm24  idex
                  exPredTk = idexPredTaken  idex
                  exPredTg = idexPredTarget idex

                  -- ALU
                  aluResult = case op of
                    0x1 -> imm                -- LOAD
                    0x2 -> rsVal + imm        -- ADD
                    0x3 -> rsVal - imm        -- SUB
                    0x4 -> rsVal .&. imm      -- AND
                    0x5 -> rsVal .|. imm      -- OR
                    0x6 -> rsVal              -- MOV
                    _   -> rsVal             -- others don't write result

                  -- Write-back: update the register file
                  regs' = case op of
                    0x1 -> replace rd aluResult cpu3Regs -- LOAD
                    0x2 -> replace rd aluResult cpu3Regs -- ADD
                    0x3 -> replace rd aluResult cpu3Regs -- SUB
                    0x4 -> replace rd aluResult cpu3Regs -- AND
                    0x5 -> replace rd aluResult cpu3Regs -- OR
                    0x6 -> replace rd aluResult cpu3Regs -- MOV
                    _   -> cpu3Regs

                  -- Branch resolution
                  (brTk, brTg) = case op of
                    0x9 -> (True,  truncateB imm)                            -- JMP
                    0xA -> (rsVal == 0, if rsVal == 0 then truncateB imm else cpu3PC) -- JZ
                    0xB -> (rsVal /= 0, if rsVal /= 0 then truncateB imm else cpu3PC) -- JNZ
                    _   -> (False, cpu3PC)

                  -- Misprediction: predictor said X, actual is Y
                  mispred = brTk /= exPredTk || (brTk && brTg /= exPredTg)
                  -- Squash the two younger pipeline stages on misprediction
                  sq = mispred

                  -- Output
                  (ov, od) = case op of
                    0x7 -> (True, slice d7 d0 (pack rsVal)) -- OUT
                    _   -> (False, 0)

                  -- Halt
                  halt' = op == 0x8

                  -- PC after branch resolution
                  pc' = if brTk then brTg else cpu3PC

               in (regs', pc', halt', ov, od, brTk, brTg, sq)
            else (cpu3Regs, cpu3PC, False, False, 0, False, 0, False)

        -- ── ID stage ──────────────────────────────────────────────────────
        -- If squashing, replace with a bubble; otherwise decode ifid.
        ifid    = cpu3IfId
        idex'   =
          if squash || not (ifidValid ifid)
            then emptyIdEx
            else
              let instr   = ifidInstr ifid
                  opcodeF = slice d31 d28 instr
                  rdF     = unpack (slice d27 d26 instr) :: RegIdx
                  rsF     = unpack (slice d25 d24 instr) :: RegIdx
                  imm24F  = resize (unpack (slice d23 d0 instr) :: Unsigned 24) :: Unsigned 32

                  -- Register read with EX→ID forwarding:
                  -- If the instruction currently in EX writes the same rd as
                  -- our rs, forward the EX result (aluResult) instead of
                  -- reading the (stale) register file.
                  rsRaw   = cpu3Regs !! rsF
                  -- Determine which rd EX would write (if any)
                  exWritesRd = idexValid idex && idexOpcode idex `elem`
                                 [0x1, 0x2, 0x3, 0x4, 0x5, 0x6]
                  exRd       = idexRd idex
                  -- Forwarded rs value
                  rsVal   = if exWritesRd && exRd == rsF
                              then cpu3Regs !! rsF  -- will be updated after EX wb
                              -- Note: since we update cpu3Regs in the same step we
                              -- actually read acc_rd (the updated file).
                              -- We re-read from acc_rd below after computing it.
                              -- See forwarding fixup note.
                              else rsRaw

               in IdExReg
                    { idexValid      = True
                    , idexPC         = ifidPC ifid
                    , idexOpcode     = opcodeF
                    , idexRd         = rdF
                    , idexRsVal      = rsVal
                    , idexImm24      = imm24F
                    , idexPredTaken  = ifidPredTaken  ifid
                    , idexPredTarget = ifidPredTarget ifid
                    }

        -- EX→ID forwarding fixup:
        -- acc_rd is the register file *after* EX writeback.  Re-apply
        -- forwarding now that we have the final value.
        idex'' =
          if idexValid idex'
            then
              let rsF'       = unpack (slice d25 d24 (ifidInstr ifid)) :: RegIdx
                  exWritesRd = idexValid idex && idexOpcode idex `elem`
                                 [0x1, 0x2, 0x3, 0x4, 0x5, 0x6 :: BitVector 4]
                  forwardVal = if exWritesRd && idexRd idex == rsF'
                                 then acc_rd !! (idexRd idex)
                                 else acc_rd !! rsF'
               in idex' { idexRsVal = forwardVal }
            else idex'

        -- ── IF stage ──────────────────────────────────────────────────────
        -- `bramOut` is the instruction fetched for the PC we issued LAST
        -- cycle, i.e. the current ifidPC (already sitting in the IF/ID latch
        -- from the previous cycle).  What we do this cycle in IF is:
        --   (a) compute the next PC to send to the BRAM address port,
        --   (b) apply the branch predictor to the instruction now in ID
        --       (bramOut) and decide whether to speculate on a branch.
        --   (c) update the IF/ID latch with bramOut.

        -- Next sequential PC (wraps at 1024)
        nextSeqPC = cpu3PC + 1

        -- Apply predictor to the instruction now being decoded (bramOut).
        (predTk, predTg) = branchPredict bramOut cpu3PC

        -- If squash: redirect to actual target, invalidate IF/ID.
        -- Else if predictor says taken: redirect to predicted target.
        -- Else: continue sequentially.
        (nextPC, ifid') =
          if squash
            then
              -- Redirect fetch to the resolved target; flush the IF/ID latch.
              (brTarget, emptyIfId)
            else
              if predTk
                then
                  -- Predictor says taken: speculatively fetch from predTg.
                  ( predTg
                  , IfIdReg
                      { ifidValid      = True
                      , ifidPC         = cpu3PC
                      , ifidInstr      = bramOut
                      , ifidPredTaken  = predTk
                      , ifidPredTarget = predTg
                      }
                  )
                else
                  -- Predict not-taken: continue sequential fetch.
                  ( nextSeqPC
                  , IfIdReg
                      { ifidValid      = True
                      , ifidPC         = cpu3PC
                      , ifidInstr      = bramOut
                      , ifidPredTaken  = False
                      , ifidPredTarget = nextSeqPC
                      }
                  )

        -- ── Assemble new state ────────────────────────────────────────────
        s' = s
              { cpu3PC       = nextPC
              , cpu3Regs     = acc_rd
              , cpu3Halt     = acc_halt
              , cpu3IfId     = ifid'
              , cpu3IdEx     = idex''
              , cpu3OutValid = acc_ov
              , cpu3OutData  = acc_od
              , cpu3BrActual = brActual
              , cpu3BrTarget = brTarget
              }
     in ( s'
        , ( nextPC, acc_ov, acc_od, acc_halt, brActual, brTarget )
        )

-- ===========================================================================
-- Controller FSM state (load/run/done, plus BRAM programming)
-- ===========================================================================

-- | Byte-level programming counter: we receive 4096 bytes (1024 words × 4
--   bytes, big-endian) and assemble them into 32-bit words.
data ProgFSM = PgIdle | PgLoad | PgRun | PgSendDone
  deriving (Generic, NFDataX, Show, Eq)

data TopState3 = TopState3
  { ctrlFSM3    :: ProgFSM
  , progByteIdx :: Unsigned 12  -- 0..4095 (4096 bytes = 1024 words)
  , progWordAcc :: BitVector 32 -- assembles one 32-bit word from 4 bytes
  , cpu3Rst     :: Bool
  , led3Latch   :: BitVector 8
  , out3Pending :: Bool
  , out3PendDat :: BitVector 8
  , runTimeout3 :: Unsigned 16  -- larger watchdog for bigger programs
  } deriving (Generic, NFDataX, Show)

initTopState3 :: TopState3
initTopState3 = TopState3
  { ctrlFSM3    = PgIdle
  , progByteIdx = 0
  , progWordAcc = 0
  , cpu3Rst     = True
  , led3Latch   = 0xFF
  , out3Pending = False
  , out3PendDat = 0
  , runTimeout3 = 0
  }

-- ===========================================================================
-- Full system state
-- ===========================================================================

data SysState3 = SysState3
  { sTop3  :: TopState3
  , sCpu3  :: CpuState3
  , sFifo3 :: FifoState3
  , sRx3   :: UartRxState3
  , sTx3   :: UartTxState3
  } deriving (Generic, NFDataX, Show)

initSysState3 :: SysState3
initSysState3 = SysState3 initTopState3 initCpuState3 initFifoState3 initRxState3 initTxState3

-- ===========================================================================
-- Full system step
-- ===========================================================================

sysStep3 :: SysState3 -> (Bit, Instr32) -> (SysState3, ((Bit, BitVector 6), PC, Maybe (PC, Instr32)))
sysStep3 SysState3{..} (rxPin, bramOut) =
  let top  = sTop3
      cpu0 = sCpu3
      fifo = sFifo3
      tx   = sTx3

      -- ── UART RX ──────────────────────────────────────────────────────────
      (rx', (rxByte, rxVld)) = uartRxT3 sRx3 rxPin

      -- ── CPU enable ───────────────────────────────────────────────────────
      cpuEn = ctrlFSM3 top == PgRun && not (out3Pending top)

      -- `bramOut` is passed in from the external `blockRam` primitive:
      -- it holds mem[rdAddr_prev], i.e. the instruction at the PC issued last cycle.

      -- ── CPU step ─────────────────────────────────────────────────────────
      (cpu1, (_nextPC, cpuOV, cpuOD, cpuHalted, _brActual, _brTarget)) =
        if cpu3Rst top
          then (initCpuState3, (0, False, 0, False, False, 0))
          else stepCpu3 cpu0 (bramOut, cpuEn)

      -- ── FIFO drain → TX ──────────────────────────────────────────────────
      txBusy = txFSM3 tx /= TxIdle3

      (fifo1, txDat, txSend) =
        if not (fifoEmpty3 fifo) && not txBusy
          then let (f', b) = fifoPop3 fifo in (f', b, True)
          else (fifo, 0, False)

      (tx', _) = uartTxT3 tx (txDat, txSend)

      -- ── Resolve out_pending ───────────────────────────────────────────────
      (fifo2, top1) =
        if out3Pending top && not (fifoFull3 fifo1)
          then (fifoPush3 fifo1 (out3PendDat top), top { out3Pending = False })
          else (fifo1, top)

      -- ── Controller FSM ───────────────────────────────────────────────────
      (top2, fifo3, cpu2, bramWrCmd) = case ctrlFSM3 top1 of

        PgIdle ->
          let top' | not rxVld      = top1 { cpu3Rst = True }
                   | rxByte == 0x50 = top1 { ctrlFSM3 = PgLoad
                                           , progByteIdx = 0
                                           , progWordAcc = 0
                                           , cpu3Rst = True }
                   | rxByte == 0x52 = top1 { ctrlFSM3 = PgRun
                                           , cpu3Rst = False
                                           , runTimeout3 = 0 }
                   | rxByte == 0x58 = top1 { cpu3Rst = True }
                   | otherwise      = top1 { cpu3Rst = True }
           in (top', fifo2, cpu1, Nothing)

        PgLoad ->
          if rxVld
            then
              -- Assemble 4 bytes into one 32-bit word (big-endian, MSB first)
              let bytePos  = progByteIdx top1 `mod` 4
                  shiftAmt = (3 - resize bytePos) * 8 :: Unsigned 5
                  newByte  = resize (unpack rxByte :: Unsigned 8) :: Unsigned 32
                  wordAcc' = progWordAcc top1 .|.
                               pack (newByte `shiftL` fromIntegral shiftAmt)
                  wordIdx  = truncateB (progByteIdx top1 `div` 4) :: Unsigned 10
                  wacc'    = if bytePos == 3 then 0 else wordAcc'
                  -- Issue a block-RAM write when the full word is assembled
                  pgWr     = if bytePos == 3 then Just (wordIdx, wordAcc') else Nothing
                  byteIdx' = progByteIdx top1 + 1
                  done     = progByteIdx top1 == 4095           -- last byte
               in if done
                    then
                      let f' = if not (fifoFull3 fifo2)
                                 then fifoPush3 fifo2 0x4B
                                 else fifo2
                       in ( top1 { ctrlFSM3    = PgIdle
                                 , progByteIdx = 0
                                 , progWordAcc = 0 }
                          , f'
                          , cpu1
                          , pgWr )
                    else
                      ( top1 { progByteIdx = byteIdx'
                              , progWordAcc = wacc' }
                      , fifo2
                      , cpu1
                      , pgWr )
            else (top1, fifo2, cpu1, Nothing)

        PgRun ->
          let top1' = top1 { cpu3Rst = False }
              (fifo2', top2') =
                if cpuOV
                  then if not (fifoFull3 fifo2)
                         then (fifoPush3 fifo2 cpuOD, top1' { runTimeout3 = 0 })
                         else (fifo2, top1' { out3Pending = True
                                            , out3PendDat = cpuOD
                                            , runTimeout3 = 0 })
                  else if cpuEn && not (out3Pending top1')
                         then (fifo2, top1' { runTimeout3 = runTimeout3 top1' + 1 })
                         else (fifo2, top1')
              top2'' =
                if (cpuHalted || runTimeout3 top2' == 0xFFFF) && not (out3Pending top2')
                  then top2' { ctrlFSM3 = PgSendDone }
                  else top2'
           in (top2'', fifo2', cpu1, Nothing)

        PgSendDone ->
          if not (fifoFull3 fifo2)
            then (top1 { ctrlFSM3 = PgIdle }, fifoPush3 fifo2 0x44, cpu1, Nothing)
            else (top1, fifo2, cpu1, Nothing)

      -- ── 'X' soft-reset reply ─────────────────────────────────────────────
      fifo4 =
        if rxVld && rxByte == 0x58 && ctrlFSM3 top == PgIdle && not (fifoFull3 fifo3)
          then fifoPush3 fifo3 0x4B
          else fifo3

      -- ── LED latch ────────────────────────────────────────────────────────
      led3Latch' = if cpuOV then cpuOD else led3Latch top
      top3 = top2 { led3Latch = led3Latch' }

      led = complement (slice d5 d0 led3Latch')
      bramRdAddr = cpu3PC cpu2
      sys' = SysState3 top3 cpu2 fifo4 rx' tx'
   in (sys', ((txPinLvl3 tx', led), bramRdAddr, bramWrCmd))

-- ===========================================================================
-- Top-level Clash entity
-- ===========================================================================

topEntity3 ::
  Clock System ->
  Signal System Bit ->
  Signal System (Bit, BitVector 6)
topEntity3 clk rxPin = withClockResetEnable clk resetGen enableGen $
  let fullOut = mealy sysStep3 initSysState3 (bundle (rxPin, bramOut))
      rdAddr  = (\(_,a,_) -> a) <$> fullOut
      wrCmd   = (\(_,_,w) -> w) <$> fullOut
      bramOut = blockRam (testProgram3' :: InstrMem) rdAddr wrCmd
  in (\(x,_,_) -> x) <$> fullOut

-- ===========================================================================
-- Instruction assembler helper
-- ===========================================================================

-- | Build a 32-bit instruction from opcode, rd, rs, and 24-bit immediate.
mkInstr3 :: BitVector 4 -> RegIdx -> RegIdx -> BitVector 24 -> Instr32
mkInstr3 op rd rs imm = op ++# pack rd ++# pack rs ++# imm

-- | Build an instruction with no register operands (NOP, HALT, JMP).
mkInstr3NoReg :: BitVector 4 -> BitVector 24 -> Instr32
mkInstr3NoReg op imm = op ++# (0 :: BitVector 4) ++# imm

-- ===========================================================================
-- Default / test program
-- ===========================================================================

-- | A simple test that exercises all instruction types including branches.
--
-- @
--  0: LOAD R0, 5       R0 = 5
--  1: LOAD R1, 3       R1 = 3
--  2: ADD  R2 = R0 + 0 = 5
--  3: ADD  R2 = R2 + 3 = 8
--  4: OUT  R2          emit 8
--  5: SUB  R3 = R2 - 2 = 6
--  6: AND  R3 = R3 .&. 7 = 6
--  7: OR   R3 = R3 .|. 1 = 7
--  8: OUT  R3          emit 7
--  9: LOAD R0, 0       R0 = 0
-- 10: JZ   R0, 12      branch taken → PC = 12
-- 11: HALT             (skipped)
-- 12: OUT  R2          emit 8
-- 13: HALT
-- @
testProgram3 :: InstrMem
testProgram3 = testProgram3'

-- Rebuild testProgram3 as a proper 1024-element Vec.
-- Clash's Vec concatenation requires type-level size arithmetic; the cleanest
-- approach for synthesis is to define the ROM as a `repeat` with the first N
-- slots overridden using `replace`.
testProgram3' :: InstrMem
testProgram3' =
  let nop = mkInstr3NoReg 0x0 0
      base = repeat nop :: InstrMem
   in replace (0  :: Unsigned 10) (mkInstr3 0x1 0 0 5  )
    $ replace (1  :: Unsigned 10) (mkInstr3 0x1 1 0 3  )
    $ replace (2  :: Unsigned 10) (mkInstr3 0x2 2 0 0  )
    $ replace (3  :: Unsigned 10) (mkInstr3 0x2 2 2 3  )
    $ replace (4  :: Unsigned 10) (mkInstr3 0x7 0 2 0  )
    $ replace (5  :: Unsigned 10) (mkInstr3 0x3 3 2 2  )
    $ replace (6  :: Unsigned 10) (mkInstr3 0x4 3 3 7  )
    $ replace (7  :: Unsigned 10) (mkInstr3 0x5 3 3 1  )
    $ replace (8  :: Unsigned 10) (mkInstr3 0x7 0 3 0  )
    $ replace (9  :: Unsigned 10) (mkInstr3 0x1 0 0 0  )
    $ replace (10 :: Unsigned 10) (mkInstr3 0xA 0 0 12 )
    $ replace (11 :: Unsigned 10) (mkInstr3 0x8 0 0 0  )
    $ replace (12 :: Unsigned 10) (mkInstr3 0x7 0 2 0  )
    $ replace (13 :: Unsigned 10) (mkInstr3 0x8 0 0 0  )
    $ base

-- ===========================================================================
-- Simulation helpers
-- ===========================================================================

-- | Simulate just the CPU (no UART) feeding testProgram3'.
simulateCpu3 :: Int -> [(PC, Bool, BitVector 8, Bool)]
simulateCpu3 n = P.take n $ go initCpuState3
  where
    go s =
      let addr  = cpu3PC s
          instr = testProgram3' !! addr
          (s', (nextAddr, ov, od, halted, _, _)) = stepCpu3 s (instr, True)
       in (nextAddr, ov, od, halted) : go s'
