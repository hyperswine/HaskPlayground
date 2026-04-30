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

-- | Tang Nano 20K top-level for CPURiscV (optimised).
--
-- ── What changed vs v1 ────────────────────────────────────────────────────
--
--   CP4 – Register file blockRam.
--         A third blockRam (regBram, 32 × 32-bit) is instantiated alongside
--         the existing instruction and data BRAMs.  Its read address is driven
--         by the rs1/rs2 fields of the instruction in IF/ID (one cycle of
--         read latency, hidden by the ID→EX pipeline register).  Its write
--         port is driven by regWrCmd from stepCpuRV.
--
--         Write-first bypass: if regWrCmd.addr == the read address supplied
--         this cycle, the written value is returned directly (same logic as
--         the data BRAM bypass already present).
--
--   The sysStepRV Mealy function now threads regRdA/regRdB into stepCpuRV
--   and routes regWrCmd to the blockRam write port.
--
-- ── Address map, UART protocol, pin assignments ──────────────────────────
--   Unchanged from v1 – see v1 header for details.
--
module CPURiscVTop where

import CPURiscV
import Clash.Annotations.TopEntity ()
import Clash.Prelude hiding (take)
import qualified Prelude as P

-- ===========================================================================
-- Clock domain
-- ===========================================================================

createDomain vSystem {vName = "Dom80", vPeriod = 12346}

type ClkFreqRV    = 81_000_000
type BaudRateRV   = 115_200
type ClksPerBitRV = Div ClkFreqRV BaudRateRV
type HalfBitRV    = Div ClksPerBitRV 2

-- ===========================================================================
-- UART RX
-- ===========================================================================

data RxFSM = RxIdle | RxStart | RxData | RxStop
  deriving (Generic, NFDataX, Show, Eq)

data UartRxSt = UartRxSt
  { rxFSM    :: RxFSM,    rxCnt    :: Unsigned 10, rxBitIdx :: Unsigned 3
  , rxShift  :: BitVector 8, rxSync1  :: Bit,       rxSync2  :: Bit
  , rxData   :: BitVector 8, rxValid  :: Bool
  } deriving (Generic, NFDataX, Show)

initRxSt :: UartRxSt
initRxSt = UartRxSt RxIdle 0 0 0 1 1 0 False

uartRxStep :: UartRxSt -> Bit -> (UartRxSt, (BitVector 8, Bool))
uartRxStep s@UartRxSt{..} rxPin =
  let sampled = rxSync2
      clksPB  = snatToNum (SNat :: SNat ClksPerBitRV) :: Unsigned 10
      half    = snatToNum (SNat :: SNat HalfBitRV)    :: Unsigned 10
      shifted = pack sampled ++# slice d7 d1 rxShift
      (fsm', cnt', bitIdx', shift', valid') = case rxFSM of
        RxIdle
          | sampled == 0 -> (RxStart, 0, 0, rxShift, False)
          | otherwise    -> (RxIdle,  0, 0, rxShift, False)
        RxStart
          | rxCnt == half - 1, sampled == 0 -> (RxData, 0, 0, rxShift, False)
          | rxCnt == half - 1               -> (RxIdle, 0, 0, rxShift, False)
          | otherwise -> (RxStart, rxCnt + 1, rxBitIdx, rxShift, False)
        RxData
          | rxCnt == clksPB - 1, rxBitIdx == 7 -> (RxStop, 0, rxBitIdx, shifted, False)
          | rxCnt == clksPB - 1                 -> (RxData, 0, rxBitIdx + 1, shifted, False)
          | otherwise -> (RxData, rxCnt + 1, rxBitIdx, rxShift, False)
        RxStop
          | rxCnt == clksPB - 1 -> (RxIdle, 0, 0, rxShift, sampled == 1)
          | otherwise            -> (RxStop, rxCnt + 1, rxBitIdx, rxShift, False)
      dataOut = if valid' then shift' else rxData
      s' = s { rxFSM = fsm', rxCnt = cnt', rxBitIdx = bitIdx', rxShift = shift'
             , rxSync1 = rxPin, rxSync2 = rxSync1, rxData = dataOut, rxValid = valid' }
   in (s', (dataOut, valid'))

-- ===========================================================================
-- UART TX
-- ===========================================================================

data TxFSM = TxIdle | TxStart | TxData | TxStop
  deriving (Generic, NFDataX, Show, Eq)

data UartTxSt = UartTxSt
  { txFSM    :: TxFSM,  txCnt    :: Unsigned 10, txBitIdx :: Unsigned 3
  , txShift  :: BitVector 8, txPin :: Bit
  } deriving (Generic, NFDataX, Show)

initTxSt :: UartTxSt
initTxSt = UartTxSt TxIdle 0 0 0 1

uartTxStep :: UartTxSt -> (BitVector 8, Bool) -> (UartTxSt, (Bit, Bool))
uartTxStep s@UartTxSt{..} (dat, send) =
  let clksPB = snatToNum (SNat :: SNat ClksPerBitRV) :: Unsigned 10
      busy   = txFSM /= TxIdle
      bit0   = lsb txShift
      shifted = 0 ++# slice d7 d1 txShift
      (fsm', cnt', bitIdx', shift', pin') = case txFSM of
        TxIdle
          | send, not busy -> (TxStart, 0, 0, dat, 1)
          | otherwise      -> (TxIdle, txCnt, txBitIdx, txShift, 1)
        TxStart
          | txCnt == clksPB - 1 -> (TxData, 0, 0, txShift, 0)
          | otherwise            -> (TxStart, txCnt + 1, txBitIdx, txShift, 0)
        TxData
          | txCnt == clksPB - 1, txBitIdx == 7 -> (TxStop, 0, txBitIdx, shifted, bit0)
          | txCnt == clksPB - 1                 -> (TxData, 0, txBitIdx + 1, shifted, bit0)
          | otherwise -> (TxData, txCnt + 1, txBitIdx, txShift, bit0)
        TxStop
          | txCnt == clksPB - 1 -> (TxIdle, 0, txBitIdx, txShift, 1)
          | otherwise            -> (TxStop, txCnt + 1, txBitIdx, txShift, 1)
      s' = s { txFSM = fsm', txCnt = cnt', txBitIdx = bitIdx', txShift = shift', txPin = pin' }
   in (s', (pin', busy))

-- ===========================================================================
-- 16-deep TX FIFO
-- ===========================================================================

data FifoSt = FifoSt
  { fifoBuf :: Vec 16 (BitVector 8), fifoWr :: Unsigned 4, fifoRd :: Unsigned 4 }
  deriving (Generic, NFDataX, Show)

initFifoSt :: FifoSt
initFifoSt = FifoSt (repeat 0) 0 0

fifoEmpty :: FifoSt -> Bool; fifoEmpty FifoSt{..} = fifoWr == fifoRd
fifoFull  :: FifoSt -> Bool; fifoFull  FifoSt{..} = (fifoWr + 1) == fifoRd

fifoPush :: FifoSt -> BitVector 8 -> FifoSt
fifoPush st@FifoSt{..} b = st { fifoBuf = replace fifoWr b fifoBuf, fifoWr = fifoWr + 1 }

fifoPop :: FifoSt -> (FifoSt, BitVector 8)
fifoPop st@FifoSt{..} = (st { fifoRd = fifoRd + 1 }, fifoBuf !! fifoRd)

-- ===========================================================================
-- Control FSM
-- ===========================================================================

data ProgFSM = PgIdle | PgLoad | PgRun | PgSendDone
  deriving (Generic, NFDataX, Show, Eq)

data TopStateRV = TopStateRV
  { ctrlFSM    :: ProgFSM
  , progByteIdx :: Unsigned 12
  , progWordAcc :: BitVector 32
  , cpuRst     :: Bool
  , ledLatch   :: BitVector 8
  , outPending :: Bool
  , outPendDat :: BitVector 8
  , runTimeout :: Unsigned 16
  } deriving (Generic, NFDataX, Show)

initTopStateRV :: TopStateRV
initTopStateRV = TopStateRV
  { ctrlFSM = PgIdle, progByteIdx = 0, progWordAcc = 0
  , cpuRst = True, ledLatch = 0xFF
  , outPending = False, outPendDat = 0, runTimeout = 0 }

-- ===========================================================================
-- Full system state
-- ===========================================================================

data SysStateRV = SysStateRV
  { sTop  :: TopStateRV
  , sCpu  :: CpuStateRV
  , sMC   :: MemCtrl
  , sFifo :: FifoSt
  , sRx   :: UartRxSt
  , sTx   :: UartTxSt
  } deriving (Generic, NFDataX, Show)

initSysStateRV :: SysStateRV
initSysStateRV = SysStateRV
  { sTop  = initTopStateRV
  , sCpu  = initCpuStateRV
  , sMC   = initMemCtrl
  , sFifo = initFifoSt
  , sRx   = initRxSt
  , sTx   = initTxSt
  }

-- ===========================================================================
-- System step (Mealy body)
-- ===========================================================================
--
-- Inputs: (rxPin, instrBRAM output, dataBRAM output, regBRAM port-A output,
--                                                     regBRAM port-B output,
--                                                     bhtBRAM output)
-- Outputs: ((txPin, led[5:0]),
--            instrBramRdAddr, instrBramWrCmd,
--            dataRdAddr,      dataWrCmd,
--            regRdAddrA,      regRdAddrB,   regWrCmd,
--            bhtRdAddr,       bhtWrCmd)
--
-- The register blockRam read addresses (regRdAddrA/B) are the rs1/rs2 fields
-- of the instruction currently in IF/ID, so the read data arrives one cycle
-- later – exactly when ID needs it.
--
-- CP5: bhtRdAddr = bhtIdx(cpu.rvPC), so the BHT prediction for the next
-- fetch arrives one cycle later -- matching CP2 registered-BHT semantics.

type SysOut =
  ( (Bit, BitVector 6)             -- (txPin, led)
  , Unsigned 10                    -- instrBramRdAddr
  , Maybe (Unsigned 10, Word32)    -- instrBramWrCmd
  , Unsigned 10                    -- dataRdAddr
  , Maybe (Unsigned 10, Word32)    -- dataWrCmd
  , Unsigned 5                     -- regRdAddrA            (CP4)
  , Unsigned 5                     -- regRdAddrB            (CP4)
  , Maybe (Unsigned 5, Word32)     -- regWrCmd              (CP4)
  , Unsigned 6                     -- bhtRdAddr             (CP5)
  , Maybe (Unsigned 6, BhtCounter) -- bhtWrCmd              (CP5)
  )

sysStepRV
  :: SysStateRV
  -> (Bit, Word32, Word32, Word32, Word32, BhtCounter)
     -- ^ (rxPin, instrBramOut, dataBramOut, regBramOutA, regBramOutB, bhtRd)
  -> (SysStateRV, SysOut)
sysStepRV SysStateRV{..} (rxPin, bramOut, dataBramOut, regBramOutA, regBramOutB, bhtRd) =
  let top  = sTop
      cpu0 = sCpu
      mc0  = sMC
      fifo = sFifo
      tx   = sTx

      -- ── UART RX ──────────────────────────────────────────────────────────
      (rx', (rxByte, rxVld)) = uartRxStep sRx rxPin

      -- ── CPU enable ───────────────────────────────────────────────────────
      cpuEn = ctrlFSM top == PgRun && not (outPending top)

      -- ── CPU step ─────────────────────────────────────────────────────────
      -- CP4: pass register file read results; receive write command back.
      -- CP5: pass BHT read result; receive BHT write command back.
      (cpu1, mc1, dataWrCmd, regWrCmd, bhtWrCmd, _nextPC, _rd, _val, _wbEn) =
        if cpuRst top
          then ( initCpuStateRV, mc0, Nothing, Nothing, Nothing, 0, 0, 0, False )
          else stepCpuRV cpu0 (bramOut, dataBramOut, regBramOutA, regBramOutB, bhtRd, mc0, cpuEn)

      -- Drain UART_TX byte written by CPU
      (mc2, cpuTxMaybe) = uartPop mc1

      -- ── TX path ──────────────────────────────────────────────────────────
      txBusy = txFSM tx /= TxIdle
      (fifo1, txDat, txSend) = case (fifoEmpty fifo, txBusy) of
        (False, False) -> let (f', b) = fifoPop fifo in (f', b, True)
        _              -> (fifo, 0, False)
      (tx', _) = uartTxStep tx (txDat, txSend)

      -- Drain outPending → FIFO
      (fifo2, top1) = case (outPending top, fifoFull fifo1) of
        (True, False) -> (fifoPush fifo1 (outPendDat top), top { outPending = False })
        _             -> (fifo1, top)

      -- ── Control FSM ──────────────────────────────────────────────────────
      (top2, fifo3, cpu2, instrWrCmd) = case ctrlFSM top1 of
        PgIdle ->
          let top'
                | not rxVld        = top1 { cpuRst = True }
                | rxByte == 0x50   = top1 { ctrlFSM = PgLoad, progByteIdx = 0
                                          , progWordAcc = 0, cpuRst = True }
                | rxByte == 0x52   = top1 { ctrlFSM = PgRun, cpuRst = False
                                          , runTimeout = 0 }
                | rxByte == 0x58   = top1 { cpuRst = True }
                | otherwise        = top1 { cpuRst = True }
           in (top', fifo2, cpu1, Nothing)

        PgLoad
          | not rxVld -> (top1, fifo2, cpu1, Nothing)
          | otherwise ->
              let bytePos  = progByteIdx top1 `mod` 4
                  shiftAmt = (3 - resize bytePos) * 8 :: Unsigned 5
                  newByte  = resize (unpack rxByte :: Unsigned 8) :: Unsigned 32
                  wordAcc' = progWordAcc top1 .|. pack (newByte `shiftL` fromIntegral shiftAmt)
                  wordIdx  = truncateB (progByteIdx top1 `div` 4) :: Unsigned 10
                  wacc''   = if bytePos == 3 then 0 else wordAcc'
                  pgWr     = if bytePos == 3 then Just (wordIdx, wordAcc') else Nothing
                  byteIdx' = progByteIdx top1 + 1
                  done     = progByteIdx top1 == 4095
                  f'       = if not (fifoFull fifo2) then fifoPush fifo2 0x4B else fifo2
               in if done
                    then (top1 { ctrlFSM = PgIdle, progByteIdx = 0, progWordAcc = 0 }, f', cpu1, pgWr)
                    else (top1 { progByteIdx = byteIdx', progWordAcc = wacc'' }, fifo2, cpu1, pgWr)

        PgRun ->
          let top1'      = top1 { cpuRst = False }
              cpuHalted  = rvHalt cpu1
              (fifo2', top2') = case cpuTxMaybe of
                Just b
                  | not (fifoFull fifo2) -> (fifoPush fifo2 b, top1' { runTimeout = 0 })
                  | otherwise            -> (fifo2, top1' { outPending = True
                                                          , outPendDat = b, runTimeout = 0 })
                Nothing
                  | cpuEn, not (outPending top1') ->
                      (fifo2, top1' { runTimeout = runTimeout top1' + 1 })
                  | otherwise -> (fifo2, top1')
              top2'' = case (cpuHalted || runTimeout top2' == 0xFFFF, outPending top2') of
                (True, False) -> top2' { ctrlFSM = PgSendDone }
                _             -> top2'
           in (top2'', fifo2', cpu1, Nothing)

        PgSendDone
          | not (fifoFull fifo2) -> (top1 { ctrlFSM = PgIdle }, fifoPush fifo2 0x44, cpu1, Nothing)
          | otherwise            -> (top1, fifo2, cpu1, Nothing)

      -- ACK reset command
      fifo4 = if rxVld && rxByte == 0x58 && ctrlFSM top == PgIdle
                         && not (fifoFull fifo3)
               then fifoPush fifo3 0x4B else fifo3

      -- LED
      ledLatch' = case cpuTxMaybe of Just b -> b; Nothing -> ledLatch top
      top3 = top2 { ledLatch = ledLatch' }
      led  = complement (slice d5 d0 ledLatch')

      -- ── Instruction BRAM read address ────────────────────────────────────
      instrRdAddr = truncateB (rvPC cpu2 `shiftR` 2) :: Unsigned 10

      -- ── Data BRAM read address ────────────────────────────────────────────
      dataRdAddr = truncateB (unpack (exmemAluOut (rvExMem cpu2)) `shiftR` 2) :: Unsigned 10

      -- ── CP4/CP6: Register BRAM read addresses ────────────────────────────
      -- CP6: Use bramOut (the raw instruction being loaded into IF/ID this
      -- cycle) rather than cpu2.rvIfId.ifidInstr.  The latter depends on
      -- `mispredicted` (which depends on the EX-stage ALU result), creating a
      -- 18+ ns critical path through: ALU → mispredicted → ifid' → regRdAddr
      -- → blockRam MUX tree.  Using bramOut (a registered DFF) breaks this
      -- chain; incorrect reads during a flush are harmless because idex' =
      -- emptyIdEx the following cycle (ifidValid = False).
      regRdAddrA = uRs1 (decode bramOut)
      regRdAddrB = uRs2 (decode bramOut)

      -- CP4: register file write command (from WB)
      regWrCmdOut :: Maybe (Unsigned 5, Word32)
      regWrCmdOut = regWrCmd

      -- ── CP5: BHT BRAM read address ────────────────────────────────────────
      -- Issue the BHT read for the NEXT fetch PC so the prediction arrives
      -- exactly one cycle later when IF needs it.
      bhtRdAddr :: Unsigned 6
      bhtRdAddr = bhtIdx (rvPC cpu2)

      -- CP5: BHT write command (from EX stage)
      bhtWrCmdOut :: Maybe (Unsigned 6, BhtCounter)
      bhtWrCmdOut = bhtWrCmd

      sys' = SysStateRV top3 cpu2 mc2 fifo4 rx' tx'

   in ( sys'
      , ( (txPin tx', led)
        , instrRdAddr, instrWrCmd
        , dataRdAddr,  dataWrCmd
        , regRdAddrA,  regRdAddrB, regWrCmdOut
        , bhtRdAddr,   bhtWrCmdOut
        )
      )

-- ===========================================================================
-- Top entity
-- ===========================================================================

{-# ANN topEntityRV
  ( Synthesize
      { t_name   = "top"
      , t_inputs = [PortName "clk", PortName "uart_rx_pin"]
      , t_output = PortProduct "" [PortName "uart_tx_pin", PortName "led"]
      }
  )
  #-}
topEntityRV :: Clock Dom80 -> Signal Dom80 Bit -> Signal Dom80 (Bit, BitVector 6)
topEntityRV clk rxPin =
  withClockResetEnable clk resetGen enableGen $
    let fullOut = mealy sysStepRV initSysStateRV
                    (bundle (rxPin, instrBramOut, dataBramOut, regBramOutA, regBramOutB, bhtRd))

        -- Instruction BRAM -----------------------------------------------
        instrRdAddr = (\(_, a, _, _, _, _, _, _, _, _) -> a) <$> fullOut
        instrWrCmd  = (\(_, _, w, _, _, _, _, _, _, _) -> w) <$> fullOut
        instrBramOut = blockRam
          (repeat (0x00000013 :: Word32) :: Vec 1024 Word32)
          instrRdAddr instrWrCmd

        -- Data BRAM -------------------------------------------------------
        dataRdAddr  = (\(_, _, _, a, _, _, _, _, _, _) -> a) <$> fullOut
        dataWrCmdS  = (\(_, _, _, _, w, _, _, _, _, _) -> w) <$> fullOut
        dataBramRaw = blockRam
          (repeat (0 :: Word32) :: Vec 1024 Word32)
          dataRdAddr dataWrCmdS
        prevDataWrCmd  = register Nothing dataWrCmdS
        prevDataRdAddr = register 0 dataRdAddr
        dataBramOut = wfBypass <$> prevDataWrCmd <*> prevDataRdAddr <*> dataBramRaw

        -- Register file BRAM (CP4) ----------------------------------------
        -- 32 × 32-bit synchronous blockRam.
        -- Read ports A and B are driven by the rs1/rs2 of the IF/ID instr;
        -- the write port is driven by regWrCmdOut from WB.
        regRdAddrA  = (\(_, _, _, _, _, a, _, _, _, _) -> a) <$> fullOut
        regRdAddrB  = (\(_, _, _, _, _, _, b, _, _, _) -> b) <$> fullOut
        regWrCmdS   = (\(_, _, _, _, _, _, _, w, _, _) -> w) <$> fullOut

        -- We split the dual read into two separate blockRam calls sharing the
        -- same write port.  Clash will merge them into a true-dual-port BRAM.
        regBramRawA = blockRam
          (repeat (0 :: Word32) :: Vec 32 Word32)
          regRdAddrA regWrCmdS
        regBramRawB = blockRam
          (repeat (0 :: Word32) :: Vec 32 Word32)
          regRdAddrB regWrCmdS

        -- Write-first bypass for both ports.
        prevRegWrCmd   = register Nothing regWrCmdS
        prevRegRdAddrA = register 0 regRdAddrA
        prevRegRdAddrB = register 0 regRdAddrB
        regBramOutA = wfBypassReg <$> prevRegWrCmd <*> prevRegRdAddrA <*> regBramRawA
        regBramOutB = wfBypassReg <$> prevRegWrCmd <*> prevRegRdAddrB <*> regBramRawB

        -- BHT blockRam (CP5) -----------------------------------------------
        -- 64 × 2-bit synchronous blockRam.
        -- Read address = bhtIdx(cpu.rvPC); result arrives one cycle later,
        -- replacing the in-state Vec 64 BhtCounter and its expensive mux trees.
        bhtRdAddrS  = (\(_, _, _, _, _, _, _, _, a, _) -> a) <$> fullOut
        bhtWrCmdS   = (\(_, _, _, _, _, _, _, _, _, w) -> w) <$> fullOut
        bhtBramRaw  = blockRam
          (repeat WNT :: Vec 64 BhtCounter)
          bhtRdAddrS bhtWrCmdS

        -- Write-first bypass for BHT.
        prevBhtWrCmd  = register Nothing bhtWrCmdS
        prevBhtRdAddr = register 0 bhtRdAddrS
        bhtRd = wfBypassBht <$> prevBhtWrCmd <*> prevBhtRdAddr <*> bhtBramRaw

     in (\(x, _, _, _, _, _, _, _, _, _) -> x) <$> fullOut

  where
    -- Write-first bypass for data BRAM (word-addressed, Unsigned 10)
    wfBypass (Just (wa, wd)) pra _ | wa == pra = wd
    wfBypass _               _   raw           = raw

    -- Write-first bypass for register BRAM (Unsigned 5)
    wfBypassReg (Just (wa, wd)) pra _ | wa == pra = wd
    wfBypassReg _               _   raw           = raw

    -- Write-first bypass for BHT (Unsigned 6)
    wfBypassBht (Just (wa, wd)) pra _ | wa == pra = wd
    wfBypassBht _               _   raw           = raw
