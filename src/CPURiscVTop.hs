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

-- | Tang Nano 20K top-level for CPURiscV.
--
-- ── Address map ───────────────────────────────────────────────────────────
--   0x0000_0000 – 0x0000_0FFF  Block RAM  (4 KiB)
--   0x0001_0000                UART_TX    (SB: write byte → serial out)
--   0x0001_0004                UART_STATUS (bit 0 = tx_ready, always 1)
--
-- ── UART programming protocol (same as Processor.hs top) ─────────────────
--   Idle:    waiting for command byte
--   0x50 'P' Load mode: receive 4096 bytes = 1024 × 32-bit big-endian words
--            into instruction BRAM.  FPGA replies 0x4B 'K' when done.
--   0x52 'R' Run: release CPU reset, stream UART_TX bytes to host.
--            When CPU writes to UART_TX the byte is forwarded to the host.
--            After CPU halts (or timeout) FPGA sends 0x44 'D'.
--   0x58 'X' Reset CPU (stays in Idle).  FPGA replies 0x4B 'K'.
--
-- ── Pins (Tang Nano 20K) ──────────────────────────────────────────────────
--   clk          pin 4   – 27 MHz oscillator
--   uart_rx_pin  pin 70  – host → FPGA
--   uart_tx_pin  pin 69  – FPGA → host
--   led[5:0]     pins 15-20 – last byte sent to UART_TX (active-low)
module CPURiscVTop where

import CPURiscV
import Clash.Annotations.TopEntity ()
import Clash.Prelude hiding (take)
import qualified Prelude as P

-- ===========================================================================
-- Clock domain – Tang Nano 20K has a 27 MHz oscillator
-- ===========================================================================

createDomain vSystem {vName = "Dom27", vPeriod = 37037}

type ClkFreqRV = 27_000_000

type BaudRateRV = 115_200

type ClksPerBitRV = Div ClkFreqRV BaudRateRV

type HalfBitRV = Div ClksPerBitRV 2

-- ===========================================================================
-- UART RX (identical to Processor.hs)
-- ===========================================================================

data RxFSM = RxIdle | RxStart | RxData | RxStop deriving (Generic, NFDataX, Show, Eq)

data UartRxSt = UartRxSt {rxFSM :: RxFSM, rxCnt :: Unsigned 9, rxBitIdx :: Unsigned 3, rxShift :: BitVector 8, rxSync1 :: Bit, rxSync2 :: Bit, rxData :: BitVector 8, rxValid :: Bool} deriving (Generic, NFDataX, Show)

initRxSt :: UartRxSt
initRxSt = UartRxSt RxIdle 0 0 0 1 1 0 False

uartRxStep :: UartRxSt -> Bit -> (UartRxSt, (BitVector 8, Bool))
uartRxStep s@UartRxSt {..} rxPin =
  let sampled = rxSync2
      clksPB = snatToNum (SNat :: SNat ClksPerBitRV) :: Unsigned 9
      half = snatToNum (SNat :: SNat HalfBitRV) :: Unsigned 9
      (fsm', cnt', bitIdx', shift', valid') = case rxFSM of
        RxIdle -> if sampled == 0 then (RxStart, 0, 0, rxShift, False) else (RxIdle, 0, 0, rxShift, False)
        RxStart -> if rxCnt == half - 1 then if sampled == 0 then (RxData, 0, 0, rxShift, False) else (RxIdle, 0, 0, rxShift, False) else (RxStart, rxCnt + 1, rxBitIdx, rxShift, False)
        RxData ->
          if rxCnt == clksPB - 1
            then
              let shifted = pack sampled ++# slice d7 d1 rxShift in if rxBitIdx == 7 then (RxStop, 0, rxBitIdx, shifted, False) else (RxData, 0, rxBitIdx + 1, shifted, False)
            else (RxData, rxCnt + 1, rxBitIdx, rxShift, False)
        RxStop -> if rxCnt == clksPB - 1 then (RxIdle, 0, 0, rxShift, sampled == 1) else (RxStop, rxCnt + 1, rxBitIdx, rxShift, False)
      dataOut = if valid' then shift' else rxData
      s' = s {rxFSM = fsm', rxCnt = cnt', rxBitIdx = bitIdx', rxShift = shift', rxSync1 = rxPin, rxSync2 = rxSync1, rxData = dataOut, rxValid = valid'}
   in (s', (dataOut, valid'))

-- ===========================================================================
-- UART TX
-- ===========================================================================

data TxFSM = TxIdle | TxStart | TxData | TxStop deriving (Generic, NFDataX, Show, Eq)

data UartTxSt = UartTxSt {txFSM :: TxFSM, txCnt :: Unsigned 9, txBitIdx :: Unsigned 3, txShift :: BitVector 8, txPin :: Bit} deriving (Generic, NFDataX, Show)

initTxSt :: UartTxSt
initTxSt = UartTxSt TxIdle 0 0 0 1

uartTxStep :: UartTxSt -> (BitVector 8, Bool) -> (UartTxSt, (Bit, Bool))
uartTxStep s@UartTxSt {..} (dat, send) =
  let clksPB = snatToNum (SNat :: SNat ClksPerBitRV) :: Unsigned 9
      busy = txFSM /= TxIdle
      (fsm', cnt', bitIdx', shift', pin') = case txFSM of
        TxIdle -> if send && not busy then (TxStart, 0, 0, dat, 1) else (TxIdle, txCnt, txBitIdx, txShift, 1)
        TxStart -> if txCnt == clksPB - 1 then (TxData, 0, 0, txShift, 0) else (TxStart, txCnt + 1, txBitIdx, txShift, 0)
        TxData ->
          let bit0 = lsb txShift
              shifted = 0 ++# slice d7 d1 txShift
           in if txCnt == clksPB - 1
                then if txBitIdx == 7 then (TxStop, 0, txBitIdx, shifted, bit0) else (TxData, 0, txBitIdx + 1, shifted, bit0)
                else (TxData, txCnt + 1, txBitIdx, txShift, bit0)
        TxStop -> if txCnt == clksPB - 1 then (TxIdle, 0, txBitIdx, txShift, 1) else (TxStop, txCnt + 1, txBitIdx, txShift, 1)
      s' = s {txFSM = fsm', txCnt = cnt', txBitIdx = bitIdx', txShift = shift', txPin = pin'}
   in (s', (pin', busy))

-- ===========================================================================
-- 16-deep TX FIFO
-- ===========================================================================

data FifoSt = FifoSt {fifoBuf :: Vec 16 (BitVector 8), fifoWr :: Unsigned 4, fifoRd :: Unsigned 4} deriving (Generic, NFDataX, Show)

initFifoSt :: FifoSt
initFifoSt = FifoSt (repeat 0) 0 0

fifoEmpty :: FifoSt -> Bool
fifoEmpty FifoSt {..} = fifoWr == fifoRd

fifoFull :: FifoSt -> Bool
fifoFull FifoSt {..} = (fifoWr + 1) == fifoRd

fifoPush :: FifoSt -> BitVector 8 -> FifoSt
fifoPush st@FifoSt {..} b = st {fifoBuf = replace fifoWr b fifoBuf, fifoWr = fifoWr + 1}

fifoPop :: FifoSt -> (FifoSt, BitVector 8)
fifoPop st@FifoSt {..} = (st {fifoRd = fifoRd + 1}, fifoBuf !! fifoRd)

-- ===========================================================================
-- Control FSM
-- ===========================================================================

data ProgFSM = PgIdle | PgLoad | PgRun | PgSendDone deriving (Generic, NFDataX, Show, Eq)

-- progByteIdx: 0..4095, progWordAcc: accumulates 4 bytes into one word
data TopStateRV = TopStateRV {ctrlFSM :: ProgFSM, progByteIdx :: Unsigned 12, progWordAcc :: BitVector 32, cpuRst :: Bool, ledLatch :: BitVector 8, outPending :: Bool, outPendDat :: BitVector 8, runTimeout :: Unsigned 16} deriving (Generic, NFDataX, Show)

initTopStateRV :: TopStateRV
initTopStateRV = TopStateRV {ctrlFSM = PgIdle, progByteIdx = 0, progWordAcc = 0, cpuRst = True, ledLatch = 0xFF, outPending = False, outPendDat = 0, runTimeout = 0}

-- ===========================================================================
-- Full system state
-- ===========================================================================

-- sMC: data RAM + UART peripheral state for CPU
data SysStateRV = SysStateRV {sTop :: TopStateRV, sCpu :: CpuStateRV, sMC :: MemCtrl, sFifo :: FifoSt, sRx :: UartRxSt, sTx :: UartTxSt} deriving (Generic, NFDataX, Show)

initSysStateRV = SysStateRV {sTop = initTopStateRV, sCpu = initCpuStateRV, sMC = initMemCtrl, sFifo = initFifoSt, sRx = initRxSt, sTx = initTxSt}

-- ===========================================================================
-- System step (Mealy body)
-- ===========================================================================

-- Outputs: ((txPin, led[5:0]), instrBramRdAddr, instrBramWrCmd)
type SysOut = ((Bit, BitVector 6), Unsigned 10, Maybe (Unsigned 10, Word32))

-- (Bit, Word32): (rxPin, instrBRAM read output)
sysStepRV :: SysStateRV -> (Bit, Word32) -> (SysStateRV, SysOut)
sysStepRV SysStateRV {..} (rxPin, bramOut) =
  let top = sTop
      cpu0 = sCpu
      mc0 = sMC
      fifo = sFifo
      tx = sTx

      -- ── UART RX ─────────────────────────────────────────────────────
      (rx', (rxByte, rxVld)) = uartRxStep sRx rxPin

      -- ── CPU enable: running and no pending TX byte ───────────────────
      cpuEn = ctrlFSM top == PgRun && not (outPending top)

      -- ── CPU step (or reset) ──────────────────────────────────────────
      (cpu1, mc1) =
        if cpuRst top
          then (initCpuStateRV, mc0)
          else let (cpu', mc', _pc, _rd, _val, _wbEn) = stepCpuRV cpu0 (bramOut, mc0, cpuEn) in (cpu', mc')

      -- Drain the UART_TX byte that the CPU may have written this cycle
      (mc2, cpuTxMaybe) = uartPop mc1

      -- ── UART TX path ────────────────────────────────────────────────
      txBusy = txFSM tx /= TxIdle
      (fifo1, txDat, txSend) = if not (fifoEmpty fifo) && not txBusy then let (f', b) = fifoPop fifo in (f', b, True) else (fifo, 0, False)
      (tx', _) = uartTxStep tx (txDat, txSend)

      -- ── Drain outPending into TX FIFO ───────────────────────────────
      (fifo2, top1) = if outPending top && not (fifoFull fifo1) then (fifoPush fifo1 (outPendDat top), top {outPending = False}) else (fifo1, top)

      -- ── Control FSM ─────────────────────────────────────────────────
      (top2, fifo3, cpu2, instrWrCmd) = case ctrlFSM top1 of
        PgIdle ->
          let top'
                | not rxVld = top1 {cpuRst = True}
                | rxByte == 0x50 = top1 {ctrlFSM = PgLoad, progByteIdx = 0, progWordAcc = 0, cpuRst = True}
                | rxByte == 0x52 = top1 {ctrlFSM = PgRun, cpuRst = False, runTimeout = 0}
                | rxByte == 0x58 = top1 {cpuRst = True}
                | otherwise = top1 {cpuRst = True}
           in (top', fifo2, cpu1, Nothing)
        PgLoad ->
          if rxVld
            then
              let bytePos = progByteIdx top1 `mod` 4
                  shiftAmt = (3 - resize bytePos) * 8 :: Unsigned 5
                  newByte = resize (unpack rxByte :: Unsigned 8) :: Unsigned 32
                  wordAcc' = progWordAcc top1 .|. pack (newByte `shiftL` fromIntegral shiftAmt)
                  wordIdx = truncateB (progByteIdx top1 `div` 4) :: Unsigned 10
                  wacc'' = if bytePos == 3 then 0 else wordAcc'
                  pgWr = if bytePos == 3 then Just (wordIdx, wordAcc') else Nothing
                  byteIdx' = progByteIdx top1 + 1
                  done = progByteIdx top1 == 4095
               in if done
                    then
                      let f' = if not (fifoFull fifo2) then fifoPush fifo2 0x4B else fifo2 in (top1 {ctrlFSM = PgIdle, progByteIdx = 0, progWordAcc = 0}, f', cpu1, pgWr)
                    else (top1 {progByteIdx = byteIdx', progWordAcc = wacc''}, fifo2, cpu1, pgWr)
            else (top1, fifo2, cpu1, Nothing)
        PgRun ->
          let top1' = top1 {cpuRst = False}
              -- Forward CPU UART_TX writes into host TX FIFO
              (fifo2', top2') = case cpuTxMaybe of
                Just b -> if not (fifoFull fifo2) then (fifoPush fifo2 b, top1' {runTimeout = 0}) else (fifo2, top1' {outPending = True, outPendDat = b, runTimeout = 0})
                Nothing -> if cpuEn && not (outPending top1') then (fifo2, top1' {runTimeout = runTimeout top1' + 1}) else (fifo2, top1')
              -- Check halt / timeout
              cpuHalted = rvHalt cpu1
              top2'' = if (cpuHalted || runTimeout top2' == 0xFFFF) && not (outPending top2') then top2' {ctrlFSM = PgSendDone} else top2'
           in (top2'', fifo2', cpu1, Nothing)
        PgSendDone -> if not (fifoFull fifo2) then (top1 {ctrlFSM = PgIdle}, fifoPush fifo2 0x44, cpu1, Nothing) else (top1, fifo2, cpu1, Nothing)

      -- ── Post-cycle: enqueue 'K' ack on reset command ─────────────────
      fifo4 = if rxVld && rxByte == 0x58 && ctrlFSM top == PgIdle && not (fifoFull fifo3) then fifoPush fifo3 0x4B else fifo3

      -- ── LED: show low 6 bits of last CPU→UART byte ───────────────────
      ledLatch' = case cpuTxMaybe of
        Just b -> b
        Nothing -> ledLatch top
      top3 = top2 {ledLatch = ledLatch'}
      led = complement (slice d5 d0 ledLatch')

      -- ── Instruction BRAM word address for next fetch ─────────────────
      instrRdAddr = truncateB (rvPC cpu2 `shiftR` 2) :: Unsigned 10

      sys' = SysStateRV top3 cpu2 mc2 fifo4 rx' tx'
   in (sys', ((txPin tx', led), instrRdAddr, instrWrCmd))

-- ===========================================================================
-- Top entity
-- ===========================================================================

{-# ANN
  topEntityRV
  ( Synthesize {t_name = "top", t_inputs = [PortName "clk", PortName "uart_rx_pin"], t_output = PortProduct "" [PortName "uart_tx_pin", PortName "led"]}
  )
  #-}
-- Signal Dom27 Bit = uart_rx_pin, Signal Dom27 (Bit, BitVector 6) = (uart_tx_pin, led[5:0])
topEntityRV :: Clock Dom27 -> Signal Dom27 Bit -> Signal Dom27 (Bit, BitVector 6)
topEntityRV clk rxPin =
  withClockResetEnable clk resetGen enableGen
    $ let fullOut = mealy sysStepRV initSysStateRV (bundle (rxPin, bramOut))
          rdAddr = (\(_, a, _) -> a) <$> fullOut
          wrCmd = (\(_, _, w) -> w) <$> fullOut
          -- Instruction block-RAM: initialised to all-NOP (ADDI x0,x0,0 = 0x0000_0013).
          -- Replace with blockRamFile "riscv-prog.hex" once you have a .hex file.
          bramOut = blockRam (repeat (0x00000013 :: Word32) :: Vec 1024 Word32) rdAddr wrCmd
       in (\(x, _, _) -> x) <$> fullOut
