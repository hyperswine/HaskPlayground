{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}

module MyHardware where

import Clash.Annotations.TopEntity ()
import Clash.Prelude hiding (take)
import Data.Proxy (Proxy (..))
import qualified Prelude as P

-- Numeric underscores and datakinds through Nat 27m
type ClkFreq = 27_000_000

type BaudRate = 115_200

-- Static division, 234 clocks per bit
type ClksPerBit = Div ClkFreq BaudRate

type HalfBit = Div ClksPerBit 2

type Instruction = BitVector 8

type ROM = Vec 16 Instruction

data RxFSM = RxIdle | RxStart | RxData | RxStop deriving (Generic, NFDataX, Show, Eq)

-- Baud counter + 2-stage metastability synchroniser + one-cycle pulse when byte ready
data UartRxState = UartRxState {rxFSM :: RxFSM, rxCnt :: Unsigned 9, rxBitIdx :: Unsigned 3, rxShift :: BitVector 8, rxSync1 :: Bit, rxSync2 :: Bit, rxData :: BitVector 8, rxValid :: Bool} deriving (Generic, NFDataX, Show)

initRxState = UartRxState RxIdle 0 0 0 1 1 0 False

-- Very similar to Rx, cause they have to be symmetric
data TxFSM = TxIdle | TxStart | TxData | TxStop deriving (Generic, NFDataX, Show, Eq)

-- NEED TO TRACK current TX line level
data UartTxState = UartTxState {txFSM :: TxFSM, txCnt :: Unsigned 9, txBitIdx :: Unsigned 3, txShift :: BitVector 8, txPinLvl :: Bit} deriving (Generic, NFDataX, Show)

initTxState = UartTxState {txFSM = TxIdle, txCnt = 0, txBitIdx = 0, txShift = 0, txPinLvl = 1}

data FifoState = FifoState {fifoBuf :: Vec 16 (BitVector 8), fifoWr :: Unsigned 4, fifoRd :: Unsigned 4} deriving (Generic, NFDataX, Show)

-- Repeat is a neat trick to have just all xs for the entire bitvec
initFifoState = FifoState (repeat 0) 0 0

-- NFDATAX is just the exception, good for simulation
data ExPhase = ExPhase0 | ExPhase1 deriving (Generic, NFDataX, Show, Eq)

-- INCLUDES: IF/ID pre-ALU buffer + EX pipeline register + ACC + OUTPUT
data CpuState = CpuState {cpuACC :: Unsigned 8, cpuPC :: Unsigned 4, cpuHalt :: Bool, idValid :: Bool, idOpcode :: BitVector 4, idImm8 :: Unsigned 8, idDest :: Unsigned 4, exValid :: Bool, exOpcode :: BitVector 4, exDest :: Unsigned 4, exACC :: Unsigned 8, exPhase :: ExPhase, exPartial :: Unsigned 8, cpuOutValid :: Bool, cpuOutData :: Unsigned 8} deriving (Generic, NFDataX, Show)

initCpuState = CpuState 0 0 False False 0 0 0 False 0 0 0 ExPhase0 0 False 0

data CtrlFSM = CtIdle | CtProgLoad | CtRun | CtSendDone deriving (Generic, NFDataX, Show, Eq)

-- pending OUT: captured when FIFO was full; stalls CPU until committed
data TopState = TopState {ctrlFSM :: CtrlFSM, progIdx :: Unsigned 4, runTimeout :: Unsigned 8, progROM :: ROM, cpuRst :: Bool, ledLatch :: BitVector 8, outPending :: Bool, outPendingData :: BitVector 8} deriving (Generic, NFDataX, Show)

initTopState = TopState {ctrlFSM = CtIdle, progIdx = 0, runTimeout = 0, progROM = testProgram, cpuRst = True, ledLatch = 0xFF, outPending = False, outPendingData = 0}

mkInstr :: BitVector 4 -> BitVector 4 -> Instruction
mkInstr op operand = op ++# operand

testProgram :: ROM
testProgram = mkInstr 0b0001 5 :> mkInstr 0b0010 3 :> mkInstr 0b0110 0 :> mkInstr 0b0001 15 :> mkInstr 0b0011 7 :> mkInstr 0b0110 0 :> mkInstr 0b1001 0 :> mkInstr 0b0100 0b1111 :> mkInstr 0b0101 0b0001 :> mkInstr 0b0110 0 :> mkInstr 0b0111 0 :> mkInstr 0b0000 0 :> mkInstr 0b0000 0 :> mkInstr 0b0000 0 :> mkInstr 0b0000 0 :> mkInstr 0b0000 0 :> Nil

-- Handles glitches. If RxIdle at RxStart then glitch. LSB first for receiving
uartRxT s@UartRxState {..} rxPin = (s', (dataOut, valid'))
  where
    sync1 = rxPin
    sync2 = rxSync1
    sampled = rxSync2
    clksPB = fromIntegral (natVal (Proxy :: Proxy ClksPerBit)) :: Unsigned 9
    half = fromIntegral (natVal (Proxy :: Proxy HalfBit)) :: Unsigned 9

    (fsm', cnt', bitIdx', shift', valid') = case rxFSM of
      RxIdle -> if sampled == 0 then (RxStart, 0, 0, rxShift, False) else (RxIdle, 0, 0, rxShift, False)
      RxStart -> if rxCnt == half - 1 then (if sampled == 0 then (RxData, 0, 0, rxShift, False) else (RxIdle, 0, 0, rxShift, False)) else (RxStart, rxCnt + 1, rxBitIdx, rxShift, False)
      RxData -> if rxCnt == clksPB - 1 then (let shifted = (pack sampled ++# slice d7 d1 rxShift) in if rxBitIdx == 7 then (RxStop, 0, rxBitIdx, shifted, False) else (RxData, 0, rxBitIdx + 1, shifted, False)) else (RxData, rxCnt + 1, rxBitIdx, rxShift, False)
      RxStop -> if rxCnt == clksPB - 1 then (RxIdle, 0, 0, rxShift, sampled == 1) else (RxStop, rxCnt + 1, rxBitIdx, rxShift, False)

    dataOut = if valid' then shift' else rxData

    -- update the UART Receiver state only
    s' = s {rxFSM = fsm', rxCnt = cnt', rxBitIdx = bitIdx', rxShift = shift', rxSync1 = sync1, rxSync2 = sync2, rxData = dataOut, rxValid = valid'}

uartRx :: (HiddenClockResetEnable dom) => Signal dom Bit -> Signal dom (BitVector 8, Bool)
uartRx = mealy uartRxT initRxState

uartTxT s@UartTxState {..} (dat, send) = (s', (pin', busy))
  where
    clksPB = fromIntegral (natVal (Proxy :: Proxy ClksPerBit)) :: Unsigned 9
    busy = txFSM /= TxIdle

    (fsm', cnt', bitIdx', shift', pin') = case txFSM of
      TxIdle -> if send && not busy then (TxStart, 0, 0, dat, 1) else (TxIdle, txCnt, txBitIdx, txShift, 1)
      TxStart -> if txCnt == clksPB - 1 then (TxData, 0, 0, txShift, 0) else (TxStart, txCnt + 1, txBitIdx, txShift, 0)
      TxData -> let bit0 = lsb txShift; shifted = 0 ++# slice d7 d1 txShift in if txCnt == clksPB - 1 then if txBitIdx == 7 then (TxStop, 0, txBitIdx, shifted, bit0) else (TxData, 0, txBitIdx + 1, shifted, bit0) else (TxData, txCnt + 1, txBitIdx, txShift, bit0)
      TxStop -> if txCnt == clksPB - 1 then (TxIdle, 0, txBitIdx, txShift, 1) else (TxStop, txCnt + 1, txBitIdx, txShift, 1)

    s' = s {txFSM = fsm', txCnt = cnt', txBitIdx = bitIdx', txShift = shift', txPinLvl = pin'}

uartTx :: (HiddenClockResetEnable dom) => Signal dom (BitVector 8, Bool) -> Signal dom (Bit, Bool)
uartTx = mealy uartTxT initTxState

-- JUST THE PROCESSOR ITSELF (not the UART connected)
stepCpuCore :: CpuState -> (Instruction, Bool) -> (CpuState, (Unsigned 4, Bool, Unsigned 8, Bool))
stepCpuCore s@CpuState {..} (romData, en) | cpuHalt = (s {cpuOutValid = False}, (cpuPC, False, 0, True))
stepCpuCore s@CpuState {..} (romData, en) | not en = (s {cpuOutValid = False}, (cpuPC, False, 0, False))
stepCpuCore s@CpuState {..} (romData, en) = (s', (pc2, outValid1, outData1, halt1))
  where
    -- ESSENTIALLY A COMBINATORIAL DECODER THATS fused with execution
    fOpcode = slice d7 d4 romData
    fImm8 = resize (unpack (slice d3 d0 romData) :: Unsigned 4) :: Unsigned 8
    fDest = unpack (slice d3 d0 romData) :: Unsigned 4
    exPickup = not exValid && idValid
    exWriteback = exValid && exPhase == ExPhase1
    idCanFetch = (not idValid || exPickup) && not exWriteback && not cpuHalt

    (acc1, pc1, halt1, idValid1, exValid1, exOpcode1, exDest1, exACC1, exPhase1, exPartial1, outValid1, outData1) =
      if exPickup
        then
          -- ESSENTIALLY THE DECODER FOR MAIN OPS THAT TELLS US WHAT OPCODES and ACC to perform in the fused dec-ex
          let partial = case idOpcode of
                0x1 -> idImm8
                0x2 -> cpuACC + idImm8
                0x3 -> cpuACC - idImm8
                0x4 -> cpuACC .&. idImm8
                0x5 -> cpuACC .|. idImm8
                _ -> cpuACC
           in (cpuACC, cpuPC, cpuHalt, False, True, idOpcode, idDest, cpuACC, ExPhase1, partial, False, 0)
        else
          if exWriteback
            then
              let (acc', pc', halt', idFlush, ov, od) = case exOpcode of
                    0x0 -> (cpuACC, cpuPC, False, False, False, 0)
                    0x1 -> (exPartial, cpuPC, False, False, False, 0)
                    0x2 -> (exPartial, cpuPC, False, False, False, 0)
                    0x3 -> (exPartial, cpuPC, False, False, False, 0)
                    0x4 -> (exPartial, cpuPC, False, False, False, 0)
                    0x5 -> (exPartial, cpuPC, False, False, False, 0)
                    0x6 -> (cpuACC, cpuPC, False, False, True, exACC)
                    0x7 -> (cpuACC, cpuPC, True, True, False, 0)
                    0x8 -> (cpuACC, exDest, False, True, False, 0)
                    0x9 -> if exACC == 0 then (cpuACC, exDest, False, True, False, 0) else (cpuACC, cpuPC, False, False, False, 0)
                    _ -> (cpuACC, cpuPC, False, False, False, 0)
               in (acc', pc', halt', idFlush || idValid, False, 0, 0, exACC, ExPhase0, exPartial, ov, od)
            else (cpuACC, cpuPC, cpuHalt, idValid, exValid, exOpcode, exDest, exACC, exPhase, exPartial, False, 0)

    (idValid2, idOpcode2, idImm82, idDest2, pc2) = if idCanFetch then (True, fOpcode, fImm8, fDest, pc1 + 1) else (idValid1, idOpcode, idImm8, idDest, pc1)

    idFlushNow = exWriteback && (exOpcode == 0x7 || exOpcode == 0x8 || (exOpcode == 0x9 && exACC == 0))

    idValidFinal = if idFlushNow then False else idValid2

    s' = s {cpuACC = acc1, cpuPC = pc2, cpuHalt = halt1, idValid = idValidFinal, idOpcode = idOpcode2, idImm8 = idImm82, idDest = idDest2, exValid = exValid1, exOpcode = exOpcode1, exDest = exDest1, exACC = exACC1, exPhase = exPhase1, exPartial = exPartial1, cpuOutValid = outValid1, cpuOutData = outData1}

-- FIFO based CIRCULAR BUFFER FUNCTIONALITIES. Notice !!, + and such are all using Clash's implementations

fifoEmpty FifoState {..} = fifoWr == fifoRd

fifoFull FifoState {..} = fifoWr + 1 == fifoRd

fifoPush st@FifoState {..} b = st {fifoBuf = replace fifoWr b fifoBuf, fifoWr = fifoWr + 1}

fifoPop st@FifoState {..} = (st {fifoRd = fifoRd + 1}, fifoBuf !! fifoRd)

-- ACTUAL SYSTEM

data SysState = SysState {sTop :: TopState, sCpu :: CpuState, sFifo :: FifoState, sRx :: UartRxState, sTx :: UartTxState} deriving (Generic, NFDataX, Show)

initSysState = SysState initTopState initCpuState initFifoState initRxState initTxState

-- BASED ON THE NEXT BIT, do something
sysStep :: SysState -> Bit -> (SysState, (Bit, BitVector 6))
sysStep SysState {..} rxPin = (sys', (txPinOut, led))
  where
    -- Just so we can refer to them locally in a intent perserving way
    top = sTop
    cpu0 = sCpu
    fifo = sFifo
    tx = sTx
    rx = sRx

    (rx', (rxByte, rxVld)) = uartRxT rx rxPin

    cpuEn = ctrlFSM top == CtRun && not (outPending top)

    (cpu1, (_cpuAddr, cpuOV, cpuOD, cpuHalted)) = if cpuRst top then (initCpuState, (0, False, 0, False)) else let cpuAddr0 = cpuPC cpu0; romWord = progROM top !! cpuAddr0 in stepCpuCore cpu0 (romWord, cpuEn)

    txBusy = txFSM tx /= TxIdle

    (fifo1, txDat, txSend) = if not (fifoEmpty fifo) && not txBusy then let (f', b) = fifoPop fifo in (f', b, True) else (fifo, 0, False)

    (tx', _) = uartTxT tx (txDat, txSend)

    (fifo2, top1) = if outPending top && not (fifoFull fifo1) then (fifoPush fifo1 (outPendingData top), top {outPending = False}) else (fifo1, top)

    (top2, fifo3, cpu2) = case ctrlFSM top1 of
      CtIdle -> (topIdle, fifo2, cpu1)
        where
          topIdle
            | not rxVld = top1 {cpuRst = True}
            | rxByte == 0x50 = top1 {ctrlFSM = CtProgLoad, progIdx = 0, cpuRst = True}
            | rxByte == 0x52 = top1 {ctrlFSM = CtRun, cpuRst = False, runTimeout = 0}
            -- 'X': soft-reset, reply 'K' below
            | rxByte == 0x58 = top1 {cpuRst = True}
            | otherwise = top1 {cpuRst = True}
      CtProgLoad -> if rxVld then let rom' = replace (progIdx top1) rxByte (progROM top1) in if progIdx top1 == 15 then let f' = if not (fifoFull fifo2) then fifoPush fifo2 0x4B else fifo2 in (top1 {progROM = rom', ctrlFSM = CtIdle}, f', cpu1) else (top1 {progROM = rom', progIdx = progIdx top1 + 1}, fifo2, cpu1) else (top1, fifo2, cpu1)
      -- Queue OUT byte (or set pending if FIFO full)
      CtRun ->
        let top1' = top1 {cpuRst = False}
            (fifo2', top2') = if cpuOV then if not (fifoFull fifo2) then (fifoPush fifo2 (pack cpuOD), top1' {runTimeout = 0}) else (fifo2, top1' {outPending = True, outPendingData = pack cpuOD, runTimeout = 0}) else if cpuEn && not (outPending top1') then (fifo2, top1' {runTimeout = runTimeout top1' + 1}) else (fifo2, top1')
            top2'' = if (cpuHalted || runTimeout top2' == 0xFF) && not (outPending top2') then top2' {ctrlFSM = CtSendDone} else top2'
         in (top2'', fifo2', cpu1)
      CtSendDone -> if not (fifoFull fifo2) then (top1 {ctrlFSM = CtIdle}, fifoPush fifo2 0x44, cpu1) else (top1, fifo2, cpu1)

    -- 'X' command: queue 'K' reply
    fifo4 =
      if rxVld && rxByte == 0x58 && ctrlFSM top == CtIdle && not (fifoFull fifo3)
        then fifoPush fifo3 0x4B
        else fifo3

    -- LED latch for displaying output
    ledLatch' = if cpuOV then pack cpuOD else ledLatch top
    top3 = top2 {ledLatch = ledLatch'}
    txPinOut = txPinLvl tx'
    led = complement (slice d5 d0 ledLatch')
    sys' = SysState top3 cpu2 fifo4 rx' tx'

topEntity clk = withClockResetEnable clk resetGen enableGen $ mealy sysStep initSysState

simulateSoC n = sampleN @System n (topEntity clockGen (pure 1))

simulateCpuCore n = P.take n $ go initCpuState
  where
    go s =
      let addr = cpuPC s
          instr = testProgram !! addr
          (s', out) = stepCpuCore s (instr, True)
       in out : go s'
