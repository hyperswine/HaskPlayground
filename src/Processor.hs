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

module Processor where

import Clash.Annotations.TopEntity ()
import Clash.Prelude hiding (take)
import qualified Data.Bits as Bits
import Numeric (showHex)
import qualified Prelude as P

createDomain vSystem {vName = "Dom27", vPeriod = 37037}

{-# ANN
  topEntity3
  (Synthesize {t_name = "top", t_inputs = [PortName "clk", PortName "uart_rx_pin"], t_output = PortProduct "" [PortName "uart_tx_pin", PortName "led"]})
  #-}

type ClkFreq3 = 27_000_000

type BaudRate3 = 115_200

type ClksPerBit3 = Div ClkFreq3 BaudRate3

type HalfBit3 = Div ClksPerBit3 2

type Instr32 = BitVector 32

type InstrMem = Vec 1024 Instr32

type RegFile = Vec 4 (Unsigned 32)

type PC = Unsigned 10

type RegIdx = Unsigned 2

data RxFSM3 = RxIdle3 | RxStart3 | RxData3 | RxStop3 deriving (Generic, NFDataX, Show, Eq)

data UartRxState3 = UartRxState3 {rxFSM3 :: RxFSM3, rxCnt3 :: Unsigned 9, rxBitIdx3 :: Unsigned 3, rxShift3 :: BitVector 8, rxSync13 :: Bit, rxSync23 :: Bit, rxData3 :: BitVector 8, rxValid3 :: Bool} deriving (Generic, NFDataX, Show)

initRxState3 = UartRxState3 RxIdle3 0 0 0 1 1 0 False

data TxFSM3 = TxIdle3 | TxStart3 | TxData3 | TxStop3 deriving (Generic, NFDataX, Show, Eq)

data UartTxState3 = UartTxState3 {txFSM3 :: TxFSM3, txCnt3 :: Unsigned 9, txBitIdx3 :: Unsigned 3, txShift3 :: BitVector 8, txPinLvl3 :: Bit} deriving (Generic, NFDataX, Show)

initTxState3 = UartTxState3 TxIdle3 0 0 0 1

data FifoState3 = FifoState3 {fifoBuf3 :: Vec 16 (BitVector 8), fifoWr3 :: Unsigned 4, fifoRd3 :: Unsigned 4} deriving (Generic, NFDataX, Show)

initFifoState3 = FifoState3 (repeat 0) 0 0

uartRxT3 s@UartRxState3 {..} rxPin =
  let sync1 = rxPin
      sync2 = rxSync13
      sampled = rxSync23
      clksPB = snatToNum (SNat :: SNat ClksPerBit3) :: Unsigned 9
      half = snatToNum (SNat :: SNat HalfBit3) :: Unsigned 9

      (fsm', cnt', bitIdx', shift', valid') = case rxFSM3 of
        RxIdle3 -> if sampled == 0 then (RxStart3, 0, 0, rxShift3, False) else (RxIdle3, 0, 0, rxShift3, False)
        RxStart3 -> if rxCnt3 == half - 1 then if sampled == 0 then (RxData3, 0, 0, rxShift3, False) else (RxIdle3, 0, 0, rxShift3, False) else (RxStart3, rxCnt3 + 1, rxBitIdx3, rxShift3, False)
        RxData3 -> if rxCnt3 == clksPB - 1 then let shifted = pack sampled ++# slice d7 d1 rxShift3 in if rxBitIdx3 == 7 then (RxStop3, 0, rxBitIdx3, shifted, False) else (RxData3, 0, rxBitIdx3 + 1, shifted, False) else (RxData3, rxCnt3 + 1, rxBitIdx3, rxShift3, False)
        RxStop3 -> if rxCnt3 == clksPB - 1 then (RxIdle3, 0, 0, rxShift3, sampled == 1) else (RxStop3, rxCnt3 + 1, rxBitIdx3, rxShift3, False)

      dataOut = if valid' then shift' else rxData3

      s' = s {rxFSM3 = fsm', rxCnt3 = cnt', rxBitIdx3 = bitIdx', rxShift3 = shift', rxSync13 = sync1, rxSync23 = sync2, rxData3 = dataOut, rxValid3 = valid'}
   in (s', (dataOut, valid'))

uartRx3 :: (HiddenClockResetEnable dom) => Signal dom Bit -> Signal dom (BitVector 8, Bool)
uartRx3 = mealy uartRxT3 initRxState3

uartTxT3 s@UartTxState3 {..} (dat, send) =
  let clksPB = snatToNum (SNat :: SNat ClksPerBit3) :: Unsigned 9
      busy = txFSM3 /= TxIdle3
      (fsm', cnt', bitIdx', shift', pin') = case txFSM3 of
        TxIdle3 -> if send && not busy then (TxStart3, 0, 0, dat, 1) else (TxIdle3, txCnt3, txBitIdx3, txShift3, 1)
        TxStart3 -> if txCnt3 == clksPB - 1 then (TxData3, 0, 0, txShift3, 0) else (TxStart3, txCnt3 + 1, txBitIdx3, txShift3, 0)
        TxData3 -> let bit0 = lsb txShift3; shifted = 0 ++# slice d7 d1 txShift3 in if txCnt3 == clksPB - 1 then if txBitIdx3 == 7 then (TxStop3, 0, txBitIdx3, shifted, bit0) else (TxData3, 0, txBitIdx3 + 1, shifted, bit0) else (TxData3, txCnt3 + 1, txBitIdx3, txShift3, bit0)
        TxStop3 -> if txCnt3 == clksPB - 1 then (TxIdle3, 0, txBitIdx3, txShift3, 1) else (TxStop3, txCnt3 + 1, txBitIdx3, txShift3, 1)

      s' = s {txFSM3 = fsm', txCnt3 = cnt', txBitIdx3 = bitIdx', txShift3 = shift', txPinLvl3 = pin'}
   in (s', (pin', busy))

uartTx3 :: (HiddenClockResetEnable dom) => Signal dom (BitVector 8, Bool) -> Signal dom (Bit, Bool)
uartTx3 = mealy uartTxT3 initTxState3

fifoEmpty3 FifoState3 {..} = fifoWr3 == fifoRd3

fifoFull3 FifoState3 {..} = (fifoWr3 + 1) == fifoRd3

fifoPush3 st@FifoState3 {..} b = st {fifoBuf3 = replace fifoWr3 b fifoBuf3, fifoWr3 = fifoWr3 + 1}

fifoPop3 st@FifoState3 {..} = (st {fifoRd3 = fifoRd3 + 1}, fifoBuf3 !! fifoRd3)

data IfIdReg = IfIdReg {ifidValid :: Bool, ifidPC :: PC, ifidInstr :: Instr32, ifidPredTaken :: Bool, ifidPredTarget :: PC} deriving (Generic, NFDataX, Show)

emptyIfId = IfIdReg False 0 0 False 0

data IdExReg = IdExReg {idexValid :: Bool, idexPC :: PC, idexOpcode :: BitVector 4, idexRd :: RegIdx, idexRsVal :: Unsigned 32, idexImm24 :: Unsigned 32, idexPredTaken :: Bool, idexPredTarget :: PC} deriving (Generic, NFDataX, Show)

emptyIdEx = IdExReg False 0 0 0 0 0 False 0

data CpuState3 = CpuState3 {cpu3PC :: PC, cpu3Regs :: RegFile, cpu3Halt :: Bool, cpu3IfId :: IfIdReg, cpu3IdEx :: IdExReg, cpu3OutValid :: Bool, cpu3OutData :: BitVector 8, cpu3BrActual :: Bool, cpu3BrTarget :: PC} deriving (Generic, NFDataX, Show)

initCpuState3 = CpuState3 {cpu3PC = 0, cpu3Regs = repeat 0, cpu3Halt = False, cpu3IfId = emptyIfId, cpu3IdEx = emptyIdEx, cpu3OutValid = False, cpu3OutData = 0, cpu3BrActual = False, cpu3BrTarget = 0}

branchPredict :: Instr32 -> PC -> (Bool, PC)
branchPredict _instr _fetchPC = (False, 0)

-- THE CPU STEPPER ONLY CONCERNS CPU, not uart or memory links
stepCpu3 :: CpuState3 -> (Instr32, Bool) -> (CpuState3, (PC, Bool, BitVector 8, Bool, Bool, PC))
stepCpu3 s@CpuState3 {..} (bramOut, en)
  -- Halted: freeze everything
  | cpu3Halt = (s {cpu3OutValid = False, cpu3BrActual = False}, (cpu3PC, False, 0, True, False, 0))
  -- Clock-enable low: freeze all state
  | not en = (s {cpu3OutValid = False, cpu3BrActual = False}, (cpu3PC, False, 0, False, False, 0))
  -- Main step: one clock cycle
  | otherwise =
      let idex = cpu3IdEx
          (acc_rd, _acc_pc, acc_halt, acc_ov, acc_od, brActual, brTarget, squash) =
            if idexValid idex
              then
                let op = idexOpcode idex
                    rd = idexRd idex
                    rsVal = idexRsVal idex
                    imm = idexImm24 idex
                    exPredTk = idexPredTaken idex
                    exPredTg = idexPredTarget idex

                    aluResult = case op of
                      0x1 -> imm
                      0x2 -> rsVal + imm
                      0x3 -> rsVal - imm
                      0x4 -> rsVal .&. imm
                      0x5 -> rsVal .|. imm
                      0x6 -> rsVal
                      _ -> rsVal

                    regs' = case op of
                      0x1 -> replace rd aluResult cpu3Regs
                      0x2 -> replace rd aluResult cpu3Regs
                      0x3 -> replace rd aluResult cpu3Regs
                      0x4 -> replace rd aluResult cpu3Regs
                      0x5 -> replace rd aluResult cpu3Regs
                      0x6 -> replace rd aluResult cpu3Regs
                      _ -> cpu3Regs

                    (brTk, brTg) = case op of
                      0x9 -> (True, truncateB imm)
                      0xA -> (rsVal == 0, if rsVal == 0 then truncateB imm else cpu3PC)
                      0xB -> (rsVal /= 0, if rsVal /= 0 then truncateB imm else cpu3PC)
                      _ -> (False, cpu3PC)

                    mispred = brTk /= exPredTk || (brTk && brTg /= exPredTg)
                    sq = mispred
                    (ov, od) = case op of 0x7 -> (True, slice d7 d0 (pack rsVal)); _ -> (False, 0)
                    halt' = op == 0x8
                    pc' = if brTk then brTg else cpu3PC
                 in (regs', pc', halt', ov, od, brTk, brTg, sq)
              else (cpu3Regs, cpu3PC, False, False, 0, False, 0, False)

          ifid = cpu3IfId
          idex' = if squash || not (ifidValid ifid) then emptyIdEx else let instr = ifidInstr ifid; opcodeF = slice d31 d28 instr; rdF = unpack (slice d27 d26 instr) :: RegIdx; rsF = unpack (slice d25 d24 instr) :: RegIdx; imm24F = resize (unpack (slice d23 d0 instr) :: Unsigned 24) :: Unsigned 32; rsRaw = cpu3Regs !! rsF; exWritesRd = idexValid idex && idexOpcode idex `elem` [0x1, 0x2, 0x3, 0x4, 0x5, 0x6]; exRd = idexRd idex; rsVal = if exWritesRd && exRd == rsF then cpu3Regs !! rsF else rsRaw in IdExReg {idexValid = True, idexPC = ifidPC ifid, idexOpcode = opcodeF, idexRd = rdF, idexRsVal = rsVal, idexImm24 = imm24F, idexPredTaken = ifidPredTaken ifid, idexPredTarget = ifidPredTarget ifid}
          idex'' = if idexValid idex' then let rsF' = unpack (slice d25 d24 (ifidInstr ifid)) :: RegIdx; exWritesRd = idexValid idex && idexOpcode idex `elem` [0x1, 0x2, 0x3, 0x4, 0x5, 0x6 :: BitVector 4]; forwardVal = if exWritesRd && idexRd idex == rsF' then acc_rd !! idexRd idex else acc_rd !! rsF' in idex' {idexRsVal = forwardVal} else idex'
          nextSeqPC = cpu3PC + 1
          (predTk, predTg) = branchPredict bramOut cpu3PC
          (nextPC, ifid') = if squash then (brTarget, emptyIfId) else if predTk then (predTg, IfIdReg {ifidValid = True, ifidPC = cpu3PC, ifidInstr = bramOut, ifidPredTaken = predTk, ifidPredTarget = predTg}) else (nextSeqPC, IfIdReg {ifidValid = True, ifidPC = cpu3PC, ifidInstr = bramOut, ifidPredTaken = False, ifidPredTarget = nextSeqPC})

          s' = s {cpu3PC = nextPC, cpu3Regs = acc_rd, cpu3Halt = acc_halt, cpu3IfId = ifid', cpu3IdEx = idex'', cpu3OutValid = acc_ov, cpu3OutData = acc_od, cpu3BrActual = brActual, cpu3BrTarget = brTarget}
       in (s', (nextPC, acc_ov, acc_od, acc_halt, brActual, brTarget))

data ProgFSM = PgIdle | PgLoad | PgRun | PgSendDone deriving (Generic, NFDataX, Show, Eq)

data TopState3 = TopState3 {ctrlFSM3 :: ProgFSM, progByteIdx :: Unsigned 12, progWordAcc :: BitVector 32, cpu3Rst :: Bool, led3Latch :: BitVector 8, out3Pending :: Bool, out3PendDat :: BitVector 8, runTimeout3 :: Unsigned 16} deriving (Generic, NFDataX, Show)

initTopState3 = TopState3 {ctrlFSM3 = PgIdle, progByteIdx = 0, progWordAcc = 0, cpu3Rst = True, led3Latch = 0xFF, out3Pending = False, out3PendDat = 0, runTimeout3 = 0}

data SysState3 = SysState3 {sTop3 :: TopState3, sCpu3 :: CpuState3, sFifo3 :: FifoState3, sRx3 :: UartRxState3, sTx3 :: UartTxState3} deriving (Generic, NFDataX, Show)

initSysState3 = SysState3 initTopState3 initCpuState3 initFifoState3 initRxState3 initTxState3

sysStep3 SysState3 {..} (rxPin, bramOut) =
  let top = sTop3
      cpu0 = sCpu3
      fifo = sFifo3
      tx = sTx3

      (rx', (rxByte, rxVld)) = uartRxT3 sRx3 rxPin
      cpuEn = ctrlFSM3 top == PgRun && not (out3Pending top)
      (cpu1, (_nextPC, cpuOV, cpuOD, cpuHalted, _brActual, _brTarget)) = if cpu3Rst top then (initCpuState3, (0, False, 0, False, False, 0)) else stepCpu3 cpu0 (bramOut, cpuEn)
      txBusy = txFSM3 tx /= TxIdle3
      (fifo1, txDat, txSend) = if not (fifoEmpty3 fifo) && not txBusy then let (f', b) = fifoPop3 fifo in (f', b, True) else (fifo, 0, False)
      (tx', _) = uartTxT3 tx (txDat, txSend)
      (fifo2, top1) = if out3Pending top && not (fifoFull3 fifo1) then (fifoPush3 fifo1 (out3PendDat top), top {out3Pending = False}) else (fifo1, top)

      (top2, fifo3, cpu2, bramWrCmd) = case ctrlFSM3 top1 of
        PgIdle -> let top' | not rxVld = top1 {cpu3Rst = True} | rxByte == 0x50 = top1 {ctrlFSM3 = PgLoad, progByteIdx = 0, progWordAcc = 0, cpu3Rst = True} | rxByte == 0x52 = top1 {ctrlFSM3 = PgRun, cpu3Rst = False, runTimeout3 = 0} | rxByte == 0x58 = top1 {cpu3Rst = True} | otherwise = top1 {cpu3Rst = True} in (top', fifo2, cpu1, Nothing)
        PgLoad -> if rxVld then let bytePos = progByteIdx top1 `mod` 4; shiftAmt = (3 - resize bytePos) * 8 :: Unsigned 5; newByte = resize (unpack rxByte :: Unsigned 8) :: Unsigned 32; wordAcc' = progWordAcc top1 .|. pack (newByte `shiftL` fromIntegral shiftAmt); wordIdx = truncateB (progByteIdx top1 `div` 4) :: Unsigned 10; wacc' = if bytePos == 3 then 0 else wordAcc'; pgWr = if bytePos == 3 then Just (wordIdx, wordAcc') else Nothing; byteIdx' = progByteIdx top1 + 1; done = progByteIdx top1 == 4095 in if done then let f' = if not (fifoFull3 fifo2) then fifoPush3 fifo2 0x4B else fifo2 in (top1 {ctrlFSM3 = PgIdle, progByteIdx = 0, progWordAcc = 0}, f', cpu1, pgWr) else (top1 {progByteIdx = byteIdx', progWordAcc = wacc'}, fifo2, cpu1, pgWr) else (top1, fifo2, cpu1, Nothing)
        PgRun -> let top1' = top1 {cpu3Rst = False}; (fifo2', top2') = if cpuOV then if not (fifoFull3 fifo2) then (fifoPush3 fifo2 cpuOD, top1' {runTimeout3 = 0}) else (fifo2, top1' {out3Pending = True, out3PendDat = cpuOD, runTimeout3 = 0}) else if cpuEn && not (out3Pending top1') then (fifo2, top1' {runTimeout3 = runTimeout3 top1' + 1}) else (fifo2, top1'); top2'' = if (cpuHalted || runTimeout3 top2' == 0xFFFF) && not (out3Pending top2') then top2' {ctrlFSM3 = PgSendDone} else top2' in (top2'', fifo2', cpu1, Nothing)
        PgSendDone -> if not (fifoFull3 fifo2) then (top1 {ctrlFSM3 = PgIdle}, fifoPush3 fifo2 0x44, cpu1, Nothing) else (top1, fifo2, cpu1, Nothing)

      fifo4 = if rxVld && rxByte == 0x58 && ctrlFSM3 top == PgIdle && not (fifoFull3 fifo3) then fifoPush3 fifo3 0x4B else fifo3
      led3Latch' = if cpuOV then cpuOD else led3Latch top
      top3 = top2 {led3Latch = led3Latch'}
      led = complement (slice d5 d0 led3Latch')
      bramRdAddr = cpu3PC cpu2
      sys' = SysState3 top3 cpu2 fifo4 rx' tx'
   in (sys', ((txPinLvl3 tx', led), bramRdAddr, bramWrCmd))

topEntity3 :: Clock Dom27 -> Signal Dom27 Bit -> Signal Dom27 (Bit, BitVector 6)
topEntity3 clk rxPin = withClockResetEnable clk resetGen enableGen $ let fullOut = mealy sysStep3 initSysState3 (bundle (rxPin, bramOut)); rdAddr = (\(_, a, _) -> a) <$> fullOut; wrCmd = (\(_, _, w) -> w) <$> fullOut; bramOut = blockRamFile (SNat @1024) "prog.hex" rdAddr wrCmd in (\(x, _, _) -> x) <$> fullOut

mkInstr3 :: BitVector 4 -> RegIdx -> RegIdx -> BitVector 24 -> Instr32
mkInstr3 op rd rs imm = op ++# pack rd ++# pack rs ++# imm

mkInstr3NoReg :: BitVector 4 -> BitVector 24 -> Instr32
mkInstr3NoReg op imm = op ++# (0 :: BitVector 4) ++# imm

testProgram3 = testProgram3'

testProgram3' = let nop = mkInstr3NoReg 0x0 0; base = repeat nop :: InstrMem in replace (0 :: Unsigned 10) (mkInstr3 0x1 0 0 5) $ replace (1 :: Unsigned 10) (mkInstr3 0x1 1 0 3) $ replace (2 :: Unsigned 10) (mkInstr3 0x2 2 0 0) $ replace (3 :: Unsigned 10) (mkInstr3 0x2 2 2 3) $ replace (4 :: Unsigned 10) (mkInstr3 0x7 0 2 0) $ replace (5 :: Unsigned 10) (mkInstr3 0x3 3 2 2) $ replace (6 :: Unsigned 10) (mkInstr3 0x4 3 3 7) $ replace (7 :: Unsigned 10) (mkInstr3 0x5 3 3 1) $ replace (8 :: Unsigned 10) (mkInstr3 0x7 0 3 0) $ replace (9 :: Unsigned 10) (mkInstr3 0x1 0 0 0) $ replace (10 :: Unsigned 10) (mkInstr3 0xA 0 0 12) $ replace (11 :: Unsigned 10) (mkInstr3 0x8 0 0 0) $ replace (12 :: Unsigned 10) (mkInstr3 0x7 0 2 0) $ replace (13 :: Unsigned 10) (mkInstr3 0x8 0 0 0) base

-- | Write testProgram3' to prog.hex for use with blockRamFile. Run once from GHCi: writeProgHex
writeProgHex :: P.IO ()
writeProgHex = P.writeFile "prog.hex" $ P.unlines $ P.map (hexLine P.. (fromIntegral :: Instr32 -> P.Int)) $ toList instrs
  where
    instrs = testProgram3' :: InstrMem
    hexLine (w :: P.Int) = P.concatMap (\i -> showHex (Bits.shiftR w (28 - i * 4) Bits..&. 0xF) "") [0 .. 7 :: P.Int]

simulateCpu3 :: Int -> [(PC, Bool, BitVector 8, Bool)]
simulateCpu3 n = P.take n $ go initCpuState3 where go s = let addr = cpu3PC s; instr = testProgram3' !! addr; (s', (nextAddr, ov, od, halted, _, _)) = stepCpu3 s (instr, True) in (nextAddr, ov, od, halted) : go s'
