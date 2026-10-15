{-# LANGUAGE DataKinds, TypeApplications, TypeOperators, FlexibleContexts, RecordWildCards, NoImplicitPrelude, NumericUnderscores, CPP #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module SdramPipeline where
import Clash.Prelude
import Data.Maybe (isJust,isNothing)
import qualified SimpleRisc as R

decodeTarget :: R.Word32 -> R.MemoryTarget
decodeTarget address
  | address .&. 0xff80_0000 == 0x8000_0000 = R.RamTarget
  | address .&. 0xffff_f000 == R.ioBase = R.UnmappedTarget
  | otherwise = R.decodeMemoryTarget address

-- A fixed-latency cache port lets the existing pipeline overlap instructions
-- on hits. On misses every logical CPU/memory stage freezes together; cycle
-- counters, physical UART and refresh continue to run on physical clocks.
initialMachine :: R.Machine
initialMachine = R.boardMachine {R.cpuExternalMemory=True,
#ifdef RISCV_COMPRESSED
  R.cpuCompressed=True
#else
  R.cpuCompressed=False
#endif
  }

type Address = Unsigned 22
type Input = (R.Word32,Bool,Maybe R.Byte,Bool,Bool,R.Word32,R.Word32)
type Output = (Address,Bool,R.Word32,BitVector 4,Maybe R.Byte)

physical :: R.Word32 -> Address
physical address = case decodeTarget address of
  R.RamTarget -> resize (address `shiftR` 2) .&. 0x1f_ffff
  R.RomTarget -> 0x20_0000 .|. resize ((address `shiftR` 2) .&. 0x1ff)
  _ -> 0x20_0000 -- idle/device accesses never reach external RAM

step :: R.Machine -> Input -> (R.Machine,Output)
step machine (memoryWord,held,received,txReady,emergency,a,b)
  | held =
      let ticking=R.tickHeld machine
          breaking=R.breakRequested machine emergency
          buffered=case (received,R.rxHolding ticking) of
            (Just byte,Nothing) | not breaking -> ticking {R.rxHolding=Just byte}
            _ -> ticking
       in (if breaking then R.resetCpu buffered else buffered,(0x20_0000,False,0,15,Nothing))
  | otherwise =
      let injected=machine {R.cpuRfA=a,R.cpuRfB=b}
          (next,(index,write,tx))=R.machineStepCoreBy R.pipelinedCpuStep decodeTarget injected (memoryWord,received,txReady,emergency)
          bus=R.systemBus machine
          busActive=case R.busPhase bus of R.BusIdle -> isJust (R.cpuBusRequest machine); _ -> True
          busAddress=R.busAddress (R.busRequest bus)
          readAddress
            | busActive = busAddress
            | R.cpuFast machine = R.cpuPortAddressFull next
            | otherwise = 0
          writeAddress=if busActive then busAddress else R.cpuPortWriteAddressFull next
          mask=if busActive then 15 else R.cpuPortByteEnable next
          output=case write of
            Just (_,value) -> (physical writeAddress,True,value,mask,tx)
            Nothing -> (physical readAddress,False,0,15,tx)
       in (next {R.cpuRegs=repeat 0},output)

{-# ANN topEntity (Synthesize {t_name="sdram_pipeline_risc",
 t_inputs=[PortName "clk",PortName "reset",PortName "enable",PortName "uart_rx",PortName "memory_data",PortName "memory_hold"],
 t_output=PortProduct "" [PortName "uart_tx",PortName "memory_address",PortName "memory_write",PortName "memory_wdata",PortName "memory_mask"]}) #-}
topEntity :: Clock XilinxSystem -> Reset XilinxSystem -> Enable XilinxSystem ->
 Signal XilinxSystem Bit -> Signal XilinxSystem R.Word32 -> Signal XilinxSystem Bool ->
 (Signal XilinxSystem Bit,Signal XilinxSystem Address,Signal XilinxSystem Bool,Signal XilinxSystem R.Word32,Signal XilinxSystem (BitVector 4))
topEntity clk rst en rx dat held = withClockResetEnable clk rst en (circuit rx dat held)

circuit :: HiddenClockResetEnable dom => Signal dom Bit -> Signal dom R.Word32 -> Signal dom Bool ->
 (Signal dom Bit,Signal dom Address,Signal dom Bool,Signal dom R.Word32,Signal dom (BitVector 4))
circuit serialRx memoryData memoryHeld = (serialTx,address,write,wdata,mask)
 where
 receivedNow=R.uartRx (register high (register high serialRx))
 emergency=register False ((==Just 3) <$> receivedNow)
 machine=register initialMachine (fst <$> stepped)
 breaking=R.breakRequested <$> machine <*> emergency
 rxWanted=(\m offered headByte -> isNothing (R.rxHolding m) && isNothing offered && isJust headByte) <$> machine <*> received <*> rxHead
 rxPop=(&&) <$> rxWanted <*> (not <$> breaking)
 received=register Nothing (mux rxPop rxHead (pure Nothing))
 (rxHead,_)=unbundle (mealy R.fifoStep (R.initialFifo :: R.Fifo 32) (bundle (breaking,receivedNow,rxPop)))
 (serialTx,uartReady)=R.uartTx txOffer
 txPush=register Nothing txNow
 txPop=(\ready headByte -> ready && isJust headByte) <$> uartReady <*> txHead
 txOffer=mux txPop txHead (pure Nothing)
 (txHead,txCount)=unbundle (mealy R.fifoStep (R.initialFifo :: R.Fifo 16) (bundle (pure False,txPush,txPop)))
 txReady=register False ((<15) <$> txCount)
 rfWrite=(\m stop pause -> if stop || pause then Nothing else R.cpuPendingWrite m) <$> machine <*> breaking <*> memoryHeld
 rfA=asyncRam (SNat @32) (fst . R.cpuReadIndices <$> machine) rfWrite
 rfB=asyncRam (SNat @32) (snd . R.cpuReadIndices <$> machine) rfWrite
 stepped=step <$> machine <*> bundle (memoryData,memoryHeld,received,txReady,emergency,rfA,rfB)
 (address,write,wdata,mask,txNow)=unbundle (snd <$> stepped)
