{-# LANGUAGE DataKinds, TypeApplications, TypeOperators, FlexibleContexts, RecordWildCards, NoImplicitPrelude, NumericUnderscores, CPP, DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module SdramPipeline where
import Clash.Prelude
import Data.Maybe (isJust,isNothing)
import qualified SimpleRisc as R

decodeTarget :: R.Word32 -> R.MemoryTarget
decodeTarget address
  | address .&. 0xff80_0000 == 0x8000_0000 = R.RamTarget
  | address >= R.ioBase && address <= R.ioBase + 12 = R.IoTarget
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
type Input = (R.Word32,Bool,Maybe R.Byte,Bool,Bool,R.Word32,R.Word32,Maybe R.Word32)
type Output = (Address,Bool,R.Word32,BitVector 4,Maybe R.Byte)

physical :: R.Word32 -> Address
physical address
  | address .&. 0xff80_0000 == 0x8000_0000 = resize (address `shiftR` 2) .&. 0x1f_ffff
  | R.romAddress address = 0x20_0000 .|. resize ((address `shiftR` 2) .&. 0x1ff)
  | otherwise = 0x20_0000 -- idle/device accesses never reach external RAM

-- The fast port's full address and byte lanes are already registers in
-- Machine. They are captured on the same edge as the old cache a0 stage.
-- Keep only the other request fields here, then look up those registers
-- directly. In particular no reset/next-Machine mux drives the cache address.
-- The cache now owns the two remaining logical stages, so the CPU still sees
-- a three-logical-clock port. A hold freezes this slot with both cache stages.
data RequestSlot = RequestSlot
  { slotBusActive :: Bool, slotBusAddress :: R.Word32, slotFast :: Bool,
    slotWrite :: Bool, slotData :: R.Word32 }
  deriving (Generic,NFDataX,Show,Eq)

initialRequestSlot :: RequestSlot
initialRequestSlot = RequestSlot False 0 False False 0

requestSlot :: R.Machine -> Output -> RequestSlot
requestSlot machine (_,writing,value,_,_) = RequestSlot active busAddress (R.cpuFast machine) writing value
 where
 bus=R.systemBus machine
 active=case R.busPhase bus of R.BusIdle -> isJust (R.cpuBusRequest machine); _ -> True
 busAddress=R.busAddress (R.busRequest bus)

requestAddress :: R.Machine -> RequestSlot -> Address
requestAddress machine RequestSlot{..} = physical selected
 where
 selected
   | slotBusActive = slotBusAddress
   | slotWrite = R.cpuPortWriteAddressFull machine
   | slotFast = R.cpuPortAddressFull machine
   | otherwise = 0

requestMask :: R.Machine -> RequestSlot -> BitVector 4
requestMask machine slot
  | slotBusActive slot || not (slotWrite slot) = 15
  | otherwise = R.cpuPortByteEnable machine

step :: R.Machine -> Input -> (R.Machine,Output)
step machine (memoryWord,held,received,txReady,emergency,a,b,ioAnswer)
  | held =
      let ticking=R.tickHeld machine
          breaking=R.breakRequested machine emergency
          buffered=case (received,R.rxHolding ticking) of
            (Just byte,Nothing) | not breaking -> ticking {R.rxHolding=Just byte}
            _ -> ticking
       in (if breaking then R.resetCpu buffered else buffered,(0x20_0000,False,0,15,Nothing))
  | otherwise =
      let injected=machine {R.cpuRfA=a,R.cpuRfB=b,R.cpuDevices=(R.cpuDevices machine) {R.devIo=ioAnswer}}
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

-- Only the SPI byte port is exposed. The BRAM image's DMA engine has a
-- 64 KiB second RAM port and cannot address SDRAM, so its registers stay
-- unmapped here. An answer survives a cache hold until the CPU can take it;
-- the physical SPI shifter keeps running while the CPU is frozen.
data SpiIo = SpiIo {slotSpi :: R.Spi, slotAnswer :: Maybe R.Word32}
  deriving (Generic,NFDataX,Show,Eq)

initialSpiIo :: SpiIo
initialSpiIo = SpiIo R.initialSpi Nothing

spiIoStep :: SpiIo -> (Maybe R.BusRequest,Bool,Bool,Bit) ->
 (SpiIo,(Maybe R.Word32,(Bit,Bit,Bit)))
spiIoStep io (request,reset,held,miso) = (next,(slotAnswer io,R.spiPins (slotSpi io)))
 where
 spi=(slotSpi io) {R.spiMiso=miso}
 taken=if held || isJust (slotAnswer io) then Nothing else request
 offset=maybe 0 (\q -> R.busAddress q .&. 0xfff) taken
 writing at=case taken of
   Just q | R.busWrite q && offset==at -> Just (R.busData q)
   _ -> Nothing
 value
   | offset==R.ioSpiData = resize (R.spiIn spi)
   | offset==R.ioSpiStatus = R.boolWord (R.spiBusy spi)
   | offset==R.ioSpiControl = R.boolWord (R.spiSelect spi) .|. (resize (R.spiDivider spi) `shiftL` 8)
   | offset==12 = 0x5344_5031 -- SDP1: SPI-only SDRAM peripheral version 1
   | otherwise = 0
 answer
   | held = slotAnswer io
   | otherwise = (\q -> if R.busWrite q then 0 else value) <$> taken
 next
   | reset = initialSpiIo
   | otherwise = SpiIo (R.stepSpi (resize <$> writing R.ioSpiData) (writing R.ioSpiControl) spi) answer

{-# ANN topEntity (Synthesize {t_name="sdram_pipeline_risc",
 t_inputs=[PortName "clk",PortName "reset",PortName "enable",PortName "uart_rx",PortName "memory_data",PortName "memory_hold",PortName "sd_miso"],
 t_output=PortProduct "" [PortName "uart_tx",PortName "memory_address",PortName "memory_write",PortName "memory_wdata",PortName "memory_mask",PortName "sd_sck",PortName "sd_mosi",PortName "sd_cs"]}) #-}
topEntity :: Clock XilinxSystem -> Reset XilinxSystem -> Enable XilinxSystem ->
 Signal XilinxSystem Bit -> Signal XilinxSystem R.Word32 -> Signal XilinxSystem Bool -> Signal XilinxSystem Bit ->
 (Signal XilinxSystem Bit,Signal XilinxSystem Address,Signal XilinxSystem Bool,Signal XilinxSystem R.Word32,Signal XilinxSystem (BitVector 4),Signal XilinxSystem Bit,Signal XilinxSystem Bit,Signal XilinxSystem Bit)
topEntity clk rst en rx dat held miso = withClockResetEnable clk rst en (circuit rx dat held miso)

circuit :: HiddenClockResetEnable dom => Signal dom Bit -> Signal dom R.Word32 -> Signal dom Bool -> Signal dom Bit ->
 (Signal dom Bit,Signal dom Address,Signal dom Bool,Signal dom R.Word32,Signal dom (BitVector 4),Signal dom Bit,Signal dom Bit,Signal dom Bit)
circuit serialRx memoryData memoryHeld sdMiso = (serialTx,address,write,wdata,mask,sck,mosi,cs)
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
 miso=register high (register high sdMiso)
 (ioAnswer,ioPins)=unbundle (mealy spiIoStep initialSpiIo
   (bundle (R.ioRequest <$> machine,R.clearRegisters <$> machine,memoryHeld,miso)))
 (sck,mosi,cs)=unbundle ioPins
 stepped=step <$> machine <*> bundle (memoryData,memoryHeld,received,txReady,emergency,rfA,rfB,ioAnswer)
 request=regEn initialRequestSlot (not <$> memoryHeld) (requestSlot <$> machine <*> (snd <$> stepped))
 address=requestAddress <$> machine <*> request
 write=slotWrite <$> request
 wdata=slotData <$> request
 mask=requestMask <$> machine <*> request
 txNow=(\(_,_,_,_,tx) -> tx) <$> (snd <$> stepped)
