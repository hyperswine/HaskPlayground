{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, TypeOperators, FlexibleContexts, RecordWildCards, NoImplicitPrelude, NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module SdramSimpleRisc where
import Clash.Prelude
import Data.Maybe (isJust, isNothing)
import qualified SimpleRisc as R

-- The whole 8 MiB of SDRAM is RAM at 0x80000000. The boot ROM, finisher,
-- BOOTCAUSE and UART follow the BRAM image's map; the ROM owns address 0.
decodeTarget :: R.Word32 -> R.MemoryTarget
decodeTarget address
  | address .&. 0xff80_0000 == 0x8000_0000 = R.RamTarget
  -- no I/O complex on this image yet
  | address .&. 0xffff_f000 == R.ioBase = R.UnmappedTarget
  | otherwise = R.decodeMemoryTarget address

-- Normal steps the core; the others hold it while a memory word is fetched,
-- from SDRAM (AwaitReady, AwaitDone) or from the boot ROM (RomRead,
-- RomCapture: one cycle for the registered address, one for the read).
data MemoryPhase = Normal | AwaitReady | AwaitDone | RomRead | RomCapture deriving (Generic,NFDataX,Eq,Show)
data State = State {machine :: R.Machine, phase :: MemoryPhase,
  word :: R.Word32, requestAddress :: Unsigned 21, requestData :: R.Word32,
  requestWrite :: Bool, romAddress :: Unsigned 9} deriving (Generic,NFDataX)
initialState :: State
-- The wrapper drives memory by the CPU's phase, so no fetch overlap here.
initialState = State R.initialMachine {R.cpuLocalMemory = False} Normal 0 0 0 False 0

-- SDRAM data, ready, done; received byte; UART ready; Ctrl-C; boot ROM word.
type Input = (R.Word32,Bool,Bool,Maybe R.Byte,Bool,Bool,R.Word32)
-- SDRAM request, write, word address, write data; UART byte; boot ROM word address.
type Output = (Bool,Bool,Unsigned 21,R.Word32,Maybe R.Byte,Unsigned 9)
step :: State -> Input -> (State,Output)
step s@State{..} (readWord,ready,done,received,txReady,emergency,romWord) =
  let wireOutput byte=(phase==AwaitReady,requestWrite,requestAddress,requestData,byte,romAddress)
      held = R.tickHeld machine
      -- Ctrl-C breaks a running program; in the boot ROM it is data.
      breaking = R.breakRequested machine emergency
      -- UART continues during memory waits. At most one byte fits the existing receiver.
      buffered = case (received,R.rxHolding held) of
        (Just b,Nothing) | not breaking -> held {R.rxHolding=Just b}
        _ -> held
      waiting = if breaking then R.resetCpu buffered else buffered
   in case phase of
      AwaitReady -> (s {machine=waiting,
        phase=if ready then AwaitDone else AwaitReady},wireOutput Nothing)
      AwaitDone -> (s {machine=waiting,
        phase=if done then Normal else AwaitDone,word=if done then readWord else word},wireOutput Nothing)
      RomRead -> (s {machine=waiting, phase=RomCapture},wireOutput Nothing)
      RomCapture -> (s {machine=waiting, phase=Normal, word=romWord},wireOutput Nothing)
      Normal ->
        let (next,(_,wr,byte))=R.machineStepDecodedWith decodeTarget machine (word,received,txReady,emergency)
            bus=R.systemBus machine
            dataRead=case R.busPhase bus of R.BusLoadIssue -> True; R.BusStoreIssue -> True; _ -> False
            fetch=(case R.cpuPhase machine of R.Fetch -> True; R.FetchCross -> True; _ -> False) && not breaking
            busWriting=case R.busPhase bus of R.BusRamWrite -> True; _ -> False
            byteAddress=if dataRead || busWriting then R.busAddress (R.busRequest bus) else R.cpuPc machine + (case R.cpuPhase machine of R.FetchCross -> 2; _ -> 0)
            access=fetch || (dataRead && not breaking) || isJust wr
            -- Bit 31 clear is the boot ROM: reads only, the bus refuses stores.
            fromRom=not (testBit byteAddress 31)
            queued
              | not access = s {machine=next,phase=Normal}
              | fromRom = s {machine=next,phase=RomRead,romAddress=resize (byteAddress `shiftR` 2)}
              | otherwise = s {machine=next,phase=AwaitReady,
                  requestAddress=resize (byteAddress `shiftR` 2),requestData=maybe 0 snd wr,requestWrite=isJust wr}
         in (queued,wireOutput byte)

{-# ANN topEntity (Synthesize {t_name="sdram_simple_risc",
 t_inputs=[PortName "clk",PortName "reset",PortName "enable",PortName "uart_rx",PortName "memory_data",PortName "memory_ready",PortName "memory_done"],
 t_output=PortProduct "" [PortName "uart_tx",PortName "memory_req",PortName "memory_write",PortName "memory_address",PortName "memory_wdata"]}) #-}
topEntity :: Clock XilinxSystem -> Reset XilinxSystem -> Enable XilinxSystem ->
 Signal XilinxSystem Bit -> Signal XilinxSystem R.Word32 -> Signal XilinxSystem Bool -> Signal XilinxSystem Bool ->
 (Signal XilinxSystem Bit,Signal XilinxSystem Bool,Signal XilinxSystem Bool,Signal XilinxSystem (Unsigned 21),Signal XilinxSystem R.Word32)
topEntity clk rst en rx dat ready done = withClockResetEnable clk rst en (circuit rx dat ready done)

circuit :: HiddenClockResetEnable dom => Signal dom Bit -> Signal dom R.Word32 -> Signal dom Bool -> Signal dom Bool ->
 (Signal dom Bit,Signal dom Bool,Signal dom Bool,Signal dom (Unsigned 21),Signal dom R.Word32)
circuit serialRx memoryData memoryReady memoryDone = (serialTx,req,wr,address,wdata)
 where
 receivedNow=R.uartRx (register high (register high serialRx))
 received=register Nothing receivedNow
 emergency=register False ((==Just 0x03) <$> receivedNow)
 (serialTx,uartReady)=R.uartTx txRequest
 txRequest=register Nothing txNow
 txReady=(\r pending -> r && isNothing pending) <$> uartReady <*> txRequest
 -- The boot ROM in block RAM: its address comes from the registered state.
 romWord=rom R.bootRomContents romAddressOut
 (req,wr,address,wdata,txNow,romAddressOut)=unbundle (mealy step initialState
   (bundle (memoryData,memoryReady,memoryDone,received,txReady,emergency,romWord)))
