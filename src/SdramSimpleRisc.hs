{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, TypeOperators, FlexibleContexts, RecordWildCards, NoImplicitPrelude, NumericUnderscores #-}
module SdramSimpleRisc where
import Clash.Prelude
import Data.Maybe (isJust, isNothing)
import qualified SimpleRisc as R

-- Keep the 64 KiB zero alias for existing programs; the full 8 MiB is at 0x80000000.
-- The finisher is outside the zero alias and remains a device, never RAM.
decodeTarget :: R.Word32 -> R.MemoryTarget
decodeTarget address
  | address .&. 0xff80_0000 == 0x8000_0000 = R.RamTarget
  | otherwise = R.decodeMemoryTarget address

data MemoryPhase = Normal | AwaitReady | AwaitDone deriving (Generic,NFDataX,Eq,Show)
data State = State {machine :: R.Machine, phase :: MemoryPhase,
  word :: R.Word32, requestAddress :: Unsigned 21, requestData :: R.Word32,
  requestWrite :: Bool} deriving (Generic,NFDataX)
initialState :: State
initialState = State R.initialMachine Normal 0 0 0 False

type Input = (R.Word32,Bool,Bool,Maybe R.Byte,Bool,Bool)
type Output = (Bool,Bool,Unsigned 21,R.Word32,Maybe R.Byte)
step :: State -> Input -> (State,Output)
step s@State{..} (readWord,ready,done,received,txReady,emergency) =
  let wireOutput byte=(phase==AwaitReady,requestWrite,requestAddress,requestData,byte)
      held = machine {R.cpuCycle=R.tickCounter True Nothing (R.cpuCycle machine)}
      -- UART continues during memory waits. At most one byte fits the existing receiver.
      buffered = case (R.cpuRunning held,received,R.rxHolding held) of
        (True,Just b,Nothing) -> held {R.rxHolding=Just b}
        _ -> held
   in case phase of
      AwaitReady -> (s {machine=if emergency then R.resetCpu buffered else buffered,
        phase=if ready then AwaitDone else AwaitReady},wireOutput Nothing)
      AwaitDone -> (s {machine=if emergency then R.resetCpu buffered else buffered,
        phase=if done then Normal else AwaitDone,word=if done then readWord else word},wireOutput Nothing)
      Normal ->
        let highRun=not (R.cpuRunning machine) && R.hostState machine==R.HostIdle && received==Just 0x48
            received'=if highRun then Just 0x52 else received
            (next0,(_,wr,byte))=R.machineStepDecodedWith decodeTarget machine (word,received',txReady,emergency)
            next=if highRun then next0 {R.cpuPc=0x8000_0000,R.programEnd=0x8000_0000 + (R.programEnd machine .&. 0x7fffff)} else next0
            bus=R.systemBus machine
            dataRead=case R.busPhase bus of R.BusLoadIssue -> True; R.BusStoreIssue -> True; _ -> False
            fetch=R.cpuRunning machine && (case R.cpuPhase machine of R.Fetch -> True; _ -> False) && not emergency
            busWriting=case R.busPhase bus of R.BusRamWrite -> True; _ -> False
            address=if dataRead || busWriting
              then resize (R.busAddress (R.busRequest bus) `shiftR` 2)
              else if fetch then resize (R.cpuPc machine `shiftR` 2)
              else maybe 0 (resize . fst) wr
            access=fetch || (R.cpuRunning machine && dataRead && not emergency) || isJust wr
            isWrite=isJust wr
            datum=maybe 0 snd wr
            queued=s {machine=next,phase=if access then AwaitReady else Normal,
              requestAddress=address,requestData=datum,requestWrite=isWrite}
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
 (req,wr,address,wdata,txNow)=unbundle (mealy step initialState (bundle (memoryData,memoryReady,memoryDone,received,txReady,emergency)))
