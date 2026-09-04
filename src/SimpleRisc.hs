{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}

-- | A deliberately small, unpipelined RV32I computer.
--
-- Memory map:
--
--   * 0x0000_0000 - 0x0000_0fff: 4 KiB unified instruction/data BRAM
--   * 0x1000_0000: UART TXDATA (write the low byte)
--   * 0x1000_0004: UART STATUS (bit 0 = TX ready, bit 1 = RX byte ready)
--   * 0x1000_0008: UART RXDATA (read low byte, consuming it)
--
-- The physical UART is 115200 8-N-1 at a 50 MHz input clock.  While the CPU
-- is stopped the host protocol consists of single-byte commands:
--
--   * 'P', countLo, countHi, words...  PROGRAM @count@ little-endian RV32
--     words.  Each word is also sent least-significant byte first.
--   * 'R'                              RUN from address zero.
--   * 'X'                              RESET PC and all registers.
--   * 'M'                              RESET-MEM (also resets the CPU), taking
--                                      1024 clocks to zero the BRAM.
--
-- PROGRAM replaces words starting at address zero and records the end of the
-- program.  After the instruction in the final programmed word retires, the
-- CPU stops and transmits the four ASCII bytes "DONE".  ECALL, EBREAK and an
-- illegal instruction also stop the CPU and produce DONE.  While running,
-- received UART bytes are placed in the one-byte RXDATA register; byte 0x03
-- (ASCII ETX / Ctrl-C) is reserved as an emergency register reset.
module SimpleRisc where

import Clash.Prelude

type Word32 = Unsigned 32

type Byte = Unsigned 8

type MemAddr = Unsigned 10

data CpuPhase
  = Fetch
  | FetchWait
  | LoadWait (Index 32) (BitVector 3) (Unsigned 2) Bool
  | StoreWait (BitVector 3) (Unsigned 2) MemAddr Word32 Bool
  deriving (Generic, NFDataX, Show, Eq)

data HostState
  = HostIdle
  | ProgramCountLo
  | ProgramCountHi Byte
  | ProgramBytes (Unsigned 11) MemAddr (Index 4) Word32
  | ClearMemory MemAddr
  deriving (Generic, NFDataX, Show, Eq)

data ReplyState = NoReply | DoneReply (Unsigned 3)
  deriving (Generic, NFDataX, Show, Eq)

data Machine = Machine
  { cpuRegs :: Vec 32 Word32,
    cpuPc :: Word32,
    cpuPhase :: CpuPhase,
    cpuRunning :: Bool,
    programEnd :: Word32,
    hostState :: HostState,
    rxHolding :: Maybe Byte,
    replyState :: ReplyState
  }
  deriving (Generic, NFDataX)

initialMachine :: Machine
initialMachine =
  Machine
    { cpuRegs = repeat 0,
      cpuPc = 0,
      cpuPhase = Fetch,
      cpuRunning = False,
      programEnd = 0,
      hostState = HostIdle,
      rxHolding = Nothing,
      replyState = NoReply
    }

-- UART -----------------------------------------------------------------------

-- 50 MHz / 115200 is 434 clocks per bit (0.006% baud-rate error).
type BaudCounter = Unsigned 9

fullBit :: BaudCounter
fullBit = 433

halfBit :: BaudCounter
halfBit = 216

data RxState
  = RxIdle
  | RxStart BaudCounter
  | RxData (Index 8) BaudCounter Byte
  | RxStop BaudCounter Byte
  deriving (Generic, NFDataX)

uartRx :: (HiddenClockResetEnable dom) => Signal dom Bit -> Signal dom (Maybe Byte)
uartRx = mealy rxStep RxIdle

rxStep :: RxState -> Bit -> (RxState, Maybe Byte)
rxStep RxIdle serialBit
  | serialBit == low = (RxStart halfBit, Nothing)
  | otherwise = (RxIdle, Nothing)
rxStep (RxStart n) serialBit
  | n /= 0 = (RxStart (n - 1), Nothing)
  | serialBit == low = (RxData 0 fullBit 0, Nothing)
  | otherwise = (RxIdle, Nothing)
rxStep (RxData bitNo n byte) serialBit
  | n /= 0 = (RxData bitNo (n - 1) byte, Nothing)
  | otherwise =
      let byte' = if serialBit == high then setBit byte (fromIntegral bitNo) else byte
       in if bitNo == maxBound
            then (RxStop fullBit byte', Nothing)
            else (RxData (succ bitNo) fullBit byte', Nothing)
rxStep (RxStop n byte) serialBit
  | n /= 0 = (RxStop (n - 1) byte, Nothing)
  | serialBit == high = (RxIdle, Just byte)
  | otherwise = (RxIdle, Nothing)

data TxState
  = TxIdle
  | TxStart BaudCounter Byte
  | TxData (Index 8) BaudCounter Byte
  | TxStop BaudCounter
  deriving (Generic, NFDataX)

uartTx :: (HiddenClockResetEnable dom) => Signal dom (Maybe Byte) -> (Signal dom Bit, Signal dom Bool)
uartTx request = unbundle (mealy txStep TxIdle request)

txStep :: TxState -> Maybe Byte -> (TxState, (Bit, Bool))
txStep TxIdle request =
  (case request of Just byte -> TxStart fullBit byte; Nothing -> TxIdle, (high, True))
txStep (TxStart n byte) _
  | n /= 0 = (TxStart (n - 1) byte, (low, False))
  | otherwise = (TxData 0 fullBit byte, (low, False))
txStep (TxData bitNo n byte) _
  | n /= 0 = (TxData bitNo (n - 1) byte, (boolToBit (testBit byte (fromIntegral bitNo)), False))
  | bitNo == maxBound = (TxStop fullBit, (boolToBit (testBit byte 7), False))
  | otherwise = (TxData (succ bitNo) fullBit byte, (boolToBit (testBit byte (fromIntegral bitNo)), False))
txStep (TxStop n) _
  | n /= 0 = (TxStop (n - 1), (high, False))
  | otherwise = (TxIdle, (high, False))

-- Top level ------------------------------------------------------------------

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "simple_risc",
        t_inputs = [PortName "clk", PortName "reset", PortName "enable", PortName "uart_rx"],
        t_output = PortName "uart_tx"
      }
  )
  #-}
topEntity ::
  "CLK" ::: Clock System ->
  "RESET" ::: Reset System ->
  "ENABLE" ::: Enable System ->
  "UART_RX" ::: Signal System Bit ->
  "UART_TX" ::: Signal System Bit
topEntity clk rst en serialRx = withClockResetEnable clk rst en (simpleRisc serialRx)

simpleRisc :: (HiddenClockResetEnable dom) => Signal dom Bit -> Signal dom Bit
simpleRisc serialRx = serialTx
  where
    received = uartRx serialRx
    (serialTx, txReady) = uartTx txRequest

    machineInput = bundle (memoryOut, received, txReady)
    machineOutput = mealy machineStep initialMachine machineInput
    (memoryAddress, memoryWrite, txRequest) = unbundle machineOutput

    -- A synchronous 1024 x 32-bit RAM.  Reads take one cycle; writes are visible
    -- on the following read.  This shape maps naturally to FPGA block RAM.
    memoryOut = blockRamPow2 (repeat 0) memoryAddress memoryWrite

-- | Result wires from the machine controller: BRAM read address, optional BRAM
-- write, and an optional byte offered to the UART transmitter.
machineStep ::
  Machine ->
  (Word32, Maybe Byte, Bool) ->
  (Machine, (MemAddr, Maybe (MemAddr, Word32), Maybe Byte))
machineStep machine (memoryWord, received, txReady) =
  case hostState machine of
    ClearMemory address -> clearStep address
    _
      | cpuRunning machine -> runningStep machine memoryWord received txReady
      | otherwise -> stoppedStep machine received txReady
  where
    clearStep address =
      let lastAddress = address == maxBound
          machine' =
            machine
              { hostState = if lastAddress then HostIdle else ClearMemory (address + 1),
                cpuRegs = repeat 0,
                cpuPc = 0,
                cpuPhase = Fetch,
                cpuRunning = False,
                programEnd = 0,
                rxHolding = Nothing
              }
       in (machine', (address, Just (address, 0), Nothing))

-- Host controller ------------------------------------------------------------

stoppedStep ::
  Machine ->
  Maybe Byte ->
  Bool ->
  (Machine, (MemAddr, Maybe (MemAddr, Word32), Maybe Byte))
stoppedStep machine received txReady =
  let (machineWithReply, replyByte) = sendReply machine txReady
   in case (hostState machineWithReply, received) of
        (HostIdle, Just 0x50) -> idleOut machineWithReply {hostState = ProgramCountLo} replyByte
        (HostIdle, Just 0x52) ->
          idleOut
            machineWithReply
              { cpuRegs = repeat 0,
                cpuPc = 0,
                cpuPhase = Fetch,
                cpuRunning = programEnd machineWithReply /= 0,
                rxHolding = Nothing
              }
            replyByte
        (HostIdle, Just 0x58) -> idleOut (resetCpu machineWithReply) replyByte
        (HostIdle, Just 0x4d) ->
          idleOut
            (resetCpu machineWithReply)
              { hostState = ClearMemory 0,
                programEnd = 0,
                replyState = NoReply
              }
            Nothing
        (ProgramCountLo, Just lowByte) ->
          idleOut machineWithReply {hostState = ProgramCountHi lowByte} replyByte
        (ProgramCountHi lowByte, Just highByte) ->
          let requested = (resize highByte `shiftL` 8) .|. resize lowByte :: Unsigned 11
              count = min requested 1024
              nextHost = if count == 0 then HostIdle else ProgramBytes count 0 0 0
           in idleOut
                machineWithReply
                  { hostState = nextHost,
                    programEnd = 0,
                    cpuRegs = repeat 0,
                    cpuPc = 0,
                    cpuPhase = Fetch
                  }
                replyByte
        (ProgramBytes wordsLeft address byteNo partial, Just byte) ->
          let shiftAmount = fromIntegral byteNo * 8
              partial' = partial .|. (resize byte `shiftL` shiftAmount)
           in if byteNo == maxBound
                then
                  let isLast = wordsLeft == 1
                      machine' =
                        machineWithReply
                          { hostState = if isLast then HostIdle else ProgramBytes (wordsLeft - 1) (address + 1) 0 0,
                            programEnd = if isLast then (resize address + 1) `shiftL` 2 else programEnd machineWithReply
                          }
                   in (machine', (address, Just (address, partial'), replyByte))
                else idleOut machineWithReply {hostState = ProgramBytes wordsLeft address (succ byteNo) partial'} replyByte
        _ -> idleOut machineWithReply replyByte
  where
    idleOut m replyByte = (m, (0, Nothing, replyByte))

sendReply :: Machine -> Bool -> (Machine, Maybe Byte)
sendReply machine False = (machine, Nothing)
sendReply machine True = case replyState machine of
  NoReply -> (machine, Nothing)
  DoneReply n ->
    let byte = case n of
          0 -> 0x44 -- D
          1 -> 0x4f -- O
          2 -> 0x4e -- N
          _ -> 0x45 -- E
        next = if n == 3 then NoReply else DoneReply (n + 1)
     in (machine {replyState = next}, Just byte)

resetCpu :: Machine -> Machine
resetCpu machine =
  machine
    { cpuRegs = repeat 0,
      cpuPc = 0,
      cpuPhase = Fetch,
      cpuRunning = False,
      rxHolding = Nothing
    }

-- CPU ------------------------------------------------------------------------

runningStep ::
  Machine ->
  Word32 ->
  Maybe Byte ->
  Bool ->
  (Machine, (MemAddr, Maybe (MemAddr, Word32), Maybe Byte))
runningStep machine memoryWord received txReady
  | received == Just 0x03 = (resetCpu machine, (0, Nothing, Nothing))
  | otherwise =
      let machineRx = case (received, rxHolding machine) of
            (Just byte, Nothing) -> machine {rxHolding = Just byte}
            _ -> machine
       in case cpuPhase machineRx of
            Fetch ->
              ( machineRx {cpuPhase = FetchWait},
                (memoryIndex (cpuPc machineRx), Nothing, Nothing)
              )
            FetchWait -> executeInstruction machineRx memoryWord txReady
            LoadWait rd funct3 byteOffset finalInstruction ->
              let value = loadValue funct3 byteOffset memoryWord
                  machine' = writeRegister rd value machineRx
               in finishInstruction finalInstruction machine' Nothing
            StoreWait funct3 byteOffset address value finalInstruction ->
              let merged = storeValue funct3 byteOffset value memoryWord
               in finishInstruction finalInstruction machineRx (Just (address, merged))

executeInstruction ::
  Machine ->
  Word32 ->
  Bool ->
  (Machine, (MemAddr, Maybe (MemAddr, Word32), Maybe Byte))
executeInstruction machine instruction txReady =
  let opcode = instruction .&. 0x7f
      rd = regIndex ((instruction `shiftR` 7) .&. 0x1f)
      funct3 = pack (resize ((instruction `shiftR` 12) .&. 7) :: Unsigned 3)
      rs1 = regIndex ((instruction `shiftR` 15) .&. 0x1f)
      rs2 = regIndex ((instruction `shiftR` 20) .&. 0x1f)
      funct7 = (instruction `shiftR` 25) .&. 0x7f
      a = readRegister rs1 machine
      b = readRegister rs2 machine
      pc = cpuPc machine
      nextPc = pc + 4
      isFinal = nextPc >= programEnd machine
      normal value = finishInstruction isFinal (writeRegister rd value machine {cpuPc = nextPc}) Nothing
      noWrite newPc = finishInstruction isFinal machine {cpuPc = newPc} Nothing
      invalid = haltMachine machine
   in case opcode of
        0x37 -> normal (immU instruction) -- LUI
        0x17 -> normal (pc + immU instruction) -- AUIPC
        0x6f ->
          -- JAL
          finishInstruction isFinal (writeRegister rd nextPc machine {cpuPc = pc + immJ instruction}) Nothing
        0x67 ->
          -- JALR
          if funct3 == 0
            then finishInstruction isFinal (writeRegister rd nextPc machine {cpuPc = (a + immI instruction) .&. complement 1}) Nothing
            else invalid
        0x63 ->
          -- branches
          case branchTaken funct3 a b of
            Just takeBranch -> noWrite (if takeBranch then pc + immB instruction else nextPc)
            Nothing -> invalid
        0x03 ->
          -- loads
          let address = a + immI instruction
           in if isUartAddress address
                then
                  let (value, machine') = readUart address txReady machine
                   in finishInstruction isFinal (writeRegister rd value machine' {cpuPc = nextPc}) Nothing
                else
                  if validMemoryAddress address && validLoad funct3
                    then
                      ( machine {cpuPc = nextPc, cpuPhase = LoadWait rd funct3 (resize address) isFinal},
                        (memoryIndex address, Nothing, Nothing)
                      )
                    else invalid
        0x23 ->
          -- stores
          let address = a + immS instruction
           in if address == uartTxData
                then
                  let request = if txReady then Just (resize b) else Nothing
                   in finishInstructionWithTx isFinal machine {cpuPc = nextPc} request
                else
                  if validMemoryAddress address && validStore funct3
                    then
                      -- Word stores need no read/modify/write cycle.
                      if funct3 == 0b010
                        then finishInstructionWithWrite isFinal machine {cpuPc = nextPc} (memoryIndex address, b)
                        else
                          ( machine {cpuPc = nextPc, cpuPhase = StoreWait funct3 (resize address) (memoryIndex address) b isFinal},
                            (memoryIndex address, Nothing, Nothing)
                          )
                    else invalid
        0x13 -> case opImmediate funct3 funct7 a (immI instruction) instruction of
          Just value -> normal value
          Nothing -> invalid
        0x33 -> case opRegister funct3 funct7 a b of
          Just value -> normal value
          Nothing -> invalid
        0x0f -> noWrite nextPc -- FENCE is a no-op in this tiny single-master core.
        0x73 -> haltMachine machine -- ECALL / EBREAK terminate the program.
        _ -> invalid

finishInstruction ::
  Bool ->
  Machine ->
  Maybe (MemAddr, Word32) ->
  (Machine, (MemAddr, Maybe (MemAddr, Word32), Maybe Byte))
finishInstruction finalInstruction machine write =
  if finalInstruction
    then (halt machine, (0, write, Nothing))
    else (machine {cpuPhase = Fetch}, (0, write, Nothing))

finishInstructionWithWrite ::
  Bool ->
  Machine ->
  (MemAddr, Word32) ->
  (Machine, (MemAddr, Maybe (MemAddr, Word32), Maybe Byte))
finishInstructionWithWrite finalInstruction machine write = finishInstruction finalInstruction machine (Just write)

finishInstructionWithTx ::
  Bool ->
  Machine ->
  Maybe Byte ->
  (Machine, (MemAddr, Maybe (MemAddr, Word32), Maybe Byte))
finishInstructionWithTx finalInstruction machine request =
  if finalInstruction
    then (halt machine, (0, Nothing, request))
    else (machine {cpuPhase = Fetch}, (0, Nothing, request))

haltMachine :: Machine -> (Machine, (MemAddr, Maybe (MemAddr, Word32), Maybe Byte))
haltMachine machine = (halt machine, (0, Nothing, Nothing))

halt :: Machine -> Machine
halt machine = machine {cpuRunning = False, cpuPhase = Fetch, replyState = DoneReply 0}

readRegister :: Index 32 -> Machine -> Word32
readRegister 0 _ = 0
readRegister index Machine {..} = cpuRegs !! index

writeRegister :: Index 32 -> Word32 -> Machine -> Machine
writeRegister 0 _ machine = machine
writeRegister index value machine@Machine {..} = machine {cpuRegs = replace index value cpuRegs}

regIndex :: Word32 -> Index 32
regIndex value = fromIntegral (resize value :: Unsigned 5)

memoryIndex :: Word32 -> MemAddr
memoryIndex address = resize (address `shiftR` 2)

validMemoryAddress :: Word32 -> Bool
validMemoryAddress address = address < 4096

uartTxData, uartStatus, uartRxData :: Word32
uartTxData = 0x1000_0000
uartStatus = 0x1000_0004
uartRxData = 0x1000_0008

isUartAddress :: Word32 -> Bool
isUartAddress address = address == uartTxData || address == uartStatus || address == uartRxData

readUart :: Word32 -> Bool -> Machine -> (Word32, Machine)
readUart address txReady machine
  | address == uartStatus =
      ( (if txReady then 1 else 0) .|. (if hasByte (rxHolding machine) then 2 else 0),
        machine
      )
  | address == uartRxData =
      (maybe 0 resize (rxHolding machine), machine {rxHolding = Nothing})
  | otherwise = (0, machine)
  where
    hasByte Nothing = False
    hasByte Just {} = True

validLoad :: BitVector 3 -> Bool
validLoad funct3 = funct3 == 0b000 || funct3 == 0b001 || funct3 == 0b010 || funct3 == 0b100 || funct3 == 0b101

validStore :: BitVector 3 -> Bool
validStore funct3 = funct3 == 0b000 || funct3 == 0b001 || funct3 == 0b010

loadValue :: BitVector 3 -> Unsigned 2 -> Word32 -> Word32
loadValue funct3 byteOffset word = case funct3 of
  0b000 -> signExtend8 byte
  0b001 -> signExtend16 half
  0b010 -> word
  0b100 -> resize byte
  0b101 -> resize half
  _ -> 0
  where
    shiftAmount = fromIntegral byteOffset * 8
    byte = resize (word `shiftR` shiftAmount) :: Unsigned 8
    half = resize (word `shiftR` shiftAmount) :: Unsigned 16

storeValue :: BitVector 3 -> Unsigned 2 -> Word32 -> Word32 -> Word32
storeValue funct3 byteOffset value oldWord = case funct3 of
  0b000 -> (oldWord .&. complement (0xff `shiftL` shiftAmount)) .|. ((value .&. 0xff) `shiftL` shiftAmount)
  0b001 -> (oldWord .&. complement (0xffff `shiftL` shiftAmount)) .|. ((value .&. 0xffff) `shiftL` shiftAmount)
  _ -> value
  where
    shiftAmount = fromIntegral byteOffset * 8

-- Decode/execute helpers ------------------------------------------------------

opImmediate :: BitVector 3 -> Word32 -> Word32 -> Word32 -> Word32 -> Maybe Word32
opImmediate funct3 funct7 a immediate instruction = case funct3 of
  0b000 -> Just (a + immediate) -- ADDI
  0b010 -> Just (boolWord (signed32 a < signed32 immediate)) -- SLTI
  0b011 -> Just (boolWord (a < immediate)) -- SLTIU
  0b100 -> Just (a `xor` immediate) -- XORI
  0b110 -> Just (a .|. immediate) -- ORI
  0b111 -> Just (a .&. immediate) -- ANDI
  0b001 -> if funct7 == 0 then Just (a `shiftL` shamt) else Nothing -- SLLI
  0b101
    | funct7 == 0 -> Just (a `shiftR` shamt) -- SRLI
    | funct7 == 0x20 -> Just (unsigned32 (signed32 a `shiftR` shamt)) -- SRAI
    | otherwise -> Nothing
  _ -> Nothing
  where
    shamt = fromIntegral ((instruction `shiftR` 20) .&. 0x1f)

opRegister :: BitVector 3 -> Word32 -> Word32 -> Word32 -> Maybe Word32
opRegister funct3 funct7 a b = case (funct3, funct7) of
  (0b000, 0x00) -> Just (a + b)
  (0b000, 0x20) -> Just (a - b)
  (0b001, 0x00) -> Just (a `shiftL` shamt)
  (0b010, 0x00) -> Just (boolWord (signed32 a < signed32 b))
  (0b011, 0x00) -> Just (boolWord (a < b))
  (0b100, 0x00) -> Just (a `xor` b)
  (0b101, 0x00) -> Just (a `shiftR` shamt)
  (0b101, 0x20) -> Just (unsigned32 (signed32 a `shiftR` shamt))
  (0b110, 0x00) -> Just (a .|. b)
  (0b111, 0x00) -> Just (a .&. b)
  _ -> Nothing
  where
    shamt = fromIntegral (b .&. 0x1f)

branchTaken :: BitVector 3 -> Word32 -> Word32 -> Maybe Bool
branchTaken funct3 a b = case funct3 of
  0b000 -> Just (a == b)
  0b001 -> Just (a /= b)
  0b100 -> Just (signed32 a < signed32 b)
  0b101 -> Just (signed32 a >= signed32 b)
  0b110 -> Just (a < b)
  0b111 -> Just (a >= b)
  _ -> Nothing

boolWord :: Bool -> Word32
boolWord False = 0
boolWord True = 1

signed32 :: Word32 -> Signed 32
signed32 = bitCoerce

unsigned32 :: Signed 32 -> Word32
unsigned32 = bitCoerce

signExtend8 :: Unsigned 8 -> Word32
signExtend8 value = if testBit value 7 then resize value .|. 0xffff_ff00 else resize value

signExtend12 :: Unsigned 12 -> Word32
signExtend12 value = if testBit value 11 then resize value .|. 0xffff_f000 else resize value

signExtend13 :: Unsigned 13 -> Word32
signExtend13 value = if testBit value 12 then resize value .|. 0xffff_e000 else resize value

signExtend16 :: Unsigned 16 -> Word32
signExtend16 value = if testBit value 15 then resize value .|. 0xffff_0000 else resize value

signExtend21 :: Unsigned 21 -> Word32
signExtend21 value = if testBit value 20 then resize value .|. 0xffe0_0000 else resize value

immI :: Word32 -> Word32
immI instruction = signExtend12 (resize (instruction `shiftR` 20))

immS :: Word32 -> Word32
immS instruction =
  signExtend12
    ( resize ((instruction `shiftR` 25) `shiftL` 5)
        .|. resize ((instruction `shiftR` 7) .&. 0x1f)
    )

immB :: Word32 -> Word32
immB instruction =
  signExtend13
    ( resize (((instruction `shiftR` 31) .&. 1) `shiftL` 12)
        .|. resize (((instruction `shiftR` 7) .&. 1) `shiftL` 11)
        .|. resize (((instruction `shiftR` 25) .&. 0x3f) `shiftL` 5)
        .|. resize (((instruction `shiftR` 8) .&. 0x0f) `shiftL` 1)
    )

immU :: Word32 -> Word32
immU instruction = instruction .&. 0xffff_f000

immJ :: Word32 -> Word32
immJ instruction =
  signExtend21
    ( resize (((instruction `shiftR` 31) .&. 1) `shiftL` 20)
        .|. resize (((instruction `shiftR` 12) .&. 0xff) `shiftL` 12)
        .|. resize (((instruction `shiftR` 20) .&. 1) `shiftL` 11)
        .|. resize (((instruction `shiftR` 21) .&. 0x3ff) `shiftL` 1)
    )
