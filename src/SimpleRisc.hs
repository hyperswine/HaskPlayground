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

-- | A deliberately small, unpipelined RV32IM computer.
--
-- Memory map:
--
--   * 0x0000_0000 - 0x0000_ffff: 64 KiB unified instruction/data BRAM
--   * 0x1000_0000: UART TXDATA (write the low byte)
--   * 0x1000_0004: UART STATUS (bit 0 = TX ready, bit 1 = RX byte ready)
--   * 0x1000_0008: UART RXDATA (read low byte, consuming it)
--
-- The physical UART is 8-N-1 at clock / 868 baud: 115200 at 100 MHz.  While the CPU
-- is stopped the host protocol consists of single-byte commands:
--
--   * 'P', countLo, countHi, words...  PROGRAM @count@ little-endian RV32
--     words.  Each word is also sent least-significant byte first.
--   * 'R'                              RUN from address zero.
--   * 'X'                              RESET PC and all registers.
--   * 'M'                              RESET-MEM (also resets the CPU), taking
--                                      16384 clocks to zero the BRAM.
--
-- PROGRAM replaces words starting at address zero and records the end of the
-- program.  When an instruction retires with its next PC at or beyond the end
-- of the program (falling off the end, or jumping past it), the CPU stops and
-- transmits the four ASCII bytes "DONE".  A backward branch in the final word
-- therefore loops as expected.  ECALL, EBREAK and an
-- illegal instruction also stop the CPU and produce DONE.  While running,
-- received UART bytes are placed in the one-byte RXDATA register; byte 0x03
-- (ASCII ETX / Ctrl-C) is reserved as an emergency register reset.
--
-- The M extension (MUL, MULH, MULHSU, MULHU, DIV, DIVU, REM, REMU) runs on an
-- iterative unit: about 36 cycles per instruction, one 33-bit add or subtract
-- per cycle.  Division by zero and overflow give the results the RISC-V spec
-- requires.
module SimpleRisc where

import Clash.Prelude

type Word32 = Unsigned 32

type Byte = Unsigned 8

-- | A word address in the 64 KiB memory.
type MemAddr = Unsigned 14

-- | CPU pipeline stage.  Every stage starts from flip-flops and does one kind
-- of work, so the clock can run fast (the design targets 100 MHz on a GW2A):
-- a simple instruction takes Fetch, FetchWait, Decode, Execute and Commit.
-- The BRAM and the UART transmitter are only ever driven from registers, in
-- stages of their own (Fetch, LoadIssue, StoreWrite, TxWrite...): on the GW2A
-- their input timing is worse than nextpnr models.  The
-- values passed between stages live in their own 'Machine' fields, each
-- written by a single stage, rather than in the phase: overlapping them in one
-- register put a many-way mux in front of every bit.
data CpuPhase
  = -- | Put the PC on the BRAM address bus.
    Fetch
  | -- | Latch the instruction word from the BRAM into 'cpuInstruction' and
    -- compare the PC against the end of the program.
    FetchWait
  | -- | Read the operands into 'cpuA' and 'cpuB', or halt if past the end.
    Decode
  | -- | Do the arithmetic into 'cpuComputed'.
    Execute
  | -- | Run the multiply/divide unit ('cpuMulDiv') until it has a result.
    MulDiv
  | -- | Write back, update the PC and start the next fetch or the memory/UART
    -- access.
    Commit
  | -- | Put the load address on the BRAM bus.
    LoadIssue
  | LoadWait
  | -- | Align the loaded word latched in 'cpuMemoryWord'.
    LoadAlign
  | -- | Write a whole word to the BRAM.
    StoreWrite
  | -- | Put a byte/halfword store's address on the BRAM bus to read the old word.
    StoreIssue
  | StoreWait
  | -- | Merge a byte or halfword into the old word latched in 'cpuMemoryWord'.
    StoreMerge
  | -- | Wait for the UART transmitter, then hand it the byte.
    TxWrite
  deriving (Generic, NFDataX, Show, Eq)

-- | Everything an instruction needs from the adders and the ALU, computed in
-- the Execute stage so the Commit stage only selects between registers.
data Computed = Computed
  { -- | Result for LUI, AUIPC, OP-IMM and OP; Nothing for an illegal encoding.
    cAlu :: Maybe Word32,
    -- | PC + 4: the fall-through PC and the JAL/JALR link value.
    cLink :: Word32,
    -- | Jump or taken-branch target.
    cTarget :: Word32,
    -- | Branch condition; Nothing for an illegal funct3.
    cTaken :: Maybe Bool,
    -- | Load/store effective address.
    cAddress :: Word32
  }
  deriving (Generic, NFDataX, Show, Eq)

-- | Where the multiply/divide unit is in an instruction.
data MdStep
  = -- | Take the operands' magnitudes, or answer a division by zero.
    MdPrepare
  | -- | One of 32 shift-and-add (multiply) or restoring-division steps.
    MdLoop (Index 32)
  | -- | Negate the low word if needed, recording the carry out of it.
    MdFixLo
  | -- | Negate the high word if needed, taking that carry.
    MdFixHi
  | -- | Pick the requested half.
    MdFinish
  deriving (Generic, NFDataX, Show, Eq)

-- | The multiply/divide unit's working registers.  A multiply keeps the
-- 64-bit product in 'mdHi' and 'mdLo' (the multiplier shifts out of 'mdLo' as
-- the product shifts in); a division keeps the remainder in 'mdHi' and the
-- quotient in 'mdLo' (the dividend shifts out as quotient bits shift in).
data MulDivUnit = MulDivUnit
  { mdStep :: MdStep,
    mdHi :: Word32,
    mdLo :: Word32,
    -- | The magnitude of the multiplicand or the divisor.
    mdOperand :: Word32,
    -- | Negate the low word (product low half or quotient) at the end.
    mdNegLo :: Bool,
    -- | Negate the high word (product high half or remainder) at the end.
    mdNegHi :: Bool,
    -- | The low word was zero before negation: the carry into the high word.
    mdCarry :: Bool
  }
  deriving (Generic, NFDataX, Show, Eq)

data HostState
  = HostIdle
  | ProgramCountLo
  | ProgramCountHi Byte
  | ProgramBytes (Unsigned 15) MemAddr (Index 4) Word32
  | ClearMemory MemAddr
  deriving (Generic, NFDataX, Show, Eq)

data ReplyState = NoReply | DoneReply (Unsigned 3)
  deriving (Generic, NFDataX, Show, Eq)

data Machine = Machine
  { cpuRegs :: Vec 32 Word32,
    cpuPc :: Word32,
    cpuPhase :: CpuPhase,
    cpuInstruction :: Word32,
    cpuPastEnd :: Bool,
    cpuA :: Word32,
    cpuB :: Word32,
    -- | The instruction's immediate, whichever format it uses.
    cpuImm :: Word32,
    -- | OP (register-register) rather than OP-IMM: the ALU takes rs2, not the
    -- immediate.  Selected in Execute so Decode's register read feeds
    -- flip-flops directly: on the GW2A the 32-way register mux is much slower
    -- than nextpnr models.
    cpuIsRegOp :: Bool,
    cpuComputed :: Computed,
    cpuMulDiv :: MulDivUnit,
    cpuMemoryWord :: Word32,
    cpuRunning :: Bool,
    programEnd :: Word32,
    hostState :: HostState,
    rxHolding :: Maybe Byte,
    replyState :: ReplyState,
    -- | Zero all registers on the next cycle.  Commands and resets set this
    -- rather than clearing the 1024 register bits directly, so the clear is
    -- driven straight from one flip-flop.
    clearRegisters :: Bool
  }
  deriving (Generic, NFDataX)

-- | BRAM read address, optional BRAM write, and an optional UART TX byte.
type StepOutput = (MemAddr, Maybe (MemAddr, Word32), Maybe Byte)

-- | A register-file write requested by the instruction being executed.
type RegisterWrite = Maybe (Index 32, Word32)

initialMachine :: Machine
initialMachine =
  Machine
    { cpuRegs = repeat 0,
      cpuPc = 0,
      cpuPhase = Fetch,
      cpuInstruction = 0,
      cpuPastEnd = False,
      cpuA = 0,
      cpuB = 0,
      cpuImm = 0,
      cpuIsRegOp = False,
      cpuComputed = Computed Nothing 0 0 Nothing 0,
      cpuMulDiv = MulDivUnit MdPrepare 0 0 0 False False False,
      cpuMemoryWord = 0,
      cpuRunning = False,
      programEnd = 0,
      hostState = HostIdle,
      rxHolding = Nothing,
      replyState = NoReply,
      clearRegisters = False
    }

-- UART -----------------------------------------------------------------------

-- 100 MHz / 115200 is 868 clocks per bit (0.007% baud-rate error).  At any
-- other clock f the UART runs at f / 868 baud.
type BaudCounter = Unsigned 10

uartClocksPerBit :: BaudCounter
uartClocksPerBit = 868

fullBit :: BaudCounter
fullBit = uartClocksPerBit - 1

halfBit :: BaudCounter
halfBit = uartClocksPerBit `div` 2 - 1

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
-- XilinxSystem is System with a synchronous reset.  Gowin flip-flops have a
-- synchronous-reset pin, and it keeps nextpnr from timing paths through
-- every flip-flop's asynchronous clear.
topEntity ::
  "CLK" ::: Clock XilinxSystem ->
  "RESET" ::: Reset XilinxSystem ->
  "ENABLE" ::: Enable XilinxSystem ->
  "UART_RX" ::: Signal XilinxSystem Bit ->
  "UART_TX" ::: Signal XilinxSystem Bit
topEntity clk rst en serialRx = withClockResetEnable clk rst en (simpleRisc serialRx)

simpleRisc :: (HiddenClockResetEnable dom) => Signal dom Bit -> Signal dom Bit
simpleRisc serialRx = serialTx
  where
    -- Two flip-flops resynchronise the asynchronous RX pin before sampling, and
    -- the received byte is registered before it reaches the CPU: a Ctrl-C
    -- resets the whole CPU, so that compare fans out to every state bit.
    received = register Nothing (uartRx (register high (register high serialRx)))
    (serialTx, txReady) = uartTx txRequest

    machineInput = bundle (memoryOut, received, txReady)
    machineOutput = mealy machineStep initialMachine machineInput
    (memoryAddress, memoryWrite, txRequest) = unbundle machineOutput

    -- A synchronous 16384 x 32-bit RAM.  Reads take one cycle; writes are visible
    -- on the following read.  This shape maps naturally to FPGA block RAM.
    memoryOut = blockRamPow2 (repeat 0) memoryAddress memoryWrite

-- | Result wires from the machine controller: BRAM read address, optional BRAM
-- write, and an optional byte offered to the UART transmitter.
machineStep ::
  Machine ->
  (Word32, Maybe Byte, Bool) ->
  (Machine, (MemAddr, Maybe (MemAddr, Word32), Maybe Byte))
machineStep machine input =
  let (machine', output) = machineStepCore machine {clearRegisters = False} input
   in (if clearRegisters machine then machine' {cpuRegs = repeat 0} else machine', output)

machineStepCore ::
  Machine ->
  (Word32, Maybe Byte, Bool) ->
  (Machine, (MemAddr, Maybe (MemAddr, Word32), Maybe Byte))
machineStepCore machine (memoryWord, received, txReady) =
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
                clearRegisters = True,
                cpuPc = 0,
                cpuPhase = Fetch,
                cpuRunning = False,
                programEnd = 0,
                rxHolding = Nothing
              }
       in (machine', (address, Just (address, 0), Nothing))

-- Host controller ------------------------------------------------------------

-- Stopped step of the host controller: handles the case when the CPU is not running and the host is idle or programming memory.
-- Basically what runs when machineStep is called and the CPU is not running. Otherwise it should call runningStep
stoppedStep ::
  Machine ->
  Maybe Byte ->
  Bool ->
  (Machine, (MemAddr, Maybe (MemAddr, Word32), Maybe Byte))
stoppedStep machine received txReady =
  let (machineWithReply, replyByte) = sendReply machine txReady
   in case (hostState machineWithReply, received) of
        (HostIdle, Just 0x50) -> idleOut machineWithReply {hostState = ProgramCountLo} replyByte
        (HostIdle, Just 0x52) -> idleOut machineWithReply {clearRegisters = True, cpuPc = 0, cpuPhase = Fetch, cpuRunning = programEnd machineWithReply /= 0, rxHolding = Nothing} replyByte
        (HostIdle, Just 0x58) -> idleOut (resetCpu machineWithReply) replyByte
        (HostIdle, Just 0x4d) -> idleOut (resetCpu machineWithReply) {hostState = ClearMemory 0, programEnd = 0, replyState = NoReply} Nothing
        (ProgramCountLo, Just lowByte) -> idleOut machineWithReply {hostState = ProgramCountHi lowByte} replyByte
        (ProgramCountHi lowByte, Just highByte) ->
          -- Clamp to the 16384-word memory with bit tests rather than
          -- comparators, which synthesise to carry chains.
          let requested = (resize highByte `shiftL` 8) .|. resize lowByte :: Unsigned 16
              tooMany = requested .&. 0xc000 /= 0
              count = if tooMany then 16384 else resize requested :: Unsigned 15
              nextHost = if requested == 0 then HostIdle else ProgramBytes count 0 0 0
           in idleOut machineWithReply {hostState = nextHost, programEnd = 0, clearRegisters = True, cpuPc = 0, cpuPhase = Fetch} replyByte
        (ProgramBytes wordsLeft address byteNo partial, Just byte) ->
          -- Bytes arrive least significant first: shift each one in from the top.
          let partial' = (resize byte `shiftL` 24) .|. (partial `shiftR` 8)
           in if byteNo == maxBound
                then
                  let isLast = wordsLeft == 1
                      machine' = machineWithReply {hostState = if isLast then HostIdle else ProgramBytes (wordsLeft - 1) (address + 1) 0 0, programEnd = if isLast then (resize address + 1) `shiftL` 2 else programEnd machineWithReply}
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
    { clearRegisters = True,
      cpuPc = 0,
      cpuPhase = Fetch,
      cpuInstruction = 0,
      cpuPastEnd = False,
      cpuA = 0,
      cpuB = 0,
      cpuImm = 0,
      cpuIsRegOp = False,
      cpuComputed = Computed Nothing 0 0 Nothing 0,
      cpuMemoryWord = 0,
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
runningStep machine memoryWord received txReady =
  let (machine', registerWrite, output) = cpuStep machine memoryWord received txReady
   in (maybe machine' (\(index, value) -> writeRegister index value machine') registerWrite, output)

-- | One CPU cycle.  Register writes are returned rather than applied in each
-- branch, so the generated hardware has a single register-file write port
-- instead of a full copy of all 32 registers per instruction kind.
cpuStep :: Machine -> Word32 -> Maybe Byte -> Bool -> (Machine, RegisterWrite, StepOutput)
cpuStep machine memoryWord received txReady
  | received == Just 0x03 = (resetCpu machine, Nothing, (0, Nothing, Nothing))
  | otherwise =
      let machineRx = case (received, rxHolding machine) of
            (Just byte, Nothing) -> machine {rxHolding = Just byte}
            _ -> machine
          next m = withoutRegister (m, (0, Nothing, Nothing))
       in let Machine {..} = machineRx
              instruction = cpuInstruction
              address = cAddress cpuComputed
           in case cpuPhase of
               Fetch ->
                 withoutRegister
                   ( machineRx {cpuPhase = FetchWait},
                     (memoryIndex cpuPc, Nothing, Nothing)
                   )
               -- The program stops once control leaves it.  The compare is
               -- registered here and acted on in Decode.
               FetchWait -> next machineRx {cpuPhase = Decode, cpuInstruction = memoryWord, cpuPastEnd = cpuPc >= programEnd}
               Decode
                 | cpuPastEnd -> withoutRegister (haltMachine machineRx)
                 | otherwise ->
                     next
                       machineRx
                         { cpuPhase = Execute,
                           cpuA = readRegister (instructionRs1 instruction) machineRx,
                           cpuB = readRegister (instructionRs2 instruction) machineRx,
                           cpuImm = decodeImmediate instruction,
                           cpuIsRegOp = instruction .&. 0x7f == 0x33
                         }
               Execute ->
                 let op2 = if cpuIsRegOp then cpuB else cpuImm
                     -- OP with funct7 = 1 is the M extension.
                     isMulDiv = cpuIsRegOp && instruction `shiftR` 25 == 1
                  in next
                       machineRx
                         { cpuPhase = if isMulDiv then MulDiv else Commit,
                           cpuComputed = compute cpuPc instruction cpuA cpuB cpuImm op2,
                           cpuMulDiv = cpuMulDiv {mdStep = MdPrepare}
                         }
               MulDiv ->
                 case mulDivStep (instructionFunct3 instruction) cpuA cpuB cpuMulDiv of
                   Left unit -> next machineRx {cpuMulDiv = unit}
                   Right result -> next machineRx {cpuPhase = Commit, cpuComputed = cpuComputed {cAlu = Just result}}
               Commit -> commitInstruction machineRx txReady
               LoadIssue -> withoutRegister (machineRx {cpuPhase = LoadWait}, (memoryIndex address, Nothing, Nothing))
               LoadWait -> next machineRx {cpuPhase = LoadAlign, cpuMemoryWord = memoryWord}
               StoreWrite -> withoutRegister (finishInstruction machineRx (Just (memoryIndex address, cpuB)))
               StoreIssue -> withoutRegister (machineRx {cpuPhase = StoreWait}, (memoryIndex address, Nothing, Nothing))
               TxWrite
                 | txReady -> withoutRegister (finishInstructionWithTx machineRx (Just (resize cpuB)))
                 | otherwise -> next machineRx
               LoadAlign ->
                 let value = loadValue (instructionFunct3 instruction) (resize address) cpuMemoryWord
                  in withRegister (instructionRd instruction) value (finishInstruction machineRx Nothing)
               StoreWait -> next machineRx {cpuPhase = StoreMerge, cpuMemoryWord = memoryWord}
               StoreMerge ->
                 let merged = storeValue (instructionFunct3 instruction) (resize address) cpuB cpuMemoryWord
                  in withoutRegister (finishInstruction machineRx (Just (memoryIndex address, merged)))

withRegister :: Index 32 -> Word32 -> (Machine, StepOutput) -> (Machine, RegisterWrite, StepOutput)
withRegister index value (machine, output) = (machine, Just (index, value), output)

withoutRegister :: (Machine, StepOutput) -> (Machine, RegisterWrite, StepOutput)
withoutRegister (machine, output) = (machine, Nothing, output)

-- | Execute stage: every adder and the ALU, in parallel, for whichever
-- instruction this turns out to be.
-- The immediate has already been decoded for the instruction's format, so
-- each adder has fixed, registered inputs and nothing is muxed in front of it.
compute :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Computed
compute pc instruction a b imm op2 =
  Computed
    { cAlu = case opcode of
        0x37 -> Just imm -- LUI
        0x17 -> Just pcPlusImm -- AUIPC
        0x13 -> alu funct3 funct7 False a op2
        0x33 -> alu funct3 funct7 True a op2
        _ -> Nothing,
      cLink = pc + 4,
      cTarget = if opcode == 0x67 then aPlusImm .&. complement 1 else pcPlusImm, -- JALR; JAL and branches
      cTaken = branchTaken funct3 a b,
      cAddress = aPlusImm
    }
  where
    opcode = instruction .&. 0x7f
    funct3 = instructionFunct3 instruction
    funct7 = (instruction `shiftR` 25) .&. 0x7f
    pcPlusImm = pc + imm
    aPlusImm = a + imm

-- | The immediate in whichever format the opcode uses.
decodeImmediate :: Word32 -> Word32
decodeImmediate instruction = case instruction .&. 0x7f of
  0x37 -> immU instruction -- LUI
  0x17 -> immU instruction -- AUIPC
  0x6f -> immJ instruction -- JAL
  0x63 -> immB instruction -- branches
  0x23 -> immS instruction -- stores
  _ -> immI instruction -- OP-IMM, loads, JALR

instructionRd, instructionRs1, instructionRs2 :: Word32 -> Index 32
instructionRd instruction = regIndex ((instruction `shiftR` 7) .&. 0x1f)
instructionRs1 instruction = regIndex ((instruction `shiftR` 15) .&. 0x1f)
instructionRs2 instruction = regIndex ((instruction `shiftR` 20) .&. 0x1f)

instructionFunct3 :: Word32 -> BitVector 3
instructionFunct3 instruction = pack (resize ((instruction `shiftR` 12) .&. 7) :: Unsigned 3)

-- | Commit stage: act on the values the Execute stage computed.
commitInstruction :: Machine -> Bool -> (Machine, RegisterWrite, StepOutput)
commitInstruction machine txReady =
  let instruction = cpuInstruction machine
      Computed {..} = cpuComputed machine
      opcode = instruction .&. 0x7f
      rd = instructionRd instruction
      funct3 = instructionFunct3 instruction
      nextPc = cLink
      normal = case cAlu of
        Just value -> withRegister rd value (finishInstruction machine {cpuPc = nextPc} Nothing)
        Nothing -> invalid
      noWrite newPc = withoutRegister (finishInstruction machine {cpuPc = newPc} Nothing)
      jumpAndLink newPc = withRegister rd nextPc (finishInstruction machine {cpuPc = newPc} Nothing)
      invalid = withoutRegister (haltMachine machine)
      address = cAddress
   in case opcode of
        0x37 -> normal -- LUI
        0x17 -> normal -- AUIPC
        0x13 -> normal -- OP-IMM
        0x33 -> normal -- OP
        -- JAL
        0x6f -> jumpAndLink cTarget
        -- JALR
        0x67 -> if funct3 == 0 then jumpAndLink cTarget else invalid
        -- branches
        0x63 ->
          case cTaken of
            Just takeBranch -> noWrite (if takeBranch then cTarget else nextPc)
            Nothing -> invalid
        -- loads
        0x03 ->
          if isUartAddress address
            then
              let (value, machine') = readUart address txReady machine
               in withRegister rd value (finishInstruction machine' {cpuPc = nextPc} Nothing)
            else
              if validMemoryAddress address && validLoad funct3
                then
                  withoutRegister (machine {cpuPc = nextPc, cpuPhase = LoadIssue}, (0, Nothing, Nothing))
                else invalid
        -- stores
        0x23 ->
          if address == uartTxData
            then
              withoutRegister (machine {cpuPc = nextPc, cpuPhase = TxWrite}, (0, Nothing, Nothing))
            else
              if validMemoryAddress address && validStore funct3
                then
                  -- Word stores need no read/modify/write cycle.
                  let phase = if funct3 == 0b010 then StoreWrite else StoreIssue
                   in withoutRegister (machine {cpuPc = nextPc, cpuPhase = phase}, (0, Nothing, Nothing))
                else invalid
        0x0f -> noWrite nextPc -- FENCE is a no-op in this tiny single-master core.
        0x73 -> invalid -- ECALL / EBREAK terminate the program.
        _ -> invalid

finishInstruction :: Machine -> Maybe (MemAddr, Word32) -> (Machine, StepOutput)
finishInstruction machine write = finishWith machine write Nothing

finishInstructionWithTx :: Machine -> Maybe Byte -> (Machine, StepOutput)
finishInstructionWithTx machine request = finishWith machine Nothing request

-- | Retire an instruction.  The next fetch starts in the Fetch stage from the
-- PC register, a cycle after any write here, so it always sees the new word.
-- Whether that PC has left the program is checked in 'cpuStep'.
finishWith :: Machine -> Maybe (MemAddr, Word32) -> Maybe Byte -> (Machine, StepOutput)
finishWith machine write request = (machine {cpuPhase = Fetch}, (0, write, request))

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
validMemoryAddress address = address .&. 0xffff_0000 == 0 -- < 64 KiB, as a bit test

uartTxData, uartStatus, uartRxData :: Word32
uartTxData = 0x1000_0000
uartStatus = 0x1000_0004
uartRxData = 0x1000_0008

isUartAddress :: Word32 -> Bool
isUartAddress address = address == uartTxData || address == uartStatus || address == uartRxData

-- read an UART register
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
    hasByte (Just {}) = True

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

-- | OP (register-register) and OP-IMM arithmetic on @a@ and @op2@; Nothing for
-- an illegal funct7.
alu :: BitVector 3 -> Word32 -> Bool -> Word32 -> Word32 -> Maybe Word32
alu funct3 funct7 isRegister a op2 = if legal then Just value else Nothing
  where
    alternate = funct7 == 0x20 -- SUB and SRA/SRAI
    isShift = funct3 == 0b001 || funct3 == 0b101
    legal
      | isRegister = funct7 == 0 || (alternate && (funct3 == 0b000 || funct3 == 0b101))
      | otherwise = not isShift || funct7 == 0 || (alternate && funct3 == 0b101)
    shamt = fromIntegral (op2 .&. 0x1f)
    value = case funct3 of
      0b000 -> if isRegister && alternate then a - op2 else a + op2 -- ADD(I), SUB
      0b001 -> a `shiftL` shamt -- SLL(I)
      0b010 -> boolWord (signed32 a < signed32 op2) -- SLT(I)
      0b011 -> boolWord (a < op2) -- SLT(I)U
      0b100 -> a `xor` op2 -- XOR(I)
      0b101 -> if alternate then unsigned32 (signed32 a `shiftR` shamt) else a `shiftR` shamt -- SRL(I), SRA(I)
      0b110 -> a .|. op2 -- OR(I)
      _ -> a .&. op2 -- AND(I)

-- | One cycle of the multiply/divide unit for the M instruction @funct3@ on
-- operands @a@ (rs1) and @b@ (rs2): the next unit state, or the result.
mulDivStep :: BitVector 3 -> Word32 -> Word32 -> MulDivUnit -> Either MulDivUnit Word32
mulDivStep funct3 a b unit@MulDivUnit {..} = case mdStep of
  MdPrepare
    -- Division by zero: quotient all ones, remainder the dividend.
    | isDiv && b == 0 -> Left unit {mdStep = MdFinish, mdLo = maxBound, mdHi = a}
    | otherwise ->
        let magnitude negative x = if negative then negate x else x
         in Left
              MulDivUnit
                { mdStep = MdLoop 0,
                  mdHi = 0,
                  mdLo = if isDiv then magnitude aNegative a else magnitude bNegative b,
                  mdOperand = if isDiv then magnitude bNegative b else magnitude aNegative a,
                  mdNegLo = aNegative /= bNegative,
                  mdNegHi = if isDiv then aNegative else aNegative /= bNegative,
                  mdCarry = False
                }
  MdLoop i ->
    let (hi, lo) = if isDiv then divideStep else multiplyStep
     in Left unit {mdStep = if i == maxBound then MdFixLo else MdLoop (succ i), mdHi = hi, mdLo = lo}
  MdFixLo -> Left unit {mdStep = MdFixHi, mdLo = if mdNegLo then negate mdLo else mdLo, mdCarry = mdLo == 0}
  -- Negating the remainder is complement + 1; negating the product's high
  -- half is complement + the carry out of the low half.
  MdFixHi -> Left unit {mdStep = MdFinish, mdHi = if mdNegHi then complement mdHi + boolWord (isDiv || mdCarry) else mdHi}
  -- MUL, DIV and DIVU take the low word; MULH*, REM and REMU the high word.
  MdFinish -> Right (if funct3 == 0b000 || funct3 == 0b100 || funct3 == 0b101 then mdLo else mdHi)
  where
    isDiv = testBit funct3 2
    -- MULH, MULHSU, DIV and REM treat rs1 as signed; MULH, DIV and REM rs2.
    aSigned = funct3 == 0b001 || funct3 == 0b010 || funct3 == 0b100 || funct3 == 0b110
    bSigned = funct3 == 0b001 || funct3 == 0b100 || funct3 == 0b110
    aNegative = aSigned && testBit a 31
    bNegative = bSigned && testBit b 31
    -- Add the multiplicand into the high half if the multiplier's low bit is
    -- set, then shift the 65-bit {carry, hi, lo} right by one.
    multiplyStep =
      let total = if testBit mdLo 0 then resize mdHi + resize mdOperand else resize mdHi :: Unsigned 33
       in (resize (total `shiftR` 1), (mdLo `shiftR` 1) .|. (if testBit total 0 then 0x8000_0000 else 0))
    -- Shift the next dividend bit into the remainder; subtract the divisor if
    -- it fits, recording a quotient bit.
    divideStep =
      let remainder = (resize mdHi `shiftL` 1) .|. (if testBit mdLo 31 then 1 else 0) :: Unsigned 33
          fits = remainder >= resize mdOperand
       in (resize (if fits then remainder - resize mdOperand else remainder), (mdLo `shiftL` 1) .|. boolWord fits)

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

-- NEEDED FOR SIGN EXTENSION OF VALUES SINCE THE REGISTER SIZE IS 32 BITS

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

-- DIFFERENT FORMATS have different immediate extraction methods

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
