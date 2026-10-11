{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with maybe" #-}

-- | A deliberately small, unpipelined RV32IM computer.
--
-- Memory map:
--
--   * 0x0000_0000 - 0x0000_07ff: 2 KiB boot ROM (the reset vector)
--   * 0x0010_0000: finisher (QEMU virt style: 0x5555 = exit 0,
--     code << 16 | 0x3333 = exit code)
--   * 0x0010_0004: BOOTCAUSE, read-only; a read clears it (0 none, 1 power-on,
--     2 Ctrl-C, 3 finisher exit)
--   * 0x1000_0000: UART TXDATA (write the low byte)
--   * 0x1000_0004: UART STATUS (bit 0 = TX ready, bit 1 = RX byte ready)
--   * 0x1000_0008: UART RXDATA (read low byte, consuming it)
--   * 0x8000_0000 - 0x8000_ffff: 64 KiB RAM
--
-- Everything else raises an access fault.  The physical UART is 8-N-1 at
-- clock / 868 baud: 115200 at 100 MHz.
--
-- The CPU is only a CPU: it runs from reset at address 0, in the boot ROM
-- (boards/tangnano20k/bootrom), which implements the host protocol in
-- software -- 'P' to load words at 0x8000_0000, 'R' to run them, 'M' to clear
-- RAM -- and sends "DONE" when a program ends.  A program ends by writing the
-- finisher, which resets the CPU into the ROM with cause EXIT, or by taking a
-- trap while mtvec is 0, which lands in the ROM directly.  While a program
-- runs, received byte 0x03 (Ctrl-C) never reaches it: it resets the CPU into
-- the ROM with cause BREAK.  In the ROM 0x03 is an ordinary byte, so images
-- containing it load intact.  Resets keep RAM.
--
-- ECALL, EBREAK, illegal instructions, misaligned and faulting accesses enter
-- the machine trap handler.  CSR instructions and MRET support direct-mode
-- handlers and split 64-bit cycle/retirement counters.  Interrupt sources are
-- still pending.
--
-- The M extension (MUL, MULH, MULHSU, MULHU, DIV, DIVU, REM, REMU) runs on an
-- iterative unit: 68 unit cycles per nonzero-divisor instruction, with each
-- 33-bit add/subtract followed by a separate registered commit.  Division by zero and overflow give the results the RISC-V spec
-- requires.
module SimpleRisc where

import Clash.Prelude
import Clash.Annotations.BitRepresentation (DataReprAnn(..), ConstrRepr(..), liftQ)
import Language.Haskell.TH.Syntax (addDependentFile, runIO)
import qualified Prelude as P

type Word32 = Unsigned 32

type Byte = Unsigned 8

-- | A physical word index: bit 14 selects the boot ROM (512 words), otherwise
-- the low 14 bits index the 64 KiB RAM.
type MemAddr = Unsigned 15

-- | The boot ROM image, embedded at compile time from the output of
-- boards/tangnano20k/bootrom/build.sh.
bootRomContents :: Vec 512 Word32
bootRomContents =
  $( do
       let path = "boards/tangnano20k/bootrom/bootrom.hex"
       addDependentFile path
       text <- runIO (P.readFile path)
       let romWords = P.map (\line -> P.read ("0x" P.++ line) :: P.Integer) (P.lines text)
       if P.length romWords P./= 512
         then P.fail (path P.++ ": expected 512 words")
         else listToVecTH (P.map P.fromInteger romWords :: [Unsigned 32])
   )

-- | CPU pipeline stage.  Every stage starts from flip-flops and does one kind
-- of work, so the clock can run fast (the design targets 100 MHz on a GW2A):
-- Fetch and register reads retain their existing registered stages.
-- OperandSelect registers ALU controls; ExecuteFinish selects registered results.
-- Ordinary instructions take nine cycles; shifts insert ShiftFinish for ten.
-- The BRAM and the UART transmitter are only ever driven from registers, in
-- stages of their own (Fetch, BusLoadIssue, BusRamWrite, BusTx...): on the GW2A
-- their input timing is worse than nextpnr models.  The
-- values passed between stages live in their own 'Machine' fields, each
-- written by a single stage, rather than in the phase: overlapping them in one
-- register put a many-way mux in front of every bit.
data CpuPhase
  = Fetch
  | FetchDelay
  | FetchCapture
  | FetchWait
  | Decode
  | OperandSelect
  | Execute
  | ExecuteFinish
  | ShiftFinish
  | MulDiv
  | Commit
  | Trap
  | CsrRead
  | CsrWrite
  | TrapReturn
  | BusWait
  deriving (Generic, NFDataX, Show, Eq)

-- One dedicated state bit per CPU stage.
{-# ANN module (DataReprAnn $(liftQ [t| CpuPhase |]) 16
  [ ConstrRepr 'Fetch 1 1 [],
    ConstrRepr 'FetchDelay 2 2 [],
    ConstrRepr 'FetchCapture 4 4 [],
    ConstrRepr 'FetchWait 8 8 [],
    ConstrRepr 'Decode 16 16 [],
    ConstrRepr 'OperandSelect 32 32 [],
    ConstrRepr 'Execute 64 64 [],
    ConstrRepr 'ShiftFinish 128 128 [],
    ConstrRepr 'MulDiv 256 256 [],
    ConstrRepr 'Commit 512 512 [],
    ConstrRepr 'Trap 1024 1024 [],
    ConstrRepr 'CsrRead 2048 2048 [],
    ConstrRepr 'CsrWrite 4096 4096 [],
    ConstrRepr 'TrapReturn 8192 8192 [],
    ConstrRepr 'BusWait 16384 16384 [],
    ConstrRepr 'ExecuteFinish 32768 32768 []
  ]) #-}

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
  | -- | Commit the registered adder/subtractor result, separately from arithmetic.
    MdCommit (Index 32)
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
    mdTotal :: Unsigned 33,
    -- | Negate the low word (product low half or quotient) at the end.
    mdNegLo :: Bool,
    -- | Negate the high word (product high half or remainder) at the end.
    mdNegHi :: Bool,
    -- | The low word was zero before negation: the carry into the high word.
    mdCarry :: Bool
  }
  deriving (Generic, NFDataX, Show, Eq)

-- | Why the CPU last entered the boot ROM through a reset.  BOOTCAUSE reads
-- it and clears it to 'causeNone', so the ROM can tell a reset from a trap.
causeNone, causePowerOn, causeBreak, causeExit :: Unsigned 2
causeNone = 0
causePowerOn = 1
causeBreak = 2
causeExit = 3

-- | Software-visible machine-mode trap registers. Only machine mode is used.
data TrapRegisters = TrapRegisters
  { trapVector :: Word32,
    trapEpc :: Word32,
    trapCause :: Word32,
    trapValue :: Word32,
    trapMie :: Bool,
    trapMpie :: Bool
  }
  deriving (Generic, NFDataX, Show, Eq)

initialTrapRegisters :: TrapRegisters
initialTrapRegisters = TrapRegisters 0 0 0 0 False False

-- | The low word increments independently; its overflow is registered and
-- applied to the high word on the following clock. CSR reads wait out that
-- single carry cycle rather than adding another carry chain to the read mux.
data Counter64 = Counter64
  { counterLo :: Word32,
    counterHi :: Word32,
    counterCarry :: Bool
  }
  deriving (Generic, NFDataX, Show, Eq)

initialCounter :: Counter64
initialCounter = Counter64 0 0 False

-- Explicit writes suppress the increment and change only the selected half.
-- A low-half write still drains an overflow from the preceding clock; a
-- high-half write replaces that high half, including any preceding carry.
tickCounter :: Bool -> Maybe (Bool, Word32) -> Counter64 -> Counter64
tickCounter increment write counter =
  let lo = counterLo counter
      hi = counterHi counter
      carriedHi = if counterCarry counter then hi + 1 else hi
   in case write of
        Just (False, value) -> Counter64 value carriedHi False
        Just (True, value) -> Counter64 lo value False
        Nothing -> Counter64
          (if increment then lo + 1 else lo)
          carriedHi
          (increment && lo == maxBound)

-- Registered memory target; wide address comparisons stay out of Commit.
data MemoryTarget = RamTarget | RomTarget | UartTxTarget | UartStatusTarget | UartRxTarget | ExitTarget | BootCauseTarget | UnmappedTarget
  deriving (Generic, NFDataX, Show, Eq)

-- Single outstanding data transaction. The CPU registers the request and
-- waits for a registered response; the bus alone owns device side effects.
data BusRequest = BusRequest
  { busAddress :: Word32, busWidth :: BitVector 3,
    busWrite :: Bool, busData :: Word32 }
  deriving (Generic, NFDataX, Show, Eq)

data BusResponse = BusResponse
  { responseValue :: Word32, responseFault :: Bool, responseExit :: Bool }
  deriving (Generic, NFDataX, Show, Eq)

data BusPhase = BusIdle | BusDecode | BusDispatch
  | BusLoadIssue | BusLoadDelay | BusLoadCapture | BusLoadWait | BusLoadAlign
  | BusReply | BusStoreIssue | BusStoreDelay | BusStoreCapture | BusStoreWait
  | BusStoreMerge | BusRamWrite | BusTx
  deriving (Generic, NFDataX, Show, Eq)

{-# ANN module (DataReprAnn $(liftQ [t| BusPhase |]) 16
  [ ConstrRepr 'BusIdle 1 1 [],
    ConstrRepr 'BusDecode 2 2 [],
    ConstrRepr 'BusDispatch 4 4 [],
    ConstrRepr 'BusLoadIssue 8 8 [],
    ConstrRepr 'BusLoadDelay 16 16 [],
    ConstrRepr 'BusLoadCapture 32 32 [],
    ConstrRepr 'BusLoadWait 64 64 [],
    ConstrRepr 'BusLoadAlign 128 128 [],
    ConstrRepr 'BusReply 256 256 [],
    ConstrRepr 'BusStoreIssue 512 512 [],
    ConstrRepr 'BusStoreDelay 1024 1024 [],
    ConstrRepr 'BusStoreCapture 2048 2048 [],
    ConstrRepr 'BusStoreWait 4096 4096 [],
    ConstrRepr 'BusStoreMerge 8192 8192 [],
    ConstrRepr 'BusRamWrite 16384 16384 [],
    ConstrRepr 'BusTx 32768 32768 []
  ]) #-}

data BusState = BusState
  { busPhase :: BusPhase, busRequest :: BusRequest, busTarget :: MemoryTarget,
    busWord :: Word32, busValue :: Word32 }
  deriving (Generic, NFDataX)

initialBus :: BusState
initialBus = BusState BusIdle (BusRequest 0 0 False 0) UnmappedTarget 0 0

-- | The side effect of a completed device read.
data BusEffect = NoEffect | ConsumeRx | ConsumeBootCause
  deriving (Generic, NFDataX, Show, Eq)

-- Response and effects are pulses. A stalled TX emits neither; a completed
-- transfer emits exactly one response and, where appropriate, one side effect.
busStep :: BusState -> Maybe BusRequest -> Word32 -> Bool -> Maybe Byte -> Unsigned 2 ->
  (BusState, Maybe BusResponse, StepOutput, BusEffect)
busStep = busStepWith decodeMemoryTarget

busStepWith :: (Word32 -> MemoryTarget) -> BusState -> Maybe BusRequest -> Word32 -> Bool -> Maybe Byte -> Unsigned 2 ->
  (BusState, Maybe BusResponse, StepOutput, BusEffect)
busStepWith decodeTarget state request memoryWord txReady rx bootCause =
  let q = busRequest state
      width = busWidth q
      address = busAddress q
      advance phase = (state {busPhase = phase}, Nothing, (0, Nothing, Nothing), NoEffect)
      respond value fault exiting effect =
        (state {busPhase = BusIdle}, Just (BusResponse value fault exiting), (0, Nothing, Nothing), effect)
      valid = (if busWrite q then validStore width else validLoad width)
        && not (misalignedMemory width address)
   in case busPhase state of
        BusIdle -> case request of
          Nothing -> advance BusIdle
          Just new -> (state {busPhase = BusDecode, busRequest = new}, Nothing, (0, Nothing, Nothing), NoEffect)
        BusDecode -> (state {busPhase = BusDispatch, busTarget = decodeTarget address}, Nothing, (0, Nothing, Nothing), NoEffect)
        BusDispatch
          | not valid -> respond 0 True False NoEffect
          | otherwise -> case busTarget state of
              RamTarget
                | not (busWrite q) -> advance BusLoadIssue
                | width == 2 -> (state {busPhase = BusRamWrite, busValue = busData q}, Nothing, (0, Nothing, Nothing), NoEffect)
                | otherwise -> advance BusStoreIssue
              RomTarget
                | busWrite q -> respond 0 True False NoEffect
                | otherwise -> advance BusLoadIssue
              UartTxTarget
                | busWrite q -> advance BusTx
                | otherwise -> respond 0 False False NoEffect
              UartStatusTarget
                | busWrite q -> respond 0 True False NoEffect
                | otherwise -> respond (loadValue width 0
                    ((if txReady then 1 else 0) .|. (case rx of Nothing -> 0; Just _ -> 2))) False False NoEffect
              UartRxTarget
                | busWrite q -> respond 0 True False NoEffect
                | otherwise -> respond (loadValue width 0 (maybe 0 resize rx)) False False
                    (case rx of Nothing -> NoEffect; Just _ -> ConsumeRx)
              ExitTarget
                | width == 0 || width == 4 -> respond 0 True False NoEffect
                | not (busWrite q) -> respond 0 False False NoEffect
                | otherwise -> let status = busData q .&. 0xffff
                    in respond 0 False (status == 0x5555 || status == 0x3333) NoEffect
              BootCauseTarget
                | busWrite q -> respond 0 True False NoEffect
                | otherwise -> respond (loadValue width 0 (resize bootCause)) False False ConsumeBootCause
              UnmappedTarget -> respond 0 True False NoEffect
        BusLoadIssue -> (state {busPhase = BusLoadDelay}, Nothing, (memoryIndex address, Nothing, Nothing), NoEffect)
        BusLoadDelay -> advance BusLoadCapture
        BusLoadCapture -> advance BusLoadWait
        BusLoadWait -> (state {busPhase = BusLoadAlign, busWord = memoryWord}, Nothing, (0, Nothing, Nothing), NoEffect)
        BusLoadAlign -> (state {busPhase = BusReply,
            busValue = loadValue width (resize address) (busWord state)}, Nothing, (0, Nothing, Nothing), NoEffect)
        BusReply -> respond (busValue state) False False NoEffect
        BusStoreIssue -> (state {busPhase = BusStoreDelay}, Nothing, (memoryIndex address, Nothing, Nothing), NoEffect)
        BusStoreDelay -> advance BusStoreCapture
        BusStoreCapture -> advance BusStoreWait
        BusStoreWait -> (state {busPhase = BusStoreMerge, busWord = memoryWord}, Nothing, (0, Nothing, Nothing), NoEffect)
        BusStoreMerge -> (state {busPhase = BusRamWrite,
            busValue = storeValue width (resize address) (busData q) (busWord state)}, Nothing, (0, Nothing, Nothing), NoEffect)
        BusRamWrite -> (state {busPhase = BusIdle}, Just (BusResponse 0 False False),
            (0, Just (memoryIndex address, busValue state), Nothing), NoEffect)
        BusTx
          | txReady -> (state {busPhase = BusIdle}, Just (BusResponse 0 False False),
              (0, Nothing, Just (resize (busData q))), NoEffect)
          | otherwise -> advance BusTx

-- Decode and operand selection are registered before the ALU carry chains;
-- the result mux runs in a separate stage after all candidates are registered.
data ExecuteControl = ExecuteControl
  { execOpcode :: Word32, execFunct3 :: BitVector 3, execLegal :: Bool,
    execSubtract :: Bool, execArithmeticShift :: Bool,
    execShift :: Bool, execMulDiv :: Bool }
  deriving (Generic, NFDataX)

initialExecuteControl :: ExecuteControl
initialExecuteControl = ExecuteControl 0 0 False False False False False

decodeExecuteControl :: Word32 -> ExecuteControl
decodeExecuteControl instruction =
  let opcode = instruction .&. 0x7f
      funct3 = instructionFunct3 instruction
      funct7 = (instruction `shiftR` 25) .&. 0x7f
      regOperation = opcode == 0x33
      shiftOperation = funct3 == 1 || funct3 == 5
      alternate = funct7 == 0x20
      legal = if regOperation
        then funct7 == 0 || (alternate && (funct3 == 0 || funct3 == 5))
        else not shiftOperation || funct7 == 0 || (alternate && funct3 == 5)
   in ExecuteControl opcode funct3 legal (regOperation && alternate) alternate
        ((opcode == 0x13 || regOperation) && shiftOperation) (regOperation && funct7 == 1)

-- High and low comparisons run in parallel; their registered results are
-- combined in ExecuteFinish, avoiding a full 32-bit comparison carry chain.
data CompareResults = CompareResults
  { compareEqual :: Bool, compareHighEqual :: Bool, compareLowLess :: Bool,
    compareSignedHighLess :: Bool, compareUnsignedHighLess :: Bool }
  deriving (Generic, NFDataX)

initialCompareResults :: CompareResults
initialCompareResults = CompareResults False False False False False

compareOperands :: Word32 -> Word32 -> CompareResults
compareOperands a b =
  let highA = resize (a `shiftR` 16) :: Unsigned 16
      highB = resize (b `shiftR` 16) :: Unsigned 16
      lowA = resize a :: Unsigned 16
      lowB = resize b :: Unsigned 16
   in CompareResults (a == b) (highA == highB) (lowA < lowB)
        ((bitCoerce highA :: Signed 16) < (bitCoerce highB :: Signed 16)) (highA < highB)

signedComparison, unsignedComparison :: CompareResults -> Bool
signedComparison parts = compareSignedHighLess parts || (compareHighEqual parts && compareLowLess parts)
unsignedComparison parts = compareUnsignedHighLess parts || (compareHighEqual parts && compareLowLess parts)

branchTakenPrepared :: BitVector 3 -> CompareResults -> Maybe Bool
branchTakenPrepared kind parts = case kind of
  0 -> Just (compareEqual parts)
  1 -> Just (not (compareEqual parts))
  4 -> Just (signedComparison parts)
  5 -> Just (not (signedComparison parts))
  6 -> Just (unsignedComparison parts)
  7 -> Just (not (unsignedComparison parts))
  _ -> Nothing

aluCandidates :: ExecuteControl -> Word32 -> Word32 -> Vec 8 Word32
aluCandidates control a b =
  (if execSubtract control then a - b else a + b) :>
  (a `shiftL` fromIntegral (b .&. 3)) :>
  0 :> 0 :>
  (a `xor` b) :>
  (if execArithmeticShift control
    then unsigned32 (signed32 a `shiftR` fromIntegral (b .&. 3))
    else a `shiftR` fromIntegral (b .&. 3)) :>
  (a .|. b) :> (a .&. b) :> Nil

selectedAlu :: ExecuteControl -> Vec 8 Word32 -> CompareResults -> Maybe Word32
selectedAlu control candidates parts = if execLegal control then Just value else Nothing
  where
    value = case execFunct3 control of
      2 -> boolWord (signedComparison parts)
      3 -> boolWord (unsignedComparison parts)
      _ -> candidates !! (fromIntegral (unpack (execFunct3 control) :: Unsigned 3) :: Index 8)

data Machine = Machine
  { cpuRegs :: Vec 32 Word32,
    -- | Registered writeback request; consumed during the next fetch.
    cpuPendingWrite :: RegisterWrite,
    cpuPc :: Word32,
    cpuPhase :: CpuPhase,
    cpuInstruction :: Word32,
    -- | The PC being fetched is outside the ROM and RAM; registered at Fetch
    -- and raised as an instruction access fault at Decode.
    cpuFetchFault :: Bool,
    -- | Executing boot ROM code, registered at Fetch. Ctrl-C only breaks
    -- programs: in the ROM a 0x03 byte is data, such as part of an image.
    cpuInRom :: Bool,
    cpuABanks :: Vec 4 Word32,
    cpuBBanks :: Vec 4 Word32,
    cpuA :: Word32,
    cpuB :: Word32,
    -- | The instruction's immediate, whichever format it uses.
    cpuImm :: Word32,
    -- | OP (register-register) rather than OP-IMM: the ALU takes rs2, not the
    -- immediate. Selected in OperandSelect so Decode's register read feeds
    -- bank flip-flops directly; OperandSelect completes the read next cycle.
    cpuIsRegOp :: Bool,
    cpuOp2 :: Word32,
    cpuExecuteControl :: ExecuteControl,
    cpuAluCandidates :: Vec 8 Word32,
    cpuAluCompare :: CompareResults,
    cpuBranchCompare :: CompareResults,
    cpuComputed :: Computed,
    cpuTrapRegisters :: TrapRegisters,
    cpuExceptionCause :: Word32,
    cpuExceptionValue :: Word32,
    cpuScratch :: Word32,
    cpuInterruptEnable :: Word32,
    cpuCsrSelection :: Vec 13 Bool,
    cpuCsrReadOnly :: Bool,
    cpuCsrValue :: Word32,
    cpuCsrSource :: Word32,
    cpuCycle :: Counter64,
    cpuInstret :: Counter64,
    -- | Registered pulse and CSR write request, applied on the next clock.
    cpuCountRetirement :: Bool,
    cpuCounterWrite :: Maybe (Unsigned 12, Word32),
    cpuMulDiv :: MulDivUnit,
    systemBus :: BusState,
    cpuBusRequest :: Maybe BusRequest,
    cpuBusResponse :: Maybe BusResponse,
    rxHolding :: Maybe Byte,
    -- | BOOTCAUSE: why the CPU last reset into the boot ROM.
    bootCause :: Unsigned 2,
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
      cpuPendingWrite = Nothing,
      cpuPc = 0,
      cpuPhase = Fetch,
      cpuInstruction = 0,
      cpuFetchFault = False,
      cpuInRom = True,
      cpuABanks = repeat 0,
      cpuBBanks = repeat 0,
      cpuA = 0,
      cpuB = 0,
      cpuImm = 0,
      cpuIsRegOp = False,
      cpuOp2 = 0,
      cpuExecuteControl = initialExecuteControl,
      cpuAluCandidates = repeat 0,
      cpuAluCompare = initialCompareResults,
      cpuBranchCompare = initialCompareResults,
      cpuComputed = Computed Nothing 0 0 Nothing 0,
      cpuTrapRegisters = initialTrapRegisters,
      cpuExceptionCause = 0,
      cpuExceptionValue = 0,
      cpuScratch = 0,
      cpuInterruptEnable = 0,
      cpuCsrSelection = repeat False,
      cpuCsrReadOnly = False,
      cpuCsrValue = 0,
      cpuCsrSource = 0,
      cpuCycle = initialCounter,
      cpuInstret = initialCounter,
      cpuCountRetirement = False,
      cpuCounterWrite = Nothing,
      cpuMulDiv = MulDivUnit MdPrepare 0 0 0 0 False False False,
      systemBus = initialBus,
      cpuBusRequest = Nothing,
      cpuBusResponse = Nothing,
      rxHolding = Nothing,
      bootCause = causePowerOn,
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

simpleRisc :: forall dom. (HiddenClockResetEnable dom) => Signal dom Bit -> Signal dom Bit
simpleRisc serialRx = serialTx
  where
    -- Two flip-flops resynchronise the asynchronous RX pin before sampling, and
    -- the received byte is registered before it reaches the CPU: a Ctrl-C
    -- resets the whole CPU, so that compare fans out to every state bit.
    receivedNow = uartRx (register high (register high serialRx))
    received = register Nothing receivedNow
    emergencyReset = register False ((== Just 0x03) <$> receivedNow)
    (serialTx, uartReady) = uartTx txRequest
    -- Break the controller -> UART path. A queued byte reserves the idle
    -- transmitter so the DONE emitter cannot enqueue a second byte too soon.
    txRequest = register Nothing txRequestNow
    txReady = (\ready pending -> ready && case pending of Nothing -> True; Just _ -> False) <$> uartReady <*> txRequest

    machineInput = bundle (memoryOut, received, txReady, emergencyReset)
    machineOutput = mealy machineStepDecoded initialMachine machineInput
    (memoryAddressNow, memoryWriteNow, txRequestNow) = unbundle machineOutput
    -- Real BRAM setup time is worse than the Gowin timing model. Register all
    -- commands at its boundary; issue/delay/wait stages account for latency.
    memoryAddress = register 0 memoryAddressNow
    memoryWrite = register Nothing memoryWriteNow

    -- A synchronous 16384 x 32-bit RAM and the 512-word boot ROM, each with an
    -- output register. Reads take two cycles after the registered command;
    -- writes (RAM only: the bus refuses ROM stores) are visible on a later
    -- read. Bit 14 of the physical index selects the ROM; the select is
    -- delayed to line up with the registered outputs, so the final mux sits
    -- between flip-flops rather than after a block RAM.
    ramOut = register 0 (blockRam1 NoClearOnReset (SNat @16384) 0
      (resize <$> memoryAddress :: Signal dom (Unsigned 14))
      (fmap (\(address, value) -> (resize address :: Unsigned 14, value)) <$> memoryWrite))
    romOut = register 0 (rom bootRomContents (resize <$> memoryAddress :: Signal dom (Unsigned 9)))
    romSelected = register False (register False ((`testBit` 14) <$> memoryAddress))
    memoryOut = mux romSelected romOut ramOut

-- | Result wires from the machine controller: BRAM read address, optional BRAM
-- write, and an optional byte offered to the UART transmitter.
machineStep ::
  Machine ->
  (Word32, Maybe Byte, Bool) ->
  (Machine, (MemAddr, Maybe (MemAddr, Word32), Maybe Byte))
machineStep machine (memoryWord, received, txReady) =
  machineStepDecoded machine (memoryWord, received, txReady, received == Just 0x03)

-- The physical circuit supplies a registered Ctrl-C decode, preventing the
-- 8-bit byte comparison from preceding the controller's high-fanout reset mux.
machineStepDecoded :: Machine -> (Word32, Maybe Byte, Bool, Bool) -> (Machine, StepOutput)
machineStepDecoded = machineStepDecodedWith decodeMemoryTarget

machineStepDecodedWith :: (Word32 -> MemoryTarget) -> Machine -> (Word32, Maybe Byte, Bool, Bool) -> (Machine, StepOutput)
machineStepDecodedWith decodeTarget machine (memoryWord, received, txReady, emergencyRequest) =
  let emergencyReset = breakRequested machine emergencyRequest
      receivedMachine = case (not emergencyReset, received, rxHolding machine) of
        (True, Just byte, Nothing) -> machine {rxHolding = Just byte}
        _ -> machine
      (machine', coreOutput) = runningStepDecodedWith decodeTarget
        receivedMachine {clearRegisters = False, cpuCountRetirement = False, cpuCounterWrite = Nothing, cpuBusRequest = Nothing}
        memoryWord received txReady emergencyReset
      cancelled = emergencyReset
      (bus', response, busOutput, effect) = if cancelled
        then ((systemBus machine) {busPhase = BusIdle}, Nothing, (0, Nothing, Nothing), NoEffect)
        else busStepWith decodeTarget (systemBus machine) (cpuBusRequest machine) memoryWord txReady (rxHolding machine) (bootCause machine)
      busActive = case busPhase (systemBus machine) of
        BusIdle -> case cpuBusRequest machine of Nothing -> False; Just _ -> True
        _ -> True
      output = if busActive && not cancelled then busOutput else coreOutput
      counterWrite lowAddress highAddress = case cpuCounterWrite machine of
        Just (address, value)
          | address == lowAddress -> Just (False, value)
          | address == highAddress -> Just (True, value)
        _ -> Nothing
      counted = machine'
        { systemBus = bus', cpuBusResponse = response,
          rxHolding = if effect == ConsumeRx then Nothing else rxHolding machine',
          bootCause = if effect == ConsumeBootCause then causeNone else bootCause machine',
          cpuCycle = tickCounter True (counterWrite 0xb00 0xb80) (cpuCycle machine),
          cpuInstret = tickCounter (cpuCountRetirement machine)
            (counterWrite 0xb02 0xb82) (cpuInstret machine)
        }
   in (if clearRegisters machine then counted {cpuRegs = repeat 0} else counted, output)

-- | Ctrl-C: reset the CPU into the boot ROM with cause BREAK.
resetCpu :: Machine -> Machine
resetCpu = systemReset causeBreak

-- | A received Ctrl-C breaks a running program; in the boot ROM it is data.
-- Both inputs are registers, so the reset fan-out starts from one gate.
breakRequested :: Machine -> Bool -> Bool
breakRequested machine requested = requested && not (cpuInRom machine)

-- | Reset architectural state and cancellation tokens and restart at the boot
-- ROM, recording why; RAM is kept. Pipeline temporaries remain held: each is
-- overwritten before its next valid use, avoiding wide reset muxes.
systemReset :: Unsigned 2 -> Machine -> Machine
systemReset cause machine =
  machine
    { clearRegisters = True,
      cpuPendingWrite = Nothing,
      bootCause = cause,
      cpuInRom = True,
      cpuPc = 0,
      cpuPhase = Fetch,
      cpuTrapRegisters = initialTrapRegisters,
      cpuScratch = 0,
      cpuInterruptEnable = 0,
      cpuCountRetirement = False,
      cpuCounterWrite = Nothing,
      systemBus = (systemBus machine) {busPhase = BusIdle},
      cpuBusRequest = Nothing,
      cpuBusResponse = Nothing,
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
  runningStepDecodedWith decodeMemoryTarget machine memoryWord received txReady (received == Just 0x03)

runningStepDecodedWith :: (Word32 -> MemoryTarget) -> Machine -> Word32 -> Maybe Byte -> Bool -> Bool -> (Machine, StepOutput)
runningStepDecodedWith decodeTarget machine memoryWord received txReady emergencyReset =
  let (machine', registerWrite, output) = cpuStepDecodedWith decodeTarget machine memoryWord received txReady emergencyReset
      pending = if emergencyReset then Nothing else cpuPendingWrite machine
      written = maybe machine' (\(index, value) -> writeRegister index value machine') pending
   in (written {cpuPendingWrite = registerWrite}, output)

-- | One CPU cycle.  Register writes are returned rather than applied in each
-- branch, so the generated hardware has a single register-file write port
-- instead of a full copy of all 32 registers per instruction kind.
cpuStep :: Machine -> Word32 -> Maybe Byte -> Bool -> (Machine, RegisterWrite, StepOutput)
cpuStep machine memoryWord received txReady
  = cpuStepDecodedWith decodeMemoryTarget machine memoryWord received txReady (received == Just 0x03)

cpuStepDecodedWith :: (Word32 -> MemoryTarget) -> Machine -> Word32 -> Maybe Byte -> Bool -> Bool -> (Machine, RegisterWrite, StepOutput)
cpuStepDecodedWith decodeTarget machine memoryWord _received txReady emergencyReset
  | emergencyReset = (resetCpu machine, Nothing, (0, Nothing, Nothing))
  | otherwise =
      let machineRx = machine
          next m = withoutRegister (m, (0, Nothing, Nothing))
       in let Machine {..} = machineRx
              instruction = cpuInstruction
              address = cAddress cpuComputed
           in case cpuPhase of
               -- Only the ROM and RAM hold instructions. The check is
               -- registered here and raised as an access fault at Decode.
               Fetch ->
                 withoutRegister
                   ( machineRx {cpuPhase = FetchDelay, cpuFetchFault = not (fetchable (decodeTarget cpuPc)),
                       cpuInRom = romAddress cpuPc},
                     (memoryIndex cpuPc, Nothing, Nothing)
                   )
               FetchDelay -> next machineRx {cpuPhase = FetchCapture}
               FetchCapture -> next machineRx {cpuPhase = FetchWait}
               FetchWait -> next machineRx {cpuPhase = Decode, cpuInstruction = memoryWord}
               Decode
                 | cpuFetchFault -> withoutRegister (raiseException machineRx 1 cpuPc)
                 | otherwise ->
                     next
                       machineRx
                         { cpuPhase = OperandSelect,
                           cpuABanks = readRegisterBanks (instructionRs1 instruction) cpuRegs,
                           cpuBBanks = readRegisterBanks (instructionRs2 instruction) cpuRegs,
                           cpuImm = decodeImmediate instruction,
                           cpuIsRegOp = instruction .&. 0x7f == 0x33
                         }
               OperandSelect ->
                 let selectedB = selectRegisterBank (instructionRs2 instruction) cpuBBanks
                  in next machineRx
                       { cpuPhase = Execute,
                         cpuA = selectRegisterBank (instructionRs1 instruction) cpuABanks,
                         cpuB = selectedB,
                         cpuOp2 = if cpuIsRegOp then selectedB else cpuImm,
                         cpuExecuteControl = decodeExecuteControl instruction
                       }
               Execute ->
                 next machineRx
                   { cpuPhase = if execMulDiv cpuExecuteControl then MulDiv else ExecuteFinish,
                     cpuComputed = (compute cpuPc instruction cpuA cpuB cpuImm cpuOp2) {cAlu = Nothing, cTaken = Nothing},
                     cpuAluCompare = compareOperands cpuA cpuOp2,
                     cpuBranchCompare = compareOperands cpuA cpuB,
                     cpuAluCandidates = aluCandidates cpuExecuteControl cpuA cpuOp2,
                     cpuMulDiv = cpuMulDiv {mdStep = MdPrepare}
                   }
               ExecuteFinish ->
                 let control = cpuExecuteControl
                     result = case execOpcode control of
                       0x37 -> Just cpuImm
                       0x17 -> Just (cTarget cpuComputed)
                       0x13 -> selectedAlu control cpuAluCandidates cpuAluCompare
                       0x33 -> selectedAlu control cpuAluCandidates cpuAluCompare
                       _ -> Nothing
                  in next machineRx
                       { cpuPhase = if execShift control then ShiftFinish else Commit,
                         cpuComputed = cpuComputed {cAlu = result,
                           cTaken = branchTakenPrepared (execFunct3 control) cpuBranchCompare}
                       }
               ShiftFinish ->
                 let control = cpuExecuteControl
                     result = fmap (\value -> shiftResult (execFunct3 control)
                       (if execArithmeticShift control then 0x20 else 0) value
                       (fromIntegral (cpuOp2 .&. 0x1c))) (cAlu cpuComputed)
                  in next machineRx {cpuPhase = Commit, cpuComputed = cpuComputed {cAlu = result}}
               MulDiv ->
                 case mulDivStep (instructionFunct3 instruction) cpuA cpuB cpuMulDiv of
                   Left unit -> next machineRx {cpuMulDiv = unit}
                   Right result -> next machineRx {cpuPhase = Commit, cpuComputed = cpuComputed {cAlu = Just result}}
               Commit -> commitInstruction machineRx txReady
               Trap -> withoutRegister (enterTrap machineRx)
               CsrRead -> csrReadStep machineRx
               CsrWrite -> csrWriteStep machineRx
               TrapReturn -> withoutRegister (returnFromTrap machineRx)
               BusWait -> case cpuBusResponse of
                 Nothing -> next machineRx
                 Just response
                   | responseFault response -> withoutRegister (raiseException machineRx
                       (if testBit instruction 5 then 7 else 5) address)
                   -- The finisher ends the program: reset into the boot ROM.
                   | responseExit response -> withoutRegister
                       ((systemReset causeExit machineRx) {cpuCountRetirement = True}, (0, Nothing, Nothing))
                   | testBit instruction 5 -> withoutRegister (finishInstruction
                       machineRx {cpuPc = cLink cpuComputed} Nothing)
                   | otherwise -> withRegister (instructionRd instruction) (responseValue response)
                       (finishInstruction machineRx {cpuPc = cLink cpuComputed} Nothing)

withRegister :: Index 32 -> Word32 -> (Machine, StepOutput) -> (Machine, RegisterWrite, StepOutput)
withRegister index value (machine, output) = (machine, Just (index, value), output)

withoutRegister :: (Machine, StepOutput) -> (Machine, RegisterWrite, StepOutput)
withoutRegister (machine, output) = (machine, Nothing, output)

-- | Execute stage: every adder and the ALU, in parallel, for whichever
-- instruction this turns out to be. Shifts compute their low two levels here
-- and ShiftFinish completes the high three levels.
-- The immediate has already been decoded for the instruction's format, so
-- each adder has fixed, registered inputs and nothing is muxed in front of it.
compute :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Computed
compute pc instruction a b imm op2 =
  Computed
    { cAlu = case opcode of
        0x37 -> Just imm -- LUI
        0x17 -> Just pcPlusImm -- AUIPC
        0x13 -> aluWithShiftMask 3 funct3 funct7 False a op2
        0x33 -> aluWithShiftMask 3 funct3 funct7 True a op2
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
commitInstruction machine _txReady =
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
      jumpAndLink newPc
        | newPc .&. 3 /= 0 = fault 0 newPc
        | otherwise = withRegister rd nextPc (finishInstruction machine {cpuPc = newPc} Nothing)
      fault cause value = withoutRegister (raiseException machine cause value)
      invalid = fault 2 instruction
      address = cAddress
      requestBus write = withoutRegister
        (machine {cpuPhase = BusWait,
          cpuBusRequest = Just (BusRequest address funct3 write (cpuB machine))}, (0, Nothing, Nothing))
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
            Just True | cTarget .&. 3 /= 0 -> fault 0 cTarget
            Just takeBranch -> noWrite (if takeBranch then cTarget else nextPc)
            Nothing -> invalid
        -- Validate the encoding and alignment before touching RAM or a device.
        -- Address misalignment takes priority over an out-of-map address.
        0x03
          | not (validLoad funct3) -> invalid
          | misalignedMemory funct3 address -> fault 4 address
          | otherwise -> requestBus False
        0x23
          | not (validStore funct3) -> invalid
          | misalignedMemory funct3 address -> fault 6 address
          | otherwise -> requestBus True
        0x0f -> noWrite nextPc -- FENCE is a no-op in this tiny single-master core.
        0x73
          | instruction == 0x0000_0073 -> fault 11 0 -- ECALL from M mode
          | instruction == 0x0010_0073 -> fault 3 (cpuPc machine) -- EBREAK
          | instruction == 0x3020_0073 -> withoutRegister (machine {cpuPhase = TrapReturn}, (0, Nothing, Nothing))
          | instruction == 0x1050_0073 -> noWrite nextPc -- WFI is a hint until interrupts exist.
          | funct3 == 1 || funct3 == 2 || funct3 == 3 || funct3 == 5 || funct3 == 6 || funct3 == 7 ->
              withoutRegister
                ( machine {cpuPhase = CsrRead,
                    cpuCsrSelection = decodeCsrSelection (csrAddress instruction),
                    cpuCsrReadOnly = csrAddress instruction == 0x301 || csrAddress instruction .&. 0xc00 == 0xc00,
                    cpuCsrSource = if testBit funct3 2
                      then resize ((instruction `shiftR` 15) .&. 0x1f)
                      else cpuA machine},
                  (0, Nothing, Nothing)
                )
          | otherwise -> invalid
        _ -> invalid

-- | A halfword must be 2-aligned and a word 4-aligned; byte accesses are free.
misalignedMemory :: BitVector 3 -> Word32 -> Bool
misalignedMemory funct3 address
  | funct3 == 0b010 = address .&. 3 /= 0
  | funct3 == 0b001 || funct3 == 0b101 = testBit address 0
  | otherwise = False

-- | Capture the exception without retiring, advancing PC, writing rd or issuing
-- a memory/UART request. The next stage owns the trap register write muxes.
raiseException :: Machine -> Word32 -> Word32 -> (Machine, StepOutput)
raiseException machine cause value =
  ( machine {cpuPhase = Trap, cpuExceptionCause = cause, cpuExceptionValue = value},
    (0, Nothing, Nothing)
  )

enterTrap :: Machine -> (Machine, StepOutput)
enterTrap machine =
  let regs = cpuTrapRegisters machine
      updated = regs
        { trapEpc = cpuPc machine .&. complement 3,
          trapCause = cpuExceptionCause machine,
          trapValue = cpuExceptionValue machine,
          trapMpie = trapMie regs,
          trapMie = False
        }
      -- With mtvec still 0 this is the boot ROM, which reports the end of
      -- the program.
      trapped = machine {cpuTrapRegisters = updated, cpuPc = trapVector regs .&. complement 3, cpuPhase = Fetch}
   in (trapped, (0, Nothing, Nothing))

-- | These CSRs have no read side effects, so CSRRW rd=x0 can share the same
-- read stage. Write suppression uses the encoded rs1/zimm, not its value.
csrWriteRequested :: Word32 -> Bool
csrWriteRequested instruction =
  let kind = instructionFunct3 instruction
   in kind == 1 || kind == 5 || instructionRs1 instruction /= 0

csrAddress :: Word32 -> Unsigned 12
csrAddress instruction = resize (instruction `shiftR` 20)

readCsr :: Unsigned 12 -> Machine -> Maybe (Word32, Bool)
readCsr address machine = case address of
  0x300 -> Just (0x1800 .|. (if trapMie regs then 8 else 0) .|. (if trapMpie regs then 0x80 else 0), False)
  0x301 -> Just (0x4000_1100, True) -- read-only RV32IM
  0x304 -> Just (cpuInterruptEnable machine, False)
  0x305 -> Just (trapVector regs, False)
  0x310 -> Just (0, False) -- mstatush, all fields WARL zero
  0x340 -> Just (cpuScratch machine, False)
  0x341 -> Just (trapEpc regs, False)
  0x342 -> Just (trapCause regs, False)
  0x343 -> Just (trapValue regs, False)
  0x344 -> Just (0, False) -- mip, no interrupt sources until step 2
  0xb00 -> Just (counterLo (cpuCycle machine), False)
  0xb80 -> Just (counterHi (cpuCycle machine), False)
  0xb02 -> Just (counterLo (cpuInstret machine), False)
  0xb82 -> Just (counterHi (cpuInstret machine), False)
  0xc00 -> Just (counterLo (cpuCycle machine), True)
  0xc80 -> Just (counterHi (cpuCycle machine), True)
  0xc02 -> Just (counterLo (cpuInstret machine), True)
  0xc82 -> Just (counterHi (cpuInstret machine), True)
  0xf11 -> Just (0, True)
  0xf12 -> Just (0, True)
  0xf13 -> Just (0, True)
  0xf14 -> Just (0, True)
  _ -> Nothing
  where
    regs = cpuTrapRegisters machine

writeCsr :: Unsigned 12 -> Word32 -> Machine -> Machine
writeCsr address value machine = case address of
  0x300 -> machine {cpuTrapRegisters = regs {trapMie = testBit value 3, trapMpie = testBit value 7}}
  0x304 -> machine {cpuInterruptEnable = value .&. 0x888}
  0x305 -> machine {cpuTrapRegisters = regs {trapVector = value .&. complement 3}}
  0x340 -> machine {cpuScratch = value}
  0x341 -> machine {cpuTrapRegisters = regs {trapEpc = value .&. complement 3}}
  0x342 -> machine {cpuTrapRegisters = regs {trapCause = value}}
  0x343 -> machine {cpuTrapRegisters = regs {trapValue = value}}
  0xb00 -> counterWrite
  0xb80 -> counterWrite
  0xb02 -> counterWrite
  0xb82 -> counterWrite
  _ -> machine -- mstatush/mip writes are ignored; invalid access checked in CsrRead
  where
    regs = cpuTrapRegisters machine
    counterWrite = machine {cpuCounterWrite = Just (address, value)}

counterReadBusy :: Unsigned 12 -> Machine -> Bool
counterReadBusy address machine = case address of
  0xb00 -> cycleCarry
  0xb80 -> cycleCarry
  0xc00 -> cycleCarry
  0xc80 -> cycleCarry
  0xb02 -> instretCarry
  0xb82 -> instretCarry
  0xc02 -> instretCarry
  0xc82 -> instretCarry
  _ -> False
  where
    cycleCarry = counterCarry (cpuCycle machine)
    instretCarry = counterCarry (cpuInstret machine)

csrReadStep :: Machine -> (Machine, RegisterWrite, StepOutput)
csrReadStep machine =
  let instruction = cpuInstruction machine
      selection = cpuCsrSelection machine
      -- Even a refused/busy read may latch a temporary value. It is used only
      -- after successful completion, avoiding validity gates on the data mux.
      captured = machine {cpuCsrValue = readSelectedCsr selection machine}
      invalid = withoutRegister (raiseException captured 2 instruction)
      busy = ((selection !! (8 :: Index 13) || selection !! (9 :: Index 13))
                && counterCarry (cpuCycle machine))
          || ((selection !! (10 :: Index 13) || selection !! (11 :: Index 13))
                && counterCarry (cpuInstret machine))
   in if not (or selection) || (cpuCsrReadOnly machine && csrWriteRequested instruction)
        then invalid
        else withoutRegister
          (captured {cpuPhase = if busy then CsrRead else CsrWrite}, (0, Nothing, Nothing))

-- CSR address comparisons are registered at Commit, before the data mux.
decodeCsrSelection :: Unsigned 12 -> Vec 13 Bool
decodeCsrSelection address =
  (address == 0x300) :> (address == 0x301) :>
  (address == 0x304) :> (address == 0x305) :>
  (address == 0x340) :> (address == 0x341) :>
  (address == 0x342) :> (address == 0x343) :>
  (address == 0xb00 || address == 0xc00) :>
  (address == 0xb80 || address == 0xc80) :>
  (address == 0xb02 || address == 0xc02) :>
  (address == 0xb82 || address == 0xc82) :>
  (address == 0x310 || address == 0x344 || address == 0xf11 ||
   address == 0xf12 || address == 0xf13 || address == 0xf14) :> Nil

readSelectedCsr :: Vec 13 Bool -> Machine -> Word32
readSelectedCsr selection machine =
  let regs = cpuTrapRegisters machine
      values = (0x1800 .|. (if trapMie regs then 8 else 0) .|. (if trapMpie regs then 0x80 else 0)) :>
        0x4000_1100 :> cpuInterruptEnable machine :> trapVector regs :>
        cpuScratch machine :> trapEpc regs :> trapCause regs :> trapValue regs :>
        counterLo (cpuCycle machine) :> counterHi (cpuCycle machine) :>
        counterLo (cpuInstret machine) :> counterHi (cpuInstret machine) :> 0 :> Nil
   in fold (.|.) (zipWith (\selected value -> if selected then value else 0) selection values)

csrWriteStep :: Machine -> (Machine, RegisterWrite, StepOutput)
csrWriteStep machine =
  let instruction = cpuInstruction machine
      old = cpuCsrValue machine
      source = cpuCsrSource machine
      value = case instructionFunct3 instruction .&. 3 of
        1 -> source
        2 -> old .|. source
        _ -> old .&. complement source
      updated = if csrWriteRequested instruction then writeCsr (csrAddress instruction) value machine else machine
   in withRegister (instructionRd instruction) old
        (finishInstruction updated {cpuPc = cLink (cpuComputed machine)} Nothing)

returnFromTrap :: Machine -> (Machine, StepOutput)
returnFromTrap machine =
  let regs = cpuTrapRegisters machine
   in finishInstruction machine
        { cpuPc = trapEpc regs,
          cpuTrapRegisters = regs {trapMie = trapMpie regs, trapMpie = True}
        } Nothing

finishInstruction :: Machine -> Maybe (MemAddr, Word32) -> (Machine, StepOutput)
finishInstruction machine write = finishWith machine write Nothing

finishInstructionWithTx :: Machine -> Maybe Byte -> (Machine, StepOutput)
finishInstructionWithTx machine request = finishWith machine Nothing request

-- | Retire an instruction.  The next fetch starts in the Fetch stage from the
-- PC register, a cycle after any write here, so it always sees the new word.
-- Whether that PC has left the program is checked in 'cpuStep'.
finishWith :: Machine -> Maybe (MemAddr, Word32) -> Maybe Byte -> (Machine, StepOutput)
finishWith machine write request = (machine {cpuPhase = Fetch, cpuCountRetirement = True}, (0, write, request))

-- Split the physical 32-way mux across two clock edges. Each first-stage
-- register sees eight candidates; the operand register then sees four banks.
readRegisterBanks :: Index 32 -> Vec 32 Word32 -> Vec 4 Word32
readRegisterBanks index regs = map (!! bankOffset) (unconcat (SNat @8) regs)
  where
    bankOffset = fromIntegral (fromIntegral index .&. 7 :: Unsigned 5) :: Index 8

selectRegisterBank :: Index 32 -> Vec 4 Word32 -> Word32
selectRegisterBank 0 _ = 0
selectRegisterBank index banks = banks !! (fromIntegral (fromIntegral index `shiftR` 3 :: Unsigned 5) :: Index 4)

readRegister :: Index 32 -> Machine -> Word32
readRegister 0 _ = 0
readRegister index Machine {..} = cpuRegs !! index

writeRegister :: Index 32 -> Word32 -> Machine -> Machine
writeRegister 0 _ machine = machine
writeRegister index value machine@Machine {..} = machine {cpuRegs = replace index value cpuRegs}

regIndex :: Word32 -> Index 32
regIndex value = fromIntegral (resize value :: Unsigned 5)

-- | The physical word index: bit 31 clear means the boot ROM (bit 14 set,
-- low nine bits the ROM word), set means RAM (the low 14 bits). Only
-- meaningful for addresses the decoder maps to ROM or RAM.
memoryIndex :: Word32 -> MemAddr
memoryIndex address
  | testBit address 31 = resize (address `shiftR` 2) .&. 0x3fff
  | otherwise = 0x4000 .|. (resize (address `shiftR` 2) .&. 0x1ff)

ramBase :: Word32
ramBase = 0x8000_0000

-- | 64 KiB of RAM at 0x8000_0000, as a bit test.
validMemoryAddress :: Word32 -> Bool
validMemoryAddress address = address .&. 0xffff_0000 == ramBase

-- | The 2 KiB boot ROM at address 0, as a bit test.
romAddress :: Word32 -> Bool
romAddress address = address .&. 0xffff_f800 == 0

fetchable :: MemoryTarget -> Bool
fetchable target = target == RamTarget || target == RomTarget

-- SiFive/QEMU-style finisher command register. DONE carries no status payload.
exitDevice :: Word32
exitDevice = 0x0010_0000

-- | BOOTCAUSE, read-only beside the finisher.
bootCauseDevice :: Word32
bootCauseDevice = 0x0010_0004

uartTxData, uartStatus, uartRxData :: Word32
uartTxData = 0x1000_0000
uartStatus = 0x1000_0004
uartRxData = 0x1000_0008

decodeMemoryTarget :: Word32 -> MemoryTarget
decodeMemoryTarget address
  | validMemoryAddress address = RamTarget
  | romAddress address = RomTarget
  | address == bootCauseDevice = BootCauseTarget
  | address == uartTxData = UartTxTarget
  | address == uartStatus = UartStatusTarget
  | address == uartRxData = UartRxTarget
  | address == exitDevice = ExitTarget
  | otherwise = UnmappedTarget

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
alu = aluWithShiftMask 0x1f

-- Only the shift distance is masked; adders retain their registered inputs.
aluWithShiftMask :: Word32 -> BitVector 3 -> Word32 -> Bool -> Word32 -> Word32 -> Maybe Word32
aluWithShiftMask mask funct3 funct7 isRegister a op2 = if legal then Just value else Nothing
  where
    alternate = funct7 == 0x20 -- SUB and SRA/SRAI
    isShift = funct3 == 0b001 || funct3 == 0b101
    legal
      | isRegister = funct7 == 0 || (alternate && (funct3 == 0b000 || funct3 == 0b101))
      | otherwise = not isShift || funct7 == 0 || (alternate && funct3 == 0b101)
    shamt = fromIntegral (op2 .&. mask)
    value = case funct3 of
      0b000 -> if isRegister && alternate then a - op2 else a + op2 -- ADD(I), SUB
      0b001 -> shiftResult funct3 funct7 a shamt -- SLL(I)
      0b010 -> boolWord (signed32 a < signed32 op2) -- SLT(I)
      0b011 -> boolWord (a < op2) -- SLT(I)U
      0b100 -> a `xor` op2 -- XOR(I)
      0b101 -> shiftResult funct3 funct7 a shamt -- SRL(I), SRA(I)
      0b110 -> a .|. op2 -- OR(I)
      _ -> a .&. op2 -- AND(I)

-- | One part of a shift; arithmetic shifts preserve the sign at both stages.
shiftResult :: BitVector 3 -> Word32 -> Word32 -> Int -> Word32
shiftResult funct3 funct7 value amount
  | funct3 == 0b001 = value `shiftL` amount
  | funct7 == 0x20 = unsigned32 (signed32 value `shiftR` amount)
  | otherwise = value `shiftR` amount

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
                  mdTotal = 0,
                  mdNegLo = aNegative /= bNegative,
                  mdNegHi = if isDiv then aNegative else aNegative /= bNegative,
                  mdCarry = False
                }
  MdLoop i ->
    Left unit {mdStep = MdCommit i, mdTotal = if isDiv then divideDifference else multiplyTotal}
  MdCommit i ->
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
      let total = mdTotal
       in (resize (total `shiftR` 1), (mdLo `shiftR` 1) .|. (if testBit total 0 then 0x8000_0000 else 0))
    -- Shift the next dividend bit into the remainder; subtract the divisor if
    -- it fits, recording a quotient bit.
    divideStep =
      let fits = not (testBit mdTotal 32)
       in (resize (if fits then mdTotal else remainder), (mdLo `shiftL` 1) .|. boolWord fits)
    multiplyTotal = if testBit mdLo 0 then resize mdHi + resize mdOperand else resize mdHi
    remainder = (resize mdHi `shiftL` 1) .|. (if testBit mdLo 31 then 1 else 0) :: Unsigned 33
    -- During restoring division remainder < 2*divisor. A successful subtract
    -- is < divisor (fits in 32 bits); bit 32 therefore encodes the borrow.
    divideDifference = remainder - resize mdOperand

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
