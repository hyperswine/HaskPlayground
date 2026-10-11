{-# LANGUAGE FlexibleContexts #-}
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
--   * 0x0200_0000: CLINT (QEMU virt layout): msip +0x0, mtimecmp +0x4000/4,
--     mtime +0xbff8/c; mtime counts core clocks
--   * 0x0c00_0000: PLIC (QEMU virt layout, one source: the UART, IRQ 10):
--     priority +0x28, pending +0x1000, enable +0x2000, threshold +0x20_0000,
--     claim/complete +0x20_0004
--   * 0x1000_0000: UART TXDATA (write the low byte)
--   * 0x1000_0004: UART STATUS (bit 0 = TX ready, bit 1 = RX byte ready)
--   * 0x1000_0008: UART RXDATA (read low byte, consuming it)
--   * 0x1000_000c: UART CTRL (bit 0 = interrupt while a byte is waiting,
--     bit 1 = interrupt while the transmitter is ready)
--   * 0x1000_1000-0x1000_1fff: the I/O complex, outside the CPU (word
--     accesses only, one handshake per access):
--     - 0x1000_1000 SPI DATA (write: send the low byte while receiving one;
--       read: the last byte received)
--     - 0x1000_1004 SPI STATUS (bit 0 = a transfer is in progress)
--     - 0x1000_1008 SPI CTRL (bit 0 = chip select asserted; bits 8-15 = half
--       an SCK period in clocks, minus one)
--     - 0x1000_1100 DMA ADDR (RAM byte address, word aligned)
--     - 0x1000_1104 DMA COUNT (bytes, a multiple of 4; reads what is left)
--     - 0x1000_1108 DMA CTRL (write starts: bit 0 = send RAM to SPI, else
--       receive SPI to RAM; bit 1 = before receiving, clock 0xff until a
--       start token, which must be 0xfe)
--     - 0x1000_110c DMA STATUS (bit 0 busy, bit 1 error, bits 8-15 token)
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
-- handlers and split 64-bit cycle/retirement counters.  Machine software
-- (msip), timer (CLINT) and external (PLIC) interrupts are taken between
-- instructions, in that priority order external > software > timer, and WFI
-- waits for one.
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
import Data.Maybe (isJust, isNothing)

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
  | MemoryWait
  | MemoryCapture
  deriving (Generic, NFDataX, Show, Eq)

-- One dedicated state bit per CPU stage.
{-# ANN module (DataReprAnn $(liftQ [t| CpuPhase |]) 18
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
    ConstrRepr 'ExecuteFinish 32768 32768 [],
    ConstrRepr 'MemoryWait 65536 65536 [],
    ConstrRepr 'MemoryCapture 131072 131072 []
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

-- | Interrupt-source state: the CLINT, the PLIC with its one source (the
-- UART, IRQ 10) and the UART's interrupt enables. Comparisons and the
-- pending bits are registered, so no wide compare or OR feeds the CPU.
data Devices = Devices
  { devMsip :: Bool,
    devMtimecmpLo :: Word32,
    devMtimecmpHi :: Word32,
    -- | Counts every core clock; a split counter like mcycle.
    devMtime :: Counter64,
    -- | mtime >= mtimecmp in two registered stages: the halves compared,
    -- then combined.
    devHiAbove :: Bool,
    devHiEqual :: Bool,
    devLoAtLeast :: Bool,
    devMtip :: Bool,
    devPlicPriority :: Unsigned 3,
    devPlicEnable :: Bool,
    devPlicThreshold :: Unsigned 3,
    -- | The gateway forwarded a request that has not been claimed yet.
    devPlicPending :: Bool,
    -- | Claimed and not yet completed: the gateway holds new requests.
    devPlicClaimed :: Bool,
    devMeip :: Bool,
    devUartRxInterrupt :: Bool,
    devUartTxInterrupt :: Bool,
    -- | The I/O complex's answer this cycle, injected by the circuit around
    -- the CPU (never stepped here).
    devIo :: Maybe Word32
  }
  deriving (Generic, NFDataX, Show, Eq)

-- | An SPI master, mode 0 (SCK idle low, sample on the rising edge, shift
-- on the falling edge), for the microSD slot. Every pin is a register.
data Spi = Spi
  { spiSelect :: Bool,
    -- | Half an SCK period in clocks, minus one.
    spiDivider :: Unsigned 8,
    spiCount :: Unsigned 8,
    -- | Rising edges still to come in this byte; 0 when idle.
    spiBits :: Unsigned 4,
    spiSck :: Bool,
    -- | MSB first; ones shift in, so MOSI idles high.
    spiOut :: Byte,
    spiIn :: Byte,
    -- | MISO, already synchronised to the core clock.
    spiMiso :: Bit
  }
  deriving (Generic, NFDataX, Show, Eq)

-- | Deselected, idle, and slow enough (about 190 kHz at 96 MHz) for a card's
-- identification phase.
initialSpi :: Spi
initialSpi = Spi False 255 0 0 False 0xff 0xff high

spiBusy :: Spi -> Bool
spiBusy spi = spiBits spi /= 0 || spiSck spi

-- | (SCK, MOSI, chip select active low).
spiPins :: Spi -> (Bit, Bit, Bit)
spiPins spi = (boolToBit (spiSck spi), msb (spiOut spi), boolToBit (not (spiSelect spi)))

-- | One clock of the SPI master; @start@ is a byte written to DATA.
stepSpi :: Maybe Byte -> Maybe Word32 -> Spi -> Spi
stepSpi start control spi
  | Just c <- control, not (spiBusy spi) =
      spi {spiSelect = testBit c 0, spiDivider = resize (c `shiftR` 8)}
  | Just byte <- start, not (spiBusy spi) =
      spi {spiOut = byte, spiBits = 8, spiCount = spiDivider spi, spiSck = False}
  | not (spiBusy spi) = spi
  | spiCount spi /= 0 = spi {spiCount = spiCount spi - 1}
  | not (spiSck spi) =
      spi {spiSck = True, spiCount = spiDivider spi, spiBits = spiBits spi - 1,
        spiIn = (spiIn spi `shiftL` 1) .|. (if spiMiso spi == high then 1 else 0)}
  | otherwise =
      spi {spiSck = False, spiCount = spiDivider spi, spiOut = (spiOut spi `shiftL` 1) .|. 1}

initialDevices :: Devices
initialDevices = Devices False maxBound maxBound initialCounter False False False False
  0 False 0 False False False False False Nothing

-- | The UART's interrupt number on the PLIC, as on QEMU virt.
uartIrq :: Word32
uartIrq = 10

-- | The PLIC would grant a claim now.
plicClaimable :: Devices -> Bool
plicClaimable devices = devPlicPending devices && devPlicEnable devices
  && devPlicPriority devices > devPlicThreshold devices

-- | mip as software reads it.
pendingInterrupts :: Devices -> Word32
pendingInterrupts devices =
  (if devMeip devices then 0x800 else 0) .|. (if devMtip devices then 0x80 else 0)
    .|. (if devMsip devices then 0x8 else 0)

-- Registered memory target; wide address comparisons stay out of Commit.
data MemoryTarget = RamTarget | RomTarget | UartTxTarget | UartStatusTarget | UartRxTarget | ExitTarget | BootCauseTarget
  | ClintMsipTarget | ClintCompareLoTarget | ClintCompareHiTarget | ClintTimeLoTarget | ClintTimeHiTarget
  | PlicPriorityTarget | PlicPendingTarget | PlicEnableTarget | PlicThresholdTarget | PlicClaimTarget
  | UartControlTarget | IoTarget | UnmappedTarget
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
  | BusStoreMerge | BusRamWrite | BusTx | BusIo
  deriving (Generic, NFDataX, Show, Eq)

{-# ANN module (DataReprAnn $(liftQ [t| BusPhase |]) 17
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
    ConstrRepr 'BusTx 32768 32768 [],
    ConstrRepr 'BusIo 65536 65536 []
  ]) #-}

data BusState = BusState
  { busPhase :: BusPhase, busRequest :: BusRequest, busTarget :: MemoryTarget,
    busWord :: Word32, busValue :: Word32 }
  deriving (Generic, NFDataX)

initialBus :: BusState
initialBus = BusState BusIdle (BusRequest 0 0 False 0) UnmappedTarget 0 0

-- | The side effect of a completed device read.
data BusEffect = NoEffect | ConsumeRx | ConsumeBootCause
  | -- | A word written to a device register.
    DeviceWrite MemoryTarget Word32
  | -- | A PLIC claim that granted the UART's interrupt.
    ClaimInterrupt
  deriving (Generic, NFDataX, Show, Eq)

-- Response and effects are pulses. A stalled TX emits neither; a completed
-- transfer emits exactly one response and, where appropriate, one side effect.
busStep :: BusState -> Maybe BusRequest -> Word32 -> Bool -> Maybe Byte -> Unsigned 2 -> Devices ->
  (BusState, Maybe BusResponse, StepOutput, BusEffect)
busStep = busStepWith decodeMemoryTarget

busStepWith :: (Word32 -> MemoryTarget) -> BusState -> Maybe BusRequest -> Word32 -> Bool -> Maybe Byte -> Unsigned 2 -> Devices ->
  (BusState, Maybe BusResponse, StepOutput, BusEffect)
busStepWith decodeTarget state request memoryWord txReady rx bootCause devices =
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
              -- The I/O complex: word accesses, answered by a handshake.
              IoTarget
                | width /= 2 -> respond 0 True False NoEffect
                | otherwise -> advance BusIo
              -- CLINT, PLIC and UART control registers are word-only.
              target
                | width /= 2 -> respond 0 True False NoEffect
                | busWrite q -> respond 0 False False (deviceWrite target (busData q))
                | target == PlicClaimTarget ->
                    if plicClaimable devices
                      then respond uartIrq False False ClaimInterrupt
                      else respond 0 False False NoEffect
                | otherwise -> respond (deviceRead target devices) False False NoEffect
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
        BusIo -> case devIo devices of
          Just value -> respond (if busWrite q then 0 else value) False False NoEffect
          Nothing -> advance BusIo

-- | A device register's value; only for the CLINT/PLIC/UART-control targets.
deviceRead :: MemoryTarget -> Devices -> Word32
deviceRead target devices = case target of
  ClintMsipTarget -> boolWord (devMsip devices)
  ClintCompareLoTarget -> devMtimecmpLo devices
  ClintCompareHiTarget -> devMtimecmpHi devices
  ClintTimeLoTarget -> counterLo (devMtime devices)
  ClintTimeHiTarget -> counterHi (devMtime devices)
  PlicPriorityTarget -> resize (devPlicPriority devices)
  PlicPendingTarget -> if devPlicPending devices then 1 `shiftL` 10 else 0
  PlicEnableTarget -> if devPlicEnable devices then 1 `shiftL` 10 else 0
  PlicThresholdTarget -> resize (devPlicThreshold devices)
  UartControlTarget -> (if devUartRxInterrupt devices then 1 else 0) .|. (if devUartTxInterrupt devices then 2 else 0)
  _ -> 0

-- | Writes to read-only device registers (PLIC pending) are ignored.
deviceWrite :: MemoryTarget -> Word32 -> BusEffect
deviceWrite PlicPendingTarget _ = NoEffect
deviceWrite target value = DeviceWrite target value

-- | One clock of the interrupt sources. @rxWaiting@ and @txReady@ are the
-- UART's levels; @effect@ is this clock's completed bus access.
stepDevices :: Bool -> Bool -> BusEffect -> Devices -> Devices
stepDevices rxWaiting txReady effect devices =
  let Devices {..} = devices
      write target = case effect of
        DeviceWrite t value | t == target -> Just value
        _ -> Nothing
      timeWrite = case effect of
        DeviceWrite ClintTimeLoTarget value -> Just (False, value)
        DeviceWrite ClintTimeHiTarget value -> Just (True, value)
        _ -> Nothing
      claimed = effect == ClaimInterrupt
      completed = write PlicClaimTarget == Just uartIrq
      level = (devUartRxInterrupt && rxWaiting) || (devUartTxInterrupt && txReady)
      timeLo = counterLo devMtime
      timeHi = counterHi devMtime
   in devices
        { devMsip = maybe devMsip (`testBit` 0) (write ClintMsipTarget),
          devMtimecmpLo = maybe devMtimecmpLo P.id (write ClintCompareLoTarget),
          devMtimecmpHi = maybe devMtimecmpHi P.id (write ClintCompareHiTarget),
          devMtime = tickCounter True timeWrite devMtime,
          devHiAbove = timeHi > devMtimecmpHi,
          devHiEqual = timeHi == devMtimecmpHi,
          devLoAtLeast = timeLo >= devMtimecmpLo,
          devMtip = devHiAbove || (devHiEqual && devLoAtLeast),
          devPlicPriority = maybe devPlicPriority resize (write PlicPriorityTarget),
          devPlicEnable = maybe devPlicEnable (`testBit` 10) (write PlicEnableTarget),
          devPlicThreshold = maybe devPlicThreshold resize (write PlicThresholdTarget),
          devPlicPending = (devPlicPending || (level && not devPlicClaimed)) && not claimed,
          devPlicClaimed = (devPlicClaimed || claimed) && not completed,
          devMeip = plicClaimable devices,
          devUartRxInterrupt = maybe devUartRxInterrupt (`testBit` 0) (write UartControlTarget),
          devUartTxInterrupt = maybe devUartTxInterrupt (`testBit` 1) (write UartControlTarget),
          devIo = Nothing
        }

-- | The interrupt to take, highest priority first (external, software,
-- timer), if any is pending, enabled in mie and MIE is set.
interruptCause :: Machine -> Maybe Word32
interruptCause machine
  | not (trapMie (cpuTrapRegisters machine)) = Nothing
  | testBit ready 11 = Just 0x8000_000b
  | testBit ready 3 = Just 0x8000_0003
  | testBit ready 7 = Just 0x8000_0007
  | otherwise = Nothing
  where
    ready = cpuInterruptEnable machine .&. pendingInterrupts (cpuDevices machine)

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
    -- | The CPU drives a local block RAM directly (the BRAM image), which
    -- enables both overlaps below. Off on images whose memory is driven by
    -- the CPU's phase (the SDRAM wrapper).
    cpuLocalMemory :: Bool,
    -- | Fast RAM access: ExecuteFinish found an aligned load or store to
    -- RAM and issued the word's read; Commit skips the bus.
    cpuFastMemory :: Bool,
    -- Fetch overlap: Decode reads the word at pc + 4 while the CPU's RAM
    -- port is otherwise idle; it arrives three cycles later. A Fetch whose
    -- pc matches a valid prefetch goes straight to Decode.
    cpuPrefetchAddress :: Word32,
    cpuPrefetchWord :: Word32,
    cpuPrefetchValid :: Bool,
    -- | Cycles until the prefetched word is on the memory bus; 0 when none.
    cpuPrefetchCount :: Unsigned 2,
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
    cpuCsrSelection :: Vec 14 Bool,
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
    cpuDevices :: Devices,
    -- | mie & mip is nonzero: WFI may finish.
    cpuWake :: Bool,
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
      cpuLocalMemory = True,
      cpuFastMemory = False,
      cpuPrefetchAddress = 0,
      cpuPrefetchWord = 0,
      cpuPrefetchValid = False,
      cpuPrefetchCount = 0,
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
      cpuDevices = initialDevices,
      cpuWake = False,
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
        t_inputs = [PortName "clk", PortName "reset", PortName "enable", PortName "uart_rx", PortName "sd_miso"],
        t_output = PortProduct "" [PortName "uart_tx", PortName "sd_sck", PortName "sd_mosi", PortName "sd_cs"]
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
  "SD_MISO" ::: Signal XilinxSystem Bit ->
  Signal XilinxSystem (Bit, Bit, Bit, Bit)
topEntity clk rst en serialRx sdMiso = withClockResetEnable clk rst en (simpleRiscSd serialRx sdMiso)

-- | The core without the SD slot (tests and the iverilog testbenches).
simpleRisc :: forall dom. (HiddenClockResetEnable dom) => Signal dom Bit -> Signal dom Bit
simpleRisc serialRx = (\(tx, _, _, _) -> tx) <$> simpleRiscSd serialRx (pure high)

-- | The core with the SPI master's pins: (UART TX, SCK, MOSI, CS active low).
simpleRiscSd :: forall dom. (HiddenClockResetEnable dom) => Signal dom Bit -> Signal dom Bit -> Signal dom (Bit, Bit, Bit, Bit)
simpleRiscSd serialRx sdMiso = bundle (serialTx, sck, mosi, cs)
  where
    -- Two flip-flops resynchronise the asynchronous RX pin before sampling, and
    -- the received byte is registered before it reaches the CPU: a Ctrl-C
    -- resets the whole CPU, so that compare fans out to every state bit.
    receivedNow = uartRx (register high (register high serialRx))
    emergencyReset = register False ((== Just 0x03) <$> receivedNow)
    -- Receive FIFO between the UART and the CPU's one-byte holding
    -- register: a byte is offered only while the holding register is empty
    -- and no offer is in flight. A Ctrl-C that breaks a program discards
    -- everything unread, as the reset itself does to the holding register.
    breaking = breakRequested <$> machineState <*> emergencyReset
    rxWanted = (\m offered headByte -> isNothing (rxHolding m) && isNothing offered && isJust headByte)
      <$> machineState <*> received <*> rxHead
    rxPop = (&&) <$> rxWanted <*> (not <$> breaking)
    received = register Nothing (mux rxPop rxHead (pure Nothing))
    (rxHead, _rxCount) = unbundle (mealy fifoStep (initialFifo :: Fifo 32) (bundle (breaking, receivedNow, rxPop)))
    -- Transmit FIFO: a TX store completes as soon as there is room, and the
    -- transmitter drains the FIFO by itself. The push is registered, so
    -- "ready" keeps one slot in reserve for a byte still on its way in.
    (serialTx, uartReady) = uartTx txOffer
    txPush = register Nothing txRequestNow
    txPop = (\ready headByte -> ready && isJust headByte) <$> uartReady <*> txHead
    txOffer = mux txPop txHead (pure Nothing)
    (txHead, txCount) = unbundle (mealy fifoStep (initialFifo :: Fifo 16) (bundle (pure False, txPush, txPop)))
    txReady = register False ((< 15) <$> txCount)

    machineInput = bundle (memoryOut, received, txReady, emergencyReset)
    -- MISO is asynchronous: two flip-flops, then into the I/O complex.
    miso = register high (register high sdMiso)
    machineState = register initialMachine (fst <$> stepped)
    stepped = withIo <$> machineState <*> machineInput <*> ioAnswer
    withIo m input answer = machineStepDecoded m {cpuDevices = (cpuDevices m) {devIo = answer}} input
    machineOutput = snd <$> stepped
    -- The I/O complex sees the CPU's request as registered bus state and
    -- answers a cycle later; a system reset returns it to idle.
    (ioAnswer, ioPins, ioRam) = unbundle (mealy ioStep initialIo
      (bundle (ioRequest <$> machineState, clearRegisters <$> machineState, miso, ramOutB)))
    (sck, mosi, cs) = unbundle ioPins
    (memoryAddressNow, memoryWriteNow, txRequestNow) = unbundle machineOutput
    -- Real BRAM setup time is worse than the Gowin timing model. Register all
    -- commands at its boundary; issue/delay/wait stages account for latency.
    memoryAddress = register (0 :: MemAddr) memoryAddressNow
    memoryWrite = register Nothing memoryWriteNow

    -- A synchronous 16384 x 32-bit RAM and the 512-word boot ROM, each with an
    -- output register. Reads take two cycles after the registered command;
    -- writes (RAM only: the bus refuses ROM stores) are visible on a later
    -- read. Bit 14 of the physical index selects the ROM; the select is
    -- delayed to line up with the registered outputs, so the final mux sits
    -- between flip-flops rather than after a block RAM.
    -- Port A is the CPU's; port B the I/O complex's DMA.
    (ramOutA, ramOutB) = trueDualPortBlockRam
      (portOp <$> memoryAddress <*> memoryWrite)
      (dmaOp <$> ioRam)
    ramIndex :: MemAddr -> Index 16384
    ramIndex a = fromIntegral (resize a :: Unsigned 14)
    portOp address write = case write of
      Just (a, v) -> RamWrite (ramIndex a) v
      Nothing -> RamRead (ramIndex address)
    dmaOp command = case command of
      Just (a, Just v) -> RamWrite (ramIndex a) v
      Just (a, Nothing) -> RamRead (ramIndex a)
      Nothing -> RamRead 0
    ramOut = register 0 ramOutA
    romOut = register 0 (rom bootRomContents (resize <$> memoryAddress :: Signal dom (Unsigned 9)))
    romSelected = register False (register False ((`testBit` 14) <$> memoryAddress))
    memoryOut = mux romSelected romOut ramOut

-- I/O complex ---------------------------------------------------------------------

-- | A small FIFO in registers. Inputs: flush, push, pop (pop only when the
-- head is present); outputs: the head and the count. A push to a full FIFO
-- is dropped.
data Fifo n = Fifo {fifoData :: Vec n Byte, fifoRead :: Index n, fifoWrite :: Index n, fifoCount :: Index (n + 1)}
  deriving (Generic, NFDataX, Show, Eq)

initialFifo :: (KnownNat n, KnownNat (n + 1), 1 <= n) => Fifo n
initialFifo = Fifo (repeat 0) 0 0 0

fifoStep :: forall n. (KnownNat n, KnownNat (n + 1), 1 <= n) => Fifo n -> (Bool, Maybe Byte, Bool) -> (Fifo n, (Maybe Byte, Index (n + 1)))
fifoStep fifo (flush, push, pop) = (next, (headByte, fifoCount fifo))
  where
    headByte = if fifoCount fifo == 0 then Nothing else Just (fifoData fifo !! fifoRead fifo)
    popped = pop && isJust headByte
    full = fifoCount fifo == maxBound
    pushed = case push of Just _ -> not full || popped; Nothing -> False
    bump i = if i == maxBound then 0 else i + 1
    next
      | flush = initialFifo
      | otherwise =
          fifo
            { fifoData = case push of Just byte | pushed -> replace (fifoWrite fifo) byte (fifoData fifo); _ -> fifoData fifo,
              fifoWrite = if pushed then bump (fifoWrite fifo) else fifoWrite fifo,
              fifoRead = if popped then bump (fifoRead fifo) else fifoRead fifo,
              fifoCount = case (pushed, popped) of
                (True, False) -> fifoCount fifo + 1
                (False, True) -> fifoCount fifo - 1
                _ -> fifoCount fifo
            }

-- | The CPU's request to the I/O complex: its registered bus state while
-- the bus waits in 'BusIo'.
ioRequest :: Machine -> Maybe BusRequest
ioRequest machine = case busPhase (systemBus machine) of
  BusIo -> Just (busRequest (systemBus machine))
  _ -> Nothing

-- | The SPI DMA engine: moves bytes between the SPI master and RAM through
-- the RAM's second port while the CPU runs.
data DmaPhase = DmaIdle | DmaToken | DmaReceive | DmaFetch | DmaFetchWait | DmaCapture | DmaSend
  deriving (Generic, NFDataX, Show, Eq)

data Dma = Dma
  { dmaPhase :: DmaPhase,
    dmaAddress :: Word32,
    dmaCount :: Unsigned 16,
    -- | The word being assembled (receive) or sent (send), little-endian.
    dmaWord :: Word32,
    dmaLane :: Unsigned 2,
    -- | A byte this engine started is still shifting.
    dmaInFlight :: Bool,
    -- | 0xff bytes still allowed before the start token.
    dmaPatience :: Unsigned 20,
    dmaToken :: Byte,
    dmaError :: Bool,
    -- | This cycle's command to the RAM's second port (registered).
    dmaRam :: Maybe (MemAddr, Maybe Word32)
  }
  deriving (Generic, NFDataX, Show, Eq)

initialDma :: Dma
initialDma = Dma DmaIdle 0 0 0 0 False 0 0 False Nothing

-- | Running, or its last RAM write has not left yet.
dmaBusy :: Dma -> Bool
dmaBusy dma = dmaPhase dma /= DmaIdle || isJust (dmaRam dma)

-- | One clock of the DMA engine. @spi@ is the SPI master's state this
-- cycle and @ramWord@ the second RAM port's output. Returns the next state
-- and the byte to start shifting, if any.
stepDma :: Dma -> Spi -> Word32 -> (Dma, Maybe Byte)
stepDma dma spi ramWord = case dmaPhase dma of
  DmaIdle -> (cleared, Nothing)
  -- The read leaves the state register next cycle; the block RAM answers
  -- the cycle after.
  DmaFetch -> (cleared {dmaPhase = DmaFetchWait, dmaRam = Just (memoryIndex (dmaAddress dma), Nothing)}, Nothing)
  DmaFetchWait -> (cleared {dmaPhase = DmaCapture}, Nothing)
  DmaCapture -> (cleared {dmaPhase = DmaSend, dmaWord = ramWord, dmaLane = 0}, Nothing)
  _
    | spiBusy spi -> (cleared, Nothing)
    | not (dmaInFlight dma) -> (cleared {dmaInFlight = True}, Just outgoing)
    | otherwise -> (finished, Nothing)
  where
    cleared = dma {dmaRam = Nothing}
    outgoing = case dmaPhase dma of
      DmaSend -> resize (dmaWord dma `shiftR` (8 * fromIntegral (dmaLane dma)))
      _ -> 0xff
    byte = spiIn spi
    lastByte = dmaCount dma == 1
    afterByte = cleared {dmaInFlight = False, dmaCount = dmaCount dma - 1, dmaLane = dmaLane dma + 1}
    finished = case dmaPhase dma of
      DmaToken
        | byte == 0xff && dmaPatience dma /= 0 -> cleared {dmaInFlight = False, dmaPatience = dmaPatience dma - 1}
        | byte == 0xfe -> cleared {dmaInFlight = False, dmaToken = byte, dmaPhase = DmaReceive}
        | otherwise -> cleared {dmaInFlight = False, dmaToken = byte, dmaError = True, dmaPhase = DmaIdle}
      DmaReceive ->
        let word = (dmaWord dma .&. complement (0xff `shiftL` lane)) .|. (resize byte `shiftL` lane)
            lane = 8 * fromIntegral (dmaLane dma)
            complete = dmaLane dma == 3 || lastByte
         in afterByte
              { dmaWord = word,
                dmaRam = if complete then Just (memoryIndex (dmaAddress dma), Just word) else Nothing,
                dmaAddress = if complete then dmaAddress dma + 4 else dmaAddress dma,
                dmaPhase = if lastByte then DmaIdle else DmaReceive
              }
      _ -- DmaSend
        | lastByte -> afterByte {dmaPhase = DmaIdle}
        | dmaLane dma == 3 -> afterByte {dmaPhase = DmaFetch, dmaAddress = dmaAddress dma + 4}
        | otherwise -> afterByte

-- | Devices outside the CPU. A request is taken once (while no answer is
-- pending) and answered from a register the next cycle, so the CPU's bus
-- and the devices meet only at flip-flops.
data Io = Io {ioSpi :: Spi, ioDma :: Dma, ioAnswered :: Maybe Word32}
  deriving (Generic, NFDataX, Show, Eq)

initialIo :: Io
initialIo = Io initialSpi initialDma Nothing

-- | Inputs: the CPU's request, a system reset, MISO, the second RAM port's
-- output. Outputs: the answer, the SPI pins (SCK, MOSI, CS active low) and
-- the command to the second RAM port.
ioStep :: Io -> (Maybe BusRequest, Bool, Bit, Word32) -> (Io, (Maybe Word32, (Bit, Bit, Bit), Maybe (MemAddr, Maybe Word32)))
ioStep io (request, reset, miso, ramWord) = (next, (ioAnswered io, spiPins (ioSpi io), dmaRam (ioDma io)))
  where
    spi = (ioSpi io) {spiMiso = miso}
    dma = ioDma io
    (dmaStepped, dmaByte) = stepDma dma spi ramWord
    -- A DMA is started by a CTRL write while idle; its first step is the
    -- next cycle's.
    dma' = case writing ioDmaControl of
      Just c | not (dmaBusy dma) ->
        dma
          { dmaPhase = if testBit c 0 then DmaFetch else if testBit c 1 then DmaToken else DmaReceive,
            dmaLane = 0, dmaWord = 0, dmaInFlight = False, dmaError = False, dmaToken = 0,
            dmaPatience = maxBound, dmaRam = Nothing
          }
      _
        | dmaBusy dma -> dmaStepped
        | otherwise ->
            dmaStepped
              { dmaAddress = maybe (dmaAddress dma) P.id (writing ioDmaAddress),
                dmaCount = maybe (dmaCount dma) resize (writing ioDmaCount)
              }
    taken = case ioAnswered io of Nothing -> request; Just _ -> Nothing
    offset = maybe 0 (\q -> busAddress q .&. 0xfff) taken
    writing at = case taken of
      Just q | busWrite q && offset == at -> Just (busData q)
      _ -> Nothing
    value
      | offset == ioSpiData = resize (spiIn spi)
      | offset == ioSpiStatus = boolWord (spiBusy spi)
      | offset == ioSpiControl = boolWord (spiSelect spi) .|. (resize (spiDivider spi) `shiftL` 8)
      | offset == ioDmaAddress = dmaAddress dma
      | offset == ioDmaCount = resize (dmaCount dma)
      | offset == ioDmaStatus =
          boolWord (dmaBusy dma) .|. (if dmaError dma then 2 else 0) .|. (resize (dmaToken dma) `shiftL` 8)
      | otherwise = 0
    -- While a DMA runs it owns the SPI master's data register.
    spiStart = if dmaBusy dma then dmaByte else resize <$> writing ioSpiData
    next
      | reset = initialIo
      | otherwise =
          Io
            { ioSpi = stepSpi spiStart (writing ioSpiControl) spi,
              ioDma = dma',
              ioAnswered = (\q -> if busWrite q then 0 else value) <$> taken
            }

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
        else busStepWith decodeTarget (systemBus machine) (cpuBusRequest machine) memoryWord txReady (rxHolding machine) (bootCause machine) (cpuDevices machine)
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
            (counterWrite 0xb02 0xb82) (cpuInstret machine),
          -- Devices step from their registers; a reset this cycle quietens
          -- them after the time and compare logic, keeping time.
          cpuDevices = if clearRegisters machine' then initialDevices {devMtime = devMtime devices'} else devices',
          -- From registers, a cycle behind: WFI may finish a cycle late.
          cpuWake = cpuInterruptEnable machine .&. pendingInterrupts (cpuDevices machine) /= 0
        }
      devices' = stepDevices (case rxHolding machine of Nothing -> False; Just _ -> True) txReady effect (cpuDevices machine)
      -- The prefetch arrives three cycles after Decode issued it; a RAM
      -- write to its word (a store into the next instruction) drops it.
      (_, memoryWrite, _) = output
      overwritten = case memoryWrite of
        Just (index, _) -> index == memoryIndex (cpuPrefetchAddress counted)
        Nothing -> False
      -- (a case, not ==: CpuPhase has a one-hot encoding)
      decoding = case cpuPhase machine of Decode -> True; _ -> False
      prefetched
        | clearRegisters counted = counted {cpuPrefetchValid = False, cpuPrefetchCount = 0}
        | cpuPrefetchCount machine == 1 =
            counted {cpuPrefetchCount = 0, cpuPrefetchWord = memoryWord, cpuPrefetchValid = not overwritten}
        | cpuPrefetchCount machine /= 0 && not decoding =
            counted {cpuPrefetchCount = cpuPrefetchCount machine - 1,
              cpuPrefetchValid = cpuPrefetchValid counted && not overwritten}
        | otherwise = counted {cpuPrefetchValid = cpuPrefetchValid counted && not overwritten}
   in (if clearRegisters machine then prefetched {cpuRegs = repeat 0} else prefetched, output)

-- | Keep time while an external memory holds the core (the SDRAM wrapper).
tickHeld :: Machine -> Machine
tickHeld machine =
  machine
    { cpuCycle = tickCounter True Nothing (cpuCycle machine),
      cpuDevices = (cpuDevices machine) {devMtime = tickCounter True Nothing (devMtime (cpuDevices machine))}
    }

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
      rxHolding = Nothing,
      cpuDevices = initialDevices {devMtime = devMtime (cpuDevices machine)},
      cpuPrefetchValid = False,
      cpuPrefetchCount = 0,
      cpuWake = False
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
               -- Interrupts are taken between instructions: mepc is the
               -- instruction about to be fetched.
               -- Every input is a register (the pending bits, mie and MIE),
               -- so a change to MIE or mie applies at the next boundary.
               Fetch -> case interruptCause machineRx of
                 Just cause -> withoutRegister (raiseException machineRx cause 0)
                 Nothing
                   | cpuPrefetchValid && cpuPrefetchAddress == cpuPc ->
                       next machineRx {cpuPhase = Decode, cpuInstruction = cpuPrefetchWord,
                         cpuFetchFault = not (fetchable (decodeTarget cpuPc)), cpuInRom = romAddress cpuPc}
                   | otherwise ->
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
                     withoutRegister
                       ( machineRx
                           { cpuPhase = OperandSelect,
                             cpuABanks = readRegisterBanks (instructionRs1 instruction) cpuRegs,
                             cpuBBanks = readRegisterBanks (instructionRs2 instruction) cpuRegs,
                             cpuImm = decodeImmediate instruction,
                             cpuIsRegOp = instruction .&. 0x7f == 0x33,
                             cpuPrefetchAddress = cpuPc + 4,
                             cpuPrefetchValid = False,
                             cpuPrefetchCount = if cpuLocalMemory then 3 else 0
                           },
                         (if cpuLocalMemory then memoryIndex (cpuPc + 4) else 0, Nothing, Nothing)
                       )
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
                     -- loads and stores: the RAM port is free (the prefetch
                     -- read went out at Decode), so read the word now
                     memoryOp = execOpcode control == 0x03 || execOpcode control == 0x23
                     -- the local RAM's 16-bit compare, not the whole map
                     fast = cpuLocalMemory && memoryOp && validMemoryAddress (cAddress cpuComputed)
                  in withoutRegister
                       ( machineRx
                           { cpuPhase = if execShift control then ShiftFinish else Commit,
                             cpuComputed = cpuComputed {cAlu = result,
                               cTaken = branchTakenPrepared (execFunct3 control) cpuBranchCompare},
                             cpuFastMemory = fast
                           },
                         (if fast then memoryIndex (cAddress cpuComputed) else 0, Nothing, Nothing)
                       )
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
               -- The fast path's word arrives three cycles after ExecuteFinish
               -- issued it: a load aligns it, a sub-word store merges into it.
               MemoryWait -> next machineRx {cpuPhase = MemoryCapture}
               MemoryCapture
                 | testBit instruction 5 ->
                     withoutRegister (finishInstruction machineRx {cpuPc = cLink cpuComputed}
                       (Just (memoryIndex address,
                         storeValue (instructionFunct3 instruction) (resize address) cpuB memoryWord)))
                 | otherwise ->
                     withRegister (instructionRd instruction)
                       (loadValue (instructionFunct3 instruction) (resize address) memoryWord)
                       (finishInstruction machineRx {cpuPc = cLink cpuComputed} Nothing)
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
          | cpuFastMemory machine -> withoutRegister (machine {cpuPhase = MemoryWait}, (0, Nothing, Nothing))
          | otherwise -> requestBus False
        0x23
          | not (validStore funct3) -> invalid
          | misalignedMemory funct3 address -> fault 6 address
          -- a word store writes at once; a byte or halfword waits for the word
          | cpuFastMemory machine && funct3 == 2 ->
              withoutRegister (finishInstruction machine {cpuPc = nextPc} (Just (memoryIndex address, cpuB machine)))
          | cpuFastMemory machine -> withoutRegister (machine {cpuPhase = MemoryWait}, (0, Nothing, Nothing))
          | otherwise -> requestBus True
        0x0f -> noWrite nextPc -- FENCE is a no-op in this tiny single-master core.
        0x73
          | instruction == 0x0000_0073 -> fault 11 0 -- ECALL from M mode
          | instruction == 0x0010_0073 -> fault 3 (cpuPc machine) -- EBREAK
          | instruction == 0x3020_0073 -> withoutRegister (machine {cpuPhase = TrapReturn}, (0, Nothing, Nothing))
          -- WFI waits in Commit until an interrupt is pending and enabled in
          -- mie (whether or not MIE is set).
          | instruction == 0x1050_0073 ->
              if cpuWake machine then noWrite nextPc else withoutRegister (machine, (0, Nothing, Nothing))
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
  0x344 -> Just (pendingInterrupts (cpuDevices machine), False) -- mip; writes ignored
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
      busy = ((selection !! (8 :: Index 14) || selection !! (9 :: Index 14))
                && counterCarry (cpuCycle machine))
          || ((selection !! (10 :: Index 14) || selection !! (11 :: Index 14))
                && counterCarry (cpuInstret machine))
   in if not (or selection) || (cpuCsrReadOnly machine && csrWriteRequested instruction)
        then invalid
        else withoutRegister
          (captured {cpuPhase = if busy then CsrRead else CsrWrite}, (0, Nothing, Nothing))

-- CSR address comparisons are registered at Commit, before the data mux.
decodeCsrSelection :: Unsigned 12 -> Vec 14 Bool
decodeCsrSelection address =
  (address == 0x300) :> (address == 0x301) :>
  (address == 0x304) :> (address == 0x305) :>
  (address == 0x340) :> (address == 0x341) :>
  (address == 0x342) :> (address == 0x343) :>
  (address == 0xb00 || address == 0xc00) :>
  (address == 0xb80 || address == 0xc80) :>
  (address == 0xb02 || address == 0xc02) :>
  (address == 0xb82 || address == 0xc82) :>
  (address == 0x310 || address == 0xf11 ||
   address == 0xf12 || address == 0xf13 || address == 0xf14) :>
  (address == 0x344) :> Nil

readSelectedCsr :: Vec 14 Bool -> Machine -> Word32
readSelectedCsr selection machine =
  let regs = cpuTrapRegisters machine
      values = (0x1800 .|. (if trapMie regs then 8 else 0) .|. (if trapMpie regs then 0x80 else 0)) :>
        0x4000_1100 :> cpuInterruptEnable machine :> trapVector regs :>
        cpuScratch machine :> trapEpc regs :> trapCause regs :> trapValue regs :>
        counterLo (cpuCycle machine) :> counterHi (cpuCycle machine) :>
        counterLo (cpuInstret machine) :> counterHi (cpuInstret machine) :> 0 :>
        pendingInterrupts (cpuDevices machine) :> Nil
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

-- | CLINT and PLIC registers (QEMU virt layout) and the UART control register.
clintMsip, clintCompareLo, clintCompareHi, clintTimeLo, clintTimeHi :: Word32
clintMsip = 0x0200_0000
clintCompareLo = 0x0200_4000
clintCompareHi = 0x0200_4004
clintTimeLo = 0x0200_bff8
clintTimeHi = 0x0200_bffc

plicPriorityUart, plicPending, plicEnable, plicThreshold, plicClaim, uartControl :: Word32
plicPriorityUart = 0x0c00_0000 + 4 * uartIrq
plicPending = 0x0c00_1000
plicEnable = 0x0c00_2000
plicThreshold = 0x0c20_0000
plicClaim = 0x0c20_0004
uartControl = 0x1000_000c

-- | The I/O complex's window and its registers' offsets within it.
ioBase :: Word32
ioBase = 0x1000_1000

ioSpiData, ioSpiStatus, ioSpiControl, ioDmaAddress, ioDmaCount, ioDmaControl, ioDmaStatus :: Word32
ioSpiData = 0x000
ioSpiStatus = 0x004
ioSpiControl = 0x008
ioDmaAddress = 0x100
ioDmaCount = 0x104
ioDmaControl = 0x108
ioDmaStatus = 0x10c

uartTxData, uartStatus, uartRxData :: Word32
uartTxData = 0x1000_0000
uartStatus = 0x1000_0004
uartRxData = 0x1000_0008

decodeMemoryTarget :: Word32 -> MemoryTarget
decodeMemoryTarget address
  | validMemoryAddress address = RamTarget
  | romAddress address = RomTarget
  | address == bootCauseDevice = BootCauseTarget
  | address == clintMsip = ClintMsipTarget
  | address == clintCompareLo = ClintCompareLoTarget
  | address == clintCompareHi = ClintCompareHiTarget
  | address == clintTimeLo = ClintTimeLoTarget
  | address == clintTimeHi = ClintTimeHiTarget
  | address == plicPriorityUart = PlicPriorityTarget
  | address == plicPending = PlicPendingTarget
  | address == plicEnable = PlicEnableTarget
  | address == plicThreshold = PlicThresholdTarget
  | address == plicClaim = PlicClaimTarget
  | address == uartControl = UartControlTarget
  | address .&. 0xffff_f000 == ioBase = IoTarget
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
