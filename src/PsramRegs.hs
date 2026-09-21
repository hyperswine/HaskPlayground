{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | FPGA version of psram_regs.ino (Playground/low-level-stuff), speaking the
-- same protocol so the unmodified psramd host driver can talk to it.
--
-- 100 x 32-bit registers: DEVICE_ID, VERSION, CTRL, STATUS, SLOT[96].
-- Host frames arrive over the UART, 8 bytes each (value little-endian):
--
-- >   A5 'W' idx v0 v1 v2 v3 crc8(bytes 1..6)   write register idx
-- >   A5 'R' 00 00 00 00 00 crc8               request a snapshot
--
-- Slot writes go to a 96-word block RAM standing in for PSRAM (the sketch's
-- USE_SPI_PSRAM 0 mode) while CTRL.enable is set, and are read back to verify.
-- The slots themselves, and the copy a snapshot prints from, are block RAMs too.
--
-- Every change to register state emits one newline-terminated text line
--
-- >   S <seq> <crc8 of the 400 raw LE bytes> <100 registers x 8 hex digits>
--
-- plus "I ..." info and "E ..." error lines, byte-for-byte the sketch's text.
--
-- The design is a pure byte-level step function ('psramStep', tested without
-- any UART in test/PsramRegs.hs) wrapped with a UART and the block RAM.
module PsramRegs
  ( -- * Hardware
    topEntity,
    psramRegs,
    PsramConfig (..),
    boardConfig,

    -- * Byte-level core
    psramStep,
    initialState,
    State (..),
    StepInput,
    StepOutput,
    RamPort,

    -- * Register map and helpers
    Byte,
    Word32,
    RegIdx,
    SlotIdx,
    regDeviceId,
    regVersion,
    regCtrl,
    regStatus,
    crc8Step,
    uartRx,
    uartTx,
  )
where

import Clash.Prelude
import qualified Prelude as P

type Byte = Unsigned 8

type Word32 = Unsigned 32

type RegIdx = Index 100

type SlotIdx = Index 96

regDeviceId, regVersion, regCtrl, regStatus :: RegIdx
regDeviceId = 0
regVersion = 1
regCtrl = 2
regStatus = 3

ctrlEnable, ctrlFlush, stErr :: Word32
ctrlEnable = 1
ctrlFlush = 2
stErr = 2 -- STATUS bit 1: sticky, write 1 to clear.  Bit 0 (BUSY) is never
-- visible: snapshots only start while the PSRAM machine is idle.

slotReg :: SlotIdx -> RegIdx
slotReg n = resize n + 4

-- Messages ---------------------------------------------------------------------

data Msg
  = MBoot
  | MBadCrc
  | MBadCmd Byte
  | MReadOnly Byte
  | MBadIndex Byte
  | MVerify SlotIdx
  | MOverrun -- a frame completed while the previous one was still pending
  | MLost Byte -- messages dropped because the queue was full (saturates at 255)
  deriving (Generic, NFDataX, Eq, Show)

-- Strings are spliced in as byte vectors (inline: splices cannot use local helpers).
sBoot :: Vec 29 Byte
sBoot = $(listToVecTH (P.map (P.fromIntegral P.. P.fromEnum) "I psram_regs 1.0 (fpga bram)\n" :: [Unsigned 8]))

sBadCrc :: Vec 10 Byte
sBadCrc = $(listToVecTH (P.map (P.fromIntegral P.. P.fromEnum) "E bad crc\n" :: [Unsigned 8]))

sBadCmd :: Vec 10 Byte
sBadCmd = $(listToVecTH (P.map (P.fromIntegral P.. P.fromEnum) "E bad cmd " :: [Unsigned 8]))

sReg :: Vec 6 Byte
sReg = $(listToVecTH (P.map (P.fromIntegral P.. P.fromEnum) "E reg " :: [Unsigned 8]))

sReadOnly :: Vec 13 Byte
sReadOnly = $(listToVecTH (P.map (P.fromIntegral P.. P.fromEnum) " is read-only" :: [Unsigned 8]))

sBadIndex :: Vec 12 Byte
sBadIndex = $(listToVecTH (P.map (P.fromIntegral P.. P.fromEnum) "E bad index " :: [Unsigned 8]))

sVerify :: Vec 27 Byte
sVerify = $(listToVecTH (P.map (P.fromIntegral P.. P.fromEnum) "E psram verify failed slot " :: [Unsigned 8]))

sOverrun :: Vec 16 Byte
sOverrun = $(listToVecTH (P.map (P.fromIntegral P.. P.fromEnum) "E frame overrun\n" :: [Unsigned 8]))

sLost :: Vec 7 Byte
sLost = $(listToVecTH (P.map (P.fromIntegral P.. P.fromEnum) "E lost " :: [Unsigned 8]))

sMessages :: Vec 10 Byte
sMessages = $(listToVecTH (P.map (P.fromIntegral P.. P.fromEnum) " messages\n" :: [Unsigned 8]))

newline :: Byte
newline = 10

hexDigit :: Unsigned 4 -> Byte
hexDigit n = if n < 10 then 48 + resize n else 55 + resize n

-- | %u of a byte: up to three digits, leading zeros as Nothing (skipped).
decimal3 :: Byte -> Vec 3 (Maybe Byte)
decimal3 n = hundreds :> tens :> Just (48 + ones) :> Nil
  where
    h = n `div` 100
    t = (n `div` 10) `mod` 10
    ones = n `mod` 10
    hundreds = if h /= 0 then Just (48 + h) else Nothing
    tens = if h /= 0 || t /= 0 then Just (48 + t) else Nothing

-- | The text of a message, padded with Nothing (which the emitter skips).
render :: Msg -> Vec 32 (Maybe Byte)
render msg = case msg of
  MBoot -> pad (map Just sBoot)
  MBadCrc -> pad (map Just sBadCrc)
  MBadCmd c -> pad (map Just sBadCmd ++ Just (hexDigit (hi c)) :> Just (hexDigit (lo c)) :> Just newline :> Nil)
  MReadOnly i -> pad (map Just sReg ++ decimal3 i ++ map Just sReadOnly ++ Just newline :> Nil)
  MBadIndex i -> pad (map Just sBadIndex ++ decimal3 i ++ Just newline :> Nil)
  MVerify n -> pad (map Just sVerify ++ decimal3 (fromIntegral n) ++ Just newline :> Nil)
  MOverrun -> pad (map Just sOverrun)
  MLost n -> pad (map Just sLost ++ decimal3 n ++ plus ++ map Just sMessages)
    where
      plus = (if n == maxBound then Just 43 else Nothing) :> Nil -- '+' when saturated
  where
    hi c = resize (c `shiftR` 4)
    lo c = resize c
    pad :: (KnownNat n) => Vec n (Maybe Byte) -> Vec 32 (Maybe Byte)
    pad v = imap (\i _ -> if fromIntegral i < length v then v !! i else Nothing) (repeat () :: Vec 32 ())

-- CRC-8, polynomial 0x07, init 0 (the sketch's crc8) --------------------------

crc8Step :: Byte -> Byte -> Byte
crc8Step c0 b = go (0 :: Index 8) (c0 `xor` b)
  where
    go i c =
      let c' = if testBit c 7 then (c `shiftL` 1) `xor` 0x07 else c `shiftL` 1
       in if i == maxBound then c' else go (i + 1) c'

crc8 :: Vec n Byte -> Byte
crc8 = foldl crc8Step 0

-- State ------------------------------------------------------------------------
--
-- DEVICE_ID and VERSION are constants and CTRL/STATUS live in flip-flops; the
-- 96 slots live in block RAM ("slots"), with a second block RAM ("shadow")
-- holding the copy a snapshot prints from, and a third standing in for PSRAM.
-- Block RAM reads take one cycle, so every RAM port address is computed from
-- the state the machine is about to enter: the data is there when it arrives.

-- | The sequencer that owns the slot RAM, the shadow RAM and the PSRAM.
data Op
  = OpIdle
  | OpSlotWrite SlotIdx Word32 -- slots[n] (old value) is on the read port
  | OpFlushWrite SlotIdx -- slots[n] is on the read port: copy it to PSRAM
  | OpPsRead SlotIdx Word32 Bool -- PSRAM write committed; read it back
  | OpPsCheck SlotIdx Word32 Bool -- PSRAM[n] is on the read port: compare. Bool: flushing
  | OpCopy SlotIdx -- snapshot start: slots[n] is on the read port, copy to shadow
  deriving (Generic, NFDataX, Eq, Show)

-- | Snapshot emitter.  The CRC pass walks the shadow byte by byte first,
-- because the CRC is printed before the register dump.
data Snap
  = SnCrc RegIdx (Index 4)
  | SnLetter -- "S "
  | SnSpace0
  | SnSeq (Index 10) Bool -- digit (most significant first), started printing
  | SnSpace1
  | SnCrcHi
  | SnCrcLo
  | SnSpace2
  | SnHex RegIdx (Index 8)
  | SnNewline
  deriving (Generic, NFDataX, Eq, Show)

data Emit
  = EIdle
  | EMsg (Vec 32 (Maybe Byte)) (Index 32)
  | ESnap Snap
  deriving (Generic, NFDataX, Eq, Show)

data State = State
  { sCtrl :: Word32,
    sStatus :: Word32,
    sShadowCtrl :: Word32, -- CTRL and STATUS as of the snapshot being printed
    sShadowStatus :: Word32,
    sSeq :: Word32,
    sSeqDigits :: Vec 10 (Index 10), -- sSeq in decimal, most significant first
    sShadowSeq :: Vec 10 (Index 10), -- sequence number of the snapshot being printed
    sSnapCrc :: Byte,
    sDirty :: Bool, -- registers changed (or a snapshot was requested) since the last snapshot began
    sFrame :: Vec 8 Byte,
    sFrameLen :: Index 9,
    sFrameAge :: Unsigned 32, -- cycles since the partial frame's first byte
    sPending :: Maybe (Vec 8 Byte),
    sOverruns :: Byte, -- frames dropped because one was still pending (saturates)
    sOp :: Op,
    sQueue :: Vec 8 Msg,
    sQueueHead :: Index 8,
    sQueueCount :: Index 9,
    sDropped :: Byte,
    sEmit :: Emit
  }
  deriving (Generic, NFDataX, Show)

initialState :: State
initialState =
  State
    { sCtrl = 0,
      sStatus = 0,
      sShadowCtrl = 0,
      sShadowStatus = 0,
      sSeq = 0,
      sSeqDigits = repeat 0,
      sShadowSeq = repeat 0,
      sSnapCrc = 0,
      sDirty = True, -- the boot snapshot
      sFrame = repeat 0,
      sFrameLen = 0,
      sFrameAge = 0,
      sPending = Nothing,
      sOverruns = 0,
      sOp = OpIdle,
      sQueue = MBoot :> repeat MBadCrc,
      sQueueHead = 0,
      sQueueCount = 1,
      sDropped = 0,
      sEmit = EIdle
    }

deviceId, version :: Word32
deviceId = 0x5053_5231 -- "PSR1"
version = 0x0001_0000 -- 1.0

-- | A block RAM port: read address, optional write.
type RamPort = (SlotIdx, Maybe (SlotIdx, Word32))

-- | (received byte, UART transmitter ready, PSRAM / slot / shadow RAM read data)
type StepInput = (Maybe Byte, Bool, Word32, Word32, Word32)

-- | (byte to transmit, PSRAM port, slot RAM port, shadow RAM port)
type StepOutput = (Maybe Byte, RamPort, RamPort, RamPort)

-- Step -------------------------------------------------------------------------

-- | One clock cycle.  frameTimeout is in cycles (the sketch's 50 ms).
psramStep :: Unsigned 32 -> State -> StepInput -> (State, StepOutput)
psramStep frameTimeout s0 (rxByte, txReady, psramOut, slotOut, shadowOut) =
  (s4, (txByte, (psramAddr, psramWr), (slotAddr, slotWr), (shadowAddr, shadowWr)))
  where
    s1 = receive frameTimeout rxByte s0
    (s2, psramWr, slotWr, shadowWr) = sequencer psramOut slotOut s1
    s3 = if snapshotDue s2 then beginSnapshot s2 else s2
    -- read addresses for the state being entered
    psramAddr = case sOp s3 of OpPsCheck n _ _ -> n; _ -> 0
    slotAddr = case sOp s3 of
      OpSlotWrite n _ -> n
      OpFlushWrite n -> n
      OpCopy n -> n
      _ -> 0
    word = shadowWord s0 shadowOut (emitReg (sEmit s3))
    (s4, txByte) = emit txReady word s3
    shadowAddr = slotOf (emitReg (sEmit s4))

-- | Register r of the snapshot being printed.  Slots come from the shadow RAM,
-- whose read port was pointed at r last cycle.
shadowWord :: State -> Word32 -> RegIdx -> Word32
shadowWord s shadowOut r
  | r == regDeviceId = deviceId
  | r == regVersion = version
  | r == regCtrl = sShadowCtrl s
  | r == regStatus = sShadowStatus s
  | otherwise = shadowOut

slotOf :: RegIdx -> SlotIdx
slotOf r = if r >= 4 then resize (r - 4) else 0

-- | Frame assembly, mirroring the sketch's loop(): a partial frame older than
-- the timeout is dropped when the next byte arrives, 0xA5 starts a frame.
receive :: Unsigned 32 -> Maybe Byte -> State -> State
receive timeout rxByte s@State {..} = case rxByte of
  Nothing -> s {sFrameAge = aged}
  Just b
    | len == 0 && b /= 0xA5 -> s {sFrameLen = 0, sFrameAge = 0}
    | len == 0 -> s {sFrame = replace (0 :: Index 8) b sFrame, sFrameLen = 1, sFrameAge = 0}
    | len == 7 ->
        let s' = s {sFrameLen = 0, sFrameAge = 0}
         in case sPending of
              Nothing -> s' {sPending = Just (replace (7 :: Index 8) b sFrame)}
              Just _ -> s' {sOverruns = if sOverruns == maxBound then sOverruns else sOverruns + 1}
    | otherwise -> s {sFrame = replace len b sFrame, sFrameLen = len + 1, sFrameAge = aged}
  where
    stale = sFrameLen /= 0 && sFrameAge > timeout
    len = if stale then 0 else sFrameLen
    aged = if sFrameLen /= 0 && sFrameAge /= maxBound then sFrameAge + 1 else sFrameAge

-- | Queue a message; count it as lost when the queue is full.
push :: Msg -> State -> State
push m s@State {..}
  | sQueueCount == 8 = s {sDropped = if sDropped == maxBound then sDropped else sDropped + 1}
  | otherwise =
      s
        { sQueue = replace (satAdd SatWrap sQueueHead (resize sQueueCount)) m sQueue,
          sQueueCount = sQueueCount + 1
        }

-- | Set STATUS.ERR (a change if it was clear) and report m.
failWith :: Msg -> State -> State
failWith m s = push m s {sStatus = sStatus s .|. stErr, sDirty = sDirty s || sStatus s .&. stErr == 0}

-- | One step of the RAM sequencer, or the pending frame when it is idle.
-- Returns the PSRAM, slot RAM and shadow RAM writes for this cycle.
sequencer :: Word32 -> Word32 -> State -> (State, Maybe (SlotIdx, Word32), Maybe (SlotIdx, Word32), Maybe (SlotIdx, Word32))
sequencer psramOut slotOut s = case sOp s of
  OpSlotWrite n v ->
    let s' = s {sDirty = sDirty s || slotOut /= v}
        enabled = sCtrl s .&. ctrlEnable /= 0
     in ( s' {sOp = if enabled then OpPsRead n v False else OpIdle},
          if enabled then Just (n, v) else Nothing,
          Just (n, v),
          Nothing
        )
  OpFlushWrite n -> (s {sOp = OpPsRead n slotOut True}, Just (n, slotOut), Nothing, Nothing)
  OpPsRead n v fl -> (s {sOp = OpPsCheck n v fl}, Nothing, Nothing, Nothing)
  OpPsCheck n v fl ->
    let next = if fl && n /= maxBound then OpFlushWrite (n + 1) else OpIdle
        s' = s {sOp = next}
     in (if psramOut /= v then failWith (MVerify n) s' else s', Nothing, Nothing, Nothing)
  OpCopy n ->
    let done = n == maxBound
     in ( s {sOp = if done then OpIdle else OpCopy (n + 1), sEmit = if done then ESnap (SnCrc 0 0) else sEmit s},
          Nothing,
          Nothing,
          Just (n, slotOut)
        )
  OpIdle -> case sPending s of
    Nothing -> (owedOverrun s, Nothing, Nothing, Nothing)
    Just f -> (handleFrame f s {sPending = Nothing}, Nothing, Nothing, Nothing)

-- | Report one dropped frame when nothing else is being reported.
owedOverrun :: State -> State
owedOverrun s
  | sOverruns s /= 0 && sQueueCount s == 0 = push MOverrun s {sOverruns = sOverruns s - 1}
  | otherwise = s

handleFrame :: Vec 8 Byte -> State -> State
handleFrame f s
  | crc8 (take (SNat :: SNat 6) (tail f)) /= f !! (7 :: Index 8) = failWith MBadCrc s
  | cmd == 0x57 = writeReg (f !! (2 :: Index 8)) value s -- 'W'
  | cmd == 0x52 = s {sDirty = True} -- 'R'
  | otherwise = failWith (MBadCmd cmd) s
  where
    cmd = f !! (1 :: Index 8)
    value = bitCoerce (f !! (6 :: Index 8) :> f !! (5 :: Index 8) :> f !! (4 :: Index 8) :> f !! (3 :: Index 8) :> Nil)

-- | The sketch's applyWrite.  A slot write reads the old value first (for
-- change detection) in OpSlotWrite; CTRL and STATUS are flip-flops.
writeReg :: Byte -> Word32 -> State -> State
writeReg idx v s
  | idx >= 100 = failWith (MBadIndex idx) s
  | r == regDeviceId || r == regVersion = failWith (MReadOnly idx) s
  | r == regCtrl =
      let ctrl' = v .&. ctrlEnable
       in s
            { sCtrl = ctrl',
              sDirty = sDirty s || ctrl' /= sCtrl s,
              sOp = if v .&. ctrlFlush /= 0 then OpFlushWrite 0 else OpIdle
            }
  | r == regStatus =
      let status' = if v .&. stErr /= 0 then sStatus s .&. complement stErr else sStatus s
       in s {sStatus = status', sDirty = sDirty s || status' /= sStatus s}
  | otherwise = s {sOp = OpSlotWrite (resize (r - 4)) v}
  where
    r = fromIntegral idx :: RegIdx -- only used once idx < 100

-- | Start a snapshot when nothing else is going on: the emitter is idle, the
-- queued messages are out (a batch's errors precede its snapshot, as in the
-- sketch), and the sequencer is idle (so BUSY is clear, as in the sketch).
snapshotDue :: State -> Bool
snapshotDue State {..} =
  sEmit == EIdle && sQueueCount == 0 && sDropped == 0 && sOverruns == 0 && sDirty && sOp == OpIdle && sPending == Nothing

-- | Latch CTRL/STATUS/seq and copy the slots to the shadow RAM; printing
-- starts when the copy finishes (see OpCopy).
beginSnapshot :: State -> State
beginSnapshot s@State {..} =
  s
    { sShadowCtrl = sCtrl,
      sShadowStatus = sStatus,
      sShadowSeq = sSeqDigits,
      sSeq = sSeq + 1,
      sSeqDigits = if sSeq == maxBound then repeat 0 else incDecimal sSeqDigits,
      sDirty = False,
      sSnapCrc = 0,
      sOp = OpCopy 0
    }

incDecimal :: Vec 10 (Index 10) -> Vec 10 (Index 10)
incDecimal ds = reverse (snd (mapAccumL step True (reverse ds)))
  where
    step carry d
      | not carry = (False, d)
      | d == maxBound = (True, 0)
      | otherwise = (False, d + 1)

-- | Which snapshot register the emitter reads in this state.
emitReg :: Emit -> RegIdx
emitReg e = case e of
  ESnap (SnCrc r _) -> r
  ESnap (SnHex r _) -> r
  _ -> 0

-- | Drive the UART: queued messages first, then the lost count, then snapshots.
-- word is snapshot register 'emitReg' of the current state.
emit :: Bool -> Word32 -> State -> (State, Maybe Byte)
emit txReady word s@State {..} = case sEmit of
  EIdle
    | sQueueCount /= 0 ->
        ( s
            { sEmit = EMsg (render (sQueue !! sQueueHead)) 0,
              sQueueHead = satSucc SatWrap sQueueHead,
              sQueueCount = sQueueCount - 1
            },
          Nothing
        )
    | sDropped /= 0 -> (s {sEmit = EMsg (render (MLost sDropped)) 0, sDropped = 0}, Nothing)
    | otherwise -> (s, Nothing)
  EMsg text pos ->
    let next = if pos == maxBound then EIdle else EMsg text (pos + 1)
     in case text !! pos of
          Nothing -> (s {sEmit = next}, Nothing)
          Just b
            | txReady -> (s {sEmit = next}, Just b)
            | otherwise -> (s, Nothing)
  ESnap snap -> case snap of
    SnCrc r i ->
      let byte = (bitCoerce word :: Vec 4 Byte) !! (3 - i) -- little-endian byte i
          next
            | i /= maxBound = SnCrc r (i + 1)
            | r /= maxBound = SnCrc (r + 1) 0
            | otherwise = SnLetter
       in (s {sSnapCrc = crc8Step sSnapCrc byte, sEmit = ESnap next}, Nothing)
    SnLetter -> send 83 (ESnap SnSpace0)
    SnSpace0 -> send 32 (ESnap (SnSeq 0 False))
    SnSeq i started ->
      let d = sShadowSeq !! i
          printing = started || d /= 0 || i == maxBound
          next = ESnap (if i == maxBound then SnSpace1 else SnSeq (i + 1) printing)
       in if not printing
            then (s {sEmit = next}, Nothing)
            else send (48 + fromIntegral d) next
    SnSpace1 -> send 32 (ESnap SnCrcHi)
    SnCrcHi -> send (hexDigit (resize (sSnapCrc `shiftR` 4))) (ESnap SnCrcLo)
    SnCrcLo -> send (hexDigit (resize sSnapCrc)) (ESnap SnSpace2)
    SnSpace2 -> send 32 (ESnap (SnHex 0 0))
    SnHex r k ->
      let nib = (bitCoerce word :: Vec 8 (Unsigned 4)) !! k -- most significant first
          next
            | k /= maxBound = SnHex r (k + 1)
            | r /= maxBound = SnHex (r + 1) 0
            | otherwise = SnNewline
       in send (hexDigit nib) (ESnap next)
    SnNewline -> send newline EIdle
  where
    send b next
      | txReady = (s {sEmit = next}, Just b)
      | otherwise = (s, Nothing)

-- UART -------------------------------------------------------------------------

-- | Board configuration: clocks per UART bit and frame timeout in cycles.
data PsramConfig = PsramConfig {clocksPerBit :: Unsigned 16, timeoutCycles :: Unsigned 32}

-- | 27 MHz: 234 clocks per bit is 115385 baud (+0.16% from 115200); 50 ms timeout.
boardConfig :: PsramConfig
boardConfig = PsramConfig {clocksPerBit = 234, timeoutCycles = 1_350_000}

data RxState
  = RxIdle
  | RxStart (Unsigned 16)
  | RxData (Index 8) (Unsigned 16) Byte
  | RxStop (Unsigned 16) Byte
  deriving (Generic, NFDataX)

-- | 8-N-1 receiver sampling mid-bit.  The input must already be synchronised.
uartRx :: (HiddenClockResetEnable dom) => Unsigned 16 -> Signal dom Bit -> Signal dom (Maybe Byte)
uartRx cpb = mealy rxStep RxIdle
  where
    full = cpb - 1
    half = cpb `div` 2 - 1
    rxStep RxIdle b
      | b == low = (RxStart half, Nothing)
      | otherwise = (RxIdle, Nothing)
    rxStep (RxStart n) b
      | n /= 0 = (RxStart (n - 1), Nothing)
      | b == low = (RxData 0 full 0, Nothing)
      | otherwise = (RxIdle, Nothing)
    rxStep (RxData i n byte) b
      | n /= 0 = (RxData i (n - 1) byte, Nothing)
      | otherwise =
          let byte' = if b == high then setBit byte (fromIntegral i) else byte
           in if i == maxBound then (RxStop full byte', Nothing) else (RxData (i + 1) full byte', Nothing)
    rxStep (RxStop n byte) b
      | n /= 0 = (RxStop (n - 1) byte, Nothing)
      | b == high = (RxIdle, Just byte)
      | otherwise = (RxIdle, Nothing)

data TxState
  = TxIdle
  | TxStart (Unsigned 16) Byte
  | TxData (Index 8) (Unsigned 16) Byte
  | TxStop (Unsigned 16)
  deriving (Generic, NFDataX)

-- | 8-N-1 transmitter.  Ready is high in idle; a request made while ready is taken.
uartTx :: (HiddenClockResetEnable dom) => Unsigned 16 -> Signal dom (Maybe Byte) -> (Signal dom Bit, Signal dom Bool)
uartTx cpb req = unbundle (mealy txStep TxIdle req)
  where
    full = cpb - 1
    txStep TxIdle r = (maybe TxIdle (TxStart full) r, (high, True))
    txStep (TxStart n byte) _
      | n /= 0 = (TxStart (n - 1) byte, (low, False))
      | otherwise = (TxData 0 full byte, (low, False))
    txStep (TxData i n byte) _
      | n /= 0 = (TxData i (n - 1) byte, (bit' i byte, False))
      | i == maxBound = (TxStop full, (bit' i byte, False))
      | otherwise = (TxData (i + 1) full byte, (bit' i byte, False))
    txStep (TxStop n) _
      | n /= 0 = (TxStop (n - 1), (high, False))
      | otherwise = (TxIdle, (high, False))
    bit' i byte = boolToBit (testBit byte (fromIntegral i))

-- Top level --------------------------------------------------------------------

psramRegs :: (HiddenClockResetEnable dom) => PsramConfig -> Signal dom Bit -> Signal dom Bit
psramRegs PsramConfig {..} serialRx = serialTx
  where
    received = register Nothing (uartRx clocksPerBit (register high (register high serialRx)))
    (serialTx, txReady) = uartTx clocksPerBit txByte
    (txByte, psramPort, slotPort, shadowPort) =
      unbundle (mealy (psramStep timeoutCycles) initialState (bundle (received, txReady, psramOut, slotOut, shadowOut)))
    ram port = let (addr, wr) = unbundle port in blockRam (repeat 0 :: Vec 96 Word32) addr wr
    psramOut = ram psramPort
    slotOut = ram slotPort
    shadowOut = ram shadowPort

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "psram_regs",
        t_inputs = [PortName "clk", PortName "reset", PortName "enable", PortName "uart_rx"],
        t_output = PortName "uart_tx"
      }
  )
  #-}
-- XilinxSystem: synchronous reset, as in SimpleRisc (Gowin FFs have a sync-reset pin).
topEntity ::
  "CLK" ::: Clock XilinxSystem ->
  "RESET" ::: Reset XilinxSystem ->
  "ENABLE" ::: Enable XilinxSystem ->
  "UART_RX" ::: Signal XilinxSystem Bit ->
  "UART_TX" ::: Signal XilinxSystem Bit
topEntity clk rst en serialRx = withClockResetEnable clk rst en (psramRegs boardConfig serialRx)
