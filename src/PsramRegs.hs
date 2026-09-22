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
    State,
    Core,
    Emitter,
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
  | EMsg Msg (Index 32) -- message, next character position
  | ESnap Snap
  deriving (Generic, NFDataX, Eq, Show)

-- | The core: frame assembly, CTRL/STATUS, and the sequencer that owns the
-- slot RAM, the PSRAM and the shadow RAM's write port.
--
-- The core and the emitter are separate machines with separate state records
-- on purpose: Clash multiplexes a whole record through every branch of a case
-- that returns it, so each case should only carry the fields it owns.
data Core = Core
  { cCtrl :: Word32,
    cStatus :: Word32,
    cShadowCtrl :: Word32, -- CTRL and STATUS as of the snapshot being printed
    cShadowStatus :: Word32,
    cDirty :: Bool, -- registers changed (or a snapshot was requested) since the last snapshot began
    cFrame :: Vec 8 Byte,
    cFrameLen :: Index 9,
    cFrameAge :: Unsigned 32, -- cycles since the partial frame's first byte
    cPending :: Maybe (Vec 8 Byte),
    cOverruns :: Byte, -- frames dropped because one was still pending (saturates)
    cOp :: Op
  }
  deriving (Generic, NFDataX, Show)

-- | The emitter: the message queue and everything the UART prints.
data Emitter = Emitter
  { eQueue :: Vec 8 Msg,
    eQueueHead :: Index 8,
    eQueueCount :: Index 9,
    eDropped :: Byte, -- messages lost to a full queue (saturates)
    eEmit :: Emit,
    eSnapCrc :: Byte,
    eSeq :: Word32,
    eSeqDigits :: Vec 10 (Index 10) -- eSeq in decimal, most significant first
  }
  deriving (Generic, NFDataX, Show)

-- | Both machines' state, for stepping the design as one function in tests.
type State = (Core, Emitter)

initialCore :: Core
initialCore =
  Core
    { cCtrl = 0,
      cStatus = 0,
      cShadowCtrl = 0,
      cShadowStatus = 0,
      cDirty = True, -- the boot snapshot
      cFrame = repeat 0,
      cFrameLen = 0,
      cFrameAge = 0,
      cPending = Nothing,
      cOverruns = 0,
      cOp = OpIdle
    }

initialEmitter :: Emitter
initialEmitter =
  Emitter
    { eQueue = MBoot :> repeat MBadCrc,
      eQueueHead = 0,
      eQueueCount = 1,
      eDropped = 0,
      eEmit = EIdle,
      eSnapCrc = 0,
      eSeq = 0,
      eSeqDigits = repeat 0
    }

initialState :: State
initialState = (initialCore, initialEmitter)

deviceId, version :: Word32
deviceId = 0x5053_5231 -- "PSR1"
version = 0x0001_0000 -- 1.0

-- | A block RAM port: read address, optional write.
type RamPort = (SlotIdx, Maybe (SlotIdx, Word32))

type RamWrite = Maybe (SlotIdx, Word32)

-- | (received byte, UART transmitter ready, PSRAM / slot / shadow RAM read data)
type StepInput = (Maybe Byte, Bool, Word32, Word32, Word32)

-- | (byte to transmit, PSRAM port, slot RAM port, shadow RAM port)
type StepOutput = (Maybe Byte, RamPort, RamPort, RamPort)

-- | What the core tells the emitter each cycle: a message (at most one), start
-- printing a snapshot (the shadow copy is complete), and CTRL/STATUS as of
-- that snapshot.
type CoreToEmitter = (Maybe Msg, Bool, Word32, Word32)

-- Step -------------------------------------------------------------------------

-- | One clock cycle of the whole design, the two machines wired as in
-- 'psramRegs'.  frameTimeout is in cycles (the sketch's 50 ms).
psramStep :: Unsigned 32 -> State -> StepInput -> (State, StepOutput)
psramStep frameTimeout (core, emitter) (rxByte, txReady, psramOut, slotOut, shadowOut) =
  ((core', emitter'), (txByte, psramPort, slotPort, (shadowAddr, shadowWr)))
  where
    (core', (toEmitter, psramPort, slotPort, shadowWr)) =
      coreStep frameTimeout core (rxByte, emitterQuiet emitter, psramOut, slotOut)
    (emitter', (txByte, shadowAddr, _)) = emitStep emitter (txReady, toEmitter, shadowOut)

-- | Nothing queued or being printed.  A function of the emitter's registers
-- only, so the core can read it without a combinational loop.
emitterQuiet :: Emitter -> Bool
emitterQuiet Emitter {..} = eEmit == EIdle && eQueueCount == 0 && eDropped == 0

-- Core -------------------------------------------------------------------------

-- | Input: (received byte, emitter quiet, PSRAM read data, slot RAM read data).
coreStep ::
  Unsigned 32 ->
  Core ->
  (Maybe Byte, Bool, Word32, Word32) ->
  (Core, (CoreToEmitter, RamPort, RamPort, RamWrite))
coreStep frameTimeout c0 (rxByte, quiet, psramOut, slotOut) =
  (c3, ((msg, startPrinting, cShadowCtrl c0, cShadowStatus c0), (psramAddr, psramWr), (slotAddr, slotWr), shadowWr))
  where
    c1 = receive frameTimeout rxByte c0
    (c2, msg, startPrinting, psramWr, slotWr, shadowWr) = sequencer quiet psramOut slotOut c1
    -- a batch's messages precede its snapshot, as in the sketch
    c3 = if quiet && msg == Nothing && snapshotDue c2 then beginSnapshot c2 else c2
    -- read addresses for the state being entered (block RAM reads take a cycle)
    psramAddr = case cOp c3 of OpPsCheck n _ _ -> n; _ -> 0
    slotAddr = case cOp c3 of
      OpSlotWrite n _ -> n
      OpFlushWrite n -> n
      OpCopy n -> n
      _ -> 0

-- | Frame assembly, mirroring the sketch's loop(): a partial frame older than
-- the timeout is dropped when the next byte arrives, 0xA5 starts a frame.
receive :: Unsigned 32 -> Maybe Byte -> Core -> Core
receive timeout rxByte c@Core {..} = case rxByte of
  Nothing -> c {cFrameAge = aged}
  Just b
    | len == 0 && b /= 0xA5 -> c {cFrameLen = 0, cFrameAge = 0}
    | len == 0 -> c {cFrame = replace (0 :: Index 8) b cFrame, cFrameLen = 1, cFrameAge = 0}
    | len == 7 ->
        let c' = c {cFrameLen = 0, cFrameAge = 0}
         in case cPending of
              Nothing -> c' {cPending = Just (replace (7 :: Index 8) b cFrame)}
              Just _ -> c' {cOverruns = if cOverruns == maxBound then cOverruns else cOverruns + 1}
    | otherwise -> c {cFrame = replace len b cFrame, cFrameLen = len + 1, cFrameAge = aged}
  where
    stale = cFrameLen /= 0 && cFrameAge > timeout
    len = if stale then 0 else cFrameLen
    aged = if cFrameLen /= 0 && cFrameAge /= maxBound then cFrameAge + 1 else cFrameAge

-- | Set STATUS.ERR (a change if it was clear) and report m.
failWith :: Msg -> Core -> (Core, Maybe Msg)
failWith m c = (c {cStatus = cStatus c .|. stErr, cDirty = cDirty c || cStatus c .&. stErr == 0}, Just m)

-- | One step of the RAM sequencer, or the pending frame when it is idle.
-- Returns the message for the emitter, whether the shadow copy just
-- finished, and the PSRAM, slot RAM and shadow RAM writes.
sequencer :: Bool -> Word32 -> Word32 -> Core -> (Core, Maybe Msg, Bool, RamWrite, RamWrite, RamWrite)
sequencer quiet psramOut slotOut c = case cOp c of
  OpSlotWrite n v ->
    let enabled = cCtrl c .&. ctrlEnable /= 0
     in ( c {cDirty = cDirty c || slotOut /= v, cOp = if enabled then OpPsRead n v False else OpIdle},
          Nothing,
          False,
          if enabled then Just (n, v) else Nothing,
          Just (n, v),
          Nothing
        )
  OpFlushWrite n -> (c {cOp = OpPsRead n slotOut True}, Nothing, False, Just (n, slotOut), Nothing, Nothing)
  OpPsRead n v fl -> (c {cOp = OpPsCheck n v fl}, Nothing, False, Nothing, Nothing, Nothing)
  OpPsCheck n v fl ->
    let c' = c {cOp = if fl && n /= maxBound then OpFlushWrite (n + 1) else OpIdle}
        (c'', msg) = if psramOut /= v then failWith (MVerify n) c' else (c', Nothing)
     in (c'', msg, False, Nothing, Nothing, Nothing)
  OpCopy n ->
    let done = n == maxBound
     in (c {cOp = if done then OpIdle else OpCopy (n + 1)}, Nothing, done, Nothing, Nothing, Just (n, slotOut))
  OpIdle -> case cPending c of
    Nothing
      -- report one dropped frame when nothing else is being reported
      | cOverruns c /= 0 && quiet -> (c {cOverruns = cOverruns c - 1}, Just MOverrun, False, Nothing, Nothing, Nothing)
      | otherwise -> (c, Nothing, False, Nothing, Nothing, Nothing)
    Just f ->
      let (c', msg) = handleFrame f c {cPending = Nothing}
       in (c', msg, False, Nothing, Nothing, Nothing)

handleFrame :: Vec 8 Byte -> Core -> (Core, Maybe Msg)
handleFrame f c
  | crc8 (take (SNat :: SNat 6) (tail f)) /= f !! (7 :: Index 8) = failWith MBadCrc c
  | cmd == 0x57 = writeReg (f !! (2 :: Index 8)) value c -- 'W'
  | cmd == 0x52 = (c {cDirty = True}, Nothing) -- 'R'
  | otherwise = failWith (MBadCmd cmd) c
  where
    cmd = f !! (1 :: Index 8)
    value = bitCoerce (f !! (6 :: Index 8) :> f !! (5 :: Index 8) :> f !! (4 :: Index 8) :> f !! (3 :: Index 8) :> Nil)

-- | The sketch's applyWrite.  A slot write reads the old value first (for
-- change detection) in OpSlotWrite; CTRL and STATUS are flip-flops.
writeReg :: Byte -> Word32 -> Core -> (Core, Maybe Msg)
writeReg idx v c
  | idx >= 100 = failWith (MBadIndex idx) c
  | r == regDeviceId || r == regVersion = failWith (MReadOnly idx) c
  | r == regCtrl =
      let ctrl' = v .&. ctrlEnable
       in ( c
              { cCtrl = ctrl',
                cDirty = cDirty c || ctrl' /= cCtrl c,
                cOp = if v .&. ctrlFlush /= 0 then OpFlushWrite 0 else OpIdle
              },
            Nothing
          )
  | r == regStatus =
      let status' = if v .&. stErr /= 0 then cStatus c .&. complement stErr else cStatus c
       in (c {cStatus = status', cDirty = cDirty c || status' /= cStatus c}, Nothing)
  | otherwise = (c {cOp = OpSlotWrite (resize (r - 4)) v}, Nothing)
  where
    r = fromIntegral idx :: RegIdx -- only used once idx < 100

-- | The sequencer is idle (so BUSY is clear, as in the sketch) and a change or
-- request is waiting.  The caller also requires a quiet emitter.
snapshotDue :: Core -> Bool
snapshotDue Core {..} = cOverruns == 0 && cDirty && cOp == OpIdle && cPending == Nothing

-- | Latch CTRL/STATUS and copy the slots to the shadow RAM; the emitter starts
-- printing when the copy finishes (see OpCopy).
beginSnapshot :: Core -> Core
beginSnapshot c = c {cShadowCtrl = cCtrl c, cShadowStatus = cStatus c, cDirty = False, cOp = OpCopy 0}

-- Emitter ----------------------------------------------------------------------

-- | Input: (UART transmitter ready, from the core, shadow RAM read data).
-- Output: (byte to transmit, shadow RAM read address, quiet).  quiet depends
-- on the state only, not the input (hence the lazy pattern).
emitStep :: Emitter -> (Bool, CoreToEmitter, Word32) -> (Emitter, (Maybe Byte, SlotIdx, Bool))
emitStep e0 ~(txReady, ~(msg, startPrinting, shadowCtrl, shadowStatus), shadowOut) = (e3, (txByte, shadowAddr, emitterQuiet e0))
  where
    e1 = maybe e0 (`push` e0) msg
    e2 = if startPrinting then e1 {eEmit = ESnap (SnCrc 0 0), eSnapCrc = 0} else e1
    -- the shadow RAM's read port was pointed at this register last cycle
    word = shadowWord shadowCtrl shadowStatus shadowOut (emitReg (eEmit e2))
    (e3, txByte) = emit txReady word e2
    shadowAddr = slotOf (emitReg (eEmit e3))

-- | Queue a message; count it as dropped when the queue is full.
push :: Msg -> Emitter -> Emitter
push m e@Emitter {..}
  | eQueueCount == 8 = e {eDropped = if eDropped == maxBound then eDropped else eDropped + 1}
  | otherwise =
      e
        { eQueue = replace (satAdd SatWrap eQueueHead (resize eQueueCount)) m eQueue,
          eQueueCount = eQueueCount + 1
        }

-- | Register r of the snapshot being printed.  Slots come from the shadow RAM.
shadowWord :: Word32 -> Word32 -> Word32 -> RegIdx -> Word32
shadowWord shadowCtrl shadowStatus shadowOut r
  | r == regDeviceId = deviceId
  | r == regVersion = version
  | r == regCtrl = shadowCtrl
  | r == regStatus = shadowStatus
  | otherwise = shadowOut

slotOf :: RegIdx -> SlotIdx
slotOf r = if r >= 4 then resize (r - 4) else 0

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

-- | Drive the UART: queued messages first, then the dropped count, then
-- snapshots.  word is snapshot register 'emitReg' of the current state.
emit :: Bool -> Word32 -> Emitter -> (Emitter, Maybe Byte)
emit txReady word e@Emitter {..} = case eEmit of
  EIdle
    | eQueueCount /= 0 ->
        ( e
            { eEmit = EMsg (eQueue !! eQueueHead) 0,
              eQueueHead = satSucc SatWrap eQueueHead,
              eQueueCount = eQueueCount - 1
            },
          Nothing
        )
    | eDropped /= 0 -> (e {eEmit = EMsg (MLost eDropped) 0, eDropped = 0}, Nothing)
    | otherwise -> (e, Nothing)
  EMsg m pos ->
    let next = if pos == maxBound then EIdle else EMsg m (pos + 1)
     in case render m !! pos of -- one character of the message per cycle
          Nothing -> (e {eEmit = next}, Nothing)
          Just b
            | txReady -> (e {eEmit = next}, Just b)
            | otherwise -> (e, Nothing)
  ESnap snap -> case snap of
    SnCrc r i ->
      let byte = (bitCoerce word :: Vec 4 Byte) !! (3 - i) -- little-endian byte i
          next
            | i /= maxBound = SnCrc r (i + 1)
            | r /= maxBound = SnCrc (r + 1) 0
            | otherwise = SnLetter
       in (e {eSnapCrc = crc8Step eSnapCrc byte, eEmit = ESnap next}, Nothing)
    SnLetter -> send 83 (ESnap SnSpace0)
    SnSpace0 -> send 32 (ESnap (SnSeq 0 False))
    SnSeq i started ->
      let d = eSeqDigits !! i
          printing = started || d /= 0 || i == maxBound
          next = ESnap (if i == maxBound then SnSpace1 else SnSeq (i + 1) printing)
       in if not printing
            then (e {eEmit = next}, Nothing)
            else send (48 + fromIntegral d) next
    SnSpace1 -> send 32 (ESnap SnCrcHi)
    SnCrcHi -> send (hexDigit (resize (eSnapCrc `shiftR` 4))) (ESnap SnCrcLo)
    SnCrcLo -> send (hexDigit (resize eSnapCrc)) (ESnap SnSpace2)
    SnSpace2 -> send 32 (ESnap (SnHex 0 0))
    SnHex r k ->
      let nib = (bitCoerce word :: Vec 8 (Unsigned 4)) !! k -- most significant first
          next
            | k /= maxBound = SnHex r (k + 1)
            | r /= maxBound = SnHex (r + 1) 0
            | otherwise = SnNewline
       in send (hexDigit nib) (ESnap next)
    SnNewline
      | txReady ->
          ( e
              { eEmit = EIdle,
                eSeq = eSeq + 1,
                eSeqDigits = if eSeq == maxBound then repeat 0 else incDecimal eSeqDigits
              },
            Just newline
          )
      | otherwise -> (e, Nothing)
  where
    send b next
      | txReady = (e {eEmit = next}, Just b)
      | otherwise = (e, Nothing)

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
    (toEmitter, psramPort, slotPort, shadowWr) =
      unbundle (mealy (coreStep timeoutCycles) initialCore (bundle (received, quiet, psramOut, slotOut)))
    -- quiet comes from the emitter's registers only, so this loop has a register in it
    (txByte, shadowAddr, quiet) = unbundle (mealy emitStep initialEmitter (bundle (txReady, toEmitter, shadowOut)))
    ram port = let (addr, wr) = unbundle port in blockRam (repeat 0 :: Vec 96 Word32) addr wr
    psramOut = ram psramPort
    slotOut = ram slotPort
    shadowOut = blockRam (repeat 0 :: Vec 96 Word32) shadowAddr shadowWr

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
