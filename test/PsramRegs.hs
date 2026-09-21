{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Tests for the FPGA psram_regs.  The byte-level core is stepped cycle by
-- cycle against a model of its block RAM, and its text output is compared
-- with a reference model ported directly from psram_regs.ino.
module PsramRegs where

import Clash.Prelude hiding (lines)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numeric (showHex)
import "haskplayground" PsramRegs
import qualified Prelude as P

-- Frames -----------------------------------------------------------------------

crc8 :: [Byte] -> Byte
crc8 = P.foldl crc8Step 0

frame :: Byte -> Byte -> Word32 -> [Byte]
frame cmd idx v = 0xA5 : body P.++ [crc8 body]
  where
    body = cmd : idx : [P.fromIntegral (v `shiftR` (8 * k)) | k <- [0 .. 3]]

writeFrame :: Byte -> Word32 -> [Byte]
writeFrame = frame 0x57

readFrame :: [Byte]
readFrame = frame 0x52 0 0

-- A host action and what it puts on the wire.
data HostOp
  = AWrite Byte Word32
  | ARead
  | ABadCrc Byte Word32
  | ABadCmd Byte
  | AJunk [Byte] -- bytes without 0xA5, which the parser must skip
  deriving (Show)

wire :: HostOp -> [Byte]
wire (AWrite i v) = writeFrame i v
wire ARead = readFrame
wire (ABadCrc i v) = let f = writeFrame i v in P.init f P.++ [P.last f `xor` 0x5A]
wire (ABadCmd c) = frame c 0 0
wire (AJunk bs) = bs

-- Reference model: psram_regs.ino's applyWrite/handleFrame ---------------------

type Regs = Map.Map Int Word32

modelInit :: Regs
modelInit = Map.fromList ([(i, 0) | i <- [0 .. 99]] P.++ [(0, 0x5053_5231), (1, 0x0001_0000)])

setErrM :: Regs -> Regs
setErrM = Map.adjust (.|. 2) 3

-- | Apply one action: new registers and the error lines it prints.
modelStep :: Regs -> HostOp -> (Regs, [P.String])
modelStep regs act = case act of
  AWrite i v -> writeM (P.fromIntegral i) v
  ARead -> (regs, [])
  ABadCrc _ _ -> (setErrM regs, ["E bad crc"])
  ABadCmd c -> (setErrM regs, ["E bad cmd " P.++ hex2 c])
  AJunk _ -> (regs, [])
  where
    writeM i v
      | i >= 100 = (setErrM regs, ["E bad index " P.++ P.show i])
      | i <= 1 = (setErrM regs, ["E reg " P.++ P.show i P.++ " is read-only"])
      | i == 2 = (Map.insert 2 (v .&. 1) regs, []) -- flush writes PSRAM only
      | i == 3 = (if v .&. 2 /= 0 then Map.adjust (.&. complement 2) 3 regs else regs, [])
      | otherwise = (Map.insert i v regs, [])

hex2 :: Byte -> P.String
hex2 b = P.map Char.toUpper (pad 2 (showHex (P.toInteger b) ""))

hex8 :: Word32 -> P.String
hex8 w = P.map Char.toUpper (pad 8 (showHex (P.toInteger w) ""))

pad :: Int -> P.String -> P.String
pad n s = P.replicate (n P.- P.length s) '0' P.++ s

snapshotLine :: Int -> Regs -> P.String
snapshotLine sq regs = "S " P.++ P.show sq P.++ " " P.++ hex2 (crc8 raw) P.++ " " P.++ P.concatMap hex8 ws
  where
    ws = Map.elems regs
    raw = P.concatMap (\w -> [P.fromIntegral (w `shiftR` (8 * k)) | k <- [0 .. 3]]) ws

-- | Parse "S seq crc hex": seq, registers, and whether the CRC matches.
parseSnapshot :: P.String -> Maybe (Int, [Word32], Bool)
parseSnapshot l = case P.words l of
  ["S", sq, c, hx]
    | P.length hx == 800 ->
        let ws = [P.fromInteger (P.read ("0x" P.++ P.take 8 (P.drop (8 * k) hx))) | k <- [0 .. 99]]
            m = Map.fromList (P.zip [0 ..] ws)
         in Just (P.read sq, ws, l == snapshotLine (P.read sq) m && P.length c == 2)
  _ -> Nothing

-- Cycle simulation -------------------------------------------------------------

data Sim = Sim
  { simState :: State,
    simRam :: Map.Map Int Word32, -- the PSRAM stand-in
    simSlots :: Map.Map Int Word32,
    simShadow :: Map.Map Int Word32,
    simReads :: (Word32, Word32, Word32), -- last cycle's read data: PSRAM, slots, shadow
    simOut :: [Byte], -- reversed
    simFault :: Int -> Word32 -> Word32 -- what the PSRAM stores for a write (fault injection)
  }

newSim :: Sim
newSim = Sim initialState Map.empty Map.empty Map.empty (0, 0, 0) [] (\_ v -> v)

-- | Like blockRam: read data appears the cycle after the address, and a write
-- lands at the end of the cycle (a same-cycle read sees the old value).
ramCycle :: (Int -> Word32 -> Word32) -> Map.Map Int Word32 -> RamPort -> (Word32, Map.Map Int Word32)
ramCycle fault ram (addr, wr) =
  ( Map.findWithDefault 0 (P.fromIntegral addr) ram,
    case wr of
      Nothing -> ram
      Just (a, v) -> let a' = P.fromIntegral a in Map.insert a' (fault a' v) ram
  )

-- | One clock cycle of the core and its three block RAMs.
step :: Unsigned 32 -> Sim -> (Maybe Byte, Bool) -> Sim
step timeout Sim {..} (rx, txReady) =
  let (psramOut, slotOut, shadowOut) = simReads
      (st', (tx, psramPort, slotPort, shadowPort)) = psramStep timeout simState (rx, txReady, psramOut, slotOut, shadowOut)
      (p', ram') = ramCycle simFault simRam psramPort
      (sl', slots') = ramCycle (\_ v -> v) simSlots slotPort
      (sh', shadow') = ramCycle (\_ v -> v) simShadow shadowPort
      out' = P.maybe simOut (: simOut) tx
   in deepseqX st' (Sim st' ram' slots' shadow' (p', sl', sh') out' simFault)

runCycles :: Unsigned 32 -> Sim -> [(Maybe Byte, Bool)] -> Sim
runCycles timeout = List.foldl' (step timeout)

-- | Each byte followed by gap idle cycles.
spaced :: Int -> [Byte] -> [Maybe Byte]
spaced gap = P.concatMap (\b -> Just b : P.replicate gap Nothing)

outputLines :: Sim -> [P.String]
outputLines sim = P.lines (P.map (Char.chr . P.fromIntegral) (P.reverse (simOut sim)))

noTimeout :: Unsigned 32
noTimeout = maxBound

-- Enough idle cycles to finish a snapshot (about 1250 cycles when never stalled).
drain :: Int -> [(Maybe Byte, Bool)]
drain n = P.replicate n (Nothing, True)

bootText :: P.String
bootText = "I psram_regs 1.0 (fpga bram)"

-- Properties -------------------------------------------------------------------

prop_boot :: Property
prop_boot = withTests 1 . property $ do
  let sim = runCycles noTimeout newSim (drain 3000)
  outputLines sim === [bootText, snapshotLine 0 modelInit]

genAction :: Gen HostOp
genAction =
  Gen.frequency
    [ (6, AWrite <$> Gen.integral (Range.linear 2 99) <*> genWord),
      (1, AWrite 2 <$> Gen.element [0, 1, 2, 3]), -- CTRL: enable / flush
      (1, AWrite 3 <$> Gen.element [0, 2]), -- STATUS: clear ERR
      (1, AWrite <$> Gen.element [0, 1] <*> genWord), -- read-only
      (1, AWrite <$> Gen.integral (Range.linear 100 255) <*> genWord),
      (1, pure ARead),
      (1, ABadCrc <$> Gen.integral (Range.linear 0 99) <*> genWord),
      (1, ABadCmd <$> Gen.filter (\c -> c /= 0x57 && c /= 0x52) (Gen.integral Range.linearBounded)),
      (1, AJunk <$> Gen.list (Range.linear 1 5) (Gen.filter (/= 0xA5) (Gen.integral Range.linearBounded)))
    ]
  where
    genWord = Gen.integral Range.linearBounded

-- Random host traffic with random transmitter stalls: the error lines match
-- the model in order, every snapshot is well formed with consecutive sequence
-- numbers, and the last snapshot is the model's final register file.
prop_matchesModel :: Property
prop_matchesModel = withTests 40 . property $ do
  acts <- forAll (Gen.list (Range.linear 1 40) genAction)
  gap <- forAll (Gen.int (Range.linear 60 150))
  stalls <- forAll (Gen.list (Range.singleton 64) (Gen.frequency [(4, pure True), (1, pure False)]))
  let rx = spaced gap (P.concatMap wire acts)
      cyclesIn = P.zip rx (P.cycle stalls)
      sim = runCycles noTimeout newSim (cyclesIn P.++ drain 6000)
      out = outputLines sim
      (final, errs) = List.foldl' (\(r, es) a -> let (r', e) = modelStep r a in (r', es P.++ e)) (modelInit, []) acts
      snaps = [s | Just s <- P.map parseSnapshot out]
  annotate (P.unlines out)
  P.take 1 out === [bootText]
  [l | l <- out, "E " `List.isPrefixOf` l] === errs
  -- every line is the boot line, an error line, or a valid snapshot
  P.length out === 1 + P.length errs + P.length snaps
  [ok | (_, _, ok) <- snaps] === P.map (P.const True) snaps
  [sq | (sq, _, _) <- snaps] === [0 .. P.length snaps P.- 1]
  (let (_, ws, _) = P.last snaps in ws) === Map.elems final

-- With CTRL.enable set, slot writes reach the RAM; a flush writes every slot.
prop_writeThrough :: Property
prop_writeThrough = withTests 1 . property $ do
  let bytes = writeFrame 2 1 P.++ writeFrame 4 0xDEAD_BEEF P.++ writeFrame 99 0xCAFE_F00D
      sim = runCycles noTimeout newSim (P.map (\b -> (b, True)) (spaced 80 bytes) P.++ drain 3000)
  Map.lookup 0 (simRam sim) === Just 0xDEAD_BEEF
  Map.lookup 95 (simRam sim) === Just 0xCAFE_F00D
  -- written while disabled, then flushed
  let bytes2 = writeFrame 10 0x1234 P.++ writeFrame 2 2
      sim2 = runCycles noTimeout newSim (P.map (\b -> (b, True)) (spaced 80 bytes2) P.++ drain 3000)
  Map.lookup 6 (simRam sim2) === Just 0x1234
  Map.size (simRam sim2) === 96

-- A RAM with a stuck bit fails verification: an error line and STATUS.ERR,
-- which the host clears by writing 1 to it.
prop_verifyFailure :: Property
prop_verifyFailure = withTests 1 . property $ do
  let faulty = newSim {simFault = \a v -> if a == 5 then v .|. 0x100 else v}
      bytes = writeFrame 2 1 P.++ writeFrame 9 0x0000_0001 P.++ writeFrame 3 2
      sim = runCycles noTimeout faulty (P.map (\b -> (b, True)) (spaced 1500 bytes) P.++ drain 3000)
      out = outputLines sim
      snaps = [ws | Just (_, ws, _) <- P.map parseSnapshot out]
  annotate (P.unlines out)
  assert ("E psram verify failed slot 5" `P.elem` out)
  assert (P.any (\ws -> ws P.!! 3 == 2) snaps) -- ERR seen
  (P.last snaps P.!! 3) === 0 -- and cleared

-- A partial frame older than the timeout is dropped when the next byte arrives.
prop_frameTimeout :: Property
prop_frameTimeout = withTests 1 . property $ do
  let timeout = 500
      partial = spaced 10 (P.take 3 (writeFrame 7 0x1111))
      full = spaced 10 (writeFrame 8 0x2222)
      sim = runCycles timeout newSim (P.map (\b -> (b, True)) (partial P.++ P.replicate 600 Nothing P.++ full) P.++ drain 3000)
      snaps = [ws | Just (_, ws, _) <- P.map parseSnapshot (outputLines sim)]
  (P.last snaps P.!! 8, P.last snaps P.!! 7) === (0x2222, 0)

-- Errors arriving while the transmitter is stuck overflow the queue (8 queued
-- plus the one being sent); the dropped ones are reported, never silently lost.
prop_lostMessages :: Property
prop_lostMessages = withTests 1 . property $ do
  let n = 20
      bytes = P.concat (P.replicate n (wire (ABadCrc 4 1)))
      stuck = P.map (\b -> (b, False)) (spaced 80 bytes)
      sim = runCycles noTimeout (runCycles noTimeout newSim (drain 3000)) (stuck P.++ drain 8000)
      out = outputLines sim
      shown = P.length (P.filter (== "E bad crc") out)
      lost = [P.read (P.takeWhile Char.isDigit (P.drop 7 l)) :: Int | l <- out, "E lost " `List.isPrefixOf` l]
  annotate (P.unlines out)
  lost === [n P.- 9]
  shown + P.sum lost === n

-- End to end through the UART at 8 clocks per bit: bits in, bits out.
uartBits :: Int -> Byte -> [Bit]
uartBits cpb b = P.concatMap (P.replicate cpb) (low : [boolToBit (testBit b k) | k <- [0 .. 7]] P.++ [high])

decodeUart :: Int -> [Bit] -> [Byte]
decodeUart cpb = go
  where
    go bs = case P.dropWhile (== high) bs of
      [] -> []
      bits ->
        let at k = bits P.!! (cpb `P.div` 2 + cpb * k)
            byte = P.foldr (\k acc -> if at (k + 1) == high then setBit acc k else acc) 0 [0 .. 7]
            rest = P.drop (cpb * 10 P.- cpb `P.div` 2) bits
         in if P.length bits < cpb * 10 then [] else byte : go rest

prop_uartEndToEnd :: Property
prop_uartEndToEnd = withTests 1 . property $ do
  let cpb = 8
      cfg = PsramConfig {clocksPerBit = P.fromIntegral cpb, timeoutCycles = 100_000}
      idle = P.replicate 90_000 high
      rxBits = idle P.++ P.concatMap (uartBits cpb) (writeFrame 5 0xA5A5_0001) P.++ P.repeat high
      txBits = sampleN @System 190_000 (psramRegs cfg (fromList rxBits))
      out = P.lines (P.map (Char.chr . P.fromIntegral) (decodeUart cpb txBits))
      expected = Map.insert 5 0xA5A5_0001 modelInit
  annotate (P.unlines out)
  P.take 3 out === [bootText, snapshotLine 0 modelInit, snapshotLine 1 expected]

psramRegsGroup :: Group
psramRegsGroup =
  Group
    "PsramRegs"
    [ ("boot", prop_boot),
      ("matches the psram_regs.ino model", prop_matchesModel),
      ("write-through and flush", prop_writeThrough),
      ("verify failure sets ERR", prop_verifyFailure),
      ("stale partial frame dropped", prop_frameTimeout),
      ("lost messages reported", prop_lostMessages),
      ("UART end to end", prop_uartEndToEnd)
    ]
