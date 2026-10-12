-- | Strict 12-TET SMF type 1 view. No microtonal pitch rounding.
module NeoMusic.Midi (midi, midiPitch, tempoChanges) where

import Control.Monad (unless)
import Data.Bits ((.|.))
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as M
import Data.Ratio (denominator, numerator)
import qualified NeoMusic.Audio as A
import qualified NeoMusic.Pitch as A
import NeoMusic.Score hiding (n)

midiPitch :: A.Tuning -> A.Step -> Either String Int
midiPitch t (A.Step x) = do
  unless (A.f0 t > 0 && not (isInfinite (A.f0 t)) && abs (A.ratio t - 2 ** (1/12)) < 1e-10)
    (Left "view requires 12-EDO tuning")
  let base = 69 + 12 * logBase 2 (A.f0 t / 440)
  unless (not (isNaN base || isInfinite base) && abs (base - fromInteger (round base)) < 1e-6)
    (Left "view requires a reference frequency on the 12-TET grid")
  let p = round base + toInteger x
  unless (p >= 0 && p <= 127) (Left "pitch outside MIDI 0..127")
  pure (fromInteger p)

midi :: Performance -> Score -> Either String BL.ByteString
midi perf score = do
  validate perf score
  (total,es) <- flatten score
  let names = nub [instrument e | e <- es]
      -- A note sounds until onset + dur * gate, which may pass the score's end.
      release e = onset e + dur (event e) * gate (event e)
      end = maximum (total : map release es)
  changes <- tempoChanges perf score
  lanes <- concat <$> mapM (instrumentLanes es) names
  unless (length lanes <= 15) (Left "MIDI needs more than 15 pitched channels for instruments, overlapping unisons and curved notes")
  -- Curves are sampled every 1/64 beat, so they need that resolution.
  let anyCurved = any (any curved) lanes
      division = foldl lcm (if anyCurved then 64 else 1)
        (map denominator (total : map fst changes ++ concat [[onset e,release e] | e <- es]))
      ticks x = numerator (x * fromInteger division)
  unless (division <= 32767) (Left "exact rhythm needs MIDI resolution above 32767 ticks/quarter")
  tracks <- mapM (makeTrack ticks end) (zip lanes ([0..8]++[10..15]))
  conductor <- track (ticks end) [(ticks at, [0xff,0x51,3] ++ bytes3 micros) | (at, micros) <- changes]
  pure $ toLazyByteString (string8 "MThd" <> word32BE 6 <> word16BE 1
    <> word16BE (fromIntegral (length tracks+1)) <> word16BE (fromInteger division)
    <> conductor <> mconcat tracks)
  where
    instrumentLanes es name = do
      i <- maybe (Left "missing instrument") Right (M.lookup name (instruments perf))
      unless (A.pitchDrop (timbre i) == 0 && all ((/= A.NoiseWave) . A.wave) (A.oscillators (timbre i)))
        (Left "noise/pitch-drop percussion requires an explicit MIDI percussion mapping")
      notes <- concat <$> mapM (convert i) [e | e <- es, instrument e == name]
      -- A separate channel prevents one unison's note-off cutting another
      -- short, and pitch bend and expression act on a whole channel, so a
      -- curved note shares its channel with nothing that overlaps it.
      pure (foldl place [] (sortOn noteOn notes))
    place [] n0 = [[n0]]
    place (lane:lanes) n0
      | all (\n1 -> (noteKey n0 /= noteKey n1 && not (curved n0 || curved n1)) || noteOff n0 <= noteOn n1 || noteOff n1 <= noteOn n0) lane = (n0:lane):lanes
      | otherwise = lane:place lanes n0
    clock = timeMap score
    clockSeconds b = performed clock b * 60 / bpm perf
    makeTrack ticks total (notes,channel) = do
      let bendRange = maximum (0 : [ceiling (maximum (0 : map abs (bendSamples n))) | n <- notes, curved n]) :: Integer
      unless (bendRange <= 127) (Left "a bend exceeds the MIDI pitch-bend range of 127 semitones")
      let range = max 1 bendRange
          -- At one tick: note-offs, then resets, then curve samples, then note-ons.
          events = sortOn (\(t,priority,_) -> (t,priority)) $ concat
            [ [(ticks (noteOn n),3::Int,[0x90+channel,noteKey n,noteVel n]), (ticks (noteOff n),0,[0x80+channel,noteKey n,0])]
              ++ (if curved n then curveEvents ticks channel range n else [])
            | n <- notes ]
          -- RPN 0 sets the bend range in semitones, then the null RPN.
          rpn = [(0,[0xb0+channel,101,0]),(0,[0xb0+channel,100,0]),(0,[0xb0+channel,6,fromInteger range]),(0,[0xb0+channel,38,0]),(0,[0xb0+channel,101,127]),(0,[0xb0+channel,100,127])]
      track (ticks total) ((0,[0xc0+channel,0]) : (if any curved notes then rpn else []) ++ [(t,bytes) | (t,_,bytes) <- events])
    -- Sample times: every 1/64 beat from the onset, up to the note-off.
    sampleBeats n = takeWhile (< noteOff n) [noteOn n, noteOn n + 1/64 ..]
    -- Position within the sounding time and seconds since onset, through the clock.
    position n b =
      let p0 = performed clock (noteOn n)
          span' = performed clock (noteOff n) - p0
      in (if span' > 0 then (performed clock b - p0) / span' else 0, clockSeconds b - clockSeconds (noteOn n))
    bendSamples n =
      [ fromRational (sum [curveValue c (toRational u) | c <- bends (noteEvent n)])
          + fromRational (vibrato (noteEvent n)) * sin (2 * pi * vibratoRate (noteInstrument n) * secs)
          + fromRational (cents (noteEvent n)) / 100
      | b <- sampleBeats n, let (u, secs) = position n b ] :: [Double]
    swellSamples n = [product [fromRational (curveValue c (toRational u)) | c <- swells (noteEvent n)] | b <- sampleBeats n, let (u, _) = position n b] :: [Double]
    curveEvents ticks channel range n =
      let bendValue x = max 0 (min 16383 (8192 + round (8191 * x / fromInteger range))) :: Int
          bendsAt = [ (ticks b, 2, [0xe0+channel, v `mod` 128, v `div` 128]) | (b, x) <- zip (sampleBeats n) (bendSamples n), let v = bendValue x ]
          -- Expression above 1 is clipped to full; below, it scales the channel.
          exprAt = [ (ticks b, 2, [0xb0+channel, 11, max 0 (min 127 (round (127 * x)))]) | (b, x) <- zip (sampleBeats n) (swellSamples n) ]
          dedupe xs = [x | (x, prev) <- zip xs (Nothing : map Just xs), fmap thd prev /= Just (thd x)]
          thd (_, _, x) = x
          resets = [(ticks (noteOff n), 1, [0xe0+channel, 0, 64]), (ticks (noteOff n), 1, [0xb0+channel, 11, 127])]
      in dedupe bendsAt ++ (if null (swells (noteEvent n)) then [] else dedupe exprAt) ++ resets
    convert i e = do
      ps <- mapM (midiPitch (tuning i)) (pitches (event e))
      pure [ MidiNote (onset e) (onset e+dur (event e)*gate (event e)) p (max 1 (round (127 * vel (event e)))) (event e) i
           | p <- ps, vel (event e) > 0]

-- | One sounding note on its way into a track.
data MidiNote = MidiNote
  { noteOn :: Rational, noteOff :: Rational, noteKey :: Int, noteVel :: Int
  , noteEvent :: Event, noteInstrument :: Instrument }

curved :: MidiNote -> Bool
-- A cents offset is a constant bend, so it needs a channel of its own too.
curved n = let e = noteEvent n in not (null (bends e) && null (swells e) && vibrato e == 0 && cents e == 0)

-- | The conductor track as (written beat, microseconds per quarter) changes.
--
-- Ticks stay in written beats, so every onset and gate is exact; the shared
-- clock becomes tempo changes. Constant stretches are one change each. A ramp
-- is stepped at every note-on and note-off inside it and at least every 1/4
-- beat, and each step's tempo makes the step last exactly as long as the
-- clock says. Integer microseconds are the only rounding, and its error is
-- carried into the next step, so note times stay within about a microsecond.
tempoChanges :: Performance -> Score -> Either String [(Rational, Integer)]
tempoChanges perf score = do
  validate perf score
  (total, es) <- flatten score
  let clock = timeMap score
      notes = concat [[onset e, onset e + dur (event e) * gate (event e)] | e <- es]
      end = maximum (total : notes)
      inside u v x = u < x && x < v
      ramps = [(u, v) | (u, v, k) <- timePieces clock, k /= 0]
      breaks = M.keys (M.fromList [(x, ()) | x <- 0 : end : concat
        [ [u, v] ++ filter (inside u v) notes ++ takeWhile (< v) [u + 1/4, u + 1/2 ..] | (u, v) <- ramps ]
        ++ concat [[u, v] | (u, v, _) <- timePieces clock], x <= end])
      micros x = performed clock x * 60000000 / bpm perf
      steps = zip breaks (drop 1 breaks)
      go _ [] = Right []
      go emitted ((x0, x1) : more) = do
        let width = x1 - x0
            target = toRational (micros x1)
            tempo = round ((target - emitted) / width) :: Integer
        unless (tempo >= 1 && tempo <= 16777215) (Left "tempo outside MIDI representable range")
        ((x0, tempo) :) <$> go (emitted + fromInteger tempo * width) more
  stepped <- if null steps then pure [] else go 0 steps
  base <- let b = round (60000000 / bpm perf) :: Integer
          in if b >= 1 && b <= 16777215 && not (isInfinite (60000000 / bpm perf))
               then Right b else Left "tempo outside MIDI representable range"
  -- Drop changes that repeat the tempo already in force.
  let kept = case stepped of
        [] -> [(0, base)]
        first : later -> first : [c | (c, (_, prev)) <- zip later stepped, snd c /= prev]
  pure kept

bytes3 :: Integer -> [Int]
bytes3 x = [fromInteger (x `div` 65536),fromInteger (x `div` 256 `mod` 256),fromInteger (x `mod` 256)]

track :: Integer -> [(Integer,[Int])] -> Either String Builder
track end events = do
  (_,chunks) <- foldl step (Right (0,[])) (events ++ [(end,[0xff,0x2f,0])])
  let content = toLazyByteString (mconcat (reverse chunks))
  unless (BL.length content <= 4294967295) (Left "MIDI track exceeds size limit")
  pure (string8 "MTrk" <> word32BE (fromIntegral (BL.length content)) <> lazyByteString content)
  where
    step acc (at,bytes) = do
      (previous,chunks) <- acc
      delta <- vlq (at-previous)
      pure (at,(delta <> foldMap (word8 . fromIntegral) bytes):chunks)
vlq :: Integer -> Either String Builder
vlq value
  | value < 0 || value > 268435455 = Left "MIDI delta time exceeds 28-bit range"
  | otherwise = Right (foldMap (word8 . fromIntegral) (encode value))
  where
    encode :: Integer -> [Int]
    encode x = case reverse (digits x) of
      [] -> [0]
      xs -> zipWith (\i b -> if i < length xs-1 then b .|. 128 else b) [0..] xs
    digits x = fromInteger (x `mod` 128) : if x < 128 then [] else digits (x `div` 128)
