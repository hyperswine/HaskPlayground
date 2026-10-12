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
import NeoMusic.Score

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
  let division = foldl lcm 1 (map denominator (total : map fst changes ++ concat [[onset e,release e] | e <- es]))
      ticks x = numerator (x * fromInteger division)
  unless (division <= 32767) (Left "exact rhythm needs MIDI resolution above 32767 ticks/quarter")
  lanes <- concat <$> mapM (instrumentLanes es) names
  unless (length lanes <= 15) (Left "MIDI needs more than 15 pitched channels for instruments and overlapping unisons")
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
      -- A separate channel prevents one unison's note-off cutting another short.
      pure (foldl place [] (sortOn (\(a,_,_,_) -> a) notes))
    place [] note = [[note]]
    place (lane:lanes) note@(a,b,p,_)
      | all (\(c,d,q,_) -> p /= q || b <= c || d <= a) lane = (note:lane):lanes
      | otherwise = lane:place lanes note
    makeTrack ticks total (notes,channel) = do
      let events = sortOn (\(t,priority,_) -> (t,priority))
            ([(ticks a,1::Int,[0x90+channel,p,v]) | (a,_,p,v) <- notes] ++
             [(ticks b,0,[0x80+channel,p,0]) | (_,b,p,_) <- notes])
      track (ticks total) ((0,[0xc0+channel,0]) : [(t,bytes) | (t,_,bytes) <- events])
    convert i e = do
      ps <- mapM (midiPitch (tuning i)) (pitches (event e))
      pure [(onset e,onset e+dur (event e)*gate (event e),p,max 1 (round (127 * vel (event e))))
           | p <- ps, vel (event e) > 0]

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
