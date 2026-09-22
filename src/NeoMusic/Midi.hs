-- | Strict 12-TET SMF type 1 view. No microtonal pitch rounding.
module NeoMusic.Midi (midi, midiPitch) where

import Control.Monad (unless)
import Data.Bits ((.|.))
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as M
import Data.Ratio (denominator, numerator)
import qualified NeoMusic as A
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
  _ <- toPiece perf score
  (total,es) <- flatten score
  let names = nub [instrument e | e <- es]
      division = foldl lcm 1 (map denominator (total : concat [[onset e,dur (event e)] | e <- es]))
      ticks x = numerator (x * fromInteger division)
      tempoMicros = 60000000 / bpm perf
  unless (not (isNaN tempoMicros || isInfinite tempoMicros) && tempoMicros >= 0.5 && tempoMicros < 16777215.5)
    (Left "tempo outside MIDI representable range")
  let micros = round tempoMicros :: Integer
  unless (division <= 32767) (Left "exact rhythm needs MIDI resolution above 32767 ticks/quarter")
  unless (micros >= 1 && micros <= 16777215) (Left "tempo outside MIDI representable range")
  lanes <- concat <$> mapM (instrumentLanes es) names
  unless (length lanes <= 15) (Left "MIDI needs more than 15 pitched channels for instruments and overlapping unisons")
  tracks <- mapM (makeTrack ticks total) (zip lanes ([0..8]++[10..15]))
  conductor <- track (ticks total) [(0, [0xff,0x51,3] ++ bytes3 micros)]
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
      pure [(onset e,onset e+dur (event e),p,max 1 (round (127 * vel (event e))))
           | p <- ps, vel (event e) > 0]

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
