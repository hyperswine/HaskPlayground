{-# LANGUAGE OverloadedStrings #-}
module NeoMusicTests (neoMusicGroup) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Either (isLeft)
import NeoMusic
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Unboxed as VU
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)

neoMusicGroup :: Group
neoMusicGroup = Group "NeoMusic"
  [ ("composition and text laws", property $ do
      xs <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear (-100) 100)))
      j <- forAll (Gen.int (Range.linear (-30) 30))
      k <- forAll (Gen.int (Range.linear (-30) 30))
      let v = concatMap n xs <> rest <> chord [1,2,1] <> chord []
      transpose j (transpose k v) === transpose (j+k) v
      invert (invert v) === v
      retro (retro v) === v
      fromDeltas (toDeltas xs) === xs
      parseSeq (prettySeq v) === Right v)
  , ("transposition under arbitrary tuning", property $ do
      divisions <- forAll (Gen.int (Range.linear 1 1200))
      x <- forAll (Gen.int (Range.linear (-20) 20))
      k <- forAll (Gen.int (Range.linear (-20) 20))
      t <- evalEither (equalTemperament 220 divisions)
      let expected = freq t (Step x) * ratio t ** fromIntegral k
      assert (abs (freq t (Step (x+k)) / expected - 1) < 1e-10))
  , ("timelines retain silence and part interpretations", withTests 1 $ property $ do
      let a = part (Tuning 220 2) (Tempo 60) Sine (n 0 <> rest)
          b = part (Tuning 330 2) (Tempo 120) Square (n 0)
      Timeline d ns <- evalEither (render ((a :||: b) :>>: b))
      d === 2.5
      map start ns === [0,0,2]
      map hz ns === [220,330,330]
      map sound ns === [Sine,Square,Square]
      silent <- evalEither (render (part (Tuning 220 2) (Tempo 60) Sine (rest <> rest)))
      seconds silent === 2
      notes silent === [])
  , ("invalid inputs are rejected", withTests 1 $ property $ do
      let p = Part (Tuning 220 2) (Tempo 60) Sine (n 0) [1] [1]
      mapM_ (assert . isLeft . render . FromPart)
        [p {rhythm = []}, p {rhythm = [0]}, p {rhythm = [-1]},
         p {dynamics = []}, p {dynamics = [1.5]}, p {dynamics = [0/0]},
         p {timbre = Synth basicPatch {resonance = 1}},
         p {tempo = Tempo 0}, p {tuning = Tuning (0/0) 2}]
      assert (isLeft (parseSeq "1x"))
      assert (isLeft (parseSeq "(1 2"))
      assert (isLeft (samples 8000 (FromPart p {tuning = Tuning 4000 2}))))
  , ("PCM duration, pitch, silence and WAV framing", withTests 1 $ property $ do
      let p = part (Tuning 440 2) (Tempo 60) Sine (n 0 <> rest)
      xs <- evalEither (samples 8000 p)
      length xs === 16000
      assert (all (==0) (drop 8120 xs))
      assert (maximum (map abs xs) <= 0.8)
      let crossings = length [() | (a,b) <- zip (take 8000 xs) (drop 1 (take 8000 xs)), a <= 0, b > 0]
      assert (abs (crossings - 440) <= 1)
      bytes <- evalIO $ withSystemTempFile "neo-test.wav" $ \path handle -> do
        hClose handle
        writeWav path 8000 p
        content <- BL.readFile path
        BL.length content `seq` pure content
      BL.length bytes === 32044
      BL.take 4 bytes === "RIFF"
      BL.take 4 (BL.drop 8 bytes) === "WAVE"
      BL.take 4 (BL.drop 40 bytes) === BL.pack [0,125,0,0])
  , ("velocity, gain, pan and send", withTests 1 $ property $ do
      let t = Tuning 440 2
          quietSecond = FromPart (Part t (Tempo 60) Sine (n 0 <> n 0) [1, 1] [1, 0.5])
      Timeline _ ns <- evalEither (render (Gain 0.5 (Pan (-0.5) (Pan (-1) (Send 0.3 quietSecond)))))
      map velocity ns === [1, 0.5]
      map gain ns === [0.5, 0.5]
      map pan ns === [-1, -1]
      map send ns === [0.3, 0.3]
      xs <- evalEither (samples 8000 quietSecond)
      let peakIn a b = maximum (map abs (take (b - a) (drop a xs)))
      assert (abs (peakIn 8400 16000 / peakIn 0 8000 - 0.5) < 1e-3)
      (l, r) <- evalEither (renderMix (defaultMix 8000) (Pan (-1) quietSecond))
      assert (VU.all (== 0) r && VU.any (/= 0) l))
  , ("reverb tails and synth releases", withTests 1 $ property $ do
      let t = Tuning 220 2
          blip = part t (Tempo 60) Sine (n 0)
          mix = (defaultMix 8000) {mixReverb = hall, mixTail = 1}
      (dryL, _) <- evalEither (renderMix mix blip)
      (wetL, wetR) <- evalEither (renderMix mix (Send 1 blip))
      VU.length wetL === 16120
      assert (VU.all (== 0) (VU.drop 8120 dryL))
      assert (VU.any ((> 1e-3) . abs) (VU.drop 8400 wetL) && VU.any (/= 0) (VU.drop 8400 wetR))
      assert (VU.all (\x -> abs x <= 0.8) wetL)
      let synth = part t (Tempo 60) (Synth pluck {ampEnvelope = Envelope 0.01 0 1 0.5}) (n 0)
      (sl, sr) <- evalEither (renderMix (defaultMix 8000) synth)
      VU.length sl === 12000
      assert (VU.any ((> 1e-3) . abs) (VU.drop 8000 sl) && VU.all (not . isNaN) sr)
      noise <- evalEither (samples 8000 (part t (Tempo 60) (Synth hat) (n 0)))
      assert (any (/= 0) noise && all (\x -> abs x <= 0.8) noise))
  ]
