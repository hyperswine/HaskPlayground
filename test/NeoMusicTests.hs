{-# LANGUAGE OverloadedStrings #-}
-- | The synthesis engine and tuning laws, driven through the score API.
module NeoMusicTests (neoMusicGroup) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Unboxed as VU
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)
import NeoMusic.Audio
  ( Envelope(..), Mix(..), Patch(..), Note(..), Timeline(..)
  , defaultMix, hall, hat, pcmWav, pluck, renderTimelineMix, sinePatch )
import NeoMusic.Pitch (Step(..), Tuning(..), equalTemperament, freq)
import NeoMusic.Score
  ( Performance(..), Score(..), defaultInstrument, defaultPerformance, hit, n, render, rest )
import qualified NeoMusic.Score as S

-- | One instrument "v" with the given tuning and patch, at the given tempo.
perfWith :: Tuning -> Patch -> Double -> Performance
perfWith t p speed = defaultPerformance
  { bpm = speed, instruments = M.singleton "v" defaultInstrument {S.tuning = t, S.timbre = p} }

stereo :: Mix -> Performance -> Score -> Either String (VU.Vector Double, VU.Vector Double)
stereo m perf music = render perf music >>= renderTimelineMix m

mono :: Int -> Performance -> Score -> Either String (VU.Vector Double)
mono rate perf music = do
  (l, r) <- stereo (defaultMix rate) perf music
  pure (VU.zipWith (\a b -> (a + b) / 2) l r)

neoMusicGroup :: Group
neoMusicGroup = Group "NeoMusic audio engine"
  [ ("transposition under arbitrary tuning", property $ do
      divisions <- forAll (Gen.int (Range.linear 1 1200))
      x <- forAll (Gen.int (Range.linear (-20) 20))
      k <- forAll (Gen.int (Range.linear (-20) 20))
      t <- evalEither (equalTemperament 220 divisions)
      let expected = freq t (Step x) * ratio t ** fromIntegral k
      assert (abs (freq t (Step (x+k)) / expected - 1) < 1e-10))
  , ("invalid inputs are rejected", withTests 1 $ property $ do
      let perf = perfWith (Tuning 220 2) sinePatch 60
          note = Line "v" (n 0)
      mapM_ (assert . isLeft)
        [ render perf (Line "v" (hit [0] 0 1))
        , render perf (Line "v" (hit [0] 1 1.5))
        , render perf (Line "v" (hit [0] 1 (0/0)))
        , render perf {bpm = 0} note
        , render (perfWith (Tuning (0/0) 2) sinePatch 60) note
        , render (perfWith (Tuning 220 2) sinePatch {resonance = 1} 60) note ]
      assert (isLeft (mono 8000 (perfWith (Tuning 4000 2) sinePatch 60) note)))
  , ("PCM duration, pitch, silence and WAV framing", withTests 1 $ property $ do
      let perf = perfWith (Tuning 440 2) sinePatch 60
      xs <- evalEither (mono 8000 perf (Line "v" (n 0 <> rest)))
      VU.length xs === 16000
      -- The trailing rest is kept, and silent once the 15 ms release has passed.
      assert (VU.all (== 0) (VU.drop 8120 xs))
      assert (VU.maximum (VU.map abs xs) <= 0.8)
      let crossings = length [() | i <- [0 .. 7998], xs VU.! i <= 0, xs VU.! (i + 1) > 0]
      assert (abs (crossings - 440) <= 1)
      bytes <- evalIO $ withSystemTempFile "neo-test.wav" $ \path handle -> do
        hClose handle
        pcmWav path 8000 [xs]
        content <- BL.readFile path
        BL.length content `seq` pure content
      BL.length bytes === 32044
      BL.take 4 bytes === "RIFF"
      BL.take 4 (BL.drop 8 bytes) === "WAVE"
      BL.take 4 (BL.drop 40 bytes) === BL.pack [0,125,0,0])
  , ("velocity, gain, pan and send", withTests 1 $ property $ do
      let base = perfWith (Tuning 440 2) sinePatch 60
          mixed = base {instruments = M.map (\i -> i {S.gain = 0.5, S.pan = -1, S.send = 0.3}) (instruments base)}
          quietSecond = Line "v" (hit [0] 1 1 <> hit [0] 1 0.5)
      Timeline _ ns <- evalEither (render mixed quietSecond)
      map velocity ns === [1, 0.5]
      map gain ns === [0.5, 0.5]
      map pan ns === [-1, -1]
      map send ns === [0.3, 0.3]
      xs <- evalEither (mono 8000 base quietSecond)
      let peakIn a b = VU.maximum (VU.map abs (VU.slice a (b - a) xs))
      -- Skip the first note's 15 ms release, which overlaps the second note.
      assert (abs (peakIn 8400 16000 / peakIn 0 8000 - 0.5) < 1e-3)
      (l, r) <- evalEither (stereo (defaultMix 8000) mixed quietSecond)
      assert (VU.all (== 0) r && VU.any (/= 0) l))
  , ("reverb tails and synth releases", withTests 1 $ property $ do
      let t = Tuning 220 2
          dry = perfWith t sinePatch 60
          wet = dry {instruments = M.map (\i -> i {S.send = 1}) (instruments dry)}
          blip = Line "v" (n 0)
          m = (defaultMix 8000) {mixReverb = hall, mixTail = 1}
      (dryL, _) <- evalEither (stereo m dry blip)
      (wetL, wetR) <- evalEither (stereo m wet blip)
      -- One beat, the sine preset's 15 ms release, then the 1 s reverb tail.
      VU.length wetL === 16120
      assert (VU.all (== 0) (VU.drop 8120 dryL))
      assert (VU.any ((> 1e-3) . abs) (VU.drop 8400 wetL) && VU.any (/= 0) (VU.drop 8400 wetR))
      assert (VU.all (\x -> abs x <= 0.8) wetL)
      let ringing = perfWith t pluck {ampEnvelope = Envelope 0.01 0 1 0.5} 60
      (sl, sr) <- evalEither (stereo (defaultMix 8000) ringing blip)
      VU.length sl === 12000
      assert (VU.any ((> 1e-3) . abs) (VU.drop 8000 sl) && VU.all (not . isNaN) sr)
      noise <- evalEither (mono 8000 (perfWith t hat 60) blip)
      assert (VU.any (/= 0) noise && VU.all (\x -> abs x <= 0.8) noise))
  ]
