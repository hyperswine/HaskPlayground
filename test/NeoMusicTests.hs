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
  ( Mix(..), Mod(..), Patch(..), Note(..), Timeline(..), Wave(..)
  , adsr, cutoffHz, defaultMix, hall, hat, kick, modAt, pcmWav, percussive, pluck
  , renderTimelineMix, sinePatch, validPatch )
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
      let ringing = perfWith t pluck {amp = Velocity :* adsr 0.01 0 1 0.5} 60
      (sl, sr) <- evalEither (stereo (defaultMix 8000) ringing blip)
      VU.length sl === 12000
      assert (VU.any ((> 1e-3) . abs) (VU.drop 8000 sl) && VU.all (not . isNaN) sr)
      noise <- evalEither (mono 8000 (perfWith t hat 60) blip)
      assert (VU.any (/= 0) noise && VU.all (\x -> abs x <= 0.8) noise))
  , ("adsr is exactly the classic envelope", property $ do
      a <- forAll (Gen.element [0, 0.0005, 0.002, 0.01, 0.3])
      d <- forAll (Gen.element [0, 0.05, 0.4, 1.5])
      sus <- forAll (Gen.element [0, 0.15, 0.8, 1])
      r <- forAll (Gen.element [0, 0.001, 0.05, 1.2])
      gate <- forAll (Gen.element [0.0001, 0.003, 0.2, 1, 2.5])
      ts <- forAll (Gen.list (Range.linear 1 40) (Gen.double (Range.linearFrac 0 5)))
      -- The envelope the engine used before patches became signals, verbatim.
      let classic t
            | t < 0 = 0
            | t < gate = held t
            | t < gate + r' = let u = (t - gate) / r' in held gate * (1 - u) * (1 - u)
            | otherwise = 0
            where
              a' = max 0.001 a
              r' = max 0.002 r
              held x
                | x < a' = x / a'
                | d <= 0 = sus
                | otherwise = sus + (1 - sus) * exp (-4.6 * (x - a') / d)
      -- Bit for bit, not approximately.
      [modAt (adsr a d sus r) gate 1 0 t | t <- ts] === map classic ts)
  , ("patch modulations: release, LFO, velocity, bounds, pitched or not", withTests 1 $ property $ do
      -- Release starts from wherever the sound has got to at note-off.
      let swellUp = Release 0.5 (Linear 0 1 2)
      modAt swellUp 1 1 0 1 === 0.5
      modAt swellUp 1 1 0 1.25 === 0.5 * 0.5 * 0.5
      modAt swellUp 1 1 0 1.6 === 0
      -- Then restarts the second part's clock; Approach is 99% of the way after its time.
      modAt (Then 1 (K 3) (Linear 0 10 2)) 9 1 0 1.5 === 2.5
      assert (abs (modAt (Approach 1 0 2) 9 1 0 2 - 0.01) < 0.0001)
      -- A fixed-Hz LFO, a velocity input, and the key.
      assert (abs (modAt (Lfo SineWave 5) 9 1 0 0.05 - 1) < 1e-12)
      modAt (Velocity :* K 2) 9 0.25 0 0 === 0.5
      modAt (K 0.5 :* Key) 9 1 2 0 === 1
      -- Velocity is the patch's to apply: without it, soft and hard notes match.
      let flat = (perfWith (Tuning 440 2) sinePatch {amp = adsr 0.005 0 1 0.015} 60)
      xs <- evalEither (mono 8000 flat (Line "v" (hit [0] 1 1 <> hit [0] 1 0.5)))
      let peakIn a' b' = VU.maximum (VU.map abs (VU.slice a' (b' - a') xs))
      assert (abs (peakIn 8400 16000 / peakIn 0 8000 - 1) < 1e-3)
      -- An amplitude that could go negative is rejected.
      assert (isLeft (validPatch sinePatch {amp = Lfo SineWave 3}))
      assert (isLeft (validPatch sinePatch {amp = Linear 0 1 0}))
      -- A vibrato patch is pitched; a pitch that falls into the note isn't.
      let vibrato = sinePatch {pitch = K 0.2 :* Lfo SineWave 5.5}
      assert (not (percussive vibrato) && percussive kick && percussive hat && not (percussive sinePatch))
      plain <- evalEither (mono 8000 (perfWith (Tuning 440 2) sinePatch 60) (Line "v" (n 0)))
      wobbly <- evalEither (mono 8000 (perfWith (Tuning 440 2) vibrato 60) (Line "v" (n 0)))
      assert (plain /= wobbly)
      -- A modulated cutoff is never bypassed, even when its base is open.
      let opened = sinePatch {oscillators = oscillators pluck, cutoff = cutoffHz 20000}
          closing = opened {cutoff = cutoffHz 20000 :+ K (-6) :* Velocity}
      bright <- evalEither (mono 8000 (perfWith (Tuning 220 2) opened 60) (Line "v" (n 0)))
      dark <- evalEither (mono 8000 (perfWith (Tuning 220 2) closing 60) (Line "v" (n 0)))
      assert (bright /= dark))
  ]
