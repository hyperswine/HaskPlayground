{-# LANGUAGE OverloadedStrings #-}
module NeoMusicTests (neoMusicGroup) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Either (isLeft)
import NeoMusic
import qualified Data.ByteString.Lazy as BL
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
      let p = Part (Tuning 220 2) (Tempo 60) Sine (n 0) [1]
      mapM_ (assert . isLeft . render . FromPart)
        [p {rhythm = []}, p {rhythm = [0]}, p {rhythm = [-1]},
         p {tempo = Tempo 0}, p {tuning = Tuning (0/0) 2}]
      assert (isLeft (parseSeq "1x"))
      assert (isLeft (parseSeq "(1 2"))
      assert (isLeft (samples 8000 (FromPart p {tuning = Tuning 4000 2}))))
  , ("PCM duration, pitch, silence and WAV framing", withTests 1 $ property $ do
      let p = part (Tuning 440 2) (Tempo 60) Sine (n 0 <> rest)
      xs <- evalEither (samples 8000 p)
      length xs === 16000
      assert (all (==0) (drop 8000 xs))
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
  ]
