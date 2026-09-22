{-# LANGUAGE OverloadedStrings #-}
module NeoMusicScoreTests (scoreGroup) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Either (isLeft)
import Data.List (sort, isInfixOf)
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified NeoMusic as A
import NeoMusic.Score
import NeoMusic.Language
import NeoMusic.Midi
import NeoMusic.Numeric
import NeoMusic.Sheet

perf :: Performance
perf = defaultPerformance {instruments=M.singleton "right" defaultInstrument}
view :: SheetView
view = SheetView 4 "C" (M.singleton "right" Treble)
header :: String
header = "tempo 120\ntuning 12 at C4\ninstrument right = piano staff treble\n"

scoreGroup :: Group
scoreGroup = Group "NeoMusic score and views"
  [ ("phrase axes and score laws", property $ do
      xs <- forAll (Gen.list (Range.linear 0 15) (Gen.int (Range.linear (-12) 12)))
      ds <- forAll (Gen.list (Range.singleton (length xs)) (Gen.int (Range.linear 1 8)))
      let phrase = concat (zipWith (\x d -> hit [x] (fromIntegral d/4) 0.7) xs ds)
          a = Line "right" phrase
          b = Silence (3/2)
          c = Line "right" (chord [0,4,7])
      retro (retro phrase) === phrase
      stretch 2 (stretch 3 phrase) === stretch 6 phrase
      map dur (transpose 7 phrase) === map dur phrase
      flatten ((a :>>: b) :>>: c) === flatten (a :>>: (b :>>: c))
      render perf (Silence 0 :>>: a) === render perf a
      render perf (a :>>: Silence 0) === render perf a
      left <- evalEither (render perf (a :||: c))
      right <- evalEither (render perf (c :||: a))
      sort (map show (A.notes left)) === sort (map show (A.notes right))
      A.seconds left === A.seconds right)
  , ("expanded source preserves timeline", property $ do
      xs <- forAll (Gen.list (Range.linear 0 12) (Gen.int (Range.linear (-12) 12)))
      let music = Silence (1/2) :>>: Line "right" (concatMap (\x -> hit [x] (3/4) 0.7) xs)
      text <- evalEither (prettyScore music)
      doc <- evalEither (parseDocument (header++text))
      expected <- evalEither (render perf music)
      actual <- evalEither (render (performance doc) (score doc))
      A.seconds actual === A.seconds expected
      sort (map show (A.notes actual)) === sort (map show (A.notes expected)))
  , ("functions, operators, comments and negative notes", withTests 1 $ property $ do
      doc <- evalEither (parseDocument (header++"x = 0 -2 (4 7) _:2\nf y = y [x + 1]\nright: rev [f [9:1/2]] -- done\n"))
      (beats,es) <- evalEither (flatten (score doc))
      beats === 11/2
      map (pitches . event) es === [[],[A.Step 5,A.Step 8],[A.Step (-1)],[A.Step 1],[A.Step 9]]
      map (dur . event) es === [2,1,1,1,1/2]
      scaled <- evalEither (parseDocument (header++"right: vel 0.5 [0 2]*2:1/2 & stretch 2 [7 - 1]\n"))
      (d,_) <- evalEither (flatten (score scaled))
      d === 2)
  , ("parallel retrograde and fractional timing", withTests 1 $ property $ do
      doc <- evalEither (parseDocument (header++"right: rev [0 & 7:2]"))
      (_,es) <- evalEither (flatten (score doc))
      [(onset e,pitches (event e)) | e <- es] === [(1,[A.Step 0]),(0,[A.Step 7])]
      let phrase = concat (replicate 10 (hit [0] (1/3) 1))
          a = Line "right" phrase
      timeline <- evalEither (render perf a)
      map A.start (A.notes timeline) === [fromRational (i/3)*0.5 | i <- [0..9]]
      assert (isLeft (midi perf {bpm=1e-320} (Silence 0))))
  , ("language rejects malformed and ambiguous inputs", withTests 1 $ property $ do
      mapM_ (assert . isLeft . parseDocument . (header++))
        ["right: 0:1/0", "right: 0:-1", "right: unknown", "a = b\nb = a\nright: a"
        ,"f x = x\nright: f", "right: vel 2 0", "right: [0", "instrument right = sine"
        ,"a = 1\na = 2\nright: a", "right: [0]*-1", "right: 9223372036854775808"]
      assert (isLeft (parseDocument "instrument x = sine pan 2\nx: 0"))
      assert (isLeft (withRhythm [] (n 0))))
  , ("sheet and audio agree exactly on onset and gate", withTests 1 $ property $ do
      let music = Silence (3/2) :>>: (Line "right" (hit [0,4,7] 3 0.8) :||: Line "right" (n 12 <> n 11))
      timeline <- evalEither (render perf music)
      events <- evalEither (sheetEvents perf view music)
      sort [(A.start v,A.duration v) | v <- A.notes timeline] ===
        sort [(fromRational t*0.5,fromRational d*0.5) | (_,t,d,ps) <- events, _ <- ps]
      sheet <- evalEither (engrave perf view music)
      assert ("~" `isInfixOf` sheet && "\\\\" `isInfixOf` sheet))
  , ("strict sheet/MIDI bounds and exact tick resolution", withTests 1 $ property $ do
      let micro = perf {instruments=M.singleton "right" defaultInstrument {tuning=A.Tuning 440 (2 ** (1/24))}}
          note = Line "right" (n 0)
      let percussion = perf {instruments=M.singleton "right" defaultInstrument {timbre=A.kick}}
      assert (isLeft (midi percussion note))
      assert (isLeft (engrave percussion view note))
      assert (isLeft (midi micro note))
      assert (isLeft (engrave micro view note))
      assert (isLeft (engrave perf view (Line "right" (hit [0] (1/3) 1))))
      assert (isLeft (engrave perf view (Line "right" (n 60))))
      unisons <- evalEither (midi perf (note :||: note))
      BL.take 2 (BL.drop 10 unisons) === BL.pack [0,3]
      assert (isLeft (midi perf (Line "right" (hit [0] (1/32768) 1))))
      bytes <- evalEither (midi perf (Line "right" (hit [0] (1/3) 1 <> hit [0] (2/3) 0.5)))
      BL.take 4 bytes === "MThd"
      BL.take 6 (BL.drop 8 bytes) === BL.pack [0,1,0,2,0,3]
      assert (BS.pack [0,0x90,60,127,1,0x80,60,0,0,0x90,60,64] `BS.isInfixOf` BL.toStrict bytes))
  , ("numeric view preserves steps, durations and independent voices", withTests 1 $ property $ do
      let line = Line "voice" (n 1 <> n 2 <> n 3 <> n (-1) <> n 0 <> chord [4,5,6])
      numeric line === Right "voice: 1 2 3 -1 0 (4 5 6)\n"
      numeric (Silence 2 :>>: Line "micro" (hit [1] (1/3) 1)) === Right "micro: _:2 1:1/3\n"
      lanes <- evalEither (numericLanes (Line "a" (n 0) :||: Line "a" (hit [4] 2 1)))
      map (sum . map dur . snd) lanes === [2,2]
      length lanes === 2
      html <- evalEither (numericHtml False (Line "<unsafe>" (n 0)))
      assert ("&lt;unsafe&gt;" `isInfixOf` html && not ("<unsafe>" `isInfixOf` html)))
  , ("LilyPond golden views", withTests 1 $ property $ do
      mapM_ (\name -> do
        source <- evalIO (readFile ("examples/neomusic/"++name++".neomusic"))
        doc <- evalEither (parseDocument source)
        actual <- evalEither (engrave (performance doc) (sheetView doc) (score doc))
        expected <- evalIO (readFile ("test/golden/neomusic/"++name++".ly"))
        actual === expected) ["scale","twinkle","voices","ties"])
  ]
