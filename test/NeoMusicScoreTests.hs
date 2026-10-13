{-# LANGUAGE OverloadedStrings #-}
module NeoMusicScoreTests (scoreGroup) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Either (isLeft)
import Data.List (sort, sortOn, isInfixOf, isPrefixOf, tails)
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as VU
import qualified NeoMusic.Audio as A
import qualified NeoMusic.Pitch as A
import NeoMusic.Score
import NeoMusic.Language
import NeoMusic.Midi
import NeoMusic.Numeric
import NeoMusic.Sheet

perf :: Performance
perf = defaultPerformance {instruments=M.singleton "right" defaultInstrument}
-- | The old per-note features, as the signals they now are.
bendBy, swellBy :: [(Rational, Rational)] -> Phrase -> Phrase
bendBy = addPitch . Points
swellBy = scaleLevel . Points
vibratoBy, detuneBy :: Rational -> Phrase -> Phrase
vibratoBy d = addPitch (Const d :*: Osc Sine 3)
detuneBy c = addPitch (Cents (Const c))

centsOf :: Event -> Rational
centsOf e = let (_, c, _) = pitchParts (pitchShift e) in c

-- | The numeric view with its reading marks on.
marked :: NumericView
marked = plain {showMarks = True}

view :: SheetView
view = SheetView 4 "C" (M.singleton "right" Treble) False
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
        sort [(fromRational t*0.5,fromRational d*0.5) | (_,t,d,ps,_) <- events, _ <- ps]
      sheet <- evalEither (engrave perf view music)
      assert ("~" `isInfixOf` sheet && "\\\\" `isInfixOf` sheet))
  , ("strict sheet/MIDI bounds and exact tick resolution", withTests 1 $ property $ do
      let micro = perf {instruments=M.singleton "right" defaultInstrument {tuning=A.Tuning 440 (2 ** (1/24))}}
          note = Line "right" (n 0)
      let percussion = perf {instruments=M.singleton "right" defaultInstrument {timbre=A.Classic A.kick}}
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
      numeric plain line === Right "voice: 1 2 3 -1 0 (4 5 6)\n"
      numeric plain (Silence 2 :>>: Line "micro" (hit [1] (1/3) 1)) === Right "micro: _:2 1:1/3\n"
      lanes <- evalEither (numericLanes (Line "a" (n 0) :||: Line "a" (hit [4] 2 1)))
      map (sum . map dur . snd) lanes === [2,2]
      length lanes === 2
      let unsafe = defaultPerformance {instruments=M.singleton "<unsafe>" defaultInstrument}
      html <- evalEither (numericHtml plain unsafe (Line "<unsafe>" (n 0)))
      assert ("&lt;unsafe&gt;" `isInfixOf` html && not ("<unsafe>" `isInfixOf` html)))
  , ("numeric lanes align on shared onsets", withTests 1 $ property $ do
      let hands = Line "a" (hit [0] 2 1 <> n 1) :||: Line "b" (n 5 <> n 6 <> n 7)
      numeric plain hands === Right "a: 0:2   1\nb: 5   6 7\n"
      -- A gap inside one lane still gets its own column in the others.
      numeric plain (Line "a" (n 0 <> hush 1 <> n 2) :||: Line "b" (hit [4] 3 1)) === Right "a: 0   _ 2\nb: 4:3\n"
      -- Wrapping only splits between columns, so every system stays aligned.
      let long = Line "right" (concatMap n [0..40]) :||: Line "right" (hit [-12] 41 1)
      page <- evalEither (numericPage plain perf long)
      let systemsOf = filter (\l -> "right / " `isPrefixOf` l) (lines page)
      assert (length systemsOf > 2 && all ((<= 80) . length) systemsOf))
  , ("legend states tempo, tuning and keys beside the numbers", withTests 1 $ property $ do
      doc <- evalEither (parseDocument (header++"instrument left = piano staff bass\nright: 0 7\nleft: -12\n"))
      page <- evalEither (numericPage plain (performance doc) (score doc))
      let legendLines = takeWhile (not . null) (lines page)
      legendLines === ["tempo 120, durations in beats (default 1)"
                      ,"right, left: 12-EDO, 0 = C4"
                      ,"  keys: -12=C3 0=C4 7=G4"]
      micro <- evalEither (parseDocument "tuning 24 at A4\ninstrument m = sine\nm: 0 1\n")
      microPage <- evalEither (numericPage plain (performance micro) (score micro))
      assert ("m: 24-EDO, 0 = 440.0 Hz" `isInfixOf` microPage && not ("keys:" `isInfixOf` microPage))
      assert (isLeft (numericPage plain perf (Line "nobody" (n 0)))))
  , ("level scales, so nested levels compose", property $ do
      a <- forAll (Gen.double (Range.linearFrac 0 1))
      b <- forAll (Gen.double (Range.linearFrac 0 1))
      let phrase = hit [0] 1 0.8 <> hit [4] 1 0.4
          by x = scaleLevel (Const (toRational x))
          close x y = abs (x - y) < 1e-12
      assert (and (zipWith close (map vel (by a (by b phrase))) (map vel (by (a*b) phrase))))
      doc <- evalEither (parseDocument (header++"right: vel 0.5 [0 vel 0.6 [4] 7]\n"))
      (_,es) <- evalEither (flatten (score doc))
      map (vel . event) es === [0.5, 0.3, 0.5])
  , ("score validation is independent of the legacy tree", withTests 1 $ property $ do
      let note = Line "right" (n 0)
      validate perf note === Right ()
      assert (isLeft (validate perf (Line "missing" (n 0))))
      assert (isLeft (validate perf {bpm=0} note))
      mapM_ (\i -> assert (isLeft (validate perf {instruments=M.singleton "right" i} note)))
        [ defaultInstrument {pan=1.5}, defaultInstrument {gain= -1}, defaultInstrument {send=2}
        , defaultInstrument {tuning=A.Tuning 0 2}, defaultInstrument {timbre=A.Classic A.pianoPatch {A.resonance=1}} ]
      -- Declared-but-unused instruments are still checked.
      assert (isLeft (validate perf {instruments=M.insert "spare" defaultInstrument {pan=9} (instruments perf)} note)))
  , ("gate: sounding length is a ratio, separate from occupied time", property $ do
      xs <- forAll (Gen.list (Range.linear 1 12) (Gen.int (Range.linear (-12) 12)))
      g <- forAll (Gen.element [1/4, 1/2, 3/4, 1, 3/2, 2])
      k <- forAll (Gen.element [1/3, 1/2, 2, 3])
      let phrase = concatMap n xs
          gated = scaleGate g phrase
      -- Articulation survives stretching, and gates never move onsets or change length.
      map gate (stretch k gated) === map gate gated
      fmap (map onset . snd) (flatten (Line "right" gated)) === fmap (map onset . snd) (flatten (Line "right" phrase))
      scoreLength (Line "right" gated) === scoreLength (Line "right" phrase)
      map gate (scaleGate 2 (scaleGate g phrase)) === map gate (scaleGate (2*g) phrase)
      timeline <- evalEither (render perf (Line "right" gated))
      map A.duration (A.notes timeline) === replicate (length xs) (fromRational (g * 1/2))
      assert (isLeft (render perf (Line "right" (withGate 0 phrase)))))
  , ("pedal holds every note to the end of its own score", withTests 1 $ property $ do
      let held = pedal (Line "right" (n 0 <> rest <> n 4 <> hit [7] 2 1))
      (_, es) <- evalEither (flatten held)
      map (gate . event) es === [5, 1, 3, 1]
      -- Inside a larger score, pedal ends where its own section ends.
      doc <- evalEither (parseDocument (header++"right: pedal [0 [4 & 7:2]] 9\n"))
      (_, ds) <- evalEither (flatten (score doc))
      [(onset e, gate (event e)) | e <- ds] === [(0,3),(1,2),(1,1),(3,1)]
      staccatoDoc <- evalEither (parseDocument (header++"right: gate 1/2 [0 gate 1/2 [4]]\n"))
      (_, ss) <- evalEither (flatten (score staccatoDoc))
      map (gate . event) ss === [1/2, 1/4]
      assert (isLeft (parseDocument (header++"gate = 0\nright: gate\n")))
      -- The canonical source round-trips gates.
      text <- evalEither (prettyScore (score staccatoDoc))
      again <- evalEither (parseDocument (header++text))
      (_, rs) <- evalEither (flatten (score again))
      sort [(onset e, gate (event e)) | e <- rs, not (null (pitches (event e)))] === sort [(onset e, gate (event e)) | e <- ss])
  , ("gate in MIDI, staff and numeric views", withTests 1 $ property $ do
      short <- evalEither (midi perf (Line "right" (staccato (n 0))))
      assert (BS.pack [0,0x90,60,127,1,0x80,60,0] `BS.isInfixOf` BL.toStrict short)
      -- A note ringing past the score's end extends the track instead of being cut.
      ringing <- evalEither (midi perf (Line "right" (withGate 2 (n 0))))
      assert (BS.pack [0,0x90,60,127,2,0x80,60,0,0,0xff,0x2f,0] `BS.isInfixOf` BL.toStrict ringing)
      -- Legato repeats of one pitch overlap, so they take separate channels.
      legato <- evalEither (midi perf (Line "right" (withGate (3/2) (n 0 <> n 0))))
      BL.take 2 (BL.drop 10 legato) === BL.pack [0,3]
      sheet <- evalEither (engrave perf view (Line "right" (staccato (n 0) <> scaleGate (3/4) (n 2) <> withGate 2 (hit [4] 6 1))))
      -- Articulation goes on a note's first segment; "let ring" on its last tied segment.
      assert ("c'4-. d'4-_ e'2~ | e'1\\laissezVibrer |" `isInfixOf` sheet)
      numeric marked (Line "right" (staccato (n 0) <> withGate 2 (n 4) <> n 7)) === Right "right: 0' 4~ 7\n"
      -- The default view is just the numbers.
      numeric plain (Line "right" (staccato (n 0) <> withGate 2 (n 4) <> n 7)) === Right "right: 0 4 7\n"
      page <- evalEither (numericPage marked perf (Line "right" (staccato (n 0))))
      assert ("' = detached: sounds shorter than written" `isInfixOf` page && not ("~ = rings" `isInfixOf` page)))
  , ("tempo laws: identity, composition, stretch, closed form", property $ do
      xs <- forAll (Gen.list (Range.linear 1 10) (Gen.int (Range.linear (-12) 12)))
      ds <- forAll (Gen.list (Range.singleton (length xs)) (Gen.element [1/4, 1/2, 1, 3/2, 2]))
      a <- forAll (Gen.element [1/3, 1/2, 3/4, 1, 5/4, 2])
      b <- forAll (Gen.element [1/2, 2/3, 1, 3/2])
      let line = Line "right" (concat (zipWith (\x d -> hit [x] d 1) xs ds))
          times sc = do
            t <- evalEither (render perf sc)
            pure (A.seconds t, [(A.start v, A.duration v) | v <- A.notes t])
          close (s1, n1) (s2, n2) = abs (s1 - s2) < 1e-9 && length n1 == length n2
            && and [abs (x - y) < 1e-9 && abs (u - w) < 1e-9 | ((x, u), (y, w)) <- zip n1 n2]
      plain <- times line
      same <- times (Tempo (steady 1) line)
      assert (close plain same)
      nested <- times (Tempo (steady a) (Tempo (steady b) line))
      product' <- times (Tempo (steady (a * b)) line)
      assert (close nested product')
      -- Log-linear ramps over the same span multiply into one ramp.
      ramps <- times (Tempo (Ramp 1 a) (Tempo (Ramp b 1) line))
      oneRamp <- times (Tempo (Ramp b a) line)
      assert (close ramps oneRamp)
      -- Audio can't tell a constant speed from rewritten durations.
      slower <- times (Tempo (steady (1 / a)) line)
      stretched <- times (mapPhrases (stretch a) line)
      assert (close slower stretched)
      (whole, _) <- times (Tempo (Ramp 1 (1/2)) (Line "right" (hit [0] 4 1)))
      assert (abs (whole - 0.5 * 4 / log 2) < 1e-12))
  , ("tempo laws: swing keeps beats, locality, monotone clock", property $ do
      xs <- forAll (Gen.list (Range.linear 2 16) (Gen.int (Range.linear (-12) 12)))
      let eighths = Line "right" (concatMap (\x -> hit [x] (1/2) 1) xs)
      straight <- evalEither (render perf eighths)
      swingRender <- evalEither (render perf (Tempo (swung 1 (2/3)) eighths))
      let pairs = zip3 [0 :: Int ..] (A.notes straight) (A.notes swingRender)
      assert (and [abs (A.start u - A.start v) < 1e-12 | (i, u, v) <- pairs, even i])
      assert (and [abs (A.start v - (A.start u - 0.25 + 0.5 * 2/3)) < 1e-12 | (i, u, v) <- pairs, odd i])
      -- Whole pairs keep the total; an unpaired last eighth is played long.
      if even (length xs) then assert (abs (A.seconds swingRender - A.seconds straight) < 1e-12)
        else assert (abs (A.seconds swingRender - (A.seconds straight - 0.25 + 0.5 * 2/3)) < 1e-12)
      -- A slowed middle section leaves earlier notes alone and shifts later ones evenly.
      let before = Line "right" (n 0 <> n 1)
          middle = Line "right" (n 2 <> n 3 <> n 4)
          after = Line "right" (n 5 <> n 6)
      flat <- evalEither (render perf (before :>>: middle :>>: after))
      warped <- evalEither (render perf (before :>>: Tempo (Ramp 1 (1/2)) middle :>>: after))
      let starts t = map A.start (A.notes t)
          extra = 0.5 * (3 / log 2) - 0.5 * 3
      take 2 (starts warped) === take 2 (starts flat)
      assert (and [abs (w - f - extra) < 1e-9 | (w, f) <- drop 5 (zip (starts warped) (starts flat))])
      -- Nested and overlapping shapes still give a strictly increasing clock.
      shape1 <- forAll (Gen.element [steady 1, steady (1/3), Ramp 2 (1/2), swung 1 (3/5)])
      shape2 <- forAll (Gen.element [steady 3, Ramp (1/4) 1, swung (1/2) (1/3)])
      clocked <- evalEither (render perf (Tempo shape1 (eighths :||: Tempo shape2 (Line "right" (concatMap n xs)))))
      let ordered = sortOn fst [(A.start v, ()) | v <- A.notes clocked]
      assert (all (\(u, w) -> fst u <= fst w) (zip ordered (drop 1 ordered))))
  , ("MIDI tempo map plays back at the audio's times", property $ do
      xs <- forAll (Gen.list (Range.linear 1 12) (Gen.int (Range.linear (-12) 12)))
      ds <- forAll (Gen.list (Range.singleton (length xs)) (Gen.element [1/4, 1/2, 3/4, 1, 2]))
      g <- forAll (Gen.element [1/2, 1, 3/2])
      shape <- forAll (Gen.element [steady (2/3), Ramp 1 (1/2), Ramp (1/2) (3/2), swung 1 (2/3), Cycle [(1, Ramp 1 (1/2)), (1/2, steady 2)]])
      let line = Line "right" (withGate g (concat (zipWith (\x d -> hit [x] d 1) xs ds)))
          music = Line "right" (n 0) :>>: Tempo shape line :>>: Line "right" (n 7)
      changes <- evalEither (tempoChanges perf music)
      timeline <- evalEither (render perf music)
      (_, es) <- evalEither (flatten music)
      -- Seconds at a written beat, from the emitted microseconds-per-quarter steps.
      let played b = sum [ fromInteger us * fromRational (min b x1 - x0) / 1e6
                         | ((x0, us), x1) <- zip changes (map fst (drop 1 changes) ++ [b]), x0 < b ]
          pitched = [e | e <- es, not (null (pitches (event e)))]
      assert (and [ abs (played (onset e) - A.start v) < 1e-6
                    && abs (played (onset e + dur (event e) * gate (event e)) - (A.start v + A.duration v)) < 1e-6
                  | (e, v) <- zip pitched (A.notes timeline) ]))
  , ("tempo in the language, staff and numeric views", withTests 1 $ property $ do
      doc <- evalEither (parseDocument (header++"right: 0 speed 1 -> 3/4 [2 4] swing 2/3 [5:1/2 7:1/2] hold 2 [9] 0\n"))
      map (\(t0, len, shape) -> (t0, len, shape)) (tempoNodes (score doc)) ===
        [(1, 2, Ramp 1 (3/4)), (3, 1, swung 1 (2/3)), (4, 1, steady (1/2))]
      mapM_ (assert . isLeft . parseDocument . (header++))
        ["right: speed 0 [0]", "right: swing 3/2 [0]", "right: swing 1 [0]", "right: hold 0 [0]", "right: speed 1 -> [0]"]
      reversed <- evalEither (parseDocument (header++"right: rev [speed 1 -> 1/2 [0 2] swing 1/3 [4:1/2 5:1/2]]\n"))
      map (\(_, _, shape) -> shape) (tempoNodes (score reversed)) === [swung 1 (2/3), Ramp (1/2) 1]
      -- Canonical source keeps the clock: tempo nodes are written over rests.
      text <- evalEither (prettyScore (score doc))
      again <- evalEither (parseDocument (header++text))
      t1 <- evalEither (render perf (score doc))
      t2 <- evalEither (render perf (score again))
      assert (abs (A.seconds t1 - A.seconds t2) < 1e-12)
      assert (and [abs (A.start u - A.start v) < 1e-12 | (u, v) <- zip (sortOn A.start (A.notes t1)) (sortOn A.start (filter ((> 0) . A.velocity) (A.notes t2)))])
      sheet <- evalEither (engrave perf view (score doc))
      assert (all (`isInfixOf` sheet)
        ["\\new Dynamics", "\\tempo 4 = 120", "\"rit.\"", "\"a tempo, Swing\"", "\"Straight\"", "a'4\\fermata"])
      page <- evalEither (numeric plain (score doc))
      assert (all (`isInfixOf` page) ["speed 1 -> 3/4", "a tempo, swing 2/3", "straight, hold 2"])
      -- A score without tempo changes prints no tempo lane.
      plain <- evalEither (numeric plain (Line "right" (n 0)))
      assert (not ("tempo" `isInfixOf` plain)))
  , ("signals: pitch adds, level multiplies, glide joins successors", withTests 1 $ property $ do
      let up = [(0, 0), (1, 2)]
          scoop = [(0, -1), (1/2, 0)]
          [single] = bendBy up (bendBy scoop (n 0))
      pitchShift single === Points up :+: Points scoop
      merged <- evalEither (render perf (Line "right" [single]))
      -- One pitch contour in octaves: the sum of both shapes everywhere.
      let near x y = abs (x - y) < 1e-9
          [bent] = A.notes merged
      assert (and [near (12 * A.contourAt (A.pitchMod bent) u) y | (u, y) <- [(0, -1), (1/2, 1), (1, 2), (3/4, 3/2)]])
      quiet <- evalEither (render perf (Line "right" (swellBy [(0, 1/2), (1, 1/2)] (swellBy [(0, 1), (1, 1/2)] (n 0)))))
      [near (A.contourAt (A.loudness v) 1) (1/4) && near (A.contourAt (A.loudness v) 0) (1/2) | v <- A.notes quiet] === [True]
      -- Constant parts stay note properties: strength, and frequency.
      strong <- evalEither (render perf (Line "right" (scaleLevel (Const (1/2)) (addPitch (Const 12) (n 0)))))
      octaveUp <- evalEither (render perf (Line "right" (n 12)))
      [(A.velocity v, A.pitchMod v, A.loudness v) | v <- A.notes strong] === [(0.5, A.Flat 0, A.Flat 1)]
      assert (and [near (A.hz u) (A.hz v) | (u, v) <- zip (A.notes strong) (A.notes octaveUp)])
      -- glide: 0 slides to 4, 4 to 7, 7 has no successor; the chord and the rest stop it.
      (_, es) <- evalEither (flatten (glide (1/4) (Line "right" (n 0 <> n 4 <> n 7 <> chord [0, 4] <> rest <> n 2))))
      map (pitchShift . event) es === [Points [(0,0),(3/4,0),(1,4)], Points [(0,0),(3/4,0),(1,3)], Const 0, Const 0, Const 0, Const 0]
      -- Across separate lines too (the language makes one line per note), but not
      -- when two parallel notes could follow.
      (_, gs) <- evalEither (flatten (glide (1/2) (Line "right" (n 0) :>>: Line "right" (n 2))))
      map (pitchShift . event) gs === [Points [(0,0),(1/2,0),(1,2)], Const 0]
      (_, amb) <- evalEither (flatten (glide (1/2) (Line "right" (n 0) :>>: (Line "right" (n 2) :||: Line "right" (n 5)))))
      map (pitchShift . event) amb === [Const 0, Const 0, Const 0]
      -- Bounds: a level that can go below 0 is rejected; tremolo above 0 is fine.
      assert (isLeft (render perf (Line "right" (swellBy [(0, -1)] (n 0)))))
      assert (isLeft (render perf (Line "right" (scaleLevel (Osc Sine 2) (n 0)))))
      _ <- evalEither (render perf (Line "right" (scaleLevel (Const (3/4) :+: Const (1/4) :*: Osc Sine 4) (n 0))))
      assert (isLeft (render perf (Line "right" (scaleLevel (Cents (Const 3)) (n 0)))))
      assert (isLeft (render perf (Line "right" (bendBy [(1, 0), (0, 1)] (n 0))))))
  , ("signals: a bend really moves the pitch; oscillators count beats", withTests 1 $ property $ do
      -- A sine at C4 bending up an octave over 1 s: twice the zero crossings by the end.
      let sinePerf = perf {bpm = 60, instruments = M.singleton "right" defaultInstrument {timbre = A.Classic A.sinePatch}}
      (l, _) <- evalEither (A.renderTimelineMix (A.defaultMix 8000) =<< render sinePerf (Line "right" (bendBy [(0, 0), (1, 12)] (n 0))))
      let crossings a b = length [() | i <- [a .. b - 2], l VU.! i <= 0, l VU.! (i + 1) > 0]
          early = crossings 0 800
          late = crossings 7200 8000
      assert (early >= 25 && early <= 29 && late >= 47 && late <= 53)
      plain <- evalEither (A.renderTimelineMix (A.defaultMix 8000) =<< render sinePerf (Line "right" (n 0)))
      vibrated <- evalEither (A.renderTimelineMix (A.defaultMix 8000) =<< render sinePerf (Line "right" (vibratoBy (1/2) (n 0))))
      assert (fst plain /= fst vibrated)
      -- A square trill at 8 cycles a beat: cycles over a note = rate x sounding beats.
      let trill = addPitch (Const (1/2) :+: Const (1/2) :*: Osc Square 8)
          cyclesIn phrase = do
            t <- evalEither (render perf (Line "right" phrase))
            let [v] = A.notes t
                ys = [A.contourAt (A.pitchMod v) (fromIntegral k / 4096) | k <- [0 .. 4096 :: Int]]
            -- The constant half goes to the frequency, so the varying part swings
            -- around 0; each cycle ends with a rise into the next (the last at u = 1).
            pure (length [() | (a, b) <- zip ys (drop 1 ys), a < 0, b > 0])
      one <- cyclesIn (trill (n 0))
      two <- cyclesIn (trill (hit [0] 2 1))
      half <- cyclesIn (trill (withGate (1/2) (hit [0] 2 1)))
      (one, two, half) === (8, 16, 8))
  , ("signals in the language, prelude, MIDI, staff and numeric views", withTests 1 $ property $ do
      doc <- evalEither (parseDocument (header++"instrument left = piano staff bass\nright: glide 1/2 [0 4] bend {0:0 1/2:1 1:0} [7] swell 1 -> 1/2 [9]\nleft: vibrato 1/4 [-12:4]\n"))
      (_, es) <- evalEither (flatten (score doc))
      [pitchShift (event e) | e <- es, instrument e == "right"] ===
        [Points [(0,0),(1/2,0),(1,4)], Const 0, Points [(0,0),(1/2,1),(1,0)], Const 0]
      [level (event e) | e <- es, instrument e == "right"] === [Const 1, Const 1, Const 1, Points [(0,1),(1,1/2)]]
      [pitchShift (event e) | e <- es, instrument e == "left"] === [Const (1/4) :*: Osc Sine 3]
      -- The primitives directly, and the forms the old fields couldn't say.
      let first src = do
            d <- evalEither (parseDocument (header ++ "right: " ++ src ++ "\n"))
            (_, xs) <- evalEither (flatten (score d))
            case xs of
              x : _ -> pure (event x)
              [] -> failure
      mixed <- first "pitch (2 + cents 14) [0]"
      pitchParts (pitchShift mixed) === (2, 14, Const 0)
      arrow <- first "pitch 0 -> 1 [0]"
      braces <- first "pitch {0:0 1:1} [0]"
      pitchShift arrow === pitchShift braces
      tremolo <- first "level (3/4 + 1/4 * sine 4) [0]"
      level tremolo === Const (3/4) :+: Const (1/4) :*: Osc Sine 4
      mapM_ (assert . isLeft . parseDocument . (header++))
        [ "right: glide 2 [0 4]", "right: bend 1 -> [0]", "right: bend {0:0 0} [0]"
        , "right: level (sine 2) [0]", "right: stretch (sine 1) [0]", "right: [0]*{0:1 1:2}"
        , "right: pitch 1 / (sine 1) [0]", "right: level (cents 3) [0]" ]
      assert (isLeft (parseDocument "instrument x = sine vibrato-rate 6\nx: 0"))
      text <- evalEither (prettyScore (score doc))
      again <- evalEither (parseDocument (header++"instrument left = piano staff bass\n"++text))
      (_, rs) <- evalEither (flatten (score again))
      sort [(instrument e, onset e, show (pitchShift (event e)), show (level (event e))) | e <- rs, not (null (pitches (event e)))]
        === sort [(instrument e, onset e, show (pitchShift (event e)), show (level (event e))) | e <- es]
      -- MIDI: a bend sets the channel's range, then moves the bend wheel.
      bent <- evalEither (midi perf (Line "right" (bendBy [(0, 0), (1, 2)] (n 0))))
      let bytes = BL.toStrict bent
      assert (BS.pack [0,0xb0,101,0,0,0xb0,100,0,0,0xb0,6,2] `BS.isInfixOf` bytes)
      assert (BS.pack [0,0xe0,0,64,0,0x90,60,127] `BS.isInfixOf` bytes)
      assert (BS.pack [1,0xe0,0,96] `BS.isInfixOf` bytes)  -- halfway: one semitone of two
      -- A curved note never shares a channel with a note it overlaps; a plain
      -- strength is velocity, not expression.
      shared <- evalEither (midi perf (Line "right" (bendBy [(0, 0), (1, 1)] (n 0)) :||: Line "right" (n 4)))
      BL.take 2 (BL.drop 10 shared) === BL.pack [0,3]
      softer <- evalEither (midi perf (Line "right" (scaleLevel (Const (1/2)) (n 0)) :||: Line "right" (n 4)))
      BL.take 2 (BL.drop 10 softer) === BL.pack [0,2]
      sheet <- evalEither (engrave perf view (Line "right" (bendBy [(0,0),(1,4)] (n 0) <> n 4 <> vibratoBy (1/4) (n 7) <> swellBy [(0,1),(1,1/2)] (n 9))))
      assert (all (`isInfixOf` sheet) ["c'4\\glissando e'4", "g'4^\\markup \\italic \"vib.\"", "a'4_\\markup \\italic \"dim.\""])
      numeric marked (Line "right" (bendBy [(0,0),(1,1)] (n 0) <> vibratoBy (1/4) (n 2) <> swellBy [(0,1),(1,0)] (n 4) <> bendBy [(0,0),(1/2,1),(1,0)] (n 5)))
        === Right "right: 0/ 2w 4> 5^\n"
      -- A constant step offset reads exactly, like cents.
      numeric plain (Line "right" (addPitch (Const (1/2)) (n 4))) === Right "right: 4+1/2\n"
      page <- evalEither (numericPage marked perf (Line "right" (vibratoBy (1/4) (n 2))))
      plainPage <- evalEither (numericPage plain perf (Line "right" (vibratoBy (1/4) (n 2))))
      assert (not ("vibrato" `isInfixOf` plainPage) && "right: 2" `isInfixOf` plainPage)
      assert ("w = vibrato" `isInfixOf` page && not ("/ = pitch" `isInfixOf` page)))
  , ("cents: a fine offset under the steps, in every view", withTests 1 $ property $ do
      -- 100 cents is one 12-EDO step; offsets add; transposing keeps them; inverting mirrors them.
      a <- evalEither (render perf (Line "right" (detuneBy 100 (n 0))))
      b <- evalEither (render perf (Line "right" (n 1)))
      assert (and [abs (A.hz u / A.hz v - 1) < 1e-12 | (u, v) <- zip (A.notes a) (A.notes b)])
      map centsOf (detuneBy 5 (detuneBy (-19) (n 4))) === [-14]
      map centsOf (transpose 7 (detuneBy 3 (n 0))) === [3]
      [(pitches e, centsOf e) | e <- invert (detuneBy 3 (n 4))] === [([A.Step (-4)], -3)]
      map centsOf (detuneBy 9 rest) === [0]
      doc <- evalEither (parseDocument (header++"right: 0:4 & cents -14 [4:4] & cents 1/2 [7:4]\n"))
      (_, es) <- evalEither (flatten (score doc))
      sort (map (centsOf . event) es) === [-14, 0, 1/2]
      text <- evalEither (prettyScore (score doc))
      again <- evalEither (parseDocument (header++text))
      (_, rs) <- evalEither (flatten (score again))
      sort [centsOf (event e) | e <- rs, not (null (pitches (event e)))] === [-14, 0, 1/2]
      -- MIDI: the key is the step, the offset a constant bend on its own channel.
      half <- evalEither (midi perf (Line "right" (detuneBy 50 (n 0))))
      assert (BS.pack [0,0xb0,6,1] `BS.isInfixOf` BL.toStrict half)
      assert (BS.pack [0,0xe0,0,96,0,0x90,60,127] `BS.isInfixOf` BL.toStrict half)
      numeric plain (Line "right" (detuneBy (-14) (n 4) <> n 7)) === Right "right: 4-14c 7\n"
      sheet <- evalEither (engrave perf view (Line "right" (detuneBy (-14) (n 4))))
      assert ("e'4^\\markup \\small \"-14¢\"" `isInfixOf` sheet))
  , ("staff carries the numeric notation under the notes", withTests 1 $ property $ do
      let numbered = view {numbers = True}
          music = Line "right" (n 0 <> chord [0, 4, 7] <> hit [12] 2 1) :||: Line "right" (hush 2 <> n 2 <> n 4)
      sheet <- evalEither (engrave perf numbered music)
      assert (all (`isInfixOf` sheet)
        [ "c'4_\\markup \\small \"0\""
        , "\\center-column { \"7\" \"4\" \"0\" }"
        , "Numbers under the notes are Neo steps: 0 = C4"
        , "{ s2 d'4" ])  -- the second voice's gap is an invisible spacer
      plain <- evalEither (engrave perf view music)
      assert (not ("Neo steps" `isInfixOf` plain) && not ("_\\markup \\small \"0\"" `isInfixOf` plain)))
  , ("number parameters: arithmetic, notes, whole numbers, kinds", withTests 1 $ property $ do
      let events src = do
            doc <- evalEither (parseDocument (header ++ src))
            (_, es) <- evalEither (flatten (score doc))
            pure es
          rejects src = assert (isLeft (parseDocument (header ++ src)))
      -- Arithmetic with parentheses; a fraction is still division.
      es <- events "f #a #b x = stretch (a + b)/2 [x]\nright: f 1 3 [0 2] 4:1/2\n"
      map (dur . event) es === [2, 2, 1/2]
      -- Decimals are exact, exponents included.
      vs <- events "right: vel 0.25 [0] vel 1.0e-2 [0]\n"
      map (vel . event) vs === [0.25, 0.01]
      -- A number parameter in a music position is a note at that step.
      ts <- events "triad #r = r & [r + 4] & [r + 7]\nright: triad 2\n"
      sort (map (pitches . event) ts) === [[A.Step 2], [A.Step 6], [A.Step 9]]
      -- Suffixes take one atom: a name, a literal fraction, or parentheses.
      ss <- events "f #k x = x:(1/k) [x + k]*k\nright: f 2 [0]\n"
      map (dur . event) ss === [1/2, 1, 1]
      map (pitches . event) ss === [[A.Step 0], [A.Step 2], [A.Step 2]]
      -- '-2' is a negative number; '- 2' subtracts.
      ns <- events "f #k x = stretch k - 1 [x]\nright: f 3 [0] -2\n"
      map (dur . event) ns === [2, 1]
      map (pitches . event) ns === [[A.Step 0], [A.Step (-2)]]
      mapM_ rejects
        [ "up2 x = up 1/2 [x]\nright: up2 [0]"           -- transposition must be whole
        , "f #k x = [x]*k\nright: f 3/2 [0]"            -- repeat count must be whole
        , "f #k = k\nright: f 1/3"                       -- a note must be a whole step
        , "f #k x = stretch 1/k [x]\nright: f 0 [0]"    -- division by zero
        , "right: hold [0] [0]"                          -- music where a number goes
        , "f x = vel x [0]\nright: f [0]"                -- music used as a number
        , "f x = vel y [x]\nright: f [0]"                -- unknown number
        , "f #k #k x = x\nright: f 1 1 [0]"             -- duplicate parameter
        , "f #speed x = x\nright: f 1 [0]" ])           -- reserved word
  , ("the prelude: ordinary definitions a score can shadow", withTests 1 $ property $ do
      let events src = do
            doc <- evalEither (parseDocument (header ++ src))
            (_, es) <- evalEither (flatten (score doc))
            pure es
      -- hold is no longer a keyword: it is speed 1/k from the prelude.
      held <- evalEither (parseDocument (header ++ "right: hold 3 [0:2] 4\n"))
      tempoNodes (score held) === [(0, 2, steady (1/3))]
      gs <- events "right: staccato [0 2] legato [4]\n"
      map (gate . event) gs === [1/2, 1/2, 3/2]
      echoes <- events "right: echo 2 1/2 [0]\n"
      map (vel . event) echoes === [1, 0.5, 1, 0.5]
      os <- events "right: octave [0] twice [7]\n"
      map (pitches . event) os === [[A.Step 12], [A.Step 7], [A.Step 7]]
      -- A score's own definition wins...
      mine <- events "staccato x = gate 1/4 [x]\nright: staccato [0]\n"
      map (gate . event) mine === [1/4]
      -- ...but prelude definitions keep their own meaning: octave uses the prelude's up.
      lexical <- events "up #n x = x\nright: octave [0] up 5 [0]\n"
      map (pitches . event) lexical === [[A.Step 12], [A.Step 0]]
      -- A duplicate binding within one file is still an error.
      assert (isLeft (parseDocument (header ++ "a = 1\na = 2\nright: a\n"))))
  , ("tempo cycles: segments repeat; swing is one; source round-trips", withTests 1 $ property $ do
      let starts sc = do
            t <- evalEither (render perf sc)
            pure (A.seconds t, map A.start (A.notes t))
      -- One beat as written, one at half speed, repeated (120 bpm: 0.5 s a beat).
      (total, onsets) <- starts (Tempo (Cycle [(1, steady 1), (1, steady (1/2))]) (Line "right" (concatMap n [0, 2, 4, 5])))
      (total, onsets) === (3, [0, 0.5, 1.5, 2])
      -- A ramp inside a segment ramps over the segment, each period.
      (ramped, _) <- starts (Tempo (Cycle [(2, Ramp 1 (1/2))]) (Line "right" (hit [0] 4 1)))
      assert (abs (ramped - 2 * 0.5 * 2 / log 2) < 1e-12)
      -- swing and swingPer are cycles from the prelude.
      let nodes src = do
            d <- evalEither (parseDocument (header ++ "right: " ++ src ++ "\n"))
            pure (d, [shape | (_, _, shape) <- tempoNodes (score d)])
      (_, written) <- nodes "speed cycle {1/2:3/4 1/2:3/2} [0:1/2 2:1/2]"
      written === [swung 1 (2/3)]
      (perDoc, per) <- nodes "swingPer 1/2 2/3 [0:1/4 2:1/4 4:1/4 5:1/4]"
      per === [swung (1/2) (2/3)]
      swingOf (swung (1/2) (2/3)) === Just (1/2, 2/3)
      speedOf (steady (3/4)) === Just (3/4)
      -- Swing at any period, and nested cycles, now have a source form.
      text <- evalEither (prettyScore (score perDoc))
      again <- evalEither (parseDocument (header ++ text))
      [shape | (_, _, shape) <- tempoNodes (score again)] === per
      (nestedDoc, nested) <- nodes "speed cycle {2:(cycle {1/2:2 1/2:1}) 1:(1 -> 1/2)} [0:3]"
      nested === [Cycle [(2, Cycle [(1/2, steady 2), (1/2, steady 1)]), (1, Ramp 1 (1/2))]]
      nestedText <- evalEither (prettyScore (score nestedDoc))
      nestedAgain <- evalEither (parseDocument (header ++ nestedText))
      [shape | (_, _, shape) <- tempoNodes (score nestedAgain)] === nested
      mapM_ (assert . isLeft . parseDocument . (header++))
        [ "right: speed cycle {} [0]", "right: speed cycle {0:1} [0]", "right: speed cycle {1:0} [0]"
        , "right: speed cycle {1:(sine 1)} [0]", "cycle x = x\nright: cycle [0]" ]
      -- Views: swing at another period, and a general cycle.
      perPage <- evalEither (numeric plain (score perDoc))
      assert ("swingPer 1/2 2/3" `isInfixOf` perPage)
      general <- evalEither (parseDocument (header ++ "right: speed cycle {1:1 1:1/2} [0 2 4 5] 7\n"))
      generalPage <- evalEither (numeric plain (score general))
      assert ("speed cycle {1:1 1:1/2}" `isInfixOf` generalPage && "a tempo" `isInfixOf` generalPage)
      sheet <- evalEither (engrave perf view (score general))
      assert ("\"cycling tempo\"" `isInfixOf` sheet && "\"a tempo\"" `isInfixOf` sheet))
  , ("sounds and mix in .neomusic", withTests 1 $ property $ do
      -- The presets file writes every built-in sound; each equals its preset exactly.
      presetText <- evalIO (readFile "examples/neomusic/presets.neomusic")
      written <- evalEither (parseSounds presetText)
      let builtIn = [ ("basic", A.basicPatch), ("piano", A.pianoPatch), ("sine", A.sinePatch), ("saw", A.sawPatch)
                    , ("square", A.squarePatch), ("pluck", A.pluck), ("supersaw", A.supersaw), ("reeseBass", A.reeseBass)
                    , ("subBass", A.subBass), ("kick", A.kick), ("snare", A.snare), ("hat", A.hat), ("noiseSweep", A.noiseSweep) ]
      [(name, M.lookup name written == Just (A.Classic patch)) | (name, patch) <- builtIn] === [(name, True) | (name, _) <- builtIn]
      -- Indented lines continue the statement above, music included.
      one <- evalEither (parseDocument (header ++ "right: 0 2 4 5 7\n"))
      wrapped <- evalEither (parseDocument (header ++ "right: 0 2\n   4 5\n\t7\n"))
      score wrapped === score one
      -- A sound from a preset: parts given replace, the rest is kept; osc lines replace the set.
      doc <- evalEither (parseDocument (unlines
        [ "tempo 120", "tuning 12 at C4"
        , "sound glass from pluck"
        , "  osc triangle"
        , "  osc sine up 12 gain 0.5"
        , "  amp velocity * adsr 0.001 0.7 0 0.6"
        , "sound wobble from sine pitch 0.2 * lfo sine 5.5"
        , "reverb hall wet 0.5"
        , "master drive 1.5 peak 0.89 tail 2 rate 48000"
        , "instrument right = glass send 0.4"
        , "instrument left = wobble"
        , "right: 0", "left: 7" ]))
      let timbreOf name = case timbre (instruments (performance doc) M.! name) of
            A.Classic patch -> patch
            other -> error ("not a classic sound: " ++ show other)
          glass = timbreOf "right"
      A.oscillators glass === [A.Osc A.TriangleWave 0 1 1 0, A.Osc A.SineWave 12 0.5 1 0]
      A.amp glass === (A.Velocity A.:* A.adsr 0.001 0.7 0 0.6)
      (A.cutoff glass, A.resonance glass, A.stereoWidth glass) === (A.cutoff A.pluck, A.resonance A.pluck, A.stereoWidth A.pluck)
      A.pitch (timbreOf "left") === (A.K 0.2 A.:* A.Lfo A.SineWave 5.5)
      mix (performance doc) === A.Mix 48000 A.hall {A.wet = 0.5} 1.5 0.89 2 True
      -- With a reverb, send means something: the wet render rings on after a dry one stops.
      let blip extra = parseDocument (unlines ["tempo 60", extra, "instrument right = sine send 1", "right: 0"])
      dryDoc <- evalEither (blip "master tail 1 rate 8000")
      wetDoc <- evalEither (blip "reverb hall\nmaster tail 1 rate 8000")
      (dryL, _) <- evalEither (render (performance dryDoc) (score dryDoc) >>= A.renderTimelineMix (mix (performance dryDoc)))
      (wetL, _) <- evalEither (render (performance wetDoc) (score wetDoc) >>= A.renderTimelineMix (mix (performance wetDoc)))
      assert (VU.all (== 0) (VU.drop 8200 dryL) && VU.any ((> 1e-3) . abs) (VU.drop 8400 wetL))
      mapM_ (assert . isLeft . parseDocument . (header ++))
        -- Each fails for its own reason: sounds are checked even when unused.
        [ "sound a from nowhere\nright: 0"
        , "sound a from b\nsound b from a\nright: 0"
        , "sound a amp 1 amp 2\nright: 0"
        , "sound a amp lfo sine 3\nright: 0"
        , "sound a cutoff hz 400 / key\nright: 0"
        , "sound a colour 3\nright: 0"
        , "sound a\nsound a\nright: 0"
        , "instrument x = nothing\nright: 0"
        , "reverb wet -1", "reverb hall loud 1", "master peak 2", "master rate 4000"
        , "master normalize maybe", "reverb hall\nreverb none" ])
  , ("voices in .neomusic", withTests 1 $ property $ do
      -- A voice expression builds exactly the Voice that Haskell would, and may
      -- name other sounds, presets included, in any order.
      sounds <- evalEither (parseSounds (unlines
        [ "sound ensemble = gain 0.5 violins + harp"
        , "sound violins = gain (velocity * adsr 0.08 0.3 0.85 0.4) (unison 4 14 0.8"
        , "    (bend (0.1 * then 0.25 0 (lfo sine 5.5))"
        , "      (bandpass (hz 450) 1.2 tone saw + bandpass (hz 1600) 1 tone saw)"
        , "     + gain (0.08 * adsr 0.005 0.06 0 0.05) (highpass (hz 3000) noise)))"
        , "sound harp = gain velocity lowpass (hz 3500 + 0.4 * key) 0 plucked 2.5 0.6"
        , "sound bright = pan -0.5 pluck" ]))
      let saw = A.Tone A.SawWave
          violins = A.Gain (A.Velocity A.:* A.adsr 0.08 0.3 0.85 0.4) (A.Unison 4 14 0.8
            (A.Bend (A.K 0.1 A.:* A.Then 0.25 (A.K 0) (A.Lfo A.SineWave 5.5))
               (A.Bandpass (A.cutoffHz 450) 1.2 saw A.:& A.Bandpass (A.cutoffHz 1600) 1 saw)
             A.:& A.Gain (A.K 0.08 A.:* A.adsr 0.005 0.06 0 0.05) (A.Highpass (A.cutoffHz 3000) A.Noise)))
          harp = A.Gain A.Velocity (A.Lowpass (A.cutoffHz 3500 A.:+ A.K 0.4 A.:* A.Key) 0 (A.Plucked 2.5 0.6))
      M.lookup "violins" sounds === Just violins
      M.lookup "harp" sounds === Just harp
      M.lookup "ensemble" sounds === Just (A.Gain (A.K 0.5) violins A.:& harp)
      M.lookup "bright" sounds === Just (A.Pan (A.K (-0.5)) (A.Classic A.pluck))
      -- A voice plays, and its notes go to MIDI and the staff as pitched notes.
      doc <- evalEither (parseDocument (unlines
        [ "tempo 120", "tuning 12 at C4", "sound harp = gain (velocity * adsr 0.001 0 1 1) plucked 2 0.5"
        , "instrument right = harp", "right: 0 4 7" ]))
      (l, _) <- evalEither (render (performance doc) (score doc) >>= A.renderTimelineMix (mix (performance doc)))
      assert (VU.any ((> 0.1) . abs) l)
      _ <- evalEither (midi (performance doc) (score doc))
      _ <- evalEither (engrave (performance doc) (sheetView doc) (score doc))
      mapM_ (assert . isLeft . parseDocument . (header ++))
        -- Each fails for its own reason.
        [ "sound a = tone noise\nright: 0"
        , "sound a = plucked 0 1\nright: 0"
        , "sound a = gain 1 b\nsound b = a\nright: 0"
        , "sound a = gain 1 nowhere\nright: 0"
        , "sound a = tone saw\nsound b from a amp 1\nright: 0"
        , "sound noise = tone saw\nright: 0"
        , "sound a = pan 2 tone saw\nright: 0"
        , "sound a = tone saw +\nright: 0" ]
      -- The last parses, but a noise voice is unpitched, so it has no MIDI note.
      drums <- evalEither (parseDocument (unlines
        [ "tempo 120", "tuning 12 at C4", "sound a = noise", "instrument drums = a", "drums: 0" ]))
      assert (isLeft (midi (performance drums) (score drums))))
  , ("phrase signals: across a group, not each note", withTests 1 $ property $ do
      let events src = do
            d <- evalEither (parseDocument (header ++ "right: " ++ src ++ "\n"))
            (_, es) <- evalEither (flatten (score d))
            pure (d, map event es)
      -- A crescendo: each note's start is its strength; the rest rises within it.
      (_, rising) <- events "cresc 0.2 1 [0 2 4 5]"
      map vel rising === [0.2, 0.4, 0.6, 0.8]
      map level rising === [Const (2/10) :*: Points [(0, 1), (1, 2)], Const (4/10) :*: Points [(0, 1), (1, 3/2)]
                           , Const (6/10) :*: Points [(0, 1), (1, 4/3)], Const (8/10) :*: Points [(0, 1), (1, 5/4)]]
      map swellMotion rising === replicate 4 Rising
      -- Breakpoints restrict exactly; a constant span folds to a constant.
      (_, held) <- events "level across {0:1 1/4:1/2 3/4:1/2 1:1} [0:4]"
      map level held === [Points [(0, 1), (1/4, 1/2), (3/4, 1/2), (1, 1)]]
      (_, flat) <- events "level across {0:1 1/2:1 1:1/2} [0:2 2:2]"
      map level flat === [Const 1, Points [(0, 1), (1, 1/2)]]
      -- Pitch adds across the group; a detached note only gets its sounding span.
      (_, drift) <- events "pitch across 0 -> -1 [0 0]"
      map pitchShift drift === [Points [(0, 0), (1, -1/2)], Points [(0, -1/2), (1, -1)]]
      (_, short) <- events "cresc 0 1 [staccato [0 0]]"
      map level short === [Points [(0, 0), (1, 1/4)], Const (1/2) :*: Points [(0, 1), (1, 3/2)]]
      -- Nesting multiplies; oscillators can't span a phrase.
      (_, nested) <- events "vel 0.5 [cresc 0.5 1 [0 0]]"
      map vel nested === [0.25, 0.375]
      assert (isLeft (parseDocument (header ++ "right: level across (sine 2) [0]\n")))
      (_, softer) <- events "dim 1 0.5 [0 0]"
      map vel softer === [1, 0.75]
      -- The staff marks a run once, where it starts.
      (d, _) <- events "cresc 0.2 1 [0 2 4 5] 7"
      sheet <- evalEither (engrave perf view (score d))
      length (filter (isPrefixOf "cresc.") (tails sheet)) === 1
      -- Canonical source keeps each note's share.
      text <- evalEither (prettyScore (score d))
      again <- evalEither (parseDocument (header ++ text))
      (_, xs) <- evalEither (flatten (score d))
      (_, ys) <- evalEither (flatten (score again))
      sort [(onset e, show (level (event e))) | e <- ys, not (null (pitches (event e)))]
        === sort [(onset e, show (level (event e))) | e <- xs])
  , ("LilyPond golden views", withTests 1 $ property $ do
      mapM_ (\name -> do
        source <- evalIO (readFile ("examples/neomusic/"++name++".neomusic"))
        doc <- evalEither (parseDocument source)
        actual <- evalEither (engrave (performance doc) (sheetView doc) (score doc))
        expected <- evalIO (readFile ("test/golden/neomusic/"++name++".ly"))
        actual === expected) ["scale","twinkle","voices","ties","tempo","curves","cents"])
  ]
