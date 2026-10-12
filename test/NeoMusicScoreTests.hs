{-# LANGUAGE OverloadedStrings #-}
module NeoMusicScoreTests (scoreGroup) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Either (isLeft)
import Data.List (sort, sortOn, isInfixOf, isPrefixOf)
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
  , ("vel scales, so nested levels compose", property $ do
      a <- forAll (Gen.double (Range.linearFrac 0 1))
      b <- forAll (Gen.double (Range.linearFrac 0 1))
      let phrase = hit [0] 1 0.8 <> hit [4] 1 0.4
          close x y = abs (x - y) < 1e-12
      assert (and (zipWith close (map vel (scaleVelocity a (scaleVelocity b phrase)))
                                 (map vel (scaleVelocity (a*b) phrase))))
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
        , defaultInstrument {tuning=A.Tuning 0 2}, defaultInstrument {timbre=A.pianoPatch {A.resonance=1}} ]
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
      same <- times (Tempo (Speed 1) line)
      assert (close plain same)
      nested <- times (Tempo (Speed a) (Tempo (Speed b) line))
      product' <- times (Tempo (Speed (a * b)) line)
      assert (close nested product')
      -- Log-linear ramps over the same span multiply into one ramp.
      ramps <- times (Tempo (Ramp 1 a) (Tempo (Ramp b 1) line))
      oneRamp <- times (Tempo (Ramp b a) line)
      assert (close ramps oneRamp)
      -- Audio can't tell a constant speed from rewritten durations.
      slower <- times (Tempo (Speed (1 / a)) line)
      stretched <- times (mapPhrases (stretch a) line)
      assert (close slower stretched)
      (whole, _) <- times (Tempo (Ramp 1 (1/2)) (Line "right" (hit [0] 4 1)))
      assert (abs (whole - 0.5 * 4 / log 2) < 1e-12))
  , ("tempo laws: swing keeps beats, locality, monotone clock", property $ do
      xs <- forAll (Gen.list (Range.linear 2 16) (Gen.int (Range.linear (-12) 12)))
      let eighths = Line "right" (concatMap (\x -> hit [x] (1/2) 1) xs)
      straight <- evalEither (render perf eighths)
      swung <- evalEither (render perf (Tempo (Swing 1 (2/3)) eighths))
      let pairs = zip3 [0 :: Int ..] (A.notes straight) (A.notes swung)
      assert (and [abs (A.start u - A.start v) < 1e-12 | (i, u, v) <- pairs, even i])
      assert (and [abs (A.start v - (A.start u - 0.25 + 0.5 * 2/3)) < 1e-12 | (i, u, v) <- pairs, odd i])
      -- Whole pairs keep the total; an unpaired last eighth is played long.
      if even (length xs) then assert (abs (A.seconds swung - A.seconds straight) < 1e-12)
        else assert (abs (A.seconds swung - (A.seconds straight - 0.25 + 0.5 * 2/3)) < 1e-12)
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
      shape1 <- forAll (Gen.element [Speed 1, Speed (1/3), Ramp 2 (1/2), Swing 1 (3/5)])
      shape2 <- forAll (Gen.element [Speed 3, Ramp (1/4) 1, Swing (1/2) (1/3)])
      clocked <- evalEither (render perf (Tempo shape1 (eighths :||: Tempo shape2 (Line "right" (concatMap n xs)))))
      let ordered = sortOn fst [(A.start v, ()) | v <- A.notes clocked]
      assert (all (\(u, w) -> fst u <= fst w) (zip ordered (drop 1 ordered))))
  , ("MIDI tempo map plays back at the audio's times", property $ do
      xs <- forAll (Gen.list (Range.linear 1 12) (Gen.int (Range.linear (-12) 12)))
      ds <- forAll (Gen.list (Range.singleton (length xs)) (Gen.element [1/4, 1/2, 3/4, 1, 2]))
      g <- forAll (Gen.element [1/2, 1, 3/2])
      shape <- forAll (Gen.element [Speed (2/3), Ramp 1 (1/2), Ramp (1/2) (3/2), Swing 1 (2/3)])
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
        [(1, 2, Ramp 1 (3/4)), (3, 1, Swing 1 (2/3)), (4, 1, Speed (1/2))]
      mapM_ (assert . isLeft . parseDocument . (header++))
        ["right: speed 0 [0]", "right: swing 3/2 [0]", "right: swing 1 [0]", "right: hold 0 [0]", "right: speed 1 -> [0]"]
      reversed <- evalEither (parseDocument (header++"right: rev [speed 1 -> 1/2 [0 2] swing 1/3 [4:1/2 5:1/2]]\n"))
      map (\(_, _, shape) -> shape) (tempoNodes (score reversed)) === [Swing 1 (2/3), Ramp (1/2) 1]
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
  , ("curves: bends add, swells multiply, glide joins successors", withTests 1 $ property $ do
      let up = [(0, 0), (1, 2)]
          scoop = [(0, -1), (1/2, 0)]
          [single] = bend up (bend scoop (n 0))
      bends single === [up, scoop]
      merged <- evalEither (render perf (Line "right" [single]))
      let near xs ys = length xs == length ys && and [abs (a - c) < 1e-9 && abs (b - d) < 1e-9 | ((a, b), (c, d)) <- zip xs ys]
      -- One merged curve in octaves: the sum of both at every breakpoint.
      assert (near [(u, y * 12) | v <- A.notes merged, (u, y) <- A.bend v] [(0, -1), (1/2, 1), (1, 2)])
      quiet <- evalEither (render perf (Line "right" (swell [(0, 1/2), (1, 1/2)] (swell [(0, 1), (1, 1/2)] (n 0)))))
      map (length . A.swells) (A.notes quiet) === [2]
      -- glide: 0 slides to 4, 4 to 7, 7 has no successor; the chord and the rest stop it.
      (_, es) <- evalEither (flatten (glide (1/4) (Line "right" (n 0 <> n 4 <> n 7 <> chord [0, 4] <> rest <> n 2))))
      map (bends . event) es === [[[(0,0),(3/4,0),(1,4)]], [[(0,0),(3/4,0),(1,3)]], [], [], [], []]
      -- Across separate lines too (the language makes one line per note), but not
      -- when two parallel notes could follow.
      (_, gs) <- evalEither (flatten (glide (1/2) (Line "right" (n 0) :>>: Line "right" (n 2))))
      map (bends . event) gs === [[[(0,0),(1/2,0),(1,2)]], []]
      (_, amb) <- evalEither (flatten (glide (1/2) (Line "right" (n 0) :>>: (Line "right" (n 2) :||: Line "right" (n 5)))))
      map (bends . event) amb === [[], [], []]
      assert (isLeft (render perf (Line "right" (swell [(0, -1)] (n 0)))))
      assert (isLeft (render perf (Line "right" (bend [(1, 0), (0, 1)] (n 0))))))
  , ("curves: a bend really moves the pitch", withTests 1 $ property $ do
      -- A sine at C4 bending up an octave over 1 s: twice the zero crossings by the end.
      let sinePerf = perf {bpm = 60, instruments = M.singleton "right" defaultInstrument {timbre = A.sinePatch}}
      (l, _) <- evalEither (A.renderTimelineMix (A.defaultMix 8000) =<< render sinePerf (Line "right" (bend [(0, 0), (1, 12)] (n 0))))
      let crossings a b = length [() | i <- [a .. b - 2], l VU.! i <= 0, l VU.! (i + 1) > 0]
          early = crossings 0 800
          late = crossings 7200 8000
      assert (early >= 25 && early <= 29 && late >= 47 && late <= 53)
      plain <- evalEither (A.renderTimelineMix (A.defaultMix 8000) =<< render sinePerf (Line "right" (n 0)))
      vibrated <- evalEither (A.renderTimelineMix (A.defaultMix 8000) =<< render sinePerf (Line "right" (addVibrato (1/2) (n 0))))
      assert (fst plain /= fst vibrated))
  , ("curves in the language, MIDI, staff and numeric views", withTests 1 $ property $ do
      doc <- evalEither (parseDocument (header++"instrument left = piano staff bass vibrato-rate 6\nright: glide 1/2 [0 4] bend {0:0 1/2:1 1:0} [7] swell 1 -> 1/2 [9]\nleft: vibrato 1/4 [-12:4]\n"))
      (_, es) <- evalEither (flatten (score doc))
      [bends (event e) | e <- es, instrument e == "right"] ===
        [[[(0,0),(1/2,0),(1,4)]], [], [[(0,0),(1/2,1),(1,0)]], []]
      [swells (event e) | e <- es, instrument e == "right"] === [[], [], [], [[(0,1),(1,1/2)]]]
      [vibrato (event e) | e <- es, instrument e == "left"] === [1/4]
      vibratoRate (instruments (performance doc) M.! "left") === 6
      mapM_ (assert . isLeft . parseDocument . (header++))
        ["right: glide 2 [0 4]", "right: bend 1 -> [0]", "right: bend {0:0 0} [0]", "right: vibrato 0 [0]"]
      text <- evalEither (prettyScore (score doc))
      again <- evalEither (parseDocument (header++"instrument left = piano staff bass vibrato-rate 6\n"++text))
      (_, rs) <- evalEither (flatten (score again))
      sort [(instrument e, onset e, bends (event e), swells (event e), vibrato (event e)) | e <- rs, not (null (pitches (event e)))]
        === sort [(instrument e, onset e, bends (event e), swells (event e), vibrato (event e)) | e <- es]
      -- MIDI: a bend sets the channel's range, then moves the bend wheel.
      bent <- evalEither (midi perf (Line "right" (bend [(0, 0), (1, 2)] (n 0))))
      let bytes = BL.toStrict bent
      assert (BS.pack [0,0xb0,101,0,0,0xb0,100,0,0,0xb0,6,2] `BS.isInfixOf` bytes)
      assert (BS.pack [0,0xe0,0,64,0,0x90,60,127] `BS.isInfixOf` bytes)
      assert (BS.pack [1,0xe0,0,96] `BS.isInfixOf` bytes)  -- halfway: one semitone of two
      -- A curved note never shares a channel with a note it overlaps.
      shared <- evalEither (midi perf (Line "right" (bend [(0, 0), (1, 1)] (n 0)) :||: Line "right" (n 4)))
      BL.take 2 (BL.drop 10 shared) === BL.pack [0,3]
      plainBoth <- evalEither (midi perf (Line "right" (n 0) :||: Line "right" (n 4)))
      BL.take 2 (BL.drop 10 plainBoth) === BL.pack [0,2]
      sheet <- evalEither (engrave perf view (Line "right" (bend [(0,0),(1,4)] (n 0) <> n 4 <> addVibrato (1/4) (n 7) <> swell [(0,1),(1,1/2)] (n 9))))
      assert (all (`isInfixOf` sheet) ["c'4\\glissando e'4", "g'4^\\markup \\italic \"vib.\"", "a'4_\\markup \\italic \"dim.\""])
      numeric marked (Line "right" (bend [(0,0),(1,1)] (n 0) <> addVibrato (1/4) (n 2) <> swell [(0,1),(1,0)] (n 4) <> bend [(0,0),(1/2,1),(1,0)] (n 5)))
        === Right "right: 0/ 2w 4> 5^\n"
      page <- evalEither (numericPage marked perf (Line "right" (addVibrato (1/4) (n 2))))
      plainPage <- evalEither (numericPage plain perf (Line "right" (addVibrato (1/4) (n 2))))
      assert (not ("vibrato" `isInfixOf` plainPage) && "right: 2" `isInfixOf` plainPage)
      assert ("w = vibrato" `isInfixOf` page && not ("/ = pitch" `isInfixOf` page)))
  , ("cents: a fine offset under the steps, in every view", withTests 1 $ property $ do
      -- 100 cents is one 12-EDO step; offsets add; transposing keeps them; inverting mirrors them.
      a <- evalEither (render perf (Line "right" (detune 100 (n 0))))
      b <- evalEither (render perf (Line "right" (n 1)))
      assert (and [abs (A.hz u / A.hz v - 1) < 1e-12 | (u, v) <- zip (A.notes a) (A.notes b)])
      map cents (detune 5 (detune (-19) (n 4))) === [-14]
      map cents (transpose 7 (detune 3 (n 0))) === [3]
      [(pitches e, cents e) | e <- invert (detune 3 (n 4))] === [([A.Step (-4)], -3)]
      map cents (detune 9 rest) === [0]
      doc <- evalEither (parseDocument (header++"right: 0:4 & cents -14 [4:4] & cents 1/2 [7:4]\n"))
      (_, es) <- evalEither (flatten (score doc))
      sort (map (cents . event) es) === [-14, 0, 1/2]
      text <- evalEither (prettyScore (score doc))
      again <- evalEither (parseDocument (header++text))
      (_, rs) <- evalEither (flatten (score again))
      sort [cents (event e) | e <- rs, not (null (pitches (event e)))] === [-14, 0, 1/2]
      -- MIDI: the key is the step, the offset a constant bend on its own channel.
      half <- evalEither (midi perf (Line "right" (detune 50 (n 0))))
      assert (BS.pack [0,0xb0,6,1] `BS.isInfixOf` BL.toStrict half)
      assert (BS.pack [0,0xe0,0,96,0,0x90,60,127] `BS.isInfixOf` BL.toStrict half)
      numeric plain (Line "right" (detune (-14) (n 4) <> n 7)) === Right "right: 4-14c 7\n"
      sheet <- evalEither (engrave perf view (Line "right" (detune (-14) (n 4))))
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
      tempoNodes (score held) === [(0, 2, Speed (1/3))]
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
  , ("LilyPond golden views", withTests 1 $ property $ do
      mapM_ (\name -> do
        source <- evalIO (readFile ("examples/neomusic/"++name++".neomusic"))
        doc <- evalEither (parseDocument source)
        actual <- evalEither (engrave (performance doc) (sheetView doc) (score doc))
        expected <- evalIO (readFile ("test/golden/neomusic/"++name++".ly"))
        actual === expected) ["scale","twinkle","voices","ties","tempo","curves","cents"])
  ]
