-- | Piano engraving is a view over exact beat events, never audio samples.
module NeoMusic.Sheet (engrave, sheetEvents) where

import Control.Monad (unless)
import Data.List (intercalate, nub, sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Ratio (denominator, numerator)
import qualified NeoMusic.Audio as A
import NeoMusic.Language (SheetView (..), Staff (..))
import NeoMusic.Midi (midiPitch)
import qualified NeoMusic.Pitch as A
import NeoMusic.Score hiding (chord)

-- | Shared pre-engraving representation: staff, onset, written duration, MIDI
-- chord and the event itself (for its gate and curves).
sheetEvents :: Performance -> SheetView -> Score -> Either String [(Staff, Rational, Rational, [Int], Event)]
sheetEvents perf view score = do
  validate perf score
  (total, es) <- flatten score
  unless (meter view > 0) (Left "meter must be positive")
  unless (dyadic total) (Left "sheet supports durations on a 32nd-note grid; tuplets are not supported")
  mapM convert es
  where
    dyadic x = denominator (x * 8) == 1
    convert e = do
      i <- maybe (Left "missing instrument") Right (M.lookup (instrument e) (instruments perf))
      staff <- maybe (Left ("missing staff for " ++ instrument e)) Right (M.lookup (instrument e) (staves view))
      unless
        (A.pitchDrop (timbre i) == 0 && all ((/= A.NoiseWave) . A.wave) (A.oscillators (timbre i)))
        (Left "noise/pitch-drop percussion cannot be engraved without a staff mapping")
      -- Validate the tuning even for an all-rest instrument.
      _ <- midiPitch (tuning i) (A.Step 0)
      ps <- mapM (midiPitch (tuning i)) (pitches (event e))
      unless (all (\p -> p >= 21 && p <= 108) ps) (Left "pitch outside piano range A0..C8")
      unless (dyadic (onset e) && dyadic (dur (event e))) (Left "sheet supports durations on a 32nd-note grid; tuplets are not supported")
      pure (staff, onset e, dur (event e), ps, event e)

engrave :: Performance -> SheetView -> Score -> Either String String
engrave perf view score = do
  es <- sheetEvents perf view score
  (total, _) <- flatten score
  keyName <- maybe (Left "unsupported major key hint") Right (M.lookup (key view) keys)
  let staff name clef =
        let voices = separate (sortOn (\(_, t, _, _, _) -> t) [e | e@(s, _, _, ps, _) <- es, s == name, not (null ps)])
            content = if null voices then [emitRest 0 total] else map (emitVoice total) voices
         in "  \\new Staff { \\clef "
              ++ clef
              ++ " \\time "
              ++ show (meter view)
              ++ "/4 \\key "
              ++ keyName
              ++ " \\major\n"
              ++ (if null marks then "    " ++ metronome (bpm perf) ++ "\n" else "")
              ++ "    << "
              ++ intercalate " \\\\ " ["{ " ++ v ++ " }" | v <- content]
              ++ " >>\n  }\n"
      -- Tempo marks live on a spacer line between the staves, so they never
      -- split the notes; LilyPond prints them above the system.
      tempoLine = if null marks then "" else "  \\new Dynamics { " ++ spacers 0 marks ++ "}\n"
      spacers at [] = spacer (total - at)
      spacers at ((t, mark) : more) = spacer (t - at) ++ mark ++ " " ++ spacers t more
      spacer d
        | d <= 0 = ""
        | otherwise = "s4*" ++ show (numerator d) ++ (if denominator d == 1 then "" else "/" ++ show (denominator d)) ++ " "
  pure ("\\version \"2.24.3\"\n\\score {\n \\new PianoStaff <<\n" ++ staff Treble "treble" ++ tempoLine ++ staff Bass "bass" ++ " >>\n \\layout { }\n}\n")
  where
    metronome speed
      | speed == fromInteger (round speed) = "\\tempo 4 = " ++ show (round speed :: Integer)
      | otherwise = "\\tempo \"" ++ show speed ++ " BPM\""
    -- Only reached after sheetEvents succeeded in the do block above.
    events = either (const []) id (sheetEvents perf view score)
    nodes = [node | node@(_, len, _) <- tempoNodes score, len > 0]
    pitchedOnsets = nub (sort [t | (_, t, _, ps, _) <- events, not (null ps)])
    -- A constant slow-down covering exactly one onset, starting on it, is a fermata.
    isFermata (t0, len, Speed r) = r < 1 && [t | t <- pitchedOnsets, t0 <= t, t < t0 + len] == [t0]
    isFermata _ = False
    fermatas = [t0 | node@(t0, _, _) <- nodes, isFermata node]
    -- Inside a tempo change that began earlier (one starting here doesn't count).
    active x = any (\(t0, len, _) -> t0 < x && x < t0 + len) nodes
    lastBeat = maximum (0 : [t + d | (_, t, d, _, _) <- events])
    -- (beat, text, metronome factor), merged per beat into one \tempo each.
    rawMarks = concat
      [ (t0, startText shape, startFactor shape)
          : [(t0 + len, endText shape, Just 1) | t0 + len < lastBeat, not (active (t0 + len))]
      | node@(t0, len, shape) <- nodes, not (isFermata node) ]
    startText (Ramp a b)
      | b < a = Just "rit."
      | b > a = Just "accel."
    startText (Swing _ _) = Just "Swing"
    startText _ = Nothing
    startFactor (Speed r) = Just r
    startFactor (Ramp a b) | a == b = Just a
    startFactor _ = Nothing
    endText (Ramp _ _) = Just "a tempo"
    endText (Swing _ _) = Just "Straight"
    endText _ = Nothing
    marks
      | null rawMarks = []
      | otherwise =
          [ (t, tempoMark texts factor)
          | t <- nub (sort (0 : [u | (u, _, _) <- rawMarks]))
          , let here = [m | m@(u, _, _) <- rawMarks, u == t]
                texts = [x | (_, Just x, _) <- here]
                factor = case [f | (_, _, Just f) <- here] of
                  f : _ -> Just f
                  [] -> if t == 0 && not (any (\(u, len, _) -> u == 0 && len > 0) nodes) then Just 1 else Nothing ]
    -- \tempo "text" 4 = n, with exact BPM text when the speed is fractional.
    tempoMark texts factor =
      let speed = fmap (\f -> bpm perf * fromRational f) factor
          whole = maybe False (\x -> x == fromInteger (round x)) speed
          shown = texts ++ [show x ++ " BPM" | Just x <- [speed], not whole]
          text = if null shown then "" else "\\markup \\italic \"" ++ intercalate ", " shown ++ "\" "
          number = case speed of
            Just x | whole -> "4 = " ++ show (round x :: Integer)
            _ -> ""
      in "\\tempo " ++ text ++ number
    flats = key view `elem` ["F", "Bb", "Eb", "Ab", "Db", "Gb", "Cb"]
    keys =
      M.fromList
        ( zip
            ["C", "G", "D", "A", "E", "B", "F#", "C#", "F", "Bb", "Eb", "Ab", "Db", "Gb", "Cb"]
            ["c", "g", "d", "a", "e", "b", "fis", "cis", "f", "bes", "ees", "aes", "des", "ges", "ces"]
        )
    pitch p = (if flats then flatNames else sharpNames) !! (p `mod` 12) ++ octave (p `div` 12 - 4)
    sharpNames = ["c", "cis", "d", "dis", "e", "f", "fis", "g", "gis", "a", "ais", "b"]
    flatNames = ["c", "des", "d", "ees", "e", "f", "ges", "g", "aes", "a", "bes", "b"]
    octave k = replicate (abs k) (if k >= 0 then '\'' else ',')
    emitRest t d = emit True ("", "") t d []
    emitVoice total = go 0
      where
        go at [] = emitRest at (total - at)
        go at ((_, t, d, ps, ev) : xs) = emitRest at (t - at) ++ emit True (articulation ev t (continues (t + d) xs)) t d ps ++ go (t + d) xs
        continues t ((_, u, _, qs, _) : _) = u == t && not (null qs)
        continues _ [] = False
    -- The gate becomes an articulation on the note's first segment, or for a
    -- note that rings past its written length, a "let ring" tie on its last.
    -- Gate and fermata as before; a bend that ends elsewhere draws a glissando
    -- to the next note in the voice, any other bend, vibrato and swells are
    -- written as words or an espressivo.
    articulation ev t followed =
      let (front, back) = gateMarks (gate ev)
          sliding = bendMotion ev `elem` [Rising, Falling] && followed
          curveFront = concat
            [ if bendMotion ev /= Flat && not sliding then "^\\markup \\italic \"bend\"" else ""
            , if vibrato ev > 0 then "^\\markup \\italic \"vib.\"" else ""
            , case swellMotion ev of
                Rising -> "_\\markup \\italic \"cresc.\""
                Falling -> "_\\markup \\italic \"dim.\""
                Returning -> "\\espressivo"
                Flat -> "" ]
       in (front ++ (if t `elem` fermatas then "\\fermata" else "") ++ curveFront, back ++ (if sliding then "\\glissando" else ""))
    gateMarks g
      | g <= 1 / 2 = ("-.", "")
      | g < 1 = ("-_", "")
      | g > 1 = ("", "\\laissezVibrer")
      | otherwise = ("", "")
    emit _ _ _ 0 _ = ""
    emit first (front, back) at remaining ps =
      let bar = fromIntegral (meter view)
          boundary = (fromInteger (floor (at / bar)) + 1) * bar
          allowed = min remaining (boundary - at)
          choices = [(6, "1."), (4, "1"), (3, "2."), (2, "2"), (3 / 2, "4."), (1, "4"), (3 / 4, "8."), (1 / 2, "8"), (3 / 8, "16."), (1 / 4, "16"), (1 / 8, "32")]
          -- Prefer values aligned to their undotted beat unit for readable rhythm.
          fitting = [(value, s) | (value, s) <- choices, value <= allowed, denominator (at / base value) == 1]
          (d, spelling) = case fitting of x : _ -> x; [] -> (1 / 8, "32")
          base value = if value `elem` [6, 3, 3 / 2, 3 / 4, 3 / 8] then value * 2 / 3 else value
          chord = case ps of [] -> "r"; [p] -> pitch p; _ -> "<" ++ unwords (map pitch ps) ++ ">"
          tie = if not (null ps) && d < remaining then "~" else ""
          barline = if at + d == boundary then " | " else " "
          marked = (if first then front else "") ++ (if d == remaining then back else "")
       in chord ++ spelling ++ (if null ps then "" else marked) ++ tie ++ barline ++ emit False (front, back) (at + d) (remaining - d) ps

-- Greedy interval partition: each lane is monophonic; chord events stay intact.
separate :: [(Staff, Rational, Rational, [Int], Event)] -> [[(Staff, Rational, Rational, [Int], Event)]]
separate = foldl insert []
  where
    insert [] e = [[e]]
    insert (lane : lanes) e@(_, t, _, _, _) = case reverse lane of
      (_, a, d, _, _) : _ | a + d <= t -> (lane ++ [e]) : lanes
      _ -> lane : insert lanes e
