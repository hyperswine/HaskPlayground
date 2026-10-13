-- | Unitless music. Interpretation is supplied once, outside the score tree.
module NeoMusic.Score
  ( Event(..), Phrase, Name, Score(..), Shape(..), Instrument(..), Performance(..)
  , steady, swung, speedOf, swingOf, shapeSource
  , TimeMap, timeMap, timePieces, performed, tempoNodes, hold
  , module NeoMusic.Signal
  , addPitch, scaleLevel, glide, vel, pitchParts, levelParts, bendMotion, swellMotion, vibrates
  , BeatEvent(..), n, chord, rest, hit, hush, transpose, invert, retro, stretch
  , withRhythm, withGate, scaleGate, staccato, pedal
  , mapPhrases, scoreLength, flatten, validate, render, writeAudio
  , defaultInstrument, defaultPerformance
  ) where

import Control.Monad (unless, forM_)
import qualified Data.Map.Strict as M
import Data.Ratio (denominator, numerator)
import qualified NeoMusic.Audio as A
import NeoMusic.Pitch (Step(..), Tuning(..), freq)
import NeoMusic.Signal

type Name = String
-- | 'dur' is the time the event occupies; the next event starts after it.
-- 'gate' is how long it sounds, as a ratio of 'dur': 1 is as written, below 1
-- is detached (staccato), above 1 rings into what follows (legato, pedal).
-- Because the gate is a ratio, 'stretch' leaves articulation unchanged.
--
-- Two signals shape a note while it sounds: 'pitchShift' is added to every
-- pitch, in steps ('Cents' parts in cents), and 'level' multiplies its
-- loudness. Pitch signals add and level signals multiply, so nesting composes
-- exactly. Rests have level 0 and ignore both.
data Event = Event
  { dur :: Rational, gate :: Rational, pitches :: [Step]
  , pitchShift :: Signal, level :: Signal }
  deriving (Eq, Show)

type Phrase = [Event]
-- | 'Tempo' says how fast its music is performed relative to the written beats.
-- It acts on everything sounding during its written extent, in every voice:
-- the score has one shared clock (see "neomusic-timemap.md").
data Score = Line Name Phrase | Silence Rational | Score :>>: Score | Score :||: Score
  | Tempo Shape Score
  deriving (Eq, Show)

-- | A tempo factor over a node's written extent: 1 is as written, 1/2 half speed.
-- Every shape is piecewise log-linear (equal tempo ratio per beat), so nested
-- shapes multiply into the same family. See neomusic-cycle.md.
data Shape
  = Ramp Rational Rational        -- ^ from factor a at the start to b at the end (a == b: constant)
  | Cycle [(Rational, Shape)]     -- ^ these segments (written beats, shape), repeated from the start
  deriving (Eq, Show)

-- | A constant speed.
steady :: Rational -> Shape
steady r = Ramp r r

-- | Swing: in each period of @p@ beats, the first half takes share @s@ of the time.
swung :: Rational -> Rational -> Shape
swung p s = Cycle [(p / 2, steady (1 / (2 * s))), (p / 2, steady (1 / (2 * (1 - s))))]

-- | A shape as the argument of @speed@ in .neomusic source.
shapeSource :: Shape -> String
shapeSource (Ramp a b) = if a == b then fraction a else fraction a ++ " -> " ++ fraction b
shapeSource (Cycle segs) = "cycle {" ++ unwords [fraction l ++ ":" ++ segment sub | (l, sub) <- segs] ++ "}"
  where segment sub = if speedOf sub /= Nothing then shapeSource sub else "(" ++ shapeSource sub ++ ")"

fraction :: Rational -> String
fraction x = show (numerator x) ++ (if denominator x == 1 then "" else "/" ++ show (denominator x))

-- | The factor of a constant speed.
speedOf :: Shape -> Maybe Rational
speedOf (Ramp a b) | a == b = Just a
speedOf _ = Nothing

-- | Period and long share of a swing: two equal constant halves whose times
-- add up to the period.
swingOf :: Shape -> Maybe (Rational, Rational)
swingOf (Cycle [(h1, Ramp a a'), (h2, Ramp b b')])
  | h1 == h2 && a == a' && b == b' && a > 0 && b > 0 && 1 / a + 1 / b == 2
  = Just (2 * h1, 1 / (2 * a))
swingOf _ = Nothing
infixr 5 :>>:
infixr 4 :||:
instance Semigroup Score where (<>) = (:>>:)
instance Monoid Score where mempty = Silence 0

data Instrument = Instrument
  { tuning :: Tuning, timbre :: A.Patch, gain :: Double, pan :: Double, send :: Double }
  deriving (Eq, Show)
data Performance = Performance
  { bpm :: Double, instruments :: M.Map Name Instrument, mix :: A.Mix }
  deriving (Eq, Show)
data BeatEvent = BeatEvent { onset :: Rational, instrument :: Name, event :: Event }
  deriving (Eq, Show)

defaultInstrument :: Instrument
defaultInstrument = Instrument (Tuning (440 * 2 ** (-9/12)) (2 ** (1/12))) A.pianoPatch 1 0 0
defaultPerformance :: Performance
defaultPerformance = Performance 120 M.empty (A.defaultMix 44100)

n :: Int -> Phrase
n x = chord [x]
chord :: [Int] -> Phrase
chord xs = hit xs 1 1
rest :: Phrase
rest = hush 1
hit :: [Int] -> Rational -> Double -> Phrase
hit xs d v = [Event d 1 (map Step xs) (Const 0) (Const (toRational v))]
hush :: Rational -> Phrase
hush d = [Event d 1 [] (Const 0) (Const 0) | d /= 0]
transpose :: Int -> Phrase -> Phrase
transpose k = map (\e -> e {pitches = [Step (x+k) | Step x <- pitches e]})
invert :: Phrase -> Phrase
-- Mirroring a pitch around 0 mirrors its offset too.
invert = map (\e -> e {pitches = [Step (negate x) | Step x <- pitches e], pitchShift = negateSignal (pitchShift e)})
retro :: Phrase -> Phrase
retro = reverse
stretch :: Rational -> Phrase -> Phrase
stretch k = map (\e -> e {dur = k * dur e})
-- | Set every event's gate, replacing whatever was there.
withGate :: Rational -> Phrase -> Phrase
withGate g = map (\e -> e {gate = g})
-- | Multiply every event's gate: @scaleGate a . scaleGate b = scaleGate (a*b)@.
scaleGate :: Rational -> Phrase -> Phrase
scaleGate k = map (\e -> e {gate = k * gate e})
-- | Detached: each note sounds for half its written length.
staccato :: Phrase -> Phrase
staccato = scaleGate (1/2)

-- | Add a pitch signal to every note: @addPitch a . addPitch b = addPitch (a + b)@.
addPitch :: Signal -> Phrase -> Phrase
addPitch sig = map (\e -> if null (pitches e) then e else e {pitchShift = plus sig (pitchShift e)})

-- | Multiply every note's level: @scaleLevel a . scaleLevel b = scaleLevel (a * b)@.
scaleLevel :: Signal -> Phrase -> Phrase
scaleLevel sig = map (\e -> if null (pitches e) then e else e {level = times sig (level e)})

-- | A pitch signal's constant steps, constant cents, and varying rest.
pitchParts :: Signal -> (Rational, Rational, Signal)
pitchParts sig = (sum steps, sum cs, foldr plus (Const 0) varying)
  where
    parts = terms sig
    steps = [k | t <- parts, Just k <- [constantValue t]]
    cs = [k | Cents t <- parts, Just k <- [constantValue t]]
    varying = [t | t <- parts, not (isConstantTerm t)]
    isConstantTerm t = case t of
      Cents a -> constantValue a /= Nothing
      _ -> constantValue t /= Nothing

-- | A level's constant part (the note's strength) and its varying rest.
levelParts :: Signal -> (Rational, Signal)
levelParts sig = (product [k | Just k <- map constantValue fs], foldr times (Const 1) [f | f <- fs, constantValue f == Nothing])
  where fs = factors sig

-- | The note's strength, its level's constant part: MIDI velocity, and what
-- velocity-to-cutoff responds to.
vel :: Event -> Double
vel = fromRational . fst . levelParts . level

-- | The pitch signal's shape: up, down, or away and back (oscillation ignored).
bendMotion :: Event -> Motion
bendMotion = motion . pitchShift

-- | The varying level's shape: growing, fading, or both.
swellMotion :: Event -> Motion
swellMotion = motion . snd . levelParts . level

-- | Whether the pitch oscillates (vibrato, trill).
vibrates :: Event -> Bool
vibrates = oscillates . pitchShift

-- | Portamento: each single-pitch note slides to the pitch of the note that
-- follows it directly (same instrument, single pitch, starting exactly when
-- its slot ends), over the last fraction @f@ of its sounding time. Notes with
-- no such successor, chords and rests are unchanged.
glide :: Rational -> Score -> Score
glide f score = fst (go 0 score)
  where
    starts = M.fromListWith (++) [((instrument e, onset e), [pitches (event e)]) | e <- either (const []) snd (flatten score)]
    target name t = case M.lookup (name, t) starts of
      Just [[Step y]] -> Just y
      _ -> Nothing
    slide name t e = case pitches e of
      [Step x] | Just y <- target name (t + dur e), y /= x ->
        e {pitchShift = plus (Points [(0, 0), (1 - f, 0), (1, fromIntegral (y - x))]) (pitchShift e)}
      _ -> e
    go at (Line name xs) =
      (Line name (zipWith (slide name) (scanl (+) at (map dur xs)) xs), at + sum (map dur xs))
    go at (Silence d) = (Silence d, at + d)
    go at (a :>>: b) = let (a', ta) = go at a; (b', tb) = go ta b in (a' :>>: b', tb)
    go at (a :||: b) = let (a', ta) = go at a; (b', tb) = go at b in (a' :||: b', max ta tb)
    go at (Tempo shape s) = let (s', t) = go at s in (Tempo shape s', t)

-- | A fermata: the music inside is held @k@ times as long ('steady' (1/k)).
-- The staff view prints a fermata when this covers a single onset.
hold :: Rational -> Score -> Score
hold k = Tempo (steady (1 / k))

-- | Sustain pedal held down for the whole score: every note rings until the
-- score's end. Onsets and lengths are unchanged; only gates grow. Rests stay rests.
pedal :: Score -> Score
pedal score = fst (go 0 score)
  where
    end = scoreLength score
    go at (Line name xs) =
      let onsets = scanl (+) at (map dur xs)
          held t e = if null (pitches e) then e else e {gate = max (gate e) ((end - t) / dur e)}
      in (Line name (zipWith held onsets xs), at + sum (map dur xs))
    go at (Silence d) = (Silence d, at + d)
    go at (a :>>: b) = let (a', ta) = go at a; (b', tb) = go ta b in (a' :>>: b', tb)
    go at (a :||: b) = let (a', ta) = go at a; (b', tb) = go at b in (a' :||: b', max ta tb)
    go at (Tempo shape s) = let (s', t) = go at s in (Tempo shape s', t)

-- | Length in beats, from durations (gates never change it).
scoreLength :: Score -> Rational
scoreLength (Line _ xs) = sum (map dur xs)
scoreLength (Silence d) = d
scoreLength (a :>>: b) = scoreLength a + scoreLength b
scoreLength (a :||: b) = max (scoreLength a) (scoreLength b)
scoreLength (Tempo _ s) = scoreLength s

withRhythm :: [Rational] -> Phrase -> Either String Phrase
withRhythm rs xs
  | null rs || any (<=0) rs = Left "rhythm pattern must be nonempty and positive"
  | otherwise = Right (zipWith (\d e -> e {dur = d}) (cycle rs) xs)
mapPhrases :: (Phrase -> Phrase) -> Score -> Score
mapPhrases f = go
  where
    go (Line name xs) = Line name (f xs)
    go (Silence d) = Silence d
    go (a :>>: b) = go a :>>: go b
    go (a :||: b) = go a :||: go b
    go (Tempo shape s) = Tempo shape (go s)

-- | Exact beat onsets, shared by audio, MIDI and engraving. Rests remain events.
flatten :: Score -> Either String (Rational, [BeatEvent])
flatten = go 0
  where
    go at (Line name xs) = do
      unless (all ((> 0) . dur) xs) (Left "events need positive durations")
      unless (all ((> 0) . gate) xs) (Left "gates must be positive")
      unless (all (\e -> validPoints (pitchShift e) && validPoints (level e)) xs)
        (Left "signal points need u within 0..1, in nondecreasing order")
      unless (all (not . hasCents . level) xs) (Left "cents only make sense in a pitch")
      -- The constant part is the strength (0..1); the whole level never goes below 0.
      unless (all (\e -> let (k, _) = levelParts (level e) in k >= 0 && k <= 1) xs)
        (Left "a note's level must have a constant part within 0..1")
      unless (all (\e -> fst (signalBounds (snd (levelParts (level e)))) >= 0) xs)
        (Left "a level cannot go below 0")
      pure (sum (map dur xs), zipWith (\t e -> BeatEvent t name e) (scanl (+) at (map dur xs)) xs)
    go _ (Silence d)
      | d < 0 = Left "silence cannot have negative duration"
      | otherwise = Right (d, [])
    go at (a :>>: b) = do
      (da, na) <- go at a
      (db, nb) <- go (at+da) b
      pure (da+db, na++nb)
    go at (a :||: b) = do
      (da, na) <- go at a
      (db, nb) <- go at b
      pure (max da db, na++nb)
    go at (Tempo shape s) = validShape shape >> go at s
    validShape (Ramp a b) = unless (a > 0 && b > 0) (Left "tempo factors must be positive")
    validShape (Cycle segs) = do
      unless (not (null segs) && all ((> 0) . fst) segs)
        (Left "a tempo cycle needs segments of positive length")
      mapM_ (validShape . snd) segs

-- | Every tempo node with its written start and length, outermost first.
tempoNodes :: Score -> [(Rational, Rational, Shape)]
tempoNodes = snd . go 0
  where
    go _ (Line _ xs) = (sum (map dur xs), [])
    go _ (Silence d) = (d, [])
    go at (a :>>: b) = let (da, xa) = go at a; (db, xb) = go (at + da) b in (da + db, xa ++ xb)
    go at (a :||: b) = let (da, xa) = go at a; (db, xb) = go at b in (max da db, xa ++ xb)
    go at (Tempo shape s) = let (d, xs) = go at s in (d, (at, d, shape) : xs)

-- | The shared clock: written beats to performed beats. Pieces are keyed by
-- written start: end, log factor at the start, log slope per beat, and the
-- performed time at the start. Outside every piece the factor is 1.
newtype TimeMap = TimeMap (M.Map Rational (Rational, Double, Double, Double))
  deriving (Eq, Show)

timeMap :: Score -> TimeMap
timeMap score = TimeMap (M.fromList (scan 0 0 merged))
  where
    pieces = concatMap nodePieces (tempoNodes score)
    nodePieces (t0, len, shape) = filter (\(u, v, _, _) -> u < v) (shapePieces t0 len (t0 + len) shape)
    -- A shape spanning [start, start + len), cut off at @clip@. A cycle lays
    -- its segments end to end and repeats them; each segment's shape spans
    -- the segment.
    shapePieces start len clip shape = case shape of
      Ramp a b -> [(start, min (start + len) clip, ln a, ln (b / a) / fromRational len) | len > 0]
      Cycle segs ->
        let end = min (start + len) clip
            period = sum (map fst segs)
        in concat [ shapePieces (t + offset) l end sub
                  | period > 0, t <- takeWhile (< end) [start, start + period ..]
                  , (offset, (l, sub)) <- zip (scanl (+) 0 (map fst segs)) segs, t + offset < end ]
    ln = log . fromRational
    breaks = M.keys (M.fromList [(x, ()) | (u, v, _, _) <- pieces, x <- [u, v]])
    -- Factors multiply where nodes overlap, so log factors and slopes add.
    merged = [ (x0, x1, sum [la + k * fromRational (x0 - u) | (u, _, la, k) <- covering], sum [k | (_, _, _, k) <- covering])
             | (x0, x1) <- zip breaks (drop 1 breaks)
             , let covering = [p | p@(u, v, _, _) <- pieces, u <= x0, x1 <= v]
             , not (null covering) ]
    scan _ _ [] = []
    scan prevEnd prevPerf ((x0, x1, la, k) : more) =
      let start = prevPerf + fromRational (x0 - prevEnd)
      in (x0, (x1, la, k, start)) : scan x1 (start + logLinear la k (fromRational (x1 - x0))) more

-- | Performed beats taken by @x@ written beats of factor @exp (la + k t)@.
logLinear :: Double -> Double -> Double -> Double
logLinear la k x
  | k == 0 = x * exp (negate la)
  | otherwise = exp (negate la) * (1 - exp (negate k * x)) / k

-- | The clock's pieces: written start, written end, log slope per beat (0 = constant).
timePieces :: TimeMap -> [(Rational, Rational, Double)]
timePieces (TimeMap m) = [(u, v, k) | (u, (v, _, k, _)) <- M.toList m]

performed :: TimeMap -> Rational -> Double
performed (TimeMap m) b = case M.lookupLE b m of
  Nothing -> fromRational b
  Just (u, (v, la, k, start))
    | b < v -> start + logLinear la k (fromRational (b - u))
    | otherwise -> start + logLinear la k (fromRational (v - u)) + fromRational (b - v)

-- | Check a performance against a score without rendering it: tempo, every
-- declared instrument, and that every line names a declared instrument.
validate :: Performance -> Score -> Either String ()
validate performance score = do
  _ <- flatten score
  unless (positive (bpm performance)) (Left "tempo must be finite and positive")
  forM_ (M.toList (instruments performance)) $ \(name, i) -> do
    let Tuning base r = tuning i
        context message = "instrument " ++ name ++ ": " ++ message
    unless (positive base && positive r) (Left (context "tuning must be finite and positive"))
    either (Left . context) Right (A.validPatch (timbre i))
    unless (finite (gain i) && gain i >= 0) (Left (context "gain must be finite and nonnegative"))
    unless (finite (pan i) && abs (pan i) <= 1) (Left (context "pan must be within -1..1"))
    unless (finite (send i) && send i >= 0 && send i <= 1) (Left (context "reverb send must be within 0..1"))
  forM_ (lineNames score) $ \name ->
    unless (M.member name (instruments performance)) (Left ("unknown instrument: " ++ name))
  where
    lineNames (Line name _) = [name]
    lineNames (Silence _) = []
    lineNames (a :>>: b) = lineNames a ++ lineNames b
    lineNames (a :||: b) = lineNames a ++ lineNames b
    lineNames (Tempo _ s) = lineNames s

-- | Beats become seconds here, once, for the whole score.
render :: Performance -> Score -> Either String A.Timeline
render performance score = do
  validate performance score
  (total,es) <- flatten score
  let clock = timeMap score
      seconds beats = performed clock beats * (60 / bpm performance)
      -- Constant parts become the note's frequency and strength; only the
      -- varying parts reach the engine's per-sample path.
      ns = [ A.Note (seconds (onset e)) hz
               (seconds (onset e + dur ev * gate ev) - seconds (onset e))
               (timbre i) (fromRational strength) (gain i) (pan i) (send i) pitchMod loud
           | e <- es, let ev = event e, let i = instruments performance M.! instrument e, step <- pitches ev
           , let octavesPerStep = logBase 2 (ratio (tuning i))
                 stepsPerCent = 1 / (1200 * octavesPerStep)
                 soundingBeats = fromRational (dur ev * gate ev)
                 (constSteps, constCents, varyingPitch) = pitchParts (pitchShift ev)
                 (strength, varyingLevel) = levelParts (level ev)
                 base = freq (tuning i) step * 2 ** (fromRational constCents / 1200)
                 hz = if constSteps == 0 then base else base * ratio (tuning i) ** fromRational constSteps
                 pitchMod = if varyingPitch == Const 0 then A.Flat 0
                            else A.Flat octavesPerStep A.:*. contour stepsPerCent soundingBeats varyingPitch
                 loud = if varyingLevel == Const 1 then A.Flat 1 else contour 0 soundingBeats varyingLevel ]
  unless (all (\v -> finite (A.start v) && positive (A.duration v) && positive (A.hz v)) ns
          && finite (seconds total)) (Left "score time or pitch cannot be represented in seconds and Hz")
  pure (A.Timeline (seconds total) ns)

-- | A signal as the engine's contour: oscillator rates become cycles over the
-- note, and 'Cents' parts become steps.
contour :: Double -> Double -> Signal -> A.Contour
contour stepsPerCent beats sig = case sig of
  Const k -> A.Flat (fromRational k)
  Points ps -> A.Breaks [(fromRational u, fromRational y) | (u, y) <- ps]
  Osc w r -> A.Cycles (case w of Sine -> A.SineWave; Square -> A.SquareWave) (fromRational r * beats)
  a :+: b -> contour stepsPerCent beats a A.:+. contour stepsPerCent beats b
  a :*: b -> contour stepsPerCent beats a A.:*. contour stepsPerCent beats b
  Cents a -> A.Flat stepsPerCent A.:*. contour stepsPerCent beats a

finite, positive :: Double -> Bool
finite x = not (isNaN x || isInfinite x)
positive x = finite x && x > 0

writeAudio :: FilePath -> Performance -> Score -> IO ()
writeAudio path performance score = do
  timeline <- either (ioError . userError) pure (render performance score)
  A.writeTimelineMix path (mix performance) timeline
