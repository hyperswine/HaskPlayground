-- | Unitless music. Interpretation is supplied once, outside the score tree.
module NeoMusic.Score
  ( Event(..), Phrase, Name, Score(..), Shape(..), Instrument(..), Performance(..)
  , TimeMap, timeMap, timePieces, performed, tempoNodes, hold
  , Curve, curveValue, bend, swell, addVibrato, glide, Motion(..), bendMotion, swellMotion
  , BeatEvent(..), n, chord, rest, hit, hush, transpose, invert, retro, stretch
  , withRhythm, withVelocity, scaleVelocity, withGate, scaleGate, staccato, pedal
  , mapPhrases, scoreLength, flatten, validate, render, writeAudio
  , defaultInstrument, defaultPerformance
  ) where

import Control.Monad (unless, forM_)
import qualified Data.Map.Strict as M
import qualified NeoMusic.Audio as A
import NeoMusic.Pitch (Step(..), Tuning(..), freq)

type Name = String
-- | 'dur' is the time the event occupies; the next event starts after it.
-- 'gate' is how long it sounds, as a ratio of 'dur': 1 is as written, below 1
-- is detached (staccato), above 1 rings into what follows (legato, pedal).
-- Because the gate is a ratio, 'stretch' leaves articulation unchanged.
--
-- Curves shape a note while it sounds, over its sounding time @u@ from 0 to 1:
-- 'bends' offset the pitch in (fractional) steps and add together; 'swells'
-- scale the loudness and multiply together; 'vibrato' is a depth in steps,
-- whose rate in Hz belongs to the instrument. Rests ignore all three.
data Event = Event
  { dur :: Rational, vel :: Double, pitches :: [Step], gate :: Rational
  , bends :: [Curve], swells :: [Curve], vibrato :: Rational }
  deriving (Eq, Show)

-- | Piecewise-linear breakpoints @(u, value)@ with @u@ in 0..1, nondecreasing;
-- flat before the first point and after the last.
type Curve = [(Rational, Rational)]

curveValue :: Curve -> Rational -> Rational
curveValue [] _ = 0
curveValue ((u0, y0) : more) u
  | u <= u0 = y0
  | otherwise = go u0 y0 more
  where
    go _ y [] = y
    go ua ya ((ub, yb) : later)
      | u <= ub = if ub == ua then yb else ya + (yb - ya) * (u - ua) / (ub - ua)
      | otherwise = go ub yb later
type Phrase = [Event]
-- | 'Tempo' says how fast its music is performed relative to the written beats.
-- It acts on everything sounding during its written extent, in every voice:
-- the score has one shared clock (see "neomusic-timemap.md").
data Score = Line Name Phrase | Silence Rational | Score :>>: Score | Score :||: Score
  | Tempo Shape Score
  deriving (Eq, Show)

-- | A tempo factor over a node's written extent: 1 is as written, 1/2 half speed.
-- Every shape is piecewise log-linear (equal tempo ratio per beat), so nested
-- shapes multiply into the same family.
data Shape
  = Speed Rational             -- ^ constant factor
  | Ramp Rational Rational     -- ^ from factor a at the start to b at the end
  | Swing Rational Rational    -- ^ period p beats; the first half of each period takes share s
  deriving (Eq, Show)
infixr 5 :>>:
infixr 4 :||:
instance Semigroup Score where (<>) = (:>>:)
instance Monoid Score where mempty = Silence 0

data Instrument = Instrument
  { tuning :: Tuning, timbre :: A.Patch, gain :: Double, pan :: Double, send :: Double
  , vibratoRate :: Double  -- ^ Hz; the score only says how deep
  }
  deriving (Eq, Show)
data Performance = Performance
  { bpm :: Double, instruments :: M.Map Name Instrument, mix :: A.Mix }
  deriving (Eq, Show)
data BeatEvent = BeatEvent { onset :: Rational, instrument :: Name, event :: Event }
  deriving (Eq, Show)

defaultInstrument :: Instrument
defaultInstrument = Instrument (Tuning (440 * 2 ** (-9/12)) (2 ** (1/12))) A.pianoPatch 1 0 0 5.5
defaultPerformance :: Performance
defaultPerformance = Performance 120 M.empty (A.defaultMix 44100)

n :: Int -> Phrase
n x = chord [x]
chord :: [Int] -> Phrase
chord xs = hit xs 1 1
rest :: Phrase
rest = hush 1
hit :: [Int] -> Rational -> Double -> Phrase
hit xs d v = [Event d v (map Step xs) 1 [] [] 0]
hush :: Rational -> Phrase
hush d = [Event d 0 [] 1 [] [] 0 | d /= 0]
transpose :: Int -> Phrase -> Phrase
transpose k = map (\e -> e {pitches = [Step (x+k) | Step x <- pitches e]})
invert :: Phrase -> Phrase
invert = map (\e -> e {pitches = [Step (negate x) | Step x <- pitches e]})
retro :: Phrase -> Phrase
retro = reverse
stretch :: Rational -> Phrase -> Phrase
stretch k = map (\e -> e {dur = k * dur e})
-- | Set every event's velocity, replacing whatever was there.
withVelocity :: Double -> Phrase -> Phrase
withVelocity v = map (\e -> e {vel = v})
-- | Multiply every event's velocity: @scaleVelocity a . scaleVelocity b = scaleVelocity (a*b)@,
-- so nested levels compose and relative accents inside keep their proportions.
scaleVelocity :: Double -> Phrase -> Phrase
scaleVelocity k = map (\e -> e {vel = k * vel e})
-- | Set every event's gate, replacing whatever was there.
withGate :: Rational -> Phrase -> Phrase
withGate g = map (\e -> e {gate = g})
-- | Multiply every event's gate: @scaleGate a . scaleGate b = scaleGate (a*b)@.
scaleGate :: Rational -> Phrase -> Phrase
scaleGate k = map (\e -> e {gate = k * gate e})
-- | Detached: each note sounds for half its written length.
staccato :: Phrase -> Phrase
staccato = scaleGate (1/2)

-- | Add a pitch curve, in steps, to every note: bends add, so nesting sums.
bend :: Curve -> Phrase -> Phrase
bend c = map (\e -> if null (pitches e) then e else e {bends = c : bends e})

-- | Add a loudness curve to every note: swells multiply.
swell :: Curve -> Phrase -> Phrase
swell c = map (\e -> if null (pitches e) then e else e {swells = c : swells e})

-- | Add vibrato depth, in steps, to every note.
addVibrato :: Rational -> Phrase -> Phrase
addVibrato d = map (\e -> if null (pitches e) then e else e {vibrato = vibrato e + d})

-- | How a note's curves move overall, for the reading views.
data Motion = Flat | Rising | Falling | Returning deriving (Eq, Show)

motion :: (Rational -> Rational) -> [Rational] -> Motion
motion at us
  | end > begin = Rising
  | end < begin = Falling
  | any ((/= begin) . at) us = Returning
  | otherwise = Flat
  where begin = at 0; end = at 1

-- | The summed bend: up, down, or away and back.
bendMotion :: Event -> Motion
bendMotion e = motion (\u -> sum [curveValue c u | c <- bends e]) [u | c <- bends e, (u, _) <- c]

-- | The product of the swells: growing, fading, or rising and falling.
swellMotion :: Event -> Motion
swellMotion e
  | null (swells e) = Flat
  | otherwise = motion (\u -> product [curveValue c u | c <- swells e]) [u | c <- swells e, (u, _) <- c]

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
        e {bends = [(0, 0), (1 - f, 0), (1, fromIntegral (y - x))] : bends e}
      _ -> e
    go at (Line name xs) =
      (Line name (zipWith (slide name) (scanl (+) at (map dur xs)) xs), at + sum (map dur xs))
    go at (Silence d) = (Silence d, at + d)
    go at (a :>>: b) = let (a', ta) = go at a; (b', tb) = go ta b in (a' :>>: b', tb)
    go at (a :||: b) = let (a', ta) = go at a; (b', tb) = go at b in (a' :||: b', max ta tb)
    go at (Tempo shape s) = let (s', t) = go at s in (Tempo shape s', t)

-- | A fermata: the music inside is held @k@ times as long ('Speed' (1/k)).
-- The staff view prints a fermata when this covers a single onset.
hold :: Rational -> Score -> Score
hold k = Tempo (Speed (1 / k))

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
      unless (all (\e -> dur e > 0 && finite (vel e) && vel e >= 0 && vel e <= 1) xs)
        (Left "events need positive durations and finite velocities within 0..1")
      unless (all ((> 0) . gate) xs) (Left "gates must be positive")
      let curveOk c = all (\(u, _) -> u >= 0 && u <= 1) c && and (zipWith (<=) (map fst c) (drop 1 (map fst c)))
      unless (all (\e -> all curveOk (bends e) && all curveOk (swells e)) xs)
        (Left "curve points need u within 0..1, in nondecreasing order")
      unless (all (\e -> all (all ((>= 0) . snd)) (swells e) && vibrato e >= 0) xs)
        (Left "swells and vibrato depth cannot be negative")
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
    validShape (Speed r) = unless (r > 0) (Left "speed must be positive")
    validShape (Ramp a b) = unless (a > 0 && b > 0) (Left "tempo ramp factors must be positive")
    validShape (Swing p sh) = unless (p > 0 && sh > 0 && sh < 1)
      (Left "swing needs a positive period and a long share strictly between 0 and 1")

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
    nodePieces (t0, len, shape) = filter (\(u, v, _, _) -> u < v) $ case shape of
      Speed r -> [(t0, t0 + len, ln r, 0)]
      Ramp a b -> [(t0, t0 + len, ln a, ln (b / a) / fromRational len) | len > 0]
      Swing p sh ->
        let end = t0 + len
        in concat [ [(t, min (t + p / 2) end, ln (1 / (2 * sh)), 0), (t + p / 2, min (t + p) end, ln (1 / (2 * (1 - sh))), 0)]
                  | t <- takeWhile (< end) [t0, t0 + p ..] ]
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
    unless (positive (vibratoRate i)) (Left (context "vibrato rate must be finite and positive"))
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
      octaves i = logBase 2 (ratio (tuning i))
      -- Bends add, so they merge exactly into one curve over all their breakpoints.
      merged cs = [ (fromRational u, fromRational (sum [curveValue c u | c <- cs]))
                  | u <- M.keys (M.fromList [(u, ()) | c <- cs, (u, _) <- c]) ]
      ns = [ A.Note (seconds (onset e)) (freq (tuning i) pitch)
               (seconds (onset e + dur (event e) * gate (event e)) - seconds (onset e))
               (timbre i) (vel (event e)) (gain i) (pan i) (send i)
               [(u, y * octaves i) | (u, y) <- merged (bends (event e))]
               (fromRational (vibrato (event e)) * octaves i) (vibratoRate i)
               [[(fromRational u, fromRational y) | (u, y) <- c] | c <- swells (event e)]
           | e <- es, let i = instruments performance M.! instrument e, pitch <- pitches (event e) ]
  unless (all (\v -> finite (A.start v) && positive (A.duration v) && positive (A.hz v)) ns
          && finite (seconds total)) (Left "score time or pitch cannot be represented in seconds and Hz")
  pure (A.Timeline (seconds total) ns)

finite, positive :: Double -> Bool
finite x = not (isNaN x || isInfinite x)
positive x = finite x && x > 0

writeAudio :: FilePath -> Performance -> Score -> IO ()
writeAudio path performance score = do
  timeline <- either (ioError . userError) pure (render performance score)
  A.writeTimelineMix path (mix performance) timeline
