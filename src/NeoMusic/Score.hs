-- | Unitless music. Interpretation is supplied once, outside the score tree.
module NeoMusic.Score
  ( Event(..), Phrase, Name, Score(..), Instrument(..), Performance(..)
  , BeatEvent(..), n, chord, rest, hit, hush, transpose, invert, retro, stretch
  , withRhythm, withVelocity, scaleVelocity, mapPhrases, flatten, validate, render, writeAudio
  , defaultInstrument, defaultPerformance
  ) where

import Control.Monad (unless, forM_)
import qualified Data.Map.Strict as M
import qualified NeoMusic.Audio as A
import NeoMusic.Pitch (Step(..), Tuning(..), freq)

type Name = String
data Event = Event { dur :: Rational, vel :: Double, pitches :: [Step] }
  deriving (Eq, Show)
type Phrase = [Event]
data Score = Line Name Phrase | Silence Rational | Score :>>: Score | Score :||: Score
  deriving (Eq, Show)
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
hit xs d v = [Event d v (map Step xs)]
hush :: Rational -> Phrase
hush d = [Event d 0 [] | d /= 0]
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

-- | Exact beat onsets, shared by audio, MIDI and engraving. Rests remain events.
flatten :: Score -> Either String (Rational, [BeatEvent])
flatten = go 0
  where
    go at (Line name xs) = do
      unless (all (\e -> dur e > 0 && finite (vel e) && vel e >= 0 && vel e <= 1) xs)
        (Left "events need positive durations and finite velocities within 0..1")
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

-- | Beats become seconds here, once, for the whole score.
render :: Performance -> Score -> Either String A.Timeline
render performance score = do
  validate performance score
  (total,es) <- flatten score
  let seconds beats = fromRational beats * (60 / bpm performance)
      ns = [ A.Note (seconds (onset e)) (freq (tuning i) pitch) (seconds (dur (event e)))
               (timbre i) (vel (event e)) (gain i) (pan i) (send i)
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
