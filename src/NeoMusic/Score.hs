-- | Unitless music. Interpretation is supplied once, outside the score tree.
module NeoMusic.Score
  ( Event(..), Phrase, Name, Score(..), Instrument(..), Performance(..)
  , BeatEvent(..), n, chord, rest, hit, hush, transpose, invert, retro, stretch
  , withRhythm, withVelocity, mapPhrases, flatten, render, toPiece, writeAudio
  , defaultInstrument, defaultPerformance
  ) where

import Control.Monad (unless, forM_)
import qualified Data.Map.Strict as M
import qualified NeoMusic as A

type Name = String
data Event = Event { dur :: Rational, vel :: Double, pitches :: [A.Step] }
  deriving (Eq, Show)
type Phrase = [Event]
data Score = Line Name Phrase | Silence Rational | Score :>>: Score | Score :||: Score
  deriving (Eq, Show)
infixr 5 :>>:
infixr 4 :||:
instance Semigroup Score where (<>) = (:>>:)
instance Monoid Score where mempty = Silence 0

data Instrument = Instrument
  { tuning :: A.Tuning, timbre :: A.Patch, gain :: Double, pan :: Double, send :: Double }
  deriving (Eq, Show)
data Performance = Performance
  { bpm :: Double, instruments :: M.Map Name Instrument, mix :: A.Mix }
  deriving (Eq, Show)
data BeatEvent = BeatEvent { onset :: Rational, instrument :: Name, event :: Event }
  deriving (Eq, Show)

defaultInstrument :: Instrument
defaultInstrument = Instrument (A.Tuning (440 * 2 ** (-9/12)) (2 ** (1/12))) A.pianoPatch 1 0 0
defaultPerformance :: Performance
defaultPerformance = Performance 120 M.empty (A.defaultMix 44100)

n :: Int -> Phrase
n x = chord [x]
chord :: [Int] -> Phrase
chord xs = hit xs 1 1
rest :: Phrase
rest = hush 1
hit :: [Int] -> Rational -> Double -> Phrase
hit xs d v = [Event d v (map A.Step xs)]
hush :: Rational -> Phrase
hush d = [Event d 0 [] | d /= 0]
transpose :: Int -> Phrase -> Phrase
transpose k = map (\e -> e {pitches = [A.Step (x+k) | A.Step x <- pitches e]})
invert :: Phrase -> Phrase
invert = map (\e -> e {pitches = [A.Step (negate x) | A.Step x <- pitches e]})
retro :: Phrase -> Phrase
retro = reverse
stretch :: Rational -> Phrase -> Phrase
stretch k = map (\e -> e {dur = k * dur e})
withVelocity :: Double -> Phrase -> Phrase
withVelocity v = map (\e -> e {vel = v})
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
    finite x = not (isNaN x || isInfinite x)

-- | Compatibility adapter; views obtain exact timing from flatten, not this tree.
toPiece :: Performance -> Score -> Either String A.Piece
toPiece performance score = do
  _ <- flatten score
  unless (bpm performance > 0 && not (isInfinite (bpm performance))) (Left "tempo must be finite and positive")
  forM_ (M.keys (instruments performance)) $ \name -> go (Line name []) >>= A.render >> pure ()
  piece <- go score
  _ <- A.render piece
  pure piece
  where
    silent d = A.FromPart (A.Part (tuning defaultInstrument) (A.Tempo (bpm performance))
      (A.Synth A.sinePatch) (if d == 0 then [] else [A.Rest]) (if d == 0 then [] else [d]) (if d == 0 then [] else [0]))
    go (Silence d) = pure (silent d)
    go (Line name xs) = do
      i <- maybe (Left ("unknown instrument: " ++ name)) Right (M.lookup name (instruments performance))
      let p = A.FromPart (A.Part (tuning i) (A.Tempo (bpm performance)) (A.Synth (timbre i))
                [A.Play (pitches e) | e <- xs] (map dur xs) (map vel xs))
      pure (A.Gain (gain i) (A.Pan (pan i) (A.Send (send i) p)))
    go (a :>>: b) = (A.:>>:) <$> go a <*> go b
    go (a :||: b) = (A.:||:) <$> go a <*> go b
render :: Performance -> Score -> Either String A.Timeline
render performance score = do
  _ <- toPiece performance score
  (total,es) <- flatten score
  let seconds beats = fromRational beats * (60 / bpm performance)
  ns <- concat <$> mapM (toNotes seconds) es
  unless (all (\v -> finite (A.start v) && finite (A.duration v) && A.duration v > 0) ns
          && finite (seconds total)) (Left "score time cannot be represented in seconds")
  pure (A.Timeline (seconds total) ns)
  where
    finite x = not (isNaN x || isInfinite x)
    toNotes seconds e = do
      i <- maybe (Left "unknown instrument") Right (M.lookup (instrument e) (instruments performance))
      pure [A.Note (seconds (onset e)) (A.freq (tuning i) pitch) (seconds (dur (event e)))
             (A.Synth (timbre i)) (vel (event e)) (gain i) (pan i) (send i)
           | pitch <- pitches (event e)]
writeAudio :: FilePath -> Performance -> Score -> IO ()
writeAudio path performance score = do
  timeline <- either (ioError . userError) pure (render performance score)
  A.writeTimelineMix path (mix performance) timeline
