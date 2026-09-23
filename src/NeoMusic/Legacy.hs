-- | The v1/v2 API: parallel pitch/rhythm/dynamics lists and a 'Piece' tree with
-- mixer nodes. Kept for existing programs; new music should use "NeoMusic.Score".
module NeoMusic.Legacy
  ( -- * Sequences
    Event(..), Seq, Rhythm, Dynamics
  , n, chord, rest, transpose, invert, retro, stretch, ramp, toDeltas, fromDeltas
  , parseSeq, prettySeq
    -- * Interpretation
  , Tempo(..), Timbre(..), Part(..), Piece(..), patchOf, part, render
    -- * Rendering and export
  , samples, renderMix, writeWav, writeMix, writeAudio, demo
  ) where

import Control.Monad (unless)
import Data.List (intercalate, zip4)
import qualified Data.Vector.Unboxed as VU
import NeoMusic.Audio
import NeoMusic.Pitch
import Text.Parsec hiding (count)
import Text.Parsec.String (Parser)

data Event = Play [Step] | Rest deriving (Eq, Show)
type Seq = [Event]
type Rhythm = [Rational]
-- | Velocity of each event in 0..1, the loudness axis parallel to 'Rhythm'.
type Dynamics = [Double]
newtype Tempo = Tempo { bpm :: Double } deriving (Eq, Show)
data Timbre = Sine | Saw | Square | Synth Patch deriving (Eq, Show)
data Part = Part
  { tuning :: Tuning, tempo :: Tempo, timbre :: Timbre
  , voice :: Seq, rhythm :: Rhythm, dynamics :: Dynamics } deriving (Eq, Show)

-- | 'Gain' multiplies amplitude, 'Pan' shifts stereo position (-1 left .. 1
-- right; nested pans add and clamp) and 'Send' adds reverb send level.
data Piece
  = FromPart Part | Piece :>>: Piece | Piece :||: Piece
  | Gain Double Piece | Pan Double Piece | Send Double Piece
  deriving (Eq, Show)
infixr 5 :>>:
infixr 4 :||:

n :: Int -> Seq
n x = chord [x]
chord :: [Int] -> Seq
chord = pure . Play . map Step
rest :: Seq
rest = [Rest]
mapSteps :: (Int -> Int) -> Seq -> Seq
mapSteps f = map go
  where
    go Rest = Rest
    go (Play xs) = Play [Step (f x) | Step x <- xs]
transpose :: Int -> Seq -> Seq
transpose k = mapSteps (+ k)
invert :: Seq -> Seq
invert = mapSteps negate
retro :: Seq -> Seq
retro = reverse
stretch :: Rational -> Rhythm -> Rhythm
stretch k = map (* k)
-- | @k@ velocities moving linearly from @a@ to @b@ inclusive (a crescendo or diminuendo).
ramp :: Double -> Double -> Int -> Dynamics
ramp a b k
  | k <= 1 = replicate k a
  | otherwise = [a + (b - a) * fromIntegral i / fromIntegral (k - 1) | i <- [0 .. k - 1]]
toDeltas :: [Int] -> [Int]
toDeltas xs = zipWith (-) xs (0 : xs)
fromDeltas :: [Int] -> [Int]
fromDeltas = scanl1 (+)

parseSeq :: String -> Either ParseError Seq
parseSeq = parse (spaces *> sepEndBy event (skipMany1 space) <* eof) "NeoMusic"
  where
    integer :: Parser Step
    integer = do
      sign <- option id (char '-' >> pure negate)
      digits <- many1 digit
      let value = sign (read digits :: Integer)
      if value < toInteger (minBound :: Int) || value > toInteger (maxBound :: Int)
        then fail "step outside Int range"
        else pure (Step (fromInteger value))
    event = (char '_' >> pure Rest)
      <|> (Play <$> between (char '(' *> spaces) (char ')')
            (sepEndBy integer (skipMany1 space)))
      <|> (Play . pure <$> integer)

prettySeq :: Seq -> String
prettySeq = unwords . map event
  where
    event Rest = "_"
    event (Play [Step x]) = show x
    event (Play xs) = "(" ++ intercalate " " [show x | Step x <- xs] ++ ")"

-- | One beat and full velocity per event.
part :: Tuning -> Tempo -> Timbre -> Seq -> Piece
part t speed tone xs = FromPart (Part t speed tone xs (replicate k 1) (replicate k 1))
  where k = length xs
finite, positive, nonNegative, unitRange :: Double -> Bool
finite x = not (isNaN x || isInfinite x)
positive x = finite x && x > 0
nonNegative x = finite x && x >= 0
unitRange x = nonNegative x && x <= 1

-- | Reject mismatched rhythms/dynamics rather than silently dropping events.
-- Silent slots contribute to duration, including at sequential part boundaries.
render :: Piece -> Either String Timeline
render (FromPart p) = do
  unless (all positive [f0 (tuning p), ratio (tuning p), bpm (tempo p)])
    (Left "tuning and tempo must be finite and positive")
  unless (length (voice p) == length (rhythm p))
    (Left "one rhythm duration is required per event")
  unless (length (voice p) == length (dynamics p))
    (Left "one dynamics level is required per event")
  unless (all unitRange (dynamics p)) (Left "dynamics must be within 0..1")
  validTimbre (timbre p)
  let ds = map ((60 / bpm (tempo p) *) . fromRational) (rhythm p)
  unless (all positive ds) (Left "durations must be finite and positive")
  let times = scanl (+) 0 ds
      ns = [Note t (freq (tuning p) x) d (patchOf (timbre p)) v 1 0 0
           | (t, d, v, Play xs) <- zip4 times ds (dynamics p) (voice p), x <- xs]
      total = last times
  unless (all (positive . hz) ns && (total == 0 || positive total))
    (Left "interpreted frequency or duration overflow")
  pure (Timeline total ns)
render (a :>>: b) = combine False a b
render (a :||: b) = combine True a b
render (Gain g p) = do
  unless (nonNegative g) (Left "gain must be finite and nonnegative")
  adjust (\v -> v { gain = gain v * g }) <$> render p
render (Pan x p) = do
  unless (finite x && abs x <= 1) (Left "pan must be within -1..1")
  adjust (\v -> v { pan = max (-1) (min 1 (pan v + x)) }) <$> render p
render (Send s p) = do
  unless (unitRange s) (Left "reverb send must be within 0..1")
  adjust (\v -> v { send = send v + s }) <$> render p

adjust :: (Note -> Note) -> Timeline -> Timeline
adjust f (Timeline d ns) = Timeline d (map f ns)

combine :: Bool -> Piece -> Piece -> Either String Timeline
combine parallel a b = do
  Timeline da na <- render a
  Timeline db nb <- render b
  let offset = if parallel then 0 else da
      total = if parallel then max da db else da + db
  unless (total == 0 || positive total) (Left "piece duration overflow")
  pure (Timeline total (na ++ [v { start = start v + offset } | v <- nb]))

validTimbre :: Timbre -> Either String ()
validTimbre = validPatch . patchOf

-- Legacy constructor names are presets, not a second synthesis implementation.
patchOf :: Timbre -> Patch
patchOf Sine = sinePatch
patchOf Saw = sawPatch
patchOf Square = squarePatch
patchOf (Synth p) = p

-- | Mono samples (the stereo 'defaultMix' render averaged to one channel).
samples :: Int -> Piece -> Either String [Double]
samples rate piece = do
  (l, r) <- renderMix (defaultMix rate) piece
  pure (VU.toList (VU.zipWith (\a b -> (a + b) / 2) l r))

-- | Stereo render. Each note is synthesised only over its own span into
-- left/right/send buffers; the send bus feeds the reverb, then the master stage.
-- All timbres use the patch engine and may ring past the gate by their release
-- time. Base pitches at or above Nyquist are rejected.
renderMix :: Mix -> Piece -> Either String (VU.Vector Double, VU.Vector Double)
renderMix mix piece = render piece >>= renderTimelineMix mix
-- | Mono PCM16 WAV of the 'defaultMix' render, requiring no external synthesizer.
writeWav :: FilePath -> Int -> Piece -> IO ()
writeWav path rate piece = do
  (l, r) <- either (ioError . userError) pure (renderMix (defaultMix rate) piece)
  pcmWav path rate [VU.zipWith (\a b -> (a + b) / 2) l r]

-- | Stereo export with explicit mix settings (.wav, or .mp3/.flac via FFmpeg).
writeMix :: FilePath -> Mix -> Piece -> IO ()
writeMix path mix piece = exportAs path $ \out -> do
  (l, r) <- either (ioError . userError) pure (renderMix mix piece)
  pcmWav out (mixRate mix) [l, r]

-- | Mono export: WAV is native; MP3 and FLAC use FFmpeg installed on PATH.
writeAudio :: FilePath -> Int -> Piece -> IO ()
writeAudio path rate piece = exportAs path (\out -> writeWav out rate piece)

demo :: Piece
demo = section :>>: micro
  where
    tuning12 = Tuning 220 (2 ** (1 / 12))
    motif = n 0 <> n 7 <> n 3 <> n 10 <> rest <> n 7 <> n 5 <> n 3
    lead = FromPart (Part tuning12 (Tempo 120) Sine
      (motif <> transpose 5 (retro motif)) (replicate 16 (1 / 2)) (ramp 0.4 1 16))
    bass = FromPart (Part tuning12 (Tempo 120) Saw
      (chord [-12,-5] <> rest <> chord [-9,-2] <> rest) [2,2,2,2] (replicate 4 0.6))
    section = lead :||: bass
    micro = FromPart (Part (Tuning 220 (2 ** (1 / 24))) (Tempo 120) Sine
      (concatMap n [0,1,2,3,4,7,10,14] <> rest) (replicate 8 (1 / 2) ++ [1]) (replicate 9 1))
