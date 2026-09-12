-- | Unitless composition, followed by explicit interpretation and audio synthesis.
module NeoMusic
  ( Step(..), Event(..), Seq, Rhythm, Tuning(..), Tempo(..), Timbre(..)
  , Part(..), Piece(..), Note(..), Timeline(..)
  , n, chord, rest, transpose, invert, retro, stretch, toDeltas, fromDeltas
  , parseSeq, prettySeq, equalTemperament, freq, part, render
  , samples, writeWav, writeAudio, demo
  ) where

import Control.Monad (unless, when)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.Int (Int16)
import Data.List (intercalate)
import System.Directory (findExecutable)
import System.FilePath (takeExtension)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess)
import Text.Parsec hiding (count)
import Text.Parsec.String (Parser)

newtype Step = Step Int deriving (Eq, Ord, Show)
data Event = Play [Step] | Rest deriving (Eq, Show)
type Seq = [Event]
type Rhythm = [Rational]
data Tuning = Tuning { f0 :: Double, ratio :: Double } deriving (Eq, Show)
newtype Tempo = Tempo { bpm :: Double } deriving (Eq, Show)
data Timbre = Sine | Saw | Square deriving (Eq, Show)
data Part = Part
  { tuning :: Tuning, tempo :: Tempo, timbre :: Timbre
  , voice :: Seq, rhythm :: Rhythm } deriving (Eq, Show)
data Piece = FromPart Part | Piece :>>: Piece | Piece :||: Piece deriving (Eq, Show)
infixr 5 :>>:
infixr 4 :||:
data Note = Note
  { start :: Double, hz :: Double, duration :: Double, sound :: Timbre }
  deriving (Eq, Show)
data Timeline = Timeline { seconds :: Double, notes :: [Note] } deriving (Eq, Show)

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

equalTemperament :: Double -> Int -> Either String Tuning
equalTemperament base divisions
  | not (positive base) || divisions <= 0 = Left "positive base Hz and divisions required"
  | otherwise = Right (Tuning base (2 ** (1 / fromIntegral divisions)))
freq :: Tuning -> Step -> Double
freq (Tuning base r) (Step x) = base * r ** fromIntegral x
part :: Tuning -> Tempo -> Timbre -> Seq -> Piece
part t speed tone xs = FromPart (Part t speed tone xs (replicate (length xs) 1))
positive :: Double -> Bool
positive x = x > 0 && not (isNaN x || isInfinite x)

-- | Reject mismatched rhythms rather than silently dropping events. Silent
-- slots contribute to duration, including at sequential part boundaries.
render :: Piece -> Either String Timeline
render (FromPart p) = do
  unless (all positive [f0 (tuning p), ratio (tuning p), bpm (tempo p)])
    (Left "tuning and tempo must be finite and positive")
  unless (length (voice p) == length (rhythm p))
    (Left "one rhythm duration is required per event")
  let ds = map ((60 / bpm (tempo p) *) . fromRational) (rhythm p)
  unless (all positive ds) (Left "durations must be finite and positive")
  let times = scanl (+) 0 ds
      ns = [Note t (freq (tuning p) x) d (timbre p)
           | (t, d, Play xs) <- zip3 times ds (voice p), x <- xs]
      total = last times
  unless (all (positive . hz) ns && (total == 0 || positive total))
    (Left "interpreted frequency or duration overflow")
  pure (Timeline total ns)
render (a :>>: b) = combine False a b
render (a :||: b) = combine True a b

combine :: Bool -> Piece -> Piece -> Either String Timeline
combine parallel a b = do
  Timeline da na <- render a
  Timeline db nb <- render b
  let offset = if parallel then 0 else da
      total = if parallel then max da db else da + db
  unless (total == 0 || positive total) (Left "piece duration overflow")
  pure (Timeline total (na ++ [v { start = start v + offset } | v <- nb]))

-- | Mono synthesis with short attack/release envelopes and bounded harmonic
-- oscillators. Reject pitches at/above Nyquist rather than aliasing them.
-- A fixed voice-count gain prevents clipping and preserves exact silence.
samples :: Int -> Piece -> Either String [Double]
samples rate piece = do
  unless (rate >= 8000 && rate <= 192000) (Left "sample rate must be 8000..192000")
  Timeline total ns <- render piece
  unless (all ((< fromIntegral rate / 2) . hz) ns) (Left "pitch reaches Nyquist frequency")
  let peakVoices = maximum (1 : [length [v | v <- ns, start v <= t, t < start v + duration v] | t <- map start ns])
      gain = 0.8 / fromIntegral peakVoices
      sample i = gain * sum [osc v (fromIntegral i / fromIntegral rate - start v) | v <- ns]
      osc v t
        | t < 0 || t >= duration v = 0
        | otherwise = envelope * wave
        where
          envelope = min 1 (t / 0.005) * min 1 ((duration v - t) / 0.015)
          phase = 2 * pi * hz v * t
          harmonics = (ceiling (min 65 (fromIntegral rate / (2 * hz v))) - 1 :: Integer)
          ks = [1 .. harmonics]
          wave = case sound v of
            Sine -> sin phase
            Saw -> sum [sin (fromIntegral k * phase) / fromIntegral k | k <- ks] / 2
            Square -> sum [sin (fromIntegral k * phase) / fromIntegral k | k <- ks, odd k] / 2
  pure [sample i | i <- [0 .. ceiling (total * fromIntegral rate) - 1 :: Integer]]

-- | PCM16 little-endian mono WAV, requiring no external synthesizer.
writeWav :: FilePath -> Int -> Piece -> IO ()
writeWav path rate piece = do
  timeline <- either (ioError . userError) pure (render piece)
  let count = ceiling (seconds timeline * fromIntegral rate) :: Integer
      size = count * 2
  when (size > 4294967259) (ioError (userError "audio exceeds RIFF WAV size limit"))
  xs <- either (ioError . userError) pure (samples rate piece)
  let header = string8 "RIFF" <> word32LE (fromInteger (36 + size)) <> string8 "WAVEfmt "
        <> word32LE 16 <> word16LE 1 <> word16LE 1 <> word32LE (fromIntegral rate)
        <> word32LE (fromIntegral (rate * 2)) <> word16LE 2 <> word16LE 16
        <> string8 "data" <> word32LE (fromInteger size)
      pcm x = int16LE (round (max (-1) (min 1 x) * 32767) :: Int16)
  BL.writeFile path (toLazyByteString (header <> foldMap pcm xs))

-- | WAV is native; MP3 and FLAC use FFmpeg installed on PATH. Process arguments
-- are passed directly (no shell interpolation); existing outputs are replaced.
writeAudio :: FilePath -> Int -> Piece -> IO ()
writeAudio path rate piece = case takeExtension path of
  ".wav" -> writeWav path rate piece
  extension | extension `elem` [".mp3", ".flac"] -> do
    executable <- findExecutable "ffmpeg"
    ffmpeg <- maybe (ioError (userError "MP3/FLAC export requires ffmpeg on PATH")) pure executable
    withSystemTempFile "neo-music.wav" $ \tmp handle -> do
      hClose handle
      writeWav tmp rate piece
      callProcess ffmpeg (["-v", "error", "-y", "-i", tmp] ++
        (if extension == ".mp3" then ["-codec:a", "libmp3lame", "-q:a", "2"] else []) ++ [path])
  _ -> ioError (userError "supported extensions: .wav, .mp3, .flac")

demo :: Piece
demo = section :>>: micro
  where
    tuning12 = Tuning 220 (2 ** (1 / 12))
    motif = n 0 <> n 7 <> n 3 <> n 10 <> rest <> n 7 <> n 5 <> n 3
    lead = FromPart (Part tuning12 (Tempo 120) Sine
      (motif <> transpose 5 (retro motif)) (replicate 16 (1 / 2)))
    bass = FromPart (Part tuning12 (Tempo 120) Saw
      (chord [-12,-5] <> rest <> chord [-9,-2] <> rest) [2,2,2,2])
    section = lead :||: bass
    micro = FromPart (Part (Tuning 220 (2 ** (1 / 24))) (Tempo 120) Sine
      (concatMap n [0,1,2,3,4,7,10,14] <> rest) (replicate 8 (1 / 2) ++ [1]))
