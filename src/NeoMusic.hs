{-# LANGUAGE BangPatterns #-}
-- | Unitless composition, followed by explicit interpretation and audio synthesis.
module NeoMusic
  ( -- * Sequences
    Step(..), Event(..), Seq, Rhythm, Dynamics
  , n, chord, rest, transpose, invert, retro, stretch, ramp, toDeltas, fromDeltas
  , parseSeq, prettySeq
    -- * Interpretation
  , Tuning(..), Tempo(..), Timbre(..), Part(..), Piece(..), Note(..), Timeline(..)
  , equalTemperament, freq, part, render
    -- * Synth patches
  , Wave(..), Osc(..), Envelope(..), Patch(..)
  , basicPatch, pluck, supersaw, reeseBass, subBass, kick, snare, hat, noiseSweep
    -- * Mixing and export
  , Reverb(..), Mix(..), noReverb, hall, defaultMix
  , samples, renderMix, writeWav, writeMix, writeAudio, demo
  ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, shiftR, xor)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.Int (Int16)
import Data.List (intercalate, zip4)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word32)
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
-- | Velocity of each event in 0..1, the loudness axis parallel to 'Rhythm'.
type Dynamics = [Double]
data Tuning = Tuning { f0 :: Double, ratio :: Double } deriving (Eq, Show)
newtype Tempo = Tempo { bpm :: Double } deriving (Eq, Show)

data Wave = SineWave | SawWave | SquareWave | TriangleWave | NoiseWave deriving (Eq, Show)

-- | One oscillator of a patch: waveform, pitch offset in semitones, gain, and a
-- unison voice count whose detune spreads across 'detuneCents' in total.
data Osc = Osc
  { wave :: Wave, semitones :: Double, oscGain :: Double, unison :: Int, detuneCents :: Double }
  deriving (Eq, Show)

-- | Attack, decay and release in seconds; sustain level in 0..1.
data Envelope = Envelope { attack :: Double, decay :: Double, sustain :: Double, release :: Double }
  deriving (Eq, Show)

-- | Subtractive voice: oscillators, resonant low-pass, optional high-pass, amp envelope.
-- The low-pass cutoff in Hz at time t is
--
-- > cutoff * 2 ** (filterAmount * filterEnvelope t + velocityToCutoff * velocity)
-- >        * (pitch / 261.63) ** keyTrack
--
-- 'pitchDrop' semitones are added at onset and decay with time constant 'pitchTime'.
-- Unison voices are spread across the stereo field by 'stereoWidth' (0..1).
data Patch = Patch
  { oscillators :: [Osc]
  , ampEnvelope :: Envelope
  , cutoff :: Double, resonance :: Double, keyTrack :: Double
  , filterEnvelope :: Envelope, filterAmount :: Double, velocityToCutoff :: Double
  , highpass :: Double
  , pitchDrop :: Double, pitchTime :: Double
  , stereoWidth :: Double
  } deriving (Eq, Show)

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
data Note = Note
  { start :: Double, hz :: Double, duration :: Double, sound :: Timbre
  , velocity :: Double, gain :: Double, pan :: Double, send :: Double }
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

equalTemperament :: Double -> Int -> Either String Tuning
equalTemperament base divisions
  | not (positive base) || divisions <= 0 = Left "positive base Hz and divisions required"
  | otherwise = Right (Tuning base (2 ** (1 / fromIntegral divisions)))
freq :: Tuning -> Step -> Double
freq (Tuning base r) (Step x) = base * r ** fromIntegral x
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
      ns = [Note t (freq (tuning p) x) d (timbre p) v 1 0 0
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
validTimbre (Synth p) = do
  let env (Envelope a d s r) = all nonNegative [a, d, r] && unitRange s
      oscOk (Osc _ st g u c) = finite st && nonNegative g && u >= 1 && nonNegative c
  unless (not (null (oscillators p)) && all oscOk (oscillators p))
    (Left "patch needs oscillators with nonnegative gain and spread, and unison >= 1")
  unless (env (ampEnvelope p) && env (filterEnvelope p))
    (Left "envelope times must be nonnegative and sustain within 0..1")
  unless (positive (cutoff p) && nonNegative (resonance p) && resonance p < 1
          && all finite [keyTrack p, filterAmount p, velocityToCutoff p, pitchDrop p]
          && nonNegative (highpass p) && unitRange (stereoWidth p))
    (Left "invalid patch filter, pitch or stereo settings")
  unless (pitchDrop p == 0 || positive (pitchTime p))
    (Left "pitchTime must be positive when pitchDrop is set")
validTimbre _ = Right ()

-- Patches ---------------------------------------------------------------------

-- | Single saw, open filter, organ-like envelope: a starting point for record updates.
basicPatch :: Patch
basicPatch = Patch
  { oscillators = [Osc SawWave 0 1 1 0]
  , ampEnvelope = Envelope 0.005 0 1 0.05
  , cutoff = 20000, resonance = 0, keyTrack = 0
  , filterEnvelope = Envelope 0.001 0 0 0.05, filterAmount = 0, velocityToCutoff = 0
  , highpass = 0, pitchDrop = 0, pitchTime = 0.05, stereoWidth = 0 }

-- | Bright plucked saw; velocity opens the filter.
pluck :: Patch
pluck = basicPatch
  { oscillators = [Osc SawWave 0 1 3 14, Osc SquareWave 12 0.25 1 0]
  , ampEnvelope = Envelope 0.002 0.4 0 0.3
  , cutoff = 450, resonance = 0.3, keyTrack = 0.5
  , filterEnvelope = Envelope 0.001 0.2 0 0.2, filterAmount = 3.5, velocityToCutoff = 1.2
  , stereoWidth = 0.6 }

-- | Seven detuned saws plus a sub-octave, slow attack, wide stereo.
supersaw :: Patch
supersaw = basicPatch
  { oscillators = [Osc SawWave 0 1 7 38, Osc SawWave (-12) 0.35 1 0]
  , ampEnvelope = Envelope 0.4 0.6 0.8 1.2
  , cutoff = 900, resonance = 0.15, keyTrack = 0.3
  , filterEnvelope = Envelope 0.5 1.5 0.4 1, filterAmount = 1.5, velocityToCutoff = 2
  , stereoWidth = 1 }

-- | Two slowly beating saws over a sine sub, low-passed.
reeseBass :: Patch
reeseBass = basicPatch
  { oscillators = [Osc SawWave 0 1 2 22, Osc SineWave (-12) 0.8 1 0]
  , ampEnvelope = Envelope 0.005 0.3 0.8 0.08
  , cutoff = 160, resonance = 0.25
  , filterEnvelope = Envelope 0.002 0.25 0 0.1, filterAmount = 2.5, velocityToCutoff = 1
  , stereoWidth = 0.3 }

subBass :: Patch
subBass = basicPatch
  { oscillators = [Osc SineWave 0 1 1 0, Osc TriangleWave 0 0.12 1 0]
  , ampEnvelope = Envelope 0.01 0 1 0.1 }

-- | Sine with a fast pitch drop: tune the note to the body frequency (~40-60 Hz).
kick :: Patch
kick = basicPatch
  { oscillators = [Osc SineWave 0 1 1 0]
  , ampEnvelope = Envelope 0.001 0.35 0 0.05
  , pitchDrop = 36, pitchTime = 0.035 }

-- | Noise burst over a pitched triangle body.
snare :: Patch
snare = basicPatch
  { oscillators = [Osc NoiseWave 0 1 2 0, Osc TriangleWave 0 0.6 1 0]
  , ampEnvelope = Envelope 0.001 0.22 0 0.08
  , cutoff = 8000, highpass = 180, pitchDrop = 7, pitchTime = 0.03, stereoWidth = 0.3 }

-- | Closed hi-hat: short high-passed noise (pitch is ignored).
hat :: Patch
hat = basicPatch
  { oscillators = [Osc NoiseWave 0 1 2 0]
  , ampEnvelope = Envelope 0.001 0.05 0 0.03
  , highpass = 7000, stereoWidth = 0.4 }

-- | Noise swell whose resonant filter sweeps up over about four seconds.
noiseSweep :: Patch
noiseSweep = basicPatch
  { oscillators = [Osc NoiseWave 0 1 2 0]
  , ampEnvelope = Envelope 3 0 1 0.3
  , cutoff = 250, resonance = 0.55
  , filterEnvelope = Envelope 4 0 1 0.3, filterAmount = 5.5
  , highpass = 120, stereoWidth = 1 }

-- Mixing ----------------------------------------------------------------------

-- | Stereo Freeverb on the send bus. 'wet' 1/3 is Freeverb's default level.
data Reverb = Reverb
  { roomSize :: Double, damping :: Double, width :: Double, wet :: Double, preDelay :: Double }
  deriving (Eq, Show)

-- | Render settings. With 'mixNormalize' the master is scaled so its peak is
-- 'mixPeak'; 'mixDrive' > 0 adds tanh saturation before that final scaling.
-- 'mixTail' seconds are appended after the last release, for reverb decay.
data Mix = Mix
  { mixRate :: Int, mixReverb :: Reverb, mixDrive :: Double
  , mixPeak :: Double, mixTail :: Double, mixNormalize :: Bool }
  deriving (Eq, Show)

noReverb :: Reverb
noReverb = Reverb 0.5 0.5 1 0 0

hall :: Reverb
hall = Reverb { roomSize = 0.85, damping = 0.4, width = 1, wet = 0.4, preDelay = 0.02 }

-- | Dry, normalised to a 0.8 peak, no tail.
defaultMix :: Int -> Mix
defaultMix rate = Mix rate noReverb 0 0.8 0 True

tailOf :: Timbre -> Double
tailOf (Synth p) = max 0.002 (release (ampEnvelope p))
tailOf _ = 0

-- | Mono samples (the stereo 'defaultMix' render averaged to one channel).
samples :: Int -> Piece -> Either String [Double]
samples rate piece = do
  (l, r) <- renderMix (defaultMix rate) piece
  pure (VU.toList (VU.zipWith (\a b -> (a + b) / 2) l r))

-- | Stereo render. Each note is synthesised only over its own span into
-- left/right/send buffers; the send bus feeds the reverb, then the master stage.
-- Sine, Saw and Square keep their additive oscillators and 5/15 ms envelopes
-- (at most 64 harmonics below Nyquist); 'Synth' patches may ring past the note
-- by their release time. Base pitches at or above Nyquist are rejected.
renderMix :: Mix -> Piece -> Either String (VU.Vector Double, VU.Vector Double)
renderMix mix piece = do
  let rate = mixRate mix
      fr = fromIntegral rate :: Double
      rv = mixReverb mix
  unless (rate >= 8000 && rate <= 192000) (Left "sample rate must be 8000..192000")
  unless (all unitRange [roomSize rv, damping rv, width rv]
          && nonNegative (wet rv) && nonNegative (preDelay rv))
    (Left "reverb room, damping and width must be within 0..1; wet and pre-delay nonnegative")
  unless (nonNegative (mixDrive mix) && nonNegative (mixTail mix) && mixPeak mix > 0 && mixPeak mix <= 1)
    (Left "mix drive and tail must be nonnegative and peak within (0, 1]")
  Timeline total ns <- render piece
  unless (all ((< fr / 2) . hz) ns) (Left "pitch reaches Nyquist frequency")
  let end = maximum (total : [start v + duration v + tailOf (sound v) | v <- ns]) + mixTail mix
      len = ceiling (end * fr) :: Int
      (outL, outR) = runST $ do
        left <- MV.replicate len 0
        right <- MV.replicate len 0
        sends <- MV.replicate len 0
        forM_ (zip [1 ..] ns) $ \(seed, v) -> renderNote fr seed v left right sends
        dryL <- VU.unsafeFreeze left
        dryR <- VU.unsafeFreeze right
        sendBus <- VU.unsafeFreeze sends
        let (wetL, wetR) = freeverb fr rv sendBus
            withWet dry w = if wet rv > 0 then VU.zipWith (+) dry w else dry
        pure (master mix (withWet dryL wetL) (withWet dryR wetR))
  when (VU.any isNaN outL || VU.any isNaN outR) (Left "synthesis produced NaN")
  pure (outL, outR)

type Buffer s = MV.MVector s Double

addTo :: Buffer s -> Int -> Double -> ST s ()
addTo buf i x = MV.unsafeModify buf (+ x) i
{-# INLINE addTo #-}

-- | Equal-power pan gains, normalised so the centre is unity on both sides.
panGains :: Double -> (Double, Double)
panGains x = (sqrt 2 * cos a, sqrt 2 * sin a) where a = (x + 1) * pi / 4

sampleRange :: Double -> Int -> Double -> Double -> (Int, Int)
sampleRange fr len t0 t1 = (max 0 (ceiling (t0 * fr)), min len (ceiling (t1 * fr)))

renderNote :: Double -> Word32 -> Note -> Buffer s -> Buffer s -> Buffer s -> ST s ()
renderNote fr seed v left right sends = case sound v of
  Synth p -> renderSynth fr seed v p left right sends
  classic -> do
    let (i0, i1) = sampleRange fr (MV.length left) (start v) (start v + duration v)
        amp = velocity v * gain v
        (gl, gr) = panGains (pan v)
    forM_ [i0 .. i1 - 1] $ \i -> do
      let x = amp * classicWave fr classic (duration v) (hz v) (fromIntegral i / fr - start v)
      addTo left i (x * gl)
      addTo right i (x * gr)
      addTo sends i (send v * x * (gl + gr) / 2)

classicWave :: Double -> Timbre -> Double -> Double -> Double -> Double
classicWave fr tone dur f t
  | t < 0 || t >= dur = 0
  | otherwise = envelope * shape
  where
    envelope = min 1 (t / 0.005) * min 1 ((dur - t) / 0.015)
    phase = 2 * pi * f * t
    harmonics = ceiling (min 65 (fr / (2 * f))) - 1 :: Int
    partials step = go 1 0
      where
        go !k !acc
          | k > harmonics = acc
          | otherwise = go (k + step) (acc + sin (fromIntegral k * phase) / fromIntegral k)
    shape = case tone of
      Saw -> partials 1 / 2
      Square -> partials 2 / 2
      _ -> sin phase

-- | Linear attack, exponential decay to sustain, quadratic release after the gate.
adsr :: Envelope -> Double -> Double -> Double
adsr (Envelope a d s r) gate t
  | t < 0 = 0
  | t < gate = held t
  | t < gate + r' = let u = (t - gate) / r' in held gate * (1 - u) * (1 - u)
  | otherwise = 0
  where
    a' = max 0.001 a
    r' = max 0.002 r
    held x
      | x < a' = x / a'
      | d <= 0 = s
      | otherwise = s + (1 - s) * exp (-4.6 * (x - a') / d)

renderSynth :: Double -> Word32 -> Note -> Patch -> Buffer s -> Buffer s -> Buffer s -> ST s ()
renderSynth fr seed v p left right sends = do
  let voices =
        [ ((waveCode (wave o), 2 ** ((semitones o + detuneCents o * (x - 0.5) / 100) / 12)
          , oscGain o / sqrt (fromIntegral (unison o)), vl, vr), phase0)
        | o <- oscillators p
        , j <- [0 .. unison o - 1]
        , let x = if unison o == 1 then 0.5 else fromIntegral j / fromIntegral (unison o - 1) :: Double
              (vl, vr) = panGains (stereoWidth p * (2 * x - 1))
              phase0 = if unison o == 1 then 0 else wrap (fromIntegral j * 0.618034 + 0.1)
        ]
      table = VU.fromList (map fst voices)
      nv = VU.length table
  phases <- VU.thaw (VU.fromList (map snd voices))
  let (i0, i1) = sampleRange fr (MV.length left) (start v) (start v + duration v + tailOf (Synth p))
      amp = velocity v * gain v
      (gl, gr) = panGains (pan v)
      lowpassOn = cutoff p < 0.45 * fr || filterAmount p > 0 || velocityToCutoff p > 0
      kq = max 0.05 (sqrt 2 * (1 - resonance p))
      hpG = tan (pi * min (0.45 * fr) (highpass p) / fr)
      oscillate !j !pitch !accL !accR !rng
        | j >= nv = pure (accL, accR, rng)
        | otherwise = do
            let (code, mult, g, vl, vr) = VU.unsafeIndex table j
                dt = min 0.49 (pitch * mult / fr)
                rng' = if code == 4 then xorshift rng else rng
            ph <- MV.unsafeRead phases j
            let s = oscSample code ph dt rng'
            MV.unsafeWrite phases j (wrap (ph + dt))
            oscillate (j + 1) pitch (accL + g * vl * s) (accR + g * vr * s) rng'
      go !i !l1 !l2 !r1 !r2 !h1 !h2 !h3 !h4 !rng
        | i >= i1 = pure ()
        | otherwise = do
            let t = fromIntegral i / fr - start v
                pitch
                  | pitchDrop p == 0 = hz v
                  | otherwise = hz v * 2 ** (pitchDrop p * exp (-t / pitchTime p) / 12)
            (oscL, oscR, rng') <- oscillate 0 pitch 0 0 rng
            let fc = cutoff p * (pitch / 261.63) ** keyTrack p
                     * 2 ** (filterAmount p * adsr (filterEnvelope p) (duration v) t
                             + velocityToCutoff p * velocity v)
                lpG = tan (pi * min (0.45 * fr) fc / fr)
                (lowL, l1', l2') = if lowpassOn then lowpass lpG kq oscL l1 l2 else (oscL, l1, l2)
                (lowR, r1', r2') = if lowpassOn then lowpass lpG kq oscR r1 r2 else (oscR, r1, r2)
                (outL, h1', h2') = if highpass p > 0 then highpassed hpG lowL h1 h2 else (lowL, h1, h2)
                (outR, h3', h4') = if highpass p > 0 then highpassed hpG lowR h3 h4 else (lowR, h3, h4)
                e = amp * adsr (ampEnvelope p) (duration v) t
                yl = e * gl * outL
                yr = e * gr * outR
            addTo left i yl
            addTo right i yr
            addTo sends i (send v * (yl + yr) / 2)
            go (i + 1) l1' l2' r1' r2' h1' h2' h3' h4' rng'
      seed0 = (seed * 2654435761) `xor` 0x9E3779B9
  go i0 0 0 0 0 0 0 0 0 (if seed0 == 0 then 1 else seed0)

wrap :: Double -> Double
wrap x = x - fromIntegral (floor x :: Int)
{-# INLINE wrap #-}

waveCode :: Wave -> Int
waveCode w = case w of
  SineWave -> 0
  SawWave -> 1
  SquareWave -> 2
  TriangleWave -> 3
  NoiseWave -> 4

xorshift :: Word32 -> Word32
xorshift x0 = x3
  where
    x1 = x0 `xor` (x0 `shiftL` 13)
    x2 = x1 `xor` (x1 `shiftR` 17)
    x3 = x2 `xor` (x2 `shiftL` 5)

-- | Band-limited step correction for saw/square discontinuities.
polyBlep :: Double -> Double -> Double
polyBlep t dt
  | t < dt = let x = t / dt in x + x - x * x - 1
  | t > 1 - dt = let x = (t - 1) / dt in x * x + x + x + 1
  | otherwise = 0
{-# INLINE polyBlep #-}

oscSample :: Int -> Double -> Double -> Word32 -> Double
oscSample code ph dt rng = case code of
  0 -> sin (2 * pi * ph)
  1 -> 2 * ph - 1 - polyBlep ph dt
  2 -> (if ph < 0.5 then 1 else -1) + polyBlep ph dt - polyBlep (wrap (ph + 0.5)) dt
  3 -> 4 * abs (ph - 0.5) - 1
  _ -> fromIntegral rng / 2147483647.5 - 1
{-# INLINE oscSample #-}

-- | Topology-preserving state-variable filter (Simper): returns band, low and new state.
svf :: Double -> Double -> Double -> Double -> Double -> (Double, Double, Double, Double)
svf g k x ic1 ic2 = (v1, v2, 2 * v1 - ic1, 2 * v2 - ic2)
  where
    a1 = 1 / (1 + g * (g + k))
    a2 = g * a1
    a3 = g * a2
    v3 = x - ic2
    v1 = a1 * ic1 + a2 * v3
    v2 = ic2 + a2 * ic1 + a3 * v3
{-# INLINE svf #-}

lowpass :: Double -> Double -> Double -> Double -> Double -> (Double, Double, Double)
lowpass g k x ic1 ic2 = let (_, low, s1, s2) = svf g k x ic1 ic2 in (low, s1, s2)
{-# INLINE lowpass #-}

highpassed :: Double -> Double -> Double -> Double -> (Double, Double, Double)
highpassed g x ic1 ic2 =
  let k = sqrt 2
      (band, low, s1, s2) = svf g k x ic1 ic2
  in (x - k * band - low, s1, s2)
{-# INLINE highpassed #-}

-- | Freeverb: eight parallel damped combs into four series allpasses per
-- channel, the right channel's delays offset by 23 samples for width.
freeverb :: Double -> Reverb -> VU.Vector Double -> (VU.Vector Double, VU.Vector Double)
freeverb fr rv input =
  ( VU.zipWith (\a b -> a * wet1 + b * wet2) outL outR
  , VU.zipWith (\a b -> b * wet1 + a * wet2) outL outR )
  where
    delaySamples = round (preDelay rv * fr) :: Int
    fed = VU.generate (VU.length input) $ \i ->
      if i >= delaySamples then 0.015 * VU.unsafeIndex input (i - delaySamples) else 0
    tuned :: Int -> Int
    tuned x = max 1 (round (fromIntegral x * fr / 44100))
    feedback = roomSize rv * 0.28 + 0.7
    damp = damping rv * 0.4
    channel offset =
      let combs = [comb feedback damp (tuned (c + offset)) fed
                  | c <- [1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617]]
      in foldl (\acc a -> allpass (tuned (a + offset)) acc)
               (foldr1 (VU.zipWith (+)) combs) [556, 441, 341, 225]
    outL = channel 0
    outR = channel 23
    wet1 = 3 * wet rv * (width rv / 2 + 0.5)
    wet2 = 3 * wet rv * ((1 - width rv) / 2)

comb :: Double -> Double -> Int -> VU.Vector Double -> VU.Vector Double
comb fb damp size xs = VU.create $ do
  buf <- MV.replicate size 0
  out <- MV.new (VU.length xs)
  let go !i !idx !store
        | i >= VU.length xs = pure out
        | otherwise = do
            y <- MV.unsafeRead buf idx
            let store' = y * (1 - damp) + store * damp
            MV.unsafeWrite buf idx (VU.unsafeIndex xs i + store' * fb)
            MV.unsafeWrite out i y
            go (i + 1) (if idx + 1 == size then 0 else idx + 1) store'
  go 0 0 0

allpass :: Int -> VU.Vector Double -> VU.Vector Double
allpass size xs = VU.create $ do
  buf <- MV.replicate size 0
  out <- MV.new (VU.length xs)
  let go !i !idx
        | i >= VU.length xs = pure out
        | otherwise = do
            y <- MV.unsafeRead buf idx
            let x = VU.unsafeIndex xs i
            MV.unsafeWrite buf idx (x + y * 0.5)
            MV.unsafeWrite out i (y - x)
            go (i + 1) (if idx + 1 == size then 0 else idx + 1)
  go 0 0

master :: Mix -> VU.Vector Double -> VU.Vector Double -> (VU.Vector Double, VU.Vector Double)
master mix l r
  | not (mixNormalize mix) = (VU.map saturate l, VU.map saturate r)
  | peakIn == 0 = (l, r)
  | otherwise = (VU.map scaled l, VU.map scaled r)
  where
    peakOf = VU.foldl' (\m x -> max m (abs x)) 0
    d = mixDrive mix
    saturate x = if d > 0 then tanh (d * x) / tanh d else x
    peakIn = max (peakOf l) (peakOf r)
    scaled x = mixPeak mix * saturate (x / peakIn)

-- Export ----------------------------------------------------------------------

-- | Interleaved little-endian PCM16 WAV, one vector per channel.
pcmWav :: FilePath -> Int -> [VU.Vector Double] -> IO ()
pcmWav path rate channels = do
  let chans = length channels
      frames = if null channels then 0 else minimum (map VU.length channels)
      size = toInteger frames * toInteger chans * 2
  when (size > 4294967259) (ioError (userError "audio exceeds RIFF WAV size limit"))
  let header = string8 "RIFF" <> word32LE (fromInteger (36 + size)) <> string8 "WAVEfmt "
        <> word32LE 16 <> word16LE 1 <> word16LE (fromIntegral chans) <> word32LE (fromIntegral rate)
        <> word32LE (fromIntegral (rate * chans * 2)) <> word16LE (fromIntegral (chans * 2))
        <> word16LE 16 <> string8 "data" <> word32LE (fromInteger size)
      pcm x = int16LE (round (max (-1) (min 1 x) * 32767) :: Int16)
      frame i = foldMap (\ch -> pcm (VU.unsafeIndex ch i)) channels
  BL.writeFile path (toLazyByteString (header <> foldMap frame [0 .. frames - 1]))

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

-- | Process arguments are passed directly (no shell interpolation); existing
-- outputs are replaced.
exportAs :: FilePath -> (FilePath -> IO ()) -> IO ()
exportAs path writeTo = case takeExtension path of
  ".wav" -> writeTo path
  extension | extension `elem` [".mp3", ".flac"] -> do
    executable <- findExecutable "ffmpeg"
    ffmpeg <- maybe (ioError (userError "MP3/FLAC export requires ffmpeg on PATH")) pure executable
    withSystemTempFile "neo-music.wav" $ \tmp handle -> do
      hClose handle
      writeTo tmp
      callProcess ffmpeg (["-v", "error", "-y", "-i", tmp] ++
        (if extension == ".mp3" then ["-codec:a", "libmp3lame", "-q:a", "2"] else []) ++ [path])
  _ -> ioError (userError "supported extensions: .wav, .mp3, .flac")

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
