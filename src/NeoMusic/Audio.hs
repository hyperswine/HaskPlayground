{-# LANGUAGE BangPatterns #-}
-- | The synthesis engine. It knows only Hz, seconds and patches: no steps,
-- beats, scores or pieces. Every front end lowers to a 'Timeline' first.
module NeoMusic.Audio
  ( -- * Patches
    Wave(..), Osc(..), Patch(..), validPatch, percussive
    -- * Modulation
  , Mod(..), modAt, modBounds, plusM, timesM, adsr, cutoffHz, filterCutoff
  , basicPatch, sinePatch, sawPatch, squarePatch, pianoPatch
  , pluck, supersaw, reeseBass, subBass, kick, snare, hat, noiseSweep
    -- * Physical timeline
  , Note(..), Timeline(..), Contour(..), note, contourAt, contourBounds
    -- * Mixing and export
  , Reverb(..), Mix(..), noReverb, hall, defaultMix, validMix
  , renderTimelineMix, writeTimelineMix, pcmWav, exportAs
  ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, shiftR, xor)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.Int (Int16)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word32)
import System.Directory (findExecutable)
import System.FilePath (takeExtension)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess)

data Wave = SineWave | SawWave | SquareWave | TriangleWave | NoiseWave deriving (Eq, Show)

-- | One oscillator of a patch: waveform, pitch offset in semitones, gain, and a
-- unison voice count whose detune spreads across 'detuneCents' in total.
data Osc = Osc
  { wave :: Wave, semitones :: Double, oscGain :: Double, unison :: Int, detuneCents :: Double }
  deriving (Eq, Show)

-- | How a sound changes while a note plays, in seconds since note-on. A sound
-- has two anchors: note-on, where attack and decay count from, and note-off,
-- where 'Release' starts from wherever the sound has got to. See
-- neomusic-patch.md.
data Mod
  = K Double                      -- ^ a constant
  | Linear Double Double Double   -- ^ from a to b over d seconds, then holds b
  | Approach Double Double Double -- ^ from a toward b, 99% of the way after t seconds
  | Then Double Mod Mod           -- ^ the first for d seconds, then the second (its clock restarted)
  | Release Double Mod            -- ^ after note-off: from its value then down to 0 over r seconds
  | Lfo Wave Double               -- ^ a unit wave at f Hz, phase 0 at note-on
  | Velocity                      -- ^ how hard the note is played, 0..1
  | Key                           -- ^ the note's current pitch, octaves above C4 (261.63 Hz)
  | Mod :+ Mod
  | Mod :* Mod
  deriving (Eq, Show)
infixl 6 :+
infixl 7 :*

-- | Addition and multiplication that drop zeros and ones, so a patch built
-- from parts stays as simple (and as cheap to play) as it can.
plusM, timesM :: Mod -> Mod -> Mod
plusM (K 0) b = b
plusM a (K 0) = a
plusM a b = a :+ b
timesM (K 0) _ = K 0
timesM _ (K 0) = K 0
timesM (K 1) b = b
timesM a (K 1) = a
timesM a b = a :* b

-- | Attack, decay, sustain, release: exactly the classic envelope, built from
-- the primitives (attack at least 1 ms, release at least 2 ms).
adsr :: Double -> Double -> Double -> Double -> Mod
adsr a d s r = Release (max 0.002 r) (Then a' (Linear 0 1 a') (if d <= 0 then K s else Approach 1 s d))
  where a' = max 0.001 a

-- | A frequency as a cutoff: octaves above C4.
cutoffHz :: Double -> Mod
cutoffHz f = K (logBase 2 (f / 261.63))

-- | The classic subtractive cutoff: a base frequency, key tracking, an
-- envelope moving it by @amount@ octaves, and velocity sensitivity in octaves.
filterCutoff :: Double -> Double -> Mod -> Double -> Double -> Mod
filterCutoff base keyTrack env amount velocityAmount =
  cutoffHz base `plusM` timesM (K keyTrack) Key `plusM` timesM (K amount) env `plusM` timesM (K velocityAmount) Velocity

usesKey :: Mod -> Bool
usesKey m = case m of
  Key -> True
  Then _ a b -> usesKey a || usesKey b
  Release _ a -> usesKey a
  a :+ b -> usesKey a || usesKey b
  a :* b -> usesKey a || usesKey b
  _ -> False

-- | A modulation prepared for one note: velocity filled in, and each
-- release's level at note-off computed once (unless it follows the key).
-- Strict fields and a known recursive evaluator keep the per-sample
-- arithmetic unboxed.
data Prepared
  = PK !Double
  | PLinear !Double !Double !Double
  | PApproach !Double !Double !Double
  | PThen !Double !Prepared !Prepared
  | PRelease !Double !Bool !Double !Prepared   -- time, level known?, level at note-off, body
  | PLfo !Wave !Double
  | PKey
  | PAdd !Prepared !Prepared
  | PMul !Prepared !Prepared

prepare :: Double -> Double -> Mod -> Prepared
prepare gate vel m = case m of
  K x -> PK x
  Linear a b d -> PLinear a b d
  Approach a b d -> PApproach a b d
  Then d x y -> PThen d (prepare gate vel x) (prepare gate vel y)
  Release r x ->
    let px = prepare gate vel x
        known = not (usesKey x)
    in PRelease r known (if known then evalPrepared gate px gate 0 else 0) px
  Lfo w f -> PLfo w f
  Velocity -> PK vel
  Key -> PKey
  a :+ b -> PAdd (prepare gate vel a) (prepare gate vel b)
  a :* b -> PMul (prepare gate vel a) (prepare gate vel b)

-- | The value at @t@ seconds since note-on, for a note released at @gate@,
-- with the current key @k@.
evalPrepared :: Double -> Prepared -> Double -> Double -> Double
evalPrepared !gate = go
  where
    go p !t !k = case p of
      PK x -> x
      PLinear a b d -> if t >= d then b else a + (b - a) * (t / d)
      PApproach a b d -> if d <= 0 then b else b + (a - b) * exp (negate (4.6 * t / d))
      PThen d x y -> if t < d then go x t k else go y (t - d) k
      PRelease r known held x
        | t < gate -> go x t k
        | t < gate + r -> let u = (t - gate) / r in (if known then held else go x gate k) * (1 - u) * (1 - u)
        | otherwise -> 0
      PLfo w f -> waveAt w (f * t)
      PKey -> k
      PAdd a b -> go a t k + go b t k
      PMul a b -> go a t k * go b t k

-- | A modulation compiled for one note (its note-off time and velocity) into a
-- function of seconds since note-on and the current key.
compileMod :: Double -> Double -> Mod -> Double -> Double -> Double
compileMod gate vel m = evalPrepared gate (prepare gate vel m)

-- | A modulation's value at @t@ seconds for a note released at @gate@.
modAt :: Mod -> Double -> Double -> Double -> Double -> Double
modAt m gate vel key t = compileMod gate vel m t key

waveAt :: Wave -> Double -> Double
waveAt w phase = case w of
  SquareWave -> if frac < 0.5 then 1 else -1
  SawWave -> 2 * frac - 1
  TriangleWave -> 4 * abs (frac - 0.5) - 1
  _ -> sin (2 * pi * phase)
  where frac = phase - fromIntegral (floor phase :: Int)

-- | Lower and upper bounds over any note (velocity 0..1, key unbounded).
modBounds :: Mod -> (Double, Double)
modBounds m = case m of
  K x -> (x, x)
  Linear a b _ -> (min a b, max a b)
  Approach a b _ -> (min a b, max a b)
  Then _ a b -> let (l1, h1) = modBounds a; (l2, h2) = modBounds b in (min l1 l2, max h1 h2)
  Release _ a -> let (l, h) = modBounds a in (min 0 l, max 0 h)
  Lfo _ _ -> (-1, 1)
  Velocity -> (0, 1)
  Key -> (-1 / 0, 1 / 0)
  a :+ b -> let (l1, h1) = modBounds a; (l2, h2) = modBounds b in (l1 + l2, h1 + h2)
  a :* b -> let (l1, h1) = modBounds a; (l2, h2) = modBounds b
                ps = [mul l1 l2, mul l1 h2, mul h1 l2, mul h1 h2]
            in (minimum ps, maximum ps)
  where mul x y = if x == 0 || y == 0 then 0 else x * y

validMod :: Mod -> Bool
validMod m = case m of
  K x -> finite x
  Linear a b d -> all finite [a, b] && positive d
  Approach a b d -> all finite [a, b] && nonNegative d
  Then d a b -> nonNegative d && validMod a && validMod b
  Release r a -> positive r && validMod a
  Lfo w f -> finite f && w /= NoiseWave
  a :+ b -> validMod a && validMod b
  a :* b -> validMod a && validMod b
  _ -> True

-- | The longest release in a modulation (how long a note rings after note-off).
releaseOf :: Mod -> Double
releaseOf m = case m of
  Release r a -> max r (releaseOf a)
  Then _ a b -> max (releaseOf a) (releaseOf b)
  a :+ b -> max (releaseOf a) (releaseOf b)
  a :* b -> max (releaseOf a) (releaseOf b)
  _ -> 0

-- | A subtractive voice: oscillators summed into a resonant low-pass, an
-- optional high-pass, then amplitude. Everything that varies while a note
-- plays is a 'Mod': 'amp' (the engine applies nothing else, velocity
-- included), 'pitch' in semitones added to the note, and 'cutoff' in octaves
-- above C4. Unison voices are spread across the stereo field by 'stereoWidth'.
data Patch = Patch
  { oscillators :: [Osc]
  , amp :: Mod
  , pitch :: Mod
  , cutoff :: Mod
  , resonance :: Double, highpass :: Double, stereoWidth :: Double
  } deriving (Eq, Show)

-- | How a note varies while it sounds, over its sounding time @u@ from 0 to 1
-- (held at 1 through the release). The engine knows nothing of steps or beats:
-- 'Cycles' counts whole cycles over the note.
data Contour
  = Flat Double
  | Breaks [(Double, Double)]   -- ^ piecewise-linear @(u, value)@
  | Cycles Wave Double          -- ^ a unit sine or square, this many cycles over the note
  | Contour :+. Contour
  | Contour :*. Contour
  deriving (Eq, Show)
infixl 6 :+.
infixl 7 :*.

-- | 'pitchMod' is added to the pitch in octaves; 'loudness' multiplies the
-- amplitude. @Flat 0@ and @Flat 1@ mean "none", and take the plain path.
data Note = Note
  { start :: Double, hz :: Double, duration :: Double, sound :: Patch
  , velocity :: Double, gain :: Double, pan :: Double, send :: Double
  , pitchMod :: Contour, loudness :: Contour }
  deriving (Eq, Show)

-- | A note that doesn't vary.
note :: Double -> Double -> Double -> Patch -> Double -> Double -> Double -> Double -> Note
note t f d p v g pn s = Note t f d p v g pn s (Flat 0) (Flat 1)

contourAt :: Contour -> Double -> Double
contourAt c u = case c of
  Flat x -> x
  Breaks ps -> breaksAt ps
  Cycles w n ->
    let phase = n * u
    in case w of
         SquareWave -> if phase - fromIntegral (floor phase :: Int) < 0.5 then 1 else -1
         _ -> sin (2 * pi * phase)
  a :+. b -> contourAt a u + contourAt b u
  a :*. b -> contourAt a u * contourAt b u
  where
    breaksAt [] = 0
    breaksAt ((u0, y0) : more)
      | u <= u0 = y0
      | otherwise = go u0 y0 more
    go _ y [] = y
    go ua ya ((ub, yb) : later)
      | u <= ub = if ub == ua then yb else ya + (yb - ya) * (u - ua) / (ub - ua)
      | otherwise = go ub yb later

-- | Lower and upper bounds over the whole note, by interval arithmetic.
contourBounds :: Contour -> (Double, Double)
contourBounds c = case c of
  Flat x -> (x, x)
  Breaks [] -> (0, 0)
  Breaks ps -> (minimum (map snd ps), maximum (map snd ps))
  Cycles _ _ -> (-1, 1)
  a :+. b -> let (l1, h1) = contourBounds a; (l2, h2) = contourBounds b in (l1 + l2, h1 + h2)
  a :*. b -> let (l1, h1) = contourBounds a; (l2, h2) = contourBounds b
                 ps = [l1 * l2, l1 * h2, h1 * l2, h1 * h2]
             in (minimum ps, maximum ps)

validContour :: Contour -> Bool
validContour c = case c of
  Flat x -> finite x
  Breaks ps -> all (\(u, y) -> unitRange u && finite y) ps
  Cycles _ n -> finite n
  a :+. b -> validContour a && validContour b
  a :*. b -> validContour a && validContour b

data Timeline = Timeline { seconds :: Double, notes :: [Note] } deriving (Eq, Show)

finite, positive, nonNegative, unitRange :: Double -> Bool
finite x = not (isNaN x || isInfinite x)
positive x = finite x && x > 0
nonNegative x = finite x && x >= 0
unitRange x = nonNegative x && x <= 1

validPatch :: Patch -> Either String ()
validPatch p = do
  let oscOk (Osc _ st g u c) = finite st && nonNegative g && u >= 1 && nonNegative c
  unless (not (null (oscillators p)) && all oscOk (oscillators p))
    (Left "patch needs oscillators with nonnegative gain and spread, and unison >= 1")
  unless (all validMod [amp p, pitch p, cutoff p])
    (Left "patch modulations need finite values and positive times")
  unless (fst (modBounds (amp p)) >= 0) (Left "a patch's amplitude cannot go below 0")
  unless (nonNegative (resonance p) && resonance p < 1
          && nonNegative (highpass p) && unitRange (stereoWidth p))
    (Left "invalid patch filter or stereo settings")

-- | Unpitched: a noise oscillator, or a pitch modulation that starts away from
-- the note (a drop or sweep into it). A vibrato starts at 0, so it isn't.
percussive :: Patch -> Bool
percussive p = any ((== NoiseWave) . wave) (oscillators p) || modAt (pitch p) (1 / 0) 1 0 0 /= 0

-- Patches ---------------------------------------------------------------------

-- | Single saw, open filter, organ-like envelope: a starting point for record updates.
basicPatch :: Patch
basicPatch = Patch
  { oscillators = [Osc SawWave 0 1 1 0]
  , amp = Velocity :* adsr 0.005 0 1 0.05
  , pitch = K 0
  , cutoff = cutoffHz 20000, resonance = 0, highpass = 0, stereoWidth = 0 }

-- | Bright plucked saw; velocity opens the filter.
pluck :: Patch
pluck = basicPatch
  { oscillators = [Osc SawWave 0 1 3 14, Osc SquareWave 12 0.25 1 0]
  , amp = Velocity :* adsr 0.002 0.4 0 0.3
  , cutoff = filterCutoff 450 0.5 (adsr 0.001 0.2 0 0.2) 3.5 1.2, resonance = 0.3
  , stereoWidth = 0.6 }

-- | Seven detuned saws plus a sub-octave, slow attack, wide stereo.
supersaw :: Patch
supersaw = basicPatch
  { oscillators = [Osc SawWave 0 1 7 38, Osc SawWave (-12) 0.35 1 0]
  , amp = Velocity :* adsr 0.4 0.6 0.8 1.2
  , cutoff = filterCutoff 900 0.3 (adsr 0.5 1.5 0.4 1) 1.5 2, resonance = 0.15
  , stereoWidth = 1 }

-- | Two slowly beating saws over a sine sub, low-passed.
reeseBass :: Patch
reeseBass = basicPatch
  { oscillators = [Osc SawWave 0 1 2 22, Osc SineWave (-12) 0.8 1 0]
  , amp = Velocity :* adsr 0.005 0.3 0.8 0.08
  , cutoff = filterCutoff 160 0 (adsr 0.002 0.25 0 0.1) 2.5 1, resonance = 0.25
  , stereoWidth = 0.3 }

subBass :: Patch
subBass = basicPatch
  { oscillators = [Osc SineWave 0 1 1 0, Osc TriangleWave 0 0.12 1 0]
  , amp = Velocity :* adsr 0.01 0 1 0.1 }

-- | Sine with a fast pitch drop: tune the note to the body frequency (~40-60 Hz).
kick :: Patch
kick = basicPatch
  { oscillators = [Osc SineWave 0 1 1 0]
  , amp = Velocity :* adsr 0.001 0.35 0 0.05
  , pitch = Approach 36 0 (4.6 * 0.035) }

-- | Noise burst over a pitched triangle body.
snare :: Patch
snare = basicPatch
  { oscillators = [Osc NoiseWave 0 1 2 0, Osc TriangleWave 0 0.6 1 0]
  , amp = Velocity :* adsr 0.001 0.22 0 0.08
  , pitch = Approach 7 0 (4.6 * 0.03)
  , cutoff = cutoffHz 8000, highpass = 180, stereoWidth = 0.3 }

-- | Closed hi-hat: short high-passed noise (pitch is ignored).
hat :: Patch
hat = basicPatch
  { oscillators = [Osc NoiseWave 0 1 2 0]
  , amp = Velocity :* adsr 0.001 0.05 0 0.03
  , highpass = 7000, stereoWidth = 0.4 }

-- | Noise swell whose resonant filter sweeps up over about four seconds.
noiseSweep :: Patch
noiseSweep = basicPatch
  { oscillators = [Osc NoiseWave 0 1 2 0]
  , amp = Velocity :* adsr 3 0 1 0.3
  , cutoff = filterCutoff 250 0 (adsr 4 0 1 0.3) 5.5 0, resonance = 0.55
  , highpass = 120, stereoWidth = 1 }

sinePatch, sawPatch, squarePatch, pianoPatch :: Patch
sinePatch = basicPatch { oscillators = [Osc SineWave 0 1 1 0], amp = Velocity :* adsr 0.005 0 1 0.015 }
sawPatch = sinePatch { oscillators = [Osc SawWave 0 1 1 0] }
squarePatch = sinePatch { oscillators = [Osc SquareWave 0 1 1 0] }
-- A synthesized piano-like preset, not a sampled acoustic piano.
pianoPatch = basicPatch
  { oscillators = [Osc SineWave 0 1 1 0, Osc SineWave 12 0.35 1 0, Osc SineWave 19 0.12 1 0]
  , amp = Velocity :* adsr 0.002 1.2 0.15 0.15 }

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

-- | The checks every mix must pass before rendering.
validMix :: Mix -> Either String ()
validMix mix = do
  let rv = mixReverb mix
  unless (mixRate mix >= 8000 && mixRate mix <= 192000) (Left "sample rate must be 8000..192000")
  unless (all unitRange [roomSize rv, damping rv, width rv]
          && nonNegative (wet rv) && nonNegative (preDelay rv))
    (Left "reverb room, damping and width must be within 0..1; wet and pre-delay nonnegative")
  unless (nonNegative (mixDrive mix) && nonNegative (mixTail mix) && mixPeak mix > 0 && mixPeak mix <= 1)
    (Left "mix drive and tail must be nonnegative and peak within (0, 1]")

-- | Dry, normalised to a 0.8 peak, no tail.
defaultMix :: Int -> Mix
defaultMix rate = Mix rate noReverb 0 0.8 0 True

tailOf :: Patch -> Double
tailOf = max 0.002 . releaseOf . amp

-- | Stereo render. Each note is synthesised only over its own span into
-- left/right/send buffers; the send bus feeds the reverb, then the master stage.
-- Notes may ring past the gate by their release time. Base pitches at or above
-- Nyquist are rejected.
-- | Synthesize a validated physical timeline, shared by legacy pieces and scores.
renderTimelineMix :: Mix -> Timeline -> Either String (VU.Vector Double, VU.Vector Double)
renderTimelineMix mix (Timeline total ns) = do
  unless (nonNegative total) (Left "timeline length must be finite and nonnegative")
  forM_ ns $ \v -> do
    unless (nonNegative (start v) && positive (duration v) && positive (hz v)
            && unitRange (velocity v) && nonNegative (gain v) && nonNegative (send v)
            && finite (pan v) && abs (pan v) <= 1) (Left "invalid timeline note")
    unless (validContour (pitchMod v) && validContour (loudness v) && fst (contourBounds (loudness v)) >= 0)
      (Left "invalid note curve")
    validPatch (sound v)
  let rate = mixRate mix
      fr = fromIntegral rate :: Double
      rv = mixReverb mix
  validMix mix
  let highest v = hz v * 2 ** max 0 (snd (contourBounds (pitchMod v)))
  unless (all ((< fr / 2) . highest) ns) (Left "pitch reaches Nyquist frequency")
  let end = maximum (total : [start v + duration v + tailOf (sound v) | v <- ns]) + mixTail mix
  unless (finite (end * fr) && end * fr < fromIntegral (maxBound :: Int))
    (Left "rendered sample count exceeds platform range")
  let len = ceiling (end * fr) :: Int
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
  when (VU.any (not . finite) outL || VU.any (not . finite) outR) (Left "synthesis produced a nonfinite sample")
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
renderNote fr seed v = renderSynth fr seed v (sound v)

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
  let (i0, i1) = sampleRange fr (MV.length left) (start v) (start v + duration v + tailOf p)
      (gl, gr) = panGains (pan v)
      -- The patch's modulations, compiled for this note's note-off and velocity.
      ampAt = compileMod (duration v) (velocity v) (amp p)
      pitchAt = compileMod (duration v) (velocity v) (pitch p)
      cutoffAt = compileMod (duration v) (velocity v) (cutoff p)
      patchPitchMoves = pitch p /= K 0
      pitchKeyed = usesKey (pitch p)
      keyed = usesKey (amp p) || usesKey (cutoff p)
      -- Skipped only when the cutoff is a constant at or above 0.45 x the rate.
      lowpassOn = case cutoff p of
        K c -> 261.63 * 2 ** c < 0.45 * fr
        _ -> True
      pitchCurved = pitchMod v /= Flat 0
      kq = max 0.05 (sqrt 2 * (1 - resonance p))
      hpG = tan (pi * min (0.45 * fr) (highpass p) / fr)
      oscillate !j !f0 !accL !accR !rng
        | j >= nv = pure (accL, accR, rng)
        | otherwise = do
            let (code, mult, g, vl, vr) = VU.unsafeIndex table j
                dt = min 0.49 (f0 * mult / fr)
                rng' = if code == 4 then xorshift rng else rng
            ph <- MV.unsafeRead phases j
            let s = oscSample code ph dt rng'
            MV.unsafeWrite phases j (wrap (ph + dt))
            oscillate (j + 1) f0 (accL + g * vl * s) (accR + g * vr * s) rng'
      go !i !l1 !l2 !r1 !r2 !h1 !h2 !h3 !h4 !rng
        | i >= i1 = pure ()
        | otherwise = do
            let t = fromIntegral i / fr - start v
                u = max 0 (min 1 (t / duration v))
                -- The score's pitch first, then the patch's (whose Key is the score's pitch).
                scored
                  | not pitchCurved = hz v
                  | otherwise = hz v * 2 ** contourAt (pitchMod v) u
                freqNow
                  | not patchPitchMoves = scored
                  | otherwise = scored * 2 ** (pitchAt t (if pitchKeyed then logBase 2 (scored / 261.63) else 0) / 12)
                key = if keyed then logBase 2 (freqNow / 261.63) else 0
            (oscL, oscR, rng') <- oscillate 0 freqNow 0 0 rng
            let fc = 261.63 * 2 ** cutoffAt t key
                lpG = tan (pi * min (0.45 * fr) fc / fr)
                (lowL, l1', l2') = if lowpassOn then lowpass lpG kq oscL l1 l2 else (oscL, l1, l2)
                (lowR, r1', r2') = if lowpassOn then lowpass lpG kq oscR r1 r2 else (oscR, r1, r2)
                (outL, h1', h2') = if highpass p > 0 then highpassed hpG lowL h1 h2 else (lowL, h1, h2)
                (outR, h3', h4') = if highpass p > 0 then highpassed hpG lowR h3 h4 else (lowR, h3, h4)
                e
                  | loudness v == Flat 1 = gain v * ampAt t key
                  | otherwise = gain v * contourAt (loudness v) u * ampAt t key
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


-- | Stereo timeline export for the unitless score API.
writeTimelineMix :: FilePath -> Mix -> Timeline -> IO ()
writeTimelineMix path mix timeline = exportAs path $ \out -> do
  (l, r) <- either (ioError . userError) pure (renderTimelineMix mix timeline)
  pcmWav out (mixRate mix) [l, r]



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
