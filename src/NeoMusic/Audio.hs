{-# LANGUAGE BangPatterns #-}
-- | The synthesis engine. It knows only Hz, seconds and patches: no steps,
-- beats, scores or pieces. Every front end lowers to a 'Timeline' first.
module NeoMusic.Audio
  ( -- * Voices
    Voice(..), validVoice, percussive, expandClassic
    -- * Patches
  , Wave(..), Osc(..), Patch(..), validPatch
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
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
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
  { start :: Double, hz :: Double, duration :: Double, sound :: Voice
  , velocity :: Double, gain :: Double, pan :: Double, send :: Double
  , pitchMod :: Contour, loudness :: Contour }
  deriving (Eq, Show)

-- | A note that doesn't vary.
note :: Double -> Double -> Double -> Voice -> Double -> Double -> Double -> Double -> Note
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

-- Voices ----------------------------------------------------------------------

-- | A voice: an audio signal built from three sources and a few shapers, each
-- shaper acting on the voice inside it (see neomusic-voice.md). Sources sound
-- at unit amplitude, at the pitch that reaches them: the note's, raised by every
-- 'Bend' above. Inside a 'Bend', 'Key' is that raised pitch. A 'Classic' patch
-- is a named shorthand; its meaning is 'expandClassic'.
data Voice
  = Tone Wave                      -- ^ a band-limited sine, saw, square or triangle
  | Noise                          -- ^ white noise, the same for the same note
  | Plucked Double Double          -- ^ a plucked string: seconds to fall 60 dB, brightness 0..1
  | Gain Mod Voice                 -- ^ multiplied by a modulation
  | Bend Mod Voice                 -- ^ everything inside sounds this many semitones higher
  | Lowpass Mod Double Voice       -- ^ cutoff (octaves above C4) and resonance 0..1
  | Highpass Mod Voice             -- ^ cutoff (octaves above C4)
  | Bandpass Mod Double Voice      -- ^ centre (octaves above C4) and width in octaves; unity at the centre
  | Unison Int Double Double Voice -- ^ n copies detuned across c cents in total, spread across stereo width w
  | Pan Mod Voice                  -- ^ stereo position, -1..1
  | Voice :& Voice                 -- ^ mixed
  | Classic Patch                  -- ^ the subtractive voice: see 'expandClassic'
  deriving (Eq, Show)
infixl 6 :&

-- | What a classic patch means: its oscillators (each a tone or noise, raised,
-- scaled and spread), mixed, low-passed, high-passed, then shaped by the
-- amplitude, all under the patch's pitch. The engine plays 'Classic' patches by
-- a faster route that a test holds to this expansion.
expandClassic :: Patch -> Voice
expandClassic p = bent (Gain (amp p) (highpassed (Lowpass (cutoff p) (resonance p) mixed)))
  where
    bent v = if pitch p == K 0 then v else Bend (pitch p) v
    highpassed v = if highpass p > 0 then Highpass (cutoffHz (highpass p)) v else v
    mixed = case map oscillator (oscillators p) of
      [] -> Gain (K 0) (Tone SineWave)
      vs -> foldr1 (:&) vs
    oscillator o = spread (scaled (raised (if wave o == NoiseWave then Noise else Tone (wave o))))
      where
        raised v = if semitones o == 0 then v else Bend (K (semitones o)) v
        scaled v = if oscGain o == 1 then v else Gain (K (oscGain o)) v
        spread v = if unison o == 1 then v else Unison (unison o) (detuneCents o) (stereoWidth p) v

validVoice :: Voice -> Either String ()
validVoice voice = case voice of
  Tone w -> unless (w /= NoiseWave) (Left "noise is a source of its own, not a tone")
  Noise -> pure ()
  Plucked d b -> unless (positive d && unitRange b) (Left "a plucked string needs a positive decay and brightness within 0..1")
  Gain m v -> modOk m >> validVoice v
  Bend m v -> modOk m >> validVoice v
  Lowpass m r v -> do
    modOk m
    unless (nonNegative r && r < 1) (Left "resonance must be within 0..1 (below 1)")
    validVoice v
  Highpass m v -> modOk m >> validVoice v
  Bandpass m w v -> do
    modOk m
    unless (positive w) (Left "a band's width must be positive")
    validVoice v
  Unison n c w v -> do
    unless (n >= 1 && nonNegative c && unitRange w) (Left "unison needs at least 1 copy, a nonnegative detune and width within 0..1")
    validVoice v
  Pan m v -> do
    modOk m
    let (lo, hi) = modBounds m
    unless (lo >= -1 && hi <= 1) (Left "pan must stay within -1..1")
    validVoice v
  a :& b -> validVoice a >> validVoice b
  Classic p -> validPatch p
  where modOk m = unless (validMod m) (Left "voice modulations need finite values and positive times")

-- | Unpitched: every part is. Noise is unpitched, and so is everything under a
-- 'Bend' that starts away from 0 (a drop or sweep into the note); a vibrato
-- starts at 0, so it isn't.
percussive :: Voice -> Bool
percussive voice = case voice of
  Tone _ -> False
  Noise -> True
  Plucked _ _ -> False
  Bend m v -> modAt m (1 / 0) 1 0 0 /= 0 || percussive v
  Gain _ v -> percussive v
  Lowpass _ _ v -> percussive v
  Highpass _ v -> percussive v
  Bandpass _ _ v -> percussive v
  Unison _ _ _ v -> percussive v
  Pan _ v -> percussive v
  a :& b -> percussive a && percussive b
  Classic p -> percussive (expandClassic p)

-- | How long a voice sounds after note-off: its longest amplitude release
-- (at least 2 ms, so a note never stops on a click).
voiceTail :: Voice -> Double
voiceTail = max 0.002 . go
  where
    go voice = case voice of
      Gain m v -> max (releaseOf m) (go v)
      Bend _ v -> go v
      Lowpass _ _ v -> go v
      Highpass _ v -> go v
      Bandpass _ _ v -> go v
      Unison _ _ _ v -> go v
      Pan _ v -> go v
      a :& b -> max (go a) (go b)
      Classic p -> releaseOf (amp p)
      _ -> 0

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
    validVoice (sound v)
  let rate = mixRate mix
      fr = fromIntegral rate :: Double
      rv = mixReverb mix
  validMix mix
  let highest v = hz v * 2 ** max 0 (snd (contourBounds (pitchMod v)))
  unless (all ((< fr / 2) . highest) ns) (Left "pitch reaches Nyquist frequency")
  let end = maximum (total : [start v + duration v + voiceTail (sound v) | v <- ns]) + mixTail mix
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

-- | A classic patch takes the fast path; any other voice, the general one.
renderNote :: Double -> Word32 -> Note -> Buffer s -> Buffer s -> Buffer s -> ST s ()
renderNote fr seed v = case sound v of
  Classic p -> renderSynth fr seed v p
  voice -> renderVoice fr seed v voice

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
  let (i0, i1) = sampleRange fr (MV.length left) (start v) (start v + duration v + voiceTail (Classic p))
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
  go i0 0 0 0 0 0 0 0 0 (noteSeed seed)

-- | The note's own noise generator state, never 0.
noteSeed :: Word32 -> Word32
noteSeed seed = let s0 = (seed * 2654435761) `xor` 0x9E3779B9 in if s0 == 0 then 1 else s0


-- The general voice ----------------------------------------------------------

-- | A voice compiled for one note: modulations prepared for its note-off and
-- velocity, constants folded, unison copies laid out, and every source and
-- filter given its own state.
data Node s
  = NTone !Int !(MV.MVector s Double)                 -- wave code, phase
  | NNoise !(MV.MVector s Word32)                     -- the note's generator, shared in tree order
  | NString !(PluckedString s)
  | NScale !Double !Double (Node s)                   -- constant gains, left and right
  | NGain !Prepared !Bool (Node s)                    -- modulation, follows the key?
  | NRaise !Double (Node s)                           -- a constant bend, as a frequency ratio
  | NBend !Prepared !Bool (Node s)
  | NFilter !Int !Coefficient !Double !Bool !(MV.MVector s Double) (Node s)  -- low/high/band, cutoff, k, mono, state
  | NPan !Prepared !Bool (Node s)
  | NAdd (Node s) (Node s)

-- | A filter's cutoff: fixed (its tan-warped gain, computed once) or moving.
data Coefficient = Fixed !Double | Moving !Prepared !Bool

-- | One output on both sides: the mono parts are filtered once.
monoNode :: Node s -> Bool
monoNode node = case node of
  NScale l r c -> l == r && monoNode c
  NGain _ _ c -> monoNode c
  NRaise _ c -> monoNode c
  NBend _ _ c -> monoNode c
  NFilter _ _ _ mono _ _ -> mono
  NPan _ _ _ -> False
  NAdd a b -> monoNode a && monoNode b
  _ -> True

constantOf :: Prepared -> Maybe Double
constantOf p = case p of
  PK x -> Just x
  PAdd a b -> (+) <$> constantOf a <*> constantOf b
  PMul a b -> (*) <$> constantOf a <*> constantOf b
  _ -> Nothing

keyOf :: Double -> Double
keyOf f = logBase 2 (f / 261.63)
{-# INLINE keyOf #-}

-- | The state of a plucked string (Karplus-Strong): a delay line one period
-- long that grows if the pitch falls, its write count, the excitation, and
-- the loop settings for the last frequency (frequency, delay, loop filter,
-- loop gain).
data PluckedString s = PluckedString
  { stringLine :: STRef s (MV.MVector s Double)
  , stringCount :: MV.MVector s Int
  , stringBurst :: VU.Vector Double
  , stringLoop :: MV.MVector s Double
  , stringDecay :: Double, stringBrightness :: Double }

-- | The excitation: one period of the note's noise, smoothed by the
-- brightness, without its mean, at unit peak.
newString :: Double -> Word32 -> Double -> Double -> Double -> ST s (PluckedString s)
newString fr seed f0 decay brightness = do
  let size = max 2 (round (fr / f0)) :: Int
      c = 0.1 + 0.9 * brightness
      raw = take size (drop 1 (iterate xorshift seed))
      smoothed = drop 1 (scanl (\e r -> e + c * ((fromIntegral r / 2147483647.5 - 1) - e)) 0 raw)
      mean = sum smoothed / fromIntegral size
      centred = map (subtract mean) smoothed
      peak = maximum (map abs centred)
      burst = VU.fromList (map (/ (if peak > 0 then peak else 1)) centred)
  line <- MV.replicate (2 * size + 8) 0
  lineRef <- newSTRef line
  count <- MV.replicate 1 0
  loop <- MV.replicate 4 0
  pure (PluckedString lineRef count burst loop decay brightness)

-- | One sample of a plucked string at frequency @f@. The loop is the delay, a
-- linear interpolation, and a two-point loss filter @(1-s) + s z^-1@. The
-- brightness sets @s@ (0.5 at 0, none at 1), lowered where needed so the
-- fundamental can still ring for the whole decay; the loop gain then makes
-- the fundamental fall exactly 60 dB in the decay.
stepString :: Double -> PluckedString s -> Double -> ST s Double
stepString fr st f = do
  n <- MV.unsafeRead (stringCount st) 0
  lastF <- MV.unsafeRead (stringLoop st) 0
  when (f /= lastF) $ do
    let w = 2 * pi * f / fr
        target = 10 ** (-3 / (f * stringDecay st))
        cw = 1 - cos w
        q = (1 - target * target) / (2 * cw)
        sMax = if q >= 0.25 then 0.5 else (1 - sqrt (1 - 4 * q)) / 2
        s = min (0.5 * (1 - stringBrightness st)) sMax
        d = max 1 (fr / f - s)
        a = wrap (negate d)
        lossH = sqrt (1 - 2 * s * (1 - s) * cw)
        lossI = sqrt (1 - 2 * a * (1 - a) * cw)
        rho = min 1 (target / (lossH * lossI))
    MV.unsafeWrite (stringLoop st) 0 f
    MV.unsafeWrite (stringLoop st) 1 d
    MV.unsafeWrite (stringLoop st) 2 s
    MV.unsafeWrite (stringLoop st) 3 rho
  d <- MV.unsafeRead (stringLoop st) 1
  s <- MV.unsafeRead (stringLoop st) 2
  rho <- MV.unsafeRead (stringLoop st) 3
  line0 <- readSTRef (stringLine st)
  line <- if ceiling d + 3 < MV.length line0 then pure line0 else do
    let cap0 = MV.length line0
        cap = 2 * (ceiling d + 3)
    grown <- MV.replicate cap 0
    forM_ [max 0 (n - cap0) .. n - 1] $ \k -> MV.unsafeRead line0 (k `mod` cap0) >>= MV.unsafeWrite grown (k `mod` cap)
    writeSTRef (stringLine st) grown
    pure grown
  let cap = MV.length line
      pos = fromIntegral n - d
      k0 = floor pos :: Int
      a = pos - fromIntegral k0
      at k = if k < 0 then pure 0 else MV.unsafeRead line (k `mod` cap)
  ym <- at (k0 - 1)
  y0 <- at k0
  y1 <- at (k0 + 1)
  let here = (1 - a) * y0 + a * y1
      before = (1 - a) * ym + a * y0
      burst = stringBurst st
      x = if n < VU.length burst then VU.unsafeIndex burst n else 0
      out = x + rho * ((1 - s) * here + s * before)
  MV.unsafeWrite line (n `mod` cap) out
  MV.unsafeWrite (stringCount st) 0 (n + 1)
  pure out

-- | Compile a voice for one note: @gate@ and @vel@ are its note-off and
-- velocity, @f0@ the frequency reaching this part at note-on, @phase@ the
-- starting phase (spread by unison copies), @seeds@ counts the strings.
compileVoice :: Double -> Double -> Double -> Word32 -> MV.MVector s Word32 -> STRef s Word32
             -> Double -> Double -> Voice -> ST s (Node s)
compileVoice fr gate vel seed rng seeds = build
  where
    prep = prepare gate vel
    build !phase !f0 voice = case voice of
      Tone w -> NTone (waveCode w) <$> MV.replicate 1 phase
      Noise -> pure (NNoise rng)
      Plucked d b -> do
        k <- readSTRef seeds
        writeSTRef seeds (k + 1)
        NString <$> newString fr (noteSeed (seed * 7919 + k * 104729)) f0 d b
      Gain m v -> let p = prep m in case constantOf p of
        Just g -> NScale g g <$> build phase f0 v
        Nothing -> NGain p (usesKey m) <$> build phase f0 v
      Bend m v -> let p = prep m in case constantOf p of
        Just x -> let r = 2 ** (x / 12) in NRaise r <$> build phase (f0 * r) v
        Nothing -> NBend p (usesKey m) <$> build phase (f0 * 2 ** (evalPrepared gate p 0 (keyOf f0) / 12)) v
      Lowpass m r v -> case constantOf (prep m) of
        Just c | 261.63 * 2 ** c >= 0.45 * fr -> build phase f0 v   -- at or above 0.45 x the rate: passes everything
        _ -> filterNode 0 m (max 0.05 (sqrt 2 * (1 - r))) phase f0 v
      Highpass m v -> filterNode 1 m (sqrt 2) phase f0 v
      Bandpass m w v -> filterNode 2 m ((2 ** w - 1) / sqrt (2 ** w)) phase f0 v
      Unison n c w v
        | n <= 1 -> build phase f0 v
        | otherwise -> foldr1 NAdd <$> mapM copy [0 .. n - 1]
        where
          copy j = do
            let x = fromIntegral j / fromIntegral (n - 1) :: Double
                r = 2 ** (c * (x - 0.5) / 100 / 12)
                (vl, vr) = if w == 0 then (1, 1) else panGains (w * (2 * x - 1))
                scale = 1 / sqrt (fromIntegral n)
            NRaise r . NScale (scale * vl) (scale * vr) <$> build (wrap (phase + fromIntegral j * 0.618034 + 0.1)) (f0 * r) v
      Pan m v -> NPan (prep m) (usesKey m) <$> build phase f0 v
      a :& b -> NAdd <$> build phase f0 a <*> build phase f0 b
      Classic p -> build phase f0 (expandClassic p)
    filterNode kind m k phase f0 v = do
      child <- build phase f0 v
      state <- MV.replicate 4 0
      let p = prep m
          coefficient = maybe (Moving p (usesKey m)) (Fixed . cutoffGain fr) (constantOf p)
      pure (NFilter kind coefficient k (monoNode child) state child)

-- | A cutoff in octaves above C4 as the filter's warped gain, at most 0.45 x the rate.
cutoffGain :: Double -> Double -> Double
cutoffGain fr octaves = tan (pi * min (0.45 * fr) (261.63 * 2 ** octaves) / fr)

-- | One stereo sample of a compiled voice at @t@ seconds since note-on, with
-- frequency @f@ reaching it.
stepVoice :: Double -> Double -> Node s -> Double -> Double -> ST s (Double, Double)
stepVoice fr gate = go
  where
    modNow p keyed t f = evalPrepared gate p t (if keyed then keyOf f else 0)
    go node !t !f = case node of
      NTone code phase -> do
        ph <- MV.unsafeRead phase 0
        let dt = min 0.49 (f / fr)
            x = oscSample code ph dt 0
        MV.unsafeWrite phase 0 (wrap (ph + dt))
        pure (x, x)
      NNoise rng -> do
        r <- MV.unsafeRead rng 0
        let r' = xorshift r
            x = fromIntegral r' / 2147483647.5 - 1
        MV.unsafeWrite rng 0 r'
        pure (x, x)
      NString st -> do
        x <- stepString fr st f
        pure (x, x)
      NScale l r c -> do
        (a, b) <- go c t f
        pure (l * a, r * b)
      NGain p keyed c -> do
        (a, b) <- go c t f
        let g = modNow p keyed t f
        pure (g * a, g * b)
      NRaise r c -> go c t (f * r)
      NBend p keyed c -> go c t (f * 2 ** (modNow p keyed t f / 12))
      NFilter kind coefficient k mono state c -> do
        (a, b) <- go c t f
        let g = case coefficient of
              Fixed x -> x
              Moving p keyed -> cutoffGain fr (modNow p keyed t f)
        ya <- filterStep kind g k state 0 a
        if mono then pure (ya, ya) else do
          yb <- filterStep kind g k state 2 b
          pure (ya, yb)
      NPan p keyed c -> do
        (a, b) <- go c t f
        let (gl, gr) = panGains (max (-1) (min 1 (modNow p keyed t f)))
        pure (gl * a, gr * b)
      NAdd x y -> do
        (a, b) <- go x t f
        (c, d) <- go y t f
        pure (a + c, b + d)

filterStep :: Int -> Double -> Double -> MV.MVector s Double -> Int -> Double -> ST s Double
filterStep kind g k state at x = do
  ic1 <- MV.unsafeRead state at
  ic2 <- MV.unsafeRead state (at + 1)
  let (band, low, s1, s2) = svf g k x ic1 ic2
  MV.unsafeWrite state at s1
  MV.unsafeWrite state (at + 1) s2
  pure $ case kind of
    0 -> low
    1 -> x - k * band - low
    _ -> k * band
{-# INLINE filterStep #-}

renderVoice :: Double -> Word32 -> Note -> Voice -> Buffer s -> Buffer s -> Buffer s -> ST s ()
renderVoice fr seed v voice left right sends = do
  rng <- MV.replicate 1 (noteSeed seed)
  seeds <- newSTRef 0
  let curved = pitchMod v /= Flat 0
      freqAt u = if curved then hz v * 2 ** contourAt (pitchMod v) u else hz v
  node <- compileVoice fr (duration v) (velocity v) seed rng seeds 0 (freqAt 0) voice
  let (i0, i1) = sampleRange fr (MV.length left) (start v) (start v + duration v + voiceTail voice)
      (gl, gr) = panGains (pan v)
      go !i
        | i >= i1 = pure ()
        | otherwise = do
            let t = fromIntegral i / fr - start v
                u = max 0 (min 1 (t / duration v))
            (a, b) <- stepVoice fr (duration v) node t (freqAt u)
            let e
                  | loudness v == Flat 1 = gain v
                  | otherwise = gain v * contourAt (loudness v) u
                yl = e * gl * a
                yr = e * gr * b
            addTo left i yl
            addTo right i yr
            addTo sends i (send v * (yl + yr) / 2)
            go (i + 1)
  go i0

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
