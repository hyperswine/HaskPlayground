-- | 30-second anime / J-pop style score for the M3x8 screw cinematic
-- (Playground/high-level-stuff/screw-cinematic). Same arc as screw-bgm: quiet
-- open, build into the hex socket, impact on the 20 s pull-back, ring out.
--
-- 144 BPM, 18 bars of four beats = exactly 30 s. D major / B minor.
--
-- >  bars 1-4    0.0-6.7 s   intro: the chorus hook previewed on a music-box bell
-- >  bars 5-8    6.7-13.3 s  verse: lead over IVM7-III7-vim7-v7 I7 ("marusa"),
-- >                          syncopated 16th bass, offbeat piano stabs
-- >  bars 9-12   13.3-20 s   pre-chorus: ii-iii-IV-Vsus4 V, climbing melody, 16th arp,
-- >                          snare roll, noise + quarter-tone risers, half-beat gap
-- >  bars 13-16  20-26.7 s   chorus on the "royal road" IVM7-V-iiim7-vim7: boom, crash,
-- >                          pumping supersaw, driving octave bass + sub, bell doubling
-- >  bars 17-18  26.7-30 s   IV-V-I tag resolving to a high D over Dadd9, glints ring out
--
-- @screw-anime [out.wav]@ renders; @--check@ validates bars; @--stems@ prints levels.
module Main (main) where

import Control.Monad (forM_, unless)
import qualified Data.Map.Strict as M
import Data.Ratio (denominator)
import qualified Data.Vector.Unboxed as VU
import NeoMusic.Audio
  ( Envelope(..), Mix(..), Osc(..), Patch(..), Reverb(..), Wave(..)
  , basicPatch, hall, hat, kick, noiseSweep, pluck, reeseBass, renderTimelineMix, snare, subBass
  , supersaw, writeTimelineMix )
import NeoMusic.Pitch (Step(..), Tuning(..))
import NeoMusic.Score (Event(..), Instrument(..), Performance(..), Phrase, Score(..), hit, hush)
import qualified NeoMusic.Score as S
import System.Environment (getArgs)
import Text.Printf (printf)

-- | Steps above D4, and quarter-tones for the riser.
d4, quarterTones :: Tuning
d4 = Tuning 293.66 (2 ** (1 / 12))
quarterTones = Tuning 293.66 (2 ** (1 / 24))

at :: String -> Rational -> Phrase -> Score
at name offset phrase = Silence offset :>>: Line name phrase

together :: [Score] -> Score
together = foldr (:||:) (Silence 0)

-- Harmony ---------------------------------------------------------------------

-- | Bass root (octave 2) and a close upper voicing around D4.
data Chord = Chord { root :: Int, voicing :: [Int] }

gM7, aMaj, fsm7, bm7, fs7, am7, d7, em7, asus4, dAdd9 :: Chord
gM7   = Chord (-19) [-3, 0, 4, 7]    -- G  | B D F# A   (maj9 colour)
aMaj  = Chord (-17) [-1, 2, 7, 9]    -- A  | C# E A B   (add9)
fsm7  = Chord (-20) [-1, 2, 4, 7]    -- F# | C# E F# A
bm7   = Chord (-15) [0, 4, 7, 9]     -- B  | D F# A B
fs7   = Chord (-20) [-1, 2, 4, 8]    -- F# | C# E F# A#
am7   = Chord (-17) [-2, 2, 5, 7]    -- A  | C E G A
d7    = Chord (-24) [0, 4, 7, 10]    -- D  | D F# A C
em7   = Chord (-22) [-3, 0, 2, 5]    -- E  | B D E G
asus4 = Chord (-17) [0, 2, 7, 9]     -- A  | D E A B
dAdd9 = Chord (-24) [4, 7, 12, 14]   -- D  | F# A D E

-- | Each bar as (chord, beats) segments.
progression :: [[(Chord, Rational)]]
progression =
  map whole [gM7, aMaj, fsm7, bm7]                           -- 1-4   intro
  ++ [whole gM7, whole fs7, whole bm7, [(am7, 2), (d7, 2)]]  -- 5-8   verse
  ++ map whole [em7, fsm7, gM7] ++ [[(asus4, 2), (aMaj, 2)]]  -- 9-12  pre-chorus
  ++ map whole [gM7, aMaj, fsm7, bm7]                        -- 13-16 chorus
  ++ [[(gM7, 2), (aMaj, 2)], whole dAdd9]                    -- 17-18 tag
  where whole c = [(c, 4)]

barCount :: Int
barCount = length progression

segments :: Int -> [(Chord, Rational)]
segments b = progression !! (b - 1)

-- | Root of the next bar's first chord, for bass approach notes.
nextRoot :: Int -> Int
nextRoot b = case drop b progression of
  ((c, _) : _) : _ -> root c
  _ -> root dAdd9

data Section = Intro | Verse | Build | Chorus | Tag deriving Eq

section :: Int -> Section
section b
  | b <= 4 = Intro
  | b <= 8 = Verse
  | b <= 12 = Build
  | b <= 16 = Chorus
  | otherwise = Tag

-- | Bar 12 ends with half a beat of silence before the chorus lands.
gapBar :: Int
gapBar = 12

-- | Rising level through the pre-chorus, 0.6 .. 0.9.
lift :: Int -> Double
lift b = 0.6 + 0.1 * fromIntegral (b - 9)

-- Melody ----------------------------------------------------------------------

-- | Melody as (step, beats); Nothing is a rest.
type Tune = [(Maybe Int, Rational)]

infix 1 ~>
(~>) :: Int -> Rational -> (Maybe Int, Rational)
x ~> d = (Just x, d)

pause :: Rational -> (Maybe Int, Rational)
pause d = (Nothing, d)

-- | Chorus hook, one bar per chord of the royal road. Peaks on C#6 (step 23)
-- on the and-of-one in bar 3, then falls back to D5.
hook :: [Tune]
hook =
  [ [19 ~> 3/4, 21 ~> 3/4, 19 ~> 1/2, 16 ~> 1, 14 ~> 1/2, 16 ~> 1/2]    -- GM7
  , [19 ~> 3/2, 16 ~> 1/2, 14 ~> 1/2, 12 ~> 1/2, 14 ~> 1/2, 16 ~> 1/2]  -- A
  , [21 ~> 1/2, 23 ~> 1, 21 ~> 1/2, 19 ~> 1/2, 21 ~> 1, 16 ~> 1/2]      -- F#m7
  , [19 ~> 3/4, 16 ~> 3/4, 14 ~> 1/2, 12 ~> 1/2, 14 ~> 1/2, 12 ~> 1]    -- Bm7
  ]

melody :: Int -> Tune
melody b = case b of
  5  -> [pause (1/2), 7 ~> 1/2, 9 ~> 1/2, 12 ~> 1/2, 14 ~> 1, 12 ~> 1/2, 9 ~> 1/2]
  6  -> [8 ~> 1, 11 ~> 1/2, 14 ~> 1/2, 11 ~> 1, 8 ~> 1/2, 4 ~> 1/2]       -- A# colours the F#7
  7  -> [pause (1/2), 4 ~> 1/2, 9 ~> 1/2, 11 ~> 1/2, 12 ~> 1, 14 ~> 1/2, 12 ~> 1/2]
  8  -> [12 ~> 1, 10 ~> 1/2, 7 ~> 1/2, 9 ~> 1, 10 ~> 1/2, 12 ~> 1/2]
  9  -> [7 ~> 1/2, 9 ~> 1/2, 12 ~> 1, 9 ~> 1/2, 12 ~> 1/2, 14 ~> 1]       -- sequence climbs
  10 -> [9 ~> 1/2, 11 ~> 1/2, 14 ~> 1, 11 ~> 1/2, 14 ~> 1/2, 16 ~> 1]     -- a step per bar
  11 -> [11 ~> 1/2, 12 ~> 1/2, 14 ~> 1/2, 16 ~> 1, 14 ~> 1/2, 16 ~> 1/2, 19 ~> 1/2]
  12 -> [19 ~> 2, 21 ~> 1/2, 23 ~> 1, pause (1/2)]
  17 -> [14 ~> 1/2, 16 ~> 1/2, 19 ~> 1, 21 ~> 1/2, 19 ~> 1/2, 23 ~> 1]
  18 -> [24 ~> 3, pause 1]
  _ | section b == Chorus -> hook !! (b - 13)
    | otherwise -> [pause 4]

-- | Slightly detached notes (90% gate); on-beat onsets get the full level.
sing :: Double -> Tune -> Phrase
sing level tones = concat (zipWith note (scanl (+) 0 (map snd tones)) tones)
  where
    note _ (Nothing, d) = hush d
    note t (Just x, d) = hit [x] (d * 9 / 10) (level * accent t) ++ hush (d / 10)
    accent t = if denominator t == 1 then 1 else 0.82

leadBar :: Int -> Phrase
leadBar b = sing level (melody b)
  where
    level = case section b of
      Verse -> 0.7
      Build -> lift b + 0.1
      Chorus -> 1
      _ -> 0.9

-- | Music box: previews the hook in the intro, doubles it an octave up in the chorus.
bellBar :: Int -> Phrase
bellBar b = case section b of
  Intro -> sing 0.6 (octaveUp (hook !! (b - 1)))
  Chorus -> sing 0.35 (octaveUp (hook !! (b - 13)))
  _ -> hush 4
  where octaveUp = map (\(x, d) -> (fmap (+ 12) x, d))

-- Accompaniment ---------------------------------------------------------------

-- | Offbeat piano stabs, one per beat.
keysBar :: Int -> Phrase
keysBar b = case section b of
  Verse -> stabs 0.7 (segments b)
  Build | b == gapBar -> stabs 0.9 [(asus4, 2)] ++ stabs 0.9 [(aMaj, 1)] ++ hush 1
        | otherwise -> stabs (lift b + 0.1) (segments b)
  Tag | b == barCount -> hit (voicing dAdd9) 4 0.6
      | otherwise -> stabs 0.75 (segments b)
  _ -> hush 4
  where
    stabs :: Double -> [(Chord, Rational)] -> Phrase
    stabs level segs = concat
      [hush (1/2) ++ hit (voicing c) (1/4) level ++ hush (1/4) | (c, beats) <- segs, _ <- [1 .. round beats :: Int]]

-- | Sustained supersaw: swells through the intro, sits out the chorus (the pump plays).
padBar :: Int -> Phrase
padBar b = case section b of
  Intro -> held (0.3 + 0.08 * fromIntegral (b - 1))
  Verse -> held 0.5
  Build | b == gapBar -> hit (voicing asus4) 2 0.9 ++ hit (voicing aMaj) (3/2) 0.95 ++ hush (1/2)
        | otherwise -> held (lift b)
  Chorus -> hush 4
  Tag -> held 0.65
  where held level = concat [hit (voicing c) beats level | (c, beats) <- segments b]

-- | Chorus supersaw retriggered in 8ths: sidechain-style pumping.
pumpBar :: Int -> Phrase
pumpBar b
  | section b == Chorus = concat
      [ hit (voicing c ++ map (+ 12) (take 2 (voicing c))) (1/2) 0.9
      | (c, beats) <- segments b, _ <- [1 .. round (beats * 2) :: Int] ]
  | otherwise = hush 4

-- | 16th pluck arpeggio over the voicing an octave up, accented on the beat.
arpBar :: Int -> Phrase
arpBar b = case section b of
  Build | b == gapBar -> cells 0.9 (asus4, 2) ++ take 6 (cells 0.9 (aMaj, 2)) ++ hush (1/2)
        | otherwise -> concatMap (cells (lift b)) (segments b)
  Chorus -> concatMap (cells 1) (segments b)
  _ -> hush 4
  where
    cells :: Double -> (Chord, Rational) -> Phrase
    cells level (c, beats) = take (round (beats * 4))
      (cycle [e | (i, acc) <- zip [0, 2, 1, 3] [1, 0.5, 0.75, 0.5], e <- hit [voicing c !! i + 12] (1/4) (level * acc)])

-- Bass ------------------------------------------------------------------------

-- | Chromatic approach into the next root.
approach :: Int -> Int -> Int
approach r next
  | next > r = next - 1
  | next < r = next + 1
  | otherwise = r + 7

bassBar :: Int -> Phrase
bassBar b = case section b of
  Intro -> hush 4
  Verse -> concat (zipWith groove [1 :: Int ..] (segments b))
  Build | b == gapBar -> eighths 0.95 (root asus4) 4 ++ eighths 1 (root aMaj) 3 ++ hush (1/2)
        | otherwise -> eighths (lift b + 0.2) (root c0) 8
  Chorus -> drive (root c0)
  Tag | b == barCount -> hit [root dAdd9] 4 0.9
      | otherwise -> concat [eighths 0.9 (root c) (round (beats * 2)) | (c, beats) <- segments b]
  where
    c0 = case segments b of
      (c, _) : _ -> c
      [] -> dAdd9
    lastSeg i = i == length (segments b)
    -- Syncopated 16th groove: dotted-8th root, 16th ghost, octave pop, then
    -- either a root or (at the end of the bar) a chromatic approach.
    groove i (c, beats) = concat
      [ cell (root c) (if lastSeg i && k == half then approach (root c) (nextRoot b) else root c)
      | k <- [1 .. half] ]
      where half = round beats `div` 2 :: Int
    cell r final = hit [r] (3/4) 0.8 ++ hit [r] (1/4) 0.4 ++ hit [r + 12] (1/2) 0.65 ++ hit [final] (1/2) 0.6
    eighths level r k = concat (take k (cycle [hit [r] (1/2) level, hit [r + 12] (1/2) (level * 0.75)]))
    drive r = concat
      [ hit [r] (1/2) 1, hit [r] (1/2) 0.7, hit [r + 12] (1/2) 0.85, hit [r] (1/4) 0.8, hit [r] (1/4) 0.6
      , hit [r] (1/2) 0.9, hit [r + 12] (1/2) 0.8, hit [r] (1/2) 0.7, hit [approach r (nextRoot b)] (1/2) 0.85 ]

-- | Sine sub: carries the intro alone, then an octave down under the bass.
subBar :: Int -> Phrase
subBar b = case section b of
  Intro -> concat [hit [root c] beats 0.45 | (c, beats) <- segments b]
  Build | b == gapBar -> hit [root asus4 - 12] 2 0.8 ++ hit [root aMaj - 12] (3/2) 0.9 ++ hush (1/2)
  Chorus -> low 1
  Tag -> low 1
  _ -> low 0.7
  where low level = concat [hit [root c - 12] beats level | (c, beats) <- segments b]

-- Drums -----------------------------------------------------------------------

-- | One bar on a 16th grid; beats accented, 8ths medium, 16ths light.
grid :: Int -> Double -> [Int] -> Phrase
grid pitch level positions = concat
  [if i `elem` positions then hit [pitch] (1/4) (level * accent i) else hush (1/4) | i <- [0 .. 15 :: Int]]
  where accent i | i `mod` 4 == 0 = 1 | even i = 0.7 | otherwise = 0.5

kickStep, snareStep, hatStep :: Int
kickStep = -33   -- about 44 Hz
snareStep = -8   -- F#3 body
hatStep = 0      -- noise; pitch only matters for key tracking

kickBar :: Int -> Phrase
kickBar b = case section b of
  Intro | b >= 3 -> grid kickStep 0.55 [0]            -- heartbeat
        | otherwise -> hush 4
  Verse -> grid kickStep 0.7 ([0, 6, 10] ++ [14 | b == 8])
  Build -> grid kickStep (lift b + 0.1) [0, 4, 8, 12]
  Chorus -> grid kickStep 1 ([4, 8, 12] ++ [0 | b /= 13] ++ [10 | even b])   -- bar 13 opens on the boom
  Tag | b == barCount -> hush 4
      | otherwise -> grid kickStep 0.9 [0, 8]

snareBar :: Int -> Phrase
snareBar b = case section b of
  Verse -> grid snareStep 0.6 ([4, 12] ++ [15 | b == 8])
  Build | b == gapBar -> concat [hit [snareStep] (1/4) v | v <- ramp 0.15 1 14] ++ hush (1/2)
        | b == 11 -> grid snareStep 0.85 [4, 10, 12, 14]
        | otherwise -> grid snareStep 0.8 [4, 12]
  Chorus -> grid snareStep 1 ([4, 12] ++ (if b == 16 then [13, 14, 15] else []))
  Tag | b /= barCount -> grid snareStep 0.9 [4, 12]
  _ -> hush 4

hatBar :: Int -> Phrase
hatBar b = case section b of
  Intro | b >= 3 -> grid hatStep 0.45 [2, 6, 10, 14]   -- clockwork ticks
  Verse -> grid hatStep 0.55 [0, 2 .. 14]
  Build | b == gapBar -> hush 4
        | b == 11 -> grid hatStep 0.75 [0 .. 15]
        | otherwise -> grid hatStep 0.7 [0, 2 .. 14]
  Chorus -> grid hatStep 0.8 [0 .. 15]
  Tag | b /= barCount -> grid hatStep 0.7 [0, 2 .. 14]
  _ -> hush 4

openHatBar :: Int -> Phrase
openHatBar b
  | section b == Chorus = grid hatStep 0.6 [2, 6, 10, 14]
  | otherwise = hush 4

-- | @k@ levels moving linearly from @a@ to @b@.
ramp :: Double -> Double -> Int -> [Double]
ramp a b k = [a + (b - a) * fromIntegral i / fromIntegral (max 1 (k - 1)) | i <- [0 .. k - 1]]

-- Arrangement -----------------------------------------------------------------

-- | Layers written bar by bar; each bar must fill exactly four beats.
barLayers :: [(String, Int -> Phrase)]
barLayers =
  [ ("lead", leadBar), ("bell", bellBar), ("keys", keysBar), ("pad", padBar), ("pump", pumpBar)
  , ("arp", arpBar), ("bass", bassBar), ("sub", subBar), ("kick", kickBar), ("snare", snareBar)
  , ("hat", hatBar), ("openHat", openHatBar) ]

layer :: String -> Score
layer name = case lookup name barLayers of
  Just f -> Line name (concatMap f [1 .. barCount])
  Nothing -> Silence 0

-- | Dotted-8th echo of a bar-written layer.
echo :: String -> String -> Score
echo source name = case lookup source barLayers of
  Just f -> at name (3/4) (concatMap f [1 .. barCount])
  Nothing -> Silence 0

impactBeat, endBeat :: Rational
impactBeat = 48
endBeat = 68

-- | Outro glints with dotted-8th echoes bouncing left and right.
glints :: Score
glints = together
  [ at name (onsetBeat + delay) (hit [x] (1/4) 0.9)
  | (onsetBeat, x) <- [(endBeat, 24), (endBeat + 1, 28), (endBeat + 2, 31)]
  , (delay, name) <- [(0, "glint"), (3/4, "glintEcho1"), (3/2, "glintEcho2"), (9/4, "glintEcho3")] ]

groups :: [(String, Score)]
groups =
  [ ("melody", together [layer "lead", echo "lead" "leadEcho", layer "bell"])
  , ("harmony", together [layer "keys", layer "pad", layer "pump", layer "arp", echo "arp" "arpEcho"])
  , ("bass", together [layer "bass", layer "sub"])
  , ("drums", together
      [ layer "kick", layer "snare", layer "hat", layer "openHat"
      , at "boom" impactBeat (hit [kickStep] 4 1), at "boom" endBeat (hit [kickStep] 4 0.55)
      , at "crash" impactBeat (hit [hatStep] 3 1), at "crash" endBeat (hit [hatStep] 3 0.45) ])
  , ("fx", together
      [ at "air" 0 (hit [hatStep] 16 0.5)
      , at "sweep" 40 (hit [hatStep] 8 0.9)
      , at "microRiser" 40 [Event (1/6) v [Step x] | (x, v) <- zip [24 .. 71] (ramp 0.2 1 48)]
      , glints ])
  ]

score :: Score
score = together (map snd groups)

-- Sound -----------------------------------------------------------------------

lead, bell, piano, pumpPad, bassSynth, glass, boom, openHat, crash :: Patch
-- | Bright, slightly hollow J-pop lead: square body, detuned saw shine, sine octave.
lead = basicPatch
  { oscillators = [Osc SquareWave 0 0.5 1 0, Osc SawWave 0 0.35 3 10, Osc SineWave 12 0.15 1 0]
  , ampEnvelope = Envelope 0.01 0.2 0.8 0.12
  , cutoff = 2400, resonance = 0.1, filterEnvelope = Envelope 0.001 0.3 0.4 0.15
  , filterAmount = 1.2, velocityToCutoff = 0.6, stereoWidth = 0.3 }
bell = basicPatch
  { oscillators = [Osc SineWave 0 1 1 0, Osc SineWave 12 0.28 1 0, Osc SineWave 28 0.06 1 0]
  , ampEnvelope = Envelope 0.002 0.5 0 0.4 }
piano = pluck
  { oscillators = [Osc TriangleWave 0 1 1 0, Osc SineWave 12 0.32 1 0, Osc SineWave 19 0.07 1 0]
  , ampEnvelope = Envelope 0.003 0.6 0.12 0.22, cutoff = 3600, filterAmount = 0.4, stereoWidth = 0.3 }
pumpPad = supersaw { ampEnvelope = Envelope 0.07 0 1 0.05, filterEnvelope = Envelope 0.01 0 1 0.1 }
-- | Growling saw over a square sub: audible on small speakers, heavy on big ones.
bassSynth = reeseBass
  { oscillators = [Osc SawWave 0 1 2 12, Osc SquareWave (-12) 0.45 1 0]
  , ampEnvelope = Envelope 0.003 0.25 0.75 0.06
  , cutoff = 320, resonance = 0.3, filterEnvelope = Envelope 0.002 0.18 0 0.1
  , filterAmount = 2.2, velocityToCutoff = 1, stereoWidth = 0.15 }
glass = pluck
  { oscillators = [Osc TriangleWave 0 1 1 0, Osc SineWave 12 0.5 1 0, Osc SineWave 19 0.12 1 0]
  , ampEnvelope = Envelope 0.001 0.7 0 0.6, cutoff = 3000, filterAmount = 1.5 }
boom = kick { ampEnvelope = Envelope 0.001 1.6 0 0.2, pitchDrop = 30, pitchTime = 0.09 }
openHat = hat { ampEnvelope = Envelope 0.001 0.25 0 0.08 }
crash = hat { ampEnvelope = Envelope 0.001 2.2 0 0.5, highpass = 3500, cutoff = 12000, stereoWidth = 1 }

master :: Mix
master = Mix
  { mixRate = 44100, mixReverb = hall { roomSize = 0.78, wet = 0.38 }
  , mixDrive = 2.0, mixPeak = 0.89, mixTail = 0, mixNormalize = True }

performance :: Performance
performance = Performance 144 (M.fromList registry) master
  where
    inst name patch level side reverbSend = (name, Instrument d4 patch level side reverbSend)
    registry =
      [ inst "lead" lead 0.53 0 0.25, inst "leadEcho" lead 0.17 0.55 0.45
      , inst "bell" bell 0.35 (-0.3) 0.45
      , inst "keys" piano 0.14 (-0.25) 0.25, inst "pad" supersaw 0.1 0 0.4
      , inst "pump" pumpPad 0.09 0 0.3
      , inst "arp" pluck 0.2 0.25 0.3, inst "arpEcho" pluck 0.07 (-0.5) 0.5
      , inst "bass" bassSynth 0.34 0 0.02, inst "sub" subBass 0.3 0 0
      , inst "kick" kick 0.9 0 0, inst "boom" boom 0.9 0 0
      , inst "snare" snare 0.42 0 0.2, inst "hat" hat 0.16 0.2 0.08, inst "openHat" openHat 0.12 0.2 0.1
      , inst "crash" crash 0.3 0 0.5
      , inst "air" noiseSweep 0.08 0 0.5, inst "sweep" noiseSweep 0.2 0 0.5
      , ("microRiser", Instrument quarterTones pluck 0.14 0 0.4)
      , inst "glint" glass 0.28 0 0.6, inst "glintEcho1" glass 0.14 (-0.7) 0.6
      , inst "glintEcho2" glass 0.08 0.7 0.6, inst "glintEcho3" glass 0.04 (-0.7) 0.6 ]

-- Checks and output -----------------------------------------------------------

check :: Either String ()
check = do
  unless (barCount == 18) (Left "expected 18 bars")
  forM_ [(name, b, f b) | (name, f) <- barLayers, b <- [1 .. barCount]] $ \(name, b, phrase) ->
    unless (sum (map dur phrase) == 4) (Left (printf "%s bar %d lasts %s beats" name b (show (sum (map dur phrase)))))
  forM_ (zip [1 :: Int ..] progression) $ \(b, segs) ->
    unless (sum (map snd segs) == 4) (Left ("harmony bar " ++ show b ++ " is not four beats"))
  S.validate performance score

main :: IO ()
main = do
  args <- getArgs
  either (ioError . userError) pure check
  case args of
    ["--check"] -> putStrLn (printf "screw-anime: %d bars at 144 BPM = %.1f s; checks passed" barCount
                               (fromIntegral (barCount * 4) * 60 / 144 :: Double))
    ["--stems"] -> forM_ groups $ \(name, piece) ->
      case S.render performance piece >>= renderTimelineMix master { mixDrive = 0, mixNormalize = False } of
        Left err -> ioError (userError err)
        Right (l, r) -> do
          let window = 22050
              rmsAt i = sqrt (VU.sum (VU.map (^ (2 :: Int)) (VU.slice i window l VU.++ VU.slice i window r))
                              / fromIntegral (2 * window))
              windows = [rmsAt i | i <- [0, window .. VU.length l - window]]
              db x = 20 * logBase 10 (max 1e-9 x) :: Double
          printf "%-8s loudest 0.5 s rms %6.1f dB | %s\n" name (db (maximum windows))
            (concatMap (\x -> let d = db x in if d < -60 then " ." else ' ' : show (round (d + 60) `div` 5 :: Int)) windows)
    _ -> do
      let path = case args of
            [p] -> p
            _ -> "screw-anime.wav"
      timeline <- either (ioError . userError) pure (S.render performance score)
      writeTimelineMix path master timeline
      putStrLn ("screw-anime: wrote " ++ path)
