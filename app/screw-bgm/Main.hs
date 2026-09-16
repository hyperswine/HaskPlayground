-- | 30-second electronic score for the M3x8 screw cinematic (Playground/high-level-stuff/screw-cinematic).
--
-- 96 BPM, 12 bars of four beats = exactly 30 s, cut to the camera move:
--
-- >  bars 1-3   0.0-7.5 s  sub drone, filtered supersaw swelling open, glass plucks with echoes
-- >  bars 4-6   7.5-15 s   16th pluck arp (velocity accents), reese bass, offbeat hats
-- >  bars 7-8   15-20 s    four-on-the-floor, snare roll crescendo, noise sweep + quarter-tone
-- >                        riser; everything but the risers drops out for the last half beat
-- >  bars 9-11  20-27.5 s  impact: boom, crash, pumping supersaw, offbeat bass, full groove
-- >  bar  12    27.5-30 s  D major pad and plucks ringing out into the hall reverb
--
-- @screw-bgm [out.wav]@ renders the mix; @screw-bgm --stems@ prints each layer's level.
module Main (main) where

import Control.Monad (forM_)
import qualified Data.Vector.Unboxed as VU
import NeoMusic
  ( Envelope(..), Event(..), Mix(..), Osc(..), Part(..), Patch(..), Piece(..), Reverb(..)
  , Step(..), Tempo(..), Timbre(..), Tuning(..), Wave(..)
  , hall, hat, kick, noiseSweep, pluck, ramp, reeseBass, renderMix, snare, subBass, supersaw
  , writeMix )
import System.Environment (getArgs)
import Text.Printf (printf)

-- | Events with their length in beats and velocity.
type Line = [(Event, Rational, Double)]

hit :: [Int] -> Rational -> Double -> Line
hit xs beats vel = [(Play (map Step xs), beats, vel)]

hush :: Rational -> Line
hush beats = [(Rest, beats, 0) | beats > 0]

-- | Semitones and quarter-tones above D3.
semis, quarterTones :: Tuning
semis = Tuning 146.83 (2 ** (1 / 12))
quarterTones = Tuning 146.83 (2 ** (1 / 24))

-- | A line for one patch, starting @offset@ beats into the piece.
lineAt :: Rational -> Tuning -> Patch -> Line -> Piece
lineAt offset t patch evs =
  FromPart (Part t (Tempo 96) (Synth patch) [e | (e, _, _) <- es] [b | (_, b, _) <- es] [v | (_, _, v) <- es])
  where es = hush offset ++ evs

together :: [Piece] -> Piece
together = foldr1 (:||:)

-- Harmony ---------------------------------------------------------------------

-- | Pad voicing, bass root, arpeggio triad (root, third, fifth).
data Harmony = Harmony { padNotes :: [Int], bassRoot :: Int, arpTriad :: (Int, Int, Int) }

dmin, bflat, fmaj, gmin, amaj, cmaj :: Harmony
dmin  = Harmony [0, 7, 12, 15]  (-12) (12, 15, 19)
bflat = Harmony [3, 8, 12, 15]  (-16) (8, 12, 15)
fmaj  = Harmony [3, 7, 10, 15]  (-9)  (15, 19, 22)
gmin  = Harmony [5, 8, 12, 17]  (-7)  (17, 20, 24)
amaj  = Harmony [7, 11, 14, 19] (-5)  (19, 23, 26)
cmaj  = Harmony [5, 10, 14, 17] (-14) (10, 14, 17)

-- | One bar of 16ths over root, fifth, octave, tenth, with an accent pattern.
arpBar :: Double -> Harmony -> Line
arpBar level h = concat (replicate 4 (concat (zipWith note cell [1, 0.45, 0.75, 0.5])))
  where
    (r, third, fifth) = arpTriad h
    cell = [r, fifth, r + 12, third + 12]
    note x accent = hit [x] (1 / 4) (level * accent)

-- Patches ---------------------------------------------------------------------

-- | Supersaw retriggered in 8ths with a short attack: sidechain-style pumping.
pumpPad :: Patch
pumpPad = supersaw { ampEnvelope = Envelope 0.09 0 1 0.06, filterEnvelope = Envelope 0.01 0 1 0.1 }

-- | Glassy bell for the glints.
glass :: Patch
glass = pluck
  { oscillators = [Osc TriangleWave 0 1 1 0, Osc SineWave 12 0.5 1 0, Osc SineWave 19 0.12 1 0]
  , ampEnvelope = Envelope 0.001 0.7 0 0.6, cutoff = 3000, filterAmount = 1.5 }

boom :: Patch
boom = kick { ampEnvelope = Envelope 0.001 1.6 0 0.2, pitchDrop = 30, pitchTime = 0.09 }

openHat :: Patch
openHat = hat { ampEnvelope = Envelope 0.001 0.3 0 0.08 }

crash :: Patch
crash = hat { ampEnvelope = Envelope 0.001 2.2 0 0.5, highpass = 3500, cutoff = 12000, stereoWidth = 1 }

-- Layers ----------------------------------------------------------------------

-- | High note with dotted-8th echoes bouncing left and right.
glint :: Rational -> Int -> Piece
glint at x = together
  [ Pan side (Gain g (lineAt (at + delay) semis glass (hit [x] (1 / 4) 0.9)))
  | (delay, g, side) <- [(0, 1, 0), (3 / 4, 0.5, -0.7), (3 / 2, 0.28, 0.7), (9 / 4, 0.15, -0.7)] ]

glints :: Piece
glints = Send 0.6 $ Gain 0.28 $ together $
  [glint at x | (at, x) <- [(1, 36), (7 / 2, 31), (6, 39), (17 / 2, 43), (21 / 2, 34)]]
  ++ [glint 44 36, glint (91 / 2) 40, glint 46 43]

pads :: Piece
pads = together
  [ Send 0.45 $ Gain 0.12 $ lineAt 0 semis supersaw $ concat
      [ hit (padNotes dmin) 4 0.3, hit (padNotes dmin) 4 0.45, hit (padNotes dmin) 4 0.55   -- filter swells open
      , hit (padNotes dmin) 4 0.55, hit (padNotes bflat) 4 0.6, hit (padNotes fmaj) 4 0.65
      , hit (padNotes gmin) 4 0.75, hit [7, 12, 14, 19] 2 0.85, hit (padNotes amaj) (3 / 2) 0.95
      , hush (25 / 2)
      , hit [0, 7, 12, 16, 19] 3 0.6 ]
  , Send 0.3 $ Gain 0.1 $ lineAt 32 semis pumpPad $ concat
      [pump [0, 7, 12, 15, 19, 24], pump [3, 8, 12, 15, 20, 24], pump [5, 10, 14, 17, 22]]
  ]
  where pump xs = concat (replicate 8 (hit xs (1 / 2) 0.9))

arps :: Piece
arps = together
  [ Send 0.3 $ Gain 0.7 $ Pan (-0.15) $ lineAt 12 semis pluck arpLine
  , Send 0.5 $ Gain 0.22 $ Pan 0.6 $ lineAt (12 + 3 / 4) semis pluck arpLine   -- dotted-8th echo
  ]
  where
    arpLine = concat
      [ arpBar 0.45 dmin, arpBar 0.55 bflat, arpBar 0.65 fmaj, arpBar 0.8 gmin
      , take 14 (arpBar 0.95 amaj), hush (1 / 2)
      , arpBar 1 dmin, arpBar 1 bflat, arpBar 1 cmaj ]

drone :: Piece
drone = Gain 0.18 (lineAt 0 semis subBass (hit [-12] 12 0.7))

bass :: Piece
bass = together
  [ Gain 0.25 $ lineAt 12 semis reeseBass $ concat
      [ hit [bassRoot dmin] 4 0.5, hit [bassRoot bflat] 4 0.55, hit [bassRoot fmaj] 4 0.6
      , octaves gmin 8, octaves amaj 7, hush (1 / 2)
      , offbeats dmin, offbeats bflat, offbeats cmaj
      , hit [-12] 3 0.6 ]
  , Gain 0.25 $ lineAt 32 semis subBass $ concat
      [hit [bassRoot h] 4 1 | h <- [dmin, bflat, cmaj]]
  ]
  where
    octaves h k = concat (take k (cycle [hit [bassRoot h] (1 / 2) 0.9, hit [bassRoot h + 12] (1 / 2) 0.6]))
    offbeats h = concat (replicate 4 (hush (1 / 2) ++ hit [bassRoot h] (1 / 2) 1))

drums :: Piece
drums = together
  [ Gain 0.9 $ together
      [ lineAt 12 semis kick (concat (replicate 3 (thump 0.6 ++ hush (15 / 4))))
      , lineAt 24 semis kick (concat (replicate 7 (thump 0.9 ++ hush (3 / 4))) ++ thump 0.8 ++ thump 1 ++ hush (1 / 2))
      , lineAt 32 semis boom (hit [kickNote] 4 1)
      , lineAt 33 semis kick (concat (replicate 11 (thump 0.95 ++ hush (3 / 4))))
      , lineAt 44 semis boom (hit [kickNote] 2 0.6) ]
  , Send 0.25 $ Gain 0.4 $ lineAt 24 semis snare $ concat
      [ backbeat 0.45
      , concat [hit [3] (1 / 4) v | v <- ramp 0.12 1 14], hush (1 / 2)   -- roll into the gap
      , concat (replicate 3 (backbeat 0.9)) ]
  , Send 0.1 $ Pan 0.25 $ Gain 0.2 $ together
      [ lineAt 12 semis hat (concat (replicate 12 (hush (1 / 2) ++ hit [0] (1 / 4) 0.6 ++ hush (1 / 4))))
      , lineAt 24 semis hat (concat (replicate 7 sixteenths) ++ hush 1)
      , lineAt 32 semis hat (concat (replicate 12 sixteenths))
      , Gain 0.7 (lineAt 32 semis openHat (concat (replicate 12 (hush (1 / 2) ++ hit [0] (1 / 2) 0.6)))) ]
  , Send 0.5 $ Gain 0.3 $ lineAt 32 semis crash (hit [0] 3 1)
  ]
  where
    kickNote = -21                                   -- F1, about 44 Hz
    thump v = hit [kickNote] (1 / 4) v
    backbeat v = hush 1 ++ hit [3] (1 / 4) v ++ hush (7 / 4) ++ hit [3] (1 / 4) v ++ hush (3 / 4)
    sixteenths = concat [hit [0] (1 / 4) v | v <- [0.55, 0.25, 0.8, 0.3]]

risers :: Piece
risers = together
  [ Send 0.5 $ Gain 0.22 $ lineAt 24 semis noiseSweep (hit [0] 8 0.9)
  , Send 0.4 $ Gain 0.16 $ lineAt 24 quarterTones pluck
      [(Play [Step x], 1 / 6, v) | (x, v) <- zip [24 .. 71] (ramp 0.2 1 48)]
  ]

layers :: [(String, Piece)]
layers =
  [ ("drone", drone), ("pads", pads), ("glints", glints), ("arps", arps)
  , ("bass", bass), ("drums", drums), ("risers", risers) ]

score :: Piece
score = together (map snd layers)

mix :: Mix
mix = Mix
  { mixRate = 44100, mixReverb = hall { roomSize = 0.82, wet = 0.45 }
  , mixDrive = 1.8, mixPeak = 0.89, mixTail = 0, mixNormalize = True }

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--stems"] -> forM_ layers $ \(name, piece) ->
      case renderMix mix { mixDrive = 0, mixNormalize = False } piece of
        Left err -> ioError (userError err)
        Right (l, r) -> do
          let window = 22050
              rmsAt i = sqrt (VU.sum (VU.map (^ (2 :: Int)) (VU.slice i window l VU.++ VU.slice i window r))
                              / fromIntegral (2 * window))
              windows = [rmsAt i | i <- [0, window .. VU.length l - window]]
              db x = 20 * logBase 10 (max 1e-9 x) :: Double
          printf "%-7s loudest 0.5 s rms %6.1f dB | %s\n" name (db (maximum windows))
            (concatMap (\x -> let d = db x in if d < -60 then " ." else ' ' : show (round (d + 60) `div` 5 :: Int)) windows)
    _ -> do
      let path = case args of
            [p] -> p
            _ -> "screw-bgm.wav"
      writeMix path mix score
      putStrLn ("screw-bgm: wrote " ++ path)
