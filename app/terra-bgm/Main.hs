-- Original NeoMusic instrumental for Terra II: "Skyward, Little World".
-- 144 BPM, D major / B minor, 56 four-beat bars (93 1/3 seconds plus tail).
-- No sampled audio, MIDI files, or melody transcribed from the reference.
module Main (main) where

import Control.Monad (unless)
import NeoMusic
import System.Environment (getArgs)
import Text.Printf (printf)

type Line = [(Event, Rational, Double)]
data Harmony = H { root :: Int, keys :: [Int] }

-- MIDI-number pitches, interpreted directly by NeoMusic's equal temperament.
tune :: Tuning
tune = Tuning 8.175798915643707 (2 ** (1/12))
note :: [Int] -> Rational -> Double -> Line
note ns d v = [(Play (map Step ns), d, v)]
silent :: Rational -> Line
silent d = [(Rest, d, 0) | d > 0]
gated :: Int -> Rational -> Double -> Line
gated p d v = note [p] (d * 9/10) v ++ silent (d/10)
line :: Patch -> Line -> Piece
line patch es = FromPart (Part tune (Tempo 144) (Synth patch)
  [e | (e,_,_) <- es] [d | (_,d,_) <- es] [v | (_,_,v) <- es])
together :: [Piece] -> Piece
together = foldr1 (:||:)

-- Close-position sevenths/ninths; the bass supplies the harmonic root.
dmaj, bmin, gmaj, amaj, fsmin, emin, fsmaj :: Harmony
dmaj = H 38 [62,66,69,73]
bmin = H 35 [62,66,69,71]
gmaj = H 31 [62,66,67,71]
amaj = H 33 [61,64,69,71]
fsmin = H 30 [61,64,66,69]
emin = H 28 [62,64,67,71]
fsmaj = H 30 [61,66,70,73]

-- Intro 4 / verse 12 / lift 4 / chorus 16 / bridge 8 / reprise 12.
harmonies :: [Harmony]
harmonies = [dmaj,amaj,bmin,gmaj]
 ++ concat (replicate 3 [bmin,gmaj,dmaj,amaj])
 ++ [emin,fsmin,gmaj,amaj]
 ++ concat (replicate 2 [gmaj,amaj,fsmin,bmin,emin,amaj,dmaj,dmaj])
 ++ [bmin,fsmin,gmaj,dmaj,emin,fsmaj,bmin,amaj]
 ++ [gmaj,amaj,fsmin,bmin,emin,amaj,dmaj,bmin,gmaj,amaj,dmaj,dmaj]

piano, lead, pad, bass, bell, cymbal :: Patch
piano = pluck
 { oscillators = [Osc TriangleWave 0 1 1 0, Osc SineWave 12 0.32 1 0, Osc SineWave 19 0.07 1 0]
 , ampEnvelope = Envelope 0.003 0.6 0.12 0.22, cutoff = 3600, filterAmount = 0.4, stereoWidth = 0.3 }
lead = basicPatch
 { oscillators = [Osc TriangleWave 0 0.9 1 0, Osc SawWave 0 0.32 3 9, Osc SineWave 12 0.1 1 0]
 , ampEnvelope = Envelope 0.016 0.1 0.78 0.09, cutoff = 2900, resonance = 0.08
 , filterAmount = 0.2, stereoWidth = 0.35 }
pad = supersaw
 { oscillators = [Osc SawWave 0 0.5 3 16, Osc TriangleWave 0 0.6 1 0]
 , ampEnvelope = Envelope 0.24 0.5 0.58 0.45, cutoff = 1050, filterAmount = 0.3, velocityToCutoff = 0.2 }
bass = reeseBass
 { oscillators = [Osc TriangleWave 0 0.65 1 0, Osc SawWave 0 0.32 1 0, Osc SineWave 0 0.5 1 0]
 , ampEnvelope = Envelope 0.004 0.12 0.65 0.06, cutoff = 420, filterAmount = 0.8, stereoWidth = 0 }
bell = piano
 { oscillators = [Osc SineWave 0 1 1 0, Osc SineWave 12 0.28 1 0, Osc SineWave 28 0.06 1 0]
 , ampEnvelope = Envelope 0.002 0.4 0 0.35, cutoff = 6000 }
cymbal = hat { ampEnvelope = Envelope 0.002 0.9 0 0.3, highpass = 4300 }

-- Eight-bar original vocal-like hook: pickups, held peaks, and answering phrases.
hook :: [[(Int,Rational)]]
hook =
 [ [(74,1/2),(78,1/2),(81,1),(78,1/2),(76,1/2),(74,1)]
 , [(76,3/4),(78,1/4),(81,1),(83,1/2),(81,1/2),(76,1)]
 , [(78,1),(76,1/2),(73,1/2),(76,1),(78,1)]
 , [(78,1/2),(81,1/2),(83,3/2),(81,1/2),(78,1)]
 , [(79,1/2),(78,1/2),(76,1),(74,1/2),(71,1/2),(74,1)]
 , [(73,1/2),(76,1/2),(78,1),(76,3/2),(73,1/2)]
 , [(74,3/2),(78,1/2),(76,1),(74,1)]
 , [(73,1/2),(74,1/2),(78,1),(76,1/2),(73,1/2),(74,1)] ]
verse :: [[(Int,Rational)]]
verse =
 [ [(71,1),(74,1/2),(73,1/2),(71,1),(66,1)]
 , [(67,3/2),(71,1/2),(74,1),(71,1)]
 , [(69,1),(66,1/2),(69,1/2),(73,1),(74,1)]
 , [(73,1),(71,1/2),(69,1/2),(64,1),(69,1)] ]
melodyBar :: Int -> Line
melodyBar b
 | b < 4 = silent 4
 | b < 16 = phrase 0.72 (verse !! ((b-4) `mod` 4))
 | b < 20 = phrase 0.8 ([(p,1/2) | p <- take 8 (drop ((b-16)*2) (cycle [71,73,74,76,78,79,81,83]))])
 | b < 36 = phrase 0.9 (hook !! ((b-20) `mod` 8))
 | b < 44 = silent 4
 | b < 52 = phrase 0.91 (hook !! ((b-44) `mod` 8))
 | b == 52 = phrase 0.8 [(79,1),(78,1),(76,1),(74,1)]
 | b == 53 = phrase 0.76 [(73,1),(76,1),(73,1),(69,1)]
 | b == 54 = phrase 0.7 [(74,3),(78,1)]
 | otherwise = phrase 0.55 [(74,4)]
 where phrase v = concatMap (\(p,d) -> gated p d v)

energy :: Int -> Double
energy b | b < 4 = 0.3 | b < 16 = 0.65 | b < 20 = 0.8
         | b < 36 = 1 | b < 40 = 0.32 | b < 44 = 0.65
         | b < 54 = 1 | otherwise = 0.3

keysBar :: Int -> Harmony -> Line
keysBar b h = concat [note (keys h) (1/3) (energy b * v) ++ silent (2/3) | v <- [0.7,0.46,0.62,0.5]]
arpBar :: Int -> Harmony -> Line
arpBar b h = concat [gated ((keys h !! k) + 12) (1/2) (energy b * v)
 | (k,v) <- zip [0,2,1,3,2,1,3,2] [0.72,0.4,0.55,0.42,0.65,0.4,0.52,0.38]]
bassBar :: Int -> Harmony -> Line
bassBar b h
 | b < 4 || (b >= 36 && b < 40) || b >= 54 = note [root h] 4 0.5
 | otherwise = concat [gated (root h + o) d (v * energy b)
     | (o,d,v) <- [(0,3/4,0.9),(0,1/4,0.5),(12,1/2,0.7),(0,1/2,0.8),(0,3/4,0.9),(7,1/4,0.6),(12,1/2,0.75),(7,1/2,0.6)]]

-- Each percussion lane is a full bar on a sixteenth-note grid.
drumBar :: Int -> Int -> [Int] -> Line
drumBar b pitch positions = concat [if i `elem` positions then note [pitch] (1/8) (energy b * accent i) ++ silent (1/8) else silent (1/4) | i <- [0..15]]
 where accent i = if i `mod` 4 == 0 then 0.95 else 0.65
kickPositions :: Int -> [Int]
kickPositions b | b < 4 || (b >= 36 && b < 40) || b >= 54 = [0]
                | b < 20 = [0,6,8,14] | otherwise = [0,4,8,10,12]
snarePositions :: Int -> [Int]
snarePositions b | b < 4 || (b >= 36 && b < 40) || b >= 54 = []
                 | b `elem` [19,35,43,51] = [4,12,14,15] | otherwise = [4,12]

score :: Piece
score = together
 [ Send 0.2 $ Gain 0.29 $ line lead (concatMap melodyBar [0..55])
 , Send 0.22 $ Pan (-0.28) $ Gain 0.14 $ line piano (concat (zipWith keysBar [0..] harmonies))
 , Send 0.3 $ Pan 0.3 $ Gain 0.12 $ line bell (concat (zipWith arpBar [0..] harmonies))
 , Send 0.28 $ Gain 0.085 $ line pad (concat [note (keys h) 4 (0.5 + 0.15 * energy b) | (b,h) <- zip [0..] harmonies])
 , Gain 0.31 $ line bass (concat (zipWith bassBar [0..] harmonies))
 , Gain 0.65 $ line kick (concat [drumBar b 30 (kickPositions b) | b <- [0..55]])
 , Send 0.1 $ Gain 0.19 $ line snare (concat [drumBar b 50 (snarePositions b) | b <- [0..55]])
 , Pan 0.2 $ Gain 0.095 $ line hat (concat [drumBar b 72 (if b<4 || b>=54 then [] else [0,2..14]) | b <- [0..55]])
 , Send 0.22 $ Pan (-0.35) $ Gain 0.12 $ line cymbal (concat [if b `elem` [4,20,28,44] then note [72] 1 0.7 ++ silent 3 else silent 4 | b <- [0..55]])
 , Send 0.3 $ Pan (-0.1) $ Gain 0.19 $ line piano (silent (36*4) ++ concat [note [p] d 0.72 | (p,d) <- concat (take 8 (cycle verse))])
 ]

check :: IO ()
check = do
 unless (length harmonies == 56) (error "expected 56 harmony bars")
 unless (all ((==4) . sum . map snd) (hook ++ verse)) (error "melody bar length")
 mapM_ (\(b,h) -> unless (all ((==4) . sum . map (\(_,d,_) -> d)) [melodyBar b,keysBar b h,arpBar b h,bassBar b h,drumBar b 30 (kickPositions b)]) (error ("bad bar " ++ show b))) (zip [0..] harmonies)
 case render score of
  Left e -> error e
  Right t -> do
   unless (abs (seconds t - 280/3) < 0.0001) (error "unexpected arrangement length")
   printf "56 bars, %.3f seconds, %d synthesized notes; score checks passed.\n" (seconds t) (length (notes t))

main :: IO ()
main = do
 args <- getArgs
 check
 case args of
  ["--check"] -> pure ()
  [out] -> writeMix out (Mix 44100 (hall { roomSize=0.67, wet=0.23 }) 0.35 0.86 1.2 True) score
  _ -> ioError (userError "usage: terra-bgm --check | terra-bgm output.wav")
