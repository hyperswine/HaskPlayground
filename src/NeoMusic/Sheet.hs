-- | Piano engraving is a view over exact beat events, never audio samples.
module NeoMusic.Sheet (engrave, sheetEvents) where

import Control.Monad (unless)
import Data.List (intercalate, sortOn)
import qualified Data.Map.Strict as M
import Data.Ratio (denominator)
import qualified NeoMusic.Audio as A
import qualified NeoMusic.Pitch as A
import NeoMusic.Score hiding (chord)
import NeoMusic.Language (SheetView(..), Staff(..))
import NeoMusic.Midi (midiPitch)

-- | Shared pre-engraving representation: staff, onset, duration and MIDI chord.
sheetEvents :: Performance -> SheetView -> Score -> Either String [(Staff,Rational,Rational,[Int])]
sheetEvents perf view score = do
  validate perf score
  (total,es) <- flatten score
  unless (meter view > 0) (Left "meter must be positive")
  unless (dyadic total) (Left "sheet supports durations on a 32nd-note grid; tuplets are not supported")
  mapM convert es
  where
    dyadic x = denominator (x*8) == 1
    convert e = do
      i <- maybe (Left "missing instrument") Right (M.lookup (instrument e) (instruments perf))
      staff <- maybe (Left ("missing staff for "++instrument e)) Right (M.lookup (instrument e) (staves view))
      unless (A.pitchDrop (timbre i) == 0 && all ((/= A.NoiseWave) . A.wave) (A.oscillators (timbre i)))
        (Left "noise/pitch-drop percussion cannot be engraved without a staff mapping")
      -- Validate the tuning even for an all-rest instrument.
      _ <- midiPitch (tuning i) (A.Step 0)
      ps <- mapM (midiPitch (tuning i)) (pitches (event e))
      unless (all (\p -> p >= 21 && p <= 108) ps) (Left "pitch outside piano range A0..C8")
      unless (dyadic (onset e) && dyadic (dur (event e))) (Left "sheet supports durations on a 32nd-note grid; tuplets are not supported")
      pure (staff,onset e,dur (event e),ps)

engrave :: Performance -> SheetView -> Score -> Either String String
engrave perf view score = do
  es <- sheetEvents perf view score
  (total,_) <- flatten score
  keyName <- maybe (Left "unsupported major key hint") Right (M.lookup (key view) keys)
  let staff name clef =
        let voices = separate (sortOn (\(_,t,_,_) -> t) [e | e@(s,_,_,ps) <- es, s == name, not (null ps)])
            content = if null voices then [emitRest 0 total] else map (emitVoice total) voices
        in "  \\new Staff { \\clef "++clef++" \\time "++show (meter view)++"/4 \\key "++keyName++" \\major\n"
           ++"    "++tempoText++"\n    << "
           ++ intercalate " \\\\ " ["{ "++v++" }" | v <- content] ++ " >>\n  }\n"
  pure ("\\version \"2.24.3\"\n\\score {\n \\new PianoStaff <<\n"++staff Treble "treble"++staff Bass "bass"++" >>\n \\layout { }\n}\n")
  where
    tempoText
      | bpm perf == fromInteger (round (bpm perf)) = "\\tempo 4 = "++show (round (bpm perf) :: Integer)
      | otherwise = "\\tempo \""++show (bpm perf)++" BPM\""
    flats = key view `elem` ["F","Bb","Eb","Ab","Db","Gb","Cb"]
    keys = M.fromList (zip ["C","G","D","A","E","B","F#","C#","F","Bb","Eb","Ab","Db","Gb","Cb"]
                          ["c","g","d","a","e","b","fis","cis","f","bes","ees","aes","des","ges","ces"])
    pitch p = (if flats then flatNames else sharpNames) !! (p `mod` 12) ++ octave (p `div` 12-4)
    sharpNames = ["c","cis","d","dis","e","f","fis","g","gis","a","ais","b"]
    flatNames = ["c","des","d","ees","e","f","ges","g","aes","a","bes","b"]
    octave k = replicate (abs k) (if k >= 0 then '\'' else ',')
    emitRest t d = emit t d []
    emitVoice total = go 0
      where
        go at [] = emitRest at (total-at)
        go at ((_,t,d,ps):xs) = emitRest at (t-at)++emit t d ps++go (t+d) xs
    emit _ 0 _ = ""
    emit at remaining ps =
      let bar = fromIntegral (meter view)
          boundary = (fromInteger (floor (at/bar))+1)*bar
          allowed = min remaining (boundary-at)
          choices = [(6,"1."),(4,"1"),(3,"2."),(2,"2"),(3/2,"4."),(1,"4"),(3/4,"8."),(1/2,"8"),(3/8,"16."),(1/4,"16"),(1/8,"32")]
          -- Prefer values aligned to their undotted beat unit for readable rhythm.
          fitting = [(value,s) | (value,s) <- choices, value <= allowed, denominator (at / base value) == 1]
          (d,spelling) = case fitting of x:_ -> x; [] -> (1/8,"32")
          base value = if value `elem` [6,3,3/2,3/4,3/8] then value*2/3 else value
          chord = case ps of [] -> "r"; [p] -> pitch p; _ -> "<"++unwords (map pitch ps)++">"
          tie = if not (null ps) && d < remaining then "~" else ""
          barline = if at+d == boundary then " | " else " "
      in chord++spelling++tie++barline++emit (at+d) (remaining-d) ps

-- Greedy interval partition: each lane is monophonic; chord events stay intact.
separate :: [(Staff,Rational,Rational,[Int])] -> [[(Staff,Rational,Rational,[Int])]]
separate = foldl insert []
  where
    insert [] e = [[e]]
    insert (lane:lanes) e@(_,t,_,_) = case reverse lane of
      (_,a,d,_):_ | a+d <= t -> (lane++[e]):lanes
      _ -> lane:insert lanes e
