module Main (main) where

import qualified Data.ByteString.Lazy as BL
import qualified NeoMusic.Audio as A
import NeoMusic.Language
import NeoMusic.Midi (midi)
import NeoMusic.Numeric (NumericView (..), numericHtml, numericPage, plain)
import NeoMusic.Sheet (engrave)
import qualified NeoMusic.Score as S
import System.Environment (getArgs, lookupEnv)
import System.Directory (findExecutable)
import System.FilePath (dropExtension, takeExtension)
import System.Process (callProcess)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [command,input,"-o",output] -> run command input output []
    "sheet":input:"-o":output:flags | flags == ["--marks"] -> run "sheet" input output flags
    "hybrid":input:"-o":output:flags | flags == ["--marks"] -> run "hybrid" input output flags
    "staff":input:"-o":output:flags | all (`elem` ["--pdf","--no-numbers"]) flags -> run "staff" input output flags
    "sheet":input:flags | all (== "--marks") flags -> readDocument input >>= \doc ->
      orDie (numericPage (plain {showMarks = not (null flags)}) (performance doc) (score doc)) >>= putStr
    ["check",input] -> do
      doc <- readDocument input
      timeline <- orDie (S.render (performance doc) (score doc))
      putStrLn (show (length (A.notes timeline))++" notes, "++show (A.seconds timeline)++" seconds")
    _ -> die "usage: neo-music {play|midi|sheet|hybrid|staff} song.neomusic -o output [sheet/hybrid: --marks] [staff: --pdf --no-numbers]\n       neo-music check song.neomusic"

-- | A document with the recordings it names, found next to it or under each
-- directory in NEOMUSIC_SAMPLES (separated by colons).
readDocument :: FilePath -> IO Document
readDocument path = do
  dirs <- maybe [] (splitOn ':') <$> lookupEnv "NEOMUSIC_SAMPLES"
  loadDocument dirs path >>= orDie
  where
    splitOn c xs = case break (== c) xs of
      (a, []) -> [a | not (null a)]
      (a, _ : rest) -> [a | not (null a)] ++ splitOn c rest
orDie :: Either String a -> IO a
orDie = either die pure
run :: String -> FilePath -> FilePath -> [String] -> IO ()
run command input output flags = do
  doc <- readDocument input
  let perf = performance doc
      music = score doc
      reading = plain {showMarks = "--marks" `elem` flags}
  case command of
    "play" -> S.writeAudio output perf music
    "midi" -> orDie (midi perf music) >>= BL.writeFile output
    "sheet" -> case takeExtension output of
      ".txt" -> orDie (numericPage reading perf music) >>= writeFile output
      ".html" -> orDie (numericHtml reading perf music) >>= writeFile output
      _ -> die "numeric sheet output must be .txt or .html; use staff for LilyPond/PDF"
    "hybrid" -> if takeExtension output == ".html"
      then orDie (numericHtml reading {flatBars = True} perf music) >>= writeFile output
      else die "hybrid preview output must be .html"
    "staff" -> do
      if takeExtension output /= ".ly" then die "staff output must have .ly extension" else pure ()
      let view = (sheetView doc) {numbers = "--no-numbers" `notElem` flags}
      text <- orDie (engrave perf view music)
      writeFile output text
      if "--pdf" `elem` flags then do
        executable <- findExecutable "lilypond"
        program <- maybe (die "LilyPond source was written; PDF requires lilypond on PATH") pure executable
        callProcess program ["--pdf","-o",dropExtension output,output]
      else pure ()
    _ -> die ("unknown command: "++command)
