module Main (main) where

import qualified Data.ByteString.Lazy as BL
import qualified NeoMusic as A
import NeoMusic.Language
import NeoMusic.Midi (midi)
import NeoMusic.Numeric (numericHtml, numericPage)
import NeoMusic.Sheet (engrave)
import qualified NeoMusic.Score as S
import System.Environment (getArgs)
import System.Directory (findExecutable)
import System.FilePath (dropExtension, takeExtension)
import System.Process (callProcess)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> A.writeAudio "neo-music.wav" 44100 A.demo
    [path] | takeExtension path `elem` [".wav",".mp3",".flac"] -> A.writeAudio path 44100 A.demo
    [command,input,"-o",output] -> run command input output False
    ["staff",input,"-o",output,"--pdf"] -> run "staff" input output True
    ["sheet",input] -> readDocument input >>= \doc -> orDie (numericPage (performance doc) (score doc)) >>= putStr
    ["check",input] -> do
      doc <- readDocument input
      timeline <- orDie (S.render (performance doc) (score doc))
      putStrLn (show (length (A.notes timeline))++" notes, "++show (A.seconds timeline)++" seconds")
    _ -> die "usage: neo-music {play|midi|sheet|hybrid|staff} song.neomusic -o output [--pdf for staff]\n       neo-music check song.neomusic\n       neo-music [demo.wav|demo.mp3|demo.flac]"

readDocument :: FilePath -> IO Document
readDocument path = readFile path >>= orDie . parseDocument
orDie :: Either String a -> IO a
orDie = either die pure
run :: String -> FilePath -> FilePath -> Bool -> IO ()
run command input output pdf = do
  doc <- readDocument input
  let perf = performance doc
      music = score doc
  case command of
    "play" -> S.writeAudio output perf music
    "midi" -> orDie (midi perf music) >>= BL.writeFile output
    "sheet" -> case takeExtension output of
      ".txt" -> orDie (numericPage perf music) >>= writeFile output
      ".html" -> orDie (numericHtml False perf music) >>= writeFile output
      _ -> die "numeric sheet output must be .txt or .html; use staff for LilyPond/PDF"
    "hybrid" -> if takeExtension output == ".html"
      then orDie (numericHtml True perf music) >>= writeFile output
      else die "hybrid preview output must be .html"
    "staff" -> do
      if takeExtension output /= ".ly" then die "staff output must have .ly extension" else pure ()
      text <- orDie (engrave perf (sheetView doc) music)
      writeFile output text
      if pdf then do
        executable <- findExecutable "lilypond"
        program <- maybe (die "LilyPond source was written; PDF requires lilypond on PATH") pure executable
        callProcess program ["--pdf","-o",dropExtension output,output]
      else pure ()
    _ -> die ("unknown command: "++command)
