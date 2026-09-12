module Main (main) where

import NeoMusic (demo, writeAudio)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> writeAudio "neo-music.wav" 44100 demo
    [path] -> writeAudio path 44100 demo
    _ -> ioError (userError "usage: neo-music [output.wav|output.mp3|output.flac]")
