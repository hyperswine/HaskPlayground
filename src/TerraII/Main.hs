{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TerraII.Main where

import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import TerraII.GameLoop (run)
import TerraII.World (initGame)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  let seed = case args of (s : _) -> read s; [] -> 42
  run (initGame seed)
