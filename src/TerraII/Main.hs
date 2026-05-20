module TerraII.Main where

import System.Environment (getArgs)
import System.IO (hSetBuffering, stdout, BufferMode(..))

import TerraII.World (initGame)
import TerraII.GameLoop (run)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  let seed = case args of { (s:_) -> read s; [] -> 42 }
  run (initGame seed)
