-- | The Clash compiler, run through cabal so it sees this project's
-- packages (cabal.project writes a GHC environment file):
--
-- > cabal run clash -- src/SimpleRisc.hs --verilog
module Main (main) where

import Clash.Main (defaultMain)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= defaultMain
