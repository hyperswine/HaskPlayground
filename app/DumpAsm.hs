{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module DumpAsm where

import qualified Data.Text.IO as TIO
import FPL1 (runCompiler)

main :: IO ()
main = do
  src <- TIO.readFile "examples/fact.fpl"
  case runCompiler src of
    Left e -> print e
    Right (asm, _ws) -> TIO.putStr asm
