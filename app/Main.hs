-- stack exec haskplayground-exe -- quantui

{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import qualified QuantUI
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["quantui"] -> QuantUI.main
    _ -> putStrLn "Not supplied, exiting..."
