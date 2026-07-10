-- stack exec haskplayground-exe -- dumpasm

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import qualified DumpAsm
import qualified QuantUI
import System.Environment (getArgs)
import qualified ARC
import qualified MailboxResponse
import qualified MVUModLiveView
import qualified FullAST
import qualified MemoryModel
import qualified SPSCArc
import qualified HaskSim1

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["quantui"] -> QuantUI.main
    ["dumpasm"] -> DumpAsm.main
    ["arc"] -> ARC.main
    ["mailbox"] -> MailboxResponse.main
    ["mvu"] -> MVUModLiveView.main
    ["full"] -> FullAST.main
    ["mem"] -> MemoryModel.main
    ["spsc"] -> SPSCArc.main
    ["sim1"] -> HaskSim1.main
    _ -> do
      putStrLn "Not supplied, running QuantUI"
      QuantUI.main
