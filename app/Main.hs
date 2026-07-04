-- stack exec haskplayground-exe -- dumpasm

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import qualified DumpAsm
import qualified QuantUI
import System.Environment (getArgs)
import qualified ARC
import qualified MailboxReponse
import qualified MVUModLiveView

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["quantui"] -> QuantUI.main
    ["dumpasm"] -> DumpAsm.main
    ["arc"] -> ARC.main
    ["mailbox"] -> MailboxReponse.main
    ["mvu"] -> MVUModLiveView.main
    _ -> do
      putStrLn "Not supplied, running QuantUI"
      QuantUI.main
