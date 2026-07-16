-- stack exec haskplayground-exe -- dumpasm

{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import qualified ARC
import qualified AsmPlan
import qualified DumpAsm
import qualified HaskSim1
import qualified MVULog
import qualified MVUModLiveView
import qualified MailboxResponse
import qualified MemoryModel
import qualified MiniBrowser
import qualified PortSim
import qualified QuantUI
import qualified SPSCArc
import qualified SolcMain
import qualified Solx
import qualified QosModel
import qualified WasmModel
import System.Environment (getArgs)
import qualified TUIMVUProofOfConcept

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["quantui"] -> QuantUI.main
    ["dumpasm"] -> DumpAsm.main
    ["arc"] -> ARC.main
    ["mailbox"] -> MailboxResponse.main
    ["mvu"] -> MVUModLiveView.main
    -- ["full"] -> FullAST.main
    ["mem"] -> MemoryModel.main
    ["spsc"] -> SPSCArc.main
    ["sim1"] -> HaskSim1.main
    ["solc", args] -> SolcMain.main args
    ["solx", args] -> Solx.main [args]
    ["mvulog"] -> MVULog.main
    ["mvupoc"] -> TUIMVUProofOfConcept.main
    ["asmplan"] -> AsmPlan.main
    ["minibrowser"] -> MiniBrowser.main
    ["portsim", args] -> PortSim.main [args]
    ["qosmodel"] -> QosModel.main
    ["wasmmodel"] -> WasmModel.main
    _ -> do
      putStrLn "Not supplied, running QuantUI"
      QuantUI.main
