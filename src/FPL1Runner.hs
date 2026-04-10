{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | FPL1 file runner: parse a .fpl source file, emit RISC-V assembly,
-- compile with riscv64-unknown-elf-gcc, and execute under spike + pk.
--
-- Usage:
--   haskplayground-exe path/to/program.fpl
module FPL1Runner where

import qualified Data.Text.IO as TIO
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import FPL1 (runCompiler)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeBaseName, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

blue :: String -> String
blue s = "\ESC[34m" <> s <> "\ESC[0m"

red :: String -> String
red s = "\ESC[31m" <> s <> "\ESC[0m"

pk :: FilePath
pk = "/opt/homebrew/Cellar/riscv-pk/main/riscv64-unknown-elf/bin/pk"

cc :: FilePath
cc = "riscv64-unknown-elf-gcc"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fplFile] -> runFile fplFile
    _ -> do
      hPutStrLn stderr "Usage: haskplayground-exe <file.fpl>"
      exitFailure

runFile :: FilePath -> IO ()
runFile fplFile = do
  src <- TIO.readFile fplFile
  t0 <- getCurrentTime
  case runCompiler src of
    Left err -> do
      hPutStrLn stderr $ red $ "Parse/compile error:\n" <> show err
      exitFailure
    Right (asm, warnings) -> do
      t1 <- getCurrentTime
      let compileElapsed = diffUTCTime t1 t0
      putStrLn $ blue $ "Compile Succeeded (" <> show compileElapsed <> "). Preparing for Assembly"

      mapM_ (\w -> hPutStrLn stderr $ red $ "WARNING: " <> show w) warnings
      let label = takeBaseName fplFile
      withSystemTempDirectory "fpl1-spike" $ \dir -> do
        let asmFile = dir </> label <> ".s"
            binFile = dir </> label

        TIO.writeFile asmFile asm

        -- Assemble + link
        t2 <- getCurrentTime
        (ccEc, ccOut, ccErr) <- readProcessWithExitCode cc [asmFile, "-o", binFile, "-static", "-lm", "-O0"] ""
        t3 <- getCurrentTime
        let assembleElapsed = diffUTCTime t3 t2
        case ccEc of
          ExitFailure _ -> do
            hPutStrLn stderr $ red $ "Assembler/linker error:\n" <> ccErr <> ccOut
            exitFailure
          ExitSuccess -> do
            putStrLn $ blue $ "Assembly Succeeded (" <> show assembleElapsed <> "). Preparing for execution with Spike PK"

            -- Run under spike + pk
            t4 <- getCurrentTime
            (spEc, spOut, spErr) <- readProcessWithExitCode "spike" [pk, binFile] ""
            t5 <- getCurrentTime
            let execElapsed = diffUTCTime t5 t4
            case spEc of
              ExitSuccess -> do
                putStrLn $ blue $ "Execution Succeeded (" <> show execElapsed <> ")."
                putStr spOut
              ExitFailure c -> do
                hPutStrLn stderr $ red $ "spike exited with code " <> show c <> ":\n" <> spErr <> spOut
                exitFailure
