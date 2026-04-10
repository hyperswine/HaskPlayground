{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | FPL1 file runner: parse a .fpl source file, emit RISC-V assembly,
-- compile with riscv64-unknown-elf-gcc, and execute under spike + pk.
--
-- Usage:
--   haskplayground-exe path/to/program.fpl
module FPL1Runner where

import qualified Data.Text.IO as TIO
import FPL1 (runCompiler)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeBaseName, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

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
  case runCompiler src of
    Left err -> do
      hPutStrLn stderr $ "Parse/compile error:\n" <> show err
      exitFailure
    Right (asm, warnings) -> do
      mapM_ (\w -> hPutStrLn stderr $ "WARNING: " <> show w) warnings
      let label = takeBaseName fplFile
      withSystemTempDirectory "fpl1-spike" $ \dir -> do
        let asmFile = dir </> label <> ".s"
            binFile = dir </> label

        TIO.writeFile asmFile asm

        -- Assemble + link
        (ccEc, ccOut, ccErr) <-
          readProcessWithExitCode cc [asmFile, "-o", binFile, "-static", "-lm", "-O0"] ""
        case ccEc of
          ExitFailure _ -> do
            hPutStrLn stderr $ "Assembler/linker error:\n" <> ccErr <> ccOut
            exitFailure
          ExitSuccess -> do
            -- Run under spike + pk
            (spEc, spOut, spErr) <-
              readProcessWithExitCode "spike" [pk, binFile] ""
            case spEc of
              ExitSuccess -> putStr spOut
              ExitFailure c -> do
                hPutStrLn stderr $ "spike exited with code " <> show c <> ":\n" <> spErr <> spOut
                exitFailure
