{-# LANGUAGE OverloadedStrings #-}
-- | Generate RISC-V assembly from RVLang examples, compile with
-- riscv64-unknown-elf-gcc, and run under spike + pk.
module SpikeRun where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import RVLang (runCodeGen)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

pk :: FilePath
pk = "/opt/homebrew/Cellar/riscv-pk/main/riscv64-unknown-elf/bin/pk"

cc :: FilePath
cc = "riscv64-unknown-elf-gcc"

examples :: [(String, String)]
examples =
  [ ("arithmetic",   "1 + 2 * 4 + 6 / 2")
  , ("function-var", "fn f x = x * 2, x = 2.0, y = 4444.0, f x + y")
  , ("multi-arg",    "fn inc x = x + 1, fn add a b = a + b, add 3 4")
  , ("if-else",      "if 1 then 42 else 99")
  , ("trig-id",      "sqrt(sin(3.14159) ^ 2 + cos(3.14159) ^ 2)")
  , ("modulo",       "10 % 3")
  , ("pi-e",         "sin(pi) + cos(0)")
  , ("higher-order", "fn myfunc x #f = if x then x else x + f (x - 1), fn myrecfunc x = fix myfunc, myrecfunc 5")
  ]

runOnSpike :: IO ()
runOnSpike = withSystemTempDirectory "rvlang-spike" $ \dir -> do
  mapM_ (runOne dir) examples

runOne :: FilePath -> (String, String) -> IO ()
runOne dir (label, src) = do
  putStrLn $ "=== " <> label <> " ==="
  putStrLn $ "  src : " <> src
  case runCodeGen (T.pack src) of
    Left err -> putStrLn $ "  CODEGEN ERROR:\n" <> err
    Right asm -> do
      let asmFile = dir </> label <> ".s"
          binFile = dir </> label
      TIO.writeFile asmFile asm
      -- Compile: -static links newlib printf; -lm for sin/cos/pow/sqrt
      (cc_ec, cc_out, cc_err) <- readProcessWithExitCode cc
        [asmFile, "-o", binFile, "-static", "-lm", "-O0"] ""
      case cc_ec of
        ExitFailure _ -> putStrLn $ "  COMPILE ERROR:\n" <> cc_err <> cc_out
        ExitSuccess   -> do
          -- Run under spike + pk
          (sp_ec, sp_out, sp_err) <- readProcessWithExitCode "spike"
            [pk, binFile] ""
          case sp_ec of
            ExitSuccess   -> putStrLn $ "  output: " <> sp_out
            ExitFailure c -> putStrLn $ "  SPIKE EXIT " <> show c <> ":\n" <> sp_err <> sp_out
