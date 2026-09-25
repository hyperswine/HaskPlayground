{-# LANGUAGE LambdaCase #-}

module WASM.Main (main) where

import WASM.Actors
import qualified Data.Map.Strict as M
import WASM.WasmVM
import WASM.WatParser

mkWorld :: String -> World
mkWorld expr =
  World
    { wGrants = M.empty,
      wFiles = M.fromList [("expr.txt", expr ++ "\n")],
      wPolicy = \case FileCap R "expr.txt" -> True; _ -> False
    }

load :: FilePath -> IO (Module, Int)
load path = do
  src <- readFile path
  case parseWat src of
    Left e -> fail (path ++ ": " ++ e)
    Right pm -> case M.lookup "_start" (mExports (pmModule pm)) of
      Just i -> pure (pmModule pm, i)
      Nothing -> fail (path ++ ": no _start export")

actorFrom :: String -> (Module, Int) -> IO Actor
actorFrom name (m, entry) = do
  inst <- instantiate m
  pure (mkActor name inst entry)

main :: IO ()
main = do
  calc <- load "calc.wat"
  tick <- load "ticker.wat"
  putStrLn "=== run 1: \"23 * 45\"  (calc + ticker interleaved; open prompts user) ==="
  c <- actorFrom "calc" calc
  t <- actorFrom "ticker" tick
  runRounds (mkWorld "23 * 45") [c, t]
  mapM_
    (\expr -> do
       putStrLn ""
       putStrLn ("=== run: \"" ++ expr ++ "\" ===")
       a <- actorFrom "calc" calc
       runRounds (mkWorld expr) [a])
    ["100 / 7", "7 - 100", "9 / 0", "1 ? 2"]
