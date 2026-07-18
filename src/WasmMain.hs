{-# LANGUAGE LambdaCase #-}

module WasmMain (main) where

import qualified Data.Map.Strict as M
import WasmModel hiding (main)
import WatParser (parseWat)

mkWorld :: String -> World
mkWorld expr =
  World
    { wGrants = M.empty,
      wFiles = M.fromList [("expr.txt", expr ++ "\n")],
      wPolicy = \case
        FileCap R "expr.txt" -> True -- user ALLOWs reading expr.txt
        _ -> False
    }

main :: IO ()
main = do
  src <- readFile "examples/calc.wat"
  case parseWat src of
    Left err -> putStrLn ("WAT parse error: " ++ err)
    Right (body, segs) -> do
      putStrLn $ "parsed calc.wat: " ++ show (length body) ++ " top-level instrs, "
              ++ show (length segs) ++ " data segments"
      putStrLn ""
      putStrLn "=== run 1: \"23 * 45\"  (calc + ticker interleaved; open prompts user) ==="
      runRounds (mkWorld "23 * 45") [mkActor "calc" body segs, ticker]
      mapM_ (quickRun body segs)
        [ "100 / 7",
          "7 - 100",
          "9 / 0",
          "1 ? 2"
        ]

quickRun :: [Instr] -> [(Int, String)] -> String -> IO ()
quickRun body segs expr = do
  putStrLn ""
  putStrLn $ "=== run: \"" ++ expr ++ "\" ==="
  runRounds (mkWorld expr) [mkActor "calc" body segs]
