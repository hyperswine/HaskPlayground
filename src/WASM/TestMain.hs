{-# LANGUAGE LambdaCase #-}

module WASM.TestMain (main) where

import Control.Monad (forM)
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Int (Int64)
import WASM.WasmVM
import WASM.WatParser

data Expect = Ok [Val] | Trap String

noHost :: HostResolver
noHost m n _ = pure (Left ("unexpected host call " ++ m ++ "." ++ n))

i32c :: Integer -> Val
i32c = VI32 . fromIntegral

i64c :: Integer -> Val
i64c = VI64 . fromIntegral

cases :: [(String, [Val], Expect)]
cases =
  [ ("fib", [i32c 20], Ok [i32c 6765]),
    ("dispatch", [i32c 0, i32c 7, i32c 5], Ok [i32c 12]),
    ("dispatch", [i32c 1, i32c 7, i32c 5], Ok [i32c 2]),
    ("dispatch", [i32c 2, i32c 7, i32c 5], Ok [i32c 35]),
    ("dispatch_badtype", [i32c 5], Trap "type mismatch"),
    ("dispatch_oob", [], Trap "undefined element"),
    ("wrap", [], Ok [i32c (-2147483648)]),
    ("divtrap", [], Trap "integer overflow"),
    ("divzero", [], Trap "divide by zero"),
    ("rem_s", [], Ok [i32c (-1)]),
    ("i64mul", [], Ok [i64c 0]),
    ("i64big", [], Ok [i64c 12884901888]),
    ("shr_s", [], Ok [i32c (-4)]),
    ("shr_u", [], Ok [i32c 1073741820]),
    ("rotl", [], Ok [i32c 3]),
    ("clz", [], Ok [i32c 31]),
    ("ctz64", [], Ok [i64c 32]),
    ("popcnt", [], Ok [i32c 8]),
    ("ext8", [], Ok [i32c (-128)]),
    ("lt_u", [], Ok [i32c 0]),
    ("lt_s", [], Ok [i32c 1]),
    ("extend_u", [], Ok [i64c 4294967295]),
    ("extend_s", [], Ok [i64c (-1)]),
    ("f64div", [], Ok [VF64 0.25]),
    ("nearest_even", [], Ok [VF64 2]),
    ("nearest_odd", [], Ok [VF64 4]),
    ("fmin_zero", [], Ok [i64c 0x8000000000000000]),
    ("copysign", [], Ok [VF32 (-3)]),
    ("truncsat", [], Ok [i32c 4294967295]),
    ("truncsat_neg", [], Ok [i32c (-2147483648)]),
    ("truncsat_nan", [], Ok [i32c 0]),
    ("trunctrap", [], Trap "integer overflow"),
    ("trunc_ok", [], Ok [i32c (-7)]),
    ("reinterp", [], Ok [i32c 1065353216]),
    ("convert", [], Ok [VF64 4294967295]),
    ("demote", [], Ok [VF32 0.1]),
    ("brtable", [i32c 0], Ok [i32c 0]),
    ("brtable", [i32c 1], Ok [i32c 10]),
    ("brtable", [i32c 2], Ok [i32c 20]),
    ("brtable", [i32c 5], Ok [i32c 20]),
    ("brval", [], Ok [i32c 43]),
    ("select", [i32c 1], Ok [i32c 111]),
    ("select", [i32c 0], Ok [i32c 222]),
    ("loop_sum", [i32c 100], Ok [i32c 5050]),
    ("early_return", [], Ok [i32c 7]),
    ("unreachable", [], Trap "unreachable"),
    ("stack_overflow", [], Trap "call stack exhausted"),
    ("global_inc", [], Ok [i32c 10]),
    ("global_inc", [], Ok [i32c 20]),
    ("mem_le", [], Ok [i32c 68]),
    ("mem16s", [], Ok [i32c (-2)]),
    ("mem_i64", [], Ok [i64c 4294967294]),
    ("mem_f64", [], Ok [VF64 2.5]),
    ("memgrow", [], Ok [i32c 3]),
    ("memfill", [], Ok [i32c 117901063]),
    ("oob", [], Trap "out of bounds"),
    ("oob_grown", [], Ok [i32c 0])
  ]

main :: IO ()
main = do
  args <- getArgs
  src <- readFile "tests.wat"
  case parseWat src of
    Left e -> putStrLn ("parse error: " ++ e) >> exitFailure
    Right pm | args == ["--diff"] -> diffMode pm
    Right pm -> do
      inst <- instantiate (pmModule pm)
      -- globals live in the VM record; thread them across calls so the
      -- instance behaves statefully (global_inc twice)
      let run (name, args, expect) = do
            r <- invoke noHost inst name args
            let ok = case (r, expect) of
                  (Right vs, Ok want) -> vs == want
                  (Left e, Trap want) -> want `isInfixOf` e
                  _ -> False
            putStrLn $ (if ok then "  ok   " else "  FAIL ") ++ name ++ show (map showV args) ++ " -> " ++ either id (show . map showV) r
            pure ok
      results <- forM cases run
      let passed = length (filter id results)
      putStrLn $ show passed ++ "/" ++ show (length results) ++ " passed"
      if passed == length results then pure () else exitFailure

showV :: Val -> String
showV = \case
  VI32 w -> show (fromIntegral w :: Int) ++ "i32(s=" ++ show (i32s (VI32 w)) ++ ")"
  VI64 w -> show w ++ "i64"
  VF32 f -> show f ++ "f32"
  VF64 d -> show d ++ "f64"

-- fresh instance per case, wasmtime-style formatting, for differential testing
diffMode :: ParsedModule -> IO ()
diffMode pm = mapM_ one cases
  where
    one (name, args, _) = do
      inst <- instantiate (pmModule pm)
      r <- invoke noHost inst name args
      putStrLn $ name ++ "|" ++ unwords (map wt args) ++ "|" ++ either id (unwords . map wt) r
    wt = \case
      VI32 w -> show (i32s (VI32 w))
      VI64 w -> show (fromIntegral w :: Int64)
      VF32 f -> show f
      VF64 d -> show d
