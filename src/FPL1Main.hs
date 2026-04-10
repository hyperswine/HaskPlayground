{-# LANGUAGE OverloadedStrings #-}
module FPL1Main where

import FPL1
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Example FP-RISC v1 programs

-- 1. Factorial with accumulator and fix (tail-jump)
prog_fact :: T.Text
prog_fact = T.unlines
  [ "fn fact n acc : Int ="
  , "  if n == 0"
  , "  then acc"
  , "  else fix fact (n - 1) (n * acc)."
  , ""
  , "fn fact0 n : Int ="
  , "  fact n 1."
  , ""
  , "fact0 5"
  ]

-- 2. Records, iso, VList (SoA/SIMD eligible)
prog_vlist_soa :: T.Text
prog_vlist_soa = T.unlines
  [ "record Vec3 { x: Float, y: Float, z: Float }"
  , ""
  , "iso Vec3 <-> VList Float = { to = vec3_to_vlist, from = vec3_from_vlist }"
  , ""
  , "fn vec3_dot a : Int b : Int : Float ="
  , "  1.0."
  , ""
  , "42"
  ]

-- 3. Tagged union with case desugaring
prog_union :: T.Text
prog_union = T.unlines
  [ "union Option { Some(value: Int) | None }"
  , ""
  , "fn unwrap_or opt default : Int ="
  , "  case opt of"
  , "  | Some v -> v"
  , "  | None   -> default."
  , ""
  , "unwrap_or 0 99"
  ]

-- 4. Higher-order function via fn-ptr (no closures)
prog_higher :: T.Text
prog_higher = T.unlines
  [ "fn apply #f x : Int ="
  , "  f x."
  , ""
  , "fn double x : Int ="
  , "  x * 2."
  , ""
  , "apply double 21"
  ]

-- 5. Actor spawn + send
prog_actor :: T.Text
prog_actor = T.unlines
  [ "fn counter n : Int ="
  , "  fix counter (n + 1)."
  , ""
  , "let pid = spawn counter 0 in"
  , "send pid 1"
  ]

-- 6. Module system
prog_module :: T.Text
prog_module = T.unlines
  [ "module Math {"
  , "  fn square x : Int = x * x."
  , "  fn cube   x : Int = x * x * x."
  , "}"
  , ""
  , "Math.square 7"
  ]

-- 7. UNIX interface
prog_unix :: T.Text
prog_unix = T.unlines
  [ "fn copy_file src_fd dst_fd : Int ="
  , "  let buf = read src_fd 4096 in"
  , "  write dst_fd buf."
  , ""
  , "42"
  ]

-- 8. VList literal + indexing
prog_vlist :: T.Text
prog_vlist = T.unlines
  [ "let xs = [1, 2, 3, 4, 5] in"
  , "42"
  ]

showResult :: String -> T.Text -> IO ()
showResult label prog = do
  putStrLn $ "\n" ++ replicate 60 '='
  putStrLn $ "=== " ++ label
  putStrLn $ replicate 60 '='
  putStrLn "-- Source:"
  TIO.putStr prog
  putStrLn "\n-- Output:"
  case runCompiler prog of
    Left  err       -> TIO.putStrLn $ "ERROR: " <> err
    Right (asm, ws) -> do
      mapM_ (\w -> TIO.putStrLn $ "WARNING: " <> w) ws
      TIO.putStr asm

main :: IO ()
main = do
  showResult "1. Factorial (fix tail-jump)"      prog_fact
  showResult "2. Records + iso + SoA/SIMD"       prog_vlist_soa
  showResult "3. Tagged union + case desugaring"  prog_union
  showResult "4. Higher-order via fn-ptr"         prog_higher
  showResult "5. Actors (spawn/send)"             prog_actor
  showResult "6. Module system"                   prog_module
  showResult "7. UNIX interface"                  prog_unix
  showResult "8. VList literal"                   prog_vlist
