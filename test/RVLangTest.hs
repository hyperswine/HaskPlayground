{-# LANGUAGE OverloadedStrings #-}

module RVLangTest where

import qualified Data.Text as T
import RVLang (runCodeGen)

-- | Run a codegen example and print the result or error.
runExample :: String -> String -> IO ()
runExample label input = do
  putStrLn $ "=== " <> label <> " ==="
  putStrLn $ "Input: " <> input
  case runCodeGen (T.pack input) of
    Left err -> putStrLn $ "ERROR:\n" <> err
    Right asm -> putStrLn $ T.unpack asm

rvLangExamples = do
  runExample "Arithmetic" "1 + 2 * 4 + 6 / 2"
  runExample "Function with variables" "fn f x = x * 2, x = 2.0, y = 4444.0, f x + y"
  runExample "Multi-arg functions" "fn inc x = x + 1, fn add a b = a + b, add 3 4"
  runExample "If-then-else" "if 1 then 42 else 99"
  runExample "Stdlib: sin/cos/sqrt" "sqrt(sin(3.14159) ^ 2 + cos(3.14159) ^ 2)"
  runExample "Higher-order with fix" "fn myfunc x #f = if x then x else x + f (x - 1), fn myrecfunc x = fix myfunc, myrecfunc 5"
  runExample "Modulo" "10 % 3"
  runExample "Named constants pi and e" "sin(pi) + cos(0)"
