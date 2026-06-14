{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module PipelinePrime where

import Data.Char (toUpper)

-- Simulated linear resource handle
newtype Linear a = Linear {runLinear :: a} deriving (Show)

-- | The pipe class: dispatch on (function type f, input type i) -> output type o.
-- No wrapper constructors — f is a raw function, e.g. (a -> b) or (a -> Either e b).
class Pipe f i o | f i -> o where
  (|>) :: i -> f -> o

infixl 1 |>

------------------------------------------------------------------------
-- Dispatch table, ordered most-specific to most-general
------------------------------------------------------------------------

-- 4. Linear-handle threading through Either:
--    i = Either e (Linear a), f = Linear a -> Either e (Linear b)
--    Always thread the handle (well, here: only on Right — the Left
--    case has no handle, matching "discharge on error path" semantics)
instance Pipe (Linear a -> Either e (Linear b)) (Either e (Linear a)) (Either e (Linear b)) where
  (|>) (Left e) _ = Left e
  (|>) (Right x) f = f x

-- 3. Result andThen: i = Either e a, f = a -> Either e b
instance Pipe (a -> Either e b) (Either e a) (Either e b) where
  (|>) (Left e) _ = Left e
  (|>) (Right x) f = f x

-- 1. Plain application: i = a, f = a -> b
instance Pipe (a -> b) a b where
  (|>) x f = f x

------------------------------------------------------------------------
-- Demo functions — all raw functions, no lift/wrap helpers needed
------------------------------------------------------------------------

double :: Int -> Int
double = (* 2)

addOne :: Int -> Int
addOne = (+ 1)

validatePositive :: Int -> Either String Int
validatePositive x
  | x > 0 = Right x
  | otherwise = Left $ "expected positive, got: " ++ show x

validateSmall :: Int -> Either String Int
validateSmall x
  | x < 1000 = Right x
  | otherwise = Left $ "too large: " ++ show x

readResource :: Linear String -> Either String (Linear String)
readResource (Linear s) = Right $ Linear $ "read: " ++ s

processResource :: Linear String -> Either String (Linear String)
processResource (Linear s) = Right $ Linear $ map toUpper s

closeResource :: Linear String -> Either String (Linear ())
closeResource (Linear _) = Right $ Linear ()

failingProcess :: Linear String -> Either String (Linear String)
failingProcess _ = Left "resource corrupted"

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Plain application ==="
  let result1 = show $ (5 :: Int) |> double |> addOne
  putStrLn $ "5 |> double |> addOne = " ++ result1

  putStrLn "\n=== Result map / andThen, auto-dispatched ==="
  let result2 = fmap double $ (Right 42 :: Either String Int) |> validatePositive |> validateSmall
  putStrLn $ "Right 42 |> validatePositive |> validateSmall, then double = " ++ show result2

  let result3 = fmap double $ (Right (-5) :: Either String Int) |> validatePositive |> validateSmall
  putStrLn $ "Right (-5) |> ... = " ++ show result3

  let result4 = fmap double $ (Right 9999 :: Either String Int) |> validatePositive |> validateSmall
  putStrLn $ "Right 9999 |> ... = " ++ show result4

  putStrLn "\n=== Linear handle threading, auto-dispatched ==="
  let result5 = (Right (Linear "myfile.txt") :: Either String (Linear String)) |> readResource |> processResource |> closeResource
  putStrLn $ "linear pipeline success = " ++ show result5

  let result6 = (Right (Linear "myfile.txt") :: Either String (Linear String)) |> readResource |> failingProcess |> closeResource
  putStrLn $ "linear pipeline failure = " ++ show result6
