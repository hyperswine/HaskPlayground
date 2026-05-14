{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Pipeline where

import Data.Char (isAsciiLower)

-- A linear token - once used it is consumed We simulate linearity here with a wrapper; true linear types would require a linear types extension
newtype Linear a = Linear {runLinear :: a} deriving (Show)

-- The three function wrapper types
newtype FApp a b = FApp (a -> b)

newtype FRes a b = FRes (a -> Either String b)

newtype FLinear a b = FLinear (Linear a -> Either String (Linear b))

-- The Pipeline typeclass. (|>) :: input -> function_wrapper -> output
class Pipeline f i o | f -> i o where
  (|>) :: i -> f -> o

infixl 1 |>

-- Case 1: FApp — plain function application. a |> FApp f = f a
instance Pipeline (FApp a b) a b where
  x |> FApp f = f x

-- Case 2: FRes — Result short circuiting. Left err short circuits, Right continues
instance Pipeline (FRes a b) (Either String a) (Either String b) where
  Left err |> _ = Left err
  Right x |> FRes f = f x

-- Case 3: FLinear — Linear + Result. Combines resource tracking with short circuiting. The Linear token must be consumed by the function
instance Pipeline (FLinear a b) (Either String (Linear a)) (Either String (Linear b)) where
  Left err |> _ = Left err
  Right (Linear x) |> FLinear f = f (Linear x)

-- Convenience: lift a plain function into FRes
liftRes :: (a -> b) -> FRes a b
liftRes f = FRes (Right . f)

-- Convenience: lift into FLinear
liftLinear :: (a -> b) -> FLinear a b
liftLinear f = FLinear $ \(Linear x) -> Right $ Linear $ f x

-- Convenience: wrap a value to start a Result pipeline
ok :: a -> Either String a
ok = Right

-- Convenience: wrap into a linear result pipeline
okL :: a -> Either String (Linear a)
okL = Right . Linear

------------------------------------------------------------------------
-- Demo
------------------------------------------------------------------------

-- Some example functions

double :: Int -> Int
double x = x * 2

addOne :: Int -> Int
addOne x = x + 1

showInt :: Int -> String
showInt = show

-- Result-returning functions
validatePositive :: Int -> Either String Int
validatePositive x | x > 0 = Right x
validatePositive x = Left $ "expected positive, got: " ++ show x

validateSmall :: Int -> Either String Int
validateSmall x | x < 1000 = Right x
validateSmall x = Left $ "too large: " ++ show x

-- Linear-aware functions (simulate reading a resource once)
readResource :: Linear String -> Either String (Linear String)
readResource (Linear s) = Right $ Linear $ "read: " ++ s

processResource :: Linear String -> Either String (Linear String)
processResource (Linear s) = Right $ Linear $ map toUpperProxy s
  where
    toUpperProxy c | isAsciiLower c = toEnum $ fromEnum c - 32
    toUpperProxy c = c

closeResource :: Linear String -> Either String (Linear ())
closeResource (Linear _) = Right $ Linear () -- consume the token, return unit

------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Case 1: FApp - plain pipeline ==="

  -- < ACTUAL COMPUTATION >
  let result1 = 5 |> FApp double |> FApp addOne |> FApp showInt

  putStrLn $ "5 |> double |> addOne |> show = " ++ result1

  putStrLn "\n=== Case 2: FRes - short circuiting on error ==="

  -- < ACTUAL COMPUTATION >
  let result2 = ok 42 |> FRes validatePositive |> FRes validateSmall |> FRes (Right . double)

  putStrLn $ "ok 42  |> validatePositive |> validateSmall |> double = " ++ show result2

  -- < ACTUAL COMPUTATION >
  let result3 =
        ok (-5)
          |> FRes validatePositive -- short circuits here
          |> FRes validateSmall -- never runs
          |> FRes (Right . double) -- never runs
  putStrLn $ "ok -5  |> validatePositive |> ... = " ++ show result3

  -- < ACTUAL COMPUTATION >
  let result4 =
        ok 9999
          |> FRes validatePositive
          |> FRes validateSmall -- short circuits here
          |> FRes (Right . double) -- never runs
  putStrLn $ "ok 9999 |> validatePositive |> validateSmall |> ... = " ++ show result4

  putStrLn "\n=== Case 3: FLinear - linear resource pipeline ==="

  -- < ACTUAL COMPUTATION >
  let result5 = okL "myfile.txt" |> FLinear readResource |> FLinear processResource |> FLinear closeResource

  putStrLn $ "linear pipeline success = " ++ show result5

  -- Simulating a failure mid-pipeline
  let failingProcess :: Linear String -> Either String (Linear String)
      failingProcess _ = Left "resource corrupted"

  -- < ACTUAL COMPUTATION >
  let result6 =
        okL "myfile.txt"
          |> FLinear readResource
          |> FLinear failingProcess -- short circuits here
          |> FLinear closeResource -- never runs, token not double-used
  putStrLn $ "linear pipeline failure = " ++ show result6
