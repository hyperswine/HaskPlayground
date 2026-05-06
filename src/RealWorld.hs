{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module RealWorld where

import Text.Read (readMaybe)

data Color = Red | Green | Blue deriving (Show)

colorEq Red Red = True
colorEq Green Green = True
colorEq Blue Blue = True
colorEq _ _ = False

--- >>> colorEq Red Red
-- True

eq1 = colorEq Red Green

--- >>> eq1
-- False

class BasicEq a where
  isEqual :: a -> a -> Bool

--- >>> :t isEqual
-- isEqual :: BasicEq a => a -> a -> Bool

class BasicEq3 a where
  -- Only have to implement one of these unctions
  isEqual3 :: a -> a -> Bool
  isEqual3 x y = not $ isNotEqual3 x y

  isNotEqual3 :: a -> a -> Bool
  isNotEqual3 x y = not $ isEqual3 x y

instance BasicEq3 Color where
  -- WITHOUT at least one, it will infinitely loop
  isEqual3 Red Red = True
  isEqual3 Green Green = True
  isEqual3 Blue Blue = True
  isEqual3 _ _ = False

--- >>> :type (read "5")
-- (read "5") :: Read a => a

x = readMaybe "5" :: Maybe Double

--- >>> x
-- Just 5.0

instance Read Color where
  readsPrec _ value = tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
    where
      tryParse [] = []
      tryParse ((attempt, res) : xs) = if take (length attempt) value == attempt then [(res, drop (length attempt) value)] else tryParse xs

x' = readMaybe "Reds" :: Maybe Color

--- >>> x'
-- Nothing

d1 = [Just 5, Nothing, Nothing, Just 8, Just 9] :: [Maybe Int]

--- >>> writeFile "serialized-d1.txt" (show d1)

input = do
  x <- readFile "serialized-d1.txt"
  let d2 = read x :: [Maybe Int]
  return d2

--- >>> input
-- [Just 5,Nothing,Nothing,Just 8,Just 9]

mymain = do
  putStr "Greetings! Name PLZ: "
  inpStr <- getLine
  putStrLn $ "Welcome, " ++ inpStr ++ "!"
