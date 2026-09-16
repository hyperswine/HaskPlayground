{-# LANGUAGE OverloadedStrings #-}

module MyStuff where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)
import Data.Int (Int64)

data Greymap = Greymap {greyWidth :: Int, greyHeight :: Int, greyMax :: Int, greyData :: L.ByteString} deriving (Eq, Show)

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString

getNat :: L.ByteString -> Maybe (Int, L.ByteString)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)

-- 1 so it doesnt beat $
infixl 1 |>
a |> b = b a

-- matchHeader prefix str | prefix `L8.isPrefixOf` str = Just $ L8.dropWhile isSpace $ L.drop (L.length prefix) str
matchHeader prefix str | prefix `L8.isPrefixOf` str = Just $ (L.length prefix) |> flip L.drop str |> L8.dropWhile isSpace
matchHeader _ _ = Nothing

b1 :: L.ByteString
b1 = "hi"

b2 :: L.ByteString
b2 = "hihi"

b3 :: L.ByteString
b3 = "h"

getNat s | Just (num, rest) <- L8.readInt s = if num <= 0 then Nothing else Just (fromIntegral num, rest)
getNat _ = Nothing

getBytes n str = let count = fromIntegral n
                     both@(prefix, _) = L.splitAt count str
                  in if L.length prefix < count then Nothing else Just both

-- does not say anything about consumption, but matcheHeader does consume
Nothing >>? _ = Nothing
Just v >>? f = f v

-- the >>? is a Just/Nothing to terminate early, railway oriented programming without having to define a failure mode each time due to common mode
-- but does not have extra state, have to do it yourself by returning (a, b) and matching on \(x, y)

skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
  matchHeader (L8.pack "P5") s  -- if it fails here, it will do Nothing >> _ = Nothing
  >>? \s -> skipSpace ((), s) -- instead of one element, return a pair
  >>? (getNat . snd) -- get snd element, then apply getNat to it (the width)
  >>? skipSpace -- optional, can just not see any spaces and the next char and keep going with Just rest
  >>? \(width, s) -> getNat s -- match a pair involving the width, then try to get a nat (height)
  >>? skipSpace
  >>? \(height, s) -> getNat s
  >>? \(maxGrey, s) -> getBytes 1 s
  >>? (getBytes (width * height) . snd)
  >>? \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

-- can use ParseState instead of manually doing it yourself or having to do _ / empty everywhere
firstParserFunction ==> secondParserFunction = Parse chainedParser where
  chainedParser initState = case (runParse firstParserFunction) initState of
    Left err -> Left err
    Right (res, state') -> runParse (secondParserFunction res) state'

-- file: ch10/Parse.hs
data ParseState = ParseState {string :: L.ByteString, offset :: Int64} deriving (Show)

newtype Parse a = Parse {runParse :: ParseState -> Either String (a, ParseState)}

lostNumbers = [4,8,15,16,23,42]

r1 = [1,2,3,4] ++ [9,10,11,12] -- ++ means append. Append means combining two lists into one. concat actually means combining a list of lists into a single list.
r2 = "hello" ++ " " ++ "world" -- uses [Char] by default rather than ByteString

r3 = 'A' : "Small Cat" -- ':' prepends a single element to the front of a list

r4 = "Steve Buscemi" !! 6 -- '!!' accesses the element at the given index in a list

-- 0 indexed, so 7th element
--- >>> r4
-- 'B'

r5 = [9.4, 33.2, 96.2, 11.2, 23.25] !! 1 -- defaults to Double because of literal decimal

--- >>> r5
-- 33.2

r6 = [[1,2,3,4], [5,3,3,3], [1,2,2,3,4], [1,2,3]] -- notice all literal ints, so it defaults to Integer, not Int or Float

-- notice how you have to ++ another list of lists
--- >>> r6 ++ [[1,1,1,1]]
-- [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3],[1,1,1,1]]

--- >>> [42,42,42] : r6
-- [[42,42,42],[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]

--- >>> head [5,4,3,2,1]
-- 5

--- >>> init [5,4,3,2,1]
-- [5,4,3,2]

mynull = null [5,4,3,2,1]

--- >>> mynull
-- False

mydroppedlist = drop 3 [8,4,2,1,5,6] -- drop the first 3 elements of the list

--- >>> mydroppedlist
-- [1,5,6]

mysum = sum [5,2,1,6,3,2,5,7]

--- >>> mysum
-- 31

myproduct = product [1,2,5,6,7,9,2,0]

--- >>> myproduct
-- 0

myrange = ['a'..'k']

--- >>> myrange
-- "abcdefghijk"

evennumbers = [2,4..20] -- start at 2, end at 20, step by 2

--- >>> evennumbers
-- [2,4,6,8,10,12,14,16,18,20]

mytaker = take 10 $ cycle [1,2,3] -- this cycles with 1 -> 2 -> 3 only rather than (1, 2, 3)

--- >>> mytaker
-- [1,2,3,1,2,3,1,2,3,1]

mytaker12 = take 12 $ cycle "LOL "

--- >>> mytaker12
-- "LOL LOL LOL "
