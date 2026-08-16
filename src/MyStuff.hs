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
