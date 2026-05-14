{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module RealWorld where

import Control.Monad (forM, forM_, liftM, when)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (runReaderT), ask)
import Control.Monad.State (MonadState, StateT (runStateT))
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (isSpace)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Void (Void)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Text.ParserCombinators.Parsec
import Text.Printf (printf)
import Text.Read (readMaybe)

data AppConfig = AppConfig {cfgMaxDepth :: Int} deriving (Show)

data AppState = AppState {stDeepestReached :: Int} deriving (Show)

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

data Greymap = Greymap {greyWidth :: Int, greyHeight :: Int, greyMax :: Int, greyData :: L.ByteString} deriving (Eq)

instance Show Greymap where
  show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

class (Show a) => MyTypeClass a where
  myfunc :: a -> String
  myfunc x = "myfunc: " ++ show x

instance MyTypeClass Greymap

bs = L8.pack

defaultGreymap = Greymap 1 2 3 (bs "Grey")

--- >>> myfunc defaultGreymap
-- "myfunc: Greymap 1x2 3"

parseP5 :: L8.ByteString -> Maybe (Greymap, L8.ByteString)
parseP5 s = case matchHeader (L8.pack "P5") s of
  Nothing -> Nothing
  Just s1 -> case getNat s1 of
    Nothing -> Nothing
    Just (width, s2) -> case getNat (L8.dropWhile isSpace s2) of
      Nothing -> Nothing
      Just (height, s3) -> case getNat (L8.dropWhile isSpace s3) of
        Nothing -> Nothing
        Just (maxGrey, _) | maxGrey > 255 -> Nothing
        Just (maxGrey, s4) -> case getBytes 1 s4 of
          Nothing -> Nothing
          Just (_, s5) -> case getBytes (width * height) s5 of
            Nothing -> Nothing
            Just (bitmap, s6) -> Just (Greymap width height maxGrey bitmap, s6)

matchHeader prefix str | prefix `L8.isPrefixOf` str = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
matchHeader _ _ = Nothing

getNat s = case L8.readInt s of
  Nothing -> Nothing
  Just (num, _) | num <= 0 -> Nothing
  Just (num, rest) -> Just (fromIntegral num, rest)

getBytes n str = if L.length prefix < count then Nothing else Just both
  where
    count = fromIntegral n
    both@(prefix, _) = L.splitAt count str

data MovieReview = MovieReview {revTitle :: String, revUser :: String, revReview :: String}

csv = endBy line eol

line = sepBy cell (char ',')

cell = many $ noneOf ",\n"

eol = string "\n"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csv "(unknown)" input

--- >>> parseCSV "1,2,3,\n4,5,6\n"
-- Right [["1","2","3"],["4","5","6"]]

data MyColor = MyColor {red :: Int, green :: Int, blue :: Int} deriving (Eq, Show, Read)

data FuncRec = FuncRec {name :: String, colourCalc :: Int -> (MyColor, Int)}

p5f c x = (c, x + 5)

purple = MyColor 255 0 255

plus5 = FuncRec {name = "plus5", colourCalc = p5f purple}

always0 = FuncRec {name = "always0", colourCalc = const (purple, 0)}

--- >>> FuncRec {name = "Hi"}
-- No instance for `Show FuncRec' arising from a use of `evalPrint'
-- In a stmt of an interactive GHCi command: evalPrint it_a1kJg

-- | The primary piece of data this program will store. It represents the fields in a POSIX /etc/passwd file
data PasswdEntry = PasswdEntry {userName :: String, password :: String, uid :: Integer, gid :: Integer, gecos :: String, homeDir :: String, shell :: String} deriving (Eq, Ord)

instance Show PasswdEntry where
  show pe = printf "%s:%s:%d:%d:%s:%s:%s" (userName pe) (password pe) (uid pe) (gid pe) (gecos pe) (homeDir pe) (shell pe)

--- >>> PasswdEntry {userName = "Hi"}
-- /Users/jasonqin/Documents/GitHub/HaskPlayground/src/RealWorld.hs:172:2-30: Missing field in record construction password

partialuser = PasswdEntry {userName = "Hi"}

p' = partialuser {password = "hi"}

-- dont use these patterns, use full record, Data.List.uncons, readMaybe

xs :: [Integer]
xs = [1, 2, 3]

x1 = List.uncons xs

-- infix L low |>. F |> G just means G(F)

infixl 0 |>

x |> f = f x

--- >>> xs |> listToMaybe
-- Just 1

-- | Converting data back out of a 'PasswdEntry'.
instance Read PasswdEntry where
  readsPrec _ value = case split ':' value of
    [f1, f2, f3, f4, f5, f6, f7] -> [(PasswdEntry f1 f2 (read f3) (read f4) f5 f6 f7, [])]
    x -> error $ "Invalid number of fields in input: " ++ show x
    where
      split _ [] = [[]]
      split delim str = let (before, remainder) = span (/= delim) str in before : case remainder of [] -> []; x -> split delim $ tail x

type UIDMap = Map.Map Integer PasswdEntry

type UserMap = Map.Map String PasswdEntry

notDots p = p /= "." && p /= ".."

listDirectory = liftM (filter notDots) . getDirectoryContents

countEntriesTrad path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
    let name' = path </> name
    isdir <- doesDirectoryExist name'
    if isdir then countEntriesTrad name' else return []
  return $ (path, length contents) : concat rest

res = fmap (List.map snd) $ countEntriesTrad "."

--- >>> res
-- [26,2,7,9,3,8,0,0,2,4,5,11,14,6,15,0,1,1,8,1,7,23,9,7,27,24,14,13,8,14,13,7,3,2,3,7,6,9,8,5,2,2,10,9,7,6,5,3,5,4,20,2,6,3,5,4,6,5,23,21,2,13,9,4,6,4,9,2,21,2,1,4,2,2,9,3,2,13,5,5,13,7,4,3,11,5,1,20,17,6,7,2,6,2,3,1,80,62,4,16,14,13,11,9,3,3,4,2,3,2,8,4,6,5,11,4,1,6,3,1,1,19,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,9,14,10,9,10,9,8,2,1,13,6,14,1,1,5,1,9,14,10,5,3,8,1,12,7,14,253,2,5,2,4,3,6,5,4,6,6,1,4,4,3,3,4,4,2,1,6,5,4,3,3,5,4,3,1,3,5,5,3,3,6,8,2,1,4,2,4,3,2,2,6,5,8,4,3,6,8,6,2,3,6,4,1,5,1,3,4,3,5,6,4,3,3,2,4,2,3,7,7,4,3,2,4,2,1,3,7,5,3,5,4,3,3,1,5,5,5,4,5,3,4,4,5,6,3,6,3,8,3,8,2,7,3,5,2,3,3,4,7,3,1,7,5,7,4,8,1,5,2,7,2,2,3,11,1,5,2,7,5,1,10,4,3,4,5,5,3,0,4,4,3,6,6,6,5,6,6,3,1,4,3,5,3,5,9,4,5,12,5,5,6,3,6,3,6,3,11,2,2,2,6,4,2,6,3,4,4,2,2,3,2,3,6,6,7,1,3,4,5,1,2,2,4,6,7,5,1,3,2,6,2,4,3,7,3,1,5,2,2,4,2,8,3,1,1,7,3,4,1,2,4,3,5,7,6,1,5,7,4,4,2,4,1,10,2,4,1,7,2,4,2,4,4,7,5,2,5,6,2,5,1,2,3,1,1,2,14,4,1,0,1,2,1,5,1,2,1,1,2,1,4,1,1,2,67,1,1,3,1,1,9,1,3,3,137,3,16,3,3,6,3,3,4,1,1,1,1,38]

myName step = do
  name <- ask
  return (step ++ ", I am " ++ name)

-- f = fmap _

x'' = undefined

x''' :: Int
x''' = undefined

intro :: a -> ((a -> Void) -> Void)
intro x = \k -> k x

-- absurd :: Void -> a
-- absurd x = case x of {}  -- no cases, because Void has none

-- must derive Functor to derive Monad
-- use generalized new type deriving
newtype MyApp a = MyA {run :: ReaderT AppConfig (StateT AppState IO) a} deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig, MonadState AppState)

-- cannot use maxdepth in new version of AppConfig
runMyApp :: MyApp a -> Int -> IO (a, AppState)
runMyApp k maxdepth = runStateT (runReaderT (run k) config) state
  where
    config = AppConfig maxdepth
    state = AppState 0

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries newName

-- This is equivalent to 2 / 10
r''' :: Double
r''' = flip (/) 10 2

--- >>> r'''
-- 0.2

newtype MyString = MyString String deriving (Show, Eq)

instance Num MyString where
  (MyString s1) + (MyString s2) = MyString (s1 ++ s2)
  fromInteger n = MyString (show n)

  abs x = x
  signum _ = MyString "1"
  negate (MyString s) = MyString (reverse s)
  (*) (MyString s) (MyString n) = MyString (concat $ replicate (read n) s)

--- >>> MyString "Hi" + MyString "Bye"
-- MyString "HiBye"

returnTest :: IO String
returnTest = do
  one <- return 1
  putStrLn "hi"
  let two = 2
  return $ show (one + two)

--- >>> returnTest
-- "3"

class Collects e c where
  insert :: e -> c -> c

instance Collects Char [Char] where
  insert = (:)

-- r2 :: [Char]
r2 = insert 'a' ['e']
