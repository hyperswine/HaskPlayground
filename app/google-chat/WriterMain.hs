module Main (main) where

import Control.Exception (catch, IOException)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as L
import GoogleChat (sendOnlyRequest)
import Network.HTTP.Client (HttpException)
import System.Exit (die)

main :: IO ()
main = reader `catch` httpFailure `catch` ioFailure
  where
    reader = do
      bytes <- L.getContents
      if L.length bytes > 180000 then die "Send query exceeds input limit" else pure ()
      value <- either (const (die "Invalid send query JSON")) pure (eitherDecode bytes)
      result <- sendOnlyRequest value
      L.putStrLn (encode result)
    httpFailure :: HttpException -> IO ()
    httpFailure _ = die "Google connection failed; check connectivity and retry"
    ioFailure :: IOException -> IO ()
    ioFailure _ = die "Private Google credentials are unavailable; check configuration and permissions"
