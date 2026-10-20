module Main (main) where

import Control.Exception (catch, IOException)
import GoogleChat (run)
import Network.HTTP.Client (HttpException)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = (getArgs >>= run) `catch` httpFailure `catch` ioFailure
  where
    httpFailure :: HttpException -> IO ()
    httpFailure _ = die "HTTP connection failed. Check connectivity and try again. Request details were suppressed to protect credentials."
    ioFailure :: IOException -> IO ()
    ioFailure _ = die "Local I/O failed. Check private file permissions, Safari availability, and whether port 8765 is free."
