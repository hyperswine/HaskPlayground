{-# LANGUAGE DataKinds #-}
-- Standalone exhaustive decoder dump used by boards/tangnano20k/rv64/check_compressed.py.
module Main where
import RiscVCompressed
import Numeric (showHex)
import Control.Monad (forM_)
main :: IO ()
main = forM_ [False,True] $ \rv64 -> forM_ [0..65535 :: Integer] $ \c ->
  putStrLn (show (if rv64 then (64 :: Int) else 32) ++ " " ++ showHex c "" ++ " " ++ maybe "-" (\w -> showHex (toInteger w) "") (decompress rv64 (fromInteger c)))
