{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import Control.Monad (unless)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.Exit (exitFailure)
import qualified SimpleRisc as SimpleRiscTests

prop_example :: Property
prop_example = property $ do
  x <- forAll $ Gen.int (Range.linear 1 100)
  y <- forAll $ Gen.int (Range.linear 1 100)
  let result = x + y
  annotate $ "x: " <> show x <> ", y: " <> show y <> ", result: " <> show result
  result === x + y

main :: IO ()
main = do
  exampleResult <- checkParallel $ Group "test" [("prop_example", prop_example)]
  simpleRiscResult <- checkParallel SimpleRiscTests.simpleRiscGroup
  unless (exampleResult && simpleRiscResult) exitFailure
