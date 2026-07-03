{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Clash3X where

import Clash.Prelude
import Control.Monad (unless)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.Directory.Internal.Prelude (exitFailure)

-- A simple counter that increments if enabled
counter :: (HiddenClockResetEnable dom) => Signal dom Bool -> Signal dom (Unsigned 4)
counter enable = q
  where
    q = regEn 0 enable (q + 1)

-- Signal dom Bool = Count enable. Same idea, if enabled then do the logic of the mux. The mux just checks if value is 7, if not then increment, else return current val (should be 7)
saturatingCounter :: (HiddenClockResetEnable dom) => Signal dom Bool -> Signal dom (Unsigned 3)
saturatingCounter en = q
  where
    q = regEn 0 en (mux (q .==. 7) q (q + 1))

-- A property to check that the counter never overflows past 7
prop_never_overflows :: Property
prop_never_overflows = property $ do
  -- 1. Generate a list of boolean inputs (simulating 0 to 30 clock cycles), for regenable
  simLength <- forAll $ Gen.int (Range.linear 0 30)
  inputs <- forAll $ Gen.list (Range.singleton simLength) Gen.bool

  -- 2. Simulate the Clash circuit
  -- We sample (simLength + 1) times to see the final state
  let outputs =
        sampleN
          (simLength + 1)
          ( withClockResetEnable
              clockGen
              resetGen
              enableGen
              (saturatingCounter (fromList inputs) :: Signal System (Unsigned 3))
          )

  -- 3. Assert our hardware invariant
  assert $ all (<= 7) outputs

main = do
  counterdone <- checkParallel $ Group "MyCounter" [("Never Overflows", prop_never_overflows)]
  unless counterdone exitFailure

main' = do
  ok <- checkParallel $$(discover)
  if ok then putStrLn "All tests passed." else exitFailure
