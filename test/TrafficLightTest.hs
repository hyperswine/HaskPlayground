{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TrafficLightTest where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import TrafficLight

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

genLight :: Gen Light
genLight = Gen.element [Red, Green, Yellow]

-- Generate a valid timer value for a given light (1 … duration l).
genTimer :: Light -> Gen Int
genTimer l = Gen.int (Range.linear 1 (duration l))

genTrafficLight :: Gen TrafficLight
genTrafficLight = do
  l <- genLight
  t <- genTimer l
  pure (TrafficLight l t)

-- ---------------------------------------------------------------------------
-- Timer properties
-- ---------------------------------------------------------------------------

-- The timer strictly decrements each tick (while still above 1).
prop_timer_decrements :: Property
prop_timer_decrements = property $ do
  l <- forAll genLight
  t <- forAll $ Gen.int (Range.linear 2 (duration l))
  let tl = TrafficLight l t
  tlTimer (step tl) === t - 1

-- Timer resets to the successor light's full duration on transition.
prop_timer_resetsOnTransition :: Property
prop_timer_resetsOnTransition = property $ do
  l <- forAll genLight
  let tl = TrafficLight l 1
  let tl' = step tl
  tlTimer tl' === duration (nextLight l)

-- Timer is always strictly positive after any number of steps.
prop_timer_alwaysPositive :: Property
prop_timer_alwaysPositive = property $ do
  tl0 <- forAll genTrafficLight
  n <- forAll $ Gen.int (Range.linear 0 500)
  let tl = stepN n tl0
  assert (tlTimer tl >= 1)

-- Timer never exceeds the maximum duration of its current light.
prop_timer_withinDuration :: Property
prop_timer_withinDuration = property $ do
  tl0 <- forAll genTrafficLight
  n <- forAll $ Gen.int (Range.linear 0 500)
  let tl = stepN n tl0
  assert (tlTimer tl <= duration (tlLight tl))

-- ---------------------------------------------------------------------------
-- Transition sequence properties
-- ---------------------------------------------------------------------------

-- nextLight cycles through exactly Red → Green → Yellow → Red.
prop_transition_order :: Property
prop_transition_order = property $ do
  nextLight Red === Green
  nextLight Green === Yellow
  nextLight Yellow === Red

-- nextLight is a 3-cycle: three applications return the original light.
prop_transition_threeCycle :: Property
prop_transition_threeCycle = property $ do
  l <- forAll genLight
  (nextLight . nextLight . nextLight) l === l

-- When timer == 1 the light always switches to nextLight.
prop_step_switchesAtTimerOne :: Property
prop_step_switchesAtTimerOne = property $ do
  l <- forAll genLight
  let tl = TrafficLight l 1
  tlLight (step tl) === nextLight l

-- When timer > 1 the light stays the same.
prop_step_lightUnchangedAboveOne :: Property
prop_step_lightUnchangedAboveOne = property $ do
  l <- forAll genLight
  t <- forAll $ Gen.int (Range.linear 2 (duration l))
  let tl = TrafficLight l t
  tlLight (step tl) === l

-- ---------------------------------------------------------------------------
-- Full-cycle properties
-- ---------------------------------------------------------------------------

-- After exactly `duration Red` steps from initial, the light is Green.
prop_fullRedPhase :: Property
prop_fullRedPhase = property $
  tlLight (stepN redDuration initial) === Green

-- After red + green steps from initial, the light is Yellow.
prop_fullGreenPhase :: Property
prop_fullGreenPhase = property $
  tlLight (stepN (redDuration + greenDuration) initial) === Yellow

-- After a complete cycle (R+G+Y) from initial, the light is Red again.
prop_fullCycle_returnsToRed :: Property
prop_fullCycle_returnsToRed = property $
  tlLight (stepN (redDuration + greenDuration + yellowDuration) initial) === Red

-- After a complete cycle from initial the full state matches initial.
prop_fullCycle_stateRestored :: Property
prop_fullCycle_stateRestored = property $
  stepN (redDuration + greenDuration + yellowDuration) initial === initial

-- All three lights are seen when running for one full cycle.
prop_allLightsSeen :: Property
prop_allLightsSeen = property $ do
  let totalCycle = redDuration + greenDuration + yellowDuration
  let lights = map (tlLight . (`stepN` initial)) [0 .. totalCycle - 1]
  assert (Red `elem` lights)
  assert (Green `elem` lights)
  assert (Yellow `elem` lights)

-- ---------------------------------------------------------------------------
-- stepN properties
-- ---------------------------------------------------------------------------

-- stepN 0 is identity.
prop_stepN_zero :: Property
prop_stepN_zero = property $ do
  tl <- forAll genTrafficLight
  stepN 0 tl === tl

-- stepN is additive: stepN (m+n) = stepN n . stepN m.
prop_stepN_additive :: Property
prop_stepN_additive = property $ do
  tl <- forAll genTrafficLight
  m <- forAll $ Gen.int (Range.linear 0 200)
  n <- forAll $ Gen.int (Range.linear 0 200)
  stepN (m + n) tl === stepN n (stepN m tl)

-- ---------------------------------------------------------------------------
-- Test group
-- ---------------------------------------------------------------------------

trafficLightGroup :: Group
trafficLightGroup =
  Group
    "TrafficLight"
    [ ("timer: decrements each step", prop_timer_decrements),
      ("timer: resets on transition", prop_timer_resetsOnTransition),
      ("timer: always positive", prop_timer_alwaysPositive),
      ("timer: within duration of current light", prop_timer_withinDuration),
      ("transition: order Red→Green→Yellow→Red", prop_transition_order),
      ("transition: 3-cycle identity", prop_transition_threeCycle),
      ("step: switches light at timer=1", prop_step_switchesAtTimerOne),
      ("step: light unchanged when timer>1", prop_step_lightUnchangedAboveOne),
      ("full-cycle: red phase ends at Green", prop_fullRedPhase),
      ("full-cycle: green phase ends at Yellow", prop_fullGreenPhase),
      ("full-cycle: returns to Red", prop_fullCycle_returnsToRed),
      ("full-cycle: state fully restored", prop_fullCycle_stateRestored),
      ("full-cycle: all lights seen", prop_allLightsSeen),
      ("stepN 0: identity", prop_stepN_zero),
      ("stepN: additive", prop_stepN_additive)
    ]

