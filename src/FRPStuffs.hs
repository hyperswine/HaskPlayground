{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module FRPStuffs where

import Prelude hiding (pure)

type Behavior a = Time -> a

type Event a = Time -> Maybe a

type Time = Double

-- from t0 to t1, sum the values of the behavior at each time step (defined as 0.01), multiplied by the time step size
definiteIntegral :: Behavior Double -> Time -> Time -> Double
definiteIntegral b t0 t1 = sum [b t * dt | t <- [t0, t0 + dt .. t1]]
  where
    dt = 0.01

-- constant
pure :: a -> Behavior a
pure x t = x

-- pointwise application (this IS your Applicative)
ap :: Behavior (a -> b) -> Behavior a -> Behavior b
ap f x t = (f t) (x t)

-- integration, defined literally as a mathematical integral
integrate :: Behavior Double -> Behavior Double
integrate b t = definiteIntegral b 0 t -- actual numerical integration, recomputed every call

-- sampling an event from a behavior
snapshot :: Behavior a -> Event b -> Event (a, b)
snapshot beh ev = \t -> case ev t of
  Nothing -> Nothing
  Just y -> Just (beh t, y)

demo =
  -- apply a behavior to a behavior, (+1) to 1
  let b = pure (+ 1) `ap` pure 1 -- setup so it is initially 2
      snap = snapshot b ev -- snapshot so the behavior is always 2
      ev t = if t >= 1 then Just t else Nothing -- event that fires at t=1
   in [snap t | t <- [0, 0.5 .. 2]] -- sample the snapshot event from t=0 to t=2, should be originally Nothing, Nothing for 0 and 0.5, then Just (2,1) for 1, then Just (2,1.5) for 1.5, then Just (2,2) for 2
  -- so by the time it reaches t = 1, the value converges to 2 and stays 2 for t = 1.5 and t = 2

-- second demo showing integration
demo' =
  let b = pure 1 -- constant behavior of 1
      integrated = integrate b -- integrate the behavior, should be t
   in [integrated t | t <- [0, 0.5 .. 2]] -- sample the integrated behavior from t=0 to t=2, should be 0, 0.5, 1, 1.5, 2

-- demo showing integration of t from 0 to 4, with events at 0.5, 2.5, 3.5 each adding 1
demo'' =
  let b = pure 1 -- constant behavior of 1
      integrated = integrate b -- integrate the behavior, should be t
      ev t = if t == 0.5 || t == 2.5 || t == 3.5 then Just t else Nothing -- event that fires at t=0.5, t=2.5, and t=3.5
      snap = snapshot integrated ev -- snapshot so the behavior is always the integrated value at the time of the event
   in [snap t | t <- [0, 0.5 .. 4]] -- sample the snapshot event from t=0 to t=4, should be originally Nothing for 0, then Just (0.5,0.5) for 0.5, then Nothing for 1, then Nothing for 1.5, then Nothing for 2, then Just (2.5,2.5) for 2.5, then Nothing for 3, then Just (3.5,3.5) for 3.5, then Nothing for 4
