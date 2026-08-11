{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
module FRPStuffs where

import Prelude hiding (pure)

type Behavior a = Time -> a

type Event a = Time -> Maybe a

type Time = Double

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
  let b = pure (+ 1) `ap` pure 1 -- an event that occurs after time 1
      snap = snapshot b ev -- a behavior that is always 2
      ev t = if t > 1 then Just t else Nothing -- snapshot the behavior at the event
   in [snap t | t <- [0, 0.5 .. 2]] -- sample the snapshot at different times
