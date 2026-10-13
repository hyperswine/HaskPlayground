-- | Signals: how a note varies while it sounds, built from a few constructors.
--
-- A note has two: its pitch offset (in steps; signals add) and its level (a
-- loudness multiplier; signals multiply). Time inside a signal is musical, never
-- seconds: 'Points' run over the note's sounding time @u@ from 0 to 1, and
-- 'Osc' runs at a rate in cycles per beat. 'Cents' marks a part measured in
-- cents rather than steps; the tuning converts it at render. See
-- neomusic-signals.md.
module NeoMusic.Signal
  ( Signal(..), Wave(..)
  , plus, times, negateSignal, constantValue, terms, factors
  , validPoints, hasCents, oscillates, signalBounds, valueAt, withoutOsc
  , Motion(..), motion
  ) where

import Data.List (nub, sort)

data Wave = Sine | Square deriving (Eq, Show)

data Signal
  = Const Rational                   -- ^ a number is a constant signal
  | Points [(Rational, Rational)]    -- ^ breakpoints @(u, value)@ over the note, linear between
  | Osc Wave Rational                -- ^ unit wave, rate in cycles per beat, starting at phase 0
  | Signal :+: Signal
  | Signal :*: Signal
  | Cents Signal                     -- ^ this part is in cents, not steps
  deriving (Eq, Show)
infixl 6 :+:
infixl 7 :*:

-- | Addition that folds constants and drops zeros, so plain notes stay 'Const'.
plus :: Signal -> Signal -> Signal
plus (Const 0) b = b
plus a (Const 0) = a
plus (Const a) (Const b) = Const (a + b)
plus a b = a :+: b

-- | Multiplication that folds constants and drops ones.
times :: Signal -> Signal -> Signal
times (Const 1) b = b
times a (Const 1) = a
times (Const a) (Const b) = Const (a * b)
times a b = a :*: b

negateSignal :: Signal -> Signal
negateSignal s = case s of
  Const k -> Const (negate k)
  Points ps -> Points [(u, negate y) | (u, y) <- ps]
  Osc _ _ -> Const (-1) :*: s
  a :+: b -> negateSignal a :+: negateSignal b
  a :*: b -> negateSignal a :*: b
  Cents a -> Cents (negateSignal a)

-- | The value of a signal that doesn't vary and has no unit marker.
constantValue :: Signal -> Maybe Rational
constantValue s = case s of
  Const k -> Just k
  a :+: b -> (+) <$> constantValue a <*> constantValue b
  a :*: b -> (*) <$> constantValue a <*> constantValue b
  _ -> Nothing

-- | The top-level summands and factors.
terms, factors :: Signal -> [Signal]
terms (a :+: b) = terms a ++ terms b
terms s = [s]
factors (a :*: b) = factors a ++ factors b
factors s = [s]

-- | Every 'Points' has u within 0..1 in nondecreasing order.
validPoints :: Signal -> Bool
validPoints s = case s of
  Points ps -> all (\(u, _) -> u >= 0 && u <= 1) ps && and (zipWith (<=) (map fst ps) (drop 1 (map fst ps)))
  a :+: b -> validPoints a && validPoints b
  a :*: b -> validPoints a && validPoints b
  Cents a -> validPoints a
  _ -> True

hasCents :: Signal -> Bool
hasCents s = case s of
  Cents _ -> True
  a :+: b -> hasCents a || hasCents b
  a :*: b -> hasCents a || hasCents b
  _ -> False

oscillates :: Signal -> Bool
oscillates s = case s of
  Osc _ _ -> True
  a :+: b -> oscillates a || oscillates b
  a :*: b -> oscillates a || oscillates b
  Cents a -> oscillates a
  _ -> False

-- | Lower and upper bounds over the whole note, by interval arithmetic
-- ('Cents' parts in their own unit).
signalBounds :: Signal -> (Rational, Rational)
signalBounds s = case s of
  Const k -> (k, k)
  Points [] -> (0, 0)
  Points ps -> (minimum (map snd ps), maximum (map snd ps))
  Osc _ _ -> (-1, 1)
  a :+: b -> let (l1, h1) = signalBounds a; (l2, h2) = signalBounds b in (l1 + l2, h1 + h2)
  a :*: b -> let (l1, h1) = signalBounds a; (l2, h2) = signalBounds b
                 ps = [l1 * l2, l1 * h2, h1 * l2, h1 * h2]
             in (minimum ps, maximum ps)
  Cents a -> signalBounds a

-- | The value at position @u@ (0..1) of a note whose sound lasts @beats@
-- written beats, with a cent worth @stepsPerCent@ steps.
valueAt :: Double -> Double -> Double -> Signal -> Double
valueAt stepsPerCent beats u s = case s of
  Const k -> fromRational k
  Points ps -> pointsAt [(fromRational a, fromRational b) | (a, b) <- ps] u
  Osc w r -> let phase = fromRational r * beats * u
             in case w of
                  Sine -> sin (2 * pi * phase)
                  Square -> if phase - fromIntegral (floor phase :: Int) < 0.5 then 1 else -1
  a :+: b -> valueAt stepsPerCent beats u a + valueAt stepsPerCent beats u b
  a :*: b -> valueAt stepsPerCent beats u a * valueAt stepsPerCent beats u b
  Cents a -> stepsPerCent * valueAt stepsPerCent beats u a

pointsAt :: [(Double, Double)] -> Double -> Double
pointsAt [] _ = 0
pointsAt ((u0, y0) : more) u
  | u <= u0 = y0
  | otherwise = go u0 y0 more
  where
    go _ y [] = y
    go ua ya ((ub, yb) : later)
      | u <= ub = if ub == ua then yb else ya + (yb - ya) * (u - ua) / (ub - ua)
      | otherwise = go ub yb later

-- | The signal with its oscillators silenced: the shape the reading views describe.
withoutOsc :: Signal -> Signal
withoutOsc s = case s of
  Osc _ _ -> Const 0
  a :+: b -> withoutOsc a :+: withoutOsc b
  a :*: b -> withoutOsc a :*: withoutOsc b
  Cents a -> Cents (withoutOsc a)
  _ -> s

-- | How a signal's shape moves over a note, for the reading views.
data Motion = Flat | Rising | Falling | Returning deriving (Eq, Show)

-- | Compares the start, the end and every breakpoint, with cents counted as
-- hundredths of a step (as in 12-EDO).
motion :: Signal -> Motion
motion s
  | end > begin = Rising
  | end < begin = Falling
  | any ((/= begin) . at) us = Returning
  | otherwise = Flat
  where
    shape = withoutOsc s
    at u = valueAt (1 / 100) 1 u shape
    begin = at 0
    end = at 1
    us = nub (sort (concat (breakpoints shape)))
    breakpoints sig = case sig of
      Points ps -> [map (fromRational . fst) ps]
      a :+: b -> breakpoints a ++ breakpoints b
      a :*: b -> breakpoints a ++ breakpoints b
      Cents a -> breakpoints a
      _ -> []
