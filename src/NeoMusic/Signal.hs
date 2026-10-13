-- | Signals: how a note varies while it sounds, built from a few constructors.
--
-- A note has two: its pitch offset (in steps; signals add) and its level (a
-- loudness multiplier; signals multiply). Time inside a signal is musical, never
-- seconds: 'Points' run over the note's sounding time @u@ from 0 to 1, and
-- 'Osc' runs at a rate in cycles per beat. 'Cents' marks a part measured in
-- cents rather than steps; the tuning converts it at render. 'Noise' and
-- 'Start' are for humanizing: a repeatable random signal, and a signal's value
-- at the note's start, held. See neomusic-signals.md and neomusic-humanize.md.
module NeoMusic.Signal
  ( Signal(..), Wave(..)
  , plus, times, negateSignal, constantValue, terms, factors
  , validPoints, hasCents, oscillates, signalBounds, valueAt, withoutOsc
  , pointsValue, restrictTo, exactAt, noiseKnot
  , Motion(..), motion
  ) where

import Data.Bits (shiftR, xor)
import Data.List (nub, sort)
import Data.Word (Word64)

data Wave = Sine | Square deriving (Eq, Show)

data Signal
  = Const Rational                   -- ^ a number is a constant signal
  | Points [(Rational, Rational)]    -- ^ breakpoints @(u, value)@ over the note, linear between
  | Osc Wave Rational                -- ^ unit wave, rate in cycles per beat, starting at phase 0
  | Signal :+: Signal
  | Signal :*: Signal
  | Cents Signal                     -- ^ this part is in cents, not steps
  | Noise Integer Rational Rational  -- ^ seed, knots per beat, and how many beats in it starts
  | Start Signal                     -- ^ the value at the note's start, held through it
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
  Noise {} -> Const (-1) :*: s
  Start a -> Start (negateSignal a)

-- | The value of a signal that doesn't vary and has no unit marker.
constantValue :: Signal -> Maybe Rational
constantValue s = case s of
  Const k -> Just k
  a :+: b -> (+) <$> constantValue a <*> constantValue b
  a :*: b -> (*) <$> constantValue a <*> constantValue b
  Start a -> exactAt 0 0 a
  _ -> Nothing

-- | The exact value at position @u@ of a note sounding @beats@ written beats,
-- where it has one: not for an oscillator, nor for a part in cents.
exactAt :: Rational -> Rational -> Signal -> Maybe Rational
exactAt beats u s = case s of
  Const k -> Just k
  Points ps -> Just (pointsValue ps u)
  Noise seed rate offset -> Just (noiseValue seed rate (offset + u * beats))
  a :+: b -> (+) <$> exactAt beats u a <*> exactAt beats u b
  a :*: b -> (*) <$> exactAt beats u a <*> exactAt beats u b
  Start a -> exactAt beats 0 a
  _ -> Nothing

-- | A knot of the noise: a value within -1..1 (multiples of 1/32768), the same
-- for the same seed and knot on every render.
noiseKnot :: Integer -> Integer -> Rational
noiseKnot seed k = fromIntegral (mixed `shiftR` 48) / 32768 - 1
  where
    w = fromIntegral seed * 0x9E3779B97F4A7C15 + fromIntegral k * 0xD1B54A32D192ED03 :: Word64
    z1 = (w `xor` (w `shiftR` 30)) * 0xBF58476D1CE4E5B9
    z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
    mixed = z2 `xor` (z2 `shiftR` 31)

-- | Noise at a position in beats: its knots, @rate@ to a beat, joined by lines.
noiseValue :: Integer -> Rational -> Rational -> Rational
noiseValue seed rate position =
  let x = position * rate
      k = floor x
      f = x - fromIntegral k
      a = noiseKnot seed k
  in a + (noiseKnot seed (k + 1) - a) * f

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
  Start a -> validPoints a
  _ -> True

hasCents :: Signal -> Bool
hasCents s = case s of
  Cents _ -> True
  a :+: b -> hasCents a || hasCents b
  a :*: b -> hasCents a || hasCents b
  Start a -> hasCents a
  _ -> False

oscillates :: Signal -> Bool
oscillates s = case s of
  Osc _ _ -> True
  a :+: b -> oscillates a || oscillates b
  a :*: b -> oscillates a || oscillates b
  Cents a -> oscillates a
  Start a -> oscillates a
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
  Noise {} -> (-1, 1)
  Start a -> signalBounds a

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
  Noise seed rate offset ->
    let x = (fromRational offset + u * beats) * fromRational rate
        k = floor x :: Integer
        f = x - fromIntegral k
        a = fromRational (noiseKnot seed k)
    in a + (fromRational (noiseKnot seed (k + 1)) - a) * f
  Start a -> valueAt stepsPerCent beats 0 a

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

-- | Exact value of breakpoints at @x@, flat beyond the first and last.
pointsValue :: [(Rational, Rational)] -> Rational -> Rational
pointsValue [] _ = 0
pointsValue ((u0, y0) : more) x
  | x <= u0 = y0
  | otherwise = go u0 y0 more
  where
    go _ y [] = y
    go ua ya ((ub, yb) : later)
      | x <= ub = if ub == ua then yb else ya + (yb - ya) * (x - ua) / (ub - ua)
      | otherwise = go ub yb later

-- | The part of a phrase-long signal (over the phrase, 0 to 1, @total@ beats
-- long) that falls between phrase positions @x0@ and @x1@, re-expressed over a
-- note's u from 0 to 1. Breakpoints restrict exactly; noise continues from
-- where the note starts in it, so 'Start' holds the value where each note
-- starts (and keeps where it came from, for the reading views).
-- An oscillator runs per note, so it can't.
restrictTo :: Rational -> Rational -> Rational -> Signal -> Either String Signal
restrictTo total x0 x1 s = case s of
  Const k -> Right (Const k)
  Points ps ->
    let start = pointsValue ps x0
        inside = [((x - x0) / (x1 - x0), y) | (x, y) <- ps, x > x0, x < x1]
        pts = (0, start) : inside ++ [(1, pointsValue ps x1)]
    in Right (if all ((== start) . snd) pts then Const start else Points pts)
  Osc _ _ -> Left "an oscillator runs per note, not across a phrase"
  a :+: b -> (:+:) <$> restrictTo total x0 x1 a <*> restrictTo total x0 x1 b
  a :*: b -> (:*:) <$> restrictTo total x0 x1 a <*> restrictTo total x0 x1 b
  Cents a -> Cents <$> restrictTo total x0 x1 a
  Noise seed rate offset -> Right (Noise seed rate (offset + x0 * total))
  Start a -> restrictTo total x0 x1 a >>= \r ->
    let reach x = case x of
          Cents c -> exactAt 0 0 c
          _ -> exactAt 0 0 x
    in maybe (Left "start needs a value it can reach exactly") (const (Right (Start r))) (reach r)

-- | The signal with its oscillators and noise silenced: the shape the reading
-- views describe (both are ornament, not a written gesture).
withoutOsc :: Signal -> Signal
withoutOsc s = case s of
  Osc _ _ -> Const 0
  Noise {} -> Const 0
  a :+: b -> withoutOsc a :+: withoutOsc b
  a :*: b -> withoutOsc a :*: withoutOsc b
  Cents a -> Cents (withoutOsc a)
  Start a -> Start (withoutOsc a)
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
