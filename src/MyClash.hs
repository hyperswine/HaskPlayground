{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module MyClash where

import qualified Clash.Explicit.Prelude as CP
import Clash.Prelude hiding (mux)
import qualified Data.List as L
import Prelude hiding (map, not)
import qualified GHC.Num as Num

infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = f x

data Polarity = High | Low

newtype Active (p :: Polarity) = MkActive {activeLevel :: Bit} deriving (Show, Eq, Ord, Generic, NFDataX, BitPack)

active :: Bit -> Active p
active = MkActive

class IsActive p where
  fromActive :: Active p -> Bool
  toActive :: Bool -> Active p

instance IsActive High where
  fromActive = bitToBool . activeLevel
  toActive = MkActive . boolToBit

instance IsActive Low where
  fromActive = bitToBool . complement . activeLevel
  toActive = MkActive . complement . boolToBit

showSS :: Vec 7 Bool -> String
showSS (a :> b :> c :> d :> e :> f :> g :> Nil) = unlines . L.concat $ [L.replicate 1 $ horiz a, L.replicate 3 $ vert f b, L.replicate 1 $ horiz g, L.replicate 3 $ vert e c, L.replicate 1 $ horiz d]
showSS _ = error "showSS: impossible"

horiz :: Bool -> String
horiz True = " ###### "
horiz False = " ...... "

vert :: Bool -> Bool -> String
vert b1 b2 = part b1 <> "      " <> part b2
  where
    part True = "#"
    part False = "."

ss5 :: Vec 7 Bool
ss5 = True :> False :> True :> True :> False :> True :> True :> Nil

--- >>> showSS ss5
-- " ###### \n#        .\n#        .\n#        .\n ###### \n.        #\n.        #\n.        #\n ###### \n"

topEntity :: "SS" ::: ("AN" ::: Signal System (Vec 4 (Active High)), "SEG" ::: Signal System (Vec 7 (Active Low)), "DP" ::: Signal System (Active Low))
topEntity = (map toActive <$> anodes, map toActive <$> segments, toActive <$> dp)

anodes :: Signal System (Vec 4 Bool)
anodes = pure $ False :> False :> False :> True :> Nil

segments :: Signal System (Vec 7 Bool)
segments = pure ss5

dp :: Signal System Bool
dp = pure False

-- need only 16 which is 2^4, so 4 bit input. Output is 7 bits for the decoding to the right wire
encodeHexSS :: Unsigned 4 -> Vec 7 Bool
encodeHexSS n = unpack $ case n of
  -- abcdefg
  0x0 -> 0b1111110
  0x1 -> 0b0110000
  0x2 -> 0b1101101
  0x3 -> 0b1111001
  0x4 -> 0b0110011
  0x5 -> 0b1011011
  0x6 -> 0b1011111
  0x7 -> 0b1110000
  0x8 -> 0b1111111
  0x9 -> 0b1111011
  0xa -> 0b1110111
  0xb -> 0b0011111
  0xc -> 0b1001110
  0xd -> 0b0111101
  0xe -> 0b1001111
  0xf -> 0b1000111
  _ -> errorX "encodeHexSS: impossible"

example :: Signal System Bit -> Signal System Bit
example = id

input1 :: Signal System Bit
input1 = fromList [low, low, high]

input2 :: Signal System Bit
input2 = fromList ([low, low, high] <> L.repeat low)

t1 :: [Bit]
t1 = L.take 20 $ sample $ example input2

t2 :: "SS" ::: ("AN" ::: Signal System (Vec 4 (Active High)), "SEG" ::: Signal System (Vec 7 (Active Low)), "DP" ::: Signal System (Active Low))
t2 = topEntity

-- requires type annotation for liftA3
-- x1, x2, switch
mux :: (Applicative f) => f Bool -> f a -> f a -> f a
mux = liftA3 $ \cond thn els -> if cond then thn else els

-- x1 is NOTed, x2 is as is, sw is as is
myCircuit :: (Applicative f, Bits a) => f Bool -> f a -> f a
myCircuit sw x = mux sw (complement <$> x) x

helloreg :: Clock System -> Reset System -> Enable System -> Signal System Bool
-- expect True False False False ...
helloreg clk rst en = CP.register clk rst en True $ pure False

res :: [Bool]
res = sampleN 5 $ helloreg clockGen resetGen enableGen

--- >>> res
-- [True,True,False,False,False]

flippy :: Clock System -> Reset System -> Enable System -> Signal System Bool
flippy clk rst en = r where r = CP.register clk rst en True (not <$> r)

res' :: [(Integer, Bool)]
res' = L.zip [0 ..] $ sampleN 8 $ flippy clockGen resetGen enableGen

--- >>> res'
-- [(0,True),(1,True),(2,False),(3,True),(4,False),(5,True),(6,False),(7,True)]

type SecondPeriods dom = 1_000_000_000_000 `Div` DomainPeriod dom

blinkingSecond :: forall dom. (KnownDomain dom, KnownNat (CLog 2 (SecondPeriods dom))) => Clock dom -> Reset dom -> Enable dom -> Signal dom Bit
blinkingSecond clk rst en = msb <$> r
  where
    r :: Signal dom (Unsigned (CLog 2 (SecondPeriods dom)))
    r = CP.register clk rst en 0 (r + 1)

type HzToPeriod (freq :: Nat) = 1_000_000_000_000 `Div` freq

type ClockDivider dom ps = ps `Div` DomainPeriod dom

blinkingSecond' :: forall dom. (KnownDomain dom, KnownNat (ClockDivider dom (HzToPeriod 1)), KnownNat (CLog 2 (ClockDivider dom (HzToPeriod 1)))) => Clock dom -> Reset dom -> Enable dom -> Signal dom Bit
blinkingSecond' clk rst en = msb <$> r
  where
    r :: Signal dom (Unsigned (CLog 2 (ClockDivider dom (HzToPeriod 1))))
    r = CP.register clk rst en 0 $ mux (r .<. limit) (r + 1) 0
    limit = snatToNum $ SNat @(ClockDivider dom (HzToPeriod 1))

res2 = [minBound .. maxBound] :: [Index 14]

-- blink :: forall dom. (KnownDomain dom, KnownNat (ClockDivider dom (HzToPeriod 2))) => Clock dom -> Reset dom -> Enable dom -> Signal dom Bit
-- blink clk rst en = oscillate False (SNat @(ClockDivider dom (HzToPeriod 2)))

-- safe successor function, pareto optimal
succIdx :: (Eq a, Enum a, Bounded a) => a -> Maybe a
succIdx x
  | x == maxBound = Nothing
  | otherwise = Just $ succ x

predIdx :: (Eq a, Enum a, Bounded a) => a -> Maybe a
predIdx x
  | x == minBound = Nothing
  | otherwise = Just $ pred x

-- NFData is haskell base thing for normal form, X for exception at simulation - runtime. Needs to normalize
data OnOff on off = On (Index on) | Off (Index off) deriving (Generic, NFDataX)

-- ignore the inner var
isOn On {} = True
isOn Off {} = False

-- type Seconds (s :: Nat) = Milliseconds (1_000 * s)

-- type Milliseconds (ms :: Nat) = Microseconds (1_000 (*) ms)

-- type Microseconds (us :: Nat) = Nanoseconds (1_000 (*) us)

-- type Nanoseconds (ns :: Nat) = Picoseconds (1_000 * ns)

-- type Picoseconds (ps :: Nat) = ps
