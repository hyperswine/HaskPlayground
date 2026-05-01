{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module MyClash where

import Clash.Prelude hiding (mux)
import qualified Data.List as L
import Prelude hiding (map)

infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = f x

data Polarity = High | Low

newtype Active (p :: Polarity) = MkActive {activeLevel :: Bit} deriving (Show, Eq, Ord, Generic, NFDataX, BitPack)

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

horiz True = " ###### "
horiz False = " ...... "

vert b1 b2 = part b1 <> "      " <> part b2
  where
    part True = "#"
    part False = "."

ss5 = True :> False :> True :> True :> False :> True :> True :> Nil

--- >>> showSS ss5
-- " ###### \n#        .\n#        .\n#        .\n ###### \n.        #\n.        #\n.        #\n ###### \n"

topEntity :: "SS" ::: ("AN" ::: Signal System (Vec 4 (Active High)), "SEG" ::: Signal System (Vec 7 (Active Low)), "DP" ::: Signal System (Active Low))
topEntity = (map toActive <$> anodes, map toActive <$> segments, toActive <$> dp)

anodes = pure $ False :> False :> False :> True :> Nil

segments = pure ss5

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

example :: Signal System Bit -> Signal System Bit
example = id

input1 = fromList [low, low, high]
input2 = fromList ([low, low, high] <> L.repeat low)

t1 = L.take 20 $ sample $ example input2

t2 = topEntity

-- requires type annotation for liftA3
-- x1, x2, switch
mux :: (Applicative f) => f Bool -> f a -> f a -> f a
mux = liftA3 $ \cond thn els -> if cond then thn else els

-- x1 is NOTed, x2 is as is, sw is as is
myCircuit sw x = mux sw (complement <$> x) x
