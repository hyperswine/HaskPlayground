{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module MyClash where

import Clash.Prelude
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
