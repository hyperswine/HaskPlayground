{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module MyClash where

import Clash.Prelude
import qualified Data.List as L

showSS :: Vec 7 Bool -> String
showSS (a :> b :> c :> d :> e :> f :> g :> Nil) = unlines . L.concat $ [L.replicate 1 $ horiz a, L.replicate 3 $ vert f b, L.replicate 1 $ horiz g, L.replicate 3 $ vert e c, L.replicate 1 $ horiz d]

horiz True = " ###### "
horiz False = " ...... "

vert b1 b2 = part b1 <> "        " <> part b2
  where
    part True = "#"
    part False = "."

ss5 = True :> False :> True :> True :> False :> True :> True :> Nil

--- >>> showSS ss5
-- " ###### \n#        .\n#        .\n#        .\n ###### \n.        #\n.        #\n.        #\n ###### \n"
