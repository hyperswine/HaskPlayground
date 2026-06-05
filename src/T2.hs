module T2 where

-- uhh

-- send back a message on activate
-- passive is checked in the calculations

data Msg = Activate

f x 0 z = x + z
f x y z = f x (y - 1) (z + 1)

--- >>> f 10 20 15215
-- 15245
