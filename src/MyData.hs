module MyData where

import Data.Complex

type Sequence = [Complex Double]

dft :: Sequence -> Sequence
-- twiddle just means the complex exponential term e^(-2*pi*i*n*k/N)
dft xs = [sum [x * twiddle n k | (n, x) <- zip [0 ..] xs] | k <- [0 .. length xs - 1]]
  where
    len = length xs
    twiddle n k = exp $ 0 :+ (- (2 * pi * fromIntegral (n * k) / fromIntegral len))

exampleSequence :: Sequence
exampleSequence = [1 :+ 0, 0 :+ 0, (-1) :+ 0, 0 :+ 0]

--- >>> dft exampleSequence
-- [0.0 :+ 0.0,2.0 :+ 1.2246467991473532e-16,0.0 :+ (-2.4492935982947064e-16),2.0 :+ 3.6739403974420594e-16]
