-- | Unitless steps and the tunings that interpret them as frequencies.
module NeoMusic.Pitch (Step(..), Tuning(..), equalTemperament, freq) where

newtype Step = Step Int deriving (Eq, Ord, Show)
data Tuning = Tuning { f0 :: Double, ratio :: Double } deriving (Eq, Show)

equalTemperament :: Double -> Int -> Either String Tuning
equalTemperament base divisions
  | not (positive base) || divisions <= 0 = Left "positive base Hz and divisions required"
  | otherwise = Right (Tuning base (2 ** (1 / fromIntegral divisions)))
  where positive x = not (isNaN x || isInfinite x) && x > 0
freq :: Tuning -> Step -> Double
freq (Tuning base r) (Step x) = base * r ** fromIntegral x
