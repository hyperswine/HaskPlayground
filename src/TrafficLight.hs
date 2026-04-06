module TrafficLight
  ( Light (..),
    TrafficLight (..),
    initial,
    step,
    stepN,
    nextLight,
    duration,
    redDuration,
    greenDuration,
    yellowDuration,
  )
where

-- | The three possible light colours, in transition order.
data Light = Red | Green | Yellow
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | A snapshot of the traffic light: which light is active and how many
--   ticks remain before it switches.
data TrafficLight = TrafficLight
  { tlLight :: Light,
    tlTimer :: Int
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Timing constants
-- ---------------------------------------------------------------------------

redDuration :: Int
redDuration = 60

greenDuration :: Int
greenDuration = 45

yellowDuration :: Int
yellowDuration = 5

-- | How many ticks a given light stays on.
duration :: Light -> Int
duration Red = redDuration
duration Green = greenDuration
duration Yellow = yellowDuration

-- ---------------------------------------------------------------------------
-- State machine
-- ---------------------------------------------------------------------------

-- | The mandatory transition order: Red → Green → Yellow → Red → …
nextLight :: Light -> Light
nextLight Red = Green
nextLight Green = Yellow
nextLight Yellow = Red

-- | The initial state: red light, full red-phase timer.
initial :: TrafficLight
initial = TrafficLight Red redDuration

-- | Advance the traffic light by one tick.
--   When the timer reaches 1 the light switches immediately and the new
--   timer is loaded with the successor light's full duration.
step :: TrafficLight -> TrafficLight
step (TrafficLight l t)
  | t <= 1 =
      let l' = nextLight l
       in TrafficLight l' (duration l')
  | otherwise = TrafficLight l (t - 1)

-- | Advance by @n@ ticks.
stepN :: Int -> TrafficLight -> TrafficLight
stepN n tl = iterate step tl !! n
