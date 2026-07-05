{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- Responsiveness simulation for an MVU actor pipeline on N harts.
--
-- Architecture modeled:
--   hart 0: AUpdate   (latency class)   -- MVU update function
--   hart 1: AView     (latency class)   -- MVU view/render function
--   hart 2: ADriver   (latency class)   -- framebuffer present / scanout
--   hart 3: ANetwork, ACompute, AAutosave (throughput class, shared core)
--
-- Pipeline: input event -> AUpdate (600us) -> AView (2500us) -> ADriver (1000us) -> photon
-- Input events arrive every 8000us (125 Hz).
--
-- Scheduler mechanisms modeled:
--   * fuel: throughput actors run at most FUEL us before being requeued (cooperative budget)
--   * tick: every TICK us the scheduler re-examines each hart (preemption + wakeup polling)
--   * event wakeup (optional): a message arriving in a latency-class mailbox immediately
--     wakes/preempts its hart instead of waiting for the next tick
--   * shared resource (optional): AUpdate and ACompute both need one exclusive resource
--     (e.g. a world-state lock). ACompute holds it for a whole 6000us job.
--   * priority inheritance (optional): while a latency actor is blocked on the resource,
--     the holder is boosted to latency class (outranks other throughput actors, no fuel cap).
--
-- Four configurations:
--   A tick-poll    : wakeups only noticed at 1ms ticks, no contention
--   B event-wake   : event-driven wakeup, no contention
--   C inversion    : event-wake + resource contention, NO priority inheritance
--   D inheritance  : event-wake + resource contention, WITH priority inheritance
--
-- Metrics: input-to-photon latency (p50/p95/p99/max), frame time (p50/p99),
-- jitter |ft[n]-ft[n-1]| (mean/p99), deadline misses (>16.7ms input-to-photon).

module MailboxResponse where

import Data.List (foldl', maximumBy, nub, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Text.Printf (printf)

type Time = Int -- microseconds

dt, tickIv, simLen, fuelBudget, deadline, bigFuel :: Time
dt = 10 -- simulation quantum
tickIv = 1000 -- scheduler tick interval (1ms)
simLen = 3000000 -- 3 seconds of simulated time
fuelBudget = 2000 -- throughput fuel slice (2ms)
deadline = 16700 -- one 60Hz frame of input-to-photon budget
bigFuel = maxBound `div` 2

-- Ord: Thr < Lat, so 'maximum' prefers latency class
data Class = Thr | Lat deriving (Eq, Ord, Show)

data AId = AUpdate | AView | ADriver | ANetwork | ACompute | AAutosave
  deriving (Eq, Ord, Show, Enum, Bounded)

allActors :: [AId]
allActors = [minBound .. maxBound]

klass :: AId -> Class
klass AUpdate = Lat
klass AView = Lat
klass ADriver = Lat
klass _ = Thr

pin :: AId -> Int
pin AUpdate = 0
pin AView = 1
pin ADriver = 2
pin _ = 3

cores :: [Int]
cores = [0 .. 3]

data Job = Job
  { jWork :: !Time, -- remaining work (us)
    jRes :: !Bool, -- needs the shared resource for its whole duration
    jBirth :: !(Maybe Time) -- original input-event timestamp (pipeline jobs)
  }

data Run = Run {rActor :: !AId, rJob :: !Job, rFuel :: !Time}

data Cfg = Cfg
  { cName :: String,
    cEvent :: Bool, -- event-driven wakeup for latency-class mailboxes
    cCont :: Bool, -- AUpdate and ACompute contend on the shared resource
    cInherit :: Bool -- priority inheritance on the resource
  }

data World = World
  { wQ :: !(M.Map AId [Job]), -- per-actor FIFO mailbox/job queue
    wRun :: !(M.Map Int Run), -- core -> currently running
    wBlk :: !(M.Map AId Job), -- actors blocked on the resource (their in-flight job)
    wRes :: !(Maybe AId), -- resource holder
    wLast :: !(M.Map AId Time), -- last-scheduled time (RR fairness among throughput)
    wPres :: ![Time], -- frame present timestamps (reversed)
    wLat :: ![Time] -- input-to-photon latencies (reversed)
  }

emptyWorld :: World
emptyWorld = World M.empty M.empty M.empty Nothing M.empty [] []

--------------------------------------------------------------------------------
-- Workload arrival schedule
--------------------------------------------------------------------------------

arrivals :: Cfg -> Time -> [(AId, Job)]
arrivals cfg t =
  concat
    [ [(AUpdate, Job 600 (cCont cfg) (Just t)) | after 1370 8130],
      [(ANetwork, Job 1500 False Nothing) | after 3000 3970],
      [(ACompute, Job 8000 (cCont cfg) Nothing) | after 2000 30000],
      [(AAutosave, Job 15000 False Nothing) | after 5000 250000]
    ]
  where
    after phase period = t >= phase && (t - phase) `mod` period == 0

enq, enqFront :: AId -> Job -> World -> World
enq a j w = w {wQ = M.insertWith (\new old -> old ++ new) a [j] (wQ w)}
enqFront a j w = w {wQ = M.insertWith (++) a [j] (wQ w)}

--------------------------------------------------------------------------------
-- Effective class (priority inheritance)
--------------------------------------------------------------------------------

eff :: Cfg -> World -> AId -> Class
eff cfg w a
  | cInherit cfg,
    wRes w == Just a,
    any ((== Lat) . klass) (M.keys (wBlk w)) =
      Lat
  | otherwise = klass a

--------------------------------------------------------------------------------
-- Scheduling
--------------------------------------------------------------------------------

eligible :: World -> Int -> [AId]
eligible w c =
  [ a | a <- allActors, pin a == c, not (M.member a (wBlk w)), maybe False (not . null) (M.lookup a (wQ w))
  ]

-- Fill an idle core with the best eligible actor. Handles resource acquisition;
-- an actor that fails to acquire is moved to the blocked set and the next
-- candidate is tried. Blocking may trigger a priority-inheritance reschedule
-- of the holder's core (event mode).
schedCore :: Cfg -> Time -> Int -> World -> World
schedCore cfg t c w
  | M.member c (wRun w) = w
  | otherwise = case eligible w c of
      [] -> w
      cs -> tryStart (pickBest cs)
  where
    pickBest = maximumBy (comparing key)
    key a = (eff cfg w a, negate (fromMaybe (-1) (M.lookup a (wLast w))))

    tryStart a =
      let (j : rest) = wQ w M.! a
          wPopped = w {wQ = M.insert a rest (wQ w)}
       in case () of
            _
              | jRes j,
                Just h <- wRes w,
                h /= a ->
                  -- resource held by someone else: block and try next candidate
                  let wBlocked = wPopped {wBlk = M.insert a j (wBlk wPopped)}
                      wNext = schedCore cfg t c wBlocked
                   in if cEvent cfg && cInherit cfg && klass a == Lat
                        then rescheduleCore cfg t (pin h) wNext -- boost holder now
                        else wNext
              | otherwise ->
                  let wRes' = if jRes j then Just a else wRes wPopped
                      wAcq = wPopped {wRes = wRes'}
                      fuel = if eff cfg wAcq a == Lat then bigFuel else fuelBudget
                   in wAcq
                        { wRun = M.insert c (Run a j fuel) (wRun wAcq),
                          wLast = M.insert a t (wLast wAcq)
                        }

-- Preempt a running throughput-class actor if a latency-class actor is waiting
-- on this core, then (re)fill the core.
rescheduleCore :: Cfg -> Time -> Int -> World -> World
rescheduleCore cfg t c w = schedCore cfg t c (preempt w)
  where
    preempt ww = case M.lookup c (wRun ww) of
      Just (Run a j _)
        | eff cfg ww a == Thr,
          any (\x -> eff cfg ww x == Lat) (eligible ww c) ->
            enqFront a j ww {wRun = M.delete c (wRun ww)}
      _ -> ww

--------------------------------------------------------------------------------
-- Advancing time: run jobs, handle fuel exhaustion, completion, release
--------------------------------------------------------------------------------

advance :: Cfg -> Time -> World -> World
advance cfg t w0 = foldl' step w0 (M.toList (wRun w0))
  where
    tEnd = t + dt
    step w (c, Run a j f)
      | rem' <= 0 =
          let wFree = w {wRun = M.delete c (wRun w)}
              wRel = release cfg tEnd a j wFree
              wFx = complete cfg tEnd a j wRel
           in schedCore cfg tEnd c wFx -- completion is a scheduler entry point
      | f' <= 0 && eff cfg w a == Thr =
          -- fuel exhausted: requeue remaining work at front (keeps the resource
          -- if it holds it -- this is exactly what enables priority inversion)
          let wFree = enqFront a j {jWork = rem'} w {wRun = M.delete c (wRun w)}
           in schedCore cfg tEnd c wFree
      | otherwise =
          w {wRun = M.insert c (Run a j {jWork = rem'} f') (wRun w)}
      where
        rem' = jWork j - dt
        f' = f - dt

release :: Cfg -> Time -> AId -> Job -> World -> World
release cfg t a j w
  | jRes j,
    wRes w == Just a =
      let unblocked = M.toList (wBlk w)
          w1 = w {wRes = Nothing, wBlk = M.empty}
          w2 = foldl' (\ww (x, jx) -> enqFront x jx ww) w1 unblocked
          -- resource release is an event: wake the cores of unblocked actors
          wakeCores = nub [pin x | (x, _) <- unblocked]
       in if cEvent cfg
            then foldl' (\ww c -> rescheduleCore cfg t c ww) w2 wakeCores
            else w2
  | otherwise = w

complete :: Cfg -> Time -> AId -> Job -> World -> World
complete cfg t a j w = case (a, jBirth j) of
  (AUpdate, Just b) -> send AView (Job 2500 False (Just b))
  (AView, Just b) -> send ADriver (Job 1000 False (Just b))
  (ADriver, Just b) -> w {wPres = t : wPres w, wLat = (t - b) : wLat w}
  _ -> w
  where
    -- pipeline sends are latency-class mailbox arrivals: event-wake downstream
    send dst job =
      let w1 = enq dst job w
       in if cEvent cfg then rescheduleCore cfg t (pin dst) w1 else w1

--------------------------------------------------------------------------------
-- One simulation quantum
--------------------------------------------------------------------------------

quantum :: Cfg -> Time -> World -> World
quantum cfg t w0 =
  let arr = arrivals cfg t
      wArr = foldl' (\w (a, j) -> enq a j w) w0 arr
      isTick = t `mod` tickIv == 0
      wakeCs
        | isTick = cores -- tick polls every hart
        | cEvent cfg = nub [pin a | (a, _) <- arr, klass a == Lat] -- IPI on latency arrival
        | otherwise = []
      wSched = foldl' (\w c -> rescheduleCore cfg t c w) wArr wakeCs
   in advance cfg t wSched

runSim :: Cfg -> World
runSim cfg = go 0 emptyWorld
  where
    go !t !w
      | t >= simLen = w
      | otherwise = go (t + dt) (quantum cfg t w)

--------------------------------------------------------------------------------
-- Metrics
--------------------------------------------------------------------------------

pct :: Double -> [Time] -> Time
pct _ [] = 0
pct q xs = s !! idx
  where
    s = sort xs
    idx = min (length s - 1) (floor (q * fromIntegral (length s - 1)))

diffs :: [Time] -> [Time]
diffs xs = zipWith (-) (drop 1 xs) xs

mean :: [Time] -> Double
mean [] = 0
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

ms :: Time -> Double
ms = (/ 1000) . fromIntegral

report :: Cfg -> World -> IO ()
report cfg w = do
  let lats = reverse (wLat w)
      pres = reverse (wPres w)
      fts = diffs pres
      jit = map abs (diffs fts)
      misses = length (filter (> deadline) lats)
  printf
    "%-14s | %6.2f %6.2f %6.2f %7.2f | %6.2f %6.2f | %6.3f %6.2f | %4d /%4d\n"
    (cName cfg)
    (ms (pct 0.50 lats))
    (ms (pct 0.95 lats))
    (ms (pct 0.99 lats))
    (ms (pct 1.0 lats))
    (ms (pct 0.50 fts))
    (ms (pct 0.99 fts))
    (mean jit / 1000)
    (ms (pct 0.99 jit))
    misses
    (length lats)

  -- show the worst individual input-to-photon events (spikes)
  let worst = take 4 (reverse (sort lats))
  printf
    "%-14s   worst events: %s\n\n"
    ""
    (unwords [printf "%.2fms" (ms x) :: String | x <- worst])

configs :: [Cfg]
configs =
  [ Cfg "A tick-poll" False False False,
    Cfg "B event-wake" True False False,
    Cfg "C inversion" True True False,
    Cfg "D inheritance" True True True
  ]

main :: IO ()
main = do
  printf "MVU actor pipeline: input(125Hz) -> update(0.6ms) -> view(2.5ms) -> driver(1.0ms)\n"
  printf "harts: update|view|driver pinned to 0|1|2, network+compute+autosave share hart 3\n"
  printf "tick=1ms, fuel=2ms, deadline=16.7ms, sim=3s. All numbers in ms.\n\n"
  printf
    "%-14s | %-29s | %-13s | %-13s | %s\n"
    "config"
    "input-to-photon"
    "frame time"
    "jitter"
    "misses"
  printf
    "%-14s | %6s %6s %6s %7s | %6s %6s | %6s %6s |\n"
    ""
    "p50"
    "p95"
    "p99"
    "max"
    "p50"
    "p99"
    "mean"
    "p99"
  putStrLn (replicate 96 '-')
  mapM_ (\cfg -> report cfg (runSim cfg)) configs
