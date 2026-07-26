{-# LANGUAGE RecordWildCards #-}

-- ---------------------------------------------------------------------------
-- ZoneSim: first-order latency / throughput model of a fully memory-zone
--          partitioned FP-RISC board (no central hub).
--
-- Topology modelled
-- -----------------
--   MPU = ring of NZONES zones. Zone i has ring links to (i-1) and (i+1).
--
--   Inside a zone (local hub-and-spoke around the memory controller):
--       2x ECP5 harts  ---\
--       2x HyperRAM 64MB --+-- memory controller --- ring router -- neighbours
--       1x SD card     ---/                      \
--       0..1 accelerator (gfx / compressor / radio, pinned to that zone)
--
-- Every number below is an ESTIMATE with its reasoning in the comment.
-- The point of the file is the *shape* of the answers and the bounds; replace
-- the constants with Clash-measured values and rerun.
--
-- Build: ghc -O2 ZoneSim.hs && ./ZoneSim      (base + containers only)
-- ---------------------------------------------------------------------------

module ZoneSim (main) where

import Data.List (foldl', sort)
import qualified Data.Map.Strict as M
import Text.Printf (printf)

-- ===========================================================================
-- 1. MACHINE PARAMETERS
-- ===========================================================================

data Params = Params
  { nZones :: Int, -- zones in the MPU ring
    hartsPerZone :: Int,
    -- Clocks -----------------------------------------------------------------
    fabricMHz :: Double, -- ECP5 fabric / hart clock. 100MHz is what the
    -- Clash pipeline was pushed to; keep them equal.

    -- HyperRAM ---------------------------------------------------------------
    -- HyperBus x8 DDR at 100MHz = 200 MB/s peak per die. Two dice per zone on
    -- independent chip-selects; a single buffer lives on one die, so single-
    -- stream BW is 200, zone aggregate (2 streams) is 400.
    hyperBWmbs :: Double, -- per-die streaming bandwidth
    hyperDies :: Int,
    hyperFirstNs :: Double, -- CA phase (3 clk) + latency count (6 clk) @100MHz
    hyperWorstNs :: Double, -- doubled latency on refresh collision

    -- Zone-internal crossbar -------------------------------------------------
    xbarNs :: Double, -- hart <-> memory controller, ~3 fabric cycles

    -- Ring link (ECP5 LVDS, not SERDES) ---------------------------------------
    -- 8 differential pairs @ 500 Mbps DDR = 4.0 Gbps = 500 MB/s raw.
    -- 8b/10b framing + protocol headers -> ~400 MB/s effective payload.
    linkRawMBs :: Double,
    linkEffMBs :: Double,
    hopNs :: Double, -- TX FIFO+gearbox, wire, CDR/align, degear,
    -- RX CDC, router arbitration. ~14 cycles @100MHz.
    routerPorts :: Int, -- inputs contending for one output: cw, ccw, local

    -- SD card (one per zone, plain SD not Express) ----------------------------
    sdBWmbs :: Double, -- 4-bit bus @ 50MHz high-speed
    sdFirstUs :: Double, -- command -> first data, random read

    -- Software ---------------------------------------------------------------
    quantumUs :: Double, -- fuel quantum ~10k instr @100MHz, ~1 IPC
    mtuBytes :: Double -- max ring packet payload -- THE latency knob
  }
  deriving (Show)

defaultParams :: Params
defaultParams =
  Params
    { nZones = 4,
      hartsPerZone = 2,
      fabricMHz = 100,
      hyperBWmbs = 200,
      hyperDies = 2,
      hyperFirstNs = 100,
      hyperWorstNs = 190,
      xbarNs = 30,
      linkRawMBs = 500,
      linkEffMBs = 400,
      hopNs = 140,
      routerPorts = 3,
      sdBWmbs = 25,
      sdFirstUs = 250,
      quantumUs = 100,
      mtuBytes = 256
    }

-- Shortest-path hop count on a bidirectional ring.
ringHops :: Int -> Int -> Int -> Int
ringHops n a b = let d = abs (a - b) in min d (n - d)

maxHops :: Params -> Int
maxHops Params {..} = nZones `div` 2

-- ===========================================================================
-- 2. UNCONTENDED LATENCY MODEL
-- ===========================================================================

-- Serialisation time of `bytes` at `mbs` MB/s, in ns.
serNs :: Double -> Double -> Double
serNs bytes mbs = bytes / mbs * 1000

-- One memory read of `bytes` from a zone `hops` away, no contention.
-- Request packet out, DRAM access, payload back.
readNs :: Params -> Int -> Double -> Double
readNs p@Params {..} hops bytes =
  2 * xbarNs
    + fromIntegral hops * 2 * hopNs
    + hyperFirstNs
    + serNs bytes payloadBW
  where
    -- Remote streams are capped by the link, not the DRAM die.
    payloadBW
      | hops == 0 = hyperBWmbs
      | otherwise = min hyperBWmbs linkEffMBs

-- Actor message send: enqueue into the receiver's SPSC ring + doorbell.
-- Local = a few cycles of crossbar. Remote = one-way ring traversal.
-- Scheduler pickup latency is deliberately NOT included: that is a software
-- quantum question, not an interconnect question.
msgNs :: Params -> Int -> Double -> Double
msgNs Params {..} hops bytes =
  2 * xbarNs + fromIntegral hops * hopNs + serNs bytes linkEffMBs

-- ===========================================================================
-- 3. WORST-CASE BOUND (deterministic, round-robin, non-preemptive)
-- ===========================================================================
--
-- At each router output port, `routerPorts` inputs arbitrate round-robin at
-- packet granularity. A packet can therefore be beaten by at most
-- (routerPorts - 1) other packets, each at most one MTU long, before it wins.
--
--     perHop  <=  hopNs + (routerPorts - 1) * serNs(MTU)
--     total   <=  hops * perHop  (each direction)  + DRAM + crossbars
--
-- This bound is only valid if injection is rate-limited (credits / token
-- bucket) so a single zone cannot monopolise a link. Without that, there is
-- no bound at all -- an unthrottled zone starves the ring indefinitely.

perHopWorstNs :: Params -> Double
perHopWorstNs p@Params {..} =
  hopNs + fromIntegral (routerPorts - 1) * serNs mtuBytes linkEffMBs

readBoundNs :: Params -> Int -> Double -> Double
readBoundNs p@Params {..} hops bytes =
  2 * xbarNs
    + fromIntegral hops * 2 * perHopWorstNs p
    + hyperWorstNs
    + serNs bytes (min hyperBWmbs linkEffMBs)

-- ===========================================================================
-- 4. RING CONTENTION SIMULATION
-- ===========================================================================
--
-- Store-and-forward, FIFO per directed link, packets chopped to MTU.
-- Each directed link is a resource with a "next free" timestamp; a packet
-- occupies it for its serialisation time. Packets are released in generation
-- order, which is an approximation of true event ordering but captures the
-- queueing behaviour we care about (how far actual latency sits below the
-- analytic bound, and where the knee is).

data Pkt = Pkt {pSrc :: Int, pDst :: Int, pGen :: Double, pBytes :: Double}

data TrafficPattern = Uniform | Hotspot Int | NeighbourOnly deriving (Eq)

instance Show TrafficPattern where
  show Uniform = "uniform"
  show (Hotspot z) = "hotspot->z" ++ show z
  show NeighbourOnly = "neighbour"

-- Cheap deterministic LCG so the file stays base-only and reproducible.
lcg :: Int -> Int
lcg s = (1103515245 * s + 12345) `mod` 2147483648

randoms' :: Int -> [Int]
randoms' = tail . iterate lcg

-- Generate an offered-load stream: `n` packets, Poisson-ish arrivals
-- approximated by uniform inter-arrival jitter around the mean.
genTraffic :: Params -> TrafficPattern -> Double -> Int -> [Pkt]
genTraffic p@Params {..} pat offeredMBs n = go 0 0 (randoms' 42)
  where
    -- offeredMBs is PER ZONE, so the global inter-arrival gap is that
    -- divided by the number of injecting zones.
    meanGapNs = serNs mtuBytes offeredMBs / fromIntegral nZones
    -- NB: take high bits. This LCG's low bits have a period of 4, which
    -- silently collapses every traffic pattern into the same stream.
    bits r = (r `div` 65536) `mod` nZones
    go i t (r1 : r2 : rs)
      | i >= n = []
      | otherwise =
          let src = bits r1
              dst = case pat of
                Uniform -> let d = bits r2 in if d == src then (d + 1) `mod` nZones else d
                Hotspot z -> z
                NeighbourOnly -> (src + 1) `mod` nZones
              jit = meanGapNs * (0.5 + fromIntegral ((r2 `div` 256) `mod` 1000) / 1000)
              t' = t + jit
           in if src == dst then go i t' rs else Pkt src dst t mtuBytes : go (i + 1) t' rs
    go _ _ _ = []

-- Route a packet clockwise or counter-clockwise, whichever is shorter.
routeOf :: Int -> Int -> Int -> [(Int, Int)]
routeOf n a b =
  let cw = (b - a) `mod` n
      ccw = (a - b) `mod` n
      path step k = take k (iterate (\z -> (z + step) `mod` n) a)
   in if cw <= ccw
        then [(z, (z + 1) `mod` n) | z <- path 1 cw]
        else [(z, (z - 1 + n) `mod` n) | z <- path (-1) ccw]

simulate :: Params -> [Pkt] -> [Double]
simulate p@Params {..} pkts = reverse . snd $ foldl' step (M.empty, []) pkts
  where
    step (links, lats) Pkt {..} =
      let hopsL = routeOf nZones pSrc pDst
          ser = serNs pBytes linkEffMBs
          (links', arrive) = foldl' hop (links, pGen) hopsL
          hop (lk, t) e =
            let free = M.findWithDefault 0 e lk
                start = max t free
                done = start + ser
             in (M.insert e done lk, done + hopNs)
       in (links', (arrive - pGen) : lats)

stats :: [Double] -> (Double, Double, Double, Double)
stats xs =
  let s = sort xs
      n = length s
      idx q = s !! min (n - 1) (floor (q * fromIntegral n))
   in (sum s / fromIntegral n, idx 0.5, idx 0.99, last s)

-- An open-loop model has unbounded queues, so past saturation the latency
-- number is meaningless -- it just reports how long the run was. What IS
-- meaningful is whether the queue is growing: compare the first decile of
-- packets to the last. Stable => flat. Unstable => the link is oversubscribed
-- and only backpressure/credits stop it.
drift :: [Double] -> Double
drift xs =
  let n = length xs
      k = max 1 (n `div` 10)
      hd = take k xs
      tl = drop (n - k) xs
      mean ys = sum ys / fromIntegral (length ys)
   in mean tl / max 1 (mean hd)

-- ===========================================================================
-- 5. APPLICATION-LEVEL MODELS
-- ===========================================================================

-- Files.qa: load a file of `mb` megabytes, either from the local zone's SD
-- card, or striped across all zones' cards in parallel.
--
--   local     : one card, all bytes, no ring traffic
--   striped1  : all cards in parallel, (n-1)/n of the bytes cross the ring
--               to a single consumer zone
--   stripedN  : all cards in parallel, consumers are co-located with their
--               chunks (his "synchronous actor block group" case) -- no ring
--               traffic at all
loadLocalUs, loadStriped1Us, loadStripedNUs :: Params -> Double -> Double
loadLocalUs Params {..} mb = sdFirstUs + mb / sdBWmbs * 1e6
loadStriped1Us p@Params {..} mb =
  let perCard = mb / fromIntegral nZones
      sdTime = sdFirstUs + perCard / sdBWmbs * 1e6
      remote = mb * (fromIntegral (nZones - 1) / fromIntegral nZones)
      ringT = remote / linkEffMBs * 1e6
   in sdTime + ringT -- pessimistic: not overlapped with the SD reads
loadStripedNUs p@Params {..} mb =
  let perCard = mb / fromIntegral nZones
   in sdFirstUs + perCard / sdBWmbs * 1e6

-- Work stealing: cost of migrating an actor (stack + heap) `hops` away,
-- versus the remaining work that makes the steal worthwhile.
migrateUs :: Params -> Int -> Double -> Double
migrateUs p@Params {..} hops kb =
  (fromIntegral hops * hopNs + serNs (kb * 1024) linkEffMBs) / 1000

-- ===========================================================================
-- 6. REPORT
-- ===========================================================================

hr :: String -> IO ()
hr t = do
  putStrLn ""
  putStrLn (replicate 74 '=')
  putStrLn t
  putStrLn (replicate 74 '=')

main :: IO ()
main = do
  let p@Params {..} = defaultParams

  hr "0. CONFIGURATION"
  printf
    "  ring zones            %d  (max %d hops, shortest-path)\n"
    nZones
    (maxHops p)
  printf
    "  harts / zone          %d   -> %d harts total\n"
    hartsPerZone
    (nZones * hartsPerZone)
  printf
    "  fabric clock          %.0f MHz  (%.0f ns / cycle)\n"
    fabricMHz
    (1000 / fabricMHz)
  printf
    "  HyperRAM              %.0f MB/s x %d dies/zone = %.0f MB/s zone aggregate\n"
    hyperBWmbs
    hyperDies
    (hyperBWmbs * fromIntegral hyperDies)
  printf
    "  ring link             %.0f MB/s raw, %.0f MB/s effective, %.0f ns/hop\n"
    linkRawMBs
    linkEffMBs
    hopNs
  printf "  SD per zone           %.0f MB/s, %.0f us first-data\n" sdBWmbs sdFirstUs
  printf
    "  ring MTU              %.0f B  (%.0f ns serialisation)\n"
    mtuBytes
    (serNs mtuBytes linkEffMBs)

  -- -----------------------------------------------------------------------
  hr "1. UNCONTENDED ACCESS LATENCY  (ns)"
  putStrLn "  A cacheline-ish read, and a small actor message, by hop distance."
  putStrLn ""
  printf "  %-14s %10s %10s %10s\n" "payload" "local" "1 hop" "2 hops"
  mapM_
    ( \b ->
        printf
          "  %-14s %10.0f %10.0f %10.0f\n"
          (show (round b :: Int) ++ " B read")
          (readNs p 0 b)
          (readNs p 1 b)
          (readNs p 2 b)
    )
    [64, 256, 1024, 4096]
  printf
    "  %-14s %10.0f %10.0f %10.0f\n"
    "64 B message"
    (msgNs p 0 64)
    (msgNs p 1 64)
    (msgNs p 2 64)
  putStrLn ""
  printf
    "  remote/local penalty for a 64 B read: %.2fx (1 hop), %.2fx (2 hops)\n"
    (readNs p 1 64 / readNs p 0 64)
    (readNs p 2 64 / readNs p 0 64)
  printf
    "  remote/local penalty for a 4 KB read: %.2fx (1 hop), %.2fx (2 hops)\n"
    (readNs p 1 4096 / readNs p 0 4096)
    (readNs p 2 4096 / readNs p 0 4096)

  -- -----------------------------------------------------------------------
  hr "2. WHERE THE LATENCY PENALTY STOPS MATTERING"
  putStrLn "  Ratio remote(2 hop)/local as transfer size grows. The penalty is"
  putStrLn "  a fixed ns cost, so it amortises; the asymptote is the bandwidth"
  putStrLn "  ratio min(link,dram)/dram, NOT the latency ratio."
  putStrLn ""
  printf "  %-12s %10s %10s %8s\n" "size" "local ns" "2hop ns" "ratio"
  mapM_
    ( \b ->
        printf
          "  %-12s %10.0f %10.0f %8.2fx\n"
          (humanBytes b)
          (readNs p 0 b)
          (readNs p 2 b)
          (readNs p 2 b / readNs p 0 b)
    )
    [64, 256, 1024, 4096, 16384, 65536, 262144, 1048576]
  printf
    "\n  asymptotic ratio = %.2fx  (link %.0f MB/s vs die %.0f MB/s)\n"
    (hyperBWmbs / min hyperBWmbs linkEffMBs)
    linkEffMBs
    hyperBWmbs
  putStrLn "  => with these numbers the ring is NOT the streaming bottleneck."
  putStrLn "     HyperRAM is. Remote bulk streaming costs a fixed ~1us, not a"
  printf
    "     bandwidth tax. Remote *small* access is what hurts (%.2fx on 64 B).\n"
    (readNs p 2 64 / readNs p 0 64)

  -- -----------------------------------------------------------------------
  hr "3. WORST-CASE BOUND vs MTU  (the one hardware knob that sets it)"
  putStrLn "  Round-robin, non-preemptive, 3 input ports per output."
  putStrLn "  Worst case = beaten by (ports-1) full MTUs at every hop."
  putStrLn ""
  printf "  %-8s %12s %14s %16s\n" "MTU" "ser ns" "per-hop worst" "2-hop 64B read"
  mapM_
    ( \m ->
        let p' = p {mtuBytes = m}
         in printf
              "  %-8s %12.0f %14.0f %16.0f\n"
              (humanBytes m)
              (serNs m linkEffMBs)
              (perHopWorstNs p')
              (readBoundNs p' 2 64)
    )
    [64, 128, 256, 512, 1024, 4096]
  putStrLn ""
  printf
    "  At the configured %.0f B MTU: uncontended %.0f ns, bounded worst %.0f ns (%.1fx).\n"
    mtuBytes
    (readNs p 2 64)
    (readBoundNs p 2 64)
    (readBoundNs p 2 64 / readNs p 2 64)
  putStrLn "  The bound holds ONLY with per-zone injection credits. Without"
  putStrLn "  rate limiting there is no bound: one zone can hold a link forever."

  -- -----------------------------------------------------------------------
  hr "4. RING UNDER LOAD  (simulated, 20k packets per point)"
  putStrLn "  Offered load is per-zone injection rate. Latency in ns."
  putStrLn "  Store-and-forward; a cut-through router would shave ~1 MTU per hop."
  putStrLn ""
  mapM_
    ( \pat -> do
        printf "  pattern: %s\n" (show pat)
        printf
          "    %-10s %10s %10s %10s %12s\n"
          "offered/zone"
          "mean"
          "p99"
          "drift"
          "state"
        mapM_
          ( \load -> do
              let ls = simulate p (genTraffic p pat load 20000)
                  (m, _, p99, _) = stats ls
                  d = drift ls
                  st = if d > 1.5 then "SATURATED" else "stable" :: String
              printf
                "    %-10s %10.0f %10.0f %9.1fx %12s\n"
                (show (round load :: Int) ++ " MB/s")
                m
                p99
                d
                st
          )
          [25, 50, 100, 150, 200, 250, 300, 400]
        putStrLn ""
    )
    [Uniform, Hotspot 2, NeighbourOnly]
  printf
    "  analytic worst-case bound for reference: %.0f ns\n"
    (fromIntegral (maxHops p) * perHopWorstNs p)
  putStrLn "  Latency is flat and equal to the uncontended figure until a link"
  putStrLn "  oversubscribes, then the queue simply diverges -- there is no"
  putStrLn "  graceful degradation region worth designing around. The number to"
  putStrLn "  care about is the last stable row, not the shape after it."

  -- -----------------------------------------------------------------------
  hr "5. HOTSPOT CAPACITY  (everyone hits one pinned accelerator)"
  let ingress = 2 * linkEffMBs -- two ring links into a zone
      zoneMem = hyperBWmbs * fromIntegral hyperDies
  printf "  ring ingress into one zone   %6.0f MB/s (2 links)\n" ingress
  printf "  that zone's DRAM aggregate   %6.0f MB/s\n" zoneMem
  printf "  local hart demand on it      %6.0f MB/s (assume 1 die's worth)\n" hyperBWmbs
  printf "  => headroom for remote users %6.0f MB/s\n" (zoneMem - hyperBWmbs)
  putStrLn ""
  putStrLn "  The accelerator's own zone memory saturates before the ring links"
  putStrLn "  do. Adding a central hub would NOT fix this -- the bottleneck is"
  putStrLn "  the destination zone, not the fabric. That is the strongest"
  putStrLn "  argument that dropping the hub costs nothing here."

  -- -----------------------------------------------------------------------
  hr "6. Files.qa: LOCAL vs STRIPED ACROSS ZONE SD CARDS"
  printf
    "  %-10s %12s %14s %14s %10s\n"
    "size"
    "local us"
    "striped(1cons)"
    "striped(Ncons)"
    "speedup"
  mapM_
    ( \mb ->
        printf
          "  %-10s %12.0f %14.0f %14.0f %9.2fx\n"
          (humanBytes (mb * 1048576))
          (loadLocalUs p mb)
          (loadStriped1Us p mb)
          (loadStripedNUs p mb)
          (loadLocalUs p mb / loadStriped1Us p mb)
    )
    [0.064, 0.5, 4, 16, 64, 128]
  putStrLn ""
  printf
    "  Striping reaches 2x above roughly %s, and asymptotes near %.1fx.\n"
    (crossover p)
    (loadLocalUs p 1024 / loadStriped1Us p 1024)
  putStrLn "  Below that, the SD command latency dominates and striping buys"
  putStrLn "  little. Model assumes Files.qa issues all N card commands"
  putStrLn "  concurrently -- serialising them would erase most of the gain."
  putStrLn "  Co-locating consumers with chunks recovers the full Nx -- that is"
  putStrLn "  the case for chunk-follows-card placement over gather-to-requester."

  -- -----------------------------------------------------------------------
  hr "7. WORK-STEALING BREAK-EVEN"
  putStrLn "  Migrating an actor costs its stack+heap over the ring. Stealing"
  putStrLn "  only pays if the remaining work exceeds that."
  putStrLn ""
  printf
    "  %-14s %10s %10s %14s\n"
    "actor footprint"
    "1 hop us"
    "2 hop us"
    "as % quantum"
  mapM_
    ( \kb ->
        printf
          "  %-14s %10.1f %10.1f %13.1f%%\n"
          (show (round kb :: Int) ++ " KB")
          (migrateUs p 1 kb)
          (migrateUs p 2 kb)
          (migrateUs p 2 kb / quantumUs * 100)
    )
    [1, 4, 16, 64, 256]
  putStrLn ""
  printf "  With a %.0f us quantum, actors under ~16 KB are essentially free to\n" quantumUs
  putStrLn "  steal; a 256 KB actor costs ~6 quanta and should be pinned unless"
  putStrLn "  the imbalance is large and persistent. A cheap policy that falls"
  putStrLn "  out of this: make footprint a field in the ACB and refuse steals"
  putStrLn "  where footprint/linkBW exceeds the victim's backlog depth."

  -- -----------------------------------------------------------------------
  hr "8. SUMMARY OF BOUNDS"
  printf "  local 64B read              %8.0f ns\n" (readNs p 0 64)
  printf
    "  worst-case remote 64B read  %8.0f ns   (2 hops, %s MTU, credited)\n"
    (readBoundNs p 2 64)
    (humanBytes mtuBytes)
  printf "  ring bisection bandwidth    %8.0f MB/s (2 links cut)\n" (2 * linkEffMBs)
  printf
    "  total DRAM bandwidth        %8.0f MB/s (%d zones)\n"
    (fromIntegral nZones * hyperBWmbs * fromIntegral hyperDies)
    nZones
  printf
    "  total SD bandwidth          %8.0f MB/s (%d cards)\n"
    (fromIntegral nZones * sdBWmbs)
    nZones
  printf
    "  compute:bisection ratio     %8.2f  (DRAM BW / bisection BW)\n"
    (fromIntegral nZones * hyperBWmbs * fromIntegral hyperDies / (2 * linkEffMBs))
  let ratio =
        fromIntegral nZones
          * hyperBWmbs
          * fromIntegral hyperDies
          / (2 * linkEffMBs)
  putStrLn ""
  printf "  Read that as: the ring carries only 1/%.1f of what the zones can chew\n" ratio
  putStrLn "  locally. The design is sound only if the common case really is"
  putStrLn "  zone-local -- which is what affinity, service pinning and"
  putStrLn "  chunk-follows-card are all buying you. They are not optimisations"
  putStrLn "  here; they are what keeps the machine off its bisection limit."

-- crude crossover search for the striping analysis
crossover :: Params -> String
crossover p =
  case [ mb | mb <- [0.002, 0.004 .. 40], loadLocalUs p mb / loadStriped1Us p mb >= 2.0
       ] of
    (m : _) -> humanBytes (m * 1048576)
    [] -> "never"

humanBytes :: Double -> String
humanBytes b
  | b >= 1048576 = show (round (b / 1048576) :: Int) ++ " MB"
  | b >= 1024 = show (round (b / 1024) :: Int) ++ " KB"
  | otherwise = show (round b :: Int) ++ " B"
