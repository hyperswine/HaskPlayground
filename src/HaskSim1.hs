{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use isNothing" #-}

-- RingSim: cycle-accurate simulation of a 4-node unidirectional slotted ring.
--
-- Topology:  node0 -> node1 -> node2 -> node3 -> node0
--   Zone A = {node0, node1}, UART attached at node0
--   Zone B = {node2, node3}, NIC  attached at node2
--
-- Interconnect: 4 slots circulate, one hop per cycle.
-- Injection policies:
--   TDM : node i may only fill slot i        (strict fixed-latency token scheme)
--   OPP : node i may fill any empty slot passing by (opportunistic)
--
-- Workload: each processor, each cycle, with prob lambda issues an RPC
-- (single-flit request) to UART or NIC (50/50). Device replies with a
-- single-flit response. Local access (proc on the device's own node)
-- bypasses the ring, fixed 2-cycle latency.
--
-- Also: bulk "DMA-style" transfer of B flits node3 -> node2 over the ring
-- vs. a dedicated neighbor link (1 flit/cycle, 1 hop).

module HaskSim1 where

import Data.Bits (shiftR)
import Data.List (foldl', sort)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq, ViewL (..), (|>))
import qualified Data.Sequence as Sq
import Data.Word (Word64)
import Text.Printf (printf)

nNodes :: Int
nNodes = 4

uartNode, nicNode :: Int
uartNode = 0
nicNode = 2

data Policy = TDM | OPP deriving (Eq, Show)

data Kind = Req | Resp | Bulk deriving (Eq, Show)

data Flit = Flit
  { fKind :: !Kind,
    fSrc :: !Int,
    fDst :: !Int,
    fBorn :: !Int
  }
  deriving (Show)

data St = St
  { stCycle :: !Int,
    stSlots :: ![(Int, Maybe Flit)], -- (slotId, payload); list index = node position
    stQueues :: !(M.Map Int (Seq Flit)),
    stDone :: ![(Int, Int)], -- (srcNode, roundTripLatency) for RPCs
    stBulkIn :: !Int, -- bulk flits delivered
    stBulkEnd :: !(Maybe Int), -- cycle last bulk flit delivered
    stRng :: !Word64,
    stIssued :: !Int -- RPCs issued after warmup
  }

-- ---------- tiny LCG ----------
nextR :: Word64 -> (Double, Word64)
nextR s =
  let s' = 6364136223846793005 * s + 1442695040888963407
      d = fromIntegral (s' `shiftR` 11) / 9007199254740992.0 -- 53 bits
   in (d, s')

-- ---------- one simulated cycle ----------
step :: Policy -> Double -> Int -> St -> St
step pol lam warm st0 =
  let cyc = stCycle st0

      -- 1. rotate: slot at position p moves to p+1 (rotate right)
      rot = let xs = stSlots st0 in last xs : init xs

      -- 2. deliver at each position
      (slots1, q1, done1, bin1, bend1) = foldl' deliver (rot, stQueues st0, stDone st0, stBulkIn st0, stBulkEnd st0) (zip [0 ..] rot)
        where
          deliver acc@(sl, q, dn, bi, be) (pos, (_, mf)) = case mf of
              Just f | fDst f == pos ->
                    let sl' = setSlot pos Nothing sl
                     in case fKind f of
                          Resp -> (sl', q, rec dn f, bi, be)
                          Bulk -> (sl', q, dn, bi + 1, Just cyc)
                          Req ->
                            -- device replies: enqueue Resp at this node, keep born
                            let resp = Flit Resp pos (fSrc f) (fBorn f)
                                q' = M.adjust (|> resp) pos q
                             in (sl', q', dn, bi, be)
              _ -> acc
          rec dn f | fBorn f >= warm = (fDst f, cyc - fBorn f) : dn
                   | otherwise = dn
          setSlot p v sl = [if i == p then (sid, v) else (sid, c) | (i, (sid, c)) <- zip [0 ..] sl]

      -- 3. injection
      (slots2, q2) = foldl' inject (slots1, q1) [0 .. nNodes - 1]
        where
          inject (sl, q) pos =
            let (sid, mf) = sl !! pos
                ok = mf == Nothing && (pol == OPP || sid == pos)
             in if not ok then
                 (sl, q)
                else case Sq.viewl (M.findWithDefault Sq.empty pos q) of
                  EmptyL -> (sl, q)
                  f :< fs -> ( [ if i == pos then (s, Just f) else (s, c) | (i, (s, c)) <- zip [0 ..] sl ], M.insert pos fs q )

      -- Maybe Flit lacks Eq via Flit; compare emptiness manually:
      -- (handled below by pattern instead)

      -- 4. traffic generation
      (q3, rng', done2, iss') =
        foldl' gen (q2, stRng st0, done1, stIssued st0) [0 .. nNodes - 1]
        where
          gen (q, r, dn, is) node =
            let (p, r1) = nextR r
             in if p >= lam then
                    (q, r1, dn, is)
                else
                    let (c, r2) = nextR r1
                        dst = if c < 0.5 then uartNode else nicNode
                        is' = if cyc >= warm then is + 1 else is
                    -- local access: bypass ring, fixed 2-cycle latency
                     in if dst == node then
                          ( q, r2, if cyc >= warm then (node, 2) : dn else dn, is' )
                      else
                          ( M.adjust (|> Flit Req node dst cyc) node q, r2, dn, is' )
   in st0 { stCycle = cyc + 1,   stSlots = slots2,   stQueues = q3,   stDone = done2,   stBulkIn = bin1,   stBulkEnd = bend1,   stRng = rng',   stIssued = iss' }

-- Eq needed for `mf == Nothing` above
instance Eq Flit where
  a == b = fBorn a == fBorn b   && fSrc a == fSrc b   && fDst a == fDst b   && fKind a == fKind b

initSt :: Word64 -> St
initSt seed = St   { stCycle = 0,     stSlots = [(i, Nothing) | i <- [0 .. nNodes - 1]],     stQueues = M.fromList [(i, Sq.empty) | i <- [0 .. nNodes - 1]],     stDone = [],     stBulkIn = 0,     stBulkEnd = Nothing,     stRng = seed,     stIssued = 0   }

runSim :: Policy -> Double -> Int -> Int -> St
runSim pol lam warm total = go (initSt 42) 0
  where
    go !st !n
      | n >= total = st
      | otherwise = go (step pol lam warm st) (n + 1)

-- ---------- bulk experiment ----------
runBulk :: Policy -> Double -> Int -> (Int, St)
runBulk pol lam bulkN =
  let warm = 2000
      injAt = 2500
      st1 = runN (initSt 7) injAt
      bulk = Sq.fromList [Flit Bulk 3 nicNode injAt | _ <- [1 .. bulkN]]
      st2 = st1 {stQueues = M.adjust (Sq.>< bulk) 3 (stQueues st1)}
      st3 = go st2
   in (injAt, st3)
  where
    runN !st 0 = st
    runN !st k = runN (step pol lam 2000 st) (k - 1)
    go !st = case stBulkEnd st of
      Just _ | stBulkIn st >= bulkN -> st
      _ | stCycle st > 400000 -> st
      _ -> go (step pol lam 2000 st)

-- ---------- stats ----------
mean :: [Int] -> Double
mean [] = 0
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

pct :: Double -> [Int] -> Int
pct _ [] = 0
pct p xs = let s = sort xs in s !! min (length s - 1) (floor (p * fromIntegral (length s)))

main :: IO ()
main = do
  let warm = 2000
      total = 42000
      meas = total - warm
      lams = [0.02, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.40]

  printf "=== RPC latency/throughput vs offered load (per-node lambda) ===\n"
  printf
    "%-6s %-6s %-10s %-8s %-8s %-8s %-10s %-8s\n"
    "pol"
    "lam"
    "issued/cyc"
    "done/cyc"
    "mean"
    "p50"
    "p95"
    "maxQ"
  results <-
    mapM
      ( \(pol, lam) -> do
          let st = runSim pol lam warm total
              lats = map snd (stDone st)
              thr = fromIntegral (length lats) / fromIntegral meas :: Double
              off = fromIntegral (stIssued st) / fromIntegral meas :: Double
              mq = maximum (map Sq.length (M.elems (stQueues st)))
          printf
            "%-6s %-6.2f %-10.3f %-8.3f %-8.1f %-8d %-10d %-8d\n"
            (show pol)
            lam
            off
            thr
            (mean lats)
            (pct 0.5 lats)
            (pct 0.95 lats)
            mq
          return (pol, lam, thr, mean lats, pct 0.95 lats)
      )
      [(p, l) | p <- [TDM, OPP], l <- lams]

  printf "\n=== per-source latency at lambda = 0.10 (remote RPCs, TDM) ===\n"
  let st10 = runSim TDM 0.10 warm total
      bySrc s = [l | (n, l) <- stDone st10, n == s, l > 2]
  mapM_
    ( \s ->
        printf
          "  node%d: mean %.1f  p95 %d   (n=%d)\n"
          s
          (mean (bySrc s))
          (pct 0.95 (bySrc s))
          (length (bySrc s))
    )
    [0 .. 3]

  printf "\n=== bulk transfer: 256 flits node3 -> node2 (NIC) over ring ===\n"
  printf "(dedicated neighbor DMA link would take ~257 cycles: 256 flits + 1 hop)\n"
  mapM_
    ( \(pol, lam) -> do
        let (injAt, st) = runBulk pol lam 256
        case stBulkEnd st of
          Just e ->
            printf
              "  %-4s bg-load %.2f : %5d cycles  (%.2fx dedicated link)\n"
              (show pol)
              lam
              (e - injAt)
              (fromIntegral (e - injAt) / 257.0 :: Double)
          Nothing -> printf "  %-4s bg-load %.2f : did not complete\n" (show pol) lam
    )
    [(p, l) | p <- [TDM, OPP], l <- [0.0, 0.05, 0.15]]

  -- machine-readable block for charting
  printf "\n#CHART\n"
  mapM_
    ( \(pol, lam, thr, ml, p95) ->
        printf "%s,%.2f,%.4f,%.2f,%d\n" (show pol) lam thr ml p95
    )
    results
