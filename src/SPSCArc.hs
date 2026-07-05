{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- ARC via message passing over SPSC rings, vs shared atomic RC.
--
-- Model: H harts in a full SPSC mesh (one Lamport ring per ordered pair,
-- cached local index copies as validated earlier). Each hart owns nObj
-- objects. RC placement:
--
--   atomic : RC words in shared memory; every inc/dec is an uncached AMO.
--   msg    : RC words live in the OWNER's hart-local memory (single-writer,
--            cached, 1 cy). Remote inc/dec are sent as messages on the
--            SPSC rings and applied by the owner when it drains.
--   msgB   : as msg, but decs are batched per (owner,object) up to 8 and
--            shipped as one counted DEC message.
--
-- Workload ("send" event): hart s picks a random object (owner o, obj b)
-- and destination d; s applies/ships +1 to the owner, ships a payload
-- message to d; d, on receipt, applies/ships -1 to the owner. Payload
-- traffic is identical across designs, so differences are pure RC cost.
--
-- Hazard instrumentation: a dec that drives an owner-local count to <= 0
-- while the system is live is a would-be PREMATURE FREE (the matching inc
-- is still in flight on another ring). Counted, not "fixed" -- the fix is
-- a design decision (deferred reclamation / ZCT at owner safepoints).

module SPSCArc where

import Data.Bits (shiftR, xor)
import qualified Data.IntMap.Strict as IM
import Text.Printf (printf)

-- ---------------------------------------------------------------- costs
cUnc, cAmo, cFen, cLoc :: Int
cUnc = 40
cAmo = 60
cFen = 8
cLoc = 1

ringSz, nObj, batchCap :: Int
ringSz = 512
nObj = 8
batchCap = 8

-- ------------------------------------------------------------------ DSL
type Addr = Int

data Prog
  = Load !Addr (Int -> Prog)
  | Store !Addr !Int Prog
  | AmoAdd !Addr !Int (Int -> Prog)
  | Fence Prog
  | Local Prog
  | Applied !Int !Bool Prog -- n RC ops applied; True = zero/neg touch
  | NoteMsg Prog -- one ring message sent

-- ------------------------------------------------------------ scheduler
data Hart = Hart {hclk :: !Int, hprog :: Prog}

data Stats = Stats {sUnc, sAmoN, sFen, sApp, sZero, sMsg :: !Int}

nextRng :: Int -> (Int, Int)
nextRng s0 =
  let s = s0 + 0x9E3779B97F4A7C15
      z1 = (s `xor` (s `shiftR` 30)) * 0xBF58476D1CE4E5B9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
   in (abs (z2 `xor` (z2 `shiftR` 31)), s)

budget :: Int
budget = 200_000_000

runSim :: IM.IntMap Int -> [Prog] -> Int -> (Stats, Int, Bool)
runSim mem0 progs target = go mem0 hs0 (Stats 0 0 0 0 0 0) 999 0
  where
    hs0 = [Hart 0 p | p <- progs]
    go !mem harts !st !r !mk
      | sApp st >= target = (st, mk, True)
      | mk > budget = (st, mk, False)
      | otherwise =
          let (i, h) = minH 0 (head harts) 1 (tail harts)
              (j, r') = nextRng r
              jit = j `mod` 5
              adv c p' =
                let h' = Hart (hclk h + c) p'
                 in go mem (upd i h' harts) st r' (max mk (hclk h + c))
              advS c p' st' =
                let h' = Hart (hclk h + c) p'
                 in go mem (upd i h' harts) st' r' (max mk (hclk h + c))
              advM c p' mem' st' =
                let h' = Hart (hclk h + c) p'
                 in go mem' (upd i h' harts) st' r' (max mk (hclk h + c))
           in case hprog h of
                Local k -> adv cLoc k
                Fence k -> advS cFen k st {sFen = sFen st + 1}
                NoteMsg k -> advS 0 k st {sMsg = sMsg st + 1}
                Applied n z k ->
                  advS
                    cLoc
                    k
                    st
                      { sApp = sApp st + n,
                        sZero = sZero st + (if z then 1 else 0)
                      }
                Load a k ->
                  advS
                    (cUnc + jit)
                    (k (IM.findWithDefault 0 a mem))
                    st {sUnc = sUnc st + 1}
                Store a v k ->
                  advM (cUnc + jit) k (IM.insert a v mem) st {sUnc = sUnc st + 1}
                AmoAdd a d k ->
                  let v = IM.findWithDefault 0 a mem
                   in advM
                        (cAmo + jit)
                        (k v)
                        (IM.insert a (v + d) mem)
                        st {sAmoN = sAmoN st + 1}
    minH !bi !bh !_ [] = (bi, bh)
    minH !bi !bh !i (x : xs)
      | hclk x < hclk bh = minH i x (i + 1) xs
      | otherwise = minH bi bh (i + 1) xs
    upd i x xs = [if j == i then x else y | (j, y) <- zip [0 ..] xs]

-- ------------------------------------------------- rings + hart closures

-- ring i -> j at base 1_000_000 * (i * 32 + j + 1); head, tail, slots+100
rBase :: Int -> Int -> Int
rBase i j = 1_000_000 * (i * 32 + j + 1)

-- message encoding: kind * 1e6 + owner * 1e4 + obj * 100 + count
-- kinds: 0 payload, 2 inc, 3 dec
enc :: Int -> Int -> Int -> Int -> Int
enc kind o b c = kind * 1_000_000 + o * 10_000 + b * 100 + c

deKind, deOwn, deObj, deCnt :: Int -> Int
deKind v = v `div` 1_000_000
deOwn v = (v `div` 10_000) `mod` 100
deObj v = (v `div` 100) `mod` 100
deCnt v = v `mod` 100

data HS = HS
  { hrng :: !Int,
    sleft :: !Int, -- sends still to issue
    outbox :: [(Int, Int)], -- (dst, val), FIFO
    tls :: !(IM.IntMap Int), -- my tail per dst ring
    phd :: !(IM.IntMap Int), -- cached head copy per dst ring
    chs :: !(IM.IntMap (Int, Int)), -- (head, cached tail) per src ring
    rcs :: !(IM.IntMap Int), -- my objects' RC (msg designs)
    dbuf :: !(IM.IntMap Int) -- pending dec counts, key o*100+b
  }

hs0 :: Int -> HS
hs0 me =
  HS
    (7919 * me + 13)
    0
    []
    IM.empty
    IM.empty
    IM.empty
    (IM.fromList [(b, 1) | b <- [0 .. nObj - 1]])
    IM.empty

rnd :: Int -> HS -> (Int, HS)
rnd m hs = let (z, r') = nextRng (hrng hs) in (z `mod` m, hs {hrng = r'})

-- try to push one message into ring me->dst; k gets success flag
trySend :: Int -> Int -> Int -> HS -> (Bool -> HS -> Prog) -> Prog
trySend me dst val hs k
  | t - ch < ringSz = commit
  | otherwise =
      Load base $ \h ->
        let hs' = hs {phd = IM.insert dst h (phd hs)}
         in if t - h < ringSz
              then trySend me dst val hs' k
              else k False hs'
  where
    base = rBase me dst
    t = IM.findWithDefault 0 dst (tls hs)
    ch = IM.findWithDefault 0 dst (phd hs)
    commit =
      Store (base + 100 + t `mod` ringSz) val $
        Fence $
          Store (base + 1) (t + 1) $
            NoteMsg $
              k True hs {tls = IM.insert dst (t + 1) (tls hs)}

-- drain up to 6 messages from each inbound ring, handling each via f
drainAll ::
  Int ->
  Int ->
  (Int -> HS -> (HS -> Prog) -> Prog) ->
  HS ->
  (HS -> Prog) ->
  Prog
drainAll me nH f hs0' k = ring 0 hs0'
  where
    ring src hs
      | src == nH = k hs
      | src == me = ring (src + 1) hs
      | otherwise =
          let base = rBase src me
              (h, ct) = IM.findWithDefault (0, 0) src (chs hs)
           in if h == ct
                then Load (base + 1) $ \t ->
                  let hs' = hs {chs = IM.insert src (h, t) (chs hs)}
                   in if h == t
                        then ring (src + 1) hs'
                        else grab src hs' 0
                else grab src hs 0
    grab src hs n =
      let base = rBase src me
          (h, ct) = IM.findWithDefault (0, 0) src (chs hs)
       in if h == ct || n == 6
            then Store base h $ ring (src + 1) hs
            else Load (base + 100 + h `mod` ringSz) $ \v ->
              f v hs {chs = IM.insert src (h + 1, ct) (chs hs)} $
                \hs' -> grab src hs' (n + 1)

-- --------------------------------------------------------------- designs
data Design = Atomic | Msg | MsgB deriving (Eq)

rcAddr :: Int -> Int -> Int
rcAddr o b = 900_000_000 + o * 100 + b

hart :: Design -> Int -> Int -> Int -> Prog
hart dsg nH me nSends = loop (hs0 me) {sleft = nSends}
  where
    loop hs = flushOut hs $ \hs1 ->
      gen hs1 $ \hs2 ->
        drainAll me nH handle hs2 $ \hs3 ->
          flushDecs hs3 $ \hs4 ->
            Local (loop hs4)

    -- push up to 2 outbox messages per iteration
    flushOut hs k = go2 (2 :: Int) hs
      where
        go2 0 h = k h
        go2 n h = case outbox h of
          [] -> k h
          ((d, v) : rest) ->
            trySend me d v h {outbox = rest} $ \ok h' ->
              if ok
                then go2 (n - 1) h'
                else k h' {outbox = (d, v) : outbox h'}

    -- issue one new send event, if any remain and outbox is small
    gen hs k
      | sleft hs == 0 || length (outbox hs) > 8 = k hs
      | otherwise =
          let (o, hs1) = rnd nH hs
              (b, hs2) = rnd nObj hs1
              (d0, hs3) = rnd (nH - 1) hs2
              d = if d0 >= me then d0 + 1 else d0
              hs4 = hs3 {sleft = sleft hs3 - 1}
              payload = (d, enc 0 o b 1)
           in case dsg of
                Atomic ->
                  AmoAdd (rcAddr o b) 1 $ \_ ->
                    Applied 1 False $
                      k hs4 {outbox = outbox hs4 ++ [payload]}
                _
                  | o == me -> -- owner-local inc: cached, 1 cy
                      let n = IM.findWithDefault 0 b (rcs hs4) + 1
                       in Local $
                            Applied 1 False $
                              k
                                hs4
                                  { rcs = IM.insert b n (rcs hs4),
                                    outbox = outbox hs4 ++ [payload]
                                  }
                  | otherwise ->
                      k hs4 {outbox = outbox hs4 ++ [(o, enc 2 o b 1), payload]}

    -- handle one inbound message
    handle v hs k = case deKind v of
      2 ->
        let b = deObj v
            n = IM.findWithDefault 0 b (rcs hs) + 1
         in Local $ Applied 1 False $ k hs {rcs = IM.insert b n (rcs hs)}
      3 -> applyDec (deObj v) (deCnt v) hs k
      _ ->
        -- payload received: drop the ref -> dec at owner
        let o = deOwn v; b = deObj v
         in case dsg of
              Atomic ->
                AmoAdd (rcAddr o b) (-1) $ \old ->
                  Applied 1 (old <= 1) $ k hs
              _
                | o == me -> applyDec b 1 hs k
                | dsg == Msg ->
                    k hs {outbox = outbox hs ++ [(o, enc 3 o b 1)]}
                | otherwise -> -- MsgB: batch decs per (owner,obj)
                    let key = o * 100 + b
                        c = IM.findWithDefault 0 key (dbuf hs) + 1
                     in if c == batchCap
                          then
                            k
                              hs
                                { dbuf = IM.delete key (dbuf hs),
                                  outbox = outbox hs ++ [(o, enc 3 o b c)]
                                }
                          else k hs {dbuf = IM.insert key c (dbuf hs)}

    applyDec b c hs k =
      let n = IM.findWithDefault 0 b (rcs hs) - c
       in Local $ Applied c (n <= 0) $ k hs {rcs = IM.insert b n (rcs hs)}

    -- once my sends are done, flush any batched decs so the system drains
    flushDecs hs k
      | sleft hs > 0 || IM.null (dbuf hs) = k hs
      | otherwise =
          let flushed =
                [ (key `div` 100, enc 3 (key `div` 100) (key `mod` 100) c)
                  | (key, c) <- IM.toList (dbuf hs)
                ]
           in k hs {dbuf = IM.empty, outbox = outbox hs ++ flushed}

-- ------------------------------------------------------------------ main
runOne :: Design -> String -> Int -> Int -> IO ()
runOne dsg name nH m = do
  let mem0 =
        if dsg == Atomic
          then
            IM.fromList
              [ (rcAddr o b, 1) | o <- [0 .. nH - 1], b <- [0 .. nObj - 1]
              ]
          else IM.empty
      progs = [hart dsg nH i m | i <- [0 .. nH - 1]]
      target = 2 * m * nH
      (st, mk, done) = runSim mem0 progs target
      sends = m * nH
      f x = fromIntegral x / fromIntegral sends :: Double
  printf
    "  %-6s H=%d  %s  %9d cy makespan  %7.1f cy/send  unc/send %5.1f  amo/send %4.2f  msgs/send %4.2f  premature-free hazards: %d\n"
    name
    nH
    (if done then "OK  " else "STALL" :: String)
    mk
    (fromIntegral mk / fromIntegral sends :: Double)
    (f (sUnc st))
    (f (sAmoN st))
    (f (sMsg st))
    (sZero st)

main :: IO ()
main = do
  printf
    "costs: uncached=%d amo=%d fence=%d local(cached RC op)=%d  ring=%d  objs/hart=%d  dec batch=%d\n\n"
    cUnc
    cAmo
    cFen
    cLoc
    ringSz
    nObj
    batchCap
  let m = 2000
  mapM_
    ( \nH -> do
        runOne Atomic "atomic" nH m
        runOne Msg "msg" nH m
        runOne MsgB "msgB" nH m
        putStrLn ""
    )
    [2, 4, 8]
