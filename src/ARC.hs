{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- ARC + two-queue mailbox correctness/efficiency simulation.
--
-- System model:
--   * nProcs processors, each with a "cache" modeled as single-owner tracking of
--     contended lines (shared-heap RC words, mailbox tails). Cross-proc access to
--     a line owned elsewhere counts as a coherence transfer (the cost metric).
--   * kActors actors pinned round-robin to processors. Each actor owns a
--     BEAM-style two-queue mailbox:
--       - writer queue: shared side, any actor appends (MPSC)
--       - reader queue: private to the owner; owner drains writer->reader
--         atomically (models taking the lock / atomically swapping the list),
--         then reads without synchronization.
--     A message, once enqueued, is never touched again by the sender: the
--     message node itself is a linear transfer (exactly-once ownership).
--   * Shared heap of ARCed objects (promoted from actor-local heaps). Sending a
--     shared reference: +1 before publish; the in-flight message owns that
--     count; the receiver adopts it (no op); dropping does -1; free at 0.
--   * Linear objects: moved through messages whole, freed by the final owner,
--     zero RC traffic by construction.
--
-- RC modes:
--   EagerRc      inc and dec are immediate atomic ops on the shared line
--   DeferredDec  inc eager (before publish), dec buffered per-proc, flushed in
--                batches  -- the correct deferral direction: RC may be
--                transiently HIGH relative to true refs, never low
--   DeferredInc  (deliberately WRONG) inc buffered, dec eager -- RC can be
--                transiently LOW: frees fire while references exist
--
-- Mailbox modes:
--   AtomicMb     append is one atomic step (fetch-and-increment tail + store)
--   RacyMb       (deliberately WRONG) append is two steps: snapshot tail, then
--                store at snapshot -- two senders can snapshot the same slot
--                and one message is overwritten (lost) or delivered reordered
--
-- Checked invariants (ground truth computed independently of the RC field):
--   RC-1  RC never negative; no RC op ever targets a freed object
--   RC-2  free happens only when true refs == 0 (no premature free / UAF)
--   RC-3  at quiescence: alive => RC == true refs (no leaked/lost counts),
--         freed => true refs == 0 (no dangling references)
--   LIN-1 a linear object has exactly one owner (held or in exactly one
--         in-flight message); never aliased, never lost
--   MB-1  per-channel FIFO: receiver sees seq 1,2,3,... from each sender
--   MB-2  conservation: every message sent is eventually delivered exactly once
--   UAF   any payload read of a freed object is flagged
--
-- Correct configs must report ZERO violations over all seeds; buggy configs
-- must be caught. Efficiency is reported as coherence transfers per operation.

module ARC where

import Data.Bits (shiftR, xor)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word64)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Parameters
--------------------------------------------------------------------------------

nProcs, kActors, stepsPerSeed, numSeeds, aliveCap, heldCap, flushEvery :: Int
nProcs = 4
kActors = 8
stepsPerSeed = 150000
numSeeds = 20
aliveCap = 200 -- max live shared objects (gates new promotions)
heldCap = 10 -- max shared refs an actor accumulates before it stops promoting
flushEvery = 32 -- ~1/32 chance per step that a proc flushes its RC deltas

--------------------------------------------------------------------------------
-- PRNG (splitmix64)
--------------------------------------------------------------------------------

smix :: Word64 -> (Word64, Word64)
smix s0 =
  let s1 = s0 + 0x9E3779B97F4A7C15
      z1 = (s1 `xor` (s1 `shiftR` 30)) * 0xBF58476D1CE4E5B9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
   in (s1, z2 `xor` (z2 `shiftR` 31))

rnd :: Int -> St -> (St, Int)
rnd n st =
  let (s', out) = smix (sRng st)
   in (st {sRng = s'}, fromIntegral (out `mod` fromIntegral (max 1 n)))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type ObjId = Int

type ActorId = Int

type Proc = Int

type SeqNo = Int

data RcMode = EagerRc | DeferredDec | DeferredAll | DeferredInc deriving (Eq, Show)

data MbMode = AtomicMb | RacyMb deriving (Eq, Show)

data Mode = Mode {mName :: String, rcMode :: RcMode, mbMode :: MbMode}

data Payload = PShared !ObjId | PLinear !ObjId deriving (Eq, Show)

data Msg = Msg {mFrom :: !ActorId, mSeq :: !SeqNo, mPay :: !Payload}

data Obj = Obj {oRc :: !Int, oAlive :: !Bool, oLinear :: !Bool}

data Actor = Actor
  { aHeld :: !(M.Map ObjId Int), -- shared refs held (multiset)
    aLin :: ![ObjId], -- linear objects owned
    aReader :: ![Msg], -- private reader queue (head = next)
    aSeqOut :: !(M.Map ActorId SeqNo), -- next seq per destination channel
    aSeqIn :: !(M.Map ActorId SeqNo) -- last seq received per sender channel
  }

data WQ = WQ {wqTail :: !Int, wqSlots :: !(M.Map Int Msg)}

-- an in-progress racy append: tail was snapshotted, store not yet done
data Pending = PendAppend {pDst :: !ActorId, pMsg :: !Msg, pSnap :: !Int}

data Stats = Stats
  { stSendShared,
    stSendLin,
    stRcOps,
    stRcXfer,
    stMbXfer,
    stFrees,
    stLinFrees,
    stPromotes ::
      !Int
  }

zeroStats :: Stats
zeroStats = Stats 0 0 0 0 0 0 0 0

data St = St
  { sRng :: !Word64,
    sHeap :: !(M.Map ObjId Obj),
    sNextObj :: !Int,
    sAlive :: !Int, -- live shared object count
    sActors :: !(M.Map ActorId Actor),
    sWQ :: !(M.Map ActorId WQ),
    sPend :: !(M.Map Proc Pending),
    sDelta :: !(M.Map Proc (M.Map ObjId Int)), -- buffered RC deltas per proc
    sRcOwner :: !(M.Map ObjId Proc), -- cache-line owner of each RC word
    sMbOwner :: !(M.Map ActorId Proc), -- cache-line owner of each mailbox tail
    sStats :: !Stats,
    sViol :: ![String], -- reversed
    sStep :: !Int
  }

initSt :: Word64 -> St
initSt seed =
  St
    { sRng = seed,
      sHeap = M.empty,
      sNextObj = 0,
      sAlive = 0,
      sActors =
        M.fromList
          [ (a, Actor M.empty [] [] M.empty M.empty)
            | a <- [0 .. kActors - 1]
          ],
      sWQ = M.fromList [(a, WQ 0 M.empty) | a <- [0 .. kActors - 1]],
      sPend = M.empty,
      sDelta = M.empty,
      sRcOwner = M.empty,
      sMbOwner = M.empty,
      sStats = zeroStats,
      sViol = [],
      sStep = 0
    }

pinOf :: ActorId -> Proc
pinOf a = a `mod` nProcs

actorsOn :: Proc -> [ActorId]
actorsOn p = [a | a <- [0 .. kActors - 1], pinOf a == p]

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

violate :: String -> St -> St
violate msg st
  | length (sViol st) >= 12 = st
  | otherwise = st {sViol = (printf "step %-7d %s" (sStep st) msg) : sViol st}

bump :: (Stats -> Stats) -> St -> St
bump f st = st {sStats = f (sStats st)}

actor :: ActorId -> St -> Actor
actor a st = sActors st M.! a

putActor :: ActorId -> Actor -> St -> St
putActor a ac st = st {sActors = M.insert a ac (sActors st)}

-- coherence accounting: proc p touches the RC line of object o
touchRcLine :: Proc -> ObjId -> St -> St
touchRcLine p o st = case M.lookup o (sRcOwner st) of
  Just q | q == p -> st
  _ ->
    bump
      (\s -> s {stRcXfer = stRcXfer s + 1})
      st {sRcOwner = M.insert o p (sRcOwner st)}

touchMbLine :: Proc -> ActorId -> St -> St
touchMbLine p d st = case M.lookup d (sMbOwner st) of
  Just q | q == p -> st
  _ ->
    bump
      (\s -> s {stMbXfer = stMbXfer s + 1})
      st {sMbOwner = M.insert p' p (sMbOwner st)}
  where
    p' = d

--------------------------------------------------------------------------------
-- Ground truth: count every live reference to an object, independent of RC
--------------------------------------------------------------------------------

trueRefs :: St -> ObjId -> Int
trueRefs st o = inHeld + inLin + inWq + inReader + inPend
  where
    payRef (PShared x) = if x == o then 1 else 0
    payRef (PLinear x) = if x == o then 1 else 0
    acs = M.elems (sActors st)
    inHeld = sum [M.findWithDefault 0 o (aHeld ac) | ac <- acs]
    inLin = sum [1 | ac <- acs, x <- aLin ac, x == o]
    inReader = sum [payRef (mPay m) | ac <- acs, m <- aReader ac]
    inWq =
      sum
        [ payRef (mPay m) | wq <- M.elems (sWQ st), m <- M.elems (wqSlots wq)
        ]
    inPend = sum [payRef (mPay (pMsg pd)) | pd <- M.elems (sPend st)]

--------------------------------------------------------------------------------
-- ARC operations
--------------------------------------------------------------------------------

globalRcAdd :: Proc -> ObjId -> Int -> St -> St
globalRcAdd p o d st0 =
  let st1 = bump (\s -> s {stRcOps = stRcOps s + 1}) (touchRcLine p o st0)
   in case M.lookup o (sHeap st1) of
        Nothing -> violate ("RC op on unknown object " ++ show o) st1
        Just ob
          | not (oAlive ob) ->
              violate (printf "RC op (%+d) on FREED object %d" d o) st1
          | otherwise ->
              let rc' = oRc ob + d
                  st2 = st1 {sHeap = M.insert o ob {oRc = rc'} (sHeap st1)}
               in if rc' < 0
                    then violate (printf "NEGATIVE RC on object %d (rc=%d)" o rc') st2
                    else if rc' == 0 then doFree o st2 else st2

doFree :: ObjId -> St -> St
doFree o st0 =
  let tr = trueRefs st0 o
      st1 =
        if tr > 0
          then
            violate
              ( printf
                  "PREMATURE FREE of object %d (rc hit 0, %d live refs remain)"
                  o
                  tr
              )
              st0
          else st0
      ob = sHeap st1 M.! o
   in bump
        (\s -> s {stFrees = stFrees s + 1})
        st1
          { sHeap = M.insert o ob {oAlive = False} (sHeap st1),
            sAlive = sAlive st1 - 1
          }

deferRc :: Proc -> ObjId -> Int -> St -> St
deferRc p o d st =
  st {sDelta = M.insertWith (M.unionWith (+)) p (M.singleton o d) (sDelta st)}

rcInc, rcDec :: RcMode -> Proc -> ObjId -> St -> St
rcInc DeferredInc p o = deferRc p o 1 -- BUG: publish before inc is visible
rcInc DeferredAll p o = deferRc p o 1 -- ok: frees wait for the epoch sweep
rcInc _ p o = globalRcAdd p o 1 -- correct: inc before publish
rcDec DeferredDec p o = deferRc p o (-1) -- correct: decs may lag safely
rcDec DeferredAll p o = deferRc p o (-1)
rcDec _ p o = globalRcAdd p o (-1)

flushProc :: Proc -> St -> St
flushProc p st0 = case M.lookup p (sDelta st0) of
  Nothing -> st0
  Just ds ->
    let st1 = st0 {sDelta = M.delete p (sDelta st0)}
     in foldl' (\st (o, d) -> globalRcAdd p o d st) st1 (M.toList ds)

-- Epoch-deferred reclamation: apply every proc's buffered deltas WITHOUT
-- freeing (counts may be transiently negative mid-application while another
-- proc's matching inc is still unapplied -- that is internal bookkeeping, not
-- an error), then sweep only the touched objects: negative after full
-- application is a real bug; zero is a genuinely dead object.
epochFlush :: St -> St
epochFlush st0 =
  let touched = M.keysSet (M.unionsWith (+) (M.elems (sDelta st0)))
      applyP st p = case M.lookup p (sDelta st) of
        Nothing -> st
        Just ds ->
          foldl'
            (\s (o, d) -> epochAdd p o d s)
            st {sDelta = M.delete p (sDelta st)}
            (M.toList ds)
      st1 = foldl' applyP st0 [0 .. nProcs - 1]
      sweep st o = case M.lookup o (sHeap st) of
        Just ob
          | oAlive ob,
            oRc ob < 0 ->
              violate
                ( printf
                    "NEGATIVE RC on object %d after epoch (rc=%d)"
                    o
                    (oRc ob)
                )
                st
          | oAlive ob, oRc ob == 0 -> doFree o st
        _ -> st
   in foldl' sweep st1 (S.toList touched)

epochAdd :: Proc -> ObjId -> Int -> St -> St
epochAdd p o d st0 =
  let st1 = bump (\s -> s {stRcOps = stRcOps s + 1}) (touchRcLine p o st0)
   in case M.lookup o (sHeap st1) of
        Just ob
          | not (oAlive ob) ->
              violate (printf "RC op (%+d) on FREED object %d" d o) st1
          | otherwise ->
              st1 {sHeap = M.insert o ob {oRc = oRc ob + d} (sHeap st1)}
        Nothing -> violate ("RC op on unknown object " ++ show o) st1

flushAll :: RcMode -> St -> St
flushAll DeferredAll st = epochFlush st
flushAll _ st = foldl' (flip flushProc) st [0 .. nProcs - 1]

epochLen :: Int
epochLen = 2000

--------------------------------------------------------------------------------
-- Mailbox operations
--------------------------------------------------------------------------------

commitAppend :: Proc -> ActorId -> Msg -> Int -> St -> St
commitAppend p dst msg slot st0 =
  let st1 = touchMbLine p dst st0
      wq = sWQ st1 M.! dst
      -- overwrite semantics: if the slot is occupied (racy collision), the
      -- previous message is silently destroyed -- the checker must catch it
      wq' = WQ (max (wqTail wq) (slot + 1)) (M.insert slot msg (wqSlots wq))
   in st1 {sWQ = M.insert dst wq' (sWQ st1)}

enqueue :: Mode -> Proc -> ActorId -> Msg -> St -> St
enqueue md p dst msg st = case mbMode md of
  AtomicMb ->
    let slot = wqTail (sWQ st M.! dst) -- fetch-and-add + store, one step
     in commitAppend p dst msg slot st
  RacyMb ->
    let slot = wqTail (sWQ st M.! dst) -- step 1: snapshot only
     in st {sPend = M.insert p (PendAppend dst msg slot) (sPend st)}

-- the owner atomically takes the whole writer queue and appends it, in slot
-- order, to its private reader queue (BEAM-style swap-under-lock)
opDrain :: ActorId -> St -> St
opDrain a st0
  | M.null (wqSlots wq) = st0
  | otherwise =
      let st1 = touchMbLine (pinOf a) a st0
          msgs = M.elems (wqSlots wq) -- ascending slot order
          ac = actor a st1
          st2 = putActor a ac {aReader = aReader ac ++ msgs} st1
       in st2 {sWQ = M.insert a (WQ (wqTail wq) M.empty) (sWQ st2)}
  where
    wq = sWQ st0 M.! a

--------------------------------------------------------------------------------
-- Actor behaviors
--------------------------------------------------------------------------------

opPromote :: Proc -> ActorId -> St -> St
opPromote _ a st0
  | sAlive st0 >= aliveCap = st0
  | M.size (aHeld ac) >= heldCap = st0
  | otherwise =
      let o = sNextObj st0
          st1 =
            st0
              { sNextObj = o + 1,
                sAlive = sAlive st0 + 1,
                sHeap = M.insert o (Obj 1 True False) (sHeap st0),
                sRcOwner = M.insert o (pinOf a) (sRcOwner st0)
              }
       in bump
            (\s -> s {stPromotes = stPromotes s + 1})
            (putActor a ac {aHeld = M.insertWith (+) o 1 (aHeld ac)} st1)
  where
    ac = actor a st0

opAllocLin :: ActorId -> St -> St
opAllocLin a st0 =
  let o = sNextObj st0
      ac = actor a st0
   in putActor
        a
        ac {aLin = o : aLin ac}
        st0
          { sNextObj = o + 1,
            sHeap = M.insert o (Obj 0 True True) (sHeap st0)
          }

pickDst :: ActorId -> St -> (St, ActorId)
pickDst self st0 =
  let (st1, h) = rnd 10 st0
   in if h < 4 && self /= 0
        then (st1, 0) -- hotspot: actor 0 is popular
        else let (st2, d) = rnd kActors st1 in (st2, d)

nextSeq :: ActorId -> ActorId -> St -> (St, SeqNo)
nextSeq a dst st =
  let ac = actor a st
      q = M.findWithDefault 0 dst (aSeqOut ac) + 1
   in (putActor a ac {aSeqOut = M.insert dst q (aSeqOut ac)} st, q)

opSendShared :: Mode -> Proc -> ActorId -> St -> St
opSendShared md p a st0
  | M.null (aHeld (actor a st0)) = st0
  | otherwise =
      let held = aHeld (actor a st0)
          (st1, i) = rnd (M.size held) st0
          (o, _) = M.elemAt i held
          (st2, d) = pickDst a st1
          (st3, q) = nextSeq a d st2
          st4 = rcInc (rcMode md) p o st3 -- the in-flight msg owns +1
          st5 = enqueue md p d (Msg a q (PShared o)) st4
       in bump (\s -> s {stSendShared = stSendShared s + 1}) st5

opSendLin :: Mode -> Proc -> ActorId -> St -> St
opSendLin md p a st0 = case aLin (actor a st0) of
  [] -> st0
  (o : rest) ->
    let st1 = putActor a (actor a st0) {aLin = rest} st0
        (st2, d) = pickDst a st1
        (st3, q) = nextSeq a d st2
        st4 = enqueue md p d (Msg a q (PLinear o)) st3 -- zero RC ops
     in bump (\s -> s {stSendLin = stSendLin s + 1}) st4

checkAlive :: String -> ObjId -> St -> St
checkAlive ctx o st = case M.lookup o (sHeap st) of
  Just ob
    | not (oAlive ob) ->
        violate (printf "USE-AFTER-FREE: %s touched freed object %d" ctx o) st
  _ -> st

opReceive :: ActorId -> St -> St
opReceive a st0 = case aReader (actor a st0) of
  [] -> st0
  (Msg from q pay : rest) ->
    let ac = actor a st0
        want = M.findWithDefault 0 from (aSeqIn ac) + 1
        st1 =
          if q /= want
            then
              violate
                ( printf
                    "MAILBOX: channel %d->%d expected seq %d, got %d (lost or reordered)"
                    from
                    a
                    want
                    q
                )
                st0
            else st0
        ac1 = ac {aReader = rest, aSeqIn = M.insert from q (aSeqIn ac)}
        st2 = putActor a ac1 st1
     in case pay of
          PShared o ->
            -- adopt the in-flight count: no RC op
            let st3 = checkAlive (printf "actor %d receive" a) o st2
                ac2 = actor a st3
             in putActor a ac2 {aHeld = M.insertWith (+) o 1 (aHeld ac2)} st3
          PLinear o ->
            let st3 = checkAlive (printf "actor %d receive-linear" a) o st2
                ac2 = actor a st3
             in putActor a ac2 {aLin = o : aLin ac2} st3

opDrop :: Mode -> Proc -> ActorId -> St -> St
opDrop md p a st0 =
  let ac = actor a st0
   in case (M.null (aHeld ac), aLin ac) of
        (False, _) ->
          let (st1, i) = rnd (M.size (aHeld ac)) st0
              (o, n) = M.elemAt i (aHeld ac)
              held' =
                if n <= 1
                  then M.delete o (aHeld ac)
                  else M.insert o (n - 1) (aHeld ac)
              st2 = putActor a ac {aHeld = held'} st1
           in rcDec (rcMode md) p o st2
        (True, o : rest) ->
          -- sole owner frees directly
          let st1 = putActor a ac {aLin = rest} st0
              st2 =
                if trueRefs st1 o /= 0
                  then violate (printf "LINEAR object %d aliased at free" o) st1
                  else st1
              ob = sHeap st2 M.! o
           in bump
                (\s -> s {stLinFrees = stLinFrees s + 1})
                st2 {sHeap = M.insert o ob {oAlive = False} (sHeap st2)}
        _ -> st0

--------------------------------------------------------------------------------
-- Scheduler: one micro-step
--------------------------------------------------------------------------------

microStep :: Mode -> St -> St
microStep md st0 =
  let st = st0 {sStep = sStep st0 + 1}
      (st1, p) = rnd nProcs st
   in case M.lookup p (sPend st1) of
        Just (PendAppend dst msg slot) ->
          -- step 2 of a racy append
          commitAppend p dst msg slot st1 {sPend = M.delete p (sPend st1)}
        Nothing ->
          let acts = actorsOn p
              (st2, ai) = rnd (length acts) st1
              a = acts !! ai
              (st3, w) = rnd 100 st2
              st4
                | w < 12 = opPromote p a st3
                | w < 36 = opSendShared md p a st3
                | w < 44 = opAllocLin a st3
                | w < 54 = opSendLin md p a st3
                | w < 70 = opDrain a st3
                | w < 88 = opReceive a st3
                | otherwise = opDrop md p a st3
              (st5, f) = rnd flushEvery st4
           in case rcMode md of
                DeferredAll
                  | sStep st5 `mod` epochLen == 0 -> epochFlush st5
                  | otherwise -> st5
                _
                  | f == 0 -> flushProc p st5
                  | otherwise -> st5

--------------------------------------------------------------------------------
-- Quiesce and final checks
--------------------------------------------------------------------------------

quiesce :: Mode -> St -> St
quiesce md st0 =
  let stP =
        M.foldlWithKey'
          (\st p (PendAppend d m s) -> commitAppend p d m s st)
          st0 {sPend = M.empty}
          (sPend st0)
      drainAll st = foldl' (\s a -> recvAll a (opDrain a s)) st [0 .. kActors - 1]
      recvAll a st = case aReader (actor a st) of
        [] -> st
        _ -> recvAll a (opReceive a st)
      stD = iterate drainAll stP !! 4
      stF = flushAll (rcMode md) stD
   in finalChecks md stF

finalChecks :: Mode -> St -> St
finalChecks _ st0 = foldl' chk (foldl' chkObj st0 (M.toList (sHeap st0))) chans
  where
    chkObj st (o, ob)
      | oLinear ob =
          let tr = trueRefs st o
           in if oAlive ob && tr /= 1
                then
                  violate
                    ( printf
                        "LINEAR object %d has %d owners at quiescence (lost or aliased)"
                        o
                        tr
                    )
                    st
                else st
      | oAlive ob =
          let tr = trueRefs st o
           in if oRc ob /= tr
                then
                  violate
                    ( printf
                        "QUIESCENCE: object %d rc=%d but true refs=%d (%s)"
                        o
                        (oRc ob)
                        tr
                        ( if oRc ob > tr
                            then "leaked count -- a counted ref vanished" :: String
                            else "undercount -- free would be premature"
                        )
                    )
                    st
                else st
      | otherwise =
          let tr = trueRefs st o
           in if tr > 0
                then
                  violate
                    ( printf
                        "DANGLING: %d live refs to freed object %d"
                        tr
                        o
                    )
                    st
                else st
    chans = [(s, d) | s <- [0 .. kActors - 1], d <- [0 .. kActors - 1]]
    chk st (s, d) =
      let sent = M.findWithDefault 0 d (aSeqOut (actor s st))
          recvd = M.findWithDefault 0 s (aSeqIn (actor d st))
       in if sent /= recvd
            then
              violate
                ( printf
                    "CONSERVATION: channel %d->%d sent %d, delivered %d"
                    s
                    d
                    sent
                    recvd
                )
                st
            else st

--------------------------------------------------------------------------------
-- Runner and report
--------------------------------------------------------------------------------

runSeed :: Mode -> Word64 -> St
runSeed md seed =
  quiesce md (iterate (microStep md) (initSt seed) !! stepsPerSeed)

data Agg = Agg {agStats :: !Stats, agViol :: ![String], agBadSeeds :: !Int}

runMode :: Mode -> Agg
runMode md = foldl' go (Agg zeroStats [] 0) [1 .. fromIntegral numSeeds]
  where
    go (Agg acc vs bad) seed =
      let st = runSeed md (seed * 0x9E3779B97F4A7C15 + 0xDEADBEEF)
          s = sStats st
          a' =
            Stats
              (stSendShared acc + stSendShared s)
              (stSendLin acc + stSendLin s)
              (stRcOps acc + stRcOps s)
              (stRcXfer acc + stRcXfer s)
              (stMbXfer acc + stMbXfer s)
              (stFrees acc + stFrees s)
              (stLinFrees acc + stLinFrees s)
              (stPromotes acc + stPromotes s)
          v = reverse (sViol st)
          vs' = if length vs < 6 then vs ++ take (6 - length vs) v else vs
       in Agg a' vs' (bad + if null v then 0 else 1)

report :: Mode -> Agg -> IO ()
report md (Agg s vs bad) = do
  printf "== %-34s rc=%-12s mb=%s\n" (mName md) (show (rcMode md)) (show (mbMode md))
  printf "   seeds with violations: %d / %d\n" bad numSeeds
  printf
    "   shared sends %-8d linear sends %-8d promotes %-7d frees %d+%d(lin)\n"
    (stSendShared s)
    (stSendLin s)
    (stPromotes s)
    (stFrees s)
    (stLinFrees s)
  printf
    "   RC global ops %-8d RC line transfers %-8d (%.3f xfer/shared-send)\n"
    (stRcOps s)
    (stRcXfer s)
    (fromIntegral (stRcXfer s) / fromIntegral (max 1 (stSendShared s)) :: Double)
  printf
    "   mailbox line transfers %d   linear sends avoided ~%d RC ops\n"
    (stMbXfer s)
    (2 * stSendLin s)
  if null vs
    then printf "   VIOLATIONS: none\n"
    else do
      printf "   VIOLATIONS (first %d shown):\n" (length vs)
      mapM_ (printf "     %s\n") vs
  putStrLn ""

modes :: [Mode]
modes =
  [ Mode "1 eager RC / atomic mailbox   (ok)" EagerRc AtomicMb,
    Mode "2 deferred-dec RC / atomic    (ok)" DeferredDec AtomicMb,
    Mode "3 epoch-deferred RC / atomic  (ok)" DeferredAll AtomicMb,
    Mode "4 deferred-INC RC   (seeded bug)" DeferredInc AtomicMb,
    Mode "5 racy mailbox append (seeded bug)" EagerRc RacyMb
  ]

main :: IO ()
main = do
  printf
    "ARC + two-queue mailbox simulation: %d procs, %d actors, %d steps x %d seeds\n"
    nProcs
    kActors
    stepsPerSeed
    numSeeds
  printf "invariants: no negative RC, no premature free, no UAF, rc==true refs at\n"
  printf "quiescence, exactly-one-owner linears, per-channel FIFO + conservation\n\n"
  mapM_ (\md -> report md (runMode md)) modes
