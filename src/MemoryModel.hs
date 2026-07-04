{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- =====================================================================
-- MemModel.hs — Executable PoC of the FPR OS memory architecture
--
-- Models, end to end:
--   * Physical RAM under a global buddy allocator (no virtual memory)
--   * Per-actor slab allocators carved out of buddy blocks
--   * Statically-sized per-actor stacks with PMP-style guard trapping
--   * PMP region checks (R/W/X) on every simulated access
--   * Capability-only cross-actor references (ActorId, never raw Addr)
--   * Residency table; whole-ACB swap-out to a disk swap log that
--     grows downward toward an append-only object log growing upward
--   * Mailbox delivery to a swapped actor triggering swap-in
--   * Memory pressure -> LRU swap-out; OOM -> crash; supervisor
--     restart from last object-log checkpoint, with escalation
--
-- Everything is deterministic; the run prints a trace and a final
-- PASS/FAIL invariant summary.
-- =====================================================================

module MemoryModel where

import Control.Monad (replicateM)
import Control.Monad.State.Strict
import Data.Bits
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word64)
import GHC.Base (when)
import Text.Printf (printf)

-- ---------------------------------------------------------------------
-- Basic units
-- ---------------------------------------------------------------------

type Addr = Int -- physical byte address

type Order = Int -- buddy order; block size = 2^order bytes

type ActorId = Int

type ObjId = Int -- actor-LOCAL logical object handle

type Tick = Int

ramOrder :: Order
ramOrder = 17 -- 128 KiB of physical RAM (small on purpose)

ramSize :: Int
ramSize = bit ramOrder

minOrder :: Order
minOrder = 6 -- 64 B smallest buddy block

slabBlockOrder :: Order
slabBlockOrder = 10 -- slabs are carved from 1 KiB buddy blocks

stackOrder :: Order
stackOrder = 12 -- fixed 4 KiB stack per actor

codeOrder :: Order
codeOrder = 10 -- 1 KiB X-only code region per actor

diskSize :: Int
diskSize = 64 * 1024 -- deliberately small to force a collision

-- ---------------------------------------------------------------------
-- Buddy allocator
-- ---------------------------------------------------------------------

data Buddy = Buddy
  { bFree :: !(M.Map Order (S.Set Addr)), -- free blocks per order
    bLive :: !(M.Map Addr Order) -- allocated blocks (for audit)
  }

buddyNew :: Buddy
buddyNew = Buddy (M.singleton ramOrder (S.singleton 0)) M.empty

orderFor :: Int -> Order
orderFor n = max minOrder (go minOrder)
  where
    go o
      | bit o >= n = o
      | otherwise = go (o + 1)

buddyAlloc :: Order -> Buddy -> Maybe (Addr, Buddy)
buddyAlloc o b = do
  (o', addr) <- findFrom o
  let b1 = removeFree o' addr b
      (a2, b2) = splitDown o' addr b1
  pure (a2, b2 {bLive = M.insert a2 o (bLive b2)})
  where
    findFrom k
      | k > ramOrder = Nothing
      | otherwise = case S.minView =<< M.lookup k (bFree b) of
          Just (a, _) -> Just (k, a)
          Nothing -> findFrom (k + 1)
    removeFree k a bb =
      bb {bFree = M.adjust (S.delete a) k (bFree bb)}
    splitDown k a bb
      | k == o = (a, bb)
      | otherwise =
          let k' = k - 1
              buddy = a + bit k'
              bb' =
                bb
                  { bFree =
                      M.insertWith
                        S.union
                        k'
                        (S.singleton buddy)
                        (bFree bb)
                  }
           in splitDown k' a bb'

buddyFree :: Addr -> Order -> Buddy -> Buddy
buddyFree a0 o0 b0 = go a0 o0 b0 {bLive = M.delete a0 (bLive b0)}
  where
    go a o b
      | o < ramOrder,
        let buddy = a `xor` bit o,
        Just s <- M.lookup o (bFree b),
        buddy `S.member` s =
          go
            (min a buddy)
            (o + 1)
            b {bFree = M.adjust (S.delete buddy) o (bFree b)}
      | otherwise =
          b {bFree = M.insertWith S.union o (S.singleton a) (bFree b)}

buddyFreeBytes :: Buddy -> Int
buddyFreeBytes b = sum [bit o * S.size s | (o, s) <- M.toList (bFree b)]

buddyLiveBytes :: Buddy -> Int
buddyLiveBytes b = sum [bit o | (_, o) <- M.toList (bLive b)]

-- Audit: free blocks pairwise disjoint, disjoint from live blocks,
-- and free + live exactly tile RAM.
buddyAudit :: Buddy -> Either String ()
buddyAudit b =
  let frees = [(a, bit o) | (o, s) <- M.toList (bFree b), a <- S.toList s]
      lives = [(a, bit o) | (a, o) <- M.toList (bLive b)]
      spans = frees ++ lives
      sorted = M.toAscList (M.fromList spans)
      overlapOrGap =
        or [a + sz /= a' | ((a, sz), (a', _)) <- zip sorted (drop 1 sorted)]
      total = sum (map snd spans)
   in if length sorted /= length spans
        then Left "duplicate block base address"
        else
          if overlapOrGap
            then Left "overlap or gap in block tiling"
            else
              if total /= ramSize
                then Left "blocks do not tile RAM"
                else Right ()

-- ---------------------------------------------------------------------
-- PMP simulation
-- ---------------------------------------------------------------------

data Perm = PR | PW | PX deriving (Eq, Ord, Show)

data Region = Region {rBase :: !Addr, rSize :: !Int, rPerms :: !(S.Set Perm)}

inRegion :: Addr -> Region -> Bool
inRegion a r = a >= rBase r && a < rBase r + rSize r

-- An access by the currently running actor against its PMP region set.
pmpCheck :: [Region] -> Addr -> Perm -> Either String ()
pmpCheck regs a p =
  case [r | r <- regs, inRegion a r] of
    (r : _)
      | p `S.member` rPerms r -> Right ()
      | otherwise -> Left (printf "PMP trap: %s denied at 0x%05x" (show p) a)
    [] -> Left (printf "PMP trap: no region covers 0x%05x (%s)" a (show p))

-- ---------------------------------------------------------------------
-- Slab allocator (actor-local)
-- ---------------------------------------------------------------------

data Slab = Slab
  { slObjSize :: !Int,
    slBlocks :: ![Addr], -- buddy blocks (each slabBlockOrder) owned
    slFree :: ![Addr] -- free object slots
  }
  deriving (Show)

slabCarve :: Int -> Addr -> [Addr]
slabCarve objSz base = [base + i * objSz | i <- [0 .. bit slabBlockOrder `div` objSz - 1]]

-- ---------------------------------------------------------------------
-- Actors
-- ---------------------------------------------------------------------

data Mailbox = Mailbox {mbOuter :: ![Msg], mbInner :: ![Msg]} deriving (Show)

data Msg = Msg {msgFrom :: !ActorId, msgPayload :: !Word64} deriving (Show)

data ACB = ACB
  { acbId :: !ActorId,
    acbCode :: !(Addr, Order),
    acbStack :: !(Addr, Order),
    acbStackUsed :: !Int,
    acbSlabs :: !(M.Map Int Slab), -- size class -> slab
    acbObjTable :: !(M.Map ObjId (Int, Addr)), -- handle -> (class, addr)
    acbObjData :: !(M.Map ObjId Word64), -- logical contents
    acbNextObj :: !ObjId,
    acbMailbox :: !Mailbox,
    acbDurable :: !Word64 -- checkpointed to object log
  }

-- The checksum an ACB must preserve across a swap round-trip.
acbChecksum :: ACB -> Word64
acbChecksum a =
  foldl'
    xor
    (fromIntegral (acbStackUsed a) + acbDurable a)
    (M.elems (acbObjData a))

acbResidentBytes :: ACB -> Int
acbResidentBytes a =
  bit (snd (acbCode a))
    + bit (snd (acbStack a))
    + sum [length (slBlocks s) * bit slabBlockOrder | s <- M.elems (acbSlabs a)]

pmpRegions :: ACB -> [Region]
pmpRegions a =
  Region (fst (acbCode a)) (bit codeOrder) (S.fromList [PR, PX])
    : Region (fst (acbStack a)) (bit stackOrder) (S.fromList [PR, PW])
    : [ Region blk (bit slabBlockOrder) (S.fromList [PR, PW])
        | s <- M.elems (acbSlabs a),
          blk <- slBlocks s
      ]

-- NOTE: no region grants PX over stack or slabs (W^X), and nothing
-- covers other actors' memory: any stray address traps.

-- ---------------------------------------------------------------------
-- Disk: object log grows UP from 0, swap log grows DOWN from diskSize
-- ---------------------------------------------------------------------

data ObjLogEntry = Checkpoint !ActorId !Word64 | AppData !Int
  deriving (Show)

data SwapRecord = SwapRecord
  { srActor :: !ActorId,
    srBytes :: !Int, -- serialized size (== resident bytes)
    srStackU :: !Int,
    srSlabDims :: !(M.Map Int Int), -- size class -> block count
    srObjs :: !(M.Map ObjId (Int, Word64)), -- handle -> (class, contents)
    srNextObj :: !ObjId,
    srMailbox :: !Mailbox,
    srDurable :: !Word64,
    srChecksum :: !Word64
  }

data Disk = Disk
  { dObjTop :: !Int, -- next free byte, object log
    dObjEntries :: ![(Int, ObjLogEntry, Bool)], -- (offset, entry, live?)
    dSwapBot :: !Int, -- lowest used byte, swap log
    dSwapRecs :: !(M.Map Int SwapRecord) -- offset -> record
  }

diskNew :: Disk
diskNew = Disk 0 [] diskSize M.empty

objLogAppend :: Int -> ObjLogEntry -> Disk -> Either String Disk
objLogAppend sz e d
  | dObjTop d + sz > dSwapBot d = Left "DISK FULL (object log met swap log)"
  | otherwise =
      Right
        d
          { dObjTop = dObjTop d + sz,
            dObjEntries = (dObjTop d, e, True) : dObjEntries d
          }

swapLogAppend :: SwapRecord -> Disk -> Either String Disk
swapLogAppend r d
  | dSwapBot d - srBytes r < dObjTop d = Left "DISK FULL (swap log met object log)"
  | otherwise =
      let off = dSwapBot d - srBytes r
       in Right d {dSwapBot = off, dSwapRecs = M.insert off r (dSwapRecs d)}

-- Object-log GC: drop entries marked dead, compact live ones to bottom.
objLogGC :: Disk -> Disk
objLogGC d =
  let live =
        [ (e, sz) | (off, e, alive) <- reverse (dObjEntries d), alive, let sz = entrySize (off, e)
        ]
      entrySize (_, Checkpoint _ _) = 64
      entrySize (_, AppData n) = n
      (top', entries') = foldl' step (0, []) live
      step (t, es) (e, sz) = (t + sz, (t, e, True) : es)
   in d {dObjTop = top', dObjEntries = entries'}

-- ---------------------------------------------------------------------
-- System state
-- ---------------------------------------------------------------------

data Residency
  = Resident !ACB
  | Swapped !Int -- disk offset
  | Failed -- supervisor gave up

data Sys = Sys
  { sBuddy :: !Buddy,
    sDisk :: !Disk,
    sResidency :: !(M.Map ActorId Residency),
    sLastActive :: !(M.Map ActorId Tick),
    sRestarts :: !(M.Map ActorId Int),
    sTick :: !Tick,
    sTrace :: ![String], -- reversed
    sChecks :: ![(String, Bool)] -- reversed
  }

type Sim = State Sys

restartLimit :: Int
restartLimit = 3

say :: String -> Sim ()
say s = modify' $ \st -> st {sTrace = s : sTrace st}

check :: String -> Bool -> Sim ()
check name ok = do
  modify' $ \st -> st {sChecks = (name, ok) : sChecks st}
  say $ (if ok then "  [ok]   " else "  [FAIL] ") ++ name

tickle :: ActorId -> Sim ()
tickle aid = modify' $ \st ->
  st
    { sTick = sTick st + 1,
      sLastActive = M.insert aid (sTick st + 1) (sLastActive st)
    }

-- ---------------------------------------------------------------------
-- Allocation under memory pressure
-- ---------------------------------------------------------------------

-- Try a buddy alloc; on failure, swap out the LRU resident actor
-- (other than the requester) and retry. Nothing left to evict => OOM.
allocWithPressure :: ActorId -> Order -> Sim (Maybe Addr)
allocWithPressure self o = do
  st <- get
  case buddyAlloc o (sBuddy st) of
    Just (a, b') -> do put st {sBuddy = b'}; pure (Just a)
    Nothing ->
      pickVictim self >>= \case
        Nothing -> pure Nothing
        Just vid -> do
          say $ printf "  memory pressure: swapping out actor %d (LRU)" vid
          okSwap <- swapOut vid
          if okSwap then allocWithPressure self o else pure Nothing

pickVictim :: ActorId -> Sim (Maybe ActorId)
pickVictim self = do
  st <- get
  let cands =
        [ (M.findWithDefault 0 aid (sLastActive st), aid)
          | (aid, Resident _) <- M.toList (sResidency st),
            aid /= self
        ]
  pure $ case cands of
    [] -> Nothing
    xs -> Just (snd (minimum xs))

-- ---------------------------------------------------------------------
-- Actor lifecycle
-- ---------------------------------------------------------------------

spawn :: ActorId -> Sim Bool
spawn aid = do
  mc <- allocWithPressure aid codeOrder
  ms <- allocWithPressure aid stackOrder
  case (mc, ms) of
    (Just c, Just s) -> do
      let acb =
            ACB
              aid
              (c, codeOrder)
              (s, stackOrder)
              0
              M.empty
              M.empty
              M.empty
              0
              (Mailbox [] [])
              0
      modify' $ \st -> st {sResidency = M.insert aid (Resident acb) (sResidency st)}
      tickle aid
      say $ printf "spawned actor %d (code@0x%05x, stack@0x%05x)" aid c s
      pure True
    _ -> do
      mapM_ (\a -> modify' $ \st -> st {sBuddy = buddyFree a codeOrder (sBuddy st)}) mc
      mapM_ (\a -> modify' $ \st -> st {sBuddy = buddyFree a stackOrder (sBuddy st)}) ms
      say $ printf "spawn of actor %d FAILED (OOM)" aid
      pure False

withResident :: ActorId -> (ACB -> Sim (Maybe ACB)) -> Sim Bool
withResident aid f = do
  st <- get
  case M.lookup aid (sResidency st) of
    Just (Resident a) ->
      f a >>= \case
        Just a' -> do
          modify' $ \s2 -> s2 {sResidency = M.insert aid (Resident a') (sResidency s2)}
          pure True
        Nothing -> pure False
    _ -> pure False

-- Allocate one logical object of a size class inside an actor's slab,
-- swapping in first if needed; PMP-checks the write to the slot.
actorAllocObj :: ActorId -> Int -> Word64 -> Sim Bool
actorAllocObj aid cls v = do
  ensureResident aid
  tickle aid
  withResident aid $ \acb -> do
    let slab = M.findWithDefault (Slab cls [] []) cls (acbSlabs acb)
    (slab', mAddr) <- case slFree slab of
      (a : rest) -> pure (slab {slFree = rest}, Just a)
      [] ->
        allocWithPressure aid slabBlockOrder >>= \case
          Nothing -> pure (slab, Nothing)
          Just blk -> do
            let (slot : rest) = slabCarve cls blk
            pure (slab {slBlocks = blk : slBlocks slab, slFree = rest}, Just slot)
    case mAddr of
      Nothing -> do
        say $ printf "  actor %d: OOM allocating class-%d object" aid cls
        oomCrash aid
        pure Nothing
      Just addr -> do
        let acb1 = acb {acbSlabs = M.insert cls slab' (acbSlabs acb)}
        case pmpCheck (pmpRegions acb1) addr PW of
          Left err -> do say ("  " ++ err); pure Nothing
          Right () -> do
            let oid = acbNextObj acb1
                acb2 =
                  acb1
                    { acbObjTable = M.insert oid (cls, addr) (acbObjTable acb1),
                      acbObjData = M.insert oid v (acbObjData acb1),
                      acbNextObj = oid + 1
                    }
            pure (Just acb2)

-- Push n bytes onto the actor's stack, PMP-checking the touched address.
actorPush :: ActorId -> Int -> Sim (Either String ())
actorPush aid n = do
  ensureResident aid
  tickle aid
  st <- get
  case M.lookup aid (sResidency st) of
    Just (Resident acb) -> do
      let (base, _) = acbStack acb
          touched = base + acbStackUsed acb + n - 1
      case pmpCheck (pmpRegions acb) touched PW of
        Left err -> do
          say $ printf "  actor %d stack overflow -> %s" aid err
          pure (Left err)
        Right () -> do
          let acb' = acb {acbStackUsed = acbStackUsed acb + n}
          modify' $ \s2 -> s2 {sResidency = M.insert aid (Resident acb') (sResidency s2)}
          pure (Right ())
    _ -> pure (Left "actor not available")

-- ---------------------------------------------------------------------
-- Swap out / swap in
-- ---------------------------------------------------------------------

freeActorBlocks :: ACB -> Sim ()
freeActorBlocks acb = modify' $ \st ->
  let b0 = sBuddy st
      b1 = buddyFree (fst (acbCode acb)) codeOrder b0
      b2 = buddyFree (fst (acbStack acb)) stackOrder b1
      b3 =
        foldl'
          (\b blk -> buddyFree blk slabBlockOrder b)
          b2
          [blk | s <- M.elems (acbSlabs acb), blk <- slBlocks s]
   in st {sBuddy = b3}

swapOut :: ActorId -> Sim Bool
swapOut aid = do
  st <- get
  case M.lookup aid (sResidency st) of
    Just (Resident acb) -> do
      let rec =
            SwapRecord
              { srActor = aid,
                srBytes = acbResidentBytes acb,
                srStackU = acbStackUsed acb,
                srSlabDims = M.map (length . slBlocks) (acbSlabs acb),
                srObjs =
                  M.intersectionWith
                    (\(c, _) v -> (c, v))
                    (acbObjTable acb)
                    (acbObjData acb),
                srNextObj = acbNextObj acb,
                srMailbox = acbMailbox acb,
                srDurable = acbDurable acb,
                srChecksum = acbChecksum acb
              }
      case swapLogAppend rec (sDisk st) of
        Left err -> do say ("  swap-out failed: " ++ err); pure False
        Right d' -> do
          let off = dSwapBot d'
          freeActorBlocks acb
          modify' $ \s2 ->
            s2
              { sDisk = d',
                sResidency = M.insert aid (Swapped off) (sResidency s2)
              }
          say $ printf "  actor %d swapped out (%d B -> disk@%d)" aid (srBytes rec) off
          pure True
    _ -> pure False

-- Swap-in: fresh blocks from the buddy allocator (addresses WILL differ);
-- the actor-local object table is rebuilt, modeling that all internal
-- references are handles rebased on reload, never raw absolute pointers.
swapIn :: ActorId -> Sim Bool
swapIn aid = do
  st <- get
  case M.lookup aid (sResidency st) of
    Just (Swapped off) | Just rec <- M.lookup off (dSwapRecs (sDisk st)) -> do
      mc <- allocWithPressure aid codeOrder
      ms <- allocWithPressure aid stackOrder
      case (mc, ms) of
        (Just c, Just s) -> do
          slabsM <- rebuildSlabs aid (srSlabDims rec) (srObjs rec)
          case slabsM of
            Nothing -> do say "  swap-in failed: OOM"; pure False
            Just (slabs, objTable) -> do
              let acb =
                    ACB
                      aid
                      (c, codeOrder)
                      (s, stackOrder)
                      (srStackU rec)
                      slabs
                      objTable
                      (M.map snd (srObjs rec))
                      (srNextObj rec)
                      (srMailbox rec)
                      (srDurable rec)
              modify' $ \s2 ->
                s2
                  { sResidency = M.insert aid (Resident acb) (sResidency s2),
                    sDisk = (sDisk s2) {dSwapRecs = M.delete off (dSwapRecs (sDisk s2))}
                  }
              check
                (printf "actor %d checksum preserved across swap" aid)
                (acbChecksum acb == srChecksum rec)
              say $ printf "  actor %d swapped in (relocated: code@0x%05x)" aid c
              pure True
        _ -> do say "  swap-in failed: OOM on code/stack"; pure False
    _ -> pure False

rebuildSlabs ::
  ActorId ->
  M.Map Int Int ->
  M.Map ObjId (Int, Word64) ->
  Sim (Maybe (M.Map Int Slab, M.Map ObjId (Int, Addr)))
rebuildSlabs aid dims objs = go (M.toList dims) M.empty
  where
    go [] slabs = pure (Just (slabs, assign slabs))
    go ((cls, n) : rest) slabs = do
      blks <- replicateM n (allocWithPressure aid slabBlockOrder)
      if any (== Nothing) blks
        then pure Nothing
        else
          let bs = [b | Just b <- blks]
              free = concatMap (slabCarve cls) bs
           in go rest (M.insert cls (Slab cls bs free) slabs)
    -- deterministically re-place each object into a fresh slot
    assign slabs =
      let perClass =
            M.fromListWith
              (++)
              [(cls, [(oid, ())]) | (oid, (cls, _)) <- M.toAscList objs]
       in M.fromList
            [ (oid, (cls, slot))
              | (cls, members) <- M.toList perClass,
                let slots = slFree (slabs M.! cls),
                ((oid, _), slot) <- zip (reverse members) slots
            ]

ensureResident :: ActorId -> Sim ()
ensureResident aid = do
  st <- get
  case M.lookup aid (sResidency st) of
    Just (Swapped _) -> do
      say $ printf "  actor %d not resident; paging in" aid
      _ <- swapIn aid
      pure ()
    _ -> pure ()

-- ---------------------------------------------------------------------
-- Messages (capabilities: sender knows only the ActorId)
-- ---------------------------------------------------------------------

sendMsg :: ActorId -> ActorId -> Word64 -> Sim ()
sendMsg from to payload = do
  say $ printf "actor %d -> actor %d : msg %d" from to payload
  ensureResident to -- delivery to a swapped actor triggers swap-in
  _ <- withResident to $ \acb ->
    pure
      ( Just
          acb
            { acbMailbox =
                (acbMailbox acb)
                  { mbOuter = Msg from payload : mbOuter (acbMailbox acb)
                  }
            }
      )
  -- receiving actor processes: moves outer->inner, allocs an object
  _ <- withResident to $ \acb ->
    let mb = acbMailbox acb
     in pure (Just acb {acbMailbox = Mailbox [] (mbOuter mb ++ mbInner mb)})
  _ <- actorAllocObj to 64 payload
  pure ()

-- ---------------------------------------------------------------------
-- Checkpoints, OOM crash, supervisor
-- ---------------------------------------------------------------------

checkpoint :: ActorId -> Sim ()
checkpoint aid = do
  ensureResident aid
  st <- get
  case M.lookup aid (sResidency st) of
    Just (Resident acb) ->
      case objLogAppend 64 (Checkpoint aid (acbDurable acb)) (sDisk st) of
        Left err -> say ("  checkpoint failed: " ++ err)
        Right d' -> do
          modify' $ \s2 -> s2 {sDisk = d'}
          say $ printf "  actor %d checkpointed durable=%d to object log" aid (acbDurable acb)
    _ -> pure ()

lastCheckpoint :: ActorId -> Sim (Maybe Word64)
lastCheckpoint aid = do
  d <- gets sDisk
  pure $ case [v | (_, Checkpoint a v, True) <- dObjEntries d, a == aid] of
    (v : _) -> Just v
    [] -> Nothing

-- Normal exit: an actor terminates and returns every block it owns.
-- (Also the cheap remedy for a pathologically fragmented actor.)
exitActor :: ActorId -> Sim ()
exitActor aid = do
  st <- get
  case M.lookup aid (sResidency st) of
    Just (Resident acb) -> do
      freeActorBlocks acb
      modify' $ \s2 -> s2 {sResidency = M.delete aid (sResidency s2)}
      say $ printf "actor %d exited; %d B returned to buddy" aid (acbResidentBytes acb)
    _ -> pure ()

oomCrash :: ActorId -> Sim ()
oomCrash aid = do
  say $ printf "  !! actor %d OOM-crashed" aid
  st <- get
  case M.lookup aid (sResidency st) of
    Just (Resident acb) -> do
      freeActorBlocks acb
      modify' $ \s2 -> s2 {sResidency = M.delete aid (sResidency s2)}
      superviseRestart aid
    _ -> pure ()

superviseRestart :: ActorId -> Sim ()
superviseRestart aid = do
  n <- gets (M.findWithDefault 0 aid . sRestarts)
  if n >= restartLimit
    then do
      modify' $ \st -> st {sResidency = M.insert aid Failed (sResidency st)}
      say $
        printf
          "  supervisor: actor %d exceeded %d restarts -> ESCALATE (marked Failed)"
          aid
          restartLimit
    else do
      modify' $ \st -> st {sRestarts = M.insertWith (+) aid 1 (sRestarts st)}
      ok <- spawn aid
      when ok $ do
        cp <- lastCheckpoint aid
        case cp of
          Just v -> do
            _ <- withResident aid (\acb -> pure (Just acb {acbDurable = v}))
            say $ printf "  supervisor: actor %d restarted from checkpoint durable=%d" aid v
          Nothing ->
            say $ printf "  supervisor: actor %d restarted fresh (no checkpoint)" aid

-- ---------------------------------------------------------------------
-- Scenarios
-- ---------------------------------------------------------------------

memHeadline :: Sim ()
memHeadline = do
  b <- gets sBuddy
  say $ printf "  [ram: %d B live / %d B free]" (buddyLiveBytes b) (buddyFreeBytes b)

residentBytesTotal :: Sim Int
residentBytesTotal = do
  st <- get
  pure $ sum [acbResidentBytes a | Resident a <- M.elems (sResidency st)]

scenario :: Sim ()
scenario = do
  say "=== T1: buddy alloc/free + coalescing audit ==="
  do
    st <- get
    let Just (a1, b1) = buddyAlloc 8 (sBuddy st)
        Just (a2, b2) = buddyAlloc 10 b1
        Just (a3, b3) = buddyAlloc 8 b2
        b4 = buddyFree a1 8 b3
        b5 = buddyFree a3 8 b4
        b6 = buddyFree a2 10 b5
    check "buddy audit mid-flight" (buddyAudit b3 == Right ())
    check
      "buddy full coalesce back to one block"
      ( M.lookup ramOrder (bFree b6) == Just (S.singleton 0)
          && buddyAudit b6 == Right ()
      )

  say ""
  say "=== T2: spawn actors, slab allocation, internal fragmentation ==="
  mapM_ spawn [1, 2, 3]
  -- actor 1 allocates 40 x 48B objects into 64B class: 25% internal frag
  mapM_ (\i -> actorAllocObj 1 64 (fromIntegral i)) [1 .. 40 :: Int]
  memHeadline
  do
    st <- get
    let Resident a1 = sResidency st M.! 1
        blocks = sum [length (slBlocks s) | s <- M.elems (acbSlabs a1)]
    check
      "slab used ceil(40/16)=3 blocks of 1KiB for 40 64B-class objs"
      (blocks == 3)

  say ""
  say "=== T3: PMP — W^X, cross-actor write, stack guard ==="
  do
    st <- get
    let Resident a1 = sResidency st M.! 1
        Resident a2 = sResidency st M.! 2
        regs1 = pmpRegions a1
    check
      "exec from own code region allowed"
      (pmpCheck regs1 (fst (acbCode a1)) PX == Right ())
    check
      "exec from own DATA (slab) trapped (W^X)"
      ( case [blk | s <- M.elems (acbSlabs a1), blk <- slBlocks s] of
          (blk : _) -> pmpCheck regs1 blk PX /= Right ()
          [] -> False
      )
    check
      "write into ANOTHER actor's stack trapped"
      (pmpCheck regs1 (fst (acbStack a2)) PW /= Right ())
    check
      "write to own code region trapped (code is RX)"
      (pmpCheck regs1 (fst (acbCode a1)) PW /= Right ())
  r <- actorPush 2 (bit stackOrder - 8) -- nearly fill the 4 KiB stack
  check "large push within fixed stack succeeds" (r == Right ())
  r2 <- actorPush 2 64 -- crosses the block boundary
  check "next push traps at PMP guard (stack overflow)" (r2 /= Right ())

  say ""
  say "=== T4: memory pressure -> LRU swap-out; message wakes swapped actor ==="
  _ <- withResident 1 (\a -> pure (Just a {acbDurable = 7777}))
  checkpoint 1
  -- Actor 3 allocates until at least one other actor must be evicted:
  -- 460 x 256B objects = 115 slab blocks = 117760 B > 112640 B free.
  mapM_ (\i -> actorAllocObj 3 256 (fromIntegral i)) [1 .. 460 :: Int]
  memHeadline
  do
    st <- get
    let swappedIds = [aid | (aid, Swapped _) <- M.toList (sResidency st)]
    check
      "at least one actor was swapped out under pressure"
      (not (null swappedIds))
  rb <- residentBytesTotal
  b <- gets sBuddy
  check
    "resident actor bytes == buddy live bytes (accounting agrees)"
    (rb == buddyLiveBytes b)
  check "resident set fits in RAM" (rb <= ramSize)
  -- message to (likely swapped) actor 1: must page it in and deliver
  sendMsg 3 1 42
  do
    st <- get
    case M.lookup 1 (sResidency st) of
      Just (Resident a1) -> do
        check "actor 1 resident again after message" True
        check
          "actor 1 received the message (obj holds payload 42)"
          (42 `elem` M.elems (acbObjData a1))
        check
          "actor 1 durable state survived swap round-trip"
          (acbDurable a1 == 7777)
      _ -> check "actor 1 resident again after message" False
  -- Actor 3 is now larger than the entire disk: unevictable. The only
  -- remedies in this design are exit/restart -- exercise normal exit.
  exitActor 3
  b4 <- gets sBuddy
  check
    "whale actor exit returns memory (audit clean)"
    (buddyAudit b4 == Right ())

  say ""
  say "=== T5: single actor exceeds RAM -> OOM crash -> supervised restart ==="
  _ <- spawn 9
  _ <- withResident 9 (\a -> pure (Just a {acbDurable = 123}))
  checkpoint 9
  -- 9 tries to allocate more than all of RAM; victims run out; it crashes.
  -- Supervisor restarts it; we do it repeatedly to trip escalation.
  let hog = mapM_ (\i -> actorAllocObj 9 256 (fromIntegral i)) [1 .. 2000 :: Int]
  hog
  do
    st <- get
    check "actor 9 crashed at least once" (M.findWithDefault 0 9 (sRestarts st) >= 1)
  hog
  hog
  hog
  do
    st <- get
    check
      "supervisor escalated after restart limit"
      (case M.lookup 9 (sResidency st) of Just Failed -> True; _ -> False)
  b2 <- gets sBuddy
  check "buddy audit clean after crash storms" (buddyAudit b2 == Right ())

  say ""
  say "=== T6: disk pressure — swap log meets object log; GC recovers ==="
  _ <- spawn 4 -- small, fresh actor: code + stack = 5120 B record
  -- Fill the object log with app data until near collision
  let fill = do
        d <- gets sDisk
        case objLogAppend 4096 (AppData 4096) d of
          Right d' -> do modify' (\s2 -> s2 {sDisk = d'}); fill
          Left _ -> pure ()
  fill
  d0 <- gets sDisk
  say $ printf "  object log top = %d, swap bottom = %d" (dObjTop d0) (dSwapBot d0)
  ok <- swapOut 4
  check "swap-out correctly refused when disk is full" (not ok)
  -- Mark 60%% of object-log entries dead and GC
  modify' $ \st ->
    let d = sDisk st
        es = dObjEntries d
        n = length es
        marked =
          [ (o, e, if i < (6 * n) `div` 10 then False else alive)
            | (i, (o, e, alive)) <- zip [0 :: Int ..] es
          ]
     in st {sDisk = objLogGC d {dObjEntries = marked}}
  d1 <- gets sDisk
  say $ printf "  after GC: object log top = %d, swap bottom = %d" (dObjTop d1) (dSwapBot d1)
  ok2 <- swapOut 4
  check "swap-out succeeds after object-log GC" ok2

  say ""
  b3 <- gets sBuddy
  check "final buddy audit clean" (buddyAudit b3 == Right ())

-- ---------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------

main :: IO ()
main = do
  let st0 = Sys buddyNew diskNew M.empty M.empty M.empty 0 [] []
      st1 = execState scenario st0
  mapM_ putStrLn (reverse (sTrace st1))
  let checks = reverse (sChecks st1)
      passed = length (filter snd checks)
  putStrLn ""
  putStrLn "======================================================"
  printf "RESULT: %d / %d invariant checks passed\n" passed (length checks)
  mapM_
    (\(n, ok) -> putStrLn ((if ok then "  PASS  " else "  FAIL  ") ++ n))
    (filter (not . snd) checks)
  if passed == length checks
    then putStrLn "ALL CHECKS PASSED"
    else putStrLn "SOME CHECKS FAILED"
