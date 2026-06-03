{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Eta reduce" #-}

module Lib where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM, forM_, forever, unless)
import Data.Bits (xor)
import Data.Char (ord)
import Data.IORef
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)

data DoorState = Open | Closed

data Door (s :: DoorState) = Door {doorId :: Int}

openDoor :: Door Closed -> Door Open
openDoor s@(Door {doorId = d}) = s {doorId = d + 1}

closeDoor :: Door Open -> Door Closed
closeDoor s = (s {doorId = 1}) :: Door Closed

-- ============================================================
--  Blockchain Node Simulation
--
--  Demonstrates:
--    * Explicit node state machine  (Synced | Syncing | Partitioned)
--    * Typed protocol messages      (Gossip / Sync / Liveness)
--    * STM-based message fabric     (per-node TQueue inbox)
--    * Partition + slow-node sim    (PartitionSet, random delay)
--    * Pure update + IO effects     (handleMsg returns [Effect])
--    * Bidirectional gossip         (announce -> want -> sendBlock)
--    * Sync catch-up                (SyncRequest / SyncResponse)
-- ============================================================

-- ============================================================
--  Mini hash (FNV-1a, 64-bit) -- no external deps
-- ============================================================

type Hash = Word64

fnv1a :: String -> Hash
fnv1a = foldl step 14695981039346656037
  where
    step acc c = (acc `xor` fromIntegral (ord c)) * 1099511628211

showHash :: Hash -> String
showHash h = take 8 $ showHex h
  where
    showHex 0 = "0000000000000000"
    showHex n = go n ""
    go 0 acc = acc
    go n acc =
      let (q, r) = n `divMod` 16
          d = "0123456789abcdef" !! fromIntegral r
       in go q (d : acc)

-- ============================================================
--  Mini LCG -- avoids System.Random dependency
-- ============================================================

type RNG = IORef Word64

newRNG :: Word64 -> IO RNG
newRNG seed = newIORef seed

nextRNG :: RNG -> IO Word64
nextRNG ref = do
  s <- readIORef ref
  let s' = s * 6364136223846793005 + 1442695040888963407
  writeIORef ref s'
  return s'

randomRangeIO :: RNG -> Int -> Int -> IO Int
randomRangeIO rng lo hi = do
  r <- nextRNG rng
  return $ lo + fromIntegral (r `mod` fromIntegral (hi - lo + 1))

-- ============================================================
--  Block
-- ============================================================

type NodeId = String

type Height = Int

data Block = Block {blockHeight :: Height, blockPrev :: Hash, blockPayload :: String, blockHash :: Hash} deriving (Eq)

instance Show Block where
  show b = "[#" <> show (blockHeight b) <> " " <> showHash (blockHash b) <> " payload:" <> blockPayload b <> "]"

-- Smart constructor: hash is always derived
mkBlock :: Height -> Hash -> String -> Block
mkBlock h prev payload =
  let raw = show h <> show prev <> payload
      bh = fnv1a raw
   in Block h prev payload bh

genesisBlock :: Block
genesisBlock = mkBlock 0 0 "genesis"

-- ============================================================
--  Protocol messages
--
--  Two binary sessions:
--
--  Gossip  :  Announce height hash
--          -> Want height         (or Ignore)
--          -> SendBlock block
--
--  Sync    :  SyncRequest fromHeight
--          -> SyncResponse [Block]
--
--  The dual of every Send is a Recv on the other side.
--  Structural pattern matching enforces protocol shape.
-- ============================================================

data Msg
  = Announce NodeId Height Hash -- "I have a block at this height"
  | Want NodeId Height -- "please send it"
  | Ignore -- "already have it"
  | SendBlock Block -- the block itself
  | SyncRequest NodeId Height -- "send me blocks above N"
  | SyncResponse [Block] -- ordered oldest-first
  | Ping NodeId
  | Pong NodeId
  deriving (Show)

-- ============================================================
--  Node state machine
-- ============================================================

data SyncStatus = Synced | Syncing | Partitioned deriving (Show, Eq)

-- nsChain: newest first
data NodeState = NodeState {nsId :: NodeId, nsChain :: [Block], nsSyncStatus :: SyncStatus, nsPeers :: [NodeId], nsSeenHashes :: Map Hash ()}

tip :: NodeState -> Block
tip = head . nsChain

chainHeight :: NodeState -> Height
chainHeight = blockHeight . tip

-- ============================================================
--  Network fabric
-- ============================================================

type Inbox = TQueue Msg

type Network = Map NodeId Inbox

type PartitionSet = TVar [(NodeId, NodeId)]

newNetwork :: [NodeId] -> STM Network
newNetwork ids = Map.fromList <$> forM ids (\nid -> (nid,) <$> newTQueue)

isPartitioned :: PartitionSet -> NodeId -> NodeId -> STM Bool
isPartitioned ps src dst = do
  parts <- readTVar ps
  return $ (src, dst) `elem` parts || (dst, src) `elem` parts

deliver :: RNG -> Network -> PartitionSet -> NodeId -> NodeId -> Msg -> IO ()
deliver rng net ps src dst msg = do
  blocked <- atomically $ isPartitioned ps src dst
  unless blocked $ do
    delay <- randomRangeIO rng 0 30000 -- 0-30ms jitter
    threadDelay delay
    atomically $ case Map.lookup dst net of
      Nothing -> return ()
      Just q -> writeTQueue q msg

-- ============================================================
--  Pure message handler  ->  (newState, [Effect])
--
--  This is the heart of the simulation. Each constructor
--  handles exactly the messages valid in its protocol phase.
-- ============================================================

data Effect = Send NodeId Msg | Log String deriving (Show)

handleMsg :: NodeState -> Msg -> (NodeState, [Effect])
-- -- Gossip: peer announces a block --------------------------
handleMsg ns (Announce sender h bh)
  | Map.member bh (nsSeenHashes ns) = (ns, [Send sender Ignore])
  | h <= chainHeight ns = (ns, [Send sender Ignore])
  | h == chainHeight ns + 1 = (ns {nsSeenHashes = Map.insert bh () (nsSeenHashes ns)}, [Log $ nsId ns <> " wants #" <> show h <> " from " <> sender, Send sender (Want (nsId ns) h)])
  -- Gap detected -- need full sync
  | otherwise = (ns {nsSyncStatus = Syncing}, [Log $ nsId ns <> " behind (have #" <> show (chainHeight ns) <> ", peer has #" <> show h <> ") triggering sync with " <> sender, Send sender (SyncRequest (nsId ns) (chainHeight ns))])
-- -- Gossip: peer wants a block we announced -----------------
handleMsg ns (Want sender h) =
  case findBlock h (nsChain ns) of
    Just blk -> (ns, [Send sender (SendBlock blk)])
    Nothing -> (ns, [Log $ nsId ns <> " can't serve #" <> show h])
handleMsg ns Ignore = (ns, [])
-- -- Gossip: receiving a block --------------------------------
handleMsg ns (SendBlock blk)
  | not (validExtension blk (tip ns)) = (ns, [Log $ nsId ns <> " REJECTED " <> show blk <> " (invalid extension of #" <> show (chainHeight ns) <> ")"])
  | otherwise =
      let ns' = appendBlock ns blk
          propagate = [Send p (Announce (nsId ns') (blockHeight blk) (blockHash blk)) | p <- nsPeers ns']
       in (ns', Log (nsId ns' <> " appended " <> show blk) : propagate)
-- -- Sync: serve blocks above requested height ---------------
handleMsg ns (SyncRequest sender fromH) =
  let blocks = filter (\b -> blockHeight b > fromH) (nsChain ns)
      ordered = reverse blocks
   in (ns, [Log $ nsId ns <> " serving sync to " <> sender <> " (" <> show (length ordered) <> " blocks)", Send sender (SyncResponse ordered)])
-- -- Sync: receive and apply blocks --------------------------
handleMsg ns (SyncResponse blocks) =
  let validBlocks = takeWhile id $ zipWith validNext (nsChain ns) blocks
      -- fold in valid blocks one at a time
      ns' = foldl appendBlock ns (filter (isValidNext ns) blocks)
      ns'' = ns' {nsSyncStatus = Synced}
      effects = [Log $ nsId ns'' <> " sync complete, now at #" <> show (chainHeight ns'')] <> [Send p (Announce (nsId ns'') (chainHeight ns'') (blockHash (tip ns''))) | p <- nsPeers ns'']
   in (ns'', effects)
  where
    isValidNext s b = blockHeight b == chainHeight s + 1 && validExtension b (tip s)
    validNext _ _ = True -- unused, keeping for clarity

-- -- Liveness ------------------------------------------------
handleMsg ns (Ping sender) = (ns, [Send sender (Pong (nsId ns))])
handleMsg ns (Pong sender) = (ns {nsPeers = nub (sender : nsPeers ns)}, [Log $ nsId ns <> " confirmed peer " <> sender])

-- ============================================================
--  Chain helpers
-- ============================================================

findBlock :: Height -> [Block] -> Maybe Block
findBlock h = foldr (\b acc -> if blockHeight b == h then Just b else acc) Nothing

validExtension :: Block -> Block -> Bool
validExtension new prev = blockHeight new == blockHeight prev + 1 && blockPrev new == blockHash prev

appendBlock :: NodeState -> Block -> NodeState
appendBlock ns blk = ns {nsChain = blk : nsChain ns, nsSeenHashes = Map.insert (blockHash blk) () (nsSeenHashes ns)}

-- ============================================================
--  Node runner
-- ============================================================

runNode :: RNG -> Network -> PartitionSet -> TVar NodeState -> IO ()
runNode rng net ps stateVar = forever $ do
  msg <- atomically $ do
    ns <- readTVar stateVar
    case Map.lookup (nsId ns) net of
      Nothing -> retry
      Just q -> readTQueue q
  ns <- readTVarIO stateVar
  let (ns', effects) = handleMsg ns msg
  atomically $ writeTVar stateVar ns'
  forM_ effects $ \case
    Send dst m -> deliver rng net ps (nsId ns') dst m
    Log line -> putStrLn $ "  " <> line

-- ============================================================
--  Miner
-- ============================================================

mineOn :: RNG -> Network -> PartitionSet -> TVar NodeState -> String -> IO ()
mineOn rng net ps stateVar payload = do
  ns <- readTVarIO stateVar
  let prev = tip ns
      blk = mkBlock (blockHeight prev + 1) (blockHash prev) payload
      ns' = appendBlock ns blk
  atomically $ writeTVar stateVar ns'
  putStrLn $ "\n  *** " <> nsId ns' <> " mined " <> show blk <> " ***\n"
  forM_ (nsPeers ns') $ \p -> deliver rng net ps (nsId ns') p (Announce (nsId ns') (blockHeight blk) (blockHash blk))

-- ============================================================
--  Partition control
-- ============================================================

partition :: PartitionSet -> NodeId -> NodeId -> IO ()
partition ps a b = do
  atomically $ modifyTVar ps ((a, b) :)
  putStrLn $ "\n  !!! PARTITION: " <> a <> " <-> " <> b <> " isolated !!!\n"

heal :: PartitionSet -> NodeId -> NodeId -> IO ()
heal ps a b = do
  atomically $ modifyTVar ps (filter (\p -> p /= (a, b) && p /= (b, a)))
  putStrLn $ "\n  !!! HEALED: " <> a <> " <-> " <> b <> " reconnected !!!\n"

-- ============================================================
--  Display
-- ============================================================

printChainSummary :: [(NodeId, TVar NodeState)] -> IO ()
printChainSummary nodes = do
  putStrLn "\n  +----------+--------+----------+----------------+"
  putStrLn "  | Node     | Height | Status   | Tip Hash       |"
  putStrLn "  +----------+--------+----------+----------------+"
  forM_ nodes $ \(nid, sv) -> do
    ns <- readTVarIO sv
    let h = chainHeight ns
        th = showHash (blockHash (tip ns))
        st = case nsSyncStatus ns of
          Synced -> "Synced  "
          Syncing -> "Syncing "
          Partitioned -> "Parted  "
    putStrLn $ "  | " <> pad 8 nid <> " | " <> pad 6 (show h) <> " | " <> pad 8 st <> " | " <> pad 14 th <> " |"
  putStrLn "  +----------+--------+----------+----------------+\n"
  where
    pad n s = take n (s <> repeat ' ')

banner :: String -> IO ()
banner msg = do
  let line = replicate (length msg + 6) '-'
  putStrLn $ "\n+" <> line <> "+"
  putStrLn $ "|   " <> msg <> "   |"
  putStrLn $ "+" <> line <> "+"

pause :: Int -> IO ()
pause ms = threadDelay (ms * 1000)

-- ============================================================
--  Main simulation
-- ============================================================

main :: IO ()
main = do
  putStrLn "\n==================================================="
  putStrLn "  Blockchain Node Simulation"
  putStrLn "  Nodes: A B C D  |  Fully connected mesh"
  putStrLn "===================================================\n"

  let nodeIds = ["A", "B", "C", "D"]
  rng <- newRNG 42
  net <- atomically $ newNetwork nodeIds
  ps <- newTVarIO []

  let peers nid = filter (/= nid) nodeIds
      mkState nid = NodeState {nsId = nid, nsChain = [genesisBlock], nsSyncStatus = Synced, nsPeers = peers nid, nsSeenHashes = Map.singleton (blockHash genesisBlock) ()}

  nodeVars <- forM nodeIds $ \nid -> do
    sv <- newTVarIO (mkState nid)
    return (nid, sv)

  let nodeMap = Map.fromList nodeVars
      getNode nid = nodeMap Map.! nid

  -- Start node event loops
  forM_ nodeVars $ \(_, sv) -> forkIO $ runNode rng net ps sv

  -- -- Phase 1: Normal propagation -------------------------
  banner "Phase 1: A mines #1 -- should propagate to B, C, D"
  pause 100
  mineOn rng net ps (getNode "A") "license:app1:user1"
  pause 600
  printChainSummary nodeVars

  -- -- Phase 2: Sequential mining --------------------------
  banner "Phase 2: B mines #2, then C mines #3"
  mineOn rng net ps (getNode "B") "license:app2:user2"
  pause 400
  mineOn rng net ps (getNode "C") "license:app1:user3"
  pause 600
  printChainSummary nodeVars

  -- -- Phase 3: Partition D, mine, then heal ---------------
  banner "Phase 3: Partition D, mine #4 and #5, then heal"
  partition ps "D" "A"
  partition ps "D" "B"
  partition ps "D" "C"
  pause 100
  mineOn rng net ps (getNode "A") "license:app3:user4"
  pause 200
  mineOn rng net ps (getNode "A") "license:app3:user5"
  pause 300
  putStrLn "  (D is isolated -- it does not receive blocks #4 or #5)"
  printChainSummary nodeVars

  banner "Phase 3b: Heal partition -- D must sync from A"
  heal ps "D" "A"
  heal ps "D" "B"
  heal ps "D" "C"
  pause 100
  -- Re-establish: A pings D, then announces tip to trigger sync
  deliver rng net ps "A" "D" (Ping "A")
  pause 200
  nsA <- readTVarIO (getNode "A")
  deliver rng net ps "A" "D" (Announce "A" (chainHeight nsA) (blockHash (tip nsA)))
  pause 700
  printChainSummary nodeVars

  -- -- Phase 4: Concurrent mining (fork pressure) ----------
  banner "Phase 4: A and B both mine concurrently (fork scenario)"
  putStrLn "  Both nodes mine at the same height simultaneously."
  putStrLn "  First valid block received wins; second is rejected.\n"
  mineOn rng net ps (getNode "A") "license:fork-a"
  mineOn rng net ps (getNode "B") "license:fork-b"
  pause 800
  printChainSummary nodeVars

  -- -- Phase 5: Slow node (C) -------------------------------
  banner "Phase 5: D mines #N+1, C is 'slow' (latency visible in log)"
  mineOn rng net ps (getNode "D") "license:app5:user7"
  pause 700
  printChainSummary nodeVars

  banner "Final state"
  printChainSummary nodeVars
  putStrLn "Simulation complete.\n"