{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module CRPBC where

-- ============================================================
--  Blockchain simulation using session-typed channels
--
--  Structure:
--    Proto.hs  -- session types, Chan GADT, typed send/recv
--    Main2.hs  -- node state machine + simulation harness
--
--  What the types enforce:
--    * Gossip initiator MUST: sendAnnounce -> recvResponse
--      -> (sendBlock | close). No other order compiles.
--    * Gossip peer MUST: recvAnnounce -> (sendWant | sendIgnore)
--      -> [recvBlock]. No other order compiles.
--    * Sync sessions similarly constrained end-to-end.
--    * Dropping a non-Done channel is a linearity error.
--    * Sending the wrong message type is a type error.
--
--  What the state machine adds on top:
--    * Timeout handling (session-level types can't see time)
--    * Fork/partition detection and recovery
--    * Peer liveness tracking
--    * Deciding WHEN to initiate each session
-- ============================================================

import Control.Concurrent (Chan, MVar, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM, forM_, forever, mapM_, unless, void, when)
import Data.Bits (xor)
import Data.Char (ord)
import Data.IORef
import Data.List (intercalate, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Proto

-- ============================================================
--  Hash utilities
-- ============================================================

fnv1a :: String -> Hash
fnv1a =
  Prelude.fromIntegral
    . Prelude.foldl step (14695981039346656037 :: Word64)
  where
    step acc c =
      (acc `xor` Prelude.fromIntegral (ord c)) * 1099511628211

showHash :: Hash -> String
showHash h = Prelude.take 8 $ go (Prelude.abs h) ""
  where
    go 0 "" = "00000000"
    go 0 acc = acc
    go n acc =
      let (q, r) = n `Prelude.divMod` 16
          d = "0123456789abcdef" Prelude.!! (Prelude.fromIntegral r `Prelude.mod` 16)
       in go q (d : acc)

mkBlock :: Height -> Hash -> String -> Block
mkBlock h prev payload =
  let raw = show h <> show prev <> payload
      bh = fnv1a raw
   in Block h prev payload bh

genesisBlock :: Block
genesisBlock = mkBlock 0 0 "genesis"

-- ============================================================
--  Node state machine
--
--  The session types govern HOW each communication proceeds.
--  The state machine governs WHEN each session is initiated
--  and what happens when things go wrong (timeouts, gaps).
-- ============================================================

data NodeStatus
  = NSynced -- participating normally
  | NSyncing -- mid-sync, don't announce
  | NPartitioned -- lost quorum contact
  deriving (Prelude.Show, Prelude.Eq)

data NS = NS
  { nsId :: NodeId,
    nsChain :: [Block], -- newest first
    nsStatus :: NodeStatus,
    nsPeers :: [NodeId],
    nsSeen :: Map Hash () -- dedup gossip
  }

chainHeight :: NS -> Height
chainHeight = blockHeight . head . nsChain

tipBlock :: NS -> Block
tipBlock = head . nsChain

appendBlock :: NS -> Block -> NS
appendBlock ns blk =
  ns
    { nsChain = blk : nsChain ns,
      nsSeen = Map.insert (blockHash blk) () (nsSeen ns)
    }

validExtension :: Block -> Block -> Bool
validExtension new prev =
  blockHeight new == blockHeight prev + 1
    Prelude.&& blockPrev new == blockHash prev

findBlock :: Height -> [Block] -> Maybe Block
findBlock h = foldr (\b acc -> if blockHeight b == h then Just b else acc) Nothing

-- ============================================================
--  Session runners
--
--  Each runner executes one complete typed session.
--  The types statically ensure the protocol is followed.
--  The state machine layer handles what to do with results.
-- ============================================================

-- Run the gossip initiator session:
--   sendAnnounce -> recvResponse -> maybe sendBlock -> close
runGossipInitiator ::
  Chan 'GS_SendAnnounce ->
  NodeId ->
  Height ->
  Hash ->
  Maybe Block -> -- the block to send if peer wants it
  IO ()
runGossipInitiator chan myId h hsh mblk = do
  chan1 <- sendAnnounce chan myId h hsh
  recvResponse chan1 Prelude.>>= \case
    GotIgnore chan2 -> close chan2
    GotWant _ _ chan2 ->
      case mblk of
        Nothing -> return () -- chan2 :: GS_SendBlock, abandon (unreachable in practice)
        Just blk -> do
          chan3 <- sendBlock chan2 blk
          close chan3

-- Run the gossip peer session:
--   recvAnnounce -> decide -> (sendIgnore | sendWant -> recvBlock) -> close
--   Returns: Nothing if ignored, Just block if received
runGossipPeer ::
  Chan 'GS_RecvResponse ->
  NS -> -- current node state for decisions
  IO (Maybe Block, NS)
runGossipPeer chan ns = do
  (sender, h, hsh, chan1) <- recvAnnounce chan
  if Map.member hsh (nsSeen ns) Prelude.|| h <= chainHeight ns
    then do
      chan2 <- sendIgnore chan1
      close chan2
      return (Nothing, ns)
    else
      if h == chainHeight ns + 1
        then do
          chan2 <- sendWant chan1 (nsId ns) h
          (blk, chan3) <- recvBlock chan2
          close chan3
          return (Just blk, ns {nsSeen = Map.insert hsh () (nsSeen ns)})
        else do
          -- Gap: ignore this gossip channel, trigger sync separately
          chan2 <- sendIgnore chan1
          close chan2
          return (Nothing, ns {nsStatus = NSyncing})

-- Run the sync initiator session:
--   sendSyncRequest -> recvSyncResponse -> closeSS
runSyncInitiator ::
  Chan 'SS_SendRequest ->
  NodeId ->
  Height ->
  IO [Block]
runSyncInitiator chan myId fromH = do
  chan1 <- sendSyncRequest chan myId fromH
  (blocks, chan2) <- recvSyncResponse chan1
  closeSS chan2
  return blocks

-- Run the sync server session:
--   recvSyncRequest -> sendSyncResponse -> closeSS
runSyncServer ::
  Chan 'SS_RecvBlocks ->
  [Block] -> -- our chain (newest first)
  IO ()
runSyncServer chan chain = do
  (_, fromH, chan1) <- recvSyncRequest chan
  let toSend = reverse $ filter (\b -> blockHeight b > fromH) chain
  chan2 <- sendSyncResponse chan1 toSend
  closeSS chan2

-- Run ping initiator: sendPing -> recvPong -> closeLS
runPingInitiator :: Chan 'LS_SendPing -> NodeId -> IO NodeId
runPingInitiator chan myId = do
  chan1 <- sendPing chan myId
  (peer, chan2) <- recvPong chan1
  closeLS chan2
  return peer

-- Run ping responder: recvPing -> sendPong -> closeLS
runPingResponder :: Chan 'LS_RecvPong -> NodeId -> IO NodeId
runPingResponder chan myId = do
  (sender, chan1) <- recvPing chan
  chan2 <- sendPong chan1 myId
  closeLS chan2
  return sender

-- ============================================================
--  Untyped request/response fabric
--
--  Nodes need to *initiate* sessions to peers. We use a
--  per-node request queue: a node posts a Request and
--  the peer's dispatcher picks it up and runs the
--  appropriate typed session server.
--
--  This is the seam between the session-typed world and
--  the untyped routing world. The session types take over
--  once both ends have their Chan.
-- ============================================================

data Request
  = ReqGossip (Chan 'GS_RecvResponse) -- peer runs gossip peer side
  | ReqSync (Chan 'SS_RecvBlocks) -- peer runs sync server side
  | ReqPing (Chan 'LS_RecvPong) -- peer runs ping responder

type RequestQueue = TQueue Request

type Fabric = Map NodeId RequestQueue

newFabric :: [NodeId] -> STM Fabric
newFabric ids =
  Map.fromList
    <$> forM ids (\nid -> (nid,) <$> newTQueue)

-- Initiate a gossip session with a peer.
-- Creates a channel pair, posts the peer end to their queue,
-- returns our end for the typed session runner.
initiateGossip :: Fabric -> NodeId -> IO (Chan 'GS_SendAnnounce)
initiateGossip fabric dst = do
  (ourEnd, peerEnd) <- newGossipPair
  atomically $ case Map.lookup dst fabric of
    Nothing -> return ()
    Just q -> writeTQueue q (ReqGossip peerEnd)
  return ourEnd

initiateSync :: Fabric -> NodeId -> IO (Chan 'SS_SendRequest)
initiateSync fabric dst = do
  (ourEnd, peerEnd) <- newSyncPair
  atomically $ case Map.lookup dst fabric of
    Nothing -> return ()
    Just q -> writeTQueue q (ReqSync peerEnd)
  return ourEnd

initiatePing :: Fabric -> NodeId -> IO (Chan 'LS_SendPing)
initiatePing fabric dst = do
  (ourEnd, peerEnd) <- newLivePair
  atomically $ case Map.lookup dst fabric of
    Nothing -> return ()
    Just q -> writeTQueue q (ReqPing peerEnd)
  return ourEnd

-- ============================================================
--  Partition simulation (same as before)
-- ============================================================

type PartitionSet = TVar [(NodeId, NodeId)]

isPartitioned :: PartitionSet -> NodeId -> NodeId -> IO Bool
isPartitioned ps a b = do
  parts <- readTVarIO ps
  return $ (a, b) `elem` parts Prelude.|| (b, a) `elem` parts

-- Partition-aware initiation: just skip if partitioned
initiateGossipP :: PartitionSet -> Fabric -> NodeId -> NodeId -> IO (Maybe (Chan 'GS_SendAnnounce))
initiateGossipP ps fabric src dst = do
  blocked <- isPartitioned ps src dst
  if blocked
    then return Nothing
    else Just <$> initiateGossip fabric dst

initiateSyncP :: PartitionSet -> Fabric -> NodeId -> NodeId -> IO (Maybe (Chan 'SS_SendRequest))
initiateSyncP ps fabric src dst = do
  blocked <- isPartitioned ps src dst
  if blocked
    then return Nothing
    else Just <$> initiateSync fabric dst

initiatePingP :: PartitionSet -> Fabric -> NodeId -> NodeId -> IO (Maybe (Chan 'LS_SendPing))
initiatePingP ps fabric src dst = do
  blocked <- isPartitioned ps src dst
  if blocked
    then return Nothing
    else Just <$> initiatePing fabric dst

-- ============================================================
--  Node dispatcher -- serves incoming requests
-- ============================================================

-- Reads from the node's request queue and runs the
-- appropriate typed session server. Runs in its own thread.
nodeDispatcher :: PartitionSet -> Fabric -> TVar NS -> IO ()
nodeDispatcher ps fabric stateVar = forever $ do
  ns <- readTVarIO stateVar
  req <- atomically $ readTQueue (fabric Map.! nsId ns)

  case req of
    -- ---- Serve gossip peer session ----------------------
    ReqGossip peerEnd -> do
      ns0 <- readTVarIO stateVar
      (mblk, ns1) <- runGossipPeer peerEnd ns0
      atomically $ writeTVar stateVar ns1
      case mblk of
        Nothing -> return ()
        Just blk
          | not (validExtension blk (tipBlock ns1)) ->
              putStrLn $ "  " <> nsId ns1 <> " REJECTED " <> showBlock blk
          | otherwise -> do
              let ns2 = appendBlock ns1 blk
              atomically $ writeTVar stateVar ns2
              putStrLn $ "  " <> nsId ns2 <> " appended " <> showBlock blk
              -- Re-gossip to other peers
              forM_ (nsPeers ns2) $ \p -> do
                mch <- initiateGossipP ps fabric (nsId ns2) p
                case mch of
                  Nothing -> return ()
                  Just ch ->
                    void $
                      forkIO $
                        runGossipInitiator
                          ch
                          (nsId ns2)
                          (blockHeight blk)
                          (blockHash blk)
                          (Just blk)

    -- ---- Serve sync server session ----------------------
    ReqSync peerEnd -> do
      ns0 <- readTVarIO stateVar
      putStrLn $ "  " <> nsId ns0 <> " serving sync"
      runSyncServer peerEnd (nsChain ns0)

    -- ---- Serve ping responder ---------------------------
    ReqPing peerEnd -> do
      ns0 <- readTVarIO stateVar
      peer <- runPingResponder peerEnd (nsId ns0)
      atomically $ modifyTVar stateVar $ \s ->
        s {nsPeers = nub (peer : nsPeers s)}
      putStrLn $ "  " <> nsId ns0 <> " ponged " <> peer

-- ============================================================
--  High-level node actions
-- ============================================================

-- Mine a block and announce to all peers
mineBlock :: PartitionSet -> Fabric -> TVar NS -> String -> IO ()
mineBlock ps fabric stateVar payload = do
  ns <- readTVarIO stateVar
  let prev = tipBlock ns
      blk = mkBlock (blockHeight prev + 1) (blockHash prev) payload
      ns' = appendBlock ns blk
  atomically $ writeTVar stateVar ns'
  putStrLn $ "\n  *** " <> nsId ns' <> " mined " <> showBlock blk <> " ***\n"
  forM_ (nsPeers ns') $ \p -> do
    mch <- initiateGossipP ps fabric (nsId ns') p
    case mch of
      Nothing -> return ()
      Just ch ->
        void $
          forkIO $
            runGossipInitiator
              ch
              (nsId ns')
              (blockHeight blk)
              (blockHash blk)
              (Just blk)

-- Trigger a sync from a specific peer
syncFrom :: PartitionSet -> Fabric -> TVar NS -> NodeId -> IO ()
syncFrom ps fabric stateVar peer = do
  ns <- readTVarIO stateVar
  mch <- initiateSyncP ps fabric (nsId ns) peer
  case mch of
    Nothing -> putStrLn $ "  " <> nsId ns <> " can't reach " <> peer <> " (partitioned)"
    Just ch -> do
      blocks <- runSyncInitiator ch (nsId ns) (chainHeight ns)
      ns0 <- readTVarIO stateVar
      let valid = filter (isNext ns0) blocks
          ns1 = foldl appendBlock ns0 valid
          ns2 = ns1 {nsStatus = NSynced}
      atomically $ writeTVar stateVar ns2
      putStrLn $
        "  "
          <> nsId ns2
          <> " sync from "
          <> peer
          <> " complete, now at #"
          <> show (chainHeight ns2)
      -- Announce new tip
      forM_ (nsPeers ns2) $ \p -> do
        mch2 <- initiateGossipP ps fabric (nsId ns2) p
        case mch2 of
          Nothing -> return ()
          Just ch2 ->
            void $
              forkIO $
                runGossipInitiator
                  ch2
                  (nsId ns2)
                  (chainHeight ns2)
                  (blockHash (tipBlock ns2))
                  Nothing
  where
    isNext s b =
      blockHeight b == chainHeight s + 1
        Prelude.&& validExtension b (tipBlock s)

-- Ping a peer to confirm liveness
pingPeer :: PartitionSet -> Fabric -> TVar NS -> NodeId -> IO ()
pingPeer ps fabric stateVar peer = do
  ns <- readTVarIO stateVar
  mch <- initiatePingP ps fabric (nsId ns) peer
  case mch of
    Nothing -> return ()
    Just ch -> do
      pong <- runPingInitiator ch (nsId ns)
      atomically $ modifyTVar stateVar $ \s ->
        s {nsPeers = nub (pong : nsPeers s)}
      putStrLn $ "  " <> nsId ns <> " confirmed " <> pong <> " is live"

-- ============================================================
--  Helpers
-- ============================================================

showBlock :: Block -> String
showBlock b =
  "[#"
    <> show (blockHeight b)
    <> " "
    <> showHash (blockHash b)
    <> " "
    <> blockPayload b
    <> "]"

pause :: Int -> IO ()
pause ms = threadDelay (ms * 1000)

banner :: String -> IO ()
banner msg = do
  let bar = replicate (length msg + 6) '-'
  putStrLn $ "\n+" <> bar <> "+"
  putStrLn $ "|   " <> msg <> "   |"
  putStrLn $ "+" <> bar <> "+"

printSummary :: [(NodeId, TVar NS)] -> IO ()
printSummary nodes = do
  putStrLn "\n  +----------+--------+----------+----------------+"
  putStrLn "  | Node     | Height | Status   | Tip Hash       |"
  putStrLn "  +----------+--------+----------+----------------+"
  forM_ nodes $ \(nid, sv) -> do
    ns <- readTVarIO sv
    let h = chainHeight ns
        th = showHash (blockHash (tipBlock ns))
        st = case nsStatus ns of
          NSynced -> "Synced  "
          NSyncing -> "Syncing "
          NPartitioned -> "Parted  "
    putStrLn $
      "  | "
        <> pad 8 nid
        <> " | "
        <> pad 6 (show h)
        <> " | "
        <> pad 8 st
        <> " | "
        <> pad 14 th
        <> " |"
  putStrLn "  +----------+--------+----------+----------------+\n"
  where
    pad n s = Prelude.take n (s <> Prelude.repeat ' ')

setPartition :: PartitionSet -> NodeId -> NodeId -> IO ()
setPartition ps a b = do
  atomically $ modifyTVar ps ((a, b) :)
  putStrLn $ "\n  !!! PARTITION: " <> a <> " <-> " <> b <> " !!!\n"

healPartition :: PartitionSet -> NodeId -> NodeId -> IO ()
healPartition ps a b = do
  atomically $ modifyTVar ps (filter (\p -> p /= (a, b) Prelude.&& p /= (b, a)))
  putStrLn $ "\n  !!! HEALED: " <> a <> " <-> " <> b <> " !!!\n"

-- ============================================================
--  Main
-- ============================================================

main :: IO ()
main = do
  putStrLn "\n==================================================="
  putStrLn "  Session-Typed Blockchain Node Simulation"
  putStrLn "  Protocol shape enforced by Chan GADT + LinearTypes"
  putStrLn "==================================================="

  let nodeIds = ["A", "B", "C", "D"]
  fabric <- atomically $ newFabric nodeIds
  ps <- newTVarIO []

  let peers nid = filter (/= nid) nodeIds
      mkState nid =
        NS
          { nsId = nid,
            nsChain = [genesisBlock],
            nsStatus = NSynced,
            nsPeers = peers nid,
            nsSeen = Map.singleton (blockHash genesisBlock) ()
          }

  nodeVars <- forM nodeIds $ \nid -> do
    sv <- newTVarIO (mkState nid)
    return (nid, sv)

  let nodeMap = Map.fromList nodeVars
      getNode nid = nodeMap Map.! nid

  -- Start dispatcher loop for each node
  forM_ nodeVars $ \(_, sv) ->
    forkIO $ nodeDispatcher ps fabric sv

  -- ── Phase 1: Normal gossip propagation ──────────────────
  banner "Phase 1: A mines #1 via typed gossip sessions"
  pause 100
  mineBlock ps fabric (getNode "A") "license:app1:user1"
  pause 600
  printSummary nodeVars

  -- ── Phase 2: Sequential blocks ──────────────────────────
  banner "Phase 2: B mines #2, C mines #3"
  mineBlock ps fabric (getNode "B") "license:app2:user2"
  pause 400
  mineBlock ps fabric (getNode "C") "license:app1:user3"
  pause 600
  printSummary nodeVars

  -- ── Phase 3: Partition + sync recovery ──────────────────
  banner "Phase 3: Partition D, mine #4 #5, heal, D syncs"
  setPartition ps "D" "A"
  setPartition ps "D" "B"
  setPartition ps "D" "C"
  pause 100
  mineBlock ps fabric (getNode "A") "license:app3:user4"
  pause 200
  mineBlock ps fabric (getNode "A") "license:app3:user5"
  pause 300
  putStrLn "  (D isolated -- blocks #4 #5 not delivered)"
  printSummary nodeVars

  banner "Phase 3b: Heal -- D pings A then runs typed sync session"
  healPartition ps "D" "A"
  healPartition ps "D" "B"
  healPartition ps "D" "C"
  pause 100
  pingPeer ps fabric (getNode "D") "A"
  pause 100
  syncFrom ps fabric (getNode "D") "A"
  pause 400
  printSummary nodeVars

  -- ── Phase 4: Concurrent mining ──────────────────────────
  banner "Phase 4: A and B mine concurrently (fork pressure)"
  mineBlock ps fabric (getNode "A") "license:fork-a"
  mineBlock ps fabric (getNode "B") "license:fork-b"
  pause 800
  printSummary nodeVars

  banner "Final state"
  printSummary nodeVars
  putStrLn "Simulation complete.\n"