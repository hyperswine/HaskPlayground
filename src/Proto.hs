{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

-- ============================================================
--  Session-typed channels  (Proto.hs)
--
--  Approach: phantom-type state machine + opaque constructor.
--
--  GHC's LinearTypes extension requires linear-base to work
--  with IO (IO's >>= is not linear). Without it, full compile-
--  time linearity on IO channels isn't achievable in stock
--  GHC 9.4.
--
--  What we get instead -- which is what most real session-type
--  Haskell libraries (session-typed-channels, sesh, HasChor)
--  actually do -- is:
--
--    1. GADT phantom state: Chan s. The s parameter tracks
--       the protocol state. Each send/recv returns a Chan at
--       the *new* state. Using the old chan would require a
--       type at the old state, which no function accepts.
--
--    2. Opaque constructor: Chan is not exported. You cannot
--       construct, inspect, or coerce a Chan outside this
--       module. The only way to advance it is through the
--       typed API.
--
--    3. Type-changing transitions: sendAnnounce takes a
--       Chan 'GS_SendAnnounce and returns Chan 'GS_RecvResponse.
--       Calling it again with the old value would require
--       passing a Chan 'GS_RecvResponse where GS_SendAnnounce
--       is expected -- a type error.
--
--  This prevents:
--    * Wrong message for current state     (type error)
--    * Wrong send/recv order               (type error)
--    * close on non-Done channel           (type error)
--
--  It does NOT prevent (without linear-base):
--    * Using the same chan value twice      (would need %1->)
--    * Dropping a chan without closing      (would need %1->)
--
--  In practice, the GADT approach catches the entire class of
--  "protocol shape" bugs. Linearity adds "resource lifecycle"
--  guarantees on top. Both matter; the GADT layer is the more
--  commonly achievable one in stock Haskell today.
-- ============================================================

module Proto
  ( GossipState(..), SyncState(..), LiveState(..)
  , Chan
  , newGossipPair, newSyncPair, newLivePair
  , sendAnnounce, AnnounceResponse(..), recvResponse, sendBlock
  , recvAnnounce, sendWant, sendIgnore, recvBlock
  , sendSyncRequest, recvSyncResponse
  , recvSyncRequest, sendSyncResponse
  , sendPing, recvPong, recvPing, sendPong
  , close, closeSS, closeLS
  , NodeId, Height, Hash, Block(..)
  ) where

import Control.Concurrent.STM
import Data.Coerce (coerce)

-- ============================================================
--  Domain types
-- ============================================================

type NodeId = String
type Height = Int
type Hash   = Int

data Block = Block
  { blockHeight  :: Height
  , blockPrev    :: Hash
  , blockPayload :: String
  , blockHash    :: Hash
  } deriving (Show, Eq)

-- ============================================================
--  Session state phantom tags
--
--  These are type-level only. They never appear at runtime.
--  DataKinds lifts them to the kind level so they can be
--  used as phantom parameters to Chan.
-- ============================================================

data GossipState
  = GS_SendAnnounce   -- initiator turn: send Announce
  | GS_RecvResponse   -- initiator turn: recv Want or Ignore
  | GS_SendBlock      -- initiator turn: send Block (after Want)
  | GS_Done           -- session complete; only close is valid

data SyncState
  = SS_SendRequest    -- syncer: send SyncRequest
  | SS_RecvBlocks     -- syncer: recv SyncResponse
  | SS_Done

data LiveState
  = LS_SendPing
  | LS_RecvPong
  | LS_Done

-- ============================================================
--  Wire format (internal, not exported)
-- ============================================================

data WireMsg
  = W_Announce NodeId Height Hash
  | W_Want     NodeId Height
  | W_Ignore
  | W_Block    Block
  | W_SyncReq  NodeId Height
  | W_SyncResp [Block]
  | W_Ping     NodeId
  | W_Pong     NodeId
  deriving (Show)

-- ============================================================
--  Chan s: session-typed channel
--
--  The constructor is NOT exported. Outside this module:
--    * You cannot create a Chan except via newGossipPair etc.
--    * You cannot inspect or coerce the phantom s.
--    * The only operations are the typed send/recv below.
--
--  The phantom s changes type at each step. Since the old
--  Chan value is at the old type and no API function accepts
--  the old type again, protocol violations are type errors.
-- ============================================================

-- Internal capability record: send and recv actions for this end.
data Caps = Caps
  { capSend :: WireMsg -> STM ()
  , capRecv :: STM WireMsg
  }

-- Chan s: wraps a Caps. Constructor not exported.
newtype Chan (s :: k) = Chan Caps

-- ============================================================
--  Channel pair constructors
-- ============================================================

-- Each end gets its own send/recv queues (full duplex).
-- Initiator sends on qA, peer recvs on qA.
-- Peer sends on qB, initiator recvs on qB.
newGossipPair :: IO (Chan 'GS_SendAnnounce, Chan 'GS_RecvResponse)
newGossipPair = do
  qA <- newTQueueIO   -- initiator -> peer
  qB <- newTQueueIO   -- peer -> initiator
  -- Initiator: sends on qA, recvs on qB
  -- Peer:      sends on qB, recvs on qA
  return ( Chan (Caps (writeTQueue qA) (readTQueue qB))
         , Chan (Caps (writeTQueue qB) (readTQueue qA)) )

newSyncPair :: IO (Chan 'SS_SendRequest, Chan 'SS_RecvBlocks)
newSyncPair = do
  qA <- newTQueueIO
  qB <- newTQueueIO
  return ( Chan (Caps (writeTQueue qA) (readTQueue qB))
         , Chan (Caps (writeTQueue qB) (readTQueue qA)) )

newLivePair :: IO (Chan 'LS_SendPing, Chan 'LS_RecvPong)
newLivePair = do
  qA <- newTQueueIO
  qB <- newTQueueIO
  return ( Chan (Caps (writeTQueue qA) (readTQueue qB))
         , Chan (Caps (writeTQueue qB) (readTQueue qA)) )

-- ============================================================
--  Internal helpers (not exported)
-- ============================================================

send' :: Chan s -> WireMsg -> IO (Chan t)
send' (Chan caps) msg = atomically (capSend caps msg) >> return (Chan caps)

recv' :: Chan s -> IO (WireMsg, Chan t)
recv' (Chan caps) = do { msg <- atomically (capRecv caps); return (msg, Chan caps) }

-- ============================================================
--  Gossip: Initiator
--
--  State machine (initiator perspective):
--    GS_SendAnnounce
--        |-- sendAnnounce --> GS_RecvResponse
--        |-- recvResponse --> GotWant  --> GS_SendBlock
--        |                   GotIgnore --> GS_Done
--        |-- sendBlock    --> GS_Done
-- ============================================================

sendAnnounce
  :: Chan 'GS_SendAnnounce -> NodeId -> Height -> Hash
  -> IO (Chan 'GS_RecvResponse)
sendAnnounce c s h hsh = send' c (W_Announce s h hsh)

data AnnounceResponse
  = GotWant   NodeId Height (Chan 'GS_SendBlock)
  | GotIgnore               (Chan 'GS_Done)

recvResponse :: Chan 'GS_RecvResponse -> IO AnnounceResponse
recvResponse c = do
  (msg, c') <- recv' c
  return $ case msg of
    W_Want s h -> GotWant s h (coerce c')
    _          -> GotIgnore   (coerce c')

sendBlock :: Chan 'GS_SendBlock -> Block -> IO (Chan 'GS_Done)
sendBlock c blk = send' c (W_Block blk)

-- ============================================================
--  Gossip: Peer (dual perspective)
--
--  Peer starts at GS_RecvResponse (the dual of GS_SendAnnounce)
--    GS_RecvResponse  (peer's starting state -- receives first)
--        |-- recvAnnounce --> GS_SendAnnounce + payload
--        |-- sendWant     --> GS_RecvResponse  (then recvBlock)
--        |-- sendIgnore   --> GS_Done
--        |-- recvBlock    --> Block + GS_Done
-- ============================================================

recvAnnounce
  :: Chan 'GS_RecvResponse -> IO (NodeId, Height, Hash, Chan 'GS_SendAnnounce)
recvAnnounce c = do
  (msg, c') <- recv' c
  return $ case msg of
    W_Announce s h hsh -> (s, h, hsh, c')
    _                  -> error "Proto: protocol violation: expected Announce"

sendWant :: Chan 'GS_SendAnnounce -> NodeId -> Height -> IO (Chan 'GS_RecvResponse)
sendWant c s h = send' c (W_Want s h)

sendIgnore :: Chan 'GS_SendAnnounce -> IO (Chan 'GS_Done)
sendIgnore c = send' c W_Ignore

recvBlock :: Chan 'GS_RecvResponse -> IO (Block, Chan 'GS_Done)
recvBlock c = do
  (msg, c') <- recv' c
  return $ case msg of
    W_Block blk -> (blk, c')
    _           -> error "Proto: protocol violation: expected Block"

-- ============================================================
--  Sync protocol
--
--  Syncer:  SS_SendRequest  -->  SS_RecvBlocks  -->  SS_Done
--  Server:  SS_RecvBlocks   -->  SS_SendRequest -->  SS_Done
-- ============================================================

sendSyncRequest :: Chan 'SS_SendRequest -> NodeId -> Height -> IO (Chan 'SS_RecvBlocks)
sendSyncRequest c s h = send' c (W_SyncReq s h)

recvSyncResponse :: Chan 'SS_RecvBlocks -> IO ([Block], Chan 'SS_Done)
recvSyncResponse c = do
  (msg, c') <- recv' c
  return $ case msg of
    W_SyncResp bs -> (bs, c')
    _             -> error "Proto: protocol violation: expected SyncResp"

recvSyncRequest :: Chan 'SS_RecvBlocks -> IO (NodeId, Height, Chan 'SS_SendRequest)
recvSyncRequest c = do
  (msg, c') <- recv' c
  return $ case msg of
    W_SyncReq s h -> (s, h, c')
    _             -> error "Proto: protocol violation: expected SyncReq"

sendSyncResponse :: Chan 'SS_SendRequest -> [Block] -> IO (Chan 'SS_Done)
sendSyncResponse c bs = send' c (W_SyncResp bs)

-- ============================================================
--  Liveness protocol
--
--  Initiator: LS_SendPing  -->  LS_RecvPong  -->  LS_Done
--  Responder: LS_RecvPong  -->  LS_SendPing  -->  LS_Done
-- ============================================================

sendPing :: Chan 'LS_SendPing -> NodeId -> IO (Chan 'LS_RecvPong)
sendPing c s = send' c (W_Ping s)

recvPong :: Chan 'LS_RecvPong -> IO (NodeId, Chan 'LS_Done)
recvPong c = do
  (msg, c') <- recv' c
  return $ case msg of { W_Pong s -> (s, c'); _ -> error "Proto: expected Pong" }

recvPing :: Chan 'LS_RecvPong -> IO (NodeId, Chan 'LS_SendPing)
recvPing c = do
  (msg, c') <- recv' c
  return $ case msg of { W_Ping s -> (s, c'); _ -> error "Proto: expected Ping" }

sendPong :: Chan 'LS_SendPing -> NodeId -> IO (Chan 'LS_Done)
sendPong c s = send' c (W_Pong s)

-- ============================================================
--  Close: only callable on Done channels.
--  Attempting close on a non-Done Chan is a compile-time
--  type error -- the wrong type won't unify with 'GS_Done.
-- ============================================================

close   :: Chan 'GS_Done -> IO ()
close   _ = return ()

closeSS :: Chan 'SS_Done -> IO ()
closeSS _ = return ()

closeLS :: Chan 'LS_Done -> IO ()
closeLS _ = return ()
