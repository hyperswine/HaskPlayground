{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- All other processes send typed messages; the actor handles them sequentially, batching writes into transactions, serving reads from the cached index without touching the log at all.

module FPFSActor where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM_, forever, void, when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import FPFS

-- ---------------------------------------------------------------------------
-- Request / Response types  (the typed message protocol)
-- ---------------------------------------------------------------------------

-- Each request carries a reply channel so the sender can await the result.
-- In a real FP-RISC actor system this would be a typed oneshot port.
type ReplyTo a = TMVar a

data FsRequest = ReqResolve FilePath String (ReplyTo ResolveResult) | ReqSearch [Tag] (ReplyTo [Node]) | ReqByName String (ReplyTo [Node]) | ReqWrite String [Tag] NodePayload (ReplyTo (Either FsError NodeAddr)) | ReqDelete NodeAddr (ReplyTo (Either FsError ())) | ReqTag NodeAddr Tag (ReplyTo (Either FsError ())) | ReqUntag NodeAddr TagName (ReplyTo (Either FsError ())) | ReqCompact (ReplyTo ()) | ReqFlush (ReplyTo ()) | ReqShutdown

data FsError = ErrNotFound | ErrPathConflict FilePath String NodeAddr | ErrAlreadyDeleted deriving (Show)

-- ---------------------------------------------------------------------------
-- Actor internal state
-- ---------------------------------------------------------------------------

data ActorState = ActorState {asLog :: Log, asIndex :: Index, asBatch :: [LogEntry], asBatchSz :: Int}

initActorState :: ActorState
initActorState = ActorState emptyLog emptyIndex [] 0

-- How many pending entries before we auto-commit the batch
batchThreshold :: Int
batchThreshold = 8

-- ---------------------------------------------------------------------------
-- Batch management
-- ---------------------------------------------------------------------------

-- Flush the pending batch into the log and rebuild the index. This is the only place the log is mutated.
flushBatch :: ActorState -> ActorState
flushBatch as
  | null (asBatch as) = as
  | otherwise =
      let entries = reverse (asBatch as) -- oldest first
          log' = foldl (\l e -> fst $ appendEntry e l) (asLog as) entries
          idx' = foldl applyEntry (asIndex as) entries
       in as {asLog = log', asIndex = idx', asBatch = [], asBatchSz = 0}

addToBatch :: LogEntry -> ActorState -> ActorState
addToBatch e as =
  let as' = as {asBatch = e : asBatch as, asBatchSz = asBatchSz as + 1}
   in if asBatchSz as' >= batchThreshold then flushBatch as' else as'

-- ---------------------------------------------------------------------------
-- Message handlers (pure)
-- ---------------------------------------------------------------------------

-- Reads hit the index only; batch is not yet committed so we build a speculative index that includes pending writes too.
speculativeIndex :: ActorState -> Index
speculativeIndex as =
  foldl applyEntry (asIndex as) (reverse $ asBatch as)

handleResolve :: FilePath -> String -> ActorState -> ResolveResult
handleResolve path name as = resolvePath path name (speculativeIndex as)

handleSearch :: [Tag] -> ActorState -> [Node]
handleSearch tags as = searchByTags tags (speculativeIndex as)

handleByName :: String -> ActorState -> [Node]
handleByName name as = searchByName name (speculativeIndex as)

-- Write: check path uniqueness first (against speculative index), then batch
handleWrite :: String -> [Tag] -> NodePayload -> ActorState -> (ActorState, Either FsError NodeAddr)
handleWrite name tags payload as =
  let idx = speculativeIndex as
      -- check (path, name) uniqueness for any path tags
      conflict =
        foldr
          ( \t acc -> case (acc, t) of
              (Just e, _) -> Just e
              (Nothing, (TagPath, TVPath p)) ->
                case Map.lookup (p, name) (idxPathKey idx) of
                  Just existing -> Just (ErrPathConflict p name existing)
                  Nothing -> Nothing
              _ -> Nothing
          )
          Nothing
          tags
   in case conflict of
        Just err -> (as, Left err)
        Nothing ->
          let (log', addr) = allocAddr (asLog as)
              node = Node addr name tags payload
              as' = as {asLog = log'}
              as'' = addToBatch (EntryWrite node) as'
           in (as'', Right addr)

handleDelete :: NodeAddr -> ActorState -> (ActorState, Either FsError ())
handleDelete addr as =
  let idx = speculativeIndex as
   in if Map.notMember addr (idxNodes idx) then (as, Left ErrNotFound) else let as' = addToBatch (EntryDelete addr) as in (as', Right ())

handleTag :: NodeAddr -> Tag -> ActorState -> (ActorState, Either FsError ())
handleTag addr tag as =
  let idx = speculativeIndex as
   in if Map.notMember addr (idxNodes idx) then (as, Left ErrNotFound) else (addToBatch (EntryTag addr tag) as, Right ())

handleUntag :: NodeAddr -> TagName -> ActorState -> (ActorState, Either FsError ())
handleUntag addr tagName as =
  let idx = speculativeIndex as
   in if Map.notMember addr (idxNodes idx) then (as, Left ErrNotFound) else (addToBatch (EntryUntag addr tagName) as, Right ())

-- ---------------------------------------------------------------------------
-- The actor loop
-- ---------------------------------------------------------------------------

-- The actor owns a TVar over its state and a mailbox (TQueue).
-- It processes one message at a time — no locks needed anywhere else.

type Mailbox = TQueue FsRequest

newMailbox :: IO Mailbox
newMailbox = newTQueueIO

-- Send a request and block until the reply arrives (synchronous call style).
-- In a real async actor system you'd just send and move on; awaiting is opt-in.
call :: Mailbox -> (ReplyTo a -> FsRequest) -> IO a
call mb mkReq = do
  reply <- newEmptyTMVarIO
  atomically $ writeTQueue mb (mkReq reply)
  atomically $ takeTMVar reply

-- Fire-and-forget cast (for writes where caller doesn't need the result)
cast :: Mailbox -> (ReplyTo () -> FsRequest) -> IO ()
cast mb mkReq = void $ forkIO $ call mb mkReq

fsActor :: Mailbox -> IO ()
fsActor mb = go initActorState
  where
    go as = do
      msg <- atomically $ readTQueue mb
      case msg of
        ReqShutdown -> do
          -- flush anything pending before exit
          let as' = flushBatch as
          putStrLn $ "[fs] shutdown. log length=" ++ show (length $ logRecords $ asLog as') ++ " live=" ++ show (length $ liveNodes $ asIndex as')
        ReqFlush reply -> do
          let as' = flushBatch as
          atomically $ putTMVar reply ()
          go as'
        ReqCompact reply -> do
          let as' = flushBatch as
              log' = compact (asLog as')
              idx' = buildIndex log'
              as'' = as' {asLog = log', asIndex = idx'}
          putStrLn $ "[fs] compacted. log length=" ++ show (length $ logRecords log')
          atomically $ putTMVar reply ()
          go as''
        ReqResolve path name reply -> do
          let result = handleResolve path name as
          atomically $ putTMVar reply result
          go as -- read: state unchanged
        ReqSearch tags reply -> do
          let result = handleSearch tags as
          atomically $ putTMVar reply result
          go as
        ReqByName name reply -> do
          let result = handleByName name as
          atomically $ putTMVar reply result
          go as
        ReqWrite name tags payload reply -> do
          let (as', result) = handleWrite name tags payload as
          atomically $ putTMVar reply result
          go as'
        ReqDelete addr reply -> do
          let (as', result) = handleDelete addr as
          atomically $ putTMVar reply result
          go as'
        ReqTag addr tag reply -> do
          let (as', result) = handleTag addr tag as
          atomically $ putTMVar reply result
          go as'
        ReqUntag addr tagName reply -> do
          let (as', result) = handleUntag addr tagName as
          atomically $ putTMVar reply result
          go as'

-- Start the actor in a background thread, return its mailbox
startFsActor :: IO Mailbox
startFsActor = do
  mb <- newMailbox
  void $ forkIO $ fsActor mb
  return mb

-- ---------------------------------------------------------------------------
-- Convenience API (hides mailbox, looks like plain function calls)
-- ---------------------------------------------------------------------------

fsResolve :: Mailbox -> FilePath -> String -> IO ResolveResult
fsResolve mb p n = call mb (ReqResolve p n)

fsSearch :: Mailbox -> [Tag] -> IO [Node]
fsSearch mb tags = call mb (ReqSearch tags)

fsByName :: Mailbox -> String -> IO [Node]
fsByName mb name = call mb (ReqByName name)

fsWriteA :: Mailbox -> String -> [Tag] -> NodePayload -> IO (Either FsError NodeAddr)
fsWriteA mb name tags payload = call mb (ReqWrite name tags payload)

fsDeleteA :: Mailbox -> NodeAddr -> IO (Either FsError ())
fsDeleteA mb addr = call mb (ReqDelete addr)

fsTagA :: Mailbox -> NodeAddr -> Tag -> IO (Either FsError ())
fsTagA mb addr tag = call mb (ReqTag addr tag)

fsCompact :: Mailbox -> IO ()
fsCompact mb = call mb ReqCompact

fsFlush :: Mailbox -> IO ()
fsFlush mb = call mb ReqFlush

-- ---------------------------------------------------------------------------
-- Demo: multiple concurrent "processes" sending messages
-- ---------------------------------------------------------------------------

demo :: IO ()
demo = do
  putStrLn "=== FprFsActor Demo ===\n"

  mb <- startFsActor

  let pathTag p = (TagPath, TVPath p)
      typeTag t = (TagType, TVString t)

  -- Simulate a "device init process" writing /dev nodes
  void $ forkIO $ do
    putStrLn "[device-init] writing /dev nodes"
    r1 <- fsWriteA mb "sdd" [pathTag "/dev/", typeTag "block-device"] (PayloadBytes [0xDEAD])
    r2 <- fsWriteA mb "null" [pathTag "/dev/", typeTag "char-device"] (PayloadBytes [])
    r3 <- fsWriteA mb "tty0" [pathTag "/dev/", typeTag "char-device"] (PayloadBytes [])
    putStrLn $ "[device-init] sdd=" ++ show r1
    putStrLn $ "[device-init] null=" ++ show r2
    putStrLn $ "[device-init] tty0=" ++ show r3

  -- Simulate a "config process" writing /etc nodes
  void $ forkIO $ do
    putStrLn "[config] writing /etc nodes"
    r <- fsWriteA mb "config" [pathTag "/etc/", typeTag "fpr-value", (TagProject, TVString "fp-risc")] (PayloadBytes [0xCAFE])
    putStrLn $ "[config] config=" ++ show r

  -- Simulate a "shell process" doing reads after a short delay
  void $ forkIO $ do
    threadDelay 50000 -- 50ms: let writes arrive first
    putStrLn "\n[shell] resolving /dev/sdd"
    r <- fsResolve mb "/dev/" "sdd"
    putStrLn $ "[shell] " ++ show r

    putStrLn "\n[shell] searching /dev/ char-devices"
    nodes <- fsSearch mb [pathTag "/dev/", typeTag "char-device"]
    forM_ nodes $ \n -> putStrLn $ "  " ++ nodeName n

    putStrLn "\n[shell] searching by name 'config'"
    nodes2 <- fsByName mb "config"
    forM_ nodes2 $ \n -> putStrLn $ "  " ++ nodeName n

  -- Simulate a "tmp cleaner process"
  void $ forkIO $ do
    threadDelay 20000
    putStrLn "\n[tmp-cleaner] writing and then deleting /tmp/scratch"
    Right addr <- fsWriteA mb "scratch" [pathTag "/tmp/", typeTag "text"] (PayloadBytes [0xFF])
    _ <- fsDeleteA mb addr
    putStrLn "[tmp-cleaner] done"

  -- Simulate a path conflict attempt
  void $ forkIO $ do
    threadDelay 30000
    putStrLn "\n[bad-actor] attempting duplicate /dev/sdd"
    r <- fsWriteA mb "sdd" [pathTag "/dev/", typeTag "block-device"] (PayloadBytes [0xBEEF])
    putStrLn $ "[bad-actor] result: " ++ show r

  -- Let everything settle then compact and inspect
  threadDelay 200000 -- 200ms
  putStrLn "\n[main] flushing and compacting"
  fsFlush mb
  fsCompact mb

  putStrLn "\n[main] final search: all /dev/ nodes"
  nodes <- fsSearch mb [pathTag "/dev/"]
  forM_ nodes $ \n -> putStrLn $ "  " ++ nodeName n

  atomically $ writeTQueue mb ReqShutdown
  threadDelay 50000
  putStrLn "\n=== Done ==="
