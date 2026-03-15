-- Compile: stack build --ghc-options="-O2 -threaded"
-- Run:     stack exec -- haskplayground-exe +RTS -N4
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module FPActors where

import Control.Concurrent (forkIO, newEmptyMVar, newMVar, putMVar, takeMVar, threadDelay, withMVar)
import Control.Concurrent.STM
import Control.Monad (forM, forM_, void, when)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Numeric.Natural (Natural)
import System.IO
import Text.Printf (printf)

-- ---------------------------------------------------------------------------
-- Primitives
-- ---------------------------------------------------------------------------

type ActorId = Int

-- Hard-bounded mailbox.  Sending to a full mailbox blocks (back-pressure).
newtype Mailbox a = Mailbox {mbQueue :: TBQueue a}

mailboxCapacity :: Natural
mailboxCapacity = 64

newMailbox :: STM (Mailbox a)
newMailbox = Mailbox <$> newTBQueue mailboxCapacity

sendMsg :: Mailbox a -> a -> STM ()
sendMsg mb = writeTBQueue (mbQueue mb)

recvMsg :: Mailbox a -> STM a
recvMsg = readTBQueue . mbQueue

tryRecvMsg :: Mailbox a -> STM (Maybe a)
tryRecvMsg = tryReadTBQueue . mbQueue

spawnActor :: (Mailbox a -> IO ()) -> IO (Mailbox a)
spawnActor body = do
  mb <- atomically newMailbox
  void $ forkIO (body mb)
  return mb

type Logger = String -> IO ()

makeLogger :: IO Logger
makeLogger = do
  lock <- newMVar ()
  return $ \msg -> withMVar lock $ \_ -> putStrLn msg

-- ---------------------------------------------------------------------------
-- Message vocabulary
-- ---------------------------------------------------------------------------

data Task = Task {taskId :: Int, taskDesc :: String} deriving (Show)

data Msg = DoWork Task | StealRequest ActorId | StealGrant Task | StealDeny | StealAck Int | LogAppend String ActorId | LogRead ActorId | LogAck | LogSnapshot [String] | Shutdown deriving (Show)

-- ---------------------------------------------------------------------------
-- 1.  Work-Stealing Scheduler
-- ---------------------------------------------------------------------------

type Registry = Map ActorId (Mailbox Msg)

data CoreState = CoreState {csId :: ActorId, csLocalQ :: TQueue Task, csRegRef :: IORef Registry, csLog :: Logger}

-- Message-handling loop: only enqueues tasks and handles steal protocol. A separate execution thread drains csLocalQ, so the queue has real depth and the idle watcher on other cores can actually find something to steal.
coreActor :: CoreState -> Mailbox Msg -> IO ()
coreActor st mb = loop
  where
    loop = do
      msg <- atomically (recvMsg mb)
      case msg of
        Shutdown ->
          csLog st (printf "[core %d] shutting down" (csId st))
        DoWork t -> do
          csLog st (printf "[core %d] queued task %d (%s)" (csId st) (taskId t) (taskDesc t))
          atomically $ writeTQueue (csLocalQ st) t
          loop
        StealRequest from_ -> do
          mTask <- atomically $ tryReadTQueue (csLocalQ st)
          reg <- readIORef (csRegRef st)
          case Map.lookup from_ reg of
            Nothing -> return ()
            Just fromMb ->
              case mTask of
                Just t -> do
                  csLog st (printf "[core %d] GRANT task %d to core %d" (csId st) (taskId t) from_)
                  atomically $ sendMsg fromMb (StealGrant t)
                Nothing -> do
                  csLog st (printf "[core %d] DENY steal from core %d" (csId st) from_)
                  atomically $ sendMsg fromMb StealDeny
          loop
        StealGrant t -> do
          csLog st (printf "[core %d] STOLE task %d (%s)" (csId st) (taskId t) (taskDesc t))
          reg <- readIORef (csRegRef st)
          let n = Map.size reg
              donorId = (csId st + 1) `mod` n
          case Map.lookup donorId reg of
            Just donorMb -> atomically $ sendMsg donorMb (StealAck (taskId t))
            Nothing -> return ()
          atomically $ writeTQueue (csLocalQ st) t
          loop
        StealDeny -> do
          csLog st (printf "[core %d] steal denied" (csId st))
          loop
        StealAck tid -> do
          csLog st (printf "[core %d] ack for task %d" (csId st) tid)
          loop
        _ -> loop

-- Execution thread: blocks on the local TQueue; runs one task at a time. Separated from the actor message loop so queue depth is visible to stealing neighbours during execution.
execThread :: CoreState -> IO ()
execThread st = do
  t <- atomically $ readTQueue (csLocalQ st)
  executeTask st t
  execThread st

executeTask :: CoreState -> Task -> IO ()
executeTask st t = do
  csLog st (printf "[core %d] EXEC  task %d: %s" (csId st) (taskId t) (taskDesc t))
  threadDelay (500000 + taskId t * 100000)
  csLog st (printf "[core %d] DONE  task %d" (csId st) (taskId t))

-- Idle-watcher thread: triggers stealing when local queue is empty
idleWatcher :: CoreState -> Mailbox Msg -> IO ()
idleWatcher st mb = loop
  where
    loop = do
      threadDelay 20000
      localEmpty <- atomically $ isEmptyTQueue (csLocalQ st)
      when localEmpty $ do
        reg <- readIORef (csRegRef st)
        let n = Map.size reg
            neighbourId = (csId st + 1) `mod` n
        case Map.lookup neighbourId reg of
          Just nMb -> do
            csLog st (printf "[core %d] idle - stealing from core %d" (csId st) neighbourId)
            atomically $ sendMsg nMb (StealRequest (csId st))
          Nothing -> return ()
      loop

buildCorePool :: Int -> Logger -> IO (IORef Registry)
buildCorePool n logger = do
  pairs <- forM [0 .. n - 1] $ \i -> (i,) <$> atomically newMailbox
  let reg = Map.fromList pairs
  regRef <- newIORef reg

  forM_ pairs $ \(i, mb) -> do
    localQ <- newTQueueIO
    let st = CoreState i localQ regRef logger
    void $ forkIO (coreActor st mb)
    void $ forkIO (execThread st)
    void $ forkIO (idleWatcher st mb)

  return regRef

dispatchTask :: IORef Registry -> ActorId -> Task -> IO ()
dispatchTask regRef coreId t = do
  reg <- readIORef regRef
  case Map.lookup coreId reg of
    Just mb -> atomically $ sendMsg mb (DoWork t)
    Nothing -> putStrLn ("ERROR: unknown core " ++ show coreId)

-- ---------------------------------------------------------------------------
-- 2.  Shared Append-Only Log via Intermediary Actor
-- ---------------------------------------------------------------------------

logActor :: TVar [String] -> Map ActorId (Mailbox Msg) -> Logger -> Mailbox Msg -> IO ()
logActor logVar workerMbs logger mb = loop
  where
    loop = do
      msg <- atomically (recvMsg mb)
      case msg of
        Shutdown ->
          logger "[log] shutting down"
        LogAppend entry replyId -> do
          atomically $ modifyTVar' logVar (entry :)
          logger (printf "[log] appended from actor %d: %s" replyId entry)
          case Map.lookup replyId workerMbs of
            Just replyMb -> atomically $ sendMsg replyMb LogAck
            Nothing -> return ()
          loop
        LogRead replyId -> do
          snap <- reverse <$> readTVarIO logVar
          logger (printf "[log] snapshot (%d entries) to actor %d" (length snap) replyId)
          case Map.lookup replyId workerMbs of
            Just replyMb -> atomically $ sendMsg replyMb (LogSnapshot snap)
            Nothing -> return ()
          loop
        _ -> loop

workerActor :: ActorId -> Mailbox Msg -> Logger -> [String] -> Mailbox Msg -> IO ()
workerActor wid logMb logger entries selfMb = do
  logger (printf "[worker %d] starting, %d entries to append" wid (length entries))

  -- Phase 1: append with ACK-based flow control
  let go [] _ = return ()
      go (e : es) idx = do
        let full = printf "w%d/e%d:%s" wid idx e :: String
        atomically $ sendMsg logMb (LogAppend full wid)
        atomically (recvMsg selfMb) >>= \case
          LogAck -> logger (printf "[worker %d] ack for entry %d" wid idx)
          other -> logger (printf "[worker %d] unexpected: %s" wid (show other))
        threadDelay 40000
        go es (idx + 1 :: Int)
  go entries 1

  -- Phase 2: read back the full snapshot
  atomically $ sendMsg logMb (LogRead wid)
  atomically (recvMsg selfMb) >>= \case
    LogSnapshot snap -> do
      logger (printf "[worker %d] snapshot (%d entries):" wid (length snap))
      mapM_ (\e -> logger ("  | " ++ e)) snap
    other ->
      logger (printf "[worker %d] unexpected snapshot response: %s" wid (show other))

  logger (printf "[worker %d] done" wid)

-- ---------------------------------------------------------------------------
-- Driver
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  logger <- makeLogger

  logger "=============================================="
  logger "  Demo 1: Work-Stealing Scheduler"
  logger "  4 cores, 8 tasks all dispatched to core 0"
  logger "=============================================="

  regRef <- buildCorePool 4 logger

  -- Batch-dispatch all 8 tasks to core 0 before any core starts draining, so idle watchers on cores 1-3 see a loaded queue and stealing fires.
  let tasks = [Task i (printf "fib(%d)" (i + 20)) | i <- [1 .. 8]]
  mapM_ (dispatchTask regRef 0) tasks

  -- wait for all tasks to complete
  threadDelay 12000000
  reg <- readIORef regRef
  forM_ (Map.elems reg) $ \mb -> atomically (sendMsg mb Shutdown)
  threadDelay 200000

  logger ""
  logger "=============================================="
  logger "  Demo 2: Shared Append-Only Log"
  logger "  Workers only interact via the log actor"
  logger "=============================================="

  logVar <- newTVarIO []
  wMb1 <- atomically newMailbox
  wMb2 <- atomically newMailbox
  let wReg = Map.fromList [(10 :: ActorId, wMb1), (11, wMb2)]

  logMb <- spawnActor (logActor logVar wReg logger)

  done1 <- newEmptyMVar
  done2 <- newEmptyMVar

  void $ forkIO $ do
    workerActor 10 logMb logger ["alpha", "beta", "gamma"] wMb1
    putMVar done1 ()

  void $ forkIO $ do
    workerActor 11 logMb logger ["delta", "epsilon", "zeta"] wMb2
    putMVar done2 ()

  takeMVar done1
  takeMVar done2

  finalLog <- reverse <$> readTVarIO logVar
  logger ""
  logger "-- Final log (ground truth from TVar) --"
  mapM_ (\e -> logger ("  " ++ e)) finalLog

  atomically $ sendMsg logMb Shutdown
  threadDelay 100000
  logger ""
  logger "[main] simulation complete"
