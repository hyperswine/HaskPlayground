{-# LANGUAGE ScopedTypeVariables #-}

-- | A minimal, self-contained software-transactional-memory built on top of
-- files. Each 'FileTVar' is backed by a file on disk plus an in-memory,
-- version-stamped cache. Transactions log their reads and writes in a 'TRec';
-- at commit time we lock the union of the read+write set, validate that every
-- value we read is still at the version we read it at, flush writes to disk
-- atomically (temp -> fsync -> rename -> fsync dir), bump versions, and unlock.
--
-- This mirrors GHC's STM commit protocol (read log + write log + validate +
-- commit), but with the filesystem as durable storage and fcntl/flock as the
-- cross-process mutual-exclusion mechanism.
--
-- Build:
--   cabal run            -- using the provided fstm-demo.cabal
-- or directly:
--   ghc -threaded -rtsopts -with-rtsopts=-N Main.hs -o fstm-demo && ./fstm-demo
--
-- Dependencies: base, bytestring, containers, directory, filepath, unix (>=2.8)
module BetterFiles (main, conflictDemo) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
  ( SomeException,
    bracket,
    bracket_,
    catch,
  )
import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    removeFile,
  )
import System.FilePath (takeDirectory, (</>))
import System.IO (SeekMode (AbsoluteSeek))
import System.Posix.Files (rename)
import System.Posix.IO
  ( LockRequest (Unlock, WriteLock),
    OpenMode (ReadOnly, ReadWrite, WriteOnly),
    closeFd,
    defaultFileFlags,
    openFd,
    setLock,
    waitToSetLock,
  )
import System.Posix.Unistd (fileSynchronise)

-- ---------------------------------------------------------------------------
-- Core types
-- ---------------------------------------------------------------------------

type Version = Int

-- | A version-stamped, file-backed transactional variable.
--
--  * 'ftvCell'       — in-memory cache: the current (version, value).
--  * 'ftvCommitLock' — an in-process mutex. fcntl locks are per-PID, so two
--                      threads in the SAME process do not exclude each other
--                      via the OS lock alone; this MVar provides intra-process
--                      mutual exclusion. The fcntl lock (see 'withOSLock')
--                      handles the cross-process case.
data FileTVar a = FileTVar
  { ftvPath :: FilePath,
    ftvCell :: MVar (Version, a),
    ftvCommitLock :: MVar (),
    ftvSerialise :: a -> ByteString,
    ftvParse :: ByteString -> a
  }

-- | A logged read: the version we observed, plus a way to re-read the current
-- version at validation time, plus the commit lock for this variable.
data ReadEntry = ReadEntry
  { reLoggedVer :: !Version,
    reCurrentVer :: IO Version,
    reLock :: MVar ()
  }

-- | A staged write: the serialised bytes to flush, an action that applies the
-- new (version, value) to the in-memory cache, and the commit lock.
data WriteEntry = WriteEntry
  { weBytes :: !ByteString,
    weBump :: IO (),
    weLock :: MVar ()
  }

-- | The transaction record: a read log and a write log, both keyed by path.
data TRec = TRec
  { trecReads :: IORef (Map.Map FilePath ReadEntry),
    trecWrites :: IORef (Map.Map FilePath WriteEntry)
  }

newTRec :: IO TRec
newTRec = TRec <$> newIORef Map.empty <*> newIORef Map.empty

-- | The transaction monad: plain IO with a 'TRec' threaded through. Kept simple
-- (no transformer stack) so the mechanics stay visible.
newtype FileTM a = FileTM {runFileTM :: TRec -> IO a}

instance Functor FileTM where
  fmap f (FileTM g) = FileTM (fmap f . g)

instance Applicative FileTM where
  pure x = FileTM (const (pure x))
  FileTM f <*> FileTM x = FileTM (\tr -> f tr <*> x tr)

instance Monad FileTM where
  FileTM x >>= f = FileTM (\tr -> x tr >>= \a -> runFileTM (f a) tr)

-- ---------------------------------------------------------------------------
-- Creating / reading variables
-- ---------------------------------------------------------------------------

-- | Create a variable. If the backing file exists, its value is loaded;
-- otherwise the file is initialised with @initial@.
newFileTVar ::
  FilePath ->
  a ->
  (a -> ByteString) ->
  (ByteString -> a) ->
  IO (FileTVar a)
newFileTVar path initial serialise parse = do
  exists <- doesFileExist path
  val <-
    if exists
      then parse <$> BS.readFile path
      else do
        atomicWriteFile path (serialise initial)
        pure initial
  cell <- newMVar (0, val)
  lock <- newMVar ()
  pure
    FileTVar
      { ftvPath = path,
        ftvCell = cell,
        ftvCommitLock = lock,
        ftvSerialise = serialise,
        ftvParse = parse
      }

-- | Transactional read. If we have already staged a write to this variable,
-- we see our own write (read-your-writes). Otherwise we read the cache and log
-- the observed version for later validation. The original read entry is kept
-- even if the variable is written later, so the dependency is validated.
readFileTVar :: FileTVar a -> FileTM a
readFileTVar ftv = FileTM $ \tr -> do
  writes <- readIORef (trecWrites tr)
  case Map.lookup (ftvPath ftv) writes of
    Just we -> pure (ftvParse ftv (weBytes we)) -- read-your-own-write
    Nothing -> do
      (ver, val) <- readMVar (ftvCell ftv)
      let entry =
            ReadEntry
              { reLoggedVer = ver,
                reCurrentVer = fst <$> readMVar (ftvCell ftv),
                reLock = ftvCommitLock ftv
              }
      -- Keep the FIRST observed version if read more than once.
      modifyIORef'
        (trecReads tr)
        (Map.insertWith (\_new old -> old) (ftvPath ftv) entry)
      pure val

-- | Transactional write. Stages the serialised value and a cache-update action.
-- Note we do NOT remove any prior read entry: the read that informed this write
-- must still be validated at commit (this is what prevents lost updates).
writeFileTVar :: FileTVar a -> a -> FileTM ()
writeFileTVar ftv newVal = FileTM $ \tr ->
  modifyIORef' (trecWrites tr) $
    Map.insert
      (ftvPath ftv)
      WriteEntry
        { weBytes = ftvSerialise ftv newVal,
          weBump = modifyMVar_ (ftvCell ftv) (\(v, _) -> pure (v + 1, newVal)),
          weLock = ftvCommitLock ftv
        }

-- | Non-transactional peek at the current value (handy for assertions).
readFileTVarIO :: FileTVar a -> IO a
readFileTVarIO ftv = snd <$> readMVar (ftvCell ftv)

-- ---------------------------------------------------------------------------
-- The commit protocol
-- ---------------------------------------------------------------------------

data Outcome = Success | Conflict deriving (Eq, Show)

commit :: TRec -> IO Outcome
commit tr = do
  reads' <- Map.toList <$> readIORef (trecReads tr)
  writes' <- Map.toList <$> readIORef (trecWrites tr)

  -- Lock the UNION of the read and write sets (deduplicated by path), in a
  -- deterministic order (sorted by path) to avoid deadlock. Locking the read
  -- set too — not just the write set — is what gives serializability and rules
  -- out write-skew, matching GHC STM's commit which acquires every TVar it
  -- touched.
  let lockMap =
        Map.fromList $
          [(p, reLock r) | (p, r) <- reads']
            ++ [(p, weLock w) | (p, w) <- writes']
      locks = sortBy (comparing fst) (Map.toList lockMap)

  withLocks locks $ do
    -- Validate: every read must still be at the version we observed.
    valid <- allM (\(_, r) -> (== reLoggedVer r) <$> reCurrentVer r) reads'
    if not valid
      then pure Conflict
      else do
        -- Flush writes to disk (atomic per file), in sorted order.
        forM_ (sortBy (comparing fst) writes') $ \(p, w) ->
          atomicWriteFile p (weBytes w)
        -- All durable; now make the in-memory caches reflect the new versions.
        forM_ writes' $ \(_, w) -> weBump w
        pure Success

-- | Acquire each (path, lock) in order: take the in-process mutex AND the
-- cross-process fcntl write lock, run the action, release both. Nested so all
-- locks are held simultaneously during validate+commit.
withLocks :: [(FilePath, MVar ())] -> IO r -> IO r
withLocks [] act = act
withLocks ((p, m) : rest) act =
  bracket_ (takeMVar m) (putMVar m ()) $
    withOSLock p $
      withLocks rest act

-- | Whole-file fcntl write lock for the duration of the action.
withOSLock :: FilePath -> IO r -> IO r
withOSLock path act =
  bracket (openFd path ReadWrite defaultFileFlags) closeFd $ \fd ->
    bracket_
      (waitToSetLock fd (WriteLock, AbsoluteSeek, 0, 0))
      (setLock fd (Unlock, AbsoluteSeek, 0, 0))
      act

-- ---------------------------------------------------------------------------
-- Atomic, durable single-file write
-- ---------------------------------------------------------------------------

-- | write temp -> fsync temp -> rename over target -> fsync directory.
-- rename(2) is atomic on POSIX; the directory fsync makes the rename itself
-- survive a crash on filesystems that need it.
atomicWriteFile :: FilePath -> ByteString -> IO ()
atomicWriteFile path content = do
  let tmp = path ++ ".tmp"
  BS.writeFile tmp content
  bracket (openFd tmp WriteOnly defaultFileFlags) closeFd fileSynchronise
  rename tmp path
  -- Directory fsync; ignore failure on filesystems that disallow it.
  ( bracket
      (openFd (takeDirectory path) ReadOnly defaultFileFlags)
      closeFd
      fileSynchronise
    )
    `catch` \(_ :: SomeException) -> pure ()

-- ---------------------------------------------------------------------------
-- Running a transaction (with conflict retry)
-- ---------------------------------------------------------------------------

-- | Run a transaction to completion, retrying on conflict. The IORef counts
-- conflicts so the demo can report contention.
atomicallyFile :: IORef Int -> FileTM a -> IO a
atomicallyFile retries tx = go
  where
    go = do
      tr <- newTRec
      a <- runFileTM tx tr -- run body, populating the logs
      out <- commit tr
      case out of
        Success -> pure a
        Conflict -> do
          atomicModifyIORef' retries (\n -> (n + 1, ()))
          threadDelay 200 -- crude backoff; see notes
          go

-- ---------------------------------------------------------------------------
-- Demo: concurrent money transfers between two file-backed accounts
-- ---------------------------------------------------------------------------

intSer :: Int -> ByteString
intSer = BS8.pack . show

intParse :: ByteString -> Int
intParse = read . BS8.unpack . BS8.takeWhile (/= '\n')

-- | Move @amt@ from one account to another, but only if funds suffice.
-- Reads both balances (logging them), then writes both. Because both reads are
-- validated at commit, two concurrent transfers that read the same starting
-- balance cannot both win — the loser sees a version bump and retries.
transfer :: FileTVar Int -> FileTVar Int -> Int -> FileTM ()
transfer from to amt = do
  bf <- readFileTVar from
  bt <- readFileTVar to
  when (bf >= amt) $ do
    writeFileTVar from (bf - amt)
    writeFileTVar to (bt + amt)

main :: IO ()
main = do
  let dir = "fstm-demo-data"
  createDirectoryIfMissing True dir
  let pa = dir </> "account_a"
      pb = dir </> "account_b"

  -- Start from a clean slate so reruns are deterministic.
  forM_ [pa, pb, pa ++ ".tmp", pb ++ ".tmp"] $ \p -> do
    e <- doesFileExist p
    when e (removeFile p)

  accA <- newFileTVar pa (500 :: Int) intSer intParse
  accB <- newFileTVar pb (500 :: Int) intSer intParse
  let total0 = 1000 :: Int

  retries <- newIORef (0 :: Int)

  let nThreads = 8
      perThread = 250 :: Int

  putStrLn $ "Starting: A=500 B=500, " ++ show nThreads ++ " threads x " ++ show perThread ++ " transfers each."

  done <- newEmptyMVar
  forM_ [1 .. nThreads] $ \tid -> forkIO $ do
    forM_ [1 .. perThread] $ \i -> do
      let amt = ((tid * 7 + i * 13) `mod` 50) + 1
          aToB = even (tid + i)
      atomicallyFile retries $ if aToB then transfer accA accB amt else transfer accB accA amt
    putMVar done ()

  forM_ [1 .. nThreads] $ \_ -> takeMVar done

  -- In-memory view
  va <- readFileTVarIO accA
  vb <- readFileTVarIO accB
  -- On-disk view (proves durability mirrors memory)
  da <- intParse <$> BS.readFile pa
  db <- intParse <$> BS.readFile pb
  r <- readIORef retries

  putStrLn ""
  putStrLn $ "In memory:  A=" ++ show va ++ "  B=" ++ show vb ++ "  total=" ++ show (va + vb)
  putStrLn $ "On disk:    A=" ++ show da ++ "  B=" ++ show db ++ "  total=" ++ show (da + db)
  putStrLn $ "Conflicts retried: " ++ show r
  putStrLn $ "Invariant " ++ (if va + vb == total0 && da + db == total0 then "HELD (total conserved)" else "VIOLATED")

-- ---------------------------------------------------------------------------
-- Tiny helper
-- ---------------------------------------------------------------------------

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM _ [] = pure True
allM p (x : xs) = do
  ok <- p x
  if ok then allM p xs else pure False

-- ---------------------------------------------------------------------------
-- Example: two threads hammering one counter, conflicts printed live
-- ---------------------------------------------------------------------------

-- | Variant of 'atomicallyFile' that logs each conflict to a Chan instead of
-- silently retrying, so the caller can observe contention in real time.
atomicallyFileLogged ::
  Chan String ->
  Int ->
  IORef Int ->
  FileTM a ->
  IO a
atomicallyFileLogged logChan tid retries tx = go (1 :: Int)
  where
    go attempt = do
      tr <- newTRec
      a <- runFileTM tx tr
      out <- commit tr
      case out of
        Success -> pure a
        Conflict -> do
          atomicModifyIORef' retries (\n -> (n + 1, ()))
          writeChan logChan $
            "  [thread "
              ++ show tid
              ++ "] conflict on attempt "
              ++ show attempt
              ++ " — retrying"
          threadDelay 100
          go (attempt + 1)

-- | Spin up two threads that each increment a shared file-backed counter
-- @n@ times (200 by default), printing every conflict as it occurs.
--
-- Usage in GHCi:
--   conflictDemo
conflictDemo :: IO ()
conflictDemo = do
  let dir = "fstm-conflict-demo"
      n = 200 :: Int
  createDirectoryIfMissing True dir
  let pc = dir </> "counter"

  -- remove any existing files for new data
  forM_ [pc, pc ++ ".tmp"] $ \p -> do
    e <- doesFileExist p
    when e (removeFile p)

  counter <- newFileTVar pc (0 :: Int) intSer intParse
  retries <- newIORef (0 :: Int)

  -- A dedicated printer thread drains the conflict log so messages appear
  -- promptly without interleaving with the worker threads' output.
  logChan <- newChan
  printerDone <- newEmptyMVar
  _ <- forkIO $ do
    let loop = do
          msg <- readChan logChan
          if msg == "__done__" then putMVar printerDone () else putStrLn msg >> loop
    loop

  putStrLn $ "conflictDemo: 2 threads x " ++ show n ++ " increments each.\n"

  -- actual work spawn
  done <- newEmptyMVar
  forM_ [1, 2] $ \tid -> forkIO $ do
    forM_ [1 .. n] $ \_ -> atomicallyFileLogged logChan tid retries $ do
      v <- readFileTVar counter
      writeFileTVar counter (v + 1)
    putMVar done ()

  takeMVar done >> takeMVar done

  -- Shut down printer and wait for it to flush.
  writeChan logChan "__done__"
  takeMVar printerDone

  v <- readFileTVarIO counter
  r <- readIORef retries
  putStrLn ""
  putStrLn $ "Final counter value : " ++ show v
  putStrLn $ "Expected (2 x " ++ show n ++ ")    : " ++ show (2 * n)
  putStrLn $ "Total conflicts     : " ++ show r
  putStrLn $ "Invariant " ++ if v == 2 * n then "HELD" else "VIOLATED (lost updates!)"
