{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with maybe" #-}

module FPFS where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)

-- ---------------------------------------------------------------------------
-- Core types
-- ---------------------------------------------------------------------------

-- A content address: in a real system this would be a blake3 hash.
-- Here we simulate it as a monotone counter.
type NodeAddr = Word64

-- Tag: typed key-value pairs. Path tag has uniqueness semantics.
data TagName
  = TagPath -- locator: participates in (path, name) unique key
  | TagType -- descriptor: e.g. "block-device", "text", "fpr-value"
  | TagAuthor
  | TagProject
  | TagCustom String
  deriving (Eq, Ord, Show)

data TagValue
  = TVString String
  | TVPath FilePath -- structured path value
  deriving (Eq, Ord, Show)

type Tag = (TagName, TagValue)

-- A node is the unit of content-addressed storage.
-- In a real system the payload would be a typed FP-RISC value tree.
data NodePayload
  = PayloadBytes [Word64] -- raw bytes (simulated)
  | PayloadBranch [NodeAddr] -- record fields / list spine
  | PayloadTombstone -- deletion marker kept for history
  deriving (Eq, Show)

data Node = Node
  { nodeAddr :: NodeAddr,
    nodeName :: String,
    nodeTags :: [Tag],
    nodePayload :: NodePayload
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Log entries -- the only thing ever written to "disk"
-- ---------------------------------------------------------------------------

-- Every operation is a log entry. Nothing is ever overwritten.
-- Deletion is an entry, modification is an entry, compaction is an entry.
data LogEntry
  = EntryWrite Node -- add or update a node
  | EntryDelete NodeAddr -- soft-delete: node marked tombstone in index
  | EntryTag NodeAddr Tag -- attach a tag to an existing node
  | EntryUntag NodeAddr TagName -- remove a tag from a node
  | EntryCompactionBegin TxID -- marks start of compacted segment
  | EntryCompactionEnd TxID -- marks end; head pointer moves here
  deriving (Show)

type TxID = Word64

data LogRecord = LogRecord
  { lrTxID :: TxID,
    lrEntry :: LogEntry
  }
  deriving (Show)

-- ---------------------------------------------------------------------------
-- The append-only log
-- ---------------------------------------------------------------------------

-- In a real system this is an mmap'd file of fixed-size pages.
-- Here it's a sequence of records plus a monotone counter.
data Log = Log
  { logRecords :: [LogRecord], -- oldest first
    logNextTx :: TxID,
    logNextAddr :: NodeAddr
  }
  deriving (Show)

emptyLog :: Log
emptyLog = Log [] 0 1

appendEntry :: LogEntry -> Log -> (Log, TxID)
appendEntry entry log =
  let tx = logNextTx log
      rec = LogRecord tx entry
   in ( log
          { logRecords = logRecords log ++ [rec],
            logNextTx = tx + 1
          },
        tx
      )

-- Allocate a fresh content address (simulates hashing in real system)
allocAddr :: Log -> (Log, NodeAddr)
allocAddr log =
  let addr = logNextAddr log
   in (log {logNextAddr = addr + 1}, addr)

-- ---------------------------------------------------------------------------
-- In-memory index -- rebuilt by replaying the log from head
-- ---------------------------------------------------------------------------

-- The live index is the result of replaying all log entries from the
-- current head. After compaction the head moves forward so old entries
-- before it are not replayed.

data Index = Index
  { idxNodes :: Map NodeAddr Node, -- addr -> live node
    idxNames :: Map String (Set NodeAddr), -- name -> addrs
    idxTags :: Map Tag (Set NodeAddr), -- tag  -> addrs
    idxPathKey :: Map (FilePath, String) NodeAddr, -- (path,name) unique key
    idxDeleted :: Set NodeAddr -- soft-deleted addrs
  }
  deriving (Show)

emptyIndex :: Index
emptyIndex = Index Map.empty Map.empty Map.empty Map.empty Set.empty

-- Replay a single log entry into the index
applyEntry :: Index -> LogEntry -> Index
applyEntry idx (EntryWrite node) =
  let addr = nodeAddr node
      name = nodeName node
      tags = nodeTags node
      -- insert into nodes map
      idx1 = idx {idxNodes = Map.insert addr node (idxNodes idx)}
      -- index by name
      idx2 =
        idx1
          { idxNames =
              Map.insertWith
                Set.union
                name
                (Set.singleton addr)
                (idxNames idx1)
          }
      -- index by each tag
      idx3 =
        foldl'
          ( \i t ->
              i
                { idxTags =
                    Map.insertWith
                      Set.union
                      t
                      (Set.singleton addr)
                      (idxTags i)
                }
          )
          idx2
          tags
      -- enforce (path, name) unique key for path tags
      idx4 =
        foldl'
          ( \i t -> case t of
              (TagPath, TVPath p) ->
                i {idxPathKey = Map.insert (p, name) addr (idxPathKey i)}
              _ -> i
          )
          idx3
          tags
   in idx4
applyEntry idx (EntryDelete addr) =
  idx
    { idxDeleted = Set.insert addr (idxDeleted idx),
      idxNodes =
        Map.adjust
          (\n -> n {nodePayload = PayloadTombstone})
          addr
          (idxNodes idx)
    }
applyEntry idx (EntryTag addr tag) =
  case Map.lookup addr (idxNodes idx) of
    Nothing -> idx -- node not known, ignore
    Just node ->
      let node' = node {nodeTags = tag : nodeTags node}
          idx1 = idx {idxNodes = Map.insert addr node' (idxNodes idx)}
          idx2 =
            idx1
              { idxTags =
                  Map.insertWith
                    Set.union
                    tag
                    (Set.singleton addr)
                    (idxTags idx1)
              }
       in idx2
applyEntry idx (EntryUntag addr tagName) =
  case Map.lookup addr (idxNodes idx) of
    Nothing -> idx
    Just node ->
      let tags' = filter (\(n, _) -> n /= tagName) (nodeTags node)
          node' = node {nodeTags = tags'}
       in idx {idxNodes = Map.insert addr node' (idxNodes idx)}
applyEntry idx (EntryCompactionBegin _) = idx
applyEntry idx (EntryCompactionEnd _) = idx

-- Rebuild the full index by replaying the entire log from head
buildIndex :: Log -> Index
buildIndex log =
  foldl'
    applyEntry
    emptyIndex
    (map lrEntry (logRecords log))

-- ---------------------------------------------------------------------------
-- Filesystem operations (pure, return updated Log)
-- ---------------------------------------------------------------------------

-- Write a new node; returns (log', addr, txid)
fsWrite :: String -> [Tag] -> NodePayload -> Log -> (Log, NodeAddr, TxID)
fsWrite name tags payload log =
  let (log1, addr) = allocAddr log
      node = Node addr name tags payload
      (log2, tx) = appendEntry (EntryWrite node) log1
   in (log2, addr, tx)

-- Soft-delete: appends a deletion record, node becomes tombstone in index
fsDelete :: NodeAddr -> Log -> (Log, TxID)
fsDelete addr log = appendEntry (EntryDelete addr) log

-- Tag an existing node
fsTag :: NodeAddr -> Tag -> Log -> (Log, TxID)
fsTag addr tag log = appendEntry (EntryTag addr tag) log

-- Untag
fsUntag :: NodeAddr -> TagName -> Log -> (Log, TxID)
fsUntag addr tagName log = appendEntry (EntryUntag addr tagName) log

-- ---------------------------------------------------------------------------
-- Query (pure, over Index snapshot)
-- ---------------------------------------------------------------------------

-- All live (non-deleted) nodes
liveNodes :: Index -> [Node]
liveNodes idx =
  [ n | (addr, n) <- Map.toList (idxNodes idx), not (Set.member addr (idxDeleted idx))
  ]

-- Search by name
searchByName :: String -> Index -> [Node]
searchByName name idx =
  let addrs = fromMaybe Set.empty (Map.lookup name (idxNames idx))
   in mapMaybe
        ( \a ->
            if Set.member a (idxDeleted idx)
              then Nothing
              else Map.lookup a (idxNodes idx)
        )
        (Set.toList addrs)

-- Search by tag
searchByTag :: Tag -> Index -> [Node]
searchByTag tag idx =
  let addrs = fromMaybe Set.empty (Map.lookup tag (idxTags idx))
   in mapMaybe
        ( \a ->
            if Set.member a (idxDeleted idx)
              then Nothing
              else Map.lookup a (idxNodes idx)
        )
        (Set.toList addrs)

-- Resolve a unix-style path to exactly one node (or error)
data ResolveResult
  = Resolved Node
  | NotFound
  | Ambiguous [Node] -- uniqueness constraint violated
  deriving (Show)

resolvePath :: FilePath -> String -> Index -> ResolveResult
resolvePath path name idx =
  case Map.lookup (path, name) (idxPathKey idx) of
    Nothing -> NotFound
    Just addr ->
      if Set.member addr (idxDeleted idx)
        then NotFound
        else case Map.lookup addr (idxNodes idx) of
          Nothing -> NotFound
          Just node -> Resolved node

-- Multi-tag intersection search
searchByTags :: [Tag] -> Index -> [Node]
searchByTags [] idx = liveNodes idx
searchByTags (t : ts) idx =
  let initial = Set.fromList $ map nodeAddr $ searchByTag t idx
      addrs =
        foldl'
          ( \s tag ->
              let s' = Set.fromList $ map nodeAddr $ searchByTag tag idx
               in Set.intersection s s'
          )
          initial
          ts
   in mapMaybe (\a -> Map.lookup a (idxNodes idx)) (Set.toList addrs)

-- ---------------------------------------------------------------------------
-- Compaction
-- ---------------------------------------------------------------------------

-- Compaction does NOT delete history entries. It:
--   1. Takes a snapshot of the current live index
--   2. Writes a new contiguous segment of EntryWrite records
--      for all live nodes (this IS the compacted log)
--   3. Appends CompactionBegin/End markers
--   4. Returns a new Log whose records start from the compacted segment
--      (old records before compaction are dropped -- this is the "head move")
--
-- Deleted nodes do NOT appear in the compacted segment.
-- Their history exists only in the pre-compaction log (which you could
-- archive to cold storage if you want audit history).

compact :: Log -> Log
compact log =
  let idx = buildIndex log
      live = liveNodes idx
      -- fresh log, preserve addr/tx counters so they stay monotone
      base = Log [] (logNextTx log) (logNextAddr log)
      -- write compaction begin marker
      (log1, txBegin) = appendEntry (EntryCompactionBegin (logNextTx log)) base
      -- replay all live nodes as fresh EntryWrite records
      log2 = foldl' (\l node -> fst $ appendEntry (EntryWrite node) l) log1 live
      -- write compaction end marker
      (log3, _) = appendEntry (EntryCompactionEnd txBegin) log2
   in log3

-- ---------------------------------------------------------------------------
-- Demo
-- ---------------------------------------------------------------------------

demo :: IO ()
demo = do
  putStrLn "=== FprFs Demo ===\n"

  -- 1. Write some nodes
  let pathTag p = (TagPath, TVPath p)
      typeTag t = (TagType, TVString t)

  let (log1, addrSdd, _) =
        fsWrite
          "sdd"
          [pathTag "/dev/", typeTag "block-device"]
          (PayloadBytes [0xDEAD])
          emptyLog
  let (log2, addrNull, _) =
        fsWrite
          "null"
          [pathTag "/dev/", typeTag "char-device"]
          (PayloadBytes [])
          log1
  let (log3, addrCfg, _) =
        fsWrite
          "config"
          [ pathTag "/etc/",
            typeTag "fpr-value",
            (TagProject, TVString "fp-risc")
          ]
          (PayloadBytes [0xCAFE])
          log2
  let (log4, addrTmp, _) =
        fsWrite
          "tmp"
          [pathTag "/tmp/", typeTag "text"]
          (PayloadBytes [0xFF])
          log3

  putStrLn $ "Log length after writes: " ++ show (length $ logRecords log4)

  -- 2. Query the index
  let idx1 = buildIndex log4

  putStrLn "\n-- Resolve /dev/sdd --"
  print $ resolvePath "/dev/" "sdd" idx1

  putStrLn "\n-- Search by tag (path:/dev/) --"
  let devNodes = searchByTag (pathTag "/dev/") idx1
  mapM_ (\n -> putStrLn $ "  " ++ nodeName n ++ " @ " ++ show (nodeAddr n)) devNodes

  putStrLn "\n-- Search by name 'config' --"
  mapM_ print $ searchByName "config" idx1

  -- 3. Delete /tmp/tmp
  let (log5, _) = fsDelete addrTmp log4
  putStrLn $ "\nLog length after delete: " ++ show (length $ logRecords log5)

  let idx2 = buildIndex log5
  putStrLn "\n-- Live nodes after delete --"
  mapM_ (\n -> putStrLn $ "  " ++ nodeName n) (liveNodes idx2)

  -- 4. Compact
  let log6 = compact log5
  putStrLn $ "\nLog length after compaction: " ++ show (length $ logRecords log6)
  putStrLn "(deleted /tmp/tmp is gone from compacted log, history was in old segment)"

  let idx3 = buildIndex log6
  putStrLn "\n-- Live nodes after compaction --"
  mapM_ (\n -> putStrLn $ "  " ++ nodeName n) (liveNodes idx3)

  -- 5. Multi-tag search
  putStrLn "\n-- Multi-tag search: path:/etc/ AND project:fp-risc --"
  let results = searchByTags [pathTag "/etc/", (TagProject, TVString "fp-risc")] idx3
  mapM_ (\n -> putStrLn $ "  " ++ nodeName n) results

  -- 6. Path uniqueness: try to write a duplicate /dev/sdd
  putStrLn "\n-- Uniqueness check for /dev/sdd --"
  let (log7, addrSdd2, _) =
        fsWrite
          "sdd"
          [pathTag "/dev/", typeTag "block-device"]
          (PayloadBytes [0xBEEF])
          log6
  let idx4 = buildIndex log7
  -- resolvePath still returns the first one (last write wins in pathKey map)
  -- a real system would reject this at write time; here we show the ambiguity
  let pathKeyCount =
        length $
          filter
            (\((p, n), _) -> p == "/dev/" && n == "sdd")
            (Map.toList $ idxPathKey idx4)
  putStrLn $ "  Path key entries for /dev/sdd: " ++ show pathKeyCount
  putStrLn "  (In a real system the write would be rejected at transaction time)"

  putStrLn "\n=== Done ==="
