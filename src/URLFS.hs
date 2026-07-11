{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

-- LogFS: the persistence layer under UrlFs.
--
-- Architecture modelled:
--
--   disk (single append-only stream)
--     |-- DObj h bytes      content-addressed object            (data plane)
--     |-- DRec record       log record: Write path caps h | Unbind path
--     v
--   replay  ::  fold, last-wins per path, torn tail discarded
--     v
--   Index   ::  Map Path (CapSet, Hash)        (small, rebuildable)
--     v
--   mount   ::  Index -> trie of BlobRef Hash  (routing structure only;
--               data stays in the store, actors are runtime-only overlays)
--
-- Protocols compared:
--   GOOD: append DObj, then DRec        (record never precedes its object)
--   BAD : append DRec, then DObj        (window where a crash danglles a hash)
--
-- Invariants checked (over generated op sequences and EVERY crash prefix):
--   I1 last-wins        full replay gives, per path, the last surviving write
--   I2 prefix-validity  recovery from any crash cut == state after the
--                       longest whole-record prefix (no partial application)
--   I3 no-dangling      GOOD: every hash in the index resolves in the store,
--                       at every crash cut. BAD: a witness cut exists.
--   I4 ls/resolve       after mount, no path is listed that resolve denies
--                       (single canSee predicate, same as UrlFs)
--   I5 ephemerality     runtime Actor binds never survive a remount
--   I6 idempotence      replaying a disk twice yields the same index

module URLFS where

import Data.Bits (shiftL, xor)
import Data.Char (ord)
import Data.List (foldl', intercalate, isPrefixOf)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Word (Word64)

--------------------------------------------------------------------------------
-- Paths and capabilities (as in UrlFs)
--------------------------------------------------------------------------------

type Seg = String

type Path = [Seg]

parse :: String -> Path
parse = filter (not . null) . splitOn '/'
  where
    splitOn c s = case break (== c) s of
      (a, []) -> [a]
      (a, _ : b) -> a : splitOn c b

render :: Path -> String
render p = '/' : intercalate "/" p

newtype Cap = Cap String deriving (Eq, Ord, Show)

type CapSet = S.Set Cap

canSee :: CapSet -> CapSet -> Bool
canSee caller required = required `S.isSubsetOf` caller

--------------------------------------------------------------------------------
-- Content-addressed store
--------------------------------------------------------------------------------

newtype Hash = Hash Word64 deriving (Eq, Ord, Show)

-- FNV-1a; stands in for whatever real hash the object store uses.
hashOf :: String -> Hash
hashOf = Hash . foldl' step 0xcbf29ce484222325
  where
    step h c = (h `xor` fromIntegral (ord c)) * 0x100000001b3

type Store = M.Map Hash String

--------------------------------------------------------------------------------
-- The disk: one append-only stream of framed entries
--------------------------------------------------------------------------------

data Record
  = RWrite Path CapSet Hash
  | RUnbind Path
  deriving (Eq, Show)

data DiskEntry
  = DObj Hash String -- object frame (data plane)
  | DRec Record -- log frame  (metadata plane)
  | DTorn -- a torn/partial frame at the tail after a crash
  deriving (Eq, Show)

type Disk = [DiskEntry]

-- Crash model: a crash cuts the stream after k whole frames, optionally
-- leaving a torn frame. Recovery must treat DTorn as end-of-log.
crashCuts :: Disk -> [Disk]
crashCuts d = concat [[pre, pre ++ [DTorn]] | pre <- prefixes d]
  where
    prefixes xs = map (`take` xs) [0 .. length xs]

--------------------------------------------------------------------------------
-- Replay: disk -> (Store, Index)
--------------------------------------------------------------------------------

type Index = M.Map Path (CapSet, Hash)

replay :: Disk -> (Store, Index)
replay = foldl' step (M.empty, M.empty) . takeWhile (/= DTorn)
  where
    step (st, ix) = \case
      DObj h bytes -> (M.insert h bytes st, ix)
      DRec (RWrite p caps h) -> (st, M.insert p (caps, h) ix)
      DRec (RUnbind p) -> (st, M.delete p ix)
      DTorn -> (st, ix) -- unreachable (takeWhile), kept total

--------------------------------------------------------------------------------
-- Write protocols
--------------------------------------------------------------------------------

data Op
  = OpWrite String [Cap] String -- url, caps, content
  | OpUnbind String
  deriving (Show)

-- GOOD: object lands on disk before the record that references it.
appendGood :: Disk -> Op -> Disk
appendGood d = \case
  OpWrite url caps bytes ->
    let h = hashOf bytes
     in d ++ [DObj h bytes, DRec (RWrite (parse url) (S.fromList caps) h)]
  OpUnbind url -> d ++ [DRec (RUnbind (parse url))]

-- BAD: record first. A crash between the two frames leaves the index
-- pointing at a hash the store has never seen.
appendBad :: Disk -> Op -> Disk
appendBad d = \case
  OpWrite url caps bytes ->
    let h = hashOf bytes
     in d ++ [DRec (RWrite (parse url) (S.fromList caps) h), DObj h bytes]
  OpUnbind url -> d ++ [DRec (RUnbind (parse url))]

buildDisk :: (Disk -> Op -> Disk) -> [Op] -> Disk
buildDisk proto = foldl' proto []

--------------------------------------------------------------------------------
-- The trie (UrlFs, with Blob String -> BlobRef Hash)
--------------------------------------------------------------------------------

data Resource
  = BlobRef Hash -- persistent: data lives in the store
  | Actor Service -- runtime-only: dynamic namespace

data Service = Service
  { svcName :: String,
    svcResolve :: Path -> Maybe Resource,
    svcList :: Path -> Maybe [Seg]
  }

data Bound = Bound {bRes :: Resource, bReq :: CapSet}

data Node = Node
  { nRes :: Maybe Bound,
    nKids :: M.Map Seg Node
  }

emptyN :: Node
emptyN = Node Nothing M.empty

bindP :: Path -> CapSet -> Resource -> Node -> Node
bindP path caps r = go path
  where
    b = Bound r caps
    go [] n = n {nRes = Just b}
    go (s : ss) n = n {nKids = M.alter (Just . go ss . fromMaybe emptyN) s (nKids n)}

bind :: String -> [Cap] -> Resource -> Node -> Node
bind url caps = bindP (parse url) (S.fromList caps)

-- Mount: the trie is a pure function of the index. Nothing else persists.
mount :: Index -> Node
mount = M.foldrWithKey (\p (caps, h) -> bindP p caps (BlobRef h)) emptyN

resolve :: CapSet -> String -> Node -> Maybe Resource
resolve caller url root = go (parse url) root
  where
    go [] n = do
      Bound r req <- nRes n
      if canSee caller req then Just r else Nothing
    go p@(s : ss) n =
      case M.lookup s (nKids n) of
        Just child -> go ss child
        Nothing -> case nRes n of
          Just (Bound (Actor svc) req) | canSee caller req -> svcResolve svc p
          _ -> Nothing

anyVisible :: CapSet -> Node -> Bool
anyVisible c n =
  maybe False (canSee c . bReq) (nRes n)
    || any (anyVisible c) (M.elems (nKids n))

data Kind = HasRes | HasKids | Both deriving (Eq, Show)

ls :: CapSet -> String -> Node -> Maybe [(Seg, Kind)]
ls caller url root = go (parse url) root
  where
    go [] n =
      let static =
            [ (s, kindOf child)
              | (s, child) <- M.toList (nKids n),
                anyVisible caller child
            ]
          dynamic = case nRes n of
            Just (Bound (Actor svc) req)
              | canSee caller req ->
                  maybe [] (map (\s -> (s, HasRes))) (svcList svc [])
            _ -> []
       in Just (static ++ dynamic)
    go p@(s : ss) n =
      case M.lookup s (nKids n) of
        Just child -> go ss child
        Nothing -> case nRes n of
          Just (Bound (Actor svc) req) | canSee caller req -> map (\seg -> (seg, HasRes)) <$> svcList svc p
          _ -> Nothing

    kindOf child =
      case (nRes child, M.null (nKids child)) of
        (Just _, True) -> HasRes
        (Just _, False) -> Both
        (Nothing, _) -> HasKids

-- File read: resolve -> BlobRef -> one store lookup. The "fold over the
-- log" happened once, at mount; reads are index+store, not log scans.
readF :: CapSet -> String -> Node -> Store -> Maybe String
readF caller url root st =
  resolve caller url root >>= \case
    BlobRef h -> M.lookup h st
    Actor _ -> Nothing

--------------------------------------------------------------------------------
-- Reference semantics for I1/I2 (what the fold SHOULD compute)
--------------------------------------------------------------------------------

refIndex :: [Op] -> Index
refIndex = foldl' step M.empty
  where
    step ix = \case
      OpWrite url caps bytes -> M.insert (parse url) (S.fromList caps, hashOf bytes) ix
      OpUnbind url -> M.delete (parse url) ix

--------------------------------------------------------------------------------
-- Deterministic op-sequence generator (tiny LCG; no external deps)
--------------------------------------------------------------------------------

lcg :: Word64 -> Word64
lcg x = x * 6364136223846793005 + 1442695040888963407

rands :: Word64 -> [Word64]
rands = tail . iterate lcg

genOps :: Word64 -> Int -> [Op]
genOps seed n = take n (go (rands seed))
  where
    urls =
      [ "/readme",
        "/etc/motd",
        "/project/x/notes",
        "/project/x/build/out",
        "/project/y/notes",
        "/a/very/deep/leaf"
      ]
    capsets = [[], [Cap "x"], [Cap "x", Cap "y"]]
    contents = ["v1", "v2", "hello", "world", "payload", ""]
    pick xs r = xs !! fromIntegral (r `mod` fromIntegral (length xs))
    go (r1 : r2 : r3 : r4 : rest)
      | r1 `mod` 4 == 0 = OpUnbind (pick urls r2) : go rest
      | otherwise = OpWrite (pick urls r2) (pick capsets r3) (pick contents r4) : go rest
    go _ = []

--------------------------------------------------------------------------------
-- Invariant checks
--------------------------------------------------------------------------------

-- I1: full replay of a GOOD disk == reference last-wins semantics,
--     and every indexed hash resolves in the store.
i1 :: [Op] -> Bool
i1 ops =
  let (st, ix) = replay (buildDisk appendGood ops)
   in ix == refIndex ops
        && all (\(_, h) -> M.member h st) (M.elems ix)

-- I2: for EVERY crash cut of a GOOD disk, the recovered index equals the
--     index of some whole-op prefix of the op sequence -- i.e. recovery is
--     always a state the system legitimately passed through. An orphan
--     trailing DObj is garbage, not corruption: it is invisible to the
--     index. No cut may expose a half-applied op.
i2 :: [Op] -> Bool
i2 ops =
  let d = buildDisk appendGood ops
      legit = S.fromList [refIndex (take k ops) | k <- [0 .. length ops]]
   in all (\cut -> snd (replay cut) `S.member` legit) (crashCuts d)

-- I3a: GOOD protocol -- at every crash cut, no index entry dangles.
i3good :: [Op] -> Bool
i3good ops = all noDangle (crashCuts (buildDisk appendGood ops))
  where
    noDangle cut =
      let (st, ix) = replay cut
       in all (\(_, h) -> M.member h st) (M.elems ix)

-- I3b: BAD protocol -- there EXISTS a crash cut with a dangling hash,
--      whenever the op sequence contains at least one write.
i3bad :: [Op] -> Bool
i3bad ops
  | not (any isWrite ops) = True -- vacuous: nothing to dangle
  | otherwise = any hasDangle (crashCuts (buildDisk appendBad ops))
  where
    isWrite OpWrite {} = True
    isWrite _ = False
    hasDangle cut =
      let (st, ix) = replay cut
       in any (\(_, h) -> not (M.member h st)) (M.elems ix)

-- I4: mount a recovered index, overlay a runtime actor, and confirm that
--     every path whose own resource ls advertises (HasRes/Both from static
--     structure) is resolvable by that caller, and every HasKids listing is
--     justified by a visible descendant. Same single canSee predicate as
--     UrlFs, now checked against log-recovered state.
i4 :: [Op] -> Bool
i4 ops =
  let (_, ix) = replay (buildDisk appendGood ops)
      root = bind "/svc/usb" [Cap "hw"] (Actor usbService) (mount ix)
      callers = [S.empty, S.fromList [Cap "x"], S.fromList [Cap "x", Cap "y", Cap "hw"]]
   in and [ok c p root | c <- callers, p <- allPaths [] root]
  where
    allPaths p n = p : concat [allPaths (p ++ [s]) c | (s, c) <- M.toList (nKids n)]
    nodeAt [] n = Just n
    nodeAt (s : ss) n = M.lookup s (nKids n) >>= nodeAt ss
    ok c p root' = case ls c (render p) root' of
      Nothing -> True
      Just entries -> all good entries
        where
          good (s, k) = case nodeAt (p ++ [s]) root' of
            Nothing -> True -- dynamic entry synthesized by a visible actor
            Just child -> case k of
              HasKids -> anyVisible c child
              _ -> case nRes child of
                Nothing -> True
                Just (Bound _ req) ->
                  -- advertised own resource must be visible, OR the entry
                  -- was justified by visible descendants (Both case)
                  canSee c req || (k == Both && any (anyVisible c) (M.elems (nKids child)))

-- I5: runtime Actor binds never persist. Overlay an actor, "reboot"
--     (remount from disk): the actor is gone; all blob state is intact.
i5 :: [Op] -> Bool
i5 ops =
  let d = buildDisk appendGood ops
      (_, ix) = replay d
      _live = bind "/svc/usb" [] (Actor usbService) (mount ix) -- pre-crash overlay
      remounted = mount (snd (replay d)) -- reboot: trie is a pure fn of disk
      actorSurvives = case resolve full "/svc/usb" remounted of
        Just (Actor _) -> True
        _ -> False
      blobsIntact =
        all
          ( \(p, (_, h)) -> case resolve full (render p) remounted of
              Just (BlobRef h') -> h == h'
              _ -> False
          )
          (M.toList ix)
   in not actorSurvives && blobsIntact
  where
    full = S.fromList [Cap "x", Cap "y", Cap "hw"]

-- I6: replay is a deterministic pure fold.
i6 :: [Op] -> Bool
i6 ops = let d = buildDisk appendGood ops in replay d == replay d

--------------------------------------------------------------------------------
-- Demo service (runtime overlay used in I4/I5)
--------------------------------------------------------------------------------

usbService :: Service
usbService =
  Service
    { svcName = "usb",
      svcResolve = \case
        [dev, "status"] | dev `elem` ["0", "1"] -> Just (BlobRef (hashOf ("usb" ++ dev)))
        _ -> Nothing,
      svcList = \case
        [] -> Just ["0", "1"]
        [dev] | dev `elem` ["0", "1"] -> Just ["status"]
        _ -> Nothing
    }

--------------------------------------------------------------------------------
-- Runner
--------------------------------------------------------------------------------

runInvariant :: String -> ([Op] -> Bool) -> [[Op]] -> IO Bool
runInvariant name prop cases = do
  let failures = [i | (i, ops) <- zip [0 :: Int ..] cases, not (prop ops)]
  case failures of
    [] -> putStrLn ("  PASS  " ++ name ++ "  (" ++ show (length cases) ++ " cases)") >> pure True
    (i : _) -> putStrLn ("  FAIL  " ++ name ++ "  first failing case #" ++ show i) >> pure False

main :: IO ()
main = do
  let seeds = [1 .. 60]
      sizes = cycle [0, 1, 2, 3, 5, 8, 12]
      cases = [genOps s n | (s, n) <- zip seeds sizes]

  putStrLn "== LogFS invariants (generated op sequences, all crash cuts) =="
  rs <-
    sequence
      [ runInvariant "I1  last-wins fold matches reference semantics" i1 cases,
        runInvariant "I2  every crash cut recovers a legitimate state" i2 cases,
        runInvariant "I3a GOOD ordering: no dangling hash at any cut" i3good cases,
        runInvariant "I3b BAD ordering: dangling-hash witness exists" i3bad cases,
        runInvariant "I4  ls/resolve single-predicate consistency" i4 cases,
        runInvariant "I5  actors are runtime-only; blobs survive boot" i5 cases,
        runInvariant "I6  replay deterministic" i6 cases
      ]

  putStrLn ""
  putStrLn "== Walkthrough: one disk, one crash, one remount =="
  let ops =
        [ OpWrite "/etc/motd" [] "hello",
          OpWrite "/project/x/notes" [Cap "x"] "plan A",
          OpWrite "/project/x/notes" [Cap "x"] "plan B",
          OpUnbind "/etc/motd",
          OpWrite "/a/very/deep/leaf" [] "no ceremony"
        ]
      disk = buildDisk appendGood ops
      (st, ix) = replay disk
      root = bind "/svc/usb" [] (Actor usbService) (mount ix)
      anon = S.empty
      dev = S.fromList [Cap "x"]

  putStrLn $ "  disk frames: " ++ show (length disk)
  putStrLn $ "  index size : " ++ show (M.size ix) ++ "  (motd unbound; notes deduped last-wins)"
  putStrLn $ "  read /project/x/notes [dev ] -> " ++ show (readF dev "/project/x/notes" root st)
  putStrLn $ "  read /project/x/notes [anon] -> " ++ show (readF anon "/project/x/notes" root st)
  putStrLn $ "  read /etc/motd        [anon] -> " ++ show (readF anon "/etc/motd" root st)
  putStrLn $ "  ls   /svc/usb (runtime actor) -> " ++ show (ls anon "/svc/usb" root)

  let torn = take (length disk - 1) disk ++ [DTorn]
      (_, ixT) = replay torn
  putStrLn $ "  crash before final record: index size " ++ show (M.size ixT) ++ "  (leaf write lost cleanly, not corrupted)"

  putStrLn ""
  if and rs then putStrLn "ALL INVARIANTS PASS" else putStrLn "SOME INVARIANTS FAILED"
