{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- MVUVfs: URL-addressed VFS discovery over one append-only disk whose
-- traffic is mostly MVU application logs.
--
--   disk (single logical append stream, typed frames)
--     |-- FObj h bytes            CAS object            (file plane)
--     |-- FIdx rec                VFS index record      (file plane)
--     |-- FDeltaG [op]/FDeltaS [op]  ONE frame per durable MVU msg
--     |-- FSnapChunk app n        snapshot payload (compressed, paged)
--     |-- FCommitG/FCommitS m     atomic snapshot commit marker
--     v
--   boot = ONE fold:  Store + Index + per-app models (last commit + tail)
--     v
--   namespace (trie):
--     /etc, /cfg, ...      BlobRef        <- index -> CAS      (files)
--     /apps/grades/...     Actor          <- live GModel       (logic)
--     /apps/shop/...       Actor          <- live SModel       (logic)
--     /services/<name>     Actor          forwards via process table
--     /proc                Actor          the (pid,aid) table itself
--     /dev/flash0          BlobVal        device geometry
--
-- Frame atomicity doctrine (lesson from the rename probe): the frame is
-- the crash-atomic unit, so every logical operation serializes to exactly
-- ONE frame. A BulkPrices msg with 500 changed entries is one FDeltaS
-- frame, not 500. Snapshots span many chunk frames but are invisible to
-- recovery until their single commit frame lands.
--
-- Invariants:
--   I1 VFS index fold is last-wins (matches reference semantics)
--   I2 full boot reproduces live state exactly (models, index, objects)
--   I3 EVERY crash cut boots to a legitimate prefix state, per app --
--      including cuts inside a bulk msg or mid-snapshot
--   I4 ephemeral msgs leave no trace on disk
--   I5 watermark GC (drop frames below last commit) never changes boot
--   I6 ls never advertises what resolve denies (single canSee predicate)
--   I7 /services/<x> answers identically to /apps/<x> (alias via proc table)

module MVULog where

import Data.Bits (shiftL, xor, (.&.))
import qualified Data.ByteString.Char8 as B
import Data.Char (ord)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Word (Word64)
import System.CPUTime
import Text.Printf
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- Paths and capabilities
--------------------------------------------------------------------------------

type Seg = String

type Path = [Seg]

parseP :: String -> Path
parseP = filter (not . null) . splitOn '/'
  where
    splitOn c s = case break (== c) s of
      (a, []) -> [a]
      (a, _ : b) -> a : splitOn c b

renderP :: Path -> String
renderP p = '/' : intercalate "/" p

newtype Cap = Cap String deriving (Eq, Ord, Show)

type CapSet = S.Set Cap

canSee :: CapSet -> CapSet -> Bool
canSee caller required = required `S.isSubsetOf` caller

--------------------------------------------------------------------------------
-- Content-addressed store
--------------------------------------------------------------------------------

newtype Hash = Hash Word64 deriving (Eq, Ord, Show)

hashOf :: String -> Hash
hashOf = Hash . foldl' step 0xcbf29ce484222325
  where
    step h c = (h `xor` fromIntegral (ord c)) * 0x100000001b3

type Store = M.Map Hash String

--------------------------------------------------------------------------------
-- MVU applications: two apps, two models, durable vs ephemeral msgs
--------------------------------------------------------------------------------

data Student = Student {sName :: !String, sAge :: !Int, sScore :: !Int}
  deriving (Eq, Ord, Show)

type GModel = M.Map Int Student -- "grades" app

type SModel = M.Map Int Int -- "shop" app (prices)

data GMsg = AddStudent !Int !String !Int | SetAge !Int !Int | SetScore !Int !Int
  deriving (Eq, Show)

newtype SMsg = BulkPrices (M.Map Int Int) deriving (Eq, Show)

updateG :: GMsg -> GModel -> GModel
updateG (AddStudent i n a) = M.insert i (Student n a 0)
updateG (SetAge i a) = M.adjust (\s -> s {sAge = a}) i
updateG (SetScore i v) = M.adjust (\s -> s {sScore = v}) i

updateS :: SMsg -> SModel -> SModel
updateS (BulkPrices ps) = M.union ps -- ps overrides

-- Structural delta ops: path + new value only. For BulkPrices this is a
-- REAL diff against the previous model -- the payload may carry 500
-- entries, the log carries only what changed.
data GOp = GNew !Int !Student | GAge !Int !Int | GScore !Int !Int
  deriving (Eq, Show)

data SOp = SPrice !Int !Int deriving (Eq, Show)

deltaG :: GMsg -> GModel -> [GOp]
deltaG msg g = case msg of
  AddStudent i n a -> [GNew i (Student n a 0)]
  SetAge i a -> [GAge i a | M.member i g]
  SetScore i v -> [GScore i v | M.member i g]

deltaS :: SMsg -> SModel -> [SOp]
deltaS (BulkPrices ps) old =
  [SPrice k v | (k, v) <- M.toList ps, M.lookup k old /= Just v]

applyG :: GOp -> GModel -> GModel
applyG = \case
  GNew i s -> M.insert i s
  GAge i a -> M.adjust (\s -> s {sAge = a}) i
  GScore i v -> M.adjust (\s -> s {sScore = v}) i

applyS :: SOp -> SModel -> SModel
applyS (SPrice k v) = M.insert k v

--------------------------------------------------------------------------------
-- The disk: one append-only stream of typed frames
--------------------------------------------------------------------------------

data AppId = Grades | Shop deriving (Eq, Ord, Show)

data IdxRec = RWrite !Path !CapSet !Hash | RUnbind !Path deriving (Eq, Show)

data Frame
  = FObj !Hash !String -- CAS object (before its FIdx: object-first ordering)
  | FIdx !IdxRec -- VFS index record
  | FDeltaG ![GOp] -- ONE frame per durable grades msg
  | FDeltaS ![SOp] -- ONE frame per durable shop msg
  | FSnapChunk !AppId !Int -- snapshot payload chunk (bytes; content abstracted)
  | FCommitG !GModel -- atomic snapshot commit (tiny frame on real flash;
  | FCommitS !SModel --   carries the model value here for simulation)
  | FTorn -- torn frame at the tail after a crash
  deriving (Eq, Show)

type Disk = [Frame]

frameBytes :: Frame -> Int
frameBytes = \case
  FObj _ b -> 24 + length b
  FIdx r -> 12 + length (show r)
  FDeltaG ops -> 8 + sum ((+ 6) . length . show <$> ops)
  FDeltaS ops -> 8 + sum ((+ 6) . length . show <$> ops)
  FSnapChunk _ n -> n
  FCommitG _ -> 32 -- seq + model-root + crc; NOT the model bytes
  FCommitS _ -> 32
  FTorn -> 0

crashCuts :: Disk -> [Disk]
crashCuts d = concat [[pre, pre ++ [FTorn]] | k <- [0 .. length d], let pre = take k d]

--------------------------------------------------------------------------------
-- Flash simulator (pages, blocks, write amplification, watermark GC)
--------------------------------------------------------------------------------

pageSize, pageHdr, pagesPerBlock :: Int
pageSize = 4096
pageHdr = 16
pagesPerBlock = 64

payloadPerPage :: Int
payloadPerPage = pageSize - pageHdr

data Flash = Flash
  { fLogical :: !Int,
    fPages :: !Int,
    fErases :: !Int,
    fFill :: !Int,
    fBlocks :: ![(Int, Int)],
    fBlockPg :: !Int,
    fBlkFirst :: !Int,
    fFreed :: !Int
  }
  deriving (Show)

flash0 :: Flash
flash0 = Flash 0 0 0 0 [] 0 0 0

appendBytes :: Int -> Flash -> Flash
appendBytes n f0 = go n f0 {fLogical = fLogical f0 + n}
  where
    go 0 f = f
    go k f =
      let room = payloadPerPage - fFill f
       in if k < room
            then f {fFill = fFill f + k}
            else go (k - room) (closePage f {fFill = 0})

closePage :: Flash -> Flash
closePage f =
  let f1 = f {fPages = fPages f + 1, fBlockPg = fBlockPg f + 1}
   in if fBlockPg f1 == pagesPerBlock
        then f1 {fBlocks = fBlocks f1 ++ [(fBlkFirst f1, fPages f1 - 1)], fBlockPg = 0, fBlkFirst = fPages f1}
        else f1

flushPartial :: Flash -> Flash
flushPartial f
  | fFill f == 0 = f
  | otherwise = closePage f {fFill = 0}

gcFlash :: Int -> Flash -> Flash
gcFlash wm f =
  let (dead, live) = span (\(_, hi) -> hi < wm) (fBlocks f)
   in f {fBlocks = live, fErases = fErases f + length dead, fFreed = fFreed f + length dead}

writeAmp :: Flash -> Double
writeAmp f
  | fLogical f == 0 = 0
  | otherwise = fromIntegral (fPages f * pageSize) / fromIntegral (fLogical f)

--------------------------------------------------------------------------------
-- LZ-style size estimator (hash-chain over 4-grams)
--------------------------------------------------------------------------------

compressedSize :: B.ByteString -> Int
compressedSize bs = go 0 IM.empty 0 0
  where
    n = B.length bs
    hash4 i =
      ( ord (B.index bs i) `shiftL` 24
          `xor` ord (B.index bs (i + 1)) `shiftL` 16
          `xor` ord (B.index bs (i + 2)) `shiftL` 8
          `xor` ord (B.index bs (i + 3))
      )
        .&. 0xFFFFF
    matchLen a b !acc
      | a < n && b < n && acc < 255 && B.index bs a == B.index bs b = matchLen (a + 1) (b + 1) (acc + 1)
      | otherwise = acc
    go !i !tbl !out !toks
      | i + 4 > n = final (out + (n - i)) (toks + (n - i))
      | otherwise =
          let h = hash4 i
              cand = IM.lookup h tbl
              tbl' = IM.insert h i tbl
           in case cand of
                Just j | let ml = matchLen j i 0, ml >= 6 -> go (i + ml) tbl' (out + 3) (toks + 1)
                _ -> go (i + 1) tbl' (out + 1) (toks + 1)
    final o t = o + (t `div` 8) + 1

--------------------------------------------------------------------------------
-- Workload events
--------------------------------------------------------------------------------

data Evt
  = EvtG !GMsg -- durable, grades log
  | EvtS !SMsg -- durable, shop log (big payload, small diff)
  | EvtFile !String ![Cap] !String -- durable, file plane (CAS + index)
  | EvtUnbind !String -- durable, file plane
  | EvtScroll !Int -- ephemeral: never reaches disk
  deriving (Eq, Show)

isScroll :: Evt -> Bool
isScroll (EvtScroll _) = True
isScroll _ = False

lcg :: Word64 -> Word64
lcg x = x * 6364136223846793005 + 1442695040888963407

genEvts :: Word64 -> Int -> [Evt]
genEvts seed total = go 0 (lcg seed) (0 :: Int)
  where
    urls = ["/etc/motd", "/cfg/app", "/project/x/notes", "/a/deep/leaf"]
    capsets = [[], [Cap "x"]]
    contents = ["v1", "v2", "hello", "config-payload", ""]
    pick xs r = xs !! fromIntegral (r `mod` fromIntegral (length xs))
    go !k !r !nStu
      | k >= total = []
      | otherwise =
          let r1 = lcg r
              r2 = lcg r1
              r3 = lcg r2
              r4 = lcg r3
              roll = r1 `mod` 100
              nxt = go (k + 1) r4
           in if
                | roll < 18 || nStu == 0 ->
                    EvtG (AddStudent nStu ("student-" ++ show nStu) (18 + fromIntegral (r2 `mod` 40))) : nxt (nStu + 1)
                | roll < 42 -> EvtG (SetAge (fromIntegral (r2 `mod` fromIntegral nStu)) (18 + fromIntegral (r3 `mod` 50))) : nxt nStu
                | roll < 58 -> EvtG (SetScore (fromIntegral (r2 `mod` fromIntegral nStu)) (fromIntegral (r3 `mod` 100))) : nxt nStu
                | roll < 68 ->
                    -- full 500-entry payload; ~5 entries actually differ
                    let changed = M.fromList [(fromIntegral ((r2 + fromIntegral j * 7) `mod` 500), fromIntegral (r3 `mod` 1000) + j) | j <- [0 .. 4 :: Int]]
                        full = M.union changed (M.fromList [(i, 100) | i <- [0 .. 499]])
                     in EvtS (BulkPrices full) : nxt nStu
                | roll < 82 -> EvtFile (pick urls r2) (pick capsets r3) (pick contents r4) : nxt nStu
                | roll < 88 -> EvtUnbind (pick urls r2) : nxt nStu
                | otherwise -> EvtScroll (fromIntegral (r2 `mod` 10000)) : nxt nStu

--------------------------------------------------------------------------------
-- The writer: live models + append-only frames + per-log flash accounting
--------------------------------------------------------------------------------

snapEvery :: Int
snapEvery = 250 -- durable msgs per app between snapshots

data Sys = Sys
  { sRDisk :: ![Frame], -- reversed accumulator
    sG :: !GModel,
    sS :: !SModel,
    sGn :: !Int, -- durable msgs since last grades snapshot
    sSn :: !Int,
    sFlG :: !Flash, -- per-log flash partitions
    sFlS :: !Flash,
    sFlV :: !Flash,
    sSnapG :: !(Int, Int, Int), -- (snaps, rawBytes, cmpBytes)
    sSnapS :: !(Int, Int, Int)
  }

sys0 :: Sys
sys0 = Sys [] M.empty M.empty 0 0 flash0 flash0 flash0 (0, 0, 0) (0, 0, 0)

diskOf :: Sys -> Disk
diskOf = reverse . sRDisk

emit :: Frame -> Flash -> ([Frame], Flash)
emit f fl = ([f], appendBytes (frameBytes f) fl)

step :: Sys -> Evt -> Sys
step sys = \case
  EvtScroll _ -> sys -- ephemeral: model-only in a real app; no disk traffic
  EvtG msg ->
    let ops = deltaG msg (sG sys)
        g' = updateG msg (sG sys)
        fr = FDeltaG ops -- ONE frame per msg: crash-atomic unit
        sys1 = sys {sRDisk = fr : sRDisk sys, sG = g', sFlG = appendBytes (frameBytes fr) (sFlG sys), sGn = sGn sys + 1}
     in if sGn sys1 >= snapEvery then snapshotG sys1 else sys1
  EvtS msg ->
    let ops = deltaS msg (sS sys)
        s' = updateS msg (sS sys)
        fr = FDeltaS ops
        sys1 = sys {sRDisk = fr : sRDisk sys, sS = s', sFlS = appendBytes (frameBytes fr) (sFlS sys), sSn = sSn sys + 1}
     in if sSn sys1 >= snapEvery then snapshotS sys1 else sys1
  EvtFile url caps bytes ->
    let h = hashOf bytes
        fo = FObj h bytes -- object BEFORE record: no dangling hash at any cut
        fi = FIdx (RWrite (parseP url) (S.fromList caps) h)
        fl' = appendBytes (frameBytes fi) (appendBytes (frameBytes fo) (sFlV sys))
     in sys {sRDisk = fi : fo : sRDisk sys, sFlV = fl'}
  EvtUnbind url ->
    let fi = FIdx (RUnbind (parseP url))
     in sys {sRDisk = fi : sRDisk sys, sFlV = appendBytes (frameBytes fi) (sFlV sys)}

-- Snapshot: chunks (compressed payload) then ONE commit frame. Recovery
-- ignores chunks entirely; a crash before the commit falls back to the
-- previous commit + a longer delta tail. After commit: flush + watermark GC.
snapshotG :: Sys -> Sys
snapshotG sys =
  let raw = B.pack (show (M.toList (sG sys)))
      rawN = B.length raw
      cmpN = compressedSize raw
      nCh = max 1 ((cmpN + payloadPerPage - 1) `div` payloadPerPage)
      chunks = [FSnapChunk Grades (min payloadPerPage (cmpN - i * payloadPerPage)) | i <- [0 .. nCh - 1]]
      commit = FCommitG (sG sys)
      fl1 = appendBytes (frameBytes commit) (appendBytes cmpN (sFlG sys))
      fl2 = flushPartial fl1
      fl3 = gcFlash (fPages fl2) fl2 -- everything before this page is dead
      (a, b, c) = sSnapG sys
   in sys
        { sRDisk = commit : reverse chunks ++ sRDisk sys,
          sFlG = fl3,
          sGn = 0,
          sSnapG = (a + 1, b + rawN, c + cmpN)
        }

snapshotS :: Sys -> Sys
snapshotS sys =
  let raw = B.pack (show (M.toList (sS sys)))
      rawN = B.length raw
      cmpN = compressedSize raw
      nCh = max 1 ((cmpN + payloadPerPage - 1) `div` payloadPerPage)
      chunks = [FSnapChunk Shop (min payloadPerPage (cmpN - i * payloadPerPage)) | i <- [0 .. nCh - 1]]
      commit = FCommitS (sS sys)
      fl1 = appendBytes (frameBytes commit) (appendBytes cmpN (sFlS sys))
      fl2 = flushPartial fl1
      fl3 = gcFlash (fPages fl2) fl2
      (a, b, c) = sSnapS sys
   in sys
        { sRDisk = commit : reverse chunks ++ sRDisk sys,
          sFlS = fl3,
          sSn = 0,
          sSnapS = (a + 1, b + rawN, c + cmpN)
        }

buildSys :: [Evt] -> Sys
buildSys = foldl' step sys0

--------------------------------------------------------------------------------
-- Boot: ONE fold over the disk -> Store + Index + per-app models
--------------------------------------------------------------------------------

type Index = M.Map Path (CapSet, Hash)

data Rec = Rec {rStore :: !Store, rIdx :: !Index, rG :: !GModel, rS :: !SModel}
  deriving (Eq, Show)

recover :: Disk -> Rec
recover = foldl' f (Rec M.empty M.empty M.empty M.empty) . takeWhile (/= FTorn)
  where
    f r = \case
      FObj h b -> r {rStore = M.insert h b (rStore r)}
      FIdx (RWrite p caps h) -> r {rIdx = M.insert p (caps, h) (rIdx r)}
      FIdx (RUnbind p) -> r {rIdx = M.delete p (rIdx r)}
      FDeltaG ops -> r {rG = foldl' (flip applyG) (rG r) ops}
      FDeltaS ops -> r {rS = foldl' (flip applyS) (rS r) ops}
      FSnapChunk _ _ -> r -- payload invisible until its commit
      FCommitG m -> r {rG = m}
      FCommitS m -> r {rS = m}
      FTorn -> r

-- Logical watermark GC: drop an app's frames strictly before its last
-- commit. (On flash this is realized as whole-block erases per partition;
-- here we verify the LOGICAL safety: boot result must be unchanged.)
gcDisk :: Disk -> Disk
gcDisk = gcApp isCommitS belongsS . gcApp isCommitG belongsG
  where
    isCommitG = \case FCommitG _ -> True; _ -> False
    isCommitS = \case FCommitS _ -> True; _ -> False
    belongsG = \case FDeltaG _ -> True; FSnapChunk Grades _ -> True; FCommitG _ -> True; _ -> False
    belongsS = \case FDeltaS _ -> True; FSnapChunk Shop _ -> True; FCommitS _ -> True; _ -> False
    gcApp isC bel d = case [i | (i, fr) <- zip [0 :: Int ..] d, isC fr] of
      [] -> d
      is -> let lastC = maximum is in [fr | (i, fr) <- zip [0 ..] d, not (bel fr) || i >= lastC]

--------------------------------------------------------------------------------
-- The trie (URL semantics + directory semantics, single canSee predicate)
--------------------------------------------------------------------------------

data Resource
  = BlobRef !Hash -- persistent: bytes live in the CAS
  | BlobVal !String -- synthesized: an actor's answer
  | Actor Service -- dynamic namespace

data Service = Service
  { svcName :: String,
    svcResolve :: Path -> Maybe Resource,
    svcList :: Path -> Maybe [Seg]
  }

data Bound = Bound {bRes :: Resource, bReq :: CapSet}

data Node = Node {nRes :: Maybe Bound, nKids :: M.Map Seg Node}

emptyN :: Node
emptyN = Node Nothing M.empty

bindP :: Path -> CapSet -> Resource -> Node -> Node
bindP path caps r = go path
  where
    b = Bound r caps
    go [] n = n {nRes = Just b}
    go (s : ss) n = n {nKids = M.alter (Just . go ss . fromMaybe emptyN) s (nKids n)}

bind :: String -> [Cap] -> Resource -> Node -> Node
bind url caps = bindP (parseP url) (S.fromList caps)

mount :: Index -> Node
mount = M.foldrWithKey (\p (caps, h) -> bindP p caps (BlobRef h)) emptyN

resolve :: CapSet -> String -> Node -> Maybe Resource
resolve caller url root = go (parseP url) root
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
ls caller url root = go (parseP url) root
  where
    go [] n =
      let static = [(s, kindOf child) | (s, child) <- M.toList (nKids n), anyVisible caller child]
          dynamic = case nRes n of
            Just (Bound (Actor svc) req) | canSee caller req -> maybe [] (map (\s -> (s, HasRes))) (svcList svc [])
            _ -> []
       in Just (static ++ dynamic)
    go p@(s : ss) n =
      case M.lookup s (nKids n) of
        Just child -> go ss child
        Nothing -> case nRes n of
          Just (Bound (Actor svc) req) | canSee caller req -> map (\sg -> (sg, HasRes)) <$> svcList svc p
          _ -> Nothing
    kindOf child = case (nRes child, M.null (nKids child)) of
      (Just _, True) -> HasRes
      (Just _, False) -> Both
      (Nothing, _) -> HasKids

readF :: CapSet -> String -> Node -> Store -> Maybe String
readF caller url root st =
  resolve caller url root >>= \case
    BlobRef h -> M.lookup h st
    BlobVal s -> Just s
    Actor _ -> Nothing

--------------------------------------------------------------------------------
-- App actors: URLs that run logic against a live model
--------------------------------------------------------------------------------

gradesSvc :: GModel -> Service
gradesSvc g =
  Service
    { svcName = "grades",
      svcResolve = \case
        ["model"] -> Just (BlobVal ("grades: " ++ show (M.size g) ++ " students"))
        ["students"] -> Just (BlobVal (show (M.size g)))
        ["students", i] -> byId i (BlobVal . show)
        ["students", i, "name"] -> byId i (BlobVal . sName)
        ["students", i, "age"] -> byId i (BlobVal . show . sAge)
        ["students", i, "score"] -> byId i (BlobVal . show . sScore)
        _ -> Nothing,
      svcList = \case
        [] -> Just ["model", "students"]
        ["students"] -> Just (map show (M.keys g))
        ["students", i] | hasId i -> Just ["name", "age", "score"]
        _ -> Nothing
    }
  where
    byId i f = readMaybe i >>= (`M.lookup` g) >>= Just . f
    hasId i = maybe False (`M.member` g) (readMaybe i :: Maybe Int)

shopSvc :: SModel -> Service
shopSvc s =
  Service
    { svcName = "shop",
      svcResolve = \case
        ["model"] -> Just (BlobVal ("shop: " ++ show (M.size s) ++ " prices"))
        ["price", k] -> readMaybe k >>= (`M.lookup` s) >>= Just . BlobVal . show
        _ -> Nothing,
      svcList = \case
        [] -> Just ["model", "price"]
        ["price"] -> Just (map show (M.keys s))
        _ -> Nothing
    }

-- Process table: name -> (pid, aid, service). /services/<name> forwards
-- through THIS table -- a live indirection, not a cached (pid,aid) blob.
type ProcTable = M.Map String (Int, Int, Service)

forwardSvc :: String -> ProcTable -> Service
forwardSvc name tbl =
  Service
    { svcName = name ++ "-fwd",
      svcResolve = \p -> look >>= \svc -> svcResolve svc p,
      svcList = \p -> look >>= \svc -> svcList svc p
    }
  where
    look = (\(_, _, svc) -> svc) <$> M.lookup name tbl

procSvc :: ProcTable -> Service
procSvc tbl =
  Service
    { svcName = "proc",
      svcResolve = \case
        [name] -> (\(pid, aid, _) -> BlobVal ("pid=" ++ show pid ++ " aid=" ++ show aid)) <$> M.lookup name tbl
        _ -> Nothing,
      svcList = \case
        [] -> Just (M.keys tbl)
        _ -> Nothing
    }

-- The "startup program": one disk fold, then assemble the namespace.
boot :: Disk -> (Node, Store, Rec)
boot d =
  let r = recover d
      tbl =
        M.fromList
          [ ("grades", (1, 101, gradesSvc (rG r))),
            ("shop", (1, 102, shopSvc (rS r)))
          ]
      root =
        bind "/dev/flash0" [Cap "hw"] (BlobVal ("pages=" ++ show pageSize ++ "B blocks=" ++ show pagesPerBlock)) $
          bind "/proc" [] (Actor (procSvc tbl)) $
            bind "/services/grades" [Cap "edu"] (Actor (forwardSvc "grades" tbl)) $
              bind "/services/shop" [] (Actor (forwardSvc "shop" tbl)) $
                bind "/apps/grades" [Cap "edu"] (Actor (gradesSvc (rG r))) $
                  bind "/apps/shop" [] (Actor (shopSvc (rS r))) $
                    mount (rIdx r)
   in (root, rStore r, r)

--------------------------------------------------------------------------------
-- Reference semantics + invariants
--------------------------------------------------------------------------------

refIndex :: [Evt] -> Index
refIndex = foldl' f M.empty
  where
    f ix = \case
      EvtFile url caps bytes -> M.insert (parseP url) (S.fromList caps, hashOf bytes) ix
      EvtUnbind url -> M.delete (parseP url) ix
      _ -> ix

gStates :: [Evt] -> [GModel]
gStates evts = scanl (flip updateG) M.empty [m | EvtG m <- evts]

sStates :: [Evt] -> [SModel]
sStates evts = scanl (flip updateS) M.empty [m | EvtS m <- evts]

-- I1: index fold is last-wins
i1 :: [Evt] -> Bool
i1 evts = rIdx (recover (diskOf (buildSys evts))) == refIndex evts

-- I2: full boot == live state (models, index, every hash resolvable)
i2 :: [Evt] -> Bool
i2 evts =
  let sys = buildSys evts
      r = recover (diskOf sys)
   in rG r == sG sys
        && rS r == sS sys
        && rIdx r == refIndex evts
        && all (\(_, h) -> M.member h (rStore r)) (M.elems (rIdx r))

-- I3: EVERY crash cut boots each app to a legitimate prefix state, and no
--     index hash dangles. Covers cuts inside bulk msgs (one frame -> can't
--     be split) and mid-snapshot (chunks invisible until commit).
i3 :: [Evt] -> Bool
i3 evts =
  let d = diskOf (buildSys evts)
      legitG = S.fromList (gStates evts)
      legitS = S.fromList (sStates evts)
      ok cut =
        let r = recover cut
         in rG r `S.member` legitG
              && rS r `S.member` legitS
              && all (\(_, h) -> M.member h (rStore r)) (M.elems (rIdx r))
   in all ok (crashCuts d)

-- I4: ephemeral msgs leave no trace: stripping them yields the same disk
i4 :: [Evt] -> Bool
i4 evts = diskOf (buildSys evts) == diskOf (buildSys (filter (not . isScroll) evts))

-- I5: logical watermark GC never changes what boot computes
i5 :: [Evt] -> Bool
i5 evts =
  let d = diskOf (buildSys evts)
   in recover (gcDisk d) == recover d

-- I6: ls never advertises what resolve denies, incl. app actors + caps
i6 :: [Evt] -> Bool
i6 evts =
  let (root, _, _) = boot (diskOf (buildSys evts))
      callers = [S.empty, S.fromList [Cap "x"], S.fromList [Cap "x", Cap "edu", Cap "hw"]]
   in and [ok c p root | c <- callers, p <- allPaths [] root]
  where
    allPaths p n = p : concat [allPaths (p ++ [s]) c | (s, c) <- M.toList (nKids n)]
    nodeAt [] n = Just n
    nodeAt (s : ss) n = M.lookup s (nKids n) >>= nodeAt ss
    ok c p root' = case ls c (renderP p) root' of
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
                  canSee c req || (k == Both && any (anyVisible c) (M.elems (nKids child)))

-- I7: /services/<x> is a true alias of /apps/<x> (same reads, same lists)
i7 :: [Evt] -> Bool
i7 evts =
  let (root, st, _) = boot (diskOf (buildSys evts))
      dev = S.fromList [Cap "edu"]
      suffixes =
        [ "/model",
          "/students",
          "/students/0",
          "/students/0/score",
          "/students/999",
          "/price/17",
          "/price/9999"
        ]
      same app sfx =
        readF dev ("/apps/" ++ app ++ sfx) root st == readF dev ("/services/" ++ app ++ sfx) root st
      sameLs app sfx =
        ls dev ("/apps/" ++ app ++ sfx) root == ls dev ("/services/" ++ app ++ sfx) root
   in and [same a sfx && sameLs a sfx | a <- ["grades", "shop"], sfx <- "" : suffixes]

--------------------------------------------------------------------------------
-- Runner
--------------------------------------------------------------------------------

runInv :: String -> ([Evt] -> Bool) -> [[Evt]] -> IO Bool
runInv name prop cases = do
  let bad = [i | (i, evts) <- zip [0 :: Int ..] cases, not (prop evts)]
  case bad of
    [] -> printf "  PASS  %-58s (%d cases)\n" name (length cases) >> pure True
    (i : _) -> printf "  FAIL  %-58s first failing case #%d\n" name i >> pure False

timeIt :: IO a -> IO (a, Double)
timeIt act = do
  t0 <- getCPUTime
  !x <- act
  t1 <- getCPUTime
  pure (x, fromIntegral (t1 - t0) / 1e9)

forceRec :: Rec -> Int
forceRec r = M.size (rStore r) + M.size (rIdx r) + M.size (rG r) + M.foldl' (+) 0 (rS r)

main :: IO ()
main = do
  let cases = [genEvts s n | (s, n) <- zip [1 .. 10] (cycle [40, 80, 120, 160])]

  putStrLn "== MVUVfs invariants (generated workloads, all crash cuts) =="
  rs <-
    sequence
      [ runInv "I1 VFS index fold is last-wins" i1 cases,
        runInv "I2 full boot == live state (models, index, objects)" i2 cases,
        runInv "I3 every crash cut boots to a legitimate prefix state" i3 cases,
        runInv "I4 ephemeral msgs leave no trace on disk" i4 cases,
        runInv "I5 watermark GC never changes what boot computes" i5 cases,
        runInv "I6 ls never advertises what resolve denies" i6 cases,
        runInv "I7 /services/<x> is a true alias of /apps/<x>" i7 cases
      ]

  -- ------------------------------------------------------------ big run
  let total = 6000
      evts = EvtFile "/etc/hostname" [] "fpr-hub" : genEvts 42 total
      sys = buildSys evts
      d = diskOf sys
      nDur = length (filter (not . isScroll) evts)

  printf "\n== Big run: %d events (%d durable, %d ephemeral) -> %d frames ==\n" total nDur (total - nDur) (length d)

  let row :: String -> Flash -> (Int, Int, Int) -> IO ()
      row name fl (sn, rawB, cmpB) = do
        printf
          "  %-8s %10d B %7d pg %6.2f WA %4d snaps %4d blocks GC'd"
          name
          (fLogical fl)
          (fPages fl)
          (writeAmp fl)
          (sn :: Int)
          (fFreed fl)
        if sn > 0
          then printf "  (snap raw %5.1f KB -> lz %5.1f KB)\n" (fromIntegral rawB / fromIntegral sn / 1024 :: Double) (fromIntegral cmpB / fromIntegral sn / 1024 :: Double)
          else printf "\n"
  putStrLn "  per-log flash partitions:"
  row "grades" (sFlG sys) (sSnapG sys)
  row "shop" (sFlS sys) (sSnapS sys)
  row "vfs" (sFlV sys) (0, 0, 0)

  let sampleBulk = head [m | EvtS m <- evts]
      verbatim = 12 + length (show sampleBulk)
      asDelta = frameBytes (FDeltaS (deltaS sampleBulk (M.fromList [(i, 100) | i <- [0 .. 499]])))
  printf "  BulkPrices: verbatim msg = %d B, delta frame = %d B (%.0fx smaller)\n" verbatim asDelta (fromIntegral verbatim / fromIntegral asDelta :: Double)

  -- ------------------------------------------------------------ boot fold cost
  (r1, tFull) <- timeIt (pure $! forceRec (recover d))
  (r2, tGc) <- timeIt (pure $! forceRec (recover (gcDisk d)))
  printf "\n  boot fold: full log %6.2f ms (%d frames) | GC'd log %6.2f ms (%d frames)  agree=%s\n" tFull (length d) tGc (length (gcDisk d)) (show (r1 == r2))

  -- ------------------------------------------------------------ namespace walkthrough
  let (root, st, rec') = boot d
      anon = S.empty
      dev = S.fromList [Cap "edu", Cap "x", Cap "hw"]
      showLs = maybe "not enumerable / not found" (\xs -> unwords [s ++ tag k | (s, k) <- take 8 xs] ++ (if length xs > 8 then " ..." else ""))
        where
          tag HasRes = ""; tag HasKids = "/"; tag Both = "/*"

  putStrLn "\n== Namespace after boot (files + app actors + proc table) =="
  printf "  ls /                       -> %s\n" (showLs (ls anon "/" root))
  printf "  ls /proc                   -> %s\n" (showLs (ls anon "/proc" root))
  printf "  read /proc/grades          -> %s\n" (show (readF anon "/proc/grades" root st))
  printf "  read /apps/shop/model      -> %s\n" (show (readF anon "/apps/shop/model" root st))
  printf "  read /apps/shop/price/17   -> %s\n" (show (readF anon "/apps/shop/price/17" root st))
  printf "  read /apps/grades/model    [anon] -> %s   (needs Cap edu)\n" (show (readF anon "/apps/grades/model" root st))
  printf "  read /apps/grades/model    [dev ] -> %s\n" (show (readF dev "/apps/grades/model" root st))
  printf "  read /services/grades/students/3/score [dev] -> %s\n" (show (readF dev "/services/grades/students/3/score" root st))
  printf "  read /etc/hostname (file plane, CAS-backed)  -> %s\n" (show (readF anon "/etc/hostname" root st))
  printf "  read /dev/flash0   [dev]                     -> %s\n" (show (readF dev "/dev/flash0" root st))
  printf "  ls / [anon] hides edu-only subtrees          -> %s\n" (showLs (ls anon "/apps" root))
  printf "  ls /apps [dev]                               -> %s\n" (showLs (ls dev "/apps" root))

  -- ------------------------------------------------------------ crash + reboot
  let cutAt = (7 * length d) `div` 10
      cut = take cutAt d ++ [FTorn]
      (_, _, recCut) = boot cut
  putStrLn "\n== Crash at 70% of the disk (torn tail), then reboot =="
  printf "  live      : %4d students, %3d prices, %2d files\n" (M.size (sG sys)) (M.size (sS sys)) (M.size (rIdx rec'))
  printf "  rebooted  : %4d students, %3d prices, %2d files   (a legitimate earlier state)\n" (M.size (rG recCut)) (M.size (rS recCut)) (M.size (rIdx recCut))

  putStrLn ""
  if and rs then putStrLn "ALL INVARIANTS PASS" else putStrLn "SOME INVARIANTS FAILED"
