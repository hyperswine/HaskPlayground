{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- MVU Append-Only Log Storage Simulation
--
-- Demonstrates:
--   * Persistent vs ephemeral msg classification
--   * Three logging strategies: naive full-model rewrite, Msg log, Delta log
--   * Flash-aware paging (4KB pages, 64-page blocks), write amplification
--   * LZ-style compression of snapshots
--   * Snapshot watermark GC at block granularity
--   * Replay cost: full log fold vs snapshot + tail

module MVULog where

import Data.Bits (shiftL, xor, (.&.))
import qualified Data.ByteString.Char8 as B
import Data.Char (ord)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import qualified Data.Map.Strict as M
import System.CPUTime
import Text.Printf

-- ---------------------------------------------------------------- Model

data Student = Student {sName :: !String, sAge :: !Int, sScore :: !Int} deriving (Show, Eq)

data PModel = PModel {students :: !(M.Map Int Student), prices :: !(M.Map Int Int)} deriving (Eq)

data Model = Model {pm :: !PModel, scrollPos :: !Int}

emptyModel :: Model
emptyModel = Model (PModel M.empty (M.fromList [(i, 100) | i <- [0 .. 499]])) 0

-- ---------------------------------------------------------------- Msgs

data Msg
  = AddStudent !Int !String !Int -- durable, small
  | SetAge !Int !Int -- durable, tiny
  | SetScore !Int !Int -- durable, tiny
  | BulkPrices !(M.Map Int Int) -- durable, LARGE payload, few real changes
  | Scrolled !Int -- ephemeral, not persisted
  deriving (Show)

isDurable :: Msg -> Bool
isDurable (Scrolled _) = False
isDurable _ = True

update :: Msg -> Model -> Model
update msg m@(Model p sc) = case msg of
  AddStudent i n a -> m {pm = p {students = M.insert i (Student n a 0) (students p)}}
  SetAge i a -> m {pm = p {students = M.adjust (\s -> s {sAge = a}) i (students p)}}
  SetScore i v -> m {pm = p {students = M.adjust (\s -> s {sScore = v}) i (students p)}}
  BulkPrices ps -> m {pm = p {prices = M.union ps (prices p)}} -- ps overrides
  Scrolled x -> m {scrollPos = x}
  where
    _ = sc

-- ---------------------------------------------------------------- Serialization (byte-accurate via show)

serMsg :: Msg -> B.ByteString
serMsg = B.pack . show

serStudent :: Int -> Student -> B.ByteString
serStudent i s = B.pack (show (i, s))

serPModel :: PModel -> B.ByteString
serPModel (PModel ss ps) = B.concat (map (uncurry serStudent) (M.toList ss)) `B.append` B.pack (show (M.toList ps))

-- ---------------------------------------------------------------- Delta ops

-- Structural ops derived from the actual change (path + new value only).
data Op
  = OpNew !B.ByteString -- serialized (path, value)
  | OpEdit !B.ByteString
  deriving (Show)

opBytes :: Op -> Int
opBytes (OpNew b) = 8 + B.length b -- 8 bytes: tag + path framing overhead
opBytes (OpEdit b) = 8 + B.length b

-- Delta producer: knows what actually changed. For BulkPrices this does a *real* generic diff against the previous map, emitting only entries whose value differs — this is where deltas win big.
deltaOps :: Msg -> PModel -> [Op]
deltaOps msg old = case msg of
  AddStudent i n a -> [OpNew (B.pack (show ("students", i, Student n a 0)))]
  SetAge i a -> [OpEdit (B.pack (show ("students", i, "age", a)))]
  SetScore i v -> [OpEdit (B.pack (show ("students", i, "score", v)))]
  BulkPrices ps -> [OpEdit (B.pack (show ("prices", k, v))) | (k, v) <- M.toList ps, M.lookup k (prices old) /= Just v]
  Scrolled _ -> []

-- ---------------------------------------------------------------- Flash simulator

pageSize, pageHdr, pagesPerBlock :: Int
pageSize = 4096
pageHdr = 16 -- magic/seq/len/flags/crc
pagesPerBlock = 64

payloadPerPage :: Int
payloadPerPage = pageSize - pageHdr

data Flash = Flash
  { fLogical :: !Int, -- logical bytes appended
    fPages :: !Int, -- pages written
    fErases :: !Int, -- block erases performed
    fFill :: !Int, -- bytes in current partial page
    fBlocks :: ![(Int, Int)], -- sealed blocks: (firstSeq, lastSeq) in units of pages
    fBlockPg :: !Int, -- pages in current active block
    fBlkFirst :: !Int, -- first page-seq of active block
    fFreed :: !Int -- blocks erased for free by watermark GC
  }
  deriving (Show)

flash0 :: Flash
flash0 = Flash 0 0 0 0 [] 0 0 0

-- Append a logical payload; pack into pages, seal blocks as they fill.
appendBytes :: Int -> Flash -> Flash
appendBytes n f0 = go n f0 {fLogical = fLogical f0 + n}
  where
    go 0 f = f
    go k f =
      let room = payloadPerPage - fFill f
       in if k < room
            then f {fFill = fFill f + k}
            else
              let f1 = closePage f {fFill = 0}
               in go (k - room) f1

closePage :: Flash -> Flash
closePage f =
  let f1 = f {fPages = fPages f + 1, fBlockPg = fBlockPg f + 1}
   in if fBlockPg f1 == pagesPerBlock
        then f1 {fBlocks = fBlocks f1 ++ [(fBlkFirst f1, fPages f1 - 1)], fBlockPg = 0, fBlkFirst = fPages f1}
        else f1

-- Flush partial page (e.g. at snapshot boundary or shutdown)
flushPartial :: Flash -> Flash
flushPartial f
  | fFill f == 0 = f
  | otherwise = closePage f {fFill = 0}

-- Watermark GC: erase sealed blocks entirely below page watermark.
gc :: Int -> Flash -> Flash
gc watermarkPage f =
  let (dead, live) = span (\(_, hi) -> hi < watermarkPage) (fBlocks f)
   in f {fBlocks = live, fErases = fErases f + length dead, fFreed = fFreed f + length dead}

writeAmp :: Flash -> Double
writeAmp f
  | fLogical f == 0 = 0
  | otherwise = fromIntegral (fPages f * pageSize) / fromIntegral (fLogical f)

-- ---------------------------------------------------------------- LZ-style compressor (size estimator)
-- Greedy hash-chain LZ over 4-grams: stand-in for LZ4/Zstd, gives a
-- realistic compressed size for repetitive structured data.

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
                Just j | let ml = matchLen j i 0, ml >= 6 -> go (i + ml) tbl' (out + 3) (toks + 1) -- 3-byte match token
                _ -> go (i + 1) tbl' (out + 1) (toks + 1) -- literal byte
    final o t = o + (t `div` 8) + 1 -- flag bits, 1 per token

-- ---------------------------------------------------------------- Workload

-- Deterministic LCG
lcg :: Int -> Int
lcg x = (x * 6364136223846793005 + 1442695040888963407) .&. 0x7FFFFFFF

genMsgs :: Int -> [Msg]
genMsgs total = go 0 (lcg 42) 0
  where
    go !k !r !nStu
      | k >= total = []
      | otherwise =
          let r1 = lcg r
              r2 = lcg r1
              r3 = lcg r2
              roll = r1 `mod` 100
           in if
                | roll < 20 || nStu == 0 -> AddStudent nStu ("student-" ++ show nStu) (18 + r2 `mod` 40) : go (k + 1) r3 (nStu + 1)
                | roll < 55 -> SetAge (r2 `mod` nStu) (18 + r3 `mod` 50) : go (k + 1) r3 nStu
                | roll < 80 -> SetScore (r2 `mod` nStu) (r3 `mod` 100) : go (k + 1) r3 nStu
                | roll < 85 ->
                    -- Bulk msg: full 500-entry price map in the payload, but only ~5 entries actually differ from current.
                    let changed = M.fromList [((r2 + j * 7) `mod` 500, r3 `mod` 1000 + j) | j <- [0 .. 4]]
                        full = M.union changed (M.fromList [(i, 100) | i <- [0 .. 499]])
                     in BulkPrices full : go (k + 1) r3 nStu
                | otherwise -> Scrolled (r2 `mod` 10000) : go (k + 1) r3 nStu

-- ---------------------------------------------------------------- Strategies

data Stats = Stats
  { stFlash :: !Flash,
    stSnapPages :: !Int, -- pages spent on snapshots
    stSnaps :: !Int,
    stSnapRawB :: !Int,
    stSnapCmpB :: !Int
  }

stats0 :: Stats
stats0 = Stats flash0 0 0 0 0

snapEvery :: Int
snapEvery = 1500 -- durable msgs between snapshots

-- Run a strategy: given a per-msg "bytes to append" function.
runStrategy :: (Msg -> PModel -> Int) -> [Msg] -> (Model, Stats)
runStrategy bytesFor msgs = foldl' step (emptyModel, stats0) (zip [1 ..] msgs)
  where
    step (!m, !st) (i, msg) =
      let m' = update msg m
          st1
            | isDurable msg = let b = bytesFor msg (pm m) in st {stFlash = appendBytes b (stFlash st)}
            | otherwise = st
          st2
            | isDurable msg && i `mod` snapEvery == 0 = snapshot m' st1
            | otherwise = st1
       in (m', st2)

    snapshot m st =
      let raw = serPModel (pm m)
          rawN = B.length raw
          cmpN = compressedSize raw
          pgs = (cmpN + payloadPerPage - 1) `div` payloadPerPage
          fl0 = flushPartial (stFlash st)
          wm = fPages fl0 -- everything before this page is dead
          fl1 = gc wm fl0
       in st {stFlash = fl1, stSnapPages = stSnapPages st + pgs, stSnaps = stSnaps st + 1, stSnapRawB = stSnapRawB st + rawN, stSnapCmpB = stSnapCmpB st + cmpN}

-- Strategy byte costs -------------------------------------------------

naiveBytes :: Msg -> PModel -> Int -- rewrite full persistent model per durable msg
naiveBytes _ p = B.length (serPModel (applyDummy p))
  where
    applyDummy = id -- size of model before ~= after; close enough for cost

msgLogBytes :: Msg -> PModel -> Int -- serialize the msg verbatim
msgLogBytes msg _ = 12 + B.length (serMsg msg) -- 12: seq + tag framing

deltaBytes :: Msg -> PModel -> Int -- structural ops, diffed against old state
deltaBytes msg old = sum (map opBytes (deltaOps msg old))

-- ---------------------------------------------------------------- Replay timing

timeIt :: IO a -> IO (a, Double)
timeIt act = do
  t0 <- getCPUTime
  !x <- act
  t1 <- getCPUTime
  pure (x, fromIntegral (t1 - t0) / 1e9) -- ms

forceModel :: Model -> Int
forceModel m = M.size (students (pm m)) + M.foldl' (\a s -> a + sAge s) 0 (students (pm m))

-- ---------------------------------------------------------------- Main

main :: IO ()
main = do
  let total = 8000
      msgs = genMsgs total
      durableMsgs = filter isDurable msgs
      nDur = length durableMsgs

  printf "=== MVU Append-Only Log Storage Simulation ===\n"
  printf "workload: %d msgs (%d durable, %d ephemeral/skipped)\n" total nDur (total - nDur)
  printf "flash: %d B pages (%d B payload), %d pages/block (%d KB blocks)\n" pageSize payloadPerPage pagesPerBlock (pageSize * pagesPerBlock `div` 1024)
  printf "snapshot every %d durable msgs, LZ-compressed, watermark GC after each\n\n" snapEvery

  let strategies =
        [ ("Naive full-model rewrite", naiveBytes),
          ("Msg log (verbatim msgs)", msgLogBytes),
          ("Delta log (structural)", deltaBytes)
        ]

  results <-
    mapM
      ( \(name, f) -> do
          let (m, st) = runStrategy f msgs
              _ = forceModel m
          pure (name, m, st)
      )
      strategies

  printf "%-28s %12s %8s %8s %7s %6s %6s %9s\n" "strategy" "log bytes" "pages" "MB" "WA" "snaps" "GC'd" "snapPages"
  printf "%s\n" (replicate 96 '-')
  mapM_
    ( \(name, _, st) -> do
        let fl = stFlash st
        printf
          "%-28s %12d %8d %8.2f %7.2f %6d %6d %9d\n"
          name
          (fLogical fl)
          (fPages fl)
          (fromIntegral (fPages fl * pageSize) / 1048576 :: Double)
          (writeAmp fl)
          (stSnaps st)
          (fFreed fl)
          (stSnapPages st)
    )
    results

  -- Snapshot compression detail (same for all; take from delta run)
  let (_, _, dst) = last results
  printf
    "\nsnapshot compression: %d snapshots, raw %.1f KB avg -> compressed %.1f KB avg (%.1f%%)\n"
    (stSnaps dst)
    (fromIntegral (stSnapRawB dst) / fromIntegral (max 1 (stSnaps dst)) / 1024 :: Double)
    (fromIntegral (stSnapCmpB dst) / fromIntegral (max 1 (stSnaps dst)) / 1024 :: Double)
    (100 * fromIntegral (stSnapCmpB dst) / fromIntegral (max 1 (stSnapRawB dst)) :: Double)

  -- Replay timing: full msg fold vs snapshot + tail
  let tailMsgs = drop (nDur - (nDur `mod` snapEvery)) durableMsgs
  (r1, tFull) <- timeIt (pure $! forceModel (foldl' (flip update) emptyModel msgs))
  -- snapshot+tail: model value held at watermark (CoW handoff), fold only the tail
  let atSnap = foldl' (flip update) emptyModel (take (length msgs - length tailMsgs) msgs)
  (_, _) <- timeIt (pure $! forceModel atSnap) -- build watermark model outside timed region
  (r2, tTail) <- timeIt (pure $! forceModel (foldl' (flip update) atSnap tailMsgs))
  printf "\nreplay: full log fold        = %.2f ms  (%d msgs)\n" tFull total
  printf "replay: snapshot + tail fold = %.2f ms  (%d msgs)  -> %.1fx faster\n" tTail (length tailMsgs) (tFull / max 0.01 tTail)
  printf "(models agree: %s)\n" (show (r1 == r2))

  -- Per-msg-type byte comparison
  printf "\nper-msg log cost (bytes appended):\n"
  printf "  %-34s %10s %10s\n" "msg" "msg-log" "delta-log"
  let sampleAdd = AddStudent 999 "student-999" 25
      sampleAge = SetAge 42 31
      pm0 = pm (foldl' (flip update) emptyModel (take 2000 msgs))
      sampleBulk = head [b | b@(BulkPrices _) <- msgs]
  mapM_
    ( \(lbl, s) ->
        printf "  %-34s %10d %10d\n" lbl (msgLogBytes s pm0) (deltaBytes s pm0)
    )
    [ ("AddStudent (new record)", sampleAdd),
      ("SetAge (one field)", sampleAge),
      ("BulkPrices (500-entry payload,", sampleBulk)
    ]
  printf "  %-34s\n" " ~5 real changes)"
