-- EncDecClash.hs
--
-- Word-level Huffman compression implemented in Clash.
--
--  PURE HASKELL (elaboration-time, not synthesisable):
--    tokenise, frequencies, buildTree, buildCodes
--    buildCodeTable  -->  Vec MaxSyms CodeEntry   (encoder ROM contents)
--    buildTreeROM    -->  Vec MaxNodes TreeNode    (decoder ROM contents)
--    encodeHeader / decodeHeader / compress / decompress  (wire-format helpers)
--
--  HARDWARE (Clash Mealy machines):
--    encoder      : Maybe (Index MaxSyms) --> Maybe (Unsigned 8)
--    compressor   : alias for encoder
--    serialiser   : Maybe (Unsigned 8)    --> (Bit, Bool)
--    decoder      : (Bit, Bool)           --> Maybe (Index MaxSyms)
--    decompressor : Maybe (Unsigned 8)    --> Maybe (Index MaxSyms)
--    roundTrip    : Maybe (Index MaxSyms) --> Maybe (Index MaxSyms)
--
--  DEMO (fixed corpus "the cat sat on the mat the cat sat"):
--    demoEncoder, demoDecoder, demoRoundTrip, simEncode, simRoundTrip
--
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module EncDecClash where

import Clash.Prelude hiding (lookup)
import Data.List (foldl', sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Word (Word8)
import qualified Prelude as P

-- ===========================================================================
-- §1  Compile-time configuration
-- ===========================================================================

-- Maximum vocabulary size (distinct word-tokens).
-- Enough ROM slots for any corpus with ≤ MaxSyms unique words.
type MaxSyms = 32 -- vocabulary slots; symbol indices are Index 32 (0..31)

type MaxNodes = 63 -- tree nodes: 2·MaxSyms − 1; node indices are Index 63

type MaxCodeLen = 16 -- maximum Huffman code length in bits

type AccWidth = 24 -- encoder bit-accumulator width: MaxCodeLen + 8

-- ===========================================================================
-- §2  Pure-Haskell tree construction  (elaboration-time, not synthesisable)
-- ===========================================================================

type Token = P.String

type FreqMap = Map.Map Token P.Int

type CodeMap = Map.Map Token [P.Bool]

-- Huffman tree: leaves hold word-tokens, branches hold subtrees.
-- Weight is cached at every node for the merge algorithm.
data HTree = Leaf {weight :: P.Int, symbol :: Token} | Branch {weight :: P.Int, left :: HTree, right :: HTree} deriving (P.Show)

-- ---------------------------------------------------------------------------
-- 2.1  Tokenise  (split on whitespace)
-- ---------------------------------------------------------------------------

tokenise :: P.String -> [Token]
tokenise = P.words

-- ---------------------------------------------------------------------------
-- 2.2  Frequency analysis
-- ---------------------------------------------------------------------------

frequencies :: [Token] -> FreqMap
frequencies = foldl' (\m w -> Map.insertWith (P.+) w 1 m) Map.empty

-- ---------------------------------------------------------------------------
-- 2.3  Build Huffman tree  (sorted-list min-priority-queue)
-- ---------------------------------------------------------------------------

insertSorted :: HTree -> [HTree] -> [HTree]
insertSorted t [] = [t]
insertSorted t (x : xs)
  | weight t P.<= weight x = t : x : xs
  | P.otherwise = x : insertSorted t xs

buildTree :: FreqMap -> P.Maybe HTree
buildTree fm
  | Map.null fm = P.Nothing
  | P.otherwise = let leaves = sortBy (comparing weight) $ P.map (\(w, c) -> Leaf c w) (Map.toList fm) in P.Just $ mergeForest leaves
  where
    mergeForest [t] = t
    mergeForest (a : b : rest) = mergeForest (insertSorted (Branch (weight a P.+ weight b) a b) rest)
    mergeForest [] = P.error "buildTree: empty forest"

-- ---------------------------------------------------------------------------
-- 2.4  Derive code table  (DFS, accumulate path as [Bool])
-- ---------------------------------------------------------------------------

buildCodes :: HTree -> CodeMap
buildCodes = go []
  where
    go path (Leaf _ sym) = Map.singleton sym (P.reverse path)
    go path (Branch _ l r) =
      go (P.False : path) l `Map.union` go (P.True : path) r

-- ---------------------------------------------------------------------------
-- 2.5  Helpers used by ROM builders
-- ---------------------------------------------------------------------------

-- DFS leaf order: the index of each leaf in this list is its symbol index.
collectLeaves :: HTree -> [(Token, P.Int)]
collectLeaves (Leaf w sym) = [(sym, w)]
collectLeaves (Branch _ l r) = collectLeaves l P.++ collectLeaves r

-- Pre-order node count (= 2·leaves − 1 for a full binary tree).
nodeCount :: HTree -> P.Int
nodeCount (Leaf _ _) = 1
nodeCount (Branch _ l r) = 1 P.+ nodeCount l P.+ nodeCount r

-- ===========================================================================
-- §3  ROM-content builders  (pure Haskell → Clash Vecs)
-- ===========================================================================

-- ---------------------------------------------------------------------------
-- 3.1  Code-table ROM  (encoder lookup)
-- ---------------------------------------------------------------------------
--
-- Entry format: (code_bits :: BitVector MaxCodeLen, code_len :: Unsigned 5)
--   code_bits  – Huffman code packed into the LOW `len` bits, MSB of code at
--                bit position (len−1), LSB at bit 0.
--   code_len   – number of valid bits (0 for unused slots).
--
-- The Vec is indexed by symbol index (= DFS leaf order from collectLeaves).

type CodeEntry = (BitVector MaxCodeLen, Unsigned 5)

buildCodeTable :: HTree -> Vec MaxSyms CodeEntry
buildCodeTable tree =
  let codes = buildCodes tree
      leafList = P.map P.fst (collectLeaves tree)
      mkEntry sym = case Map.lookup sym codes of
        P.Nothing -> (0, 0)
        P.Just bits -> let bv = foldl' (\acc b -> (acc `shiftL` 1) .|. if b then 1 else 0) (0 :: P.Int) bits in (fromIntegral bv, fromIntegral (P.length bits))
      pairs = P.take (snatToNum (SNat @MaxSyms)) $ P.zip [0 ..] (P.map mkEntry leafList)
      base = repeat (0, 0) :: Vec MaxSyms CodeEntry
   in P.foldl (\v (i, e) -> replace (fromIntegral i :: Index MaxSyms) e v) base pairs

-- ---------------------------------------------------------------------------
-- 3.2  Tree ROM  (decoder node table)
-- ---------------------------------------------------------------------------
--
-- Node layout: (isLeaf, symbolIdx, leftChild, rightChild)
--   isLeaf=True  → symbolIdx (Index MaxSyms) is the decoded word;
--                  child fields are ignored.
--   isLeaf=False → leftChild taken on bit=0, rightChild on bit=1;
--                  both are indices into this Vec (Index MaxNodes).
--
-- Root node is always at index 0.  Nodes are numbered in pre-order.

type NodeIdx = Index MaxNodes

type TreeNode = (Bool, Index MaxSyms, NodeIdx, NodeIdx)

buildTreeROM :: [(Token, P.Int)] -> HTree -> Vec MaxNodes TreeNode
buildTreeROM vocab tree = go tree 0 1 (repeat (P.True, 0, 0, 0))
  where
    symIdx w = fromIntegral $ fromMaybe 0 (P.lookup w (P.zip (P.map P.fst vocab) [0 ..]))

    go (Leaf _ sym) idx _ v = replace (fromIntegral idx :: NodeIdx) (P.True, symIdx sym, 0, 0) v
    go (Branch _ l r) idx nextFree v =
      let lIdx = nextFree
          lSize = nodeCount l
          rIdx = nextFree P.+ lSize
          v1 = replace (fromIntegral idx :: NodeIdx) (P.False, 0, fromIntegral lIdx, fromIntegral rIdx) v
          v2 = go l lIdx (lIdx P.+ 1) v1
          v3 = go r rIdx (rIdx P.+ 1) v2
       in v3

-- ===========================================================================
-- §4  ENCODER  —  symbol-index stream → packed-byte stream
-- ===========================================================================
--
-- Invariant: encFill ≤ AccWidth − 1 = 23 at the start of every cycle.
-- After appending a code of up to MaxCodeLen=16 bits the fill reaches ≤ 23,
-- so the 24-bit accumulator is always wide enough.

data EncState = EncState
  { encAcc :: Unsigned AccWidth, -- bit accumulator, most-significant bits first
    encFill :: Unsigned 5 -- number of valid bits currently in encAcc
  }
  deriving (P.Show, Generic, NFDataX)

encInit = EncState {encAcc = 0, encFill = 0}

-- | One encoder step.
--
--   inp  = Just sym  → pack the symbol's Huffman code into the accumulator.
--   inp  = Nothing   → no new bits; still emits if fill ≥ 8.
--   flush = True     → if 0 < fill < 8, zero-pad to a full byte and emit
--                      (used to drain the last partial byte at end-of-stream).
encStep :: Vec MaxSyms CodeEntry -> EncState -> (P.Maybe (Index MaxSyms), Bool) -> (EncState, P.Maybe (Unsigned 8))
encStep codeROM st (inp, flush) = (st', outByte)
  where
    -- 1. Append new code bits when a symbol is presented.
    (acc1, fill1) = case inp of
      P.Nothing -> (encAcc st, encFill st)
      P.Just sym ->
        let (codeBits, codeLen) = codeROM !! sym
            shifted =
              (encAcc st `shiftL` fromIntegral codeLen)
                .|. resize (unpack codeBits :: Unsigned MaxCodeLen)
         in (shifted, encFill st + resize codeLen)

    -- 2. Emit: full byte when fill ≥ 8; zero-padded partial byte on flush.
    (outByte, acc2, fill2)
      | fill1 >= 8 =
          let rem8 = fill1 - 8
              top8 = resize (acc1 `shiftR` fromIntegral rem8) :: Unsigned 8
              mask = (1 `shiftL` fromIntegral rem8) - 1 :: Unsigned AccWidth
           in (P.Just top8, acc1 .&. mask, rem8)
      | flush && fill1 > 0 =
          -- Zero-pad the remaining bits to a full byte and emit.
          let padded = acc1 `shiftL` fromIntegral (8 - fill1)
              top8 = resize padded :: Unsigned 8
           in (P.Just top8, 0, 0)
      | P.otherwise = (P.Nothing, acc1, fill1)

    st' = EncState {encAcc = acc2, encFill = fill2}

-- | Mealy-wrapped encoder.
--
--   Combine the symbol and flush signals via 'bundle (symSig, flushSig)'.
--   For normal encoding, tie flush to 'pure False'; assert it once after
--   the last symbol to drain any remaining bits as a zero-padded final byte.
encoder :: (HiddenClockResetEnable dom) => Vec MaxSyms CodeEntry -> Signal dom (P.Maybe (Index MaxSyms)) -> Signal dom Bool -> Signal dom (P.Maybe (Unsigned 8))
encoder ct symSig flushSig = mealy (encStep ct) encInit (bundle (symSig, flushSig))

-- ===========================================================================
-- §5  COMPRESSOR  —  alias for encoder (same interface, named for clarity)
-- ===========================================================================

compressor :: (HiddenClockResetEnable dom) => Vec MaxSyms CodeEntry -> Signal dom (P.Maybe (Index MaxSyms)) -> Signal dom Bool -> Signal dom (P.Maybe (Unsigned 8))
compressor = encoder

-- ===========================================================================
-- §6  SERIALISER  —  byte stream → bit stream  (MSB first)
-- ===========================================================================

-- serBuf: byte currently being serialised, serBits: remaining valid bits (0 = idle)
data SerState = SerState {serBuf :: Unsigned 8, serBits :: Unsigned 4} deriving (P.Show, Generic, NFDataX)

serInit = SerState {serBuf = 0, serBits = 0}

serStep :: SerState -> P.Maybe (Unsigned 8) -> (SerState, (Bit, Bool))
serStep st inp =
  case (serBits st, inp) of
    -- Idle: load new byte, emit MSB immediately.
    (0, P.Just byte) -> let topBit = msb (pack byte); st' = SerState {serBuf = byte `shiftL` 1, serBits = 7} in (st', (topBit, P.True))
    -- Busy: emit current MSB, shift left.
    (n, _) | n > 0 -> let topBit = msb (pack (serBuf st)); st' = SerState {serBuf = serBuf st `shiftL` 1, serBits = n - 1} in (st', (topBit, P.True))
    -- Idle and no new byte.
    _ -> (st, (0, P.False))

serialiser :: (HiddenClockResetEnable dom) => Signal dom (P.Maybe (Unsigned 8)) -> Signal dom (Bit, Bool)
serialiser = mealy serStep serInit

-- ===========================================================================
-- §7  DECODER  —  bit stream → symbol-index stream
-- ===========================================================================
--
-- State is the current tree-node index (root = 0, always an internal node
-- at the start of a symbol).  On each valid bit we move left (0) or right (1);
-- if the destination is a leaf we emit its symbol and reset to the root.

-- Current position in the tree ROM (root = 0)
data DecState = DecState {decNode :: NodeIdx} deriving (P.Show, Generic, NFDataX)

decInit = DecState {decNode = 0}

decStep :: Vec MaxNodes TreeNode -> DecState -> (Bit, Bool) -> (DecState, P.Maybe (Index MaxSyms))
decStep treeROM st (b, valid)
  | P.not valid = (st, P.Nothing)
  | P.otherwise =
      let (_, _, lChild, rChild) = treeROM !! decNode st
          nextNode = if bitToBool b then rChild else lChild
          (isLeaf, sym, _, _) = treeROM !! nextNode
       in -- leaf reached: emit, reset or still traversing
          if isLeaf then (decInit, P.Just sym) else (DecState nextNode, P.Nothing)

-- | Mealy-wrapped decoder.
decoder :: (HiddenClockResetEnable dom) => Vec MaxNodes TreeNode -> Signal dom (Bit, Bool) -> Signal dom (P.Maybe (Index MaxSyms))
decoder tr = mealy (decStep tr) decInit

-- ===========================================================================
-- §8  DECOMPRESSOR  —  byte stream → symbol-index stream
--     (serialiser feeding the decoder, composed as Signals)
-- ===========================================================================
--
-- Note: the serialiser ignores a new byte when still busy streaming the
-- previous one (8 cycles per byte).  Feed bytes at most every 8 cycles, or
-- use backpressure from 'serBits == 0' in real hardware.

decompressor :: (HiddenClockResetEnable dom) => Vec MaxNodes TreeNode -> Signal dom (P.Maybe (Unsigned 8)) -> Signal dom (P.Maybe (Index MaxSyms))
decompressor tr byteIn = decoder tr (serialiser byteIn)

-- ===========================================================================
-- §9  ROUND-TRIP CIRCUIT
--     symbol stream → compressed bytes → decompressed symbol stream
-- ===========================================================================
--
-- The encoder can emit one byte per cycle while the serialiser needs 8
-- cycles to stream each byte.  In real hardware this is resolved with a FIFO
-- or backpressure.  For simulation the 'simRoundTrip' helper spaces symbols
-- 9 cycles apart so the serialiser always finishes before the next byte
-- arrives.

-- Requires flush strobe (assert once after last symbol)
roundTrip :: (HiddenClockResetEnable dom) => Vec MaxSyms CodeEntry -> Vec MaxNodes TreeNode -> Signal dom (P.Maybe (Index MaxSyms)) -> Signal dom Bool -> Signal dom (P.Maybe (Index MaxSyms))
roundTrip ct tr symIn flushSig = decompressor tr (compressor ct symIn flushSig)

-- ===========================================================================
-- §10  Demo corpus  (elaboration-time)
-- ===========================================================================

demoText :: P.String
demoText = "the cat sat on the mat the cat sat"

demoTree :: HTree
demoTree = fromMaybe (P.error "empty") (buildTree (frequencies (tokenise demoText)))

demoVocab :: [(Token, P.Int)]
demoVocab = collectLeaves demoTree

demoCodeTable :: Vec MaxSyms CodeEntry
demoCodeTable = buildCodeTable demoTree

demoTreeROM :: Vec MaxNodes TreeNode
demoTreeROM = buildTreeROM demoVocab demoTree

-- | Encoder specialised to the demo corpus.
demoEncoder :: (HiddenClockResetEnable dom) => Signal dom (P.Maybe (Index MaxSyms)) -> Signal dom Bool -> Signal dom (P.Maybe (Unsigned 8))
demoEncoder = encoder demoCodeTable

-- | Decoder specialised to the demo corpus.
demoDecoder :: (HiddenClockResetEnable dom) => Signal dom (Bit, Bool) -> Signal dom (P.Maybe (Index MaxSyms))
demoDecoder = decoder demoTreeROM

-- | Full round-trip specialised to the demo corpus.
demoRoundTrip :: (HiddenClockResetEnable dom) => Signal dom (P.Maybe (Index MaxSyms)) -> Signal dom Bool -> Signal dom (P.Maybe (Index MaxSyms))
demoRoundTrip = roundTrip demoCodeTable demoTreeROM

-- ===========================================================================
-- §11  Simulation helpers
-- ===========================================================================

-- | Encode a list of symbol indices using the demo code table.
--
--   Symbols are spaced one per cycle.  A flush strobe is asserted for one
--   cycle after the last symbol so the final partial byte is emitted.
--   Returns the compact byte output (Nothings removed).
simEncode :: [Index MaxSyms] -> [Unsigned 8]
simEncode syms =
  let n = P.length syms
      -- symbol stream: Just s ... Nothing ...
      symIn = P.map P.Just syms P.++ P.repeat P.Nothing
      -- flush strobe: False for all symbol cycles + one True cycle to drain
      flush = P.replicate n P.False P.++ [P.True] P.++ P.repeat P.False
      raw = P.take (n P.+ 16) $ simulate @System (\sf -> encoder demoCodeTable (fmap P.fst sf) (fmap P.snd sf)) (P.zip symIn flush)
   in [b | P.Just b <- raw]

-- | Full round-trip simulation over the demo corpus.
--
--   Symbols are spaced 9 cycles apart so that each compressed byte fully
--   serialises before the next one arrives (serialiser takes 8 cycles/byte).
--   A flush strobe drains the final partial byte.  We take exactly
--   'length syms' decoded symbols, discarding any extras caused by the
--   zero-padding on the last flushed byte (mirroring how the pure
--   decompressor uses nBits to stop).
simRoundTrip :: [Index MaxSyms] -> [Index MaxSyms]
simRoundTrip syms =
  let n = P.length syms
      -- Spread: 1 Just + 8 Nothings per symbol, then flush, then idle
      symIn = P.concatMap (\s -> P.Just s : P.replicate 8 P.Nothing) syms P.++ [P.Nothing] P.++ P.repeat P.Nothing
      flush = P.replicate (n P.* 9) P.False P.++ [P.True] P.++ P.repeat P.False
      total = n P.* 9 P.+ 80 -- generous window
      raw = P.take total $ simulate @System (\sf -> roundTrip demoCodeTable demoTreeROM (fmap P.fst sf) (fmap P.snd sf)) (P.zip symIn flush)
   in P.take n [s | P.Just s <- raw]

-- | Look up the symbol index for a token in the demo vocabulary.
tokenToIndex :: Token -> P.Maybe (Index MaxSyms)
tokenToIndex tok = fromIntegral P.<$> P.lookup tok (P.zip (P.map P.fst demoVocab) [(0 :: P.Int) ..])

-- | Look up the token for a symbol index in the demo vocabulary.
indexToToken :: Index MaxSyms -> Token
indexToToken i = P.fst (demoVocab P.!! fromIntegral i)

-- ===========================================================================
-- §12  Pure header encode/decode  (wire-format helpers, not synthesised)
-- ===========================================================================
--
-- Wire format (same as original EncDecClash):
--   [4 bytes] number of vocabulary entries (big-endian Word32)
--   for each entry:
--     [4 bytes] word length in bytes
--     [N bytes] word UTF-8 bytes
--     [4 bytes] frequency count
--   [4 bytes] number of encoded bits in body
--   [body bytes] bit-packed body, MSB first, last byte zero-padded

word32BE :: P.Int -> [Word8]
word32BE n = [fromIntegral (n `shiftR` 24), fromIntegral (n `shiftR` 16), fromIntegral (n `shiftR` 8), fromIntegral n]

readWord32BE :: [Word8] -> P.Int
readWord32BE [a, b, c, d] = fromIntegral a `shiftL` 24 .|. fromIntegral b `shiftL` 16 .|. fromIntegral c `shiftL` 8 .|. fromIntegral d
readWord32BE _ = 0

packBits :: [P.Bool] -> [Word8]
packBits [] = []
packBits bits =
  let (byte8, rest) = P.splitAt 8 bits
      padded = byte8 P.++ P.replicate (8 P.- P.length byte8) P.False
      w = foldl' (\acc b -> acc P.* 2 P.+ if b then 1 else 0) (0 :: P.Int) padded
   in fromIntegral w : packBits rest

unpackBits :: [Word8] -> [P.Bool]
unpackBits = P.concatMap byteToBits
  where
    byteToBits w = P.map (testBit (fromIntegral w :: P.Int)) [7, 6 .. 0]

encodeHeader :: HTree -> P.Int -> [Word8]
encodeHeader tree nBits =
  let vocab = collectLeaves tree
      nWords = P.length vocab
   in word32BE nWords P.++ P.concatMap encodeEntry vocab P.++ word32BE nBits
  where
    encodeEntry (w, freq) = let bs = P.map (fromIntegral . fromEnum) w :: [Word8] in word32BE (P.length bs) P.++ bs P.++ word32BE freq

decodeHeader :: [Word8] -> (FreqMap, P.Int, [Word8])
decodeHeader bytes =
  let (nWordsB, rest0) = P.splitAt 4 bytes
      nWords = readWord32BE nWordsB
      (vocab, rest1) = readEntries nWords rest0
      (nBitsB, body) = P.splitAt 4 rest1
      nBits = readWord32BE nBitsB
      fm = Map.fromList vocab
   in (fm, nBits, body)
  where
    readEntries 0 bs = ([], bs)
    readEntries n bs =
      let (lenB, bs1) = P.splitAt 4 bs
          wlen = readWord32BE lenB
          (wbytes, bs2) = P.splitAt wlen bs1
          w = P.map (toEnum . fromIntegral) wbytes
          (freqB, bs3) = P.splitAt 4 bs2
          freq = readWord32BE freqB
          (rest, bs4) = readEntries (n P.- 1) bs3
       in ((w, freq) : rest, bs4)

-- ===========================================================================
-- §13  Pure compress / decompress  (software reference, uses header)
-- ===========================================================================

compress :: P.String -> [Word8]
compress text =
  let tokens = tokenise text
      fm = frequencies tokens
      tree = fromMaybe (P.error "empty input") (buildTree fm)
      codes = buildCodes tree
      bodyBits = P.concatMap (\t -> fromMaybe (P.error $ "OOV: " P.++ t) (Map.lookup t codes)) tokens
      bodyBytes = packBits bodyBits
      header = encodeHeader tree (P.length bodyBits)
   in header P.++ bodyBytes

decompress :: [Word8] -> P.String
decompress bytes =
  let (fm, nBits, body) = decodeHeader bytes
      tree = fromMaybe (P.error "bad header") (buildTree fm)
      bits = P.take nBits (unpackBits body)
   in P.unwords (walkTree tree tree bits)
  where
    walkTree root (Leaf _ sym) bs = sym : walkTree root root bs
    walkTree root (Branch _ l _) (P.False : bs) = walkTree root l bs
    walkTree root (Branch _ _ r) (P.True : bs) = walkTree root r bs
    walkTree _ _ [] = []

-- ===========================================================================
-- §14  Debug helpers
-- ===========================================================================

printTree :: HTree -> P.IO ()
printTree = go 0
  where
    go depth (Leaf w sym) = P.putStrLn $ P.replicate (depth P.* 2) ' ' P.++ "Leaf(" P.++ P.show w P.++ ") " P.++ P.show sym
    go depth (Branch w l r) = do
      P.putStrLn $ P.replicate (depth P.* 2) ' ' P.++ "Branch(" P.++ P.show w P.++ ")"
      go (depth P.+ 1) l
      go (depth P.+ 1) r

printCodes :: CodeMap -> P.IO ()
printCodes cm = P.mapM_ (\(w, code) -> P.putStrLn $ P.show w P.++ "\t" P.++ P.map (\b -> if b then '1' else '0') code) $ sortBy (comparing (P.length . P.snd)) $ Map.toList cm

-- ===========================================================================
-- §15  Quick demo
-- ===========================================================================

demo :: P.IO ()
demo = do
  let tree = demoTree
      cm = buildCodes tree
  P.putStrLn $ "Text:   " P.++ P.show demoText
  P.putStrLn $ "Tokens: " P.++ P.show (tokenise demoText)
  P.putStrLn ""
  P.putStrLn "Code table:"
  printCodes cm

  P.putStrLn ""
  P.putStrLn $ "Vocab (index → token): " P.++ P.show demoVocab
  let idxs = P.map (fromMaybe (P.error "OOV") . tokenToIndex) (tokenise demoText)
      encoded = simEncode idxs
  P.putStrLn $ "Sim-encoded bytes: " P.++ P.show encoded
  let decoded = simRoundTrip idxs
  P.putStrLn $ "Round-trip tokens: " P.++ P.show (P.map indexToToken decoded)
  P.putStrLn $ "Round-trip OK:     " P.++ P.show (decoded P.== idxs)
  let pureBytes = compress demoText
      pureDecoded = decompress pureBytes
  P.putStrLn ""
  P.putStrLn $ "Pure compress:   " P.++ P.show (P.length pureBytes) P.++ " bytes"
  P.putStrLn $ "Pure decompress: " P.++ P.show pureDecoded
  P.putStrLn $ "Pure round-trip OK: " P.++ P.show (demoText P.== pureDecoded)
