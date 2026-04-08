-- WordHuffman.hs
--
-- Word-level Huffman compression.
-- Tokenises on whitespace, builds a frequency-optimal prefix tree,
-- serialises the tree as a header, encodes/decodes the body.
--
-- No dependencies beyond base + containers + bytestring.
-- Compile:  ghc -O2 WordHuffman.hs -o whuff
-- Usage:    whuff encode input.txt output.wh
--           whuff decode output.wh decoded.txt

module EncDecClash where

import Data.List          (sortBy, foldl', intercalate)
import Data.Ord           (comparing)
import Data.Maybe         (fromMaybe)
import Data.Char          (intToDigit)
import Data.Bits          (testBit, shiftL, shiftR, (.|.), (.&.))
import Data.Word          (Word8)
import Numeric            (showHex)

import qualified Data.Map.Strict as Map


-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

type Word'    = String                     -- a whitespace-delimited token
type FreqMap  = Map.Map Word' Int
type CodeMap  = Map.Map Word' [Bool]       -- True = 1, False = 0

-- Huffman tree: leaves hold words, branches hold subtrees.
-- Weight cached at every node for the merge algorithm.
data HTree
  = Leaf   { weight :: Int, symbol :: Word' }
  | Branch { weight :: Int, left :: HTree, right :: HTree }
  deriving (Show)


-- ---------------------------------------------------------------------------
-- 1. Tokenise
-- ---------------------------------------------------------------------------

tokenise :: String -> [Word']
tokenise = words           -- splits on any whitespace, drops empties


-- ---------------------------------------------------------------------------
-- 2. Frequency analysis
-- ---------------------------------------------------------------------------

frequencies :: [Word'] -> FreqMap
frequencies = foldl' (\m w -> Map.insertWith (+) w 1 m) Map.empty


-- ---------------------------------------------------------------------------
-- 3. Build Huffman tree  (simple sorted-list min-heap)
-- ---------------------------------------------------------------------------
--
-- Algorithm:
--   Start with a forest of single-node trees, one per symbol.
--   Repeatedly extract the two lowest-weight trees, merge them into a Branch,
--   reinsert.  Stop when one tree remains.

insertSorted :: HTree -> [HTree] -> [HTree]
insertSorted t []     = [t]
insertSorted t (x:xs)
  | weight t <= weight x = t : x : xs
  | otherwise            = x : insertSorted t xs

buildTree :: FreqMap -> Maybe HTree
buildTree fm
  | Map.null fm = Nothing
  | otherwise   =
      let leaves  = sortBy (comparing weight)
                  $ map (\(w,c) -> Leaf c w) (Map.toList fm)
      in  Just $ merge leaves
  where
    merge [t]        = t
    merge (a:b:rest) =
      let branch = Branch (weight a + weight b) a b
      in  merge (insertSorted branch rest)
    merge []         = error "buildTree: empty forest"


-- ---------------------------------------------------------------------------
-- 4. Derive code table  (DFS, accumulate path as [Bool])
-- ---------------------------------------------------------------------------

buildCodes :: HTree -> CodeMap
buildCodes = go []
  where
    go path (Leaf _ sym)     = Map.singleton sym (reverse path)
    go path (Branch _ l r)   =
      go (False:path) l `Map.union` go (True:path) r


-- ---------------------------------------------------------------------------
-- 5. Encode
-- ---------------------------------------------------------------------------
--
-- Output format:
--   [4 bytes] number of vocabulary entries (big-endian Word32)
--   for each entry:
--     [4 bytes] word length in bytes
--     [N bytes] word UTF-8 bytes
--     [4 bytes] frequency count
--   [4 bytes] number of encoded bits in body
--   [body bytes] bit-packed body, MSB first, last byte zero-padded

encode :: HTree -> CodeMap -> [Word'] -> [Word8]
encode tree codes tokens =
  let bodyBits  = concatMap (\t -> fromMaybe (error $ "OOV: " ++ t)
                                             (Map.lookup t codes)) tokens
      bodyBytes = packBits bodyBits
      header    = encodeHeader tree (length bodyBits)
  in  header ++ bodyBytes

-- Serialise the tree as a vocabulary list (word + frequency pairs).
-- The decoder reconstructs an identical tree from this.
encodeHeader :: HTree -> Int -> [Word8]
encodeHeader tree nBits =
  let vocab  = collectLeaves tree
      nWords = length vocab
  in  word32BE nWords
   ++ concatMap encodeEntry vocab
   ++ word32BE nBits
  where
    encodeEntry (w, freq) =
      let bs = map (fromIntegral . fromEnum) w :: [Word8]
      in  word32BE (length bs) ++ bs ++ word32BE freq

collectLeaves :: HTree -> [(Word', Int)]
collectLeaves (Leaf w sym)    = [(sym, w)]
collectLeaves (Branch _ l r)  = collectLeaves l ++ collectLeaves r

-- Pack [Bool] into [Word8], MSB first, last byte zero-padded
packBits :: [Bool] -> [Word8]
packBits []   = []
packBits bits =
  let (byte8, rest) = splitAt 8 bits
      padded        = byte8 ++ replicate (8 - length byte8) False
      w             = foldl' (\acc b -> acc * 2 + if b then 1 else 0) 0 padded
  in  fromIntegral w : packBits rest

word32BE :: Int -> [Word8]
word32BE n =
  [ fromIntegral (n `shiftR` 24)
  , fromIntegral (n `shiftR` 16)
  , fromIntegral (n `shiftR`  8)
  , fromIntegral  n
  ]


-- ---------------------------------------------------------------------------
-- 6. Decode header
-- ---------------------------------------------------------------------------

decodeHeader :: [Word8] -> (FreqMap, Int, [Word8])
decodeHeader bytes =
  let (nWordsB, rest0) = splitAt 4 bytes
      nWords           = readWord32BE nWordsB
      (vocab, rest1)   = readEntries nWords rest0
      (nBitsB, body)   = splitAt 4 rest1
      nBits            = readWord32BE nBitsB
      fm               = Map.fromList vocab
  in  (fm, nBits, body)
  where
    readEntries 0 bs = ([], bs)
    readEntries n bs =
      let (lenB, bs1)  = splitAt 4 bs
          wlen         = readWord32BE lenB
          (wbytes, bs2)= splitAt wlen bs1
          w            = map (toEnum . fromIntegral) wbytes
          (freqB, bs3) = splitAt 4 bs2
          freq         = readWord32BE freqB
          (rest, bs4)  = readEntries (n-1) bs3
      in  ((w, freq) : rest, bs4)

readWord32BE :: [Word8] -> Int
readWord32BE [a,b,c,d] =
  fromIntegral a `shiftL` 24
  .|. fromIntegral b `shiftL` 16
  .|. fromIntegral c `shiftL`  8
  .|. fromIntegral d
readWord32BE _ = 0


-- ---------------------------------------------------------------------------
-- 7. Decode body
-- ---------------------------------------------------------------------------

decode :: HTree -> Int -> [Word8] -> [Word']
decode tree nBits bytes =
  let bits = take nBits (unpackBits bytes)
  in  walkTree tree tree bits
  where
    walkTree root (Leaf _ sym)    bs     = sym : walkTree root root bs
    walkTree root (Branch _ l _) (False:bs) = walkTree root l bs
    walkTree root (Branch _ _ r) (True :bs) = walkTree root r bs
    walkTree _    _               []        = []

unpackBits :: [Word8] -> [Bool]
unpackBits = concatMap byteToBits
  where
    byteToBits w = map (testBit (fromIntegral w :: Int)) [7,6..0]


-- ---------------------------------------------------------------------------
-- 8. Top-level compress / decompress
-- ---------------------------------------------------------------------------

compress :: String -> [Word8]
compress text =
  let tokens = tokenise text
      fm     = frequencies tokens
      tree   = fromMaybe (error "empty input") (buildTree fm)
      codes  = buildCodes tree
  in  encode tree codes tokens

decompress :: [Word8] -> String
decompress bytes =
  let (fm, nBits, body) = decodeHeader bytes
      tree              = fromMaybe (error "bad header") (buildTree fm)
  in  unwords (decode tree nBits body)


-- ---------------------------------------------------------------------------
-- 9. Debug / introspection
-- ---------------------------------------------------------------------------

printTree :: HTree -> IO ()
printTree = go 0
  where
    go depth (Leaf w sym) =
      putStrLn $ replicate (depth*2) ' ' ++ "Leaf(" ++ show w ++ ") " ++ show sym
    go depth (Branch w l r) = do
      putStrLn $ replicate (depth*2) ' ' ++ "Branch(" ++ show w ++ ")"
      go (depth+1) l
      go (depth+1) r

printCodes :: CodeMap -> IO ()
printCodes cm =
  mapM_ (\(w,code) -> putStrLn $ show w ++ "\t" ++ map (\b -> if b then '1' else '0') code)
  $ sortBy (comparing (length . snd))
  $ Map.toList cm

-- Quick demo
demo :: IO ()
demo = do
  let text    = "the cat sat on the mat the cat sat"
      bytes   = compress text
      decoded = decompress bytes

  putStrLn $ "Original:    " ++ show text
  putStrLn $ "Tokens:      " ++ show (tokenise text)
  putStrLn ""

  let fm   = frequencies (tokenise text)
      tree = fromMaybe (error "") (buildTree fm)
      cm   = buildCodes tree
  putStrLn "Code table:"
  printCodes cm
  putStrLn ""

  putStrLn $ "Compressed:  " ++ show (length bytes) ++ " bytes"
  putStrLn $ "Decoded:     " ++ show decoded
  putStrLn $ "Round-trip:  " ++ show (text == decoded)
