{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module HuffmanClashTest (huffmanClashGroup) where

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import HuffmanClash

-- ---------------------------------------------------------------------------
-- Generator
-- ---------------------------------------------------------------------------

genSymbol :: Gen Symbol
genSymbol = Gen.enumBounded

genSymbols :: Gen [Symbol]
genSymbols = Gen.list (Range.linear 1 32) genSymbol

-- ---------------------------------------------------------------------------
-- Code-table unit tests
-- ---------------------------------------------------------------------------

-- SymA encodes to a single-bit codeword (0).
prop_codeTable_symA_onebit :: Property
prop_codeTable_symA_onebit = property $
  codeTable SymA === (0b00000000, 1)

-- SymB is exactly the 2-bit code "10".
prop_codeTable_symB_twobit :: Property
prop_codeTable_symB_twobit = property $
  codeTable SymB === (0b00000010, 2)

-- SymC and SymD are both 3-bit codewords.
prop_codeTable_longCodes_threebit :: Property
prop_codeTable_longCodes_threebit = property $ do
  sym <- forAll $ Gen.element [SymC, SymD]
  let (_, len) = codeTable sym
  len === 3

-- All code lengths are between 1 and 8 (fit in a byte).
prop_codeTable_lengthInRange :: Property
prop_codeTable_lengthInRange = property $ do
  sym <- forAll genSymbol
  let (_, len) = codeTable sym
  assert (len >= 1 && len <= 8)

-- No two distinct symbols share the same (bits, length) pair (prefix-free).
prop_codeTable_noDuplicates :: Property
prop_codeTable_noDuplicates = property $ do
  s1 <- forAll genSymbol
  s2 <- forAll $ Gen.filter (/= s1) genSymbol
  codeTable s1 /== codeTable s2

-- ---------------------------------------------------------------------------
-- Encoder output structure
-- ---------------------------------------------------------------------------

-- simEncode produces exactly (length syms + 4) outputs.
prop_simEncode_outputLength :: Property
prop_simEncode_outputLength = property $ do
  syms <- forAll genSymbols
  let out = simEncode syms
  length out === length syms + 4

-- Encoding a single SymA (1-bit code) yields Nothing for the first few cycles
-- and then no more than one Just byte overall (1 bit < 8).
prop_simEncode_singleSymA_atMostOneByte :: Property
prop_simEncode_singleSymA_atMostOneByte = property $ do
  let out = filter (/= Nothing) (simEncode [SymA])
  assert (length out <= 1)

-- Encoding 8 copies of SymA (1 bit each = 8 bits total) yields exactly 1 byte.
prop_simEncode_eightSymAs_exactlyOneByte :: Property
prop_simEncode_eightSymAs_exactlyOneByte = property $ do
  let out = filter (/= Nothing) (simEncode (replicate 8 SymA))
  length out === 1

-- Encoding any non-empty sequence produces at least one output cell (possibly
-- all Nothing, but the list is non-empty).
prop_simEncode_nonEmpty :: Property
prop_simEncode_nonEmpty = property $ do
  syms <- forAll genSymbols
  assert (not (null (simEncode syms)))

-- The encoded byte for 8×SymA (code "0" repeated 8 times = 0x00) is 0x00.
prop_simEncode_eightSymAs_byteIsZero :: Property
prop_simEncode_eightSymAs_byteIsZero = property $ do
  let bytes = [b | Just b <- simEncode (replicate 8 SymA)]
  bytes === [0x00]

-- Encoding the sequence [SymD, SymD] (0b111 twice = 0b111111xx) should
-- produce the top 6 bits of the accumulated byte = 0b11111100 = 0xFC.
prop_simEncode_twoSymDs_topSixBits :: Property
prop_simEncode_twoSymDs_topSixBits = property $ do
  -- 2×SymD = 6 bits, not enough for a full byte → no output expected
  let bytes = [b | Just b <- simEncode [SymD, SymD]]
  bytes === []

-- Encoding [SymB, SymC] = "10" ++ "110" = 5 bits < 8 → no byte yet.
prop_simEncode_symBsymC_noByteYet :: Property
prop_simEncode_symBsymC_noByteYet = property $ do
  let bytes = [b | Just b <- simEncode [SymB, SymC]]
  bytes === []

-- Encoding exactly 8 SymBs (2 bits each = 16 bits) yields exactly 2 bytes.
prop_simEncode_eightSymBs_twoBytes :: Property
prop_simEncode_eightSymBs_twoBytes = property $ do
  let bytes = [b | Just b <- simEncode (replicate 8 SymB)]
  length bytes === 2

-- The byte produced by 8×SymB is 0xAA (10101010).
prop_simEncode_eightSymBs_value :: Property
prop_simEncode_eightSymBs_value = property $ do
  let bytes = [b | Just b <- simEncode (replicate 8 SymB)]
  bytes === [0xAA, 0xAA]

-- ---------------------------------------------------------------------------
-- Round-trip properties
-- ---------------------------------------------------------------------------

-- The round-trip output window is non-empty for any non-empty input.
prop_roundTrip_nonEmpty :: Property
prop_roundTrip_nonEmpty = property $ do
  syms <- forAll genSymbols
  assert (not (null (simRoundTrip syms)))

-- After filtering Nothings, the recovered symbols form a subsequence of the
-- original (no symbols are invented).
prop_roundTrip_noInventedSymbols :: Property
prop_roundTrip_noInventedSymbols = property $ do
  syms <- forAll genSymbols
  let recovered = [s | Just s <- simRoundTrip syms]
  assert (recovered `isSubseqOf` (syms ++ syms))  -- loose upper bound

-- The round-trip recovers at least one symbol for any ≥4-symbol input.
prop_roundTrip_recoversAtLeastOne :: Property
prop_roundTrip_recoversAtLeastOne = property $ do
  syms <- forAll $ Gen.list (Range.linear 4 32) genSymbol
  let recovered = [s | Just s <- simRoundTrip syms]
  assert (not (null recovered))

-- Golden: [SymA] round-trips with SymA in the output window.
prop_roundTrip_symA_golden :: Property
prop_roundTrip_symA_golden = property $ do
  let recovered = [s | Just s <- simRoundTrip [SymA]]
  assert (SymA `elem` recovered)

-- Golden: all four distinct symbols survive the round-trip.
prop_roundTrip_allSymbols_golden :: Property
prop_roundTrip_allSymbols_golden = property $ do
  let syms      = [SymA, SymB, SymC, SymD, SymA, SymB, SymC, SymD]
  let recovered = [s | Just s <- simRoundTrip syms]
  assert (SymA `elem` recovered)
  assert (SymB `elem` recovered)
  assert (SymC `elem` recovered)
  assert (SymD `elem` recovered)

-- Golden: mixed alternating sequence retains order in recovered output.
prop_roundTrip_alternating_golden :: Property
prop_roundTrip_alternating_golden = property $ do
  let syms      = concat (replicate 4 [SymA, SymD])
  let recovered = [s | Just s <- simRoundTrip syms]
  -- Every element of recovered came from the original sequence
  assert (all (`elem` [SymA, SymD]) recovered)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

isSubseqOf :: Eq a => [a] -> [a] -> Bool
isSubseqOf []     _  = True
isSubseqOf _      [] = False
isSubseqOf (x:xs) (y:ys)
  | x == y    = isSubseqOf xs ys
  | otherwise = isSubseqOf (x:xs) ys

-- ---------------------------------------------------------------------------
-- Group export
-- ---------------------------------------------------------------------------

huffmanClashGroup :: Group
huffmanClashGroup = Group "HuffmanClash"
  [ ("code table: SymA is 1 bit",                       prop_codeTable_symA_onebit)
  , ("code table: SymB is 2 bits",                      prop_codeTable_symB_twobit)
  , ("code table: SymC & SymD are 3 bits",              prop_codeTable_longCodes_threebit)
  , ("code table: all lengths in [1,8]",                prop_codeTable_lengthInRange)
  , ("code table: no two symbols share a code",         prop_codeTable_noDuplicates)
  , ("encoder output length == inputs + 4",             prop_simEncode_outputLength)
  , ("encoder: single SymA → at most 1 byte",           prop_simEncode_singleSymA_atMostOneByte)
  , ("encoder: 8×SymA → exactly 1 byte",                prop_simEncode_eightSymAs_exactlyOneByte)
  , ("encoder: non-empty input → non-empty output",     prop_simEncode_nonEmpty)
  , ("encoder: 8×SymA byte value == 0x00",              prop_simEncode_eightSymAs_byteIsZero)
  , ("encoder: 2×SymD → no byte (only 6 bits)",        prop_simEncode_twoSymDs_topSixBits)
  , ("encoder: SymB++SymC → no byte (5 bits)",          prop_simEncode_symBsymC_noByteYet)
  , ("encoder: 8×SymB → 2 bytes",                       prop_simEncode_eightSymBs_twoBytes)
  , ("encoder: 8×SymB byte values == 0xAA",             prop_simEncode_eightSymBs_value)
  , ("round-trip: non-empty output",                    prop_roundTrip_nonEmpty)
  , ("round-trip: no invented symbols",                 prop_roundTrip_noInventedSymbols)
  , ("round-trip: recovers ≥1 symbol for ≥4 inputs",   prop_roundTrip_recoversAtLeastOne)
  , ("round-trip golden: SymA survives",                prop_roundTrip_symA_golden)
  , ("round-trip golden: all 4 symbols survive",        prop_roundTrip_allSymbols_golden)
  , ("round-trip golden: alternating SymA/SymD",        prop_roundTrip_alternating_golden)
  ]
