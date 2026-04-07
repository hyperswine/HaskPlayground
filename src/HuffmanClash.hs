{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HuffmanClash where

import Clash.Prelude hiding (Symbol)
import qualified Prelude as P

data Symbol = SymA | SymB | SymC | SymD deriving (Show, Eq, Generic, NFDataX, Enum, Bounded)

-- | (code bits packed into low bits of Unsigned 8, code length in bits)
type Code = (Unsigned 8, Unsigned 4)

codeTable :: Symbol -> Code
codeTable SymA = (0b00000000, 1) -- 0
codeTable SymB = (0b00000010, 2) -- 10
codeTable SymC = (0b00000110, 3) -- 110
codeTable SymD = (0b00000111, 3) -- 111

-- EncAcc: bit buffer, bits packed toward MSB number of valid bits in encAcc
data EncState = EncState {encAcc :: Unsigned 16, encFill :: Unsigned 5} deriving (Show, Generic, NFDataX)

encInit :: EncState
encInit = EncState {encAcc = 0, encFill = 0}

-- | Encoder transition + output.
encStep :: EncState -> Maybe Symbol -> (EncState, Maybe (Unsigned 8))
encStep st inp = (st', outByte)
  where
    -- 1. Optionally append new code bits into the accumulator
    (acc1, fill1) =
      case inp of
        Nothing -> (encAcc st, encFill st)
        Just sym ->
          let (bits, len) = codeTable sym
              -- shift current acc left by len, OR in new bits at the bottom
              shifted = (encAcc st `shiftL` fromIntegral len) .|. resize bits
              newFill = encFill st + resize len
           in (shifted, newFill)

    -- 2. If we have ≥ 8 bits, emit the top byte
    (outByte, acc2, fill2) = if fill1 >= 8 then let top8 = resize (acc1 `shiftR` (fromIntegral fill1 - 8)) :: Unsigned 8; mask = (1 `shiftL` fromIntegral (fill1 - 8)) - 1; rest = resize (acc1 .&. mask) :: Unsigned 16 in (Just top8, rest, fill1 - 8) else (Nothing, acc1, fill1)

    st' = EncState {encAcc = acc2, encFill = fill2}

-- | Mealy-wrapped encoder; one-symbol-per-cycle streaming.
encoder :: (HiddenClockResetEnable dom) => Signal dom (Maybe Symbol) -> Signal dom (Maybe (Unsigned 8))
encoder = mealy encStep encInit

-- Can basically encode prefix tree as a state machine

-- Contains current tree node (0–2)
data DecState = DecState {decNode :: Unsigned 2} deriving (Show, Generic, NFDataX)

decInit = DecState {decNode = 0}

-- | Decoder transition + output.
decStep :: DecState -> (Bit, Bool) -> (DecState, Maybe Symbol)
decStep st (b, valid)
  | not valid = (st, Nothing)
  | otherwise = case (decNode st, b) of
      (0, 0) -> (decInit, Just SymA)
      (0, 1) -> (DecState {decNode = 1}, Nothing)
      (1, 0) -> (decInit, Just SymB)
      (1, 1) -> (DecState {decNode = 2}, Nothing)
      (2, 0) -> (decInit, Just SymC)
      (2, 1) -> (decInit, Just SymD)
      _ -> (decInit, Nothing) -- unreachable

-- | Mealy-wrapped decoder.
decoder :: (HiddenClockResetEnable dom) => Signal dom (Bit, Bool) -> Signal dom (Maybe Symbol)
decoder = mealy decStep decInit

-- serBuf: byte being serialised, serBits: bits remaining (0 = idle)
data SerState = SerState {serBuf :: Unsigned 8, serBits :: Unsigned 4} deriving (Show, Generic, NFDataX)

serInit :: SerState
serInit = SerState {serBuf = 0, serBits = 0}

serStep :: SerState -> Maybe (Unsigned 8) -> (SerState, (Bit, Bool))
serStep st inp =
  case (serBits st, inp) of
    -- Idle and new byte arrives: load it, emit MSB this cycle
    (0, Just byte) -> let topBit = boolToBit (testBit (toInteger byte) 7); st' = SerState {serBuf = byte `shiftL` 1, serBits = 7} in (st', (topBit, True))
    -- Busy: emit next MSB, shift left
    (n, _) | n > 0 -> let topBit = boolToBit (testBit (toInteger (serBuf st)) 7); st' = SerState {serBuf = serBuf st `shiftL` 1, serBits = n - 1} in (st', (topBit, True))
    -- Idle, nothing to do
    _ -> (st, (0, False))

serialiser :: (HiddenClockResetEnable dom) => Signal dom (Maybe (Unsigned 8)) -> Signal dom (Bit, Bool)
serialiser = mealy serStep serInit

roundTrip :: (HiddenClockResetEnable dom) => Signal dom (Maybe Symbol) -> Signal dom (Maybe Symbol)
roundTrip symIn = let compressed = encoder symIn; bits = serialiser compressed in decoder bits

-- | Run encoder over a list of symbols, collect output bytes.
simEncode :: [Symbol] -> [Maybe (Unsigned 8)]
simEncode syms = P.take (P.length syms + 4) $ simulate @System encoder $ P.map Just syms P.++ P.repeat Nothing

-- | Run the full round-trip simulation. P.length syms * 6 generous window for pipeline latency
simRoundTrip :: [Symbol] -> [Maybe Symbol]
simRoundTrip syms = P.take (P.length syms * 6) $ simulate @System roundTrip $ P.map Just syms P.++ P.repeat Nothing

-- simEncode [SymA, SymB, SymC, SymD, SymA, SymA, SymA, SymA]
-- filter (/= Nothing) $ simRoundTrip [SymA, SymB, SymA, SymC, SymD]
