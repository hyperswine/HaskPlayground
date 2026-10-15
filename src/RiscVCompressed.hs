{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Integer C 2.0 decompression. The caller retains the original halfword
-- for mtval and uses a two-byte link/sequential PC for a compressed instruction.
-- Floating encodings and RV32 custom shift encodings remain illegal. HINTs
-- expand to ADDI x0,x0,0; reserved encodings return Nothing.
module RiscVCompressed (decompress, decompress32, decompress64) where

import Clash.Prelude

decompress32, decompress64 :: Unsigned 16 -> Maybe (Unsigned 32)
decompress32 = decompress False
decompress64 = decompress True

decompress :: Bool -> Unsigned 16 -> Maybe (Unsigned 32)
decompress rv64 half = case (h .&. 3, field 13 7) of
  (0,0) | addi4 /= 0 -> Just (encI 0x13 0 rp2 2 addi4)
  (0,2) -> Just (encI 3 2 rp2 rp1 offW)
  (0,3) | rv64 -> Just (encI 3 3 rp2 rp1 offD)
  (0,6) -> Just (encS 2 rp1 rp2 offW)
  (0,7) | rv64 -> Just (encS 3 rp1 rp2 offD)
  (1,0) -> hint (rd == 0 || ci == 0) (encI 0x13 0 rd rd ci)
  (1,1) | not rv64 -> Just (encJ 1 jump)
        | rd /= 0 -> Just (encI 0x1b 0 rd rd ci)
  (1,2) -> hint (rd == 0) (encI 0x13 0 rd 0 ci)
  (1,3) | rd == 2 && addi16 /= 0 -> Just (encI 0x13 0 2 2 addi16)
        | rd /= 2 && ci /= 0 -> hint (rd == 0) ((ci `shiftL` 12) .|. (rd `shiftL` 7) .|. 0x37)
  (1,4) -> case field 10 3 of
    0 | shiftValid -> hint (shamt == 0) (encI 0x13 5 rp1 rp1 shamt)
    1 | shiftValid -> hint (shamt == 0) (encI 0x13 5 rp1 rp1 (0x400 .|. shamt))
    2 -> Just (encI 0x13 7 rp1 rp1 ci)
    3 -> case (testBit h 12, field 5 3) of
      (False,0) -> Just (encR 0x33 0 0x20 rp1 rp1 rp2)
      (False,1) -> Just (encR 0x33 4 0 rp1 rp1 rp2)
      (False,2) -> Just (encR 0x33 6 0 rp1 rp1 rp2)
      (False,3) -> Just (encR 0x33 7 0 rp1 rp1 rp2)
      (True,0) | rv64 -> Just (encR 0x3b 0 0x20 rp1 rp1 rp2)
      (True,1) | rv64 -> Just (encR 0x3b 0 0 rp1 rp1 rp2)
      _ -> Nothing
    _ -> Nothing
  (1,5) -> Just (encJ 0 jump)
  (1,6) -> Just (encB 0 rp1 branch)
  (1,7) -> Just (encB 1 rp1 branch)
  (2,0) | shiftValid -> hint (rd == 0 || shamt == 0) (encI 0x13 1 rd rd shamt)
  (2,2) | rd /= 0 -> Just (encI 3 2 rd 2 lwsp)
  (2,3) | rv64 && rd /= 0 -> Just (encI 3 3 rd 2 ldsp)
  (2,4) -> case (testBit h 12, rs2 == 0, rd == 0) of
    (False,True,False) -> Just (encI 0x67 0 0 rd 0)
    (False,False,_) -> hint (rd == 0) (encR 0x33 0 0 rd 0 rs2)
    (True,True,True) -> Just 0x00100073
    (True,True,False) -> Just (encI 0x67 0 1 rd 0)
    (True,False,_) -> hint (rd == 0) (encR 0x33 0 0 rd rd rs2)
    _ -> Nothing
  (2,6) -> Just (encS 2 2 rs2 swsp)
  (2,7) | rv64 -> Just (encS 3 2 rs2 sdsp)
  _ -> Nothing
 where
  h = resize half :: Unsigned 32
  field n mask = (h `shiftR` n) .&. mask
  rd = field 7 31
  rs2 = field 2 31
  rp1 = 8 + field 7 7
  rp2 = 8 + field 2 7
  shamt = (field 12 1 `shiftL` 5) .|. field 2 31
  shiftValid = rv64 || not (testBit h 12)
  ci = sext 6 shamt
  addi4 = (field 11 3 `shiftL` 4) .|. (field 7 15 `shiftL` 6) .|. (field 6 1 `shiftL` 2) .|. (field 5 1 `shiftL` 3)
  offW = (field 10 7 `shiftL` 3) .|. (field 6 1 `shiftL` 2) .|. (field 5 1 `shiftL` 6)
  offD = (field 10 7 `shiftL` 3) .|. (field 5 3 `shiftL` 6)
  addi16 = sext 10 ((field 12 1 `shiftL` 9) .|. (field 6 1 `shiftL` 4) .|. (field 5 1 `shiftL` 6) .|. (field 3 3 `shiftL` 7) .|. (field 2 1 `shiftL` 5))
  jump = sext 12 ((field 12 1 `shiftL` 11) .|. (field 11 1 `shiftL` 4) .|. (field 9 3 `shiftL` 8) .|. (field 8 1 `shiftL` 10) .|. (field 7 1 `shiftL` 6) .|. (field 6 1 `shiftL` 7) .|. (field 3 7 `shiftL` 1) .|. (field 2 1 `shiftL` 5))
  branch = sext 9 ((field 12 1 `shiftL` 8) .|. (field 10 3 `shiftL` 3) .|. (field 5 3 `shiftL` 6) .|. (field 3 3 `shiftL` 1) .|. (field 2 1 `shiftL` 5))
  lwsp = (field 12 1 `shiftL` 5) .|. (field 4 7 `shiftL` 2) .|. (field 2 3 `shiftL` 6)
  ldsp = (field 12 1 `shiftL` 5) .|. (field 5 3 `shiftL` 3) .|. (field 2 7 `shiftL` 6)
  swsp = (field 9 15 `shiftL` 2) .|. (field 7 3 `shiftL` 6)
  sdsp = (field 10 7 `shiftL` 3) .|. (field 7 7 `shiftL` 6)

hint :: Bool -> Unsigned 32 -> Maybe (Unsigned 32)
hint True _ = Just 0x13
hint False word = Just word

sext :: Int -> Unsigned 32 -> Unsigned 32
sext n x = if testBit x (n-1) then x .|. complement ((1 `shiftL` n)-1) else x

encI :: Unsigned 32 -> Unsigned 32 -> Unsigned 32 -> Unsigned 32 -> Unsigned 32 -> Unsigned 32
encI op f rd rs imm = ((imm .&. 0xfff) `shiftL` 20) .|. (rs `shiftL` 15) .|. (f `shiftL` 12) .|. (rd `shiftL` 7) .|. op

encR :: Unsigned 32 -> Unsigned 32 -> Unsigned 32 -> Unsigned 32 -> Unsigned 32 -> Unsigned 32 -> Unsigned 32
encR op f f7 rd rs rt = (f7 `shiftL` 25) .|. (rt `shiftL` 20) .|. (rs `shiftL` 15) .|. (f `shiftL` 12) .|. (rd `shiftL` 7) .|. op

encS :: Unsigned 32 -> Unsigned 32 -> Unsigned 32 -> Unsigned 32 -> Unsigned 32
encS f rs rt imm = ((imm .&. 0xfe0) `shiftL` 20) .|. (rt `shiftL` 20) .|. (rs `shiftL` 15) .|. (f `shiftL` 12) .|. ((imm .&. 31) `shiftL` 7) .|. 0x23

encB :: Unsigned 32 -> Unsigned 32 -> Unsigned 32 -> Unsigned 32
encB f rs imm = ((imm .&. 0x1000) `shiftL` 19) .|. ((imm .&. 0x7e0) `shiftL` 20) .|. (rs `shiftL` 15) .|. (f `shiftL` 12) .|. ((imm .&. 30) `shiftL` 7) .|. ((imm .&. 0x800) `shiftR` 4) .|. 0x63

encJ :: Unsigned 32 -> Unsigned 32 -> Unsigned 32
encJ rd imm = ((imm .&. 0x100000) `shiftL` 11) .|. ((imm .&. 0x7fe) `shiftL` 20) .|. ((imm .&. 0x800) `shiftL` 9) .|. (imm .&. 0xff000) .|. (rd `shiftL` 7) .|. 0x6f
