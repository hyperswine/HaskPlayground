{-# LANGUAGE NumericUnderscores #-}
-- | An independent RV32IM + Zicsr reference model for differential testing
-- of SimpleRisc: an instruction-set interpreter over plain values, written
-- from the RISC-V specification rather than from the core.
--
-- It models exactly what the differential programs use: RAM at
-- 0x8000_0000 (64 KiB), the 2 KiB boot ROM at 0 (loads only), synchronous
-- traps into a direct-mode mtvec, the CLINT's msip and the machine software
-- interrupt it raises (taken at the next instruction boundary when MIE and
-- mie.MSIE are set, as SimpleRisc does), MRET, and the machine CSRs
-- SimpleRisc implements (counters excluded). Where the
-- specification leaves a choice, the comments say which one SimpleRisc
-- made: mtval holds the instruction for an illegal instruction and the pc
-- for EBREAK; a misaligned address traps before an unmapped one.
--
-- SimpleRisc's other device registers (timer, PLIC, UART, I/O, exit) are
-- mapped, so an access to one doesn't fault, but their values aren't
-- modelled: the model stops there ('mDevice') and a test can't compare.
module Rv32Model
  ( Model (..),
    initialModel,
    step,
    runModel,
    ramBase,
    ramWords,
  )
where

import Data.Bits
import Data.Int (Int32, Int64)
import qualified Data.IntMap.Strict as IM
import Data.Word (Word32, Word64)

data Model = Model
  { mPc :: Word32,
    mRegs :: IM.IntMap Word32,
    -- | RAM by word index from 'ramBase'; absent words read as zero.
    mMem :: IM.IntMap Word32,
    mTvec :: Word32,
    mEpc :: Word32,
    mCause :: Word32,
    mTval :: Word32,
    mScratch :: Word32,
    mIe :: Word32,
    mMie :: Bool,
    mMpie :: Bool,
    -- | Instructions retired (an instruction that traps does not retire).
    mRetired :: Int,
    -- | The boot ROM's words, readable by loads at 0x0000_0000.
    mRom :: IM.IntMap Word32,
    -- | The CLINT's msip (0x0200_0000, word accesses).
    mMsip :: Bool,
    -- | Stopped at an access to a device register it doesn't model.
    mDevice :: Bool
  }
  deriving (Show, Eq)

ramBase :: Word32
ramBase = 0x8000_0000

ramWords :: Word32
ramWords = 16384

initialModel :: [(Int, Word32)] -> IM.IntMap Word32 -> [Word32] -> Model
initialModel regs mem rom =
  Model ramBase (IM.fromList [(r, v) | (r, v) <- regs, r /= 0]) mem 0 0 0 0 0 0 False False 0
    (IM.fromList (zip [0 ..] rom)) False False

clintMsip :: Word32
clintMsip = 0x0200_0000

reg :: Model -> Word32 -> Word32
reg m r = if r == 0 then 0 else IM.findWithDefault 0 (fromIntegral r) (mRegs m)

setReg :: Word32 -> Word32 -> Model -> Model
setReg r v m = if r == 0 then m else m {mRegs = IM.insert (fromIntegral r) v (mRegs m)}

ramIndex :: Word32 -> Maybe Int
ramIndex address
  | address >= ramBase && address - ramBase < 4 * ramWords = Just (fromIntegral ((address - ramBase) `shiftR` 2))
  | otherwise = Nothing

readWord :: Model -> Int -> Word32
readWord m i = IM.findWithDefault 0 i (mMem m)

-- | Take a trap: mepc is the instruction, MPIE := MIE, MIE := 0.
trap :: Word32 -> Word32 -> Model -> Model
trap cause value m =
  m {mEpc = mPc m, mCause = cause, mTval = value, mMpie = mMie m, mMie = False, mPc = mTvec m}

-- | Retire with the next pc.
retire :: Word32 -> Model -> Model
retire next m = m {mPc = next, mRetired = mRetired m + 1}

sext :: Int -> Word32 -> Word32
sext bits v = if testBit v (bits - 1) then v .|. complement (bit bits - 1) else v .&. (bit bits - 1)

field :: Int -> Int -> Word32 -> Word32
field hi lo w = (w `shiftR` lo) .&. (bit (hi - lo + 1) - 1)

-- | Run until the pc enters SimpleRisc's boot ROM (a trap reaching mtvec 0,
-- or a jump there) or fuel ends. The ROM is the first 2 KiB, as the core's
-- 'romAddress' says; any other pc outside RAM is fetched, and faults.
runModel :: Int -> Model -> Model
runModel fuel m
  | fuel <= 0 || inRom (mPc m) || mDevice m = m
  | otherwise = runModel (fuel - 1) (step m)

inRom :: Word32 -> Bool
inRom address = address .&. 0xffff_f800 == 0

-- | SimpleRisc's device registers, msip included (other widths than a word
-- aren't modelled): the exit and boot-cause devices, the CLINT, the PLIC's
-- UART source, the UART, and the I/O window at 0x1000_1000.
deviceAddress :: Word32 -> Bool
deviceAddress address =
  address `elem` [ 0x0010_0000, 0x0010_0004, 0x0200_0000, 0x0200_4000, 0x0200_4004, 0x0200_bff8, 0x0200_bffc
                 , 0x0c00_0028, 0x0c00_1000, 0x0c00_2000, 0x0c20_0000, 0x0c20_0004
                 , 0x1000_0000, 0x1000_0004, 0x1000_0008, 0x1000_000c ]
    || address .&. 0xffff_f000 == 0x1000_1000

-- | One instruction, or the software interrupt before it.
step :: Model -> Model
step m
  | mMie m && testBit (mIe m) 3 && mMsip m = trap 0x8000_0003 0 m
  | otherwise = case ramIndex (mPc m) of
      Nothing -> trap 1 (mPc m) m
      Just i -> execute (readWord m i) m

execute :: Word32 -> Model -> Model
execute ins m = case opcode of
  0x37 -> retire next (setReg rd (ins .&. 0xffff_f000) m)
  0x17 -> retire next (setReg rd (pc + (ins .&. 0xffff_f000)) m)
  0x6f -> jump (pc + sext 21 jImm)
  0x67
    | funct3 == 0 -> jump ((reg m rs1 + sext 12 iImm) .&. complement 1)
    | otherwise -> illegal
  0x63 -> branch
  0x03 -> load
  0x23 -> store
  0x13 -> opImm
  0x33 -> op
  0x0f
    | funct3 == 0 || funct3 == 1 -> retire next m -- FENCE, FENCE.I: nothing to order here
    | otherwise -> illegal
  0x73 -> system
  _ -> illegal
  where
    pc = mPc m
    next = pc + 4
    opcode = ins .&. 0x7f
    rd = field 11 7 ins
    rs1 = field 19 15 ins
    rs2 = field 24 20 ins
    funct3 = field 14 12 ins
    funct7 = field 31 25 ins
    iImm = field 31 20 ins
    sImm = (field 31 25 ins `shiftL` 5) .|. field 11 7 ins
    bImm = (field 31 31 ins `shiftL` 12) .|. (field 7 7 ins `shiftL` 11) .|. (field 30 25 ins `shiftL` 5) .|. (field 11 8 ins `shiftL` 1)
    jImm = (field 31 31 ins `shiftL` 20) .|. (field 19 12 ins `shiftL` 12) .|. (field 20 20 ins `shiftL` 11) .|. (field 30 21 ins `shiftL` 1)
    a = reg m rs1
    b = reg m rs2
    illegal = trap 2 ins m
    -- RAM, or the boot ROM for loads
    loadable address = case ramIndex address of
      Just i -> Just (readWord m i)
      Nothing
        | address < 0x800 -> Just (IM.findWithDefault 0 (fromIntegral (address `shiftR` 2)) (mRom m))
        | address == clintMsip && funct3 == 2 -> Just (if mMsip m then 1 else 0)
        | otherwise -> Nothing
    signed :: Word32 -> Int32
    signed = fromIntegral

    jump target
      | target .&. 3 /= 0 = trap 0 target m
      | otherwise = retire target (setReg rd next m)

    branch =
      let taken = case funct3 of
            0 -> Just (a == b)
            1 -> Just (a /= b)
            4 -> Just (signed a < signed b)
            5 -> Just (signed a >= signed b)
            6 -> Just (a < b)
            7 -> Just (a >= b)
            _ -> Nothing
          target = pc + sext 13 bImm
       in case taken of
            Nothing -> illegal
            Just False -> retire next m
            Just True
              | target .&. 3 /= 0 -> trap 0 target m
              | otherwise -> retire target m

    -- Misalignment first, then the map (SimpleRisc's order).
    load =
      let address = a + sext 12 iImm
          size = case funct3 of 0 -> Just 1; 1 -> Just 2; 2 -> Just 4; 4 -> Just 1; 5 -> Just 2; _ -> Nothing
       in case size of
            Nothing -> illegal
            Just n
              | address .&. (n - 1) /= 0 -> trap 4 address m
              | otherwise -> case loadable address of
                  Nothing
                    | deviceAddress address -> m {mDevice = True}
                    | otherwise -> trap 5 address m
                  Just word ->
                    let
                        shift = fromIntegral (8 * (address .&. 3))
                        raw = word `shiftR` shift
                        value = case funct3 of
                          0 -> sext 8 raw
                          1 -> sext 16 raw
                          2 -> word
                          4 -> raw .&. 0xff
                          _ -> raw .&. 0xffff
                     in retire next (setReg rd value m)

    store =
      let address = a + sext 12 sImm
          size = case funct3 of 0 -> Just 1; 1 -> Just 2; 2 -> Just 4; _ -> Nothing
       in case size of
            Nothing -> illegal
            Just n
              | address .&. (n - 1) /= 0 -> trap 6 address m
              | address == clintMsip && n == 4 -> retire next (m {mMsip = testBit b 0})
              | otherwise -> case ramIndex address of
                  Nothing
                    | deviceAddress address -> m {mDevice = True}
                    | otherwise -> trap 7 address m
                  Just i ->
                    let shift = fromIntegral (8 * (address .&. 3))
                        mask = (if n == 4 then 0xffff_ffff else bit (8 * fromIntegral n) - 1) `shiftL` shift
                        old = readWord m i
                        new = (old .&. complement mask) .|. ((b `shiftL` shift) .&. mask)
                     in retire next (m {mMem = IM.insert i new (mMem m)})

    opImm =
      let imm = sext 12 iImm
          shamt = fromIntegral (iImm .&. 31)
       in case funct3 of
            0 -> ok (a + imm)
            2 -> ok (if signed a < signed imm then 1 else 0)
            3 -> ok (if a < imm then 1 else 0)
            4 -> ok (a `xor` imm)
            6 -> ok (a .|. imm)
            7 -> ok (a .&. imm)
            1 | funct7 == 0 -> ok (a `shiftL` shamt)
            5 | funct7 == 0 -> ok (a `shiftR` shamt)
              | funct7 == 0x20 -> ok (fromIntegral (signed a `shiftR` shamt))
            _ -> illegal

    op = case (funct7, funct3) of
      (0, 0) -> ok (a + b)
      (0x20, 0) -> ok (a - b)
      (0, 1) -> ok (a `shiftL` sh)
      (0, 2) -> ok (if signed a < signed b then 1 else 0)
      (0, 3) -> ok (if a < b then 1 else 0)
      (0, 4) -> ok (a `xor` b)
      (0, 5) -> ok (a `shiftR` sh)
      (0x20, 5) -> ok (fromIntegral (signed a `shiftR` sh))
      (0, 6) -> ok (a .|. b)
      (0, 7) -> ok (a .&. b)
      (1, f) -> ok (mulDiv f)
      _ -> illegal
      where
        sh = fromIntegral (b .&. 31)

    mulDiv f =
      let sa = fromIntegral (signed a) :: Int64
          sb = fromIntegral (signed b) :: Int64
          ua = fromIntegral a :: Word64
          ub = fromIntegral b :: Word64
          high64 :: Int64 -> Word32
          high64 x = fromIntegral (x `shiftR` 32)
       in case f of
            0 -> a * b
            1 -> high64 (sa * sb)
            2 -> high64 (sa * fromIntegral ub)
            3 -> fromIntegral ((ua * ub) `shiftR` 32)
            4
              | b == 0 -> 0xffff_ffff
              | a == 0x8000_0000 && b == 0xffff_ffff -> a
              | otherwise -> fromIntegral (signed a `quot` signed b)
            5
              | b == 0 -> 0xffff_ffff
              | otherwise -> a `div` b
            6
              | b == 0 -> a
              | a == 0x8000_0000 && b == 0xffff_ffff -> 0
              | otherwise -> fromIntegral (signed a `rem` signed b)
            _
              | b == 0 -> a
              | otherwise -> a `mod` b

    ok v = retire next (setReg rd v m)

    system
      | ins == 0x0000_0073 = trap 11 0 m
      | ins == 0x0010_0073 = trap 3 pc m -- SimpleRisc puts the pc in mtval
      | ins == 0x3020_0073 =
          (m {mPc = mEpc m, mMie = mMpie m, mMpie = True, mRetired = mRetired m + 1})
      | funct3 == 0 || funct3 == 4 = illegal
      | otherwise = csr

    csr =
      let address = iImm
          source = if testBit funct3 2 then rs1 else reg m rs1 -- uimm or rs1
          writes = funct3 .&. 3 == 1 || rs1 /= 0 -- CSRRS/C with x0 or 0 do not write
          readOnly = address == 0x301 || address .&. 0xc00 == 0xc00
       in case readCsr address of
            Nothing -> illegal
            Just old
              | writes && readOnly -> illegal
              | otherwise ->
                  let value = case funct3 .&. 3 of
                        1 -> source
                        2 -> old .|. source
                        _ -> old .&. complement source
                      written = if writes then writeCsr address value m else m
                   in retire next (setReg rd old written)

    readCsr address = case address of
      0x300 -> Just (0x1800 .|. (if mMie m then 8 else 0) .|. (if mMpie m then 0x80 else 0))
      0x301 -> Just 0x4000_1100
      0x304 -> Just (mIe m)
      0x305 -> Just (mTvec m)
      0x310 -> Just 0
      0x340 -> Just (mScratch m)
      0x341 -> Just (mEpc m)
      0x342 -> Just (mCause m)
      0x343 -> Just (mTval m)
      0x344 -> Just (if mMsip m then 8 else 0) -- only msip is ever raised here
      x | x >= 0xf11 && x <= 0xf14 -> Just 0
      _ -> Nothing

writeCsr :: Word32 -> Word32 -> Model -> Model
writeCsr address value m = case address of
  0x300 -> m {mMie = testBit value 3, mMpie = testBit value 7}
  0x304 -> m {mIe = value .&. 0x888}
  0x305 -> m {mTvec = value .&. complement 3}
  0x340 -> m {mScratch = value}
  0x341 -> m {mEpc = value .&. complement 3}
  0x342 -> m {mCause = value}
  0x343 -> m {mTval = value}
  _ -> m -- mstatush and mip ignore writes
