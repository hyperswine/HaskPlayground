{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

-- ============================================================================
-- WasmVM: a small-step WebAssembly (MVP + bulk-memory + sign-ext + trunc_sat)
-- interpreter whose *entire* execution state is one record, so that fuel
-- exhaustion or a host call is "return the record" -- the same trick as the
-- original toy model, now with real semantics:
--
--   * typed operand stack: i32/i64 stored as Word32/Word64 (two's complement
--     wrap for free), f32/f64 as Float/Double
--   * structured control flow with LABELS carrying arities (br with values,
--     multi-value blocks), plus br_table / return / unreachable / select
--   * FUNCTION FRAMES: call, call_indirect (table + runtime type check),
--     locals per activation, result arity, depth limit
--   * globals, memory.size/grow/fill/copy, all load/store widths
--   * linear memory is a mutable unboxed byte array in IO, bounds-checked
--   * host functions are imports; the VM stops with NeedHost and lets the
--     embedder decide what "fd_write" means (capability layer lives outside)
-- ============================================================================

module WASM.WasmVM where

import Control.Monad (forM_, when)
import Data.Array.IO (IOUArray, getBounds, newArray, readArray, writeArray)
import Data.Bits
import Data.IORef
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float
  ( castDoubleToWord64,
    castFloatToWord32,
    castWord32ToFloat,
    castWord64ToDouble,
    double2Float,
    float2Double,
  )

-- ----------------------------------------------------------------------------
-- Values and types
-- ----------------------------------------------------------------------------

data ValType = TI32 | TI64 | TF32 | TF64 deriving (Show, Eq, Ord)

data Val = VI32 !Word32 | VI64 !Word64 | VF32 !Float | VF64 !Double
  deriving (Show, Eq)

typeOf :: Val -> ValType
typeOf = \case VI32 _ -> TI32; VI64 _ -> TI64; VF32 _ -> TF32; VF64 _ -> TF64

zeroOf :: ValType -> Val
zeroOf = \case TI32 -> VI32 0; TI64 -> VI64 0; TF32 -> VF32 0; TF64 -> VF64 0

data FuncType = FuncType {ftParams :: [ValType], ftResults :: [ValType]}
  deriving (Show, Eq)

-- signed views, for printing / tests
i32s :: Val -> Int32
i32s (VI32 w) = fromIntegral w
i32s v = error ("i32s: " ++ show v)

-- ----------------------------------------------------------------------------
-- Instructions
-- ----------------------------------------------------------------------------

data ISz = S32 | S64 deriving (Show, Eq)

data FSz = P32 | P64 deriving (Show, Eq)

data IBin = IAdd | ISub | IMul | IDivS | IDivU | IRemS | IRemU | IAnd | IOr | IXor | IShl | IShrS | IShrU | IRotl | IRotr
  deriving (Show, Eq)

data IUn = IClz | ICtz | IPopcnt | IExt8S | IExt16S | IExt32S deriving (Show, Eq)

data IRel = IEq | INe | ILtS | ILtU | IGtS | IGtU | ILeS | ILeU | IGeS | IGeU deriving (Show, Eq)

data FBin = FAdd | FSub | FMul | FDiv | FMin | FMax | FCopysign deriving (Show, Eq)

data FUn = FAbs | FNeg | FCeil | FFloor | FTrunc | FNearest | FSqrt deriving (Show, Eq)

data FRel = FEq | FNe | FLt | FGt | FLe | FGe deriving (Show, Eq)

data Cvt
  = Wrap -- i32.wrap_i64
  | ExtendS -- i64.extend_i32_s
  | ExtendU
  | TruncS ISz FSz -- iN.trunc_fM_s  (traps)
  | TruncU ISz FSz
  | TruncSatS ISz FSz -- iN.trunc_sat_fM_s
  | TruncSatU ISz FSz
  | ConvertS FSz ISz -- fM.convert_iN_s
  | ConvertU FSz ISz
  | Demote -- f32.demote_f64
  | Promote -- f64.promote_f32
  | Reinterpret ValType -- target type
  deriving (Show, Eq)

-- block type, fully resolved to (params, results)
data BlockType = BT [ValType] [ValType] deriving (Show, Eq)

data Instr
  = Unreachable
  | Nop
  | Block BlockType [Instr]
  | Loop BlockType [Instr]
  | If BlockType [Instr] [Instr]
  | Br Int
  | BrIf Int
  | BrTable [Int] Int
  | Return
  | Call Int
  | CallIndirect Int -- type index
  | Drop
  | Select
  | LocalGet Int
  | LocalSet Int
  | LocalTee Int
  | GlobalGet Int
  | GlobalSet Int
  | Load ValType (Maybe (Int, Bool)) Int -- type, narrow (bytes, signed), offset
  | Store ValType (Maybe Int) Int -- type, narrow bytes, offset
  | MemSize
  | MemGrow
  | MemFill
  | MemCopy
  | Const Val
  | IBinop ISz IBin
  | IUnop ISz IUn
  | IEqz ISz
  | IRelop ISz IRel
  | FBinop FSz FBin
  | FUnop FSz FUn
  | FRelop FSz FRel
  | Cvtop Cvt
  deriving (Show)

-- ----------------------------------------------------------------------------
-- Module and instance
-- ----------------------------------------------------------------------------

data Func = Func {fnType :: FuncType, fnLocals :: [ValType], fnBody :: [Instr]}
  deriving (Show)

data Import = Import {imMod :: String, imName :: String, imType :: FuncType}
  deriving (Show)

data Global = Global {glType :: ValType, glMut :: Bool, glInit :: Val}
  deriving (Show)

data Module = Module
  { mTypes :: IM.IntMap FuncType,
    mImports :: [Import], -- function imports, in index order
    mFuncs :: IM.IntMap Func, -- defined functions (index = funcidx - #imports)
    mTableSize :: Int,
    mTable :: IM.IntMap Int, -- elem index -> funcidx
    mMemPages :: Maybe (Int, Maybe Int), -- (min, max)
    mGlobals :: [Global],
    mData :: [(Int, [Word8])],
    mExports :: M.Map String Int, -- function exports
    mStart :: Maybe Int
  }
  deriving (Show)

emptyModule :: Module
emptyModule = Module IM.empty [] IM.empty 0 IM.empty Nothing [] [] M.empty Nothing

data Memory = Memory
  { memBuf :: IORef (IOUArray Int Word8),
    memMaxPages :: Int
  }

pageSize :: Int
pageSize = 65536

data Inst = Inst {inMod :: Module, inMem :: Memory, inGlobals :: IORef (IM.IntMap Val)}

instantiate :: Module -> IO Inst
instantiate m = do
  let (minP, maxP) = maybe (0, Nothing) id (mMemPages m)
      cap = maybe 1024 (min 1024) maxP -- runtime cap: 64 MiB
  arr <- newArray (0, minP * pageSize - 1) 0
  ref <- newIORef arr
  let mem = Memory ref cap
  forM_ (mData m) $ \(off, bytes) ->
    forM_ (zip [off ..] bytes) $ \(a, b) -> writeArray arr a b
  gref <- newIORef (IM.fromList (zip [0 ..] (map glInit (mGlobals m))))
  pure (Inst m mem gref)

-- function index space = imports ++ defined
funcTypeAt :: Module -> Int -> Maybe FuncType
funcTypeAt m i
  | i < ni = Just (imType (mImports m !! i))
  | otherwise = fnType <$> IM.lookup (i - ni) (mFuncs m)
  where
    ni = length (mImports m)

-- ----------------------------------------------------------------------------
-- Memory operations (all bounds-checked; Left = trap)
-- ----------------------------------------------------------------------------

memSizeBytes :: Memory -> IO Int
memSizeBytes mem = do
  arr <- readIORef (memBuf mem)
  (_, hi) <- getBounds arr
  pure (hi + 1)

oob :: String
oob = "out of bounds memory access"

memReadBytes :: Memory -> Int -> Int -> IO (Either String [Word8])
memReadBytes mem addr n = do
  sz <- memSizeBytes mem
  if addr < 0 || n < 0 || addr + n > sz
    then pure (Left oob)
    else do
      arr <- readIORef (memBuf mem)
      Right <$> mapM (readArray arr) [addr .. addr + n - 1]

memWriteBytes :: Memory -> Int -> [Word8] -> IO (Either String ())
memWriteBytes mem addr bytes = do
  sz <- memSizeBytes mem
  let n = length bytes
  if addr < 0 || addr + n > sz
    then pure (Left oob)
    else do
      arr <- readIORef (memBuf mem)
      forM_ (zip [addr ..] bytes) $ \(a, b) -> writeArray arr a b
      pure (Right ())

memGrow :: Memory -> Int -> IO Int -- returns old page count, or -1
memGrow mem delta = do
  sz <- memSizeBytes mem
  let old = sz `div` pageSize
      new = old + delta
  if delta < 0 || new > memMaxPages mem
    then pure (-1)
    else do
      arr <- readIORef (memBuf mem)
      arr' <- newArray (0, new * pageSize - 1) 0
      forM_ [0 .. sz - 1] $ \i -> readArray arr i >>= writeArray arr' i
      writeIORef (memBuf mem) arr'
      pure old

-- convenience for embedders
memReadString :: Memory -> Int -> Int -> IO (Either String String)
memReadString mem p n = fmap (map (toEnum . fromIntegral)) <$> memReadBytes mem p n

memWriteString :: Memory -> Int -> String -> IO (Either String ())
memWriteString mem p s = memWriteBytes mem p (map (fromIntegral . fromEnum) s)

-- little-endian packing --------------------------------------------------------

bytesToWord :: [Word8] -> Word64
bytesToWord = foldr (\b acc -> (acc `shiftL` 8) .|. fromIntegral b) 0

wordToBytes :: Int -> Word64 -> [Word8]
wordToBytes n w = [fromIntegral (w `shiftR` (8 * i)) | i <- [0 .. n - 1]]

-- sign- or zero-extend the low (8*bytes) bits of a Word64 to 64 bits
extendFrom :: Int -> Bool -> Word64 -> Word64
extendFrom bytes signed w
  | not signed = w .&. mask
  | testBit w (bits - 1) = w .|. complement mask
  | otherwise = w .&. mask
  where
    bits = 8 * bytes
    mask = (1 `shiftL` bits) - 1

-- ----------------------------------------------------------------------------
-- Numerics
-- ----------------------------------------------------------------------------

divZero, intOverflow, badConv :: String
divZero = "integer divide by zero"
intOverflow = "integer overflow"
badConv = "invalid conversion to integer"

ibinG ::
  (FiniteBits w, Integral w, Integral s, Bounded s, Bits s) =>
  (w -> s) ->
  (s -> w) ->
  IBin ->
  w ->
  w ->
  Either String w
ibinG toS fromS op a b = case op of
  IAdd -> Right (a + b)
  ISub -> Right (a - b)
  IMul -> Right (a * b)
  IDivU
    | b == 0 -> Left divZero
    | otherwise -> Right (a `div` b)
  IRemU
    | b == 0 -> Left divZero
    | otherwise -> Right (a `mod` b)
  IDivS
    | b == 0 -> Left divZero
    | toS a == minBound && toS b == -1 -> Left intOverflow
    | otherwise -> Right (fromS (toS a `quot` toS b))
  IRemS
    | b == 0 -> Left divZero
    | toS b == -1 -> Right 0
    | otherwise -> Right (fromS (toS a `rem` toS b))
  IAnd -> Right (a .&. b)
  IOr -> Right (a .|. b)
  IXor -> Right (a `xor` b)
  IShl -> Right (a `shiftL` k)
  IShrU -> Right (a `shiftR` k)
  IShrS -> Right (fromS (toS a `shiftR` k)) -- arithmetic on signed
  IRotl -> Right (a `rotateL` k)
  IRotr -> Right (a `rotateR` k)
  where
    k = fromIntegral (b `mod` fromIntegral (finiteBitSize a))

iunG :: (FiniteBits w, Integral w) => IUn -> w -> w
iunG op a = case op of
  IClz -> fromIntegral (countLeadingZeros a)
  ICtz -> fromIntegral (countTrailingZeros a)
  IPopcnt -> fromIntegral (popCount a)
  IExt8S -> fromIntegral (fromIntegral a :: Int8)
  IExt16S -> fromIntegral (fromIntegral a :: Int16)
  IExt32S -> fromIntegral (fromIntegral a :: Int32)

irelG :: (Ord w, Ord s) => (w -> s) -> IRel -> w -> w -> Bool
irelG toS op a b = case op of
  IEq -> a == b
  INe -> a /= b
  ILtU -> a < b
  IGtU -> a > b
  ILeU -> a <= b
  IGeU -> a >= b
  ILtS -> toS a < toS b
  IGtS -> toS a > toS b
  ILeS -> toS a <= toS b
  IGeS -> toS a >= toS b

class RealFloat f => WFloat f where
  signBitOf :: f -> Bool

instance WFloat Float where signBitOf = flip testBit 31 . castFloatToWord32

instance WFloat Double where signBitOf = flip testBit 63 . castDoubleToWord64

fcopysign :: WFloat f => f -> f -> f
fcopysign a b = if signBitOf b then negate (abs a) else abs a

fbinG :: WFloat f => FBin -> f -> f -> f
fbinG op a b = case op of
  FAdd -> a + b
  FSub -> a - b
  FMul -> a * b
  FDiv -> a / b
  FMin
    | isNaN a || isNaN b -> 0 / 0
    | a == 0 && b == 0 -> if signBitOf a || signBitOf b then -0.0 else 0.0
    | otherwise -> min a b
  FMax
    | isNaN a || isNaN b -> 0 / 0
    | a == 0 && b == 0 -> if signBitOf a && signBitOf b then -0.0 else 0.0
    | otherwise -> max a b
  FCopysign -> fcopysign a b

-- integral-result rounding that preserves the sign of zero (wasm requires it)
roundWith :: WFloat f => (f -> Integer) -> f -> f
roundWith f x
  | isNaN x || isInfinite x = x
  | abs x >= 2 ^ (floatDigits x - 1) = x -- already integral
  | otherwise =
      let r = fromInteger (f x)
       in if r == 0 then fcopysign 0 x else r

funG :: WFloat f => FUn -> f -> f
funG op x = case op of
  FAbs -> abs x
  FNeg -> negate x
  FCeil -> roundWith ceiling x
  FFloor -> roundWith floor x
  FTrunc -> roundWith truncate x
  FNearest -> roundWith round x -- Haskell round = ties-to-even, as wasm
  FSqrt -> sqrt x

frelG :: RealFloat f => FRel -> f -> f -> Bool
frelG op a b = case op of
  FEq -> a == b
  FNe -> a /= b
  FLt -> a < b
  FGt -> a > b
  FLe -> a <= b
  FGe -> a >= b

-- float -> integer truncation with wasm's trap / saturate rules
truncFloat :: RealFloat f => Bool -> Int -> Bool -> f -> Either String Integer
truncFloat signed bits sat x
  | isNaN x = if sat then Right 0 else Left badConv
  | isInfinite x = if sat then Right (if x < 0 then lo else hi) else Left intOverflow
  | t < lo = if sat then Right lo else Left intOverflow
  | t > hi = if sat then Right hi else Left intOverflow
  | otherwise = Right t
  where
    t = truncate x
    (lo, hi)
      | signed = (negate (2 ^ (bits - 1)), 2 ^ (bits - 1) - 1)
      | otherwise = (0, 2 ^ bits - 1)

cvt :: Cvt -> Val -> Either String Val
cvt op v = case (op, v) of
  (Wrap, VI64 w) -> Right (VI32 (fromIntegral w))
  (ExtendS, VI32 w) -> Right (VI64 (fromIntegral (fromIntegral w :: Int32)))
  (ExtendU, VI32 w) -> Right (VI64 (fromIntegral w))
  (TruncS sz _, _) -> mkI sz <$> truncFloat True (isz sz) False (asD v)
  (TruncU sz _, _) -> mkI sz <$> truncFloat False (isz sz) False (asD v)
  (TruncSatS sz _, _) -> mkI sz <$> truncFloat True (isz sz) True (asD v)
  (TruncSatU sz _, _) -> mkI sz <$> truncFloat False (isz sz) True (asD v)
  (ConvertS fs _, _) -> Right (mkF fs (fromInteger (asSigned v)))
  (ConvertU fs _, _) -> Right (mkF fs (fromInteger (asUnsigned v)))
  (Demote, VF64 d) -> Right (VF32 (double2Float d))
  (Promote, VF32 f) -> Right (VF64 (float2Double f))
  (Reinterpret TI32, VF32 f) -> Right (VI32 (castFloatToWord32 f))
  (Reinterpret TI64, VF64 d) -> Right (VI64 (castDoubleToWord64 d))
  (Reinterpret TF32, VI32 w) -> Right (VF32 (castWord32ToFloat w))
  (Reinterpret TF64, VI64 w) -> Right (VF64 (castWord64ToDouble w))
  _ -> Left ("type error in conversion " ++ show op ++ " of " ++ show v)
  where
    isz S32 = 32
    isz S64 = 64
    mkI S32 i = VI32 (fromInteger i)
    mkI S64 i = VI64 (fromInteger i)
    -- NOTE: truncation of an f32 goes via Double, which is exact.
    asD (VF32 f) = float2Double f
    asD (VF64 d) = d
    asD x = error ("expected float, got " ++ show x)
    asSigned (VI32 w) = toInteger (fromIntegral w :: Int32)
    asSigned (VI64 w) = toInteger (fromIntegral w :: Int64)
    asSigned x = error ("expected int, got " ++ show x)
    asUnsigned (VI32 w) = toInteger w
    asUnsigned (VI64 w) = toInteger w
    asUnsigned x = error ("expected int, got " ++ show x)
    mkF :: FSz -> Double -> Val
    mkF P32 d = VF32 (double2Float d) -- via Double: correct for 32-bit sources; f32.convert_i64 may double-round
    mkF P64 d = VF64 d

-- ----------------------------------------------------------------------------
-- VM state
-- ----------------------------------------------------------------------------

data Label = Label
  { lbArity :: !Int, -- values a branch carries to this label
    lbStack :: [Val], -- operand stack below the label's params
    lbCont :: [Instr], -- code after the construct
    lbBr :: [Instr] -- code to run when branching here (loop: re-enter)
  }

data Frame = Frame -- a suspended caller
  { frArity :: !Int,
    frCode :: [Instr],
    frLabels :: [Label],
    frLocals :: IM.IntMap Val,
    frStack :: [Val]
  }

data VM = VM
  { vmInst :: Inst,
    vmCode :: [Instr],
    vmStk :: [Val], -- head = top
    vmLabels :: [Label],
    vmLocals :: IM.IntMap Val,
    vmArity :: !Int, -- result count of the current function
    vmFrames :: [Frame],
    vmDepth :: !Int,
    vmFuel :: !Int
  }

data Step
  = Running VM
  | Yielded VM -- fuel exhausted (cooperative yield point)
  | NeedHost String String [Val] VM -- import (module, name), args in order
  | Finished [Val]
  | Trapped String

maxDepth :: Int
maxDepth = 2000

-- make a VM ready to run function `idx` with `args`
newVM :: Inst -> Int -> [Val] -> Either String VM
newVM inst idx args = do
  let m = inMod inst
      ni = length (mImports m)
  f <- maybe (Left ("no such function " ++ show idx)) Right (IM.lookup (idx - ni) (mFuncs m))
  when (idx < ni) $ Left "cannot start in an imported function"
  when (map typeOf args /= ftParams (fnType f)) $ Left "argument type mismatch"
  pure
    VM
      { vmInst = inst,
        vmCode = fnBody f,
        vmStk = [],
        vmLabels = [],
        vmLocals = IM.fromList (zip [0 ..] (args ++ map zeroOf (fnLocals f))),
        vmArity = length (ftResults (fnType f)),
        vmFrames = [],
        vmDepth = 0,
        vmFuel = 0
      }

-- ----------------------------------------------------------------------------
-- The small-step transition
-- ----------------------------------------------------------------------------

step :: VM -> IO Step
step vm
  | vmFuel vm <= 0 = pure (Yielded vm)
  | otherwise = case vmCode vm of
      [] -> case vmLabels vm of
        (l : ls) ->
          -- fall off the end of a block: keep exactly `arity` results
          pure $
            Running
              vm
                { vmCode = lbCont l,
                  vmLabels = ls,
                  vmStk = take (lbArity l) (vmStk vm) ++ lbStack l
                }
        [] -> pure (doReturn vm)
      (i : rest) -> exec i vm {vmCode = rest, vmFuel = vmFuel vm - 1}

doReturn :: VM -> Step
doReturn vm =
  let rs = take (vmArity vm) (vmStk vm)
   in case vmFrames vm of
        [] -> Finished (reverse rs)
        (f : fs) ->
          Running
            vm
              { vmCode = frCode f,
                vmStk = rs ++ frStack f,
                vmLabels = frLabels f,
                vmLocals = frLocals f,
                vmArity = frArity f,
                vmFrames = fs,
                vmDepth = vmDepth vm - 1
              }

branch :: Int -> VM -> Step
branch n vm = case drop n (vmLabels vm) of
  (l : ls) ->
    Running
      vm
        { vmCode = lbBr l,
          vmLabels = ls,
          vmStk = take (lbArity l) (vmStk vm) ++ lbStack l
        }
  [] -> Trapped ("br depth " ++ show n ++ " exceeds label stack")

enterBlock :: BlockType -> Int -> [Instr] -> [Instr] -> VM -> Step
enterBlock (BT ps rs) arity body brCode vm =
  Running
    vm
      { vmCode = body,
        vmLabels =
          Label
            { lbArity = arity,
              lbStack = drop (length ps) (vmStk vm),
              lbCont = vmCode vm,
              lbBr = brCode
            }
            : vmLabels vm
      }
  where
    _ = rs

pop1 :: VM -> (Val -> VM -> IO Step) -> IO Step
pop1 vm k = case vmStk vm of
  (v : vs) -> k v vm {vmStk = vs}
  [] -> pure (Trapped "stack underflow")

pop2 :: VM -> (Val -> Val -> VM -> IO Step) -> IO Step
pop2 vm k = case vmStk vm of
  (b : a : vs) -> k a b vm {vmStk = vs}
  _ -> pure (Trapped "stack underflow")

push :: Val -> VM -> IO Step
push v vm = pure (Running vm {vmStk = v : vmStk vm})

pushE :: Either String Val -> VM -> IO Step
pushE (Left e) _ = pure (Trapped e)
pushE (Right v) vm = push v vm

i32 :: Val -> Int
i32 (VI32 w) = fromIntegral w
i32 v = error ("expected i32, got " ++ show v)

bool :: Bool -> Val
bool b = VI32 (if b then 1 else 0)

exec :: Instr -> VM -> IO Step
exec ins vm = case ins of
  Unreachable -> pure (Trapped "unreachable executed")
  Nop -> pure (Running vm)
  Block bt@(BT _ rs) body -> pure (enterBlock bt (length rs) body (vmCode vm) vm)
  Loop bt@(BT ps _) body -> pure (enterBlock bt (length ps) body (Loop bt body : vmCode vm) vm)
  If bt@(BT _ rs) t e -> pop1 vm $ \c m ->
    pure (enterBlock bt (length rs) (if i32 c /= 0 then t else e) (vmCode m) m)
  Br n -> pure (branch n vm)
  BrIf n -> pop1 vm $ \c m -> pure (if i32 c /= 0 then branch n m else Running m)
  BrTable ls d -> pop1 vm $ \c m ->
    let k = fromIntegral (i32 c) :: Word32
        tgt = if k < fromIntegral (length ls) then ls !! fromIntegral k else d
     in pure (branch tgt m)
  Return -> pure (doReturn vm)
  Call idx -> callFunc idx vm
  CallIndirect tyIdx -> pop1 vm $ \c m ->
    let mo = inMod (vmInst m)
        k = fromIntegral (i32 c) :: Word32
     in if k >= fromIntegral (mTableSize mo)
          then pure (Trapped "undefined element")
          else case IM.lookup (fromIntegral k) (mTable mo) of
            Nothing -> pure (Trapped "uninitialized element")
            Just fidx -> case (IM.lookup tyIdx (mTypes mo), funcTypeAt mo fidx) of
              (Just want, Just have)
                | want == have -> callFunc fidx m
                | otherwise -> pure (Trapped "indirect call type mismatch")
              _ -> pure (Trapped "bad type index in call_indirect")
  Drop -> pop1 vm $ \_ m -> pure (Running m)
  Select -> pop1 vm $ \c m -> pop2 m $ \v1 v2 m' ->
    push (if i32 c /= 0 then v1 else v2) m'
  LocalGet k -> case IM.lookup k (vmLocals vm) of
    Just v -> push v vm
    Nothing -> pure (Trapped ("unknown local " ++ show k))
  LocalSet k -> pop1 vm $ \v m -> pure (Running m {vmLocals = IM.insert k v (vmLocals m)})
  LocalTee k -> pop1 vm $ \v m ->
    pure (Running m {vmLocals = IM.insert k v (vmLocals m), vmStk = v : vmStk m})
  GlobalGet k -> do
    gs <- readIORef (inGlobals (vmInst vm))
    case IM.lookup k gs of
      Just v -> push v vm
      Nothing -> pure (Trapped ("unknown global " ++ show k))
  GlobalSet k -> pop1 vm $ \v m -> do
    modifyIORef' (inGlobals (vmInst m)) (IM.insert k v)
    pure (Running m)
  Load ty narrow off -> pop1 vm $ \a m -> do
    let ea = fromIntegral (fromIntegral (i32 a) :: Word32) + off
        (bytes, signed) = case narrow of
          Just (b, s) -> (b, s)
          Nothing -> (valSize ty, False)
    r <- memReadBytes (inMem (vmInst m)) ea bytes
    case r of
      Left e -> pure (Trapped e)
      Right bs ->
        let w = extendFrom bytes signed (bytesToWord bs)
            v = case ty of
              TI32 -> VI32 (fromIntegral w)
              TI64 -> VI64 w
              TF32 -> VF32 (castWord32ToFloat (fromIntegral w))
              TF64 -> VF64 (castWord64ToDouble w)
         in push v m
  Store ty narrow off -> pop2 vm $ \a v m -> do
    let ea = fromIntegral (fromIntegral (i32 a) :: Word32) + off
        bytes = maybe (valSize ty) id narrow
        w = case v of
          VI32 x -> fromIntegral x
          VI64 x -> x
          VF32 x -> fromIntegral (castFloatToWord32 x)
          VF64 x -> castDoubleToWord64 x
    r <- memWriteBytes (inMem (vmInst m)) ea (wordToBytes bytes w)
    pure (either Trapped (const (Running m)) r)
  MemSize -> do
    sz <- memSizeBytes (inMem (vmInst vm))
    push (VI32 (fromIntegral (sz `div` pageSize))) vm
  MemGrow -> pop1 vm $ \d m -> do
    old <- memGrow (inMem (vmInst m)) (fromIntegral (fromIntegral (i32 d) :: Word32))
    push (VI32 (fromIntegral old)) m
  MemFill -> pop3 vm $ \d v n m -> do
    r <- memWriteBytes (inMem (vmInst m)) (u32 d) (replicate (u32 n) (fromIntegral (i32 v)))
    pure (either Trapped (const (Running m)) r)
  MemCopy -> pop3 vm $ \d s n m -> do
    let mem = inMem (vmInst m)
    r <- memReadBytes mem (u32 s) (u32 n)
    case r of
      Left e -> pure (Trapped e)
      Right bs -> do
        r' <- memWriteBytes mem (u32 d) bs
        pure (either Trapped (const (Running m)) r')
  Const v -> push v vm
  IBinop S32 op -> pop2 vm $ \a b m -> case (a, b) of
    (VI32 x, VI32 y) -> pushE (VI32 <$> ibinG (fromIntegral :: Word32 -> Int32) fromIntegral op x y) m
    _ -> tyErr
  IBinop S64 op -> pop2 vm $ \a b m -> case (a, b) of
    (VI64 x, VI64 y) -> pushE (VI64 <$> ibinG (fromIntegral :: Word64 -> Int64) fromIntegral op x y) m
    _ -> tyErr
  IUnop S32 op -> pop1 vm $ \a m -> case a of
    VI32 x -> push (VI32 (iunG op x)) m
    _ -> tyErr
  IUnop S64 op -> pop1 vm $ \a m -> case a of
    VI64 x -> push (VI64 (iunG op x)) m
    _ -> tyErr
  IEqz _ -> pop1 vm $ \a m -> case a of
    VI32 x -> push (bool (x == 0)) m
    VI64 x -> push (bool (x == 0)) m
    _ -> tyErr
  IRelop S32 op -> pop2 vm $ \a b m -> case (a, b) of
    (VI32 x, VI32 y) -> push (bool (irelG (fromIntegral :: Word32 -> Int32) op x y)) m
    _ -> tyErr
  IRelop S64 op -> pop2 vm $ \a b m -> case (a, b) of
    (VI64 x, VI64 y) -> push (bool (irelG (fromIntegral :: Word64 -> Int64) op x y)) m
    _ -> tyErr
  FBinop P32 op -> pop2 vm $ \a b m -> case (a, b) of
    (VF32 x, VF32 y) -> push (VF32 (fbinG op x y)) m
    _ -> tyErr
  FBinop P64 op -> pop2 vm $ \a b m -> case (a, b) of
    (VF64 x, VF64 y) -> push (VF64 (fbinG op x y)) m
    _ -> tyErr
  FUnop P32 op -> pop1 vm $ \a m -> case a of
    VF32 x -> push (VF32 (funG op x)) m
    _ -> tyErr
  FUnop P64 op -> pop1 vm $ \a m -> case a of
    VF64 x -> push (VF64 (funG op x)) m
    _ -> tyErr
  FRelop P32 op -> pop2 vm $ \a b m -> case (a, b) of
    (VF32 x, VF32 y) -> push (bool (frelG op x y)) m
    _ -> tyErr
  FRelop P64 op -> pop2 vm $ \a b m -> case (a, b) of
    (VF64 x, VF64 y) -> push (bool (frelG op x y)) m
    _ -> tyErr
  Cvtop op -> pop1 vm $ \a m -> pushE (cvt op a) m
  where
    tyErr = pure (Trapped ("operand type mismatch at " ++ show ins))
    u32 v = fromIntegral (fromIntegral (i32 v) :: Word32) :: Int
    pop3 m k = case vmStk m of
      (c : b : a : vs) -> k a b c m {vmStk = vs}
      _ -> pure (Trapped "stack underflow")

valSize :: ValType -> Int
valSize = \case TI32 -> 4; TI64 -> 8; TF32 -> 4; TF64 -> 8

callFunc :: Int -> VM -> IO Step
callFunc idx vm
  | idx < ni = case mImports mo !! idx of
      Import md nm ft ->
        let np = length (ftParams ft)
            (args, rest) = splitAt np (vmStk vm)
         in pure (NeedHost md nm (reverse args) vm {vmStk = rest})
  | vmDepth vm >= maxDepth = pure (Trapped "call stack exhausted")
  | otherwise = case IM.lookup (idx - ni) (mFuncs mo) of
      Nothing -> pure (Trapped ("unknown function " ++ show idx))
      Just f ->
        let np = length (ftParams (fnType f))
            (args, rest) = splitAt np (vmStk vm)
            frame =
              Frame
                { frArity = vmArity vm,
                  frCode = vmCode vm,
                  frLabels = vmLabels vm,
                  frLocals = vmLocals vm,
                  frStack = rest
                }
         in pure $
              Running
                vm
                  { vmCode = fnBody f,
                    vmStk = [],
                    vmLabels = [],
                    vmLocals = IM.fromList (zip [0 ..] (reverse args ++ map zeroOf (fnLocals f))),
                    vmArity = length (ftResults (fnType f)),
                    vmFrames = frame : vmFrames vm,
                    vmDepth = vmDepth vm + 1
                  }
  where
    mo = inMod (vmInst vm)
    ni = length (mImports mo)

-- push host-call results back (results in order)
resumeHost :: [Val] -> VM -> VM
resumeHost rs vm = vm {vmStk = reverse rs ++ vmStk vm}

-- ----------------------------------------------------------------------------
-- Convenience driver: run to completion with unlimited fuel and a simple
-- synchronous host resolver.  (The actor scheduler in Actors.hs is the
-- interesting driver; this one is for tests.)
-- ----------------------------------------------------------------------------

type HostResolver = String -> String -> [Val] -> IO (Either String [Val])

runToEnd :: HostResolver -> VM -> IO (Either String [Val])
runToEnd host = go . refuel
  where
    refuel vm = vm {vmFuel = 1000000}
    go vm = do
      s <- step vm
      case s of
        Running vm' -> go vm'
        Yielded vm' -> go (refuel vm')
        Finished rs -> pure (Right rs)
        Trapped e -> pure (Left ("trap: " ++ e))
        NeedHost md nm args vm' -> do
          r <- host md nm args
          case r of
            Left e -> pure (Left e)
            Right rs -> go (resumeHost rs vm')

invoke :: HostResolver -> Inst -> String -> [Val] -> IO (Either String [Val])
invoke host inst name args = case M.lookup name (mExports (inMod inst)) of
  Nothing -> pure (Left ("no export " ++ name))
  Just idx -> either (pure . Left) (runToEnd host) (newVM inst idx args)
