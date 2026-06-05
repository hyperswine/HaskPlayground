{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module VM where

import Data.Array
import Data.Array.IO
import Data.Binary (Binary, decode, encode, get, put)
import qualified Data.Binary.Get as BG
import qualified Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word32, Word8)

-- --------------------------------------------------------
-- INSTRUCTION SET
-- --------------------------------------------------------
type Reg = Int -- 0..31

type Label = Int

data Instr
  = -- Loads
    LOAD_INT !Reg !Int64
  | LOAD_STR !Reg !Int -- string pool index
  | LOAD_BOOL !Reg !Bool
  | LOAD_NONE !Reg
  | MOV !Reg !Reg
  | -- Arithmetic
    ADD !Reg !Reg !Reg
  | SUB !Reg !Reg !Reg
  | MUL !Reg !Reg !Reg
  | DIV !Reg !Reg !Reg
  | NEG !Reg !Reg
  | -- Comparison
    CMP_EQ !Reg !Reg !Reg
  | CMP_LT !Reg !Reg !Reg
  | CMP_LE !Reg !Reg !Reg
  | -- Jumps
    JMP !Label
  | JMP_IF !Reg !Label
  | JMP_IFN !Reg !Label
  | -- Functions (de Bruijn env carried in closure)
    CALL !Reg !Reg !Int -- dst, fn-reg, argc
  | TAILCALL !Reg !Int -- fn-reg, argc
  | RET !Reg
  | CLOSURE !Reg !Label !Int -- dst, code-pc, n-captured-regs
  | LOAD_ENV !Reg !Int -- dst, de-Bruijn index into env
  -- Records (row-polymorphic: Map String Value)
  | NEW_RECORD !Reg
  | GET_FIELD !Reg !Reg !Int -- dst, rec-reg, field-pool-index
  | SET_FIELD !Reg !Int !Reg -- rec-reg, field-pool-index, src
  | HAS_FIELD !Reg !Reg !Int
  | -- Sum type dispatch
    MATCH !Reg ![(Int, Label)] !Label
  | -- Maybe / Result
    WRAP_SOME !Reg !Reg
  | WRAP_OK !Reg !Reg
  | WRAP_ERR !Reg !Reg
  | UNWRAP !Reg !Reg
  | UNWRAP_OR !Reg !Reg !Reg
  | IS_SOME !Reg !Reg
  | IS_OK !Reg !Reg
  | -- Lists
    NEW_LIST !Reg
  | LIST_CONS !Reg !Reg !Reg
  | LIST_HEAD !Reg !Reg
  | LIST_TAIL !Reg !Reg
  | LIST_EMPTY !Reg !Reg
  | -- IO
    PRINT !Reg
  | HALT
  deriving (Show)

-- Compact serialisation via Data.Binary
-- (Real flat encoding uses bit-level tags; this uses 1 byte/opcode)
instance Binary Instr where
  put i = case i of
    LOAD_INT d v -> p 0 >> put d >> put v
    LOAD_STR d x -> p 1 >> put d >> put x
    LOAD_BOOL d b -> p 2 >> put d >> put b
    LOAD_NONE d -> p 3 >> put d
    MOV d s -> p 4 >> put d >> put s
    ADD d a b -> p 5 >> put d >> put a >> put b
    SUB d a b -> p 6 >> put d >> put a >> put b
    MUL d a b -> p 7 >> put d >> put a >> put b
    DIV d a b -> p 8 >> put d >> put a >> put b
    NEG d s -> p 9 >> put d >> put s
    CMP_EQ d a b -> p 10 >> put d >> put a >> put b
    CMP_LT d a b -> p 11 >> put d >> put a >> put b
    CMP_LE d a b -> p 12 >> put d >> put a >> put b
    JMP l -> p 13 >> put l
    JMP_IF r l -> p 14 >> put r >> put l
    JMP_IFN r l -> p 15 >> put r >> put l
    CALL d f c -> p 16 >> put d >> put f >> put c
    TAILCALL f c -> p 17 >> put f >> put c
    RET r -> p 18 >> put r
    CLOSURE d l c -> p 19 >> put d >> put l >> put c
    LOAD_ENV d i -> p 20 >> put d >> put i
    NEW_RECORD d -> p 21 >> put d
    GET_FIELD d r i -> p 22 >> put d >> put r >> put i
    SET_FIELD r i s -> p 23 >> put r >> put i >> put s
    HAS_FIELD d r i -> p 24 >> put d >> put r >> put i
    MATCH r alts def -> p 25 >> put r >> put alts >> put def
    WRAP_SOME d s -> p 26 >> put d >> put s
    WRAP_OK d s -> p 27 >> put d >> put s
    WRAP_ERR d s -> p 28 >> put d >> put s
    UNWRAP d s -> p 29 >> put d >> put s
    UNWRAP_OR d v x -> p 30 >> put d >> put v >> put x
    IS_SOME d s -> p 31 >> put d >> put s
    IS_OK d s -> p 32 >> put d >> put s
    NEW_LIST d -> p 33 >> put d
    LIST_CONS d h t -> p 34 >> put d >> put h >> put t
    LIST_HEAD d s -> p 35 >> put d >> put s
    LIST_TAIL d s -> p 36 >> put d >> put s
    LIST_EMPTY d s -> p 37 >> put d >> put s
    PRINT r -> p 38 >> put r
    HALT -> p 39
    where
      p n = put (fromIntegral n :: Word8)

  get = do
    t <- BG.getWord8
    case t of
      0 -> LOAD_INT <$> get <*> get
      1 -> LOAD_STR <$> get <*> get
      2 -> LOAD_BOOL <$> get <*> get
      3 -> LOAD_NONE <$> get
      4 -> MOV <$> get <*> get
      5 -> ADD <$> get <*> get <*> get
      6 -> SUB <$> get <*> get <*> get
      7 -> MUL <$> get <*> get <*> get
      8 -> DIV <$> get <*> get <*> get
      9 -> NEG <$> get <*> get
      10 -> CMP_EQ <$> get <*> get <*> get
      11 -> CMP_LT <$> get <*> get <*> get
      12 -> CMP_LE <$> get <*> get <*> get
      13 -> JMP <$> get
      14 -> JMP_IF <$> get <*> get
      15 -> JMP_IFN <$> get <*> get
      16 -> CALL <$> get <*> get <*> get
      17 -> TAILCALL <$> get <*> get
      18 -> RET <$> get
      19 -> CLOSURE <$> get <*> get <*> get
      20 -> LOAD_ENV <$> get <*> get
      21 -> NEW_RECORD <$> get
      22 -> GET_FIELD <$> get <*> get <*> get
      23 -> SET_FIELD <$> get <*> get <*> get
      24 -> HAS_FIELD <$> get <*> get <*> get
      25 -> MATCH <$> get <*> get <*> get
      26 -> WRAP_SOME <$> get <*> get
      27 -> WRAP_OK <$> get <*> get
      28 -> WRAP_ERR <$> get <*> get
      29 -> UNWRAP <$> get <*> get
      30 -> UNWRAP_OR <$> get <*> get <*> get
      31 -> IS_SOME <$> get <*> get
      32 -> IS_OK <$> get <*> get
      33 -> NEW_LIST <$> get
      34 -> LIST_CONS <$> get <*> get <*> get
      35 -> LIST_HEAD <$> get <*> get
      36 -> LIST_TAIL <$> get <*> get
      37 -> LIST_EMPTY <$> get <*> get
      38 -> PRINT <$> get
      39 -> pure HALT
      _ -> fail $ "unknown opcode " ++ show t

-- --------------------------------------------------------
-- PROGRAM
-- --------------------------------------------------------
data Program = Program
  { instrs :: Array Int Instr,
    strings :: Array Int String,
    fields :: Array Int String,
    size :: !Int
  }

mkProg :: [Instr] -> [String] -> [String] -> Program
mkProg is ss fs =
  Program
    { instrs = listArray (0, length is - 1) is,
      strings = listArray (0, max 0 (length ss - 1)) (if null ss then [""] else ss),
      fields = listArray (0, max 0 (length fs - 1)) (if null fs then [""] else fs),
      size = length is
    }

-- --------------------------------------------------------
-- VALUES
-- --------------------------------------------------------
data Value
  = VInt !Int64
  | VBool !Bool
  | VStr !String
  | VNone
  | VSome Value
  | VOk Value
  | VErr Value
  | VList [Value]
  | VRecord (Map String Value)
  | VClosure !Int [Value] -- code-pc, captured env
  | VTag !Int Value
  deriving (Eq)

pretty :: Value -> String
pretty (VInt n) = show n
pretty (VBool b) = if b then "true" else "false"
pretty (VStr s) = s
pretty VNone = "none"
pretty (VSome v) = "some(" ++ pretty v ++ ")"
pretty (VOk v) = "ok(" ++ pretty v ++ ")"
pretty (VErr v) = "err(" ++ pretty v ++ ")"
pretty (VList vs) = "[" ++ commaS (map pretty vs) ++ "]"
pretty (VRecord m) = "{" ++ commaS (map f (Map.toAscList m)) ++ "}"
  where
    f (k, v) = k ++ ": " ++ pretty v
pretty (VClosure l _) = "<fn@" ++ show l ++ ">"
pretty (VTag t v) = "tag" ++ show t ++ "(" ++ pretty v ++ ")"

commaS :: [String] -> String
commaS [] = ""
commaS [x] = x
commaS (x : xs) = x ++ ", " ++ commaS xs

ctag :: Value -> Int
ctag VNone = 0
ctag (VSome _) = 1
ctag (VOk _) = 2
ctag (VErr _) = 3
ctag (VBool False) = 0
ctag (VBool True) = 1
ctag (VTag t _) = t
ctag _ = 255

-- --------------------------------------------------------
-- VM: 32-register file + call stack
-- --------------------------------------------------------
numR :: Int
numR = 32

data Frame = Frame
  { fRegs :: IOArray Int Value,
    fEnv :: [Value],
    fRetPC :: !Int,
    fRetDst :: !Int
  }

data VM = VM
  { vPC :: IORef Int,
    vStk :: IORef [Frame],
    vReg :: IORef (IOArray Int Value),
    vEnv :: IORef [Value],
    vProg :: Program
  }

newVM :: Program -> IO VM
newVM p = do
  pc <- newIORef 0
  arr <- newArray (0, numR - 1) VNone
  r <- newIORef arr
  e <- newIORef []
  s <- newIORef []
  pure (VM pc s r e p)

rr :: VM -> Int -> IO Value
rr vm i = readIORef (vReg vm) >>= \a -> readArray a i

wr :: VM -> Int -> Value -> IO ()
wr vm i v = readIORef (vReg vm) >>= \a -> writeArray a i v

-- --------------------------------------------------------
-- INTERPRETER
-- --------------------------------------------------------
data Res = Go | Done | Ret Value

exec :: VM -> IO Res
exec vm = do
  pc <- readIORef (vPC vm)
  let p = vProg vm
  let ins = instrs p ! pc
  let next = modifyIORef' (vPC vm) (+ 1)

  case ins of
    LOAD_INT d v -> wr vm d (VInt v) >> next >> pure Go
    LOAD_BOOL d b -> wr vm d (VBool b) >> next >> pure Go
    LOAD_NONE d -> wr vm d VNone >> next >> pure Go
    LOAD_STR d i -> wr vm d (VStr (strings p ! i)) >> next >> pure Go
    MOV d s -> rr vm s >>= wr vm d >> next >> pure Go
    ADD d a b -> ibin vm d a b (+) >> next >> pure Go
    SUB d a b -> ibin vm d a b (-) >> next >> pure Go
    MUL d a b -> ibin vm d a b (*) >> next >> pure Go
    DIV d a b -> ibin vm d a b div >> next >> pure Go
    NEG d s -> rr vm s >>= \(VInt n) -> wr vm d (VInt (negate n)) >> next >> pure Go
    CMP_EQ d a b -> do
      va <- rr vm a
      vb <- rr vm b
      wr vm d (VBool (va == vb)) >> next >> pure Go
    CMP_LT d a b -> icmp vm d a b (<) >> next >> pure Go
    CMP_LE d a b -> icmp vm d a b (<=) >> next >> pure Go
    JMP l -> writeIORef (vPC vm) l >> pure Go
    JMP_IF r l ->
      rr vm r >>= \v ->
        (case v of VBool True -> writeIORef (vPC vm) l; _ -> next) >> pure Go
    JMP_IFN r l ->
      rr vm r >>= \v ->
        ( case v of
            VBool False -> writeIORef (vPC vm) l
            VNone -> writeIORef (vPC vm) l
            _ -> next
        )
          >> pure Go
    CALL dst fnR argc -> do
      fn <- rr vm fnR
      args <- mapM (rr vm) [fnR + 1 .. fnR + argc - 1]
      case fn of
        VClosure lbl cap -> do
          old <- readIORef (vReg vm)
          env <- readIORef (vEnv vm)
          ret <- fmap (+ 1) (readIORef (vPC vm))
          modifyIORef' (vStk vm) (Frame old env ret dst :)
          arr <- newArray (0, numR - 1) VNone
          mapM_ (\(i, v) -> writeArray arr i v) (zip [0 ..] args)
          writeIORef (vReg vm) arr
          writeIORef (vEnv vm) cap
          writeIORef (vPC vm) lbl
          pure Go
        _ -> error $ "CALL: not a closure"
    TAILCALL fnR argc -> do
      fn <- rr vm fnR
      args <- mapM (rr vm) [fnR + 1 .. fnR + argc - 1]
      case fn of
        VClosure lbl cap -> do
          arr <- readIORef (vReg vm)
          mapM_ (\(i, v) -> writeArray arr i v) (zip [0 ..] args)
          writeIORef (vEnv vm) cap
          writeIORef (vPC vm) lbl
          pure Go
        _ -> error "TAILCALL: not a closure"
    RET r -> do
      v <- rr vm r
      stk <- readIORef (vStk vm)
      case stk of
        [] -> pure (Ret v)
        (Frame arr env ret dst : rest) -> do
          writeIORef (vStk vm) rest
          writeArray arr dst v
          writeIORef (vReg vm) arr
          writeIORef (vEnv vm) env
          writeIORef (vPC vm) ret
          pure Go
    CLOSURE d lbl nCap -> do
      cap <- mapM (rr vm) [0 .. nCap - 1]
      wr vm d (VClosure lbl cap) >> next >> pure Go
    LOAD_ENV d i -> do
      env <- readIORef (vEnv vm)
      wr vm d (env !! i) >> next >> pure Go
    NEW_RECORD d -> wr vm d (VRecord Map.empty) >> next >> pure Go
    GET_FIELD d rec fi -> do
      rv <- rr vm rec
      let fld = fields p ! fi
      case rv of
        VRecord m -> wr vm d (fromMaybe VNone (Map.lookup fld m))
        _ -> error "GET_FIELD: not a record"
      next >> pure Go
    SET_FIELD rec fi src -> do
      rv <- rr vm rec
      let fld = fields p ! fi
      v <- rr vm src
      case rv of
        VRecord m -> wr vm rec (VRecord (Map.insert fld v m))
        _ -> error "SET_FIELD: not a record"
      next >> pure Go
    HAS_FIELD d rec fi -> do
      rv <- rr vm rec
      let fld = fields p ! fi
      case rv of
        VRecord m -> wr vm d (VBool (Map.member fld m))
        _ -> wr vm d (VBool False)
      next >> pure Go
    MATCH reg alts def -> do
      v <- rr vm reg
      let t = ctag v
          lbl = case lookup t alts of Just l -> l; Nothing -> def
      writeIORef (vPC vm) lbl >> pure Go
    WRAP_SOME d s -> rr vm s >>= wr vm d . VSome >> next >> pure Go
    WRAP_OK d s -> rr vm s >>= wr vm d . VOk >> next >> pure Go
    WRAP_ERR d s -> rr vm s >>= wr vm d . VErr >> next >> pure Go
    UNWRAP d s ->
      rr vm s >>= \v -> case v of
        VSome x -> wr vm d x >> next >> pure Go
        VOk x -> wr vm d x >> next >> pure Go
        _ -> error $ "UNWRAP: " ++ show (ctag v)
    UNWRAP_OR d val def -> do
      v <- rr vm val
      dv <- rr vm def
      wr vm d (case v of VSome x -> x; VOk x -> x; VNone -> dv; VErr _ -> dv; o -> o)
      next >> pure Go
    IS_SOME d s ->
      rr vm s >>= \v ->
        wr vm d (VBool (case v of VSome _ -> True; _ -> False))
          >> next
          >> pure Go
    IS_OK d s ->
      rr vm s >>= \v ->
        wr vm d (VBool (case v of VOk _ -> True; _ -> False))
          >> next
          >> pure Go
    NEW_LIST d -> wr vm d (VList []) >> next >> pure Go
    LIST_CONS d h t -> do
      vh <- rr vm h
      vt <- rr vm t
      case vt of
        VList xs -> wr vm d (VList (vh : xs))
        _ -> error "LIST_CONS"
      next >> pure Go
    LIST_HEAD d s ->
      rr vm s >>= \case
        VList (x : _) -> wr vm d x >> next >> pure Go
        _ -> error "LIST_HEAD"
    LIST_TAIL d s ->
      rr vm s >>= \case
        VList (_ : xs) -> wr vm d (VList xs) >> next >> pure Go
        _ -> error "LIST_TAIL"
    LIST_EMPTY d s ->
      rr vm s >>= \case
        VList xs -> wr vm d (VBool (null xs)) >> next >> pure Go
        _ -> error "LIST_EMPTY"
    PRINT r -> rr vm r >>= putStrLn . pretty >> next >> pure Go
    HALT -> pure Done

ibin :: VM -> Int -> Int -> Int -> (Int64 -> Int64 -> Int64) -> IO ()
ibin vm d a b f = do
  va <- rr vm a
  vb <- rr vm b
  case (va, vb) of
    (VInt x, VInt y) -> wr vm d (VInt (f x y))
    _ -> error "arithmetic on non-Int"

icmp :: VM -> Int -> Int -> Int -> (Int64 -> Int64 -> Bool) -> IO ()
icmp vm d a b f = do
  va <- rr vm a
  vb <- rr vm b
  case (va, vb) of
    (VInt x, VInt y) -> wr vm d (VBool (f x y))
    _ -> error "CMP on non-Int"

run :: Program -> IO ()
run p = do
  vm <- newVM p
  let loop = exec vm >>= \case Go -> loop; Done -> pure (); Ret _ -> pure ()
  loop

-- --------------------------------------------------------
-- EXAMPLE PROGRAMS
-- --------------------------------------------------------

-- Sum 1..10 via tail loop (expected: 55)
progSum :: [Instr]
progSum =
  [ LOAD_INT 0 10, -- 0  r0=10
    LOAD_INT 1 0, -- 1  r1=0 (acc)
    LOAD_INT 3 0, -- 2  r3=0 (const)
    -- loop@3
    CMP_EQ 2 0 3, -- 3  r2 = (r0==0)
    JMP_IF 2 9, -- 4  if r2 goto done
    ADD 1 1 0, -- 5  r1 += r0
    LOAD_INT 2 1, -- 6  r2=1
    SUB 0 0 2, -- 7  r0 -= 1
    JMP 3, -- 8  loop
    -- done@9
    PRINT 1, -- 9
    HALT -- 10
  ]

-- Record: {name: "Ursula K. Le Guin", age: 73} -> update age -> print both
-- fields[0]="name" fields[1]="age"  strings[0]="Ursula K. Le Guin"
progRecord :: [Instr]
progRecord =
  [ NEW_RECORD 0, -- 0  r0={}
    LOAD_STR 1 0, -- 1  r1="Ursula K. Le Guin"
    SET_FIELD 0 0 1, -- 2  r0.name = r1
    LOAD_INT 1 73, -- 3  r1=73
    SET_FIELD 0 1 1, -- 4  r0.age = 73
    GET_FIELD 2 0 0, -- 5  r2=r0.name
    PRINT 2, -- 6  -> "Ursula K. Le Guin"
    GET_FIELD 2 0 1, -- 7  r2=73
    LOAD_INT 3 1, -- 8  r3=1
    ADD 2 2 3, -- 9  r2=74
    SET_FIELD 0 1 2, -- 10 r0.age=74
    GET_FIELD 2 0 1, -- 11 r2=r0.age
    PRINT 2, -- 12 -> 74
    HALT -- 13
  ]

-- MATCH: Some(42) -> print 42 / None -> print "nothing"
-- strings[0]="nothing"
progMaybe :: [Instr]
progMaybe =
  [ LOAD_INT 0 42, -- 0  r0=42
    WRAP_SOME 1 0, -- 1  r1=Some(42)
    MATCH 1 [(1, 4)] 7, -- 2  Some->4, else->7
    JMP 9, -- 3  (dead)
    -- Some branch @ 4
    UNWRAP 2 1, -- 4  r2=42
    PRINT 2, -- 5  -> 42
    JMP 9, -- 6
    -- None branch @ 7
    LOAD_STR 2 0, -- 7  r2="nothing"
    PRINT 2, -- 8
    -- done @ 9
    HALT -- 9
  ]

-- List [1,2,3] head/tail traversal
progList :: [Instr]
progList =
  [ NEW_LIST 0, -- 0  r0=[]
    LOAD_INT 1 3, -- 1
    LIST_CONS 0 1 0, -- 2  r0=[3]
    LOAD_INT 1 2, -- 3
    LIST_CONS 0 1 0, -- 4  r0=[2,3]
    LOAD_INT 1 1, -- 5
    LIST_CONS 0 1 0, -- 6  r0=[1,2,3]
    -- loop@7
    LIST_EMPTY 1 0, -- 7  r1=null?
    JMP_IF 1 13, -- 8  done
    LIST_HEAD 2 0, -- 9  r2=head
    PRINT 2, -- 10
    LIST_TAIL 0 0, -- 11 r0=tail
    JMP 7, -- 12
    -- done@13
    HALT -- 13
  ]

-- Closure: square(7) = 49 via CALL+RET
-- main(0-4), square body(5-6)
progClosure :: [Instr]
progClosure =
  [ CLOSURE 0 5 0, -- 0  r0=closure(pc=5, 0 caps)
    LOAD_INT 1 7, -- 1  r1=7
    CALL 2 0 2, -- 2  r2=r0(r1)
    PRINT 2, -- 3  -> 49
    HALT, -- 4
    -- square @ 5: arg in r0
    MUL 2 0 0, -- 5  r2=r0*r0
    RET 2 -- 6
  ]

-- De Bruijn closure: make_adder(5)(3) = 8
-- Closure captures r0=5; body: LOAD_ENV 1 0 -> r1=5; ADD 2 0 1 -> r2=3+5
progEnv :: [Instr]
progEnv =
  [ LOAD_INT 0 5, -- 0  r0=5 (to capture)
    CLOSURE 1 8 1, -- 1  r1=closure(pc=8, caps=[r0])
    MOV 3 1, -- 2  r3=closure (fn slot)
    LOAD_INT 4 3, -- 3  r4=3 (argument)
    CALL 5 3 2, -- 4  r5=r3(r4)
    PRINT 5, -- 5  -> 8
    HALT, -- 6
    HALT, -- 7  (alignment pad)
    -- adder body @ 8: arg in r0
    LOAD_ENV 1 0, -- 8  r1=env[0]=5
    ADD 2 0 1, -- 9  r2=3+5
    RET 2 -- 10
  ]

-- Fibonacci via TAILCALL (no stack growth)
-- fib_iter n acc: if n==0 return acc else tailcall fib_iter (n-1) (acc+n)
-- Main: load closure, call fib(10, 0)
-- Layout: main(0-5), fib body(6-14)
progFib :: [Instr]
progFib =
  [ CLOSURE 0 6 0, -- 0  r0 = closure(pc=6, no caps)
    MOV 1 0, -- 1  r1 = fn (for call)
    LOAD_INT 2 10, -- 2  r2 = 10 (n)
    LOAD_INT 3 0, -- 3  r3 = 0  (acc)
    CALL 4 1 3, -- 4  r4 = r1(r2,r3) -- fn in r1, args r2,r3, argc=3
    PRINT 4, -- 5  -> 55
    HALT, -- 5b -- unreachable but needs a HALT
    -- fib body @ 6: r0=n, r1=acc
    LOAD_INT 2 0, -- 6  r2=0
    CMP_EQ 3 0 2, -- 7  r3=(n==0)
    JMP_IF 3 14, -- 8  if n==0 goto return
    ADD 4 1 0, -- 9  r4=acc+n
    LOAD_INT 5 1, -- 10 r5=1
    SUB 6 0 5, -- 11 r6=n-1
    -- TAILCALL: fn=r7, args in r8,r9; set them up first
    MOV 7 0, -- 12 r7=... hmm, we need closure ref
    -- Use LOAD_ENV instead: capture self via env
    HALT, -- placeholder
    RET 1 -- 14: base case, return acc
  ]

-- Simpler fib: iterative, no closure needed (inline loop)
-- Compute fib(10) = 55 iteratively: a=0,b=1, loop 10 times: a,b = b, a+b
progFibIter :: [Instr]
progFibIter =
  [ LOAD_INT 0 10, -- 0  r0=10 (counter)
    LOAD_INT 1 0, -- 1  r1=a=0
    LOAD_INT 2 1, -- 2  r2=b=1
    LOAD_INT 5 0, -- 3  r5=0 (const)
    -- loop@4
    CMP_EQ 3 0 5, -- 4  r3=(counter==0)
    JMP_IF 3 11, -- 5  done
    ADD 4 1 2, -- 6  r4=a+b
    MOV 1 2, -- 7  a=b
    MOV 2 4, -- 8  b=a+b
    LOAD_INT 4 1, -- 9  r4=1
    SUB 0 0 4, -- 10 counter--
    JMP 4, -- 11... wait label collision
    HALT
  ]

-- fix label collision
progFibIter2 :: [Instr]
progFibIter2 =
  [ LOAD_INT 0 10, -- 0  counter=10
    LOAD_INT 1 0, -- 1  a=0
    LOAD_INT 2 1, -- 2  b=1
    LOAD_INT 5 0, -- 3  zero=0
    -- loop @ 4
    CMP_EQ 3 0 5, -- 4  r3=(counter==0)
    JMP_IF 3 12, -- 5  if done goto 12
    ADD 4 1 2, -- 6  tmp=a+b
    MOV 1 2, -- 7  a=b
    MOV 2 4, -- 8  b=tmp
    LOAD_INT 4 1, -- 9  one=1
    SUB 0 0 4, -- 10 counter--
    JMP 4, -- 11 loop
    -- done @ 12
    PRINT 1, -- 12 print a (fib(10)=55)
    HALT -- 13
  ]

-- --------------------------------------------------------
-- SERIALISE ROUNDTRIP DEMO
-- --------------------------------------------------------
roundtrip :: IO ()
roundtrip = do
  let bs = encode (progSum, [] :: [String], [] :: [String])
  putStrLn $ "  progSum serialised: " ++ show (BL.length bs) ++ " bytes"
  let (is, ss, fs) = decode bs :: ([Instr], [String], [String])
  let p = mkProg is ss fs
  putStr "  roundtrip result (sum 1..10): "
  run p

-- opcode size survey
opSizes :: IO ()
opSizes = do
  let examples =
        [ ("LOAD_INT r0 42", LOAD_INT 0 42),
          ("ADD r2 r0 r1", ADD 2 0 1),
          ("CMP_EQ r2 r0 r1", CMP_EQ 2 0 1),
          ("JMP 100", JMP 100),
          ("CALL r2 r0 3", CALL 2 0 3),
          ("TAILCALL r0 2", TAILCALL 0 2),
          ("GET_FIELD r2 r0 0", GET_FIELD 2 0 0),
          ("SET_FIELD r0 0 r1", SET_FIELD 0 0 1),
          ("MATCH r1 [(1,5)] 9", MATCH 1 [(1, 5)] 9),
          ("WRAP_SOME r1 r0", WRAP_SOME 1 0),
          ("HALT", HALT)
        ]
  mapM_
    ( \(nm, i) ->
        putStrLn $ "  " ++ nm ++ "  ->  " ++ show (BL.length (encode i)) ++ " bytes"
    )
    examples

-- --------------------------------------------------------
-- MAIN
-- --------------------------------------------------------
sep :: String -> IO ()
sep s = putStrLn ("\n--- " ++ s ++ " ---")

ex :: String -> [Instr] -> [String] -> [String] -> IO ()
ex name is ss fs = sep name >> run (mkProg is ss fs)

main :: IO ()
main = do
  ex
    "Sum 1..10 (expected 55)"
    progSum
    []
    []

  ex
    "Fib(10) iterative (expected 55)"
    progFibIter2
    []
    []

  ex
    "Record {name, age}: get/set/update"
    progRecord
    ["Ursula K. Le Guin"]
    ["name", "age"]

  ex
    "MATCH on Maybe: Some(42) -> print 42"
    progMaybe
    ["nothing"]
    []

  ex
    "List [1,2,3]: head/tail traversal"
    progList
    []
    []

  ex
    "Closure CALL+RET: square(7) -> 49"
    progClosure
    []
    []

  ex
    "De Bruijn env: make_adder(5)(3) -> 8"
    progEnv
    []
    []

  sep "Serialise / deserialise roundtrip"
  roundtrip

  sep "Instruction encoding sizes (Data.Binary, 1 byte/opcode)"
  opSizes

  sep "Compact tag bits (flat-style)"
  putStrLn $ "  40 opcodes would need: " ++ show (ceiling (logBase 2 40 :: Double) :: Int) ++ " bits/opcode (vs 8 here)"
  putStrLn $ "  savings vs byte tags: " ++ show (8 - ceiling (logBase 2 40 :: Double) :: Int) ++ " bits per instruction"
