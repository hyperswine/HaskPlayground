{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use record patterns" #-}
{-# HLINT ignore "Use unless" #-}

module FPInterpreter (Expr (..), Pattern (..), Value (..), TypeName, Env, primEnv, ActorState (..), ActorM (..), runProgram, eval, VFSHandlers (..), VFSMap, emptyVFSMap) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (void, when)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)

-- =============================================================================
-- VIRTUAL FILE SYSTEM TYPES
-- Allows Haskell code to register path-keyed handlers that fplang can call
-- via fopen / fread / fwrite / fclose / fseek.
-- =============================================================================

-- | Per-path handler bundle.  Each handler receives the current mutable file
--   state plus any extra arguments supplied by fplang.
--   open  : extra args (e.g. mode)           → initial state
--   read  : (current state, extra args)      → (result data, new state)
--   write : (current state, extra args)      → new state
--   close : current state                    → ()
--   seek  : (current state, extra args)      → new state
data VFSHandlers = VFSHandlers
  { vhOpen  :: [Value] -> ActorM Value
  , vhRead  :: Value -> [Value] -> ActorM (Value, Value)
  , vhWrite :: Value -> [Value] -> ActorM Value
  , vhClose :: Value -> ActorM ()
  , vhSeek  :: Value -> [Value] -> ActorM Value
  }

type VFSMap = Map FilePath VFSHandlers

emptyVFSMap :: VFSMap
emptyVFSMap = Map.empty

-- | One entry in an actor's open-file-descriptor table.
data FDEntry = FDEntry
  { fdeHandlers :: VFSHandlers
  , fdeState    :: IORef Value   -- mutable per-fd state (position, buffers, …)
  }

type FDTable = Map Int FDEntry

-- =============================================================================
-- STAGE 1 ── AST TYPES
-- The language's three core data types: runtime values, syntax tree nodes,
-- and patterns used in destructuring / receive clauses.
-- =============================================================================

-- -----------------------------------------------------------------------------
-- § Values
-- Every value carries a constructor tag so type/1 is always available.
-- -----------------------------------------------------------------------------

data Value
  = VInt Int
  | VBool Bool
  | VStr String
  | VUnit
  | -- Tagged constructor: name + payload  (covers user-defined types)
    VTagged String [Value]
  | -- First-class function (name for function/1, closure env, arg names, body)
    VFn String Env [String] Expr
  | -- Built-in primitive
    VPrim String ([Value] -> ActorM Value)
  | -- Actor address  (processId, actorId)
    VAddr ActorAddr
  | -- A list (sugar over VTagged really, kept separate for convenience)
    VList [Value]
  | -- The metatype returned by type/1  e.g. VType "Int"
    VType String

instance Show Value where
  show (VInt n) = show n
  show (VBool b) = show b
  show (VStr s) = show s
  show VUnit = "()"
  show (VTagged t []) = t
  show (VTagged t vs) = "(" ++ t ++ " " ++ unwords (map show vs) ++ ")"
  show (VFn n _ _ _) = "<fn:" ++ n ++ ">"
  show (VPrim n _) = "<prim:" ++ n ++ ">"
  show (VAddr a) = "<actor:" ++ show a ++ ">"
  show (VList vs) = "[" ++ commas vs ++ "]"
  show (VType t) = "Type:" ++ t

commas :: (Show a) => [a] -> String
commas = foldr (\x acc -> show x ++ if null acc then "" else ", " ++ acc) ""

-- -----------------------------------------------------------------------------
-- § Expressions  (the AST)
-- -----------------------------------------------------------------------------

data Expr
  = Lit Value
  | Var String
  | App Expr [Expr] -- f args
  | Lam [String] Expr -- \x y -> body
  | Let String Expr Expr -- let x = e in body
  | If Expr Expr Expr
  | Seq [Expr] -- sequential block, last value returned
  | Send Expr Expr Expr -- send addr actorId msg
  | Receive [(Pattern, Expr)] -- receive { pat -> expr, ... }
  | Spawn String Expr [Expr] -- spawn "name" fn initArgs
  | Self -- own ActorAddr
  | TypeOf Expr -- type/1
  | FnOf Expr -- function/1  (name of a VFn)
  | Tag String [Expr] -- Tag "Foo" [e1, e2]  → VTagged
  | Alloc Expr -- alloc v  (adds to actor RC heap)
  | Dealloc Expr -- dealloc ref
  | GetRef Expr -- deref a heap ref
  -- Recursion
  | Fix String [String] Expr -- fix f (x y ...) = body  (f is self-ref in body)
  -- Isomorphism registry
  | IsoDecl TypeName TypeName Expr Expr -- iso A B fwd bkwd  (registers pair)
  | LookupIso TypeName TypeName -- iso A B  → Just (fwd, bkwd) | Nothing
  -- Direct pattern match
  | Match Expr [(Pattern, Expr)] -- match(scrutinee) { Pat => expr | ... }
  deriving (Show)

-- -----------------------------------------------------------------------------
-- § Patterns  (used in Receive clauses and future pattern-match expressions)
-- -----------------------------------------------------------------------------

data Pattern
  = PVar String -- binds anything
  | PTagged String [Pattern] -- constructor match
  | PLit Value -- literal match
  | PWild -- _
  deriving (Show)

-- =============================================================================
-- STAGE 2 ── ENVIRONMENT
-- A flat name → Value map threaded through evaluation.
-- Lookup fails loudly on unbound names; extend by insertion.
-- =============================================================================

type Env = Map String Value

envLookup :: String -> Env -> Value
envLookup k e = fromMaybe (error $ "Unbound: " ++ k) (Map.lookup k e)

envExtend :: String -> Value -> Env -> Env
envExtend = Map.insert

-- =============================================================================
-- STAGE 3 ── RUNTIME INFRASTRUCTURE
-- Pure data structures and their operations for the four shared resources
-- (heap, mailbox, iso map, registry) and the per-actor state record that
-- bundles them together.  No IO or ActorM yet — just plain types.
-- =============================================================================

-- -----------------------------------------------------------------------------
-- § Actor identity
-- Each actor is addressed by (ProcessId, ActorId); within a process,
-- ActorId (a String) is the stable handle used for sends.
-- -----------------------------------------------------------------------------

type ProcessId = Int

type ActorId = String

type ActorAddr = (ProcessId, ActorId)

-- -----------------------------------------------------------------------------
-- § Heap  (per-actor reference-counted allocator)
-- Maps HeapRef → (Value, refcount).  Dealloc decrements; drops at 0.
-- -----------------------------------------------------------------------------

type HeapRef = Int

-- heapCells: value + refcount
data Heap = Heap {heapCells :: Map HeapRef (Value, Int), heapNextRef :: HeapRef}

emptyHeap = Heap Map.empty 0

heapAlloc :: Value -> Heap -> (HeapRef, Heap)
heapAlloc v h = let ref = heapNextRef h in (ref, h {heapCells = Map.insert ref (v, 1) (heapCells h), heapNextRef = ref + 1})

heapDealloc :: HeapRef -> Heap -> Heap
heapDealloc ref h = case Map.lookup ref (heapCells h) of
  Nothing -> error $ "heapDealloc: invalid ref " ++ show ref
  Just (_, 1) -> h {heapCells = Map.delete ref (heapCells h)}
  Just (v, rc) -> h {heapCells = Map.insert ref (v, rc - 1) (heapCells h)}

heapGet :: HeapRef -> Heap -> Value
heapGet ref h = case Map.lookup ref (heapCells h) of
  Nothing -> error $ "heapGet: invalid ref " ++ show ref
  Just (v, _) -> v

-- -----------------------------------------------------------------------------
-- § Mailbox  (bounded, crashes on overflow)
-- -----------------------------------------------------------------------------

mailboxCapacity :: Int
mailboxCapacity = 64

data Mailbox = Mailbox {mbQueue :: TQueue Value, mbSize :: TVar Int}

newMailbox :: STM Mailbox
newMailbox = Mailbox <$> newTQueue <*> newTVar 0

-- Returns False if overflow → caller should crash
mailboxPush :: Mailbox -> Value -> STM Bool
mailboxPush mb v = do
  sz <- readTVar (mbSize mb)
  if sz >= mailboxCapacity
    then return False
    else do
      writeTQueue (mbQueue mb) v
      modifyTVar' (mbSize mb) (+ 1)
      return True

mailboxPop :: Mailbox -> STM Value
mailboxPop mb = do
  v <- readTQueue (mbQueue mb)
  modifyTVar' (mbSize mb) (subtract 1)
  return v

-- -----------------------------------------------------------------------------
-- § Iso map  (process-global type isomorphism registry)
-- Maps (TypeName, TypeName) → (fwd :: a→b, bkwd :: b→a)
-- -----------------------------------------------------------------------------

type TypeName = String

type IsoKey = (TypeName, TypeName)

type IsoMap = TVar (Map IsoKey (Value, Value))

newIsoMap :: IO IsoMap
newIsoMap = newTVarIO Map.empty

isoRegister :: IsoMap -> TypeName -> TypeName -> Value -> Value -> IO ()
isoRegister im a b fwd bkwd = atomically $ modifyTVar' im (Map.insert (a, b) (fwd, bkwd))

isoLookup :: IsoMap -> TypeName -> TypeName -> IO (Maybe (Value, Value))
isoLookup im a b = Map.lookup (a, b) <$> readTVarIO im

-- -----------------------------------------------------------------------------
-- § Registry  (process-global actor directory: ActorId → Mailbox)
-- -----------------------------------------------------------------------------

type Registry = TVar (Map ActorId Mailbox)

newRegistry :: IO Registry
newRegistry = newTVarIO Map.empty

registryRegister :: Registry -> ActorId -> Mailbox -> IO ()
registryRegister reg aid mb = atomically $ modifyTVar' reg (Map.insert aid mb)

registryLookup :: Registry -> ActorId -> IO (Maybe Mailbox)
registryLookup reg aid = Map.lookup aid <$> readTVarIO reg

-- -----------------------------------------------------------------------------
-- § Actor state  (mutable, owned by one thread)
-- Bundles an actor's private heap with the shared registry and iso map.
-- -----------------------------------------------------------------------------

-- actorRegistry: shared across all actors in process. actorIsoMap: shared across all actors in process.
-- actorVFS: read-only virtual file system shared across all actors in the process.
-- actorFDTable: per-actor open file-descriptor table (private mutable state).
data ActorState = ActorState
  { actorAddr     :: ActorAddr
  , actorHeap     :: IORef Heap
  , actorMailbox  :: Mailbox
  , actorRegistry :: Registry
  , actorIsoMap   :: IsoMap
  , actorVFS      :: VFSMap
  , actorFDTable  :: IORef FDTable
  }

-- =============================================================================
-- STAGE 4 ── EFFECT LAYER  (ActorM monad)
-- IO threaded with an implicit ActorState.  All eval-time effects run here.
-- =============================================================================

newtype ActorM a = ActorM {runActorM :: ActorState -> IO a}

instance Functor ActorM where
  fmap f (ActorM g) = ActorM (fmap f . g)

instance Applicative ActorM where
  pure x = ActorM (const (pure x))
  ActorM f <*> ActorM x = ActorM $ \s -> f s <*> x s

instance Monad ActorM where
  return = pure
  ActorM x >>= f = ActorM $ \s -> x s >>= \a -> runActorM (f a) s

liftIO :: IO a -> ActorM a
liftIO io = ActorM $ const io

getActorState :: ActorM ActorState
getActorState = ActorM return

actorFail :: String -> ActorM a
actorFail msg = ActorM $ \st -> error $ "[Actor " ++ show (actorAddr st) ++ "] " ++ msg

-- =============================================================================
-- STAGE 5 ── EFFECTFUL OPERATIONS
-- ActorM actions for heap mutation, inter-actor messaging, and spawning.
-- These are thin wrappers over the pure Stage 3 functions that thread IO.
-- =============================================================================

-- -----------------------------------------------------------------------------
-- § Heap operations
-- -----------------------------------------------------------------------------

allocValue :: Value -> ActorM Value
allocValue v = ActorM $ \st -> do
  heap <- readIORef $ actorHeap st
  let (ref, heap') = heapAlloc v heap
  writeIORef (actorHeap st) heap'
  return (VTagged "Ref" [VInt ref])

deallocRef :: Value -> ActorM ()
deallocRef (VTagged "Ref" [VInt ref]) = ActorM $ \st -> do modifyIORef' (actorHeap st) (heapDealloc ref)
deallocRef v = actorFail $ "dealloc: not a Ref: " ++ show v

getRef :: Value -> ActorM Value
getRef (VTagged "Ref" [VInt ref]) = ActorM $ \st -> do
  heap <- readIORef (actorHeap st)
  return (heapGet ref heap)
getRef v = actorFail $ "getRef: not a Ref: " ++ show v

-- -----------------------------------------------------------------------------
-- § Messaging  (send / receive / spawn)
-- -----------------------------------------------------------------------------

actorSend :: ActorId -> Value -> ActorM ()
actorSend targetId msg = ActorM $ \st -> do
  mMb <- registryLookup (actorRegistry st) targetId
  case mMb of
    Nothing -> error $ "send: unknown actor: " ++ targetId
    Just mb -> do
      ok <- atomically (mailboxPush mb msg)
      when (not ok) $ error $ "send: mailbox overflow for actor: " ++ targetId

actorReceive :: ActorM Value
actorReceive = ActorM $ \st -> atomically (mailboxPop (actorMailbox st))

-- a lot of storage stuff and boiler
actorSpawn :: ActorState -> Env -> [String] -> Expr -> [Value] -> String -> ActorM Value
actorSpawn parentState env params body initArgs requestedId = ActorM $ \_ -> do
  let reg = actorRegistry parentState
      im = actorIsoMap parentState
      vfs = actorVFS parentState
  actualId <- freshActorId reg requestedId
  mb <- atomically newMailbox
  ref <- newIORef emptyHeap
  childFDTable <- newIORef Map.empty   -- each actor gets its own fresh FD table
  let childAddr = (fst (actorAddr parentState), actualId)
      childState = ActorState childAddr ref mb reg im vfs childFDTable
      baseChildEnv = Map.insert "self" (VAddr childAddr) env
      childEnv = foldr (\(p, v) e -> envExtend p v e) baseChildEnv (zip params initArgs)
  registryRegister reg actualId mb
  void $ forkIO $ void $ runActorM (eval childEnv body) childState
  return (VAddr childAddr)

actorCounter :: IORef Int
{-# NOINLINE actorCounter #-}
actorCounter = unsafePerformIO (newIORef 0)

freshActorId :: Registry -> ActorId -> IO ActorId
freshActorId reg base = do
  m <- readTVarIO reg
  if base `Map.notMember` m
    then return base
    else do
      n <- atomicModifyIORef' actorCounter (\n -> (n + 1, n))
      let candidate = base ++ "-" ++ show n

      -- retry on collision on else
      if candidate `Map.notMember` m then return candidate else freshActorId reg base

-- =============================================================================
-- STAGE 6 ── EVALUATION
-- eval walks the AST and reduces every node to a Value inside ActorM.
-- Helper functions (value reflection, pattern matching) live here first so
-- the main eval body reads as a straight top-to-bottom reduction table.
-- =============================================================================

-- -----------------------------------------------------------------------------
-- § Value reflection  (type/1 and function/1)
-- -----------------------------------------------------------------------------

typeOf :: Value -> Value
typeOf (VInt _) = VType "Int"
typeOf (VBool _) = VType "Bool"
typeOf (VStr _) = VType "Str"
typeOf VUnit = VType "Unit"
typeOf (VTagged t _) = VType t
typeOf (VFn _ _ _ _) = VType "Fn"
typeOf (VPrim _ _) = VType "Fn"
typeOf (VAddr _) = VType "ActorAddr"
typeOf (VList _) = VType "List"
typeOf (VType _) = VType "Type"

fnNameOf :: Value -> Value
fnNameOf (VFn n _ _ _) = VStr n
fnNameOf (VPrim n _) = VStr n
fnNameOf v = error $ "function/1: not a function: " ++ show v

-- -----------------------------------------------------------------------------
-- § Pattern matching
-- -----------------------------------------------------------------------------

matchPattern :: Pattern -> Value -> Maybe Env
matchPattern PWild _ = Just Map.empty
matchPattern (PVar x) v = Just (Map.singleton x v)
matchPattern (PLit l) v
  | showVal l == showVal v = Just Map.empty
  | otherwise = Nothing
-- matching a constructor, need to match all its fields
matchPattern (PTagged t ps) (VTagged t' vs) | t == t' && length ps == length vs = foldl (\acc (p, v) -> acc >>= \e -> fmap (Map.union e) (matchPattern p v)) (Just Map.empty) (zip ps vs)
-- VList sugar: treat VList [] as Nil, VList (v:vs) as Cons(v, VList vs)
-- so that [1,2,3] literals can be destructured with the same Nil/Cons patterns
matchPattern (PTagged "Nil" []) (VList []) = Just Map.empty
matchPattern (PTagged "Cons" [ph, pt]) (VList (v : vs)) =
  matchPattern ph v >>= \he ->
    matchPattern pt (VList vs) >>= \te ->
      Just (Map.union he te)
matchPattern _ _ = Nothing

showVal :: Value -> String
showVal = show

matchClauses :: [(Pattern, Expr)] -> Value -> Maybe (Env, Expr)
matchClauses [] _ = Nothing
matchClauses ((p, e) : rest) v = case matchPattern p v of
  Just binds -> Just (binds, e)
  Nothing -> matchClauses rest v

-- -----------------------------------------------------------------------------
-- § eval  (the main reduction table)
-- -----------------------------------------------------------------------------

eval :: Env -> Expr -> ActorM Value
eval env = \case
  Lit v -> return v
  Var x -> return (envLookup x env)
  Lam params body ->
    return (VFn "<lambda>" env params body)
  Let x e body -> do
    v <- eval env e
    eval (envExtend x v env) body
  If cond t f -> do
    cv <- eval env cond
    case cv of
      VBool True -> eval env t
      VBool False -> eval env f
      _ -> actorFail "if: condition not a Bool"
  Seq [] -> return VUnit
  Seq [e] -> eval env e
  Seq (e : es) -> eval env e >> eval env (Seq es)
  App f argExprs -> do
    fv <- eval env f
    args <- mapM (eval env) argExprs
    applyFn fv args
  Send addrExpr actorIdExpr msgExpr -> do
    -- for inter-process this would carry pid
    _addr <- eval env addrExpr
    targetV <- eval env actorIdExpr
    msg <- eval env msgExpr
    let targetId = case targetV of
          VStr s -> s
          _ -> error "send: actorId must be a Str"
    actorSend targetId msg
    return VUnit
  Receive clauses -> do
    msg <- actorReceive
    case matchClauses clauses msg of
      Nothing -> actorFail $ "receive: no matching pattern for " ++ show msg
      Just (binds, e) -> eval (Map.union binds env) e
  Spawn hint fnExpr argExprs -> do
    fnv <- eval env fnExpr
    args <- mapM (eval env) argExprs
    st <- getActorState
    case fnv of
      VFn _ closedEnv params body ->
        actorSpawn st closedEnv params body args hint
      _ -> actorFail "spawn: not a function"
  Self -> do
    st <- getActorState
    return (VAddr (actorAddr st))
  -- Note: 'self' is now injected as Var "self" into the environment at actor
  -- startup; the Self AST node is kept for backward compatibility with
  -- Haskell-constructed ASTs but is no longer emitted by the parser.
  TypeOf e -> typeOf <$> eval env e
  FnOf e -> fnNameOf <$> eval env e
  Tag t argExprs -> do
    args <- mapM (eval env) argExprs
    return (VTagged t args)
  Alloc e -> eval env e >>= allocValue
  Dealloc e -> eval env e >>= deallocRef >> return VUnit
  GetRef e -> eval env e >>= getRef
  -- Fix: bind name to a self-referential function in the env. The trick: build a VFn whose closed env contains a reference back to itself. We achieve this with a lazy knot: construct the env with the binding pointing to the VFn that closes over that very env.
  Fix fname params body ->
    let selfFn = VFn fname selfEnv params body
        selfEnv = Map.insert fname selfFn env
     in return selfFn
  -- IsoDecl: evaluate fwd and bkwd, register in the process iso map, return Unit.
  IsoDecl a b fwdExpr bkwdExpr -> do
    fwd <- eval env fwdExpr
    bkwd <- eval env bkwdExpr
    st <- getActorState
    liftIO $ isoRegister (actorIsoMap st) a b fwd bkwd
    return VUnit

  -- LookupIso: consult the iso map, return Just (Pair fwd bkwd) or Nothing.
  LookupIso a b -> do
    st <- getActorState
    mr <- liftIO $ isoLookup (actorIsoMap st) a b
    return $ case mr of
      Nothing -> VTagged "Nothing" []
      Just (fwd, bk) -> VTagged "Just" [VTagged "Pair" [fwd, bk]]

  -- Match: evaluate scrutinee then find first matching clause.
  Match scrutinee clauses -> do
    v <- eval env scrutinee
    case matchClauses clauses v of
      Nothing -> actorFail $ "match: no matching pattern for " ++ show v
      Just (binds, e) -> eval (Map.union binds env) e

applyFn :: Value -> [Value] -> ActorM Value
applyFn (VFn _ closedEnv params body) args
  -- concise way to combine args and params into a map so each param has an arg value, then combine args with external env, prioritizing args
  | length params == length args = eval (Map.union (Map.fromList (zip params args)) closedEnv) body
  | otherwise = actorFail $ "arity mismatch: expected " ++ show (length params) ++ " got " ++ show (length args)
applyFn (VPrim _ f) args = f args
applyFn v _ = actorFail $ "apply: not a function: " ++ show v

-- =============================================================================
-- STAGE 7 ── BOOTSTRAP
-- Wire up a fresh process (registry, iso map, main mailbox, heap) and run
-- a program expression in the context of a "main" actor.
-- =============================================================================

runProgram :: Env -> VFSMap -> Expr -> IO Value
runProgram baseEnv vfs prog = do
  reg <- newRegistry
  im <- newIsoMap
  mb <- atomically newMailbox
  heap <- newIORef emptyHeap
  fdTable <- newIORef Map.empty
  let addr = (0, "main")
      state = ActorState addr heap mb reg im vfs fdTable
      env0 = Map.insert "self" (VAddr addr) baseEnv
  registryRegister reg "main" mb
  runActorM (eval env0 prog) state

-- =============================================================================
-- STAGE 8 ── BUILT-IN PRIMITIVES
-- primEnv is the starting environment handed to every program.  Each entry
-- is a VPrim wrapping a plain Haskell function lifted into ActorM.
-- =============================================================================

primEnv :: Env
primEnv = Map.fromList
  [ ("println",    VPrim "println"    primPrintln)
  , ("intAdd",     VPrim "intAdd"     primIntAdd)
  , ("intSub",     VPrim "intSub"     primIntSub)
  , ("intMul",     VPrim "intMul"     primIntMul)
  , ("intDiv",     VPrim "intDiv"     primIntDiv)
  , ("intMod",     VPrim "intMod"     primIntMod)
  , ("intNeg",     VPrim "intNeg"     primIntNeg)
  , ("intAbs",     VPrim "intAbs"     primIntAbs)
  , ("intEq",      VPrim "intEq"      primIntEq)
  , ("intLt",      VPrim "intLt"      primIntLt)
  , ("intLe",      VPrim "intLe"      primIntLe)
  , ("intGt",      VPrim "intGt"      primIntGt)
  , ("intGe",      VPrim "intGe"      primIntGe)
  , ("boolNot",    VPrim "boolNot"    primBoolNot)
  , ("strConcat",  VPrim "strConcat"  primStrConcat)
  , ("strEq",      VPrim "strEq"      primStrEq)
  , ("typeEq",     VPrim "typeEq"     primTypeEq)
  , ("intToStr",   VPrim "intToStr"   primIntToStr)
  , ("strToInt",   VPrim "strToInt"   primStrToInt)
  , ("showVal",    VPrim "showVal"    primShowVal)
  , ("tagPayload", VPrim "tagPayload" primTagPayload)
  , ("withIso",    VPrim "withIso"    primWithIso)
  -- Fixed-point combinator: fix(f) = f(fn(args) => fix(f)(args))
  -- This is the Z combinator so it works under strict evaluation.
  , ("fix",        VPrim "fix"        primFix)
  -- List utilities
  , ("listHead",   VPrim "listHead"   primListHead)
  , ("listTail",   VPrim "listTail"   primListTail)
  , ("listLen",    VPrim "listLen"    primListLen)
  , ("listToCons", VPrim "listToCons" primListToCons)
  -- Virtual UNIX file API
  , ("fopen",      VPrim "fopen"      primFOpen)
  , ("fread",      VPrim "fread"      primFRead)
  , ("fwrite",     VPrim "fwrite"     primFWrite)
  , ("fclose",     VPrim "fclose"     primFClose)
  , ("fseek",      VPrim "fseek"      primFSeek)
  , ("true",       VBool True)
  , ("false",      VBool False)
  , ("unit",       VUnit)
  ]

primPrintln :: [Value] -> ActorM Value
primPrintln [v] = liftIO (putStrLn (show v)) >> return VUnit
primPrintln vs = liftIO (putStrLn (unwords (map show vs))) >> return VUnit

primIntAdd :: [Value] -> ActorM Value
primIntAdd [VInt a, VInt b] = return (VInt (a + b))
primIntAdd _ = actorFail "intAdd: type error"

primIntSub :: [Value] -> ActorM Value
primIntSub [VInt a, VInt b] = return (VInt (a - b))
primIntSub _ = actorFail "intSub: type error"

primIntMul :: [Value] -> ActorM Value
primIntMul [VInt a, VInt b] = return (VInt (a * b))
primIntMul _ = actorFail "intMul: type error"

primIntDiv :: [Value] -> ActorM Value
primIntDiv [VInt _, VInt 0] = actorFail "intDiv: division by zero"
primIntDiv [VInt a, VInt b] = return (VInt (a `div` b))
primIntDiv _ = actorFail "intDiv: type error"

primIntMod :: [Value] -> ActorM Value
primIntMod [VInt _, VInt 0] = actorFail "intMod: modulo by zero"
primIntMod [VInt a, VInt b] = return (VInt (a `mod` b))
primIntMod _ = actorFail "intMod: type error"

primIntNeg :: [Value] -> ActorM Value
primIntNeg [VInt a] = return (VInt (negate a))
primIntNeg _ = actorFail "intNeg: type error"

primIntAbs :: [Value] -> ActorM Value
primIntAbs [VInt a] = return (VInt (abs a))
primIntAbs _ = actorFail "intAbs: type error"

primIntLt :: [Value] -> ActorM Value
primIntLt [VInt a, VInt b] = return (VBool (a < b))
primIntLt _ = actorFail "intLt: type error"

primIntLe :: [Value] -> ActorM Value
primIntLe [VInt a, VInt b] = return (VBool (a <= b))
primIntLe _ = actorFail "intLe: type error"

primIntGt :: [Value] -> ActorM Value
primIntGt [VInt a, VInt b] = return (VBool (a > b))
primIntGt _ = actorFail "intGt: type error"

primIntGe :: [Value] -> ActorM Value
primIntGe [VInt a, VInt b] = return (VBool (a >= b))
primIntGe _ = actorFail "intGe: type error"

primBoolNot :: [Value] -> ActorM Value
primBoolNot [VBool b] = return (VBool (not b))
primBoolNot _ = actorFail "boolNot: type error"

primIntEq :: [Value] -> ActorM Value
primIntEq [VInt a, VInt b] = return (VBool (a == b))
primIntEq _ = actorFail "intEq: type error"

primStrConcat :: [Value] -> ActorM Value
primStrConcat [VStr a, VStr b] = return (VStr (a ++ b))
primStrConcat _ = actorFail "strConcat: type error"

primStrEq :: [Value] -> ActorM Value
primStrEq [VStr a, VStr b] = return (VBool (a == b))
primStrEq _ = actorFail "strEq: type error"

primTypeEq :: [Value] -> ActorM Value
primTypeEq [VType a, VType b] = return (VBool (a == b))
primTypeEq _ = actorFail "typeEq: type error"

primIntToStr :: [Value] -> ActorM Value
primIntToStr [VInt n] = return (VStr (show n))
primIntToStr _ = actorFail "intToStr: expected Int"

primStrToInt :: [Value] -> ActorM Value
primStrToInt [VStr s] = case reads s of
  [(n, "")] -> return (VInt n)
  _         -> actorFail $ "strToInt: cannot parse as integer: " ++ show s
primStrToInt _ = actorFail "strToInt: expected a String"

primShowVal :: [Value] -> ActorM Value
primShowVal [v] = return (VStr (show v))
primShowVal _ = actorFail "showVal: expected one argument"

-- Extract the first payload field from a VTagged value
primTagPayload :: [Value] -> ActorM Value
primTagPayload [VTagged _ (v : _)] = return v
primTagPayload [VTagged t []] = actorFail $ "tagPayload: " ++ t ++ " has no payload"
primTagPayload [v] = actorFail $ "tagPayload: not a tagged value: " ++ show v
primTagPayload _ = actorFail "tagPayload: expected one argument"

-- Apply iso: given Just (Pair fwd bkwd) and a value, show both directions
primWithIso :: [Value] -> ActorM Value
primWithIso [VTagged "Just" [VTagged "Pair" [fwd, bkwd]], val] = do
  forwarded <- applyFn fwd [val]
  restored <- applyFn bkwd [forwarded]
  liftIO $ do
    putStrLn $ "fwd applied: " ++ show forwarded
    putStrLn $ "bkwd applied: " ++ show restored
  return VUnit
primWithIso [VTagged "Nothing" [], _] = actorFail "withIso: iso not found"
primWithIso _ = actorFail "withIso: bad arguments"

-- Z combinator: fix(f) = f(fn(args...) => fix(f)(args...))
-- The thunk delays recursive unfolding until the self-reference is actually
-- called, which is necessary for strict (IO-based) evaluation.
primFix :: [Value] -> ActorM Value
primFix [f] = do
  let thunk = VPrim "fix-thunk" $ \args -> primFix [f] >>= \r -> applyFn r args
  applyFn f [thunk]
primFix _ = actorFail "fix: expected exactly one function argument"

-- =============================================================================
-- LIST PRIMITIVES
-- Operate on both VList (literal syntax [1,2,3]) and VTagged Cons/Nil chains.
-- listToCons converts a VList literal to a Cons/Nil chain for pattern matching.
-- =============================================================================

-- Convert VList to a Cons/Nil chain.  Cons/Nil values pass through unchanged.
primListToCons :: [Value] -> ActorM Value
primListToCons [VList vs] = return (toCons vs)
  where
    toCons []       = VTagged "Nil" []
    toCons (v : rest) = VTagged "Cons" [v, toCons rest]
primListToCons [v@(VTagged "Nil" [])]  = return v
primListToCons [v@(VTagged "Cons" _)]  = return v
primListToCons _ = actorFail "listToCons: expected a List"

primListHead :: [Value] -> ActorM Value
primListHead [VList (v : _)]            = return v
primListHead [VTagged "Cons" (v : _)]   = return v
primListHead [VList []]                 = actorFail "listHead: empty list"
primListHead [VTagged "Nil" _]          = actorFail "listHead: empty list"
primListHead _ = actorFail "listHead: expected a List"

primListTail :: [Value] -> ActorM Value
primListTail [VList (_ : vs)]           = return (VList vs)
primListTail [VTagged "Cons" [_, t]]    = return t
primListTail [VList []]                 = actorFail "listTail: empty list"
primListTail [VTagged "Nil" _]          = actorFail "listTail: empty list"
primListTail _ = actorFail "listTail: expected a List"

primListLen :: [Value] -> ActorM Value
primListLen [VList vs]  = return (VInt (length vs))
primListLen [v]         = return (VInt (conLen v))
  where
    conLen (VTagged "Nil" [])       = 0
    conLen (VTagged "Cons" [_, t])  = 1 + conLen t
    conLen _                        = 0
primListLen _ = actorFail "listLen: expected a List"

-- =============================================================================
-- VIRTUAL UNIX FILE API PRIMITIVES
-- fopen / fread / fwrite / fclose / fseek
-- Each primitive looks up the path (or fd) in the ActorState and delegates
-- to the Haskell handler bundle registered in actorVFS / actorFDTable.
-- =============================================================================

-- Helper: pick next available fd (≥3, reserving 0/1/2 for stdio)
nextFD :: FDTable -> Int
nextFD t
  | Map.null t = 3
  | otherwise  = fst (Map.findMax t) + 1

primFOpen :: [Value] -> ActorM Value
primFOpen (VStr path : extraArgs) = ActorM $ \st -> do
  case Map.lookup path (actorVFS st) of
    Nothing -> error $ "fopen: no virtual file registered at: " ++ show path
    Just handlers -> do
      initState <- runActorM (vhOpen handlers extraArgs) st
      stRef     <- newIORef initState
      fdTable   <- readIORef (actorFDTable st)
      let fd    = nextFD fdTable
          entry = FDEntry handlers stRef
      writeIORef (actorFDTable st) (Map.insert fd entry fdTable)
      return (VInt fd)
primFOpen _ = actorFail "fopen: expected a string path as first argument"

primFRead :: [Value] -> ActorM Value
primFRead (VInt fd : extraArgs) = ActorM $ \st -> do
  fdTable <- readIORef (actorFDTable st)
  case Map.lookup fd fdTable of
    Nothing    -> error $ "fread: unknown file descriptor: " ++ show fd
    Just entry -> do
      state <- readIORef (fdeState entry)
      (result, newState) <- runActorM (vhRead (fdeHandlers entry) state extraArgs) st
      writeIORef (fdeState entry) newState
      return result
primFRead _ = actorFail "fread: expected an Int file descriptor as first argument"

primFWrite :: [Value] -> ActorM Value
primFWrite (VInt fd : extraArgs) = ActorM $ \st -> do
  fdTable <- readIORef (actorFDTable st)
  case Map.lookup fd fdTable of
    Nothing    -> error $ "fwrite: unknown file descriptor: " ++ show fd
    Just entry -> do
      state    <- readIORef (fdeState entry)
      newState <- runActorM (vhWrite (fdeHandlers entry) state extraArgs) st
      writeIORef (fdeState entry) newState
      return VUnit
primFWrite _ = actorFail "fwrite: expected an Int file descriptor as first argument"

primFClose :: [Value] -> ActorM Value
primFClose [VInt fd] = ActorM $ \st -> do
  fdTable <- readIORef (actorFDTable st)
  case Map.lookup fd fdTable of
    Nothing    -> error $ "fclose: unknown file descriptor: " ++ show fd
    Just entry -> do
      state <- readIORef (fdeState entry)
      runActorM (vhClose (fdeHandlers entry) state) st
      writeIORef (actorFDTable st) (Map.delete fd fdTable)
      return VUnit
primFClose _ = actorFail "fclose: expected exactly one Int file descriptor"

primFSeek :: [Value] -> ActorM Value
primFSeek (VInt fd : extraArgs) = ActorM $ \st -> do
  fdTable <- readIORef (actorFDTable st)
  case Map.lookup fd fdTable of
    Nothing    -> error $ "fseek: unknown file descriptor: " ++ show fd
    Just entry -> do
      state    <- readIORef (fdeState entry)
      newState <- runActorM (vhSeek (fdeHandlers entry) state extraArgs) st
      writeIORef (fdeState entry) newState
      return VUnit
primFSeek _ = actorFail "fseek: expected an Int file descriptor as first argument"

-- =============================================================================
-- EXAMPLES  (inline test programs; main entry point is in Main.hs)
-- =============================================================================

-- ── Example 1: type/1 reflection ────────────────────────────────────────────
-- let x = Num 5
-- let t = type x        -- VType "Num"
-- println t
-- typeEq t (type 42)    -- False  (Num vs Int)
-- typeEq (type 42) (type 99)  -- True

example1 =
  Seq
    [ Let "x" (Lit (VTagged "Num" [VInt 5])) $
        Let "t" (TypeOf (Var "x")) $
          Seq
            [ App (Var "println") [Var "t"],
              Let "isNum" (App (Var "typeEq") [Var "t", TypeOf (Lit (VInt 42))]) $
                App (Var "println") [Var "isNum"], -- False
              App (Var "typeEq") [TypeOf (Lit (VInt 42)), TypeOf (Lit (VInt 99))]
            ]
    ]

-- ── Example 2: RC allocator ──────────────────────────────────────────────────
-- ref = alloc (Num 42)
-- println (getref ref)
-- dealloc ref

example2 =
  Seq
    [ Let "ref" (Alloc (Lit (VTagged "Num" [VInt 42]))) $
        Seq
          [ App (Var "println") [GetRef (Var "ref")],
            Dealloc (Var "ref"),
            App (Var "println") [Lit (VStr "deallocated")]
          ]
    ]

-- ── Example 3: two actors communicating ─────────────────────────────────────
-- Spawns a "worker" actor that waits for a message, doubles it, sends back.
-- Main sends it a number and receives the result.

example3 =
  Let "workerAddr" (Spawn "worker" (Lam [] workerBody) []) $
    Seq
      [ Send
          (Lit VUnit)
          (Lit (VStr "worker"))
          (Tag "Task" [Lit (VInt 21), Lit (VStr "main")]),
        Receive
          [ ( PTagged "Result" [PVar "r"],
              App
                (Var "println")
                [ App
                    (Var "strConcat")
                    [Lit (VStr "main received: "), App (Var "intToStr") [Var "r"]]
                ]
            )
          ]
      ]
  where
    workerBody =
      Receive
        [ ( PTagged "Task" [PVar "n", PVar "replyTo"],
            Let "doubled" (App (Var "intMul") [Var "n", Lit (VInt 2)]) $
              Seq
                [ App
                    (Var "println")
                    [ App
                        (Var "strConcat")
                        [ Lit (VStr "worker doubling "),
                          App (Var "intToStr") [Var "n"]
                        ]
                    ],
                  Send (Lit VUnit) (Var "replyTo") (Tag "Result" [Var "doubled"])
                ]
          )
        ]

-- ── Example 4: function/1 reflection ────────────────────────────────────────
-- let f = \x -> intAdd x 1
-- println (function f)    -- "<lambda>"
-- println (function println)  -- "println"

example4 =
  Let "f" (Lam ["x"] (App (Var "intAdd") [Var "x", Lit (VInt 1)])) $
    Seq
      [ App (Var "println") [FnOf (Var "f")],
        App (Var "println") [FnOf (Var "println")]
      ]

-- ── Example 5: fix combinator — recursive factorial ──────────────────────────
-- fact = fix fact (n) = if n == 0 then 1 else n * fact (n - 1)
-- println (fact 6)   →  720

example5 =
  Let
    "fact"
    ( Fix
        "fact"
        ["n"]
        ( If
            (App (Var "intEq") [Var "n", Lit (VInt 0)])
            (Lit (VInt 1))
            ( App
                (Var "intMul")
                [ Var "n",
                  App (Var "fact") [App (Var "intSub") [Var "n", Lit (VInt 1)]]
                ]
            )
        )
    )
    $ Seq
      [ App
          (Var "println")
          [ App
              (Var "strConcat")
              [Lit (VStr "fact 6 = "), App (Var "intToStr") [App (Var "fact") [Lit (VInt 6)]]]
          ],
        App
          (Var "println")
          [ App
              (Var "strConcat")
              [Lit (VStr "fact 0 = "), App (Var "intToStr") [App (Var "fact") [Lit (VInt 0)]]]
          ]
      ]

-- ── Example 6: iso registry ──────────────────────────────────────────────────
-- MyList wraps a List.  We declare an iso between them, then look it up
-- and use both directions.
--
-- iso MyList List
--   fwd = \(MyList xs) -> xs          -- MyList -> List (unwrap)
--   bkwd = \xs -> MyList xs           -- List -> MyList (wrap)
--
-- Just (Pair fwd bkwd) = iso MyList List
-- println (fwd (MyList [1,2,3]))   → [1,2,3]  (as VList)
-- println (bkwd [1,2,3])           → (MyList [1,2,3])

example6 =
  -- Helper defined first so the iso closure can capture it
  Let
    "unwrapMyList"
    ( Fix
        "unwrapMyList"
        ["v"]
        ( If
            (App (Var "typeEq") [TypeOf (Var "v"), Lit (VType "MyList")])
            (App (Var "tagPayload") [Var "v"])
            (App (Var "showVal") [Var "v"])
        )
    )
    -- Register the iso (fwd/bkwd close over unwrapMyList in env)
    $ Let
      "_"
      ( IsoDecl
          "MyList"
          "List"
          (Lam ["v"] (App (Var "unwrapMyList") [Var "v"]))
          (Lam ["xs"] (Tag "MyList" [Var "xs"]))
      )
    -- Look it up and use it
    $ Let "isoResult" (LookupIso "MyList" "List")
    $ Let "myVal" (Tag "MyList" [Lit (VList [VInt 1, VInt 2, VInt 3])])
    $ Seq
      [ App
          (Var "println")
          [ App
              (Var "strConcat")
              [Lit (VStr "original:   "), App (Var "showVal") [Var "myVal"]]
          ],
        App (Var "withIso") [Var "isoResult", Var "myVal"],
        -- Also show Nothing case for an unregistered iso
        Let "missing" (LookupIso "Foo" "Bar") $
          App
            (Var "println")
            [ App
                (Var "strConcat")
                [Lit (VStr "iso Foo Bar: "), App (Var "showVal") [Var "missing"]]
            ]
      ]
