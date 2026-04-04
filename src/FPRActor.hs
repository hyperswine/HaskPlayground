{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module FPRActor where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad          (forM_, void, when)
import           Data.IORef
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromMaybe)
import           System.IO.Unsafe       (unsafePerformIO)

-- ─────────────────────────────────────────────────────────────────────────────
-- VALUES
-- Every value carries a constructor tag so type/1 is always available.
-- ─────────────────────────────────────────────────────────────────────────────

data Value
  = VInt    Int
  | VBool   Bool
  | VStr    String
  | VUnit
  -- Tagged constructor: name + payload  (covers user-defined types)
  | VTagged String [Value]
  -- First-class function (name for function/1, closure env, arg names, body)
  | VFn     String Env [String] Expr
  -- Built-in primitive
  | VPrim   String ([Value] -> ActorM Value)
  -- Actor address  (processId, actorId)
  | VAddr   ActorAddr
  -- A list (sugar over VTagged really, kept separate for convenience)
  | VList   [Value]
  -- The metatype returned by type/1  e.g. VType "Int"
  | VType   String

instance Show Value where
  show (VInt n)        = show n
  show (VBool b)       = show b
  show (VStr s)        = show s
  show VUnit           = "()"
  show (VTagged t [])  = t
  show (VTagged t vs)  = "(" ++ t ++ " " ++ unwords (map show vs) ++ ")"
  show (VFn n _ _ _)   = "<fn:" ++ n ++ ">"
  show (VPrim n _)     = "<prim:" ++ n ++ ">"
  show (VAddr a)       = "<actor:" ++ show a ++ ">"
  show (VList vs)      = "[" ++ commas vs ++ "]"
  show (VType t)       = "Type:" ++ t

commas :: Show a => [a] -> String
commas = foldr (\x acc -> show x ++ if null acc then "" else ", " ++ acc) ""

-- ─────────────────────────────────────────────────────────────────────────────
-- EXPRESSIONS  (the AST)
-- ─────────────────────────────────────────────────────────────────────────────

data Expr
  = Lit     Value
  | Var     String
  | App     Expr [Expr]              -- f args
  | Lam     [String] Expr            -- \x y -> body
  | Let     String Expr Expr         -- let x = e in body
  | If      Expr Expr Expr
  | Seq     [Expr]                   -- sequential block, last value returned
  | Send    Expr Expr Expr           -- send addr actorId msg
  | Receive [(Pattern, Expr)]        -- receive { pat -> expr, ... }
  | Spawn   String Expr [Expr]         -- spawn "name" fn initArgs
  | Self                             -- own ActorAddr
  | TypeOf  Expr                     -- type/1
  | FnOf    Expr                     -- function/1  (name of a VFn)
  | Tag     String [Expr]             -- Tag "Foo" [e1, e2]  → VTagged
  | Alloc   Expr                     -- alloc v  (adds to actor RC heap)
  | Dealloc Expr                     -- dealloc ref
  | GetRef  Expr                     -- deref a heap ref
  deriving (Show)

data Pattern
  = PVar    String                   -- binds anything
  | PTagged String [Pattern]         -- constructor match
  | PLit    Value                    -- literal match
  | PWild                            -- _
  deriving (Show)

-- ─────────────────────────────────────────────────────────────────────────────
-- ENVIRONMENT
-- ─────────────────────────────────────────────────────────────────────────────

type Env = Map String Value

envLookup :: String -> Env -> Value
envLookup k e = fromMaybe (error $ "Unbound: " ++ k) (Map.lookup k e)

envExtend :: String -> Value -> Env -> Env
envExtend = Map.insert

-- ─────────────────────────────────────────────────────────────────────────────
-- ACTOR IDENTITY
-- ─────────────────────────────────────────────────────────────────────────────

type ProcessId = Int
type ActorId   = String
type ActorAddr = (ProcessId, ActorId)

-- ─────────────────────────────────────────────────────────────────────────────
-- RC ALLOCATOR
-- Each actor has its own heap: a map from HeapRef to (Value, refcount)
-- ─────────────────────────────────────────────────────────────────────────────

type HeapRef = Int

data Heap = Heap
  { heapCells   :: Map HeapRef (Value, Int)   -- value + refcount
  , heapNextRef :: HeapRef
  }

emptyHeap :: Heap
emptyHeap = Heap Map.empty 0

heapAlloc :: Value -> Heap -> (HeapRef, Heap)
heapAlloc v h =
  let ref = heapNextRef h
  in  (ref, h { heapCells   = Map.insert ref (v, 1) (heapCells h)
              , heapNextRef = ref + 1 })

heapDealloc :: HeapRef -> Heap -> Heap
heapDealloc ref h =
  case Map.lookup ref (heapCells h) of
    Nothing      -> error $ "heapDealloc: invalid ref " ++ show ref
    Just (_, 1)  -> h { heapCells = Map.delete ref (heapCells h) }
    Just (v, rc) -> h { heapCells = Map.insert ref (v, rc - 1) (heapCells h) }

heapGet :: HeapRef -> Heap -> Value
heapGet ref h =
  case Map.lookup ref (heapCells h) of
    Nothing     -> error $ "heapGet: invalid ref " ++ show ref
    Just (v, _) -> v

-- ─────────────────────────────────────────────────────────────────────────────
-- MAILBOX  (bounded, crashes on overflow)
-- ─────────────────────────────────────────────────────────────────────────────

mailboxCapacity :: Int
mailboxCapacity = 64

data Mailbox = Mailbox
  { mbQueue :: TQueue Value
  , mbSize  :: TVar Int
  }

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
      modifyTVar' (mbSize mb) (+1)
      return True

mailboxPop :: Mailbox -> STM Value
mailboxPop mb = do
  v <- readTQueue (mbQueue mb)
  modifyTVar' (mbSize mb) (subtract 1)
  return v

-- ─────────────────────────────────────────────────────────────────────────────
-- ACTOR STATE  (mutable, owned by one thread)
-- ─────────────────────────────────────────────────────────────────────────────

data ActorState = ActorState
  { actorAddr    :: ActorAddr
  , actorHeap    :: IORef Heap
  , actorMailbox :: Mailbox
  , actorRegistry :: Registry    -- shared with all actors in process
  }

-- ─────────────────────────────────────────────────────────────────────────────
-- REGISTRY  (process-global, maps ActorId to Mailbox)
-- ─────────────────────────────────────────────────────────────────────────────

type Registry = TVar (Map ActorId Mailbox)

newRegistry :: IO Registry
newRegistry = newTVarIO Map.empty

registryRegister :: Registry -> ActorId -> Mailbox -> IO ()
registryRegister reg aid mb =
  atomically $ modifyTVar' reg (Map.insert aid mb)

registryLookup :: Registry -> ActorId -> IO (Maybe Mailbox)
registryLookup reg aid = Map.lookup aid <$> readTVarIO reg

-- ─────────────────────────────────────────────────────────────────────────────
-- ACTORM MONAD  (IO with actor state threaded)
-- ─────────────────────────────────────────────────────────────────────────────

newtype ActorM a = ActorM { runActorM :: ActorState -> IO a }

instance Functor ActorM where
  fmap f (ActorM g) = ActorM (fmap f . g)

instance Applicative ActorM where
  pure x = ActorM (const (pure x))
  ActorM f <*> ActorM x = ActorM $ \s -> f s <*> x s

instance Monad ActorM where
  return = pure
  ActorM x >>= f = ActorM $ \s -> x s >>= \a -> runActorM (f a) s

liftIO :: IO a -> ActorM a
liftIO io = ActorM (const io)

getActorState :: ActorM ActorState
getActorState = ActorM return

actorFail :: String -> ActorM a
actorFail msg = ActorM $ \st ->
  error $ "[Actor " ++ show (actorAddr st) ++ "] " ++ msg

-- ─────────────────────────────────────────────────────────────────────────────
-- HEAP OPERATIONS IN ACTORM
-- ─────────────────────────────────────────────────────────────────────────────

allocValue :: Value -> ActorM Value
allocValue v = ActorM $ \st -> do
  heap <- readIORef (actorHeap st)
  let (ref, heap') = heapAlloc v heap
  writeIORef (actorHeap st) heap'
  return (VTagged "Ref" [VInt ref])

deallocRef :: Value -> ActorM ()
deallocRef (VTagged "Ref" [VInt ref]) = ActorM $ \st -> do
  modifyIORef' (actorHeap st) (heapDealloc ref)
deallocRef v = actorFail $ "dealloc: not a Ref: " ++ show v

getRef :: Value -> ActorM Value
getRef (VTagged "Ref" [VInt ref]) = ActorM $ \st -> do
  heap <- readIORef (actorHeap st)
  return (heapGet ref heap)
getRef v = actorFail $ "getRef: not a Ref: " ++ show v

-- ─────────────────────────────────────────────────────────────────────────────
-- SEND / RECEIVE / SPAWN
-- ─────────────────────────────────────────────────────────────────────────────

actorSend :: ActorId -> Value -> ActorM ()
actorSend targetId msg = ActorM $ \st -> do
  mMb <- registryLookup (actorRegistry st) targetId
  case mMb of
    Nothing -> error $ "send: unknown actor: " ++ targetId
    Just mb -> do
      ok <- atomically (mailboxPush mb msg)
      when (not ok) $
        error $ "send: mailbox overflow for actor: " ++ targetId

actorReceive :: ActorM Value
actorReceive = ActorM $ \st ->
  atomically (mailboxPop (actorMailbox st))

actorSpawn :: ActorState -> Env -> [String] -> Expr -> [Value] -> String -> ActorM Value
actorSpawn parentState env params body initArgs requestedId = ActorM $ \_ -> do
  let reg = actorRegistry parentState
  actualId <- freshActorId reg requestedId
  mb  <- atomically newMailbox
  ref <- newIORef emptyHeap
  let childAddr  = (fst (actorAddr parentState), actualId)
      childState = ActorState childAddr ref mb reg
      childEnv   = foldr (\(p,v) e -> envExtend p v e) env (zip params initArgs)
  registryRegister reg actualId mb
  void $ forkIO $
    void $ runActorM (eval childEnv body) childState
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
      n <- atomicModifyIORef' actorCounter (\n -> (n+1, n))
      let candidate = base ++ "-" ++ show n
      if candidate `Map.notMember` m
        then return candidate
        else freshActorId reg base  -- retry on collision

-- ─────────────────────────────────────────────────────────────────────────────
-- type/1  and  function/1
-- ─────────────────────────────────────────────────────────────────────────────

typeOf :: Value -> Value
typeOf (VInt _)       = VType "Int"
typeOf (VBool _)      = VType "Bool"
typeOf (VStr _)       = VType "Str"
typeOf VUnit          = VType "Unit"
typeOf (VTagged t _)  = VType t
typeOf (VFn _ _ _ _)  = VType "Fn"
typeOf (VPrim _ _)    = VType "Fn"
typeOf (VAddr _)      = VType "ActorAddr"
typeOf (VList _)      = VType "List"
typeOf (VType _)      = VType "Type"

fnNameOf :: Value -> Value
fnNameOf (VFn n _ _ _) = VStr n
fnNameOf (VPrim n _)   = VStr n
fnNameOf v             = error $ "function/1: not a function: " ++ show v

-- ─────────────────────────────────────────────────────────────────────────────
-- PATTERN MATCHING
-- ─────────────────────────────────────────────────────────────────────────────

matchPattern :: Pattern -> Value -> Maybe Env
matchPattern PWild          _                          = Just Map.empty
matchPattern (PVar x)       v                          = Just (Map.singleton x v)
matchPattern (PLit l)       v | showVal l == showVal v = Just Map.empty
                              | otherwise              = Nothing
matchPattern (PTagged t ps) (VTagged t' vs)
  | t == t' && length ps == length vs =
      foldl (\acc (p,v) -> acc >>= \e -> fmap (Map.union e) (matchPattern p v))
            (Just Map.empty) (zip ps vs)
matchPattern _ _ = Nothing

showVal :: Value -> String
showVal = show

matchClauses :: [(Pattern, Expr)] -> Value -> Maybe (Env, Expr)
matchClauses [] _ = Nothing
matchClauses ((p,e):rest) v =
  case matchPattern p v of
    Just binds -> Just (binds, e)
    Nothing    -> matchClauses rest v

-- ─────────────────────────────────────────────────────────────────────────────
-- EVALUATOR
-- ─────────────────────────────────────────────────────────────────────────────

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
      VBool True  -> eval env t
      VBool False -> eval env f
      _           -> actorFail "if: condition not a Bool"

  Seq []     -> return VUnit
  Seq [e]    -> eval env e
  Seq (e:es) -> eval env e >> eval env (Seq es)

  App f argExprs -> do
    fv   <- eval env f
    args <- mapM (eval env) argExprs
    applyFn fv args

  Send addrExpr actorIdExpr msgExpr -> do
    _addr    <- eval env addrExpr     -- for inter-process this would carry pid
    targetV  <- eval env actorIdExpr
    msg      <- eval env msgExpr
    let targetId = case targetV of
                     VStr s -> s
                     _      -> error "send: actorId must be a Str"
    actorSend targetId msg
    return VUnit

  Receive clauses -> do
    msg <- actorReceive
    case matchClauses clauses msg of
      Nothing          -> actorFail $ "receive: no matching pattern for " ++ show msg
      Just (binds, e)  -> eval (Map.union binds env) e

  Spawn hint fnExpr argExprs -> do
    fnv  <- eval env fnExpr
    args <- mapM (eval env) argExprs
    st   <- getActorState
    case fnv of
      VFn _ closedEnv params body ->
        actorSpawn st closedEnv params body args hint
      _ -> actorFail "spawn: not a function"

  Self -> do
    st <- getActorState
    return (VAddr (actorAddr st))

  TypeOf e   -> typeOf  <$> eval env e
  FnOf   e   -> fnNameOf <$> eval env e

  Tag t argExprs -> do
    args <- mapM (eval env) argExprs
    return (VTagged t args)

  Alloc   e  -> eval env e >>= allocValue
  Dealloc e  -> eval env e >>= deallocRef >> return VUnit
  GetRef  e  -> eval env e >>= getRef

applyFn :: Value -> [Value] -> ActorM Value
applyFn (VFn _ closedEnv params body) args
  | length params == length args =
      eval (Map.union (Map.fromList (zip params args)) closedEnv) body
  | otherwise = actorFail $
      "arity mismatch: expected " ++ show (length params) ++
      " got " ++ show (length args)
applyFn (VPrim _ f) args = f args
applyFn v _ = actorFail $ "apply: not a function: " ++ show v

-- ─────────────────────────────────────────────────────────────────────────────
-- BOOTSTRAP  — create the main actor and run a program
-- ─────────────────────────────────────────────────────────────────────────────

runProgram :: Env -> Expr -> IO Value
runProgram baseEnv prog = do
  reg  <- newRegistry
  mb   <- atomically newMailbox
  heap <- newIORef emptyHeap
  let addr  = (0, "main")
      state = ActorState addr heap mb reg
  registryRegister reg "main" mb
  runActorM (eval baseEnv prog) state

-- ─────────────────────────────────────────────────────────────────────────────
-- PRIMITIVES  (small std-like set)
-- ─────────────────────────────────────────────────────────────────────────────

primEnv :: Env
primEnv = Map.fromList
  [ ("println",   VPrim "println"  primPrintln)
  , ("intAdd",    VPrim "intAdd"   primIntAdd)
  , ("intSub",    VPrim "intSub"   primIntSub)
  , ("intMul",    VPrim "intMul"   primIntMul)
  , ("intEq",     VPrim "intEq"    primIntEq)
  , ("strConcat", VPrim "strConcat" primStrConcat)
  , ("typeEq",    VPrim "typeEq"   primTypeEq)
  , ("intToStr",  VPrim "intToStr"  primIntToStr)
  , ("showVal",   VPrim "showVal"   primShowVal)
  , ("true",      VBool True)
  , ("false",     VBool False)
  , ("unit",      VUnit)
  ]

primPrintln :: [Value] -> ActorM Value
primPrintln [v] = liftIO (putStrLn (show v)) >> return VUnit
primPrintln vs  = liftIO (putStrLn (unwords (map show vs))) >> return VUnit

primIntAdd :: [Value] -> ActorM Value
primIntAdd [VInt a, VInt b] = return (VInt (a + b))
primIntAdd _ = actorFail "intAdd: type error"

primIntSub :: [Value] -> ActorM Value
primIntSub [VInt a, VInt b] = return (VInt (a - b))
primIntSub _ = actorFail "intSub: type error"

primIntMul :: [Value] -> ActorM Value
primIntMul [VInt a, VInt b] = return (VInt (a * b))
primIntMul _ = actorFail "intMul: type error"

primIntEq :: [Value] -> ActorM Value
primIntEq [VInt a, VInt b] = return (VBool (a == b))
primIntEq _ = actorFail "intEq: type error"

primStrConcat :: [Value] -> ActorM Value
primStrConcat [VStr a, VStr b] = return (VStr (a ++ b))
primStrConcat _ = actorFail "strConcat: type error"

primTypeEq :: [Value] -> ActorM Value
primTypeEq [VType a, VType b] = return (VBool (a == b))
primTypeEq _ = actorFail "typeEq: type error"

primIntToStr :: [Value] -> ActorM Value
primIntToStr [VInt n] = return (VStr (show n))
primIntToStr _ = actorFail "intToStr: expected Int"

primShowVal :: [Value] -> ActorM Value
primShowVal [v] = return (VStr (show v))
primShowVal _   = actorFail "showVal: expected one argument"

-- ─────────────────────────────────────────────────────────────────────────────
-- EXAMPLE PROGRAMS
-- ─────────────────────────────────────────────────────────────────────────────

-- ── Example 1: type/1 reflection ────────────────────────────────────────────
-- let x = Num 5
-- let t = type x        -- VType "Num"
-- println t
-- typeEq t (type 42)    -- False  (Num vs Int)
-- typeEq (type 42) (type 99)  -- True

example1 :: Expr
example1 = Seq
  [ Let "x"  (Lit (VTagged "Num" [VInt 5]))
  $ Let "t"  (TypeOf (Var "x"))
  $ Seq
    [ App (Var "println") [Var "t"]
    , Let "isNum" (App (Var "typeEq") [Var "t", TypeOf (Lit (VInt 42))])
      $ App (Var "println") [Var "isNum"]                        -- False
    , App (Var "typeEq") [TypeOf (Lit (VInt 42)), TypeOf (Lit (VInt 99))]
    ]
  ]

-- ── Example 2: RC allocator ──────────────────────────────────────────────────
-- ref = alloc (Num 42)
-- println (getref ref)
-- dealloc ref

example2 :: Expr
example2 = Seq
  [ Let "ref" (Alloc (Lit (VTagged "Num" [VInt 42])))
  $ Seq
    [ App (Var "println") [GetRef (Var "ref")]
    , Dealloc (Var "ref")
    , App (Var "println") [Lit (VStr "deallocated")]
    ]
  ]

-- ── Example 3: two actors communicating ─────────────────────────────────────
-- Spawns a "worker" actor that waits for a message, doubles it, sends back.
-- Main sends it a number and receives the result.

example3 :: Expr
example3 =
  Let "workerAddr" (Spawn "worker" (Lam [] workerBody) [])
  $ Seq
    [ Send (Lit VUnit) (Lit (VStr "worker"))
        (Tag "Task" [Lit (VInt 21), Lit (VStr "main")])
    , Receive
        [ ( PTagged "Result" [PVar "r"]
          , App (Var "println")
              [App (Var "strConcat")
                [Lit (VStr "main received: "), App (Var "intToStr") [Var "r"]]]
          )
        ]
    ]
  where
    workerBody =
      Receive
        [ ( PTagged "Task" [PVar "n", PVar "replyTo"]
          , Let "doubled" (App (Var "intMul") [Var "n", Lit (VInt 2)])
            $ Seq
              [ App (Var "println")
                  [App (Var "strConcat")
                    [ Lit (VStr "worker doubling ")
                    , App (Var "intToStr") [Var "n"]
                    ]]
              , Send (Lit VUnit) (Var "replyTo") (Tag "Result" [Var "doubled"])
              ]
          )
        ]

-- ── Example 4: function/1 reflection ────────────────────────────────────────
-- let f = \x -> intAdd x 1
-- println (function f)    -- "<lambda>"
-- println (function println)  -- "println"

example4 :: Expr
example4 =
  Let "f" (Lam ["x"] (App (Var "intAdd") [Var "x", Lit (VInt 1)]))
  $ Seq
    [ App (Var "println") [FnOf (Var "f")]
    , App (Var "println") [FnOf (Var "println")]
    ]

-- ─────────────────────────────────────────────────────────────────────────────
-- MAIN
-- ─────────────────────────────────────────────────────────────────────────────

main :: IO ()
main = do
  putStrLn "── Example 1: type/1 reflection ──"
  _ <- runProgram primEnv example1

  putStrLn "\n── Example 2: RC allocator ──"
  _ <- runProgram primEnv example2

  putStrLn "\n── Example 3: actor send/receive ──"
  _ <- runProgram primEnv example3
  threadDelay 100000   -- let worker thread finish printing

  putStrLn "\n── Example 4: function/1 reflection ──"
  _ <- runProgram primEnv example4

  return ()
