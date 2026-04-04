{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FPActorProgram where

import Control.Monad (forM_, when, unless)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

-- ---------------------------------------------------------------------------
-- AST
-- ---------------------------------------------------------------------------

type Name = String

data Expr
  = Lit    Value                      -- literal value (including VType)
  | Var    Name                       -- variable lookup
  | App    Expr [Expr]                -- function application
  | Lam    [Name] Expr                -- lambda
  | Let    Name Expr Expr             -- let binding
  | If     Expr Expr Expr             -- conditional
  | Seq    [Expr]                     -- sequencing; yields last value
  | Send   Expr Expr                  -- send msg to actor mailbox ref
  | Receive (Map Name ([Name], Expr)) -- pattern-match on next mailbox msg
  | Alloc  Expr                       -- allocate a value on actor heap, return Ref
  | Deref  Expr                       -- dereference a Ref
  | Assign Expr Expr                  -- assign to a Ref
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Values
-- ---------------------------------------------------------------------------

-- Every value carries its constructor tag so type/1 is a trivial projection.
data Value
  = VInt    Int
  | VBool   Bool
  | VStr    String
  | VUnit
  -- Tagged constructor: name + fields  (covers user-defined data)
  | VTagged Name [Value]
  -- Type-level value: the name of a constructor (Type 0)
  | VType   Name
  -- Function-level value: the name of a function (Type 1)
  | VFn     Name Fn
  -- Heap reference (index into actor heap)
  | VRef    Int
  -- Tuple
  | VTuple  [Value]

-- Fn is the actual callable
type Fn = [Value] -> Eval Value

instance Show Value where
  show (VInt n)       = show n
  show (VBool b)      = if b then "True" else "False"
  show (VStr s)       = show s
  show VUnit          = "()"
  show (VTagged n []) = n
  show (VTagged n vs) = "(" ++ n ++ " " ++ unwords (map show vs) ++ ")"
  show (VType n)      = "#type:" ++ n
  show (VFn n _)      = "#fn:" ++ n
  show (VRef i)       = "#ref:" ++ show i
  show (VTuple vs)    = "(" ++ intercalate ", " (map show vs) ++ ")"

-- Constructor tag — the thing type/1 returns
tagOf :: Value -> Name
tagOf (VInt _)      = "Int"
tagOf (VBool _)     = "Bool"
tagOf (VStr _)      = "Str"
tagOf VUnit         = "Unit"
tagOf (VTagged n _) = n
tagOf (VType _)     = "Type"
tagOf (VFn _ _)     = "Fn"
tagOf (VRef _)      = "Ref"
tagOf (VTuple _)    = "Tuple"

-- ---------------------------------------------------------------------------
-- Actor state
-- ---------------------------------------------------------------------------

data HeapCell = HeapCell
  { hcValue :: Value
  , hcRC    :: Int          -- reference count
  } deriving (Show)

type Heap    = Map Int HeapCell
type Mailbox = [Value]

data ActorState = ActorState
  { heap        :: Heap
  , nextRef     :: Int
  , mailbox     :: Mailbox
  , mailboxMax  :: Int      -- crash if exceeded
  , actorLog    :: [String] -- trace of send/receive for demo
  } deriving (Show)

initialActorState :: Int -> ActorState
initialActorState mbMax = ActorState
  { heap       = Map.empty
  , nextRef    = 0
  , mailbox    = []
  , mailboxMax = mbMax
  , actorLog   = []
  }

-- ---------------------------------------------------------------------------
-- Eval monad
-- ReaderT Env (StateT ActorState (ExceptT String IO))
-- ---------------------------------------------------------------------------

type Env  = Map Name Value

newtype Eval a = Eval
  { runEval :: ReaderT Env (StateT ActorState (ExceptT String IO)) a }
  deriving ( Functor, Applicative, Monad
           , MonadReader Env
           , MonadState  ActorState
           , MonadError  String
           , MonadIO
           )

evalWith :: Env -> ActorState -> Eval a -> IO (Either String (a, ActorState))
evalWith env st (Eval m) = runExceptT (runStateT (runReaderT m env) st)

-- ---------------------------------------------------------------------------
-- Environment helpers
-- ---------------------------------------------------------------------------

lookupVar :: Name -> Eval Value
lookupVar n = do
  env <- ask
  case Map.lookup n env of
    Just v  -> pure v
    Nothing -> throwError $ "Unbound variable: " ++ n

withBinding :: Name -> Value -> Eval a -> Eval a
withBinding n v = local (Map.insert n v)

withBindings :: [(Name, Value)] -> Eval a -> Eval a
withBindings bs m = foldr (uncurry withBinding) m bs

-- ---------------------------------------------------------------------------
-- Heap / RC operations
-- ---------------------------------------------------------------------------

heapAlloc :: Value -> Eval Value
heapAlloc v = do
  st <- get
  let i   = nextRef st
      cell = HeapCell { hcValue = v, hcRC = 1 }
  put st { heap = Map.insert i cell (heap st), nextRef = i + 1 }
  pure (VRef i)

heapDeref :: Int -> Eval Value
heapDeref i = do
  h <- gets heap
  case Map.lookup i h of
    Nothing -> throwError $ "Dangling reference: #ref:" ++ show i
    Just c  -> pure (hcValue c)

heapAssign :: Int -> Value -> Eval ()
heapAssign i v = do
  h <- gets heap
  case Map.lookup i h of
    Nothing -> throwError $ "Dangling reference on assign: #ref:" ++ show i
    Just c  -> modify $ \st -> st { heap = Map.insert i (c { hcValue = v }) (heap st) }

-- Increment RC (called when a Ref is copied into a new binding)
rcIncr :: Int -> Eval ()
rcIncr i = modify $ \st ->
  st { heap = Map.adjust (\c -> c { hcRC = hcRC c + 1 }) i (heap st) }

-- Decrement RC; free if zero
rcDecr :: Int -> Eval ()
rcDecr i = do
  h <- gets heap
  case Map.lookup i h of
    Nothing -> pure ()   -- already freed, ignore
    Just c  ->
      if hcRC c <= 1
        then modify $ \st -> st { heap = Map.delete i (heap st) }
        else modify $ \st -> st { heap = Map.adjust (\x -> x { hcRC = hcRC x - 1 }) i (heap st) }

-- ---------------------------------------------------------------------------
-- Mailbox operations
-- ---------------------------------------------------------------------------

mailSend :: Value -> Eval ()
mailSend msg = do
  st <- get
  let mb  = mailbox st
      cap = mailboxMax st
  when (length mb >= cap) $
    throwError $ "Mailbox overflow (max " ++ show cap ++ ")"
  let entry = "SEND -> " ++ show msg
  put st { mailbox  = mb ++ [msg]
         , actorLog = actorLog st ++ [entry]
         }

-- Take the next message; block would be async in reality, here we just error
-- if mailbox is empty (simulating a would-block).
mailReceive :: Eval Value
mailReceive = do
  mb <- gets mailbox
  case mb of
    []     -> throwError "Receive on empty mailbox (would block)"
    (m:ms) -> do
      entry <- pure $ "RECV <- " ++ show m
      modify $ \st -> st { mailbox  = ms
                         , actorLog = actorLog st ++ [entry]
                         }
      pure m

-- ---------------------------------------------------------------------------
-- Built-in functions (the standard library / reflection primitives)
-- ---------------------------------------------------------------------------

-- type/1 : Value -> VType (the constructor tag as a Type-level value)
builtinTypeOf :: Fn
builtinTypeOf [v] = pure $ VType (tagOf v)
builtinTypeOf _   = throwError "type/1 expects exactly one argument"

-- function/1 : VFn -> VStr (name of the function)
builtinFnName :: Fn
builtinFnName [VFn n _] = pure $ VStr n
builtinFnName [_]       = throwError "function/1 expects a Fn value"
builtinFnName _         = throwError "function/1 expects exactly one argument"

-- eq/2
builtinEq :: Fn
builtinEq [a, b] = pure $ VBool (showEq a b)
  where
    showEq (VInt x)    (VInt y)    = x == y
    showEq (VBool x)   (VBool y)   = x == y
    showEq (VStr x)    (VStr y)    = x == y
    showEq VUnit       VUnit       = True
    showEq (VType x)   (VType y)   = x == y
    showEq (VTagged x xs) (VTagged y ys) = x == y && length xs == length ys && all (uncurry showEq) (zip xs ys)
    showEq _ _                     = False
builtinEq _ = throwError "eq/2 expects two arguments"

-- add, sub, mul for Int
builtinArith :: String -> (Int -> Int -> Int) -> Fn
builtinArith _ f [VInt a, VInt b] = pure $ VInt (f a b)
builtinArith n _ _ = throwError $ n ++ " expects two Int arguments"

-- print/1 — side-effecting IO
builtinPrint :: Fn
builtinPrint [v] = do
  liftIO $ putStrLn $ "  >> " ++ show v
  pure VUnit
builtinPrint _ = throwError "print/1 expects one argument"

-- rc_incr / rc_decr exposed as language primitives for demo
builtinRcIncr :: Fn
builtinRcIncr [VRef i] = rcIncr i >> pure VUnit
builtinRcIncr _ = throwError "rc_incr expects a Ref"

builtinRcDecr :: Fn
builtinRcDecr [VRef i] = rcDecr i >> pure VUnit
builtinRcDecr _ = throwError "rc_decr expects a Ref"

-- rc_count/1 — inspect RC for demo/debugging
builtinRcCount :: Fn
builtinRcCount [VRef i] = do
  h <- gets heap
  case Map.lookup i h of
    Nothing -> pure $ VInt 0
    Just c  -> pure $ VInt (hcRC c)
builtinRcCount _ = throwError "rc_count expects a Ref"

-- ---------------------------------------------------------------------------
-- Base environment
-- ---------------------------------------------------------------------------

baseEnv :: Env
baseEnv = Map.fromList
  [ ("type",     VFn "type"     builtinTypeOf)
  , ("function", VFn "function" builtinFnName)
  , ("eq",       VFn "eq"       builtinEq)
  , ("add",      VFn "add"      (builtinArith "add" (+)))
  , ("sub",      VFn "sub"      (builtinArith "sub" (-)))
  , ("mul",      VFn "mul"      (builtinArith "mul" (*)))
  , ("print",    VFn "print"    builtinPrint)
  , ("rc_incr",  VFn "rc_incr"  builtinRcIncr)
  , ("rc_decr",  VFn "rc_decr"  builtinRcDecr)
  , ("rc_count", VFn "rc_count" builtinRcCount)
  -- type-level constants that programs can compare against
  , ("Int",      VType "Int")
  , ("Bool",     VType "Bool")
  , ("Str",      VType "Str")
  , ("Fn",       VType "Fn")
  , ("Type",     VType "Type")
  , ("Unit",     VType "Unit")
  , ("Ref",      VType "Ref")
  ]

-- ---------------------------------------------------------------------------
-- Interpreter
-- ---------------------------------------------------------------------------

eval :: Expr -> Eval Value

eval (Lit v) = pure v

eval (Var n) = lookupVar n

eval (Lam params body) = do
  env <- ask
  let name = "<lam>"
      fn args
        | length args /= length params =
            throwError $ "Arity mismatch: expected " ++ show (length params)
                      ++ " got " ++ show (length args)
        | otherwise =
            -- capture current env (Reader), extend with params
            local (const (foldr (uncurry Map.insert) env (zip params args)))
                  (eval body)
  pure $ VFn name fn

eval (App f argExprs) = do
  fv   <- eval f
  args <- mapM eval argExprs
  case fv of
    VFn _ fn -> fn args
    _        -> throwError $ "Application of non-function: " ++ show fv

eval (Let n rhs body) = do
  v <- eval rhs
  withBinding n v (eval body)

eval (If cond t f) = do
  cv <- eval cond
  case cv of
    VBool True  -> eval t
    VBool False -> eval f
    _           -> throwError $ "If condition must be Bool, got: " ++ show cv

eval (Seq [])     = pure VUnit
eval (Seq [e])    = eval e
eval (Seq (e:es)) = eval e >> eval (Seq es)

eval (Send msgExpr destExpr) = do
  -- destExpr should evaluate to a VRef pointing at a mailbox-like structure.
  -- In this single-actor demo we just send to our own mailbox for simplicity.
  -- (In the real multi-actor version destExpr would be an ActorAddr.)
  _dest <- eval destExpr   -- evaluate but ignore for single-actor demo
  msg   <- eval msgExpr
  mailSend msg
  pure VUnit

eval (Receive handlers) = do
  msg <- mailReceive
  let tag = tagOf msg
  case Map.lookup tag handlers of
    Nothing -> throwError $ "No receive handler for message tag: " ++ tag
    Just (params, body) ->
      -- bind the fields of the tagged value to the params
      let fields = case msg of
                     VTagged _ vs -> vs
                     _            -> [msg]
      in if length params /= length fields
           then throwError $ "Receive handler arity mismatch for " ++ tag
           else withBindings (zip params fields) (eval body)

eval (Alloc e) = do
  v <- eval e
  heapAlloc v

eval (Deref e) = do
  v <- eval e
  case v of
    VRef i -> heapDeref i
    _      -> throwError $ "Deref of non-Ref: " ++ show v

eval (Assign dest src) = do
  dv <- eval dest
  sv <- eval src
  case dv of
    VRef i -> heapAssign i sv >> pure VUnit
    _      -> throwError $ "Assign to non-Ref: " ++ show dv

-- ---------------------------------------------------------------------------
-- Helpers for building AST expressions more concisely
-- ---------------------------------------------------------------------------

app :: Name -> [Expr] -> Expr
app f = App (Var f)

int :: Int -> Expr
int = Lit . VInt

str :: String -> Expr
str = Lit . VStr

bool :: Bool -> Expr
bool = Lit . VBool

tagged :: Name -> [Value] -> Expr
tagged n vs = Lit (VTagged n vs)

typeVal :: Name -> Expr
typeVal = Lit . VType

-- ---------------------------------------------------------------------------
-- Demo programs
-- ---------------------------------------------------------------------------

-- Program 1: Reflection — type/1 and function/1
-- Shows that type-level operations are just regular function calls.
program1 :: Expr
program1 = Seq
  [ Let "x" (int 42) $
    Let "t" (app "type" [Var "x"]) $
    Seq [ app "print" [Var "t"]               -- #type:Int
        , app "print" [app "eq" [Var "t", typeVal "Int"]]  -- True
        ]
  , Let "tagged_val" (tagged "Num" [VInt 5]) $
    Let "t2" (app "type" [Var "tagged_val"]) $
    app "print" [Var "t2"]                    -- #type:Num
  , Let "f" (Lam ["a"] (app "add" [Var "a", int 1])) $
    Let "fname" (app "function" [Var "f"]) $
    app "print" [Var "fname"]                 -- #fn:<lam>
  , Let "myFn" (Lit (VFn "myFn" (\[VInt n] -> pure (VInt (n * 2))
                                  `catchError` \_ -> throwError "myFn: Int expected"))) $
    Seq [ app "print" [app "function" [Var "myFn"]]  -- #fn:myFn
        , app "print" [app "type" [Var "myFn"]]      -- #type:Fn
        ]
  ]

-- Program 2: Runtime type dispatch (what PE would specialise away)
-- A polymorphic "describe" that branches on the type of its argument.
-- PE with a known argument would collapse the if-chain to a single branch.
program2 :: Expr
program2 =
  Let "describe" (Lam ["v"] $
    If (app "eq" [app "type" [Var "v"], typeVal "Int"])
       (app "print" [Lit (VStr "got an Int")])
    $ If (app "eq" [app "type" [Var "v"], typeVal "Str"])
         (app "print" [Lit (VStr "got a Str")])
    $ app "print" [Lit (VStr "got something else")]) $
  Seq
    [ app "describe" [int 99]
    , app "describe" [str "hello"]
    , app "describe" [bool True]
    ]

-- Program 3: Heap allocation and reference counting
program3 :: Expr
program3 =
  Let "ref" (Alloc (int 10)) $
  Seq
    [ app "print" [app "rc_count" [Var "ref"]]    -- RC = 1
    , app "rc_incr" [Var "ref"]                    -- manual incr (simulating copy)
    , app "print" [app "rc_count" [Var "ref"]]    -- RC = 2
    , app "print" [Deref (Var "ref")]             -- 10
    , Assign (Var "ref") (int 42)
    , app "print" [Deref (Var "ref")]             -- 42
    , app "rc_decr" [Var "ref"]                    -- simulating copy dropped
    , app "print" [app "rc_count" [Var "ref"]]    -- RC = 1
    , app "rc_decr" [Var "ref"]                    -- drops to 0 → freed
    , app "print" [app "rc_count" [Var "ref"]]    -- RC = 0 (freed)
    ]

-- Program 4: Mailbox / actor message passing (single actor, self-send)
-- Simulates the actor loop: send messages to own mailbox, then receive.
program4 :: Expr
program4 =
  Seq
    -- Send two messages (dest is ignored in single-actor demo — just VUnit)
    [ Send (tagged "Request" [VStr "read", VStr "/dev/mouse"]) (Lit VUnit)
    , Send (tagged "Request" [VStr "write", VStr "/tmp/out"])  (Lit VUnit)
    -- Receive and handle
    , Receive $ Map.fromList
        [ ("Request", (["op", "path"],
            Seq [ app "print" [Lit (VStr "handling request")]
                , app "print" [Var "op"]
                , app "print" [Var "path"]
                ]))
        ]
    , Receive $ Map.fromList
        [ ("Request", (["op", "path"],
            Seq [ app "print" [Lit (VStr "handling request")]
                , app "print" [Var "op"]
                , app "print" [Var "path"]
                ]))
        ]
    ]

-- Program 5: Combined — a "main actor" that does a bit of everything
programMain :: Expr
programMain = Seq
  [ section "=== type reflection ==="
    (Seq [ Let "v" (tagged "Ok" [VInt 200]) $
           app "print" [app "type" [Var "v"]]    -- #type:Ok
         ])
  , section "=== heap + RC ==="
    (Let "cell" (Alloc (str "hello")) $
     Seq [ app "print" [Deref (Var "cell")]
         , Assign (Var "cell") (str "world")
         , app "print" [Deref (Var "cell")]
         , app "rc_decr" [Var "cell"]
         ])
  , section "=== mailbox ==="
    (Seq [ Send (tagged "Ping" [VInt 1]) (Lit VUnit)
         , Send (tagged "Pong" [VInt 2]) (Lit VUnit)
         , Receive $ Map.fromList
             [ ("Ping", (["n"], app "print" [app "add" [Var "n", int 100]])) ]
         , Receive $ Map.fromList
             [ ("Pong", (["n"], app "print" [app "add" [Var "n", int 200]])) ]
         ])
  ]
  where
    section lbl body = Seq [app "print" [Lit (VStr lbl)], body]

-- ---------------------------------------------------------------------------
-- Run a program as a "main actor"
-- ---------------------------------------------------------------------------

runAsActor :: String -> Expr -> IO ()
runAsActor name prog = do
  putStrLn $ "\n=== Actor: " ++ name ++ " ==="
  let st = initialActorState 16   -- mailbox capacity 16
  result <- evalWith baseEnv st (eval prog)
  case result of
    Left err       -> putStrLn $ "CRASH: " ++ err
    Right (_, st') -> do
      putStrLn $ "OK  Heap cells alive: " ++ show (Map.size (heap st'))
      unless (null (actorLog st')) $ do
        putStrLn "Mailbox log:"
        mapM_ (putStrLn . ("   " ++)) (actorLog st')
  putStrLn $ replicate (12 + length name) '-'

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  runAsActor "reflection"   program1
  runAsActor "type-dispatch" program2
  runAsActor "heap-rc"      program3
  runAsActor "mailbox"      program4
  runAsActor "main"         programMain