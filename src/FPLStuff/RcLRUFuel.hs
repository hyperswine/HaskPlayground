{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

-- ============================================================
-- TablingModel: an operational model of the composed call path
--
--   fuel charge (2-stage) -> hash -> probe -> hit/miss
--   -> precondition check -> Rc/Arc accounting -> LRU insert/evict
--   -> yield-on-exhaustion -> crash-with-cleanup
--
-- Local actor memory is modeled as "bulk-freed on death" (no
-- per-object walk). Arc.qa is modeled as a separate refcount
-- ledger that only changes when this actor sends it messages
-- (incref/decref/alloc) -- mirroring the linearized-owner design.
-- ============================================================

module FPLStuff.RcLRUFuel where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Bits                  (shiftR)
import           Data.List                  (minimumBy)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Ord                   (comparing)
import           Data.Word                  (Word64)
import Control.Monad (foldM)

-- ------------------------------------------------------------
-- Values / AST
-- ------------------------------------------------------------

type Name  = String
type ArcId = Int

data Value
  = IntVal  Integer
  | BoolVal Bool
  | ArcVal  ArcId          -- handle to Arc.qa-owned shared memory
  | UnitVal
  deriving (Show, Eq)

data Op = Add | Sub | Mul | Lt | Ge | Eq'
  deriving (Show, Eq)

data Expr
  = Lit Value
  | Var Name
  | BinOp Op Expr Expr
  | If Expr Expr Expr
  | Call Name [Expr]       -- function call: the composed gate lives here
  | AllocArc Expr          -- allocate a shared object via Arc.qa
  | Seq [Expr]
  deriving (Show)

-- A function definition. `tabled` is the OUTPUT of the compile-time
-- eligibility check (effect-free && WCET-bounded && cheap-to-hash args);
-- the model takes it as given, as discussed.
data FnDef = FnDef
  { fnParams  :: [Name]
  , fnPrecond :: Maybe Expr   -- dynamic precondition over params
  , fnBody    :: Expr
  , fnTabled  :: Bool
  , fnCost    :: Int          -- full fuel cost of a real call (CALL_COST)
  }

probeCost :: Int
probeCost = 1                 -- PROBE_COST: flat fee for the tabling attempt

-- ------------------------------------------------------------
-- Per-function LRU table (open addressing, multiply-shift hash)
-- ------------------------------------------------------------

data Entry = Entry
  { eKey  :: [Integer]        -- the actual args (checked on probe, not trusted hash)
  , eVal  :: Value
  , eUsed :: Int              -- last-access tick, for LRU
  } deriving (Show)

data FnTable = FnTable
  { tSlots :: Map Int Entry   -- slot index -> entry
  , tSize  :: Int             -- power of two
  , tK     :: Int             -- log2 tSize
  , tAs    :: [Word64]        -- random odd constants (Dietzfelbinger / Carter-Wegman)
  }

newTable :: Int -> Int -> [Word64] -> FnTable
newTable size k as = FnTable Map.empty size k as

-- h(x1..xn) = (a1*x1 + a2*x2 + ...) >> (64 - k)
-- one multiply-add per arg, one shift; wraparound is intended
hashArgs :: FnTable -> [Integer] -> Int
hashArgs t xs =
  let s = sum (zipWith (*) (tAs t) (map fromIntegral xs)) :: Word64
  in fromIntegral (s `shiftR` (64 - tK t))

-- ------------------------------------------------------------
-- World: actor-local state + the Arc.qa ledger + event log
-- ------------------------------------------------------------

data World = World
  { wFuel     :: Int
  , wBudget   :: Int                 -- refuel amount on reschedule
  , wTick     :: Int                 -- monotonic clock for LRU
  , wTables   :: Map Name FnTable    -- per-actor, per-function
  , wHeldArcs :: [ArcId]             -- actor's live local Arc references
  , wArcQa    :: Map ArcId Int       -- Arc.qa process: refcount ledger
  , wNextArc  :: ArcId
  , wLog      :: [String]            -- reversed event log
  }

data Crash = Crash String

type M = ExceptT Crash (State World)

say :: String -> M ()
say s = modify (\w -> w { wLog = s : wLog w })

tickUp :: M Int
tickUp = do w <- get; put w { wTick = wTick w + 1 }; pure (wTick w)

-- ------------------------------------------------------------
-- Fuel: charged BEFORE work; exhaustion forces yield -> hart scheduler
-- ------------------------------------------------------------

chargeFuel :: String -> Int -> M ()
chargeFuel what cost = do
  w <- get
  if wFuel w < cost
    then do
      say $ "  [fuel]  exhausted (" ++ show (wFuel w) ++ " < " ++ show cost
            ++ " for " ++ what ++ ") -> yield()"
      say   "  [sched] hart_schedule(): actor suspended ... resumed, refueled"
      modify (\w' -> w' { wFuel = wBudget w' })
      chargeFuel what cost                      -- resume: re-attempt the charge
    else do
      put w { wFuel = wFuel w - cost }
      say $ "  [fuel]  -" ++ show cost ++ " (" ++ what ++ "), remaining "
            ++ show (wFuel w - cost)

-- ------------------------------------------------------------
-- Arc.qa protocol: this actor only ever SENDS messages;
-- the ledger is Arc.qa's state, mutated on receipt.
-- ------------------------------------------------------------

arcSend :: String -> (Map ArcId Int -> Map ArcId Int) -> M ()
arcSend msg f = do
  say $ "  [arc->] " ++ msg
  modify (\w -> w { wArcQa = f (wArcQa w) })

arcAlloc :: M ArcId
arcAlloc = do
  w <- get
  let aid = wNextArc w
  put w { wNextArc = aid + 1 }
  arcSend ("alloc #" ++ show aid ++ " (count=1, caller's ref)")
          (Map.insert aid 1)
  modify (\w' -> w' { wHeldArcs = aid : wHeldArcs w' })
  pure aid

arcIncref :: String -> ArcId -> M ()
arcIncref why aid =
  arcSend ("incref #" ++ show aid ++ " (" ++ why ++ ")")
          (Map.adjust (+ 1) aid)

arcDecref :: String -> ArcId -> M ()
arcDecref why aid =
  arcSend ("decref #" ++ show aid ++ " (" ++ why ++ ")")
          (Map.adjust (subtract 1) aid)

-- ------------------------------------------------------------
-- The composed call gate
-- ------------------------------------------------------------

callFn :: Map Name FnDef -> Name -> [Value] -> M Value
callFn defs fname argVals = do
  let fn = defs Map.! fname
  case (fnTabled fn, traverse asInt argVals) of

    -- ============ TABLED PATH (all-Int args) ============
    (True, Just intArgs) -> do
      -- 1. flat probe fee, paid before we even hash
      chargeFuel (fname ++ " probe") probeCost
      t   <- getTable fname
      let idx0 = hashArgs t intArgs
      say $ "  [hash]  h" ++ show intArgs ++ " = (sum a_i*x_i) >> " ++ show (64 - tK t)
            ++ " = slot " ++ show idx0
      now <- tickUp
      -- 2. linear probe: match -> HIT, empty -> insert slot, else remember LRU victim
      case probe t idx0 intArgs of
        PHit slot entry -> do
          say $ "  [table] HIT at slot " ++ show slot ++ " -> "
                ++ show (eVal entry) ++ "  (precond + body SKIPPED)"
          putEntry fname slot entry { eUsed = now }        -- refresh LRU
          -- hit hands the caller a NEW reference to a shared value
          case eVal entry of
            ArcVal aid -> do
              arcIncref "hit hands caller a new ref" aid
              modify (\w -> w { wHeldArcs = aid : wHeldArcs w })
            _ -> pure ()
          pure (eVal entry)

        PMiss whereTo -> do
          say "  [table] MISS"
          -- 3. pay the remainder: total == full CALL_COST
          chargeFuel (fname ++ " body") (fnCost fn - probeCost)
          -- 4. dynamic precondition (only ever runs on the miss path)
          v <- runPrecondAndBody defs fn fname argVals
          -- 5. insert; table's own reference to a shared value is counted
          slot <- case whereTo of
            EmptySlot s  -> pure s
            EvictLRU s victim -> do
              say $ "  [table] full: evict LRU slot " ++ show s
                    ++ " key=" ++ show (eKey victim)
              case eVal victim of
                ArcVal aid -> arcDecref "LRU eviction releases table's ref" aid
                _          -> pure ()
              pure s
          case v of
            ArcVal aid -> arcIncref "table insert holds a ref" aid
            _          -> pure ()
          putEntry fname slot (Entry intArgs v now)
          say $ "  [table] insert slot " ++ show slot ++ " := " ++ show v
          pure v

    -- ============ NON-TABLED PATH ============
    _ -> do
      chargeFuel (fname ++ " (untabled)") (fnCost fn)
      runPrecondAndBody defs fn fname argVals

-- precondition -> crash on failure; then Rc-for-args + body.
runPrecondAndBody :: Map Name FnDef -> FnDef -> Name -> [Value] -> M Value
runPrecondAndBody defs fn fname argVals = do
  let env = Map.fromList (zip (fnParams fn) argVals)
  case fnPrecond fn of
    Nothing -> pure ()
    Just p  -> do
      pv <- eval defs env p
      case pv of
        BoolVal True  -> say "  [check] precondition OK"
        _             -> do
          say $ "  [check] PRECONDITION FAILED for " ++ fname ++ show argVals
          throwError (Crash (fname ++ show argVals ++ ": precondition violated"))
  -- (local Rc bumps for bound args happen here in the real system;
  --  they're arena-internal, so the model doesn't track them individually)
  eval defs env (fnBody fn)

-- ------------------------------------------------------------
-- Probe machinery
-- ------------------------------------------------------------

data ProbeResult
  = PHit  Int Entry
  | PMiss MissSlot

data MissSlot
  = EmptySlot Int
  | EvictLRU  Int Entry

probe :: FnTable -> Int -> [Integer] -> ProbeResult
probe t idx0 key = go 0 Nothing
  where
    n = tSize t
    go i firstEmpty
      | i == n =
          case firstEmpty of
            Just s  -> PMiss (EmptySlot s)
            Nothing ->
              let occupied = [ (s, e) | s <- [0 .. n - 1]
                                      , Just e <- [Map.lookup s (tSlots t)] ]
                  (vs, ve)  = minimumBy (comparing (eUsed . snd)) occupied
              in PMiss (EvictLRU vs ve)
      | otherwise =
          let s = (idx0 + i) `mod` n
          in case Map.lookup s (tSlots t) of
               Just e | eKey e == key -> PHit s e
               Just _                 -> go (i + 1) firstEmpty
               Nothing                -> go (i + 1) (firstEmpty `orElse` Just s)
    orElse (Just x) _ = Just x
    orElse Nothing  y = y

getTable :: Name -> M FnTable
getTable f = gets ((Map.! f) . wTables)

putEntry :: Name -> Int -> Entry -> M ()
putEntry f s e =
  modify (\w -> w { wTables =
    Map.adjust (\t -> t { tSlots = Map.insert s e (tSlots t) }) f (wTables w) })

-- ------------------------------------------------------------
-- Evaluator
-- ------------------------------------------------------------

eval :: Map Name FnDef -> Map Name Value -> Expr -> M Value
eval defs env = \case
  Lit v   -> pure v
  Var x   -> maybe (throwError (Crash ("unbound " ++ x))) pure (Map.lookup x env)
  BinOp op a b -> do
    va <- eval defs env a
    vb <- eval defs env b
    binop op va vb
  If c th el -> do
    cv <- eval defs env c
    case cv of
      BoolVal True  -> eval defs env th
      BoolVal False -> eval defs env el
      _             -> throwError (Crash "if: non-bool condition")
  Seq es -> foldM (\_ e -> eval defs env e) UnitVal es
  AllocArc e -> do
    _payload <- eval defs env e
    ArcVal <$> arcAlloc
  Call f args -> do
    argVals <- mapM (eval defs env) args
    say $ "[call] " ++ f ++ show argVals
    callFn defs f argVals

binop :: Op -> Value -> Value -> M Value
binop op (IntVal a) (IntVal b) = pure $ case op of
  Add -> IntVal (a + b);  Sub -> IntVal (a - b);  Mul -> IntVal (a * b)
  Lt  -> BoolVal (a < b); Ge  -> BoolVal (a >= b); Eq' -> BoolVal (a == b)
binop _ _ _ = throwError (Crash "binop: bad operands")

asInt :: Value -> Maybe Integer
asInt (IntVal n) = Just n
asInt _          = Nothing

-- ------------------------------------------------------------
-- Crash cleanup: the ONLY per-item walk is over Arc handles.
-- Local memory (arenas, slabs, the table's backing array) is
-- reclaimed in bulk with no traversal of local Rc objects.
-- ------------------------------------------------------------

crashCleanup :: String -> State World ()
crashCleanup reason = do
  logC $ "[CRASH] actor crashing: " ++ reason
  w <- get
  -- 1. table entries holding Arc refs -> one decref message each
  let tableArcs = [ aid | t <- Map.elems (wTables w)
                        , e <- Map.elems (tSlots t)
                        , ArcVal aid <- [eVal e] ]
  mapM_ (dec "table entry ref released by crash") tableArcs
  -- 2. actor's live local Arc handles -> same walk, same message
  mapM_ (dec "actor-held local ref released by crash") (wHeldArcs w)
  -- 3. everything local: one bulk operation, no per-object work
  logC "[CRASH] bulk-free: actor arenas + slabs + LRU table backing memory"
  logC "[CRASH] notify_scheduler(actor_dead)"
  where
    logC s = modify (\w -> w { wLog = s : wLog w })
    dec why aid = do
      logC $ "  [arc->] decref #" ++ show aid ++ " (" ++ why ++ ")"
      modify (\w -> w { wArcQa = Map.adjust (subtract 1) aid (wArcQa w) })

-- ------------------------------------------------------------
-- Demo program
-- ------------------------------------------------------------

defs :: Map Name FnDef
defs = Map.fromList
  [ ("square", FnDef
      { fnParams  = ["n"]
      , fnPrecond = Just (BinOp Ge (Var "n") (Lit (IntVal 0)))
      , fnBody    = BinOp Mul (Var "n") (Var "n")
      , fnTabled  = True
      , fnCost    = 5 })
  , ("mkShared", FnDef                       -- returns Arc.qa-backed memory
      { fnParams  = ["n"]
      , fnPrecond = Nothing
      , fnBody    = AllocArc (Var "n")
      , fnTabled  = True
      , fnCost    = 6 })
  ]

program :: Expr
program = Seq
  [ Call "square" [Lit (IntVal 7)]     -- MISS: full cost, precond, insert
  , Call "square" [Lit (IntVal 7)]     -- HIT: probe cost only, skips precond+body
  , Call "square" [Lit (IntVal 8)]     -- MISS (also triggers a fuel yield)
  , Call "square" [Lit (IntVal 7)]     -- HIT again after yield
  , Call "mkShared" [Lit (IntVal 1)]   -- MISS: Arc alloc + table incref
  , Call "mkShared" [Lit (IntVal 1)]   -- HIT: incref (caller's new ref)
  , Call "mkShared" [Lit (IntVal 2)]   -- MISSes below fill the 4-slot table...
  , Call "mkShared" [Lit (IntVal 3)]
  , Call "mkShared" [Lit (IntVal 4)]
  , Call "mkShared" [Lit (IntVal 5)]   -- ...forcing an LRU eviction -> decref
  , Call "square" [Lit (IntVal (-3))]  -- precondition fails -> crash + cleanup
  , Call "square" [Lit (IntVal 9)]     -- never reached
  ]

initialWorld :: World
initialWorld = World
  { wFuel     = 14
  , wBudget   = 14
  , wTick     = 0
  , wTables   = Map.fromList
      [ ("square",   newTable 4 2 [0x9E3779B97F4A7C15, 0xC2B2AE3D27D4EB4F])
      , ("mkShared", newTable 4 2 [0xFF51AFD7ED558CCD, 0xC4CEB9FE1A85EC53]) ]
  , wHeldArcs = []
  , wArcQa    = Map.empty
  , wNextArc  = 0
  , wLog      = []
  }

main :: IO ()
main = do
  let (result, w) = runState (runExceptT (eval defs Map.empty program)) initialWorld
      w' = case result of
             Left (Crash why) -> execState (crashCleanup why) w
             Right _          -> w
  mapM_ putStrLn (reverse (wLog w'))
  putStrLn ""
  putStrLn $ "final Arc.qa ledger: " ++ show (Map.toList (wArcQa w'))
  case result of
    Left  _ -> putStrLn "actor status: DEAD (crashed, cleaned up)"
    Right v -> putStrLn $ "actor status: alive, result = " ++ show v
