{-# LANGUAGE LambdaCase #-}

module PEDemo where

import Control.Monad (forM)
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, addUTCTime)
import Text.Printf (printf)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

-- ============================================================
-- Core AST  (unchanged)
-- ============================================================

type Name = String

data Op = Add | Sub | Mul | Lt
  deriving (Show, Eq)

data Expr
  = Lit Int
  | Var Name
  | BinOp Op Expr Expr
  | If Expr Expr Expr
  | Let Name Expr Expr
  | Dynamic String [Expr]
  deriving (Show, Eq)

data Module = Module
  { modName :: String
  , modBody :: Expr
  } deriving Show

apply :: Op -> Int -> Int -> Int
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Lt  = \a b -> if a < b then 1 else 0

opSym :: Op -> String
opSym Add = "+"
opSym Sub = "-"
opSym Mul = "*"
opSym Lt  = "<"

-- ShowS (String -> String) instead of naive (++) -- composing functions
-- with (.) is O(1) per node; naive (++) here would append a small
-- string onto an already-large one at every recursion level, which
-- is the classic O(n^2) trap (confirmed by direct measurement: it's
-- what actually hung this demo, not the fuel/time bookkeeping).
prettyS :: Expr -> ShowS
prettyS = \case
  Lit n           -> shows n
  Var x           -> showString x
  BinOp op l r    -> showChar '(' . prettyS l . showChar ' ' . showString (opSym op)
                     . showChar ' ' . prettyS r . showChar ')'
  If c t e        -> showString "if " . prettyS c . showString " then " . prettyS t
                     . showString " else " . prettyS e
  Let x b body    -> showString "let " . showString x . showString " = " . prettyS b
                     . showString " in " . prettyS body
  Dynamic nm args -> showString nm . showChar '(' . showArgs args . showChar ')'
    where
      showArgs []     = id
      showArgs [a]    = prettyS a
      showArgs (a:as) = prettyS a . showString ", " . showArgs as

pretty :: Expr -> String
pretty e = prettyS e ""

data Binding = Known Expr | Residual
type Env = Map Name Binding

-- ============================================================
-- Fuel + wall-clock budget
--
-- `fsBlocked` records whether *any* rule during this pass was
-- ready to fire but got refused purely by budget (fuel or time)
-- -- as opposed to genuinely having no more rules left to try.
-- That distinction is the whole point:
--
--   fsBlocked == False  ->  true fixpoint. Nothing in this term
--     matches any rule any more. More fuel, more time, or both,
--     changes nothing -- there's nothing left for a rule to hit.
--
--   fsBlocked == True   ->  fuel-starved or time-starved. More
--     budget would provably let it make more progress.
--
-- `fsTimedOut` records *why* it's blocked, for reporting only --
-- ran out of fuel vs. tripped the wall-clock breaker. Both are
-- funnelled through the same `outOfBudget` check, so a rewrite
-- rule never has to know or care which limit stopped it: from
-- its point of view a fuel-out and a time-out are the same kind
-- of "stuck" as hitting a genuine Dynamic node.
-- ============================================================

data FuelState = FuelState
  { fsRemaining :: !Int
  , fsBlocked   :: !Bool
  , fsTimedOut  :: !Bool
  , fsDeadline  :: !UTCTime
  }

type Spec a = StateT FuelState IO a

outOfBudget :: Spec Bool
outOfBudget = do
  fs <- get
  if fsRemaining fs <= 0
    then do put fs { fsBlocked = True }; pure True
    else do
      now <- liftIO getCurrentTime
      if now >= fsDeadline fs
        then do put fs { fsBlocked = True, fsTimedOut = True }; pure True
        else pure False

spend :: Spec ()
spend = modify $ \fs -> fs { fsRemaining = fsRemaining fs - 1 }

specialize :: Env -> Expr -> Spec Expr
specialize env expr = case expr of

  Lit n -> pure (Lit n)

  Var x -> case M.lookup x env of
    Just (Known v) -> pure v
    Just Residual  -> pure (Var x)
    Nothing        -> pure (Var x)

  BinOp op l r -> do
    l' <- specialize env l
    r' <- specialize env r
    case (l', r') of
      (Lit a, Lit b) -> do
        stuck <- outOfBudget
        if stuck then pure (BinOp op l' r') else spend >> pure (Lit (apply op a b))
      _ -> pure (BinOp op l' r')

  If c t e -> do
    c' <- specialize env c
    case c' of
      Lit n -> do
        stuck <- outOfBudget
        if stuck
          then If c' <$> specialize env t <*> specialize env e
          else spend >> specialize env (if n /= 0 then t else e)
      _ -> If c' <$> specialize env t <*> specialize env e

  Let x b body -> do
    b' <- specialize env b
    case b' of
      Lit _ -> do
        stuck <- outOfBudget
        if stuck
          then Let x b' <$> specialize (M.insert x Residual env) body
          else spend >> specialize (M.insert x (Known b') env) body
      _ -> Let x b' <$> specialize (M.insert x Residual env) body

  Dynamic nm args -> Dynamic nm <$> mapM (specialize env) args

runSpecialize :: Int -> UTCTime -> Expr -> IO (Expr, FuelState)
runSpecialize fuel deadline e = runStateT (specialize M.empty e) (FuelState fuel False False deadline)

-- ============================================================
-- Static-work estimate -- only used to justify a *starting*
-- split. The worklist below is what actually corrects a bad
-- guess, by taking fuel back from modules that turn out idle
-- and handing it to modules that are still blocked.
-- ============================================================

estimateStaticWork :: Expr -> Int
estimateStaticWork = \case
  Lit _           -> 0
  Var _           -> 0
  BinOp _ l r     -> 1 + estimateStaticWork l + estimateStaticWork r
  If c t e        -> 1 + estimateStaticWork c + estimateStaticWork t + estimateStaticWork e
  Let _ b body    -> 1 + estimateStaticWork b + estimateStaticWork body
  Dynamic _ args  -> sum (map estimateStaticWork args)

-- ============================================================
-- The worklist: a *shared* fuel pool, redistributed round by
-- round. Each round, every module still `fsBlocked` gets an
-- equal slice of whatever pool remains; anyone who reaches a
-- genuine fixpoint drops out and stops drawing from the pool,
-- leaving their unused share for whoever's still working. This
-- is "always try to spend as much of the budget as it can
-- usefully absorb" -- but because idle modules are detected and
-- excluded, fuel never gets handed to a module with nothing
-- left to do with it.
--
-- A single shared wall-clock deadline is checked at the top of
-- every round, and inside outOfBudget on every single rewrite
-- attempt. Once it passes, the loop stops unconditionally,
-- however much fuel is left in the pool -- it's a ceiling on
-- elapsed time, not on fuel, specifically to guard against what
-- the fuel accounting can't see: some rules doing far more real
-- work per unit of charged fuel than others.
-- ============================================================

data ModuleResult = ModuleResult
  { mrModule   :: Module
  , mrExpr     :: Expr
  , mrDone     :: Bool
  , mrTimedOut :: Bool
  , mrRounds   :: Int
  , mrSpent    :: Int
  }

worklist :: UTCTime -> Int -> [Module] -> IO [ModuleResult]
worklist deadline pool0 mods0 = loop pool0 initial (1 :: Int)
  where
    initial = [ ModuleResult m (modBody m) False False 0 0 | m <- mods0 ]

    runRound grant r = do
      (residual, fs) <- runSpecialize grant deadline (mrExpr r)
      let spentNow = grant - fsRemaining fs
      pure r { mrExpr     = residual
             , mrRounds   = mrRounds r + 1
             , mrSpent    = mrSpent r + spentNow
             , mrDone     = not (fsBlocked fs)
             , mrTimedOut = fsTimedOut fs
             }

    loop pool results roundNo = do
      now <- getCurrentTime
      let blocked = filter (not . mrDone) results
      if pool <= 0 || null blocked || now >= deadline
        then pure results
        else do
          let share = max 1 (pool `div` length blocked)
          printf "  round %d: pool=%d, %d module(s) still working, %d fuel each\n"
                 roundNo pool (length blocked) share
          updated <- forM results $ \r -> if mrDone r then pure r else runRound share r
          let spentThisRound = sum (zipWith (\a b -> mrSpent b - mrSpent a) results updated)
          loop (pool - spentThisRound) updated (roundNo + 1)

reportResult :: ModuleResult -> IO ()
reportResult r = printf "-- %-18s rounds=%-2d spent=%-6d status=%-18s\n   residual: %s\n\n"
  (modName (mrModule r)) (mrRounds r) (mrSpent r) status (truncateShow (pretty (mrExpr r)))
  where
    status | mrTimedOut r = "TIMED OUT" :: String
           | mrDone r     = "fixpoint (idle)"
           | otherwise    = "pool exhausted"
    truncateShow s
      | length s <= 200 = s
      | otherwise = take 200 s ++ "...  [truncated, " ++ show (length s) ++ " chars total]"

-- ============================================================
-- Example modules
-- ============================================================

cheapConfig :: Expr
cheapConfig =
  Let "a" (BinOp Add (Lit 2) (Lit 3)) $
  Let "b" (BinOp Mul (Var "a") (Lit 4)) $
  Let "c" (If (BinOp Lt (Var "b") (Lit 100))
              (BinOp Add (Var "b") (Lit 1))
              (Lit 0)) $
  Var "c"

expensiveStatic :: Int -> Expr
expensiveStatic depth = chain depth
  where
    chain :: Int -> Expr
    chain 0 = Lit 1
    chain n = BinOp Add (Lit 1) (chain (n - 1))

ioModule :: Expr
ioModule =
  Let "cfg" (BinOp Mul (Lit 10) (Lit 10)) $
  Let "x"   (Dynamic "input" []) $
  If (BinOp Lt (Var "cfg") (Lit 1000))
     (BinOp Add (Var "x") (Var "cfg"))
     (Dynamic "println" [BinOp Add (Var "x") (Lit 1)])

deadlineFromNow :: NominalDiffTime -> IO UTCTime
deadlineFromNow secs = addUTCTime secs <$> getCurrentTime

-- ============================================================
-- Main
-- ============================================================

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let modules = [ Module "cheap_config" cheapConfig
                , Module "expensive_static" (expensiveStatic 60)
                , Module "io_module" ioModule
                ]

  putStrLn "=== Worklist: shared pool of 50, redistributed round by round ==="
  putStrLn ""
  deadline1 <- deadlineFromNow 30   -- generous; fuel is the only real constraint here
  results1 <- worklist deadline1 50 modules
  putStrLn ""
  mapM_ reportResult results1

  putStrLn "=== Same shape of module, but a hostile wall-clock deadline ==="
  putStrLn "    (stands in for a rule whose real cost per fuel unit is far"
  putStrLn "     higher than the others -- fuel bookkeeping alone can't see"
  putStrLn "     this, only the clock does)"
  putStrLn ""
  deadline2 <- deadlineFromNow 0.010   -- deliberately tiny: 10ms
  results2 <- worklist deadline2 1000000 [Module "expensive_static" (expensiveStatic 50000)]
  putStrLn ""
  mapM_ reportResult results2
