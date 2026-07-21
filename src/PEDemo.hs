{-# LANGUAGE LambdaCase #-}

module PEDemo where

import Control.Monad (forM_)
import Control.Monad.State
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.Printf (printf)

-- ============================================================
-- Core AST
--
-- Everything is static by construction *except* `Dynamic`,
-- which stands for an opaque effect boundary -- `input()`,
-- `println(..)`, a sensor read, whatever. There is no other
-- way for dynamism to enter a term: no free variables float in
-- from outside, no ambient IO. If a subtree contains no
-- `Dynamic` node anywhere inside it, it is, in principle, fully
-- reducible to a value.
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
  | Dynamic String [Expr] -- opaque effect: input(), println(x), readSensor()...
  deriving (Show, Eq)

data Module = Module
  { modName :: String,
    modBody :: Expr
  }
  deriving (Show)

apply :: Op -> Int -> Int -> Int
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Lt = \a b -> if a < b then 1 else 0

opSym :: Op -> String
opSym Add = "+"
opSym Sub = "-"
opSym Mul = "*"
opSym Lt = "<"

pretty :: Expr -> String
pretty = \case
  Lit n -> show n
  Var x -> x
  BinOp op l r -> "(" ++ pretty l ++ " " ++ opSym op ++ " " ++ pretty r ++ ")"
  If c t e -> "if " ++ pretty c ++ " then " ++ pretty t ++ " else " ++ pretty e
  Let x b body -> "let " ++ x ++ " = " ++ pretty b ++ " in " ++ pretty body
  Dynamic nm args -> nm ++ "(" ++ intercalate ", " (map pretty args) ++ ")"

-- ============================================================
-- Binding-time environment
--
-- A `let`-bound name is either `Known` (its definition already
-- reduced to a value, so occurrences get inlined) or `Residual`
-- (its definition didn't fully reduce -- blocked by fuel or by
-- a Dynamic node -- so occurrences stay as a symbolic Var
-- referring to whatever the residual program will compute for
-- it at runtime).
-- ============================================================

data Binding = Known Expr | Residual

type Env = Map Name Binding

-- ============================================================
-- The specializer
--
-- One `Spec` computation threads a single Int fuel counter.
-- The rule throughout: fuel is spent only on *productive*
-- rewrites -- a delta rule firing, an `if` collapsing to one
-- branch, a `let` being inlined away. Merely visiting/rebuilding
-- a node is free; fuel tracks static work *eliminated*, not
-- tree traversal.
--
-- Running out of fuel and hitting a `Dynamic` node have exactly
-- the same observable effect on the caller: the node in
-- question is left as residual code instead of a value. Fuel
-- exhaustion is never a correctness failure -- it just means
-- the residual program will redo that work at runtime instead
-- of it having been done once, here, at spec time.
-- ============================================================

type Spec a = State Int a

outOfFuel :: Spec Bool
outOfFuel = gets (<= 0)

spend :: Spec ()
spend = modify (subtract 1)

specialize :: Env -> Expr -> Spec Expr
specialize env expr = case expr of
  Lit n -> pure (Lit n)
  Var x -> case M.lookup x env of
    Just (Known v) -> pure v -- fully known: inline the value
    Just Residual -> pure (Var x) -- exists, but its value isn't known yet
    Nothing -> pure (Var x) -- free / unbound, leave symbolic
  BinOp op l r -> do
    l' <- specialize env l
    r' <- specialize env r
    case (l', r') of
      (Lit a, Lit b) -> do
        stuck <- outOfFuel
        if stuck
          then pure (BinOp op l' r') -- would reduce, but budget's gone
          else spend >> pure (Lit (apply op a b))
      _ -> pure (BinOp op l' r') -- one side dynamic/residual: delta rule can't match
  If c t e -> do
    c' <- specialize env c
    case c' of
      Lit n -> do
        stuck <- outOfFuel
        if stuck
          then If c' <$> specialize env t <*> specialize env e
          else spend >> specialize env (if n /= 0 then t else e)
      -- note: the branch NOT taken is dropped entirely,
      -- never even specialized -- real code-size win,
      -- not just constant folding.
      _ -> If c' <$> specialize env t <*> specialize env e
  Let x b body -> do
    b' <- specialize env b
    case b' of
      Lit _ -> do
        stuck <- outOfFuel
        if stuck
          then Let x b' <$> specialize (M.insert x Residual env) body
          else spend >> specialize (M.insert x (Known b') env) body
      -- let eliminated entirely: body specialized with x inlined
      _ -> Let x b' <$> specialize (M.insert x Residual env) body
  Dynamic nm args -> Dynamic nm <$> mapM (specialize env) args

-- the node itself never reduces -- but its
-- arguments may still contain static work
-- worth doing (contagion only taints the
-- Dynamic node's own reducibility, not
-- unrelated static subterms inside its args)

runSpecialize :: Int -> Expr -> (Expr, Int)
runSpecialize fuel e = runState (specialize M.empty e) fuel

-- ============================================================
-- Adaptive per-module fuel allocation
--
-- Heuristic: count nodes that are *candidate* rewrite sites
-- (BinOp / If / Let), crediting a Dynamic node's arguments (there
-- may be static work worth doing inside them) but not the
-- Dynamic node itself (it will never fire, no matter how much
-- fuel it gets). A module dominated by Dynamic nodes scores low
-- and is allocated little fuel, since spending more wouldn't
-- help it; a module that's deeply static scores high and is
-- allocated proportionally more from the shared pool.
--
-- This is a toy proxy, not the real metric discussed for a
-- production system (which would want to weight by *actual*
-- eliminated-runtime-cost per fuel unit, not raw node count) --
-- but it's enough to demonstrate the allocation mechanism.
-- ============================================================

estimateStaticWork :: Expr -> Int
estimateStaticWork = \case
  Lit _ -> 0
  Var _ -> 0
  BinOp _ l r -> 1 + estimateStaticWork l + estimateStaticWork r
  If c t e -> 1 + estimateStaticWork c + estimateStaticWork t + estimateStaticWork e
  Let _ b body -> 1 + estimateStaticWork b + estimateStaticWork body
  Dynamic _ args -> sum (map estimateStaticWork args)

allocateFuel :: Int -> [Module] -> [(Module, Int)]
allocateFuel pool mods = map alloc weighted
  where
    weighted = [(m, max 1 (estimateStaticWork (modBody m))) | m <- mods]
    total = sum (map snd weighted)
    alloc (m, w) = (m, max 1 (pool * w `div` total))

-- ============================================================
-- Example modules
-- ============================================================

-- Small, fully static config-style computation. Should fully
-- reduce to a literal well within a modest fuel budget -- the
-- "compute a constant table once, bake it in" case.
cheapConfig :: Expr
cheapConfig =
  Let "a" (BinOp Add (Lit 2) (Lit 3))
    $ Let "b" (BinOp Mul (Var "a") (Lit 4))
    $ Let
      "c"
      ( If
          (BinOp Lt (Var "b") (Lit 100))
          (BinOp Add (Var "b") (Lit 1))
          (Lit 0)
      )
    $ Var "c"

-- A deep, fully static chain of additions -- 30 nested redexes.
-- With a fuel budget well below 30, this module "bleeds" its
-- budget and gets cut off mid-fold: the deepest redexes (fired
-- first, as recursion unwinds bottom-up) collapse to a literal,
-- and the remaining outer layers are left as residual BinOp
-- nesting wrapping that partial result.
expensiveStatic :: Expr
expensiveStatic = chain 30
  where
    chain :: Int -> Expr
    chain 0 = Lit 1
    chain n = BinOp Add (Lit 1) (chain (n - 1))

-- A genuinely dynamic value (input()) sits right next to a
-- static one (cfg). The point: dynamism in *data* doesn't stop
-- specialization of *control flow* that doesn't depend on that
-- data -- the `if`'s condition is purely static, so it still
-- collapses to one branch, and the *entire* println(...) branch
-- is dropped from the residual, never even inspected further.
ioModule :: Expr
ioModule =
  -- let cfg = 10 * 10
  --     x = input
  --   in
  --     if cfg < 1000 then x + cfg else println $ x + 1
  Let "cfg" (BinOp Mul (Lit 10) (Lit 10)) $
    Let "x" (Dynamic "input" []) $
      If
        (BinOp Lt (Var "cfg") (Lit 1000))
        (BinOp Add (Var "x") (Var "cfg"))
        (Dynamic "println" [BinOp Add (Var "x") (Lit 1)])

-- ============================================================
-- Runner
-- ============================================================

runAndReport :: Int -> Module -> IO ()
runAndReport fuel m = do
  let (residual, remaining) = runSpecialize fuel (modBody m)
      spent = fuel - remaining
  printf
    "-- %-18s fuel budget=%-3d spent=%-3d remaining=%-3d\n"
    (modName m)
    fuel
    spent
    remaining
  printf "   residual: %s\n\n" (pretty residual)

main :: IO ()
main = do
  let modules =
        [ Module "cheap_config" cheapConfig,
          Module "expensive_static" expensiveStatic,
          Module "io_module" ioModule
        ]

  putStrLn "=== Flat fuel budget (10 units each) ==="
  putStrLn ""
  mapM_ (runAndReport 10) modules

  putStrLn "=== Adaptive allocation from a shared pool (40 units total) ==="
  putStrLn ""
  let allocation = allocateFuel 40 modules
  forM_ allocation $ \(m, f) ->
    printf
      "  %-18s estimated static work=%-4d -> allocated fuel=%d\n"
      (modName m)
      (estimateStaticWork (modBody m))
      f
  putStrLn ""
  mapM_ (uncurry runAndReport) [(f, m) | (m, f) <- allocation]
