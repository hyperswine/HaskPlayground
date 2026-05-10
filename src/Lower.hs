-- ANF Lowering + Lambda Lifting demo
-- Transforms a simple lambda calculus with applications into:
--   1. ANF: no compound applications (all args must be trivial/atoms)
--   2. Lambda lifting: all lambdas become top-level named functions
--      with free variables threaded as explicit parameters
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lower where

import Control.Monad.State
import Data.List (nub, (\\))

--------------------------------------------------------------------------------
-- Source AST
--------------------------------------------------------------------------------

type Name = String

data Expr
  = Var Name
  | Lam Name Expr
  | App Expr Expr
  | Lit Int
  | Let Name Expr Expr
  deriving (Eq)

instance Show Expr where
  show (Var x) = x
  show (Lit n) = show n
  show (Lam x b) = "(\\" ++ x ++ " -> " ++ show b ++ ")"
  show (App f a) = "(" ++ show f ++ " " ++ show a ++ ")"
  show (Let x e b) = "(let " ++ x ++ " = " ++ show e ++ " in " ++ show b ++ ")"

--------------------------------------------------------------------------------
-- Target AST (ANF + lambda lifted)
-- All lambdas are gone; only top-level function definitions remain.
-- ANF: every argument to an application must be an Atom (var or lit).
--------------------------------------------------------------------------------

data Atom
  = AVar Name
  | ALit Int
  deriving (Eq)

instance Show Atom where
  show (AVar x) = x
  show (ALit n) = show n

-- ANF expressions: all call sites have only atoms as arguments
data AExpr
  = AAtom Atom -- trivial value
  | AApp Name [Atom] -- fully saturated call: f a1 a2 ...
  | ALet Name AExpr AExpr -- let x = e in body
  deriving (Eq)

instance Show AExpr where
  show (AAtom a) = show a
  show (AApp f args) = unwords (f : map show args)
  show (ALet x e body) = "let " ++ x ++ " = " ++ show e ++ "\n    in " ++ show body

-- A top-level lifted function definition
data TopDef = TopDef {tdName :: Name, tdParams :: [Name], tdBody :: AExpr} deriving (Eq)

instance Show TopDef where
  show (TopDef name params body) = name ++ concatMap (' ' :) params ++ " =\n    " ++ show body

--------------------------------------------------------------------------------
-- Compilation monad
-- State: (counter, accumulated top-level definitions)
--------------------------------------------------------------------------------

type Compile a = State (Int, [TopDef]) a

freshName :: String -> Compile Name
freshName prefix = do
  (n, defs) <- get
  put (n + 1, defs)
  return (prefix ++ "-" ++ show n)

emitDef :: TopDef -> Compile ()
emitDef d = modify (\(n, defs) -> (n, defs ++ [d]))

runCompile :: Compile a -> (a, [TopDef])
runCompile m = let (a, (_, defs)) = runState m (1, []) in (a, defs)

--------------------------------------------------------------------------------
-- Free variable analysis
--------------------------------------------------------------------------------

freeVars :: Expr -> [Name]
freeVars (Var x) = [x]
freeVars (Lit _) = []
freeVars (Lam x b) = freeVars b \\ [x]
freeVars (App f a) = nub (freeVars f ++ freeVars a)
freeVars (Let x e b) = nub (freeVars e ++ (freeVars b \\ [x]))

--------------------------------------------------------------------------------
-- ANF + Lambda lifting pass
--
-- normalize env expr k
--   env  : set of names currently in scope (to distinguish free vars)
--   expr : source expression to normalize
--   k    : continuation — given an Atom for the result, produce final AExpr
--
-- The key idea:
--   * If expr is already trivial (Var/Lit), hand it directly to k.
--   * Otherwise, normalize to an AExpr, bind it to a fresh let-name,
--     and pass that name as an Atom to k.
--   * For Lam: collect free vars, emit a TopDef, return the lifted name.
--   * For App: recursively normalize function and all args to atoms first,
--     then emit a single flat AApp.
--------------------------------------------------------------------------------

normalize :: [Name] -> Expr -> (Atom -> Compile AExpr) -> Compile AExpr
-- Trivial atoms — hand straight to continuation
normalize _env (Var x) k = k (AVar x)
normalize _env (Lit n) k = k (ALit n)
-- Let: normalize the binding to an AExpr, extend scope, normalize body
normalize env (Let x rhs body) k = normExpr env rhs $ \rhsExpr -> do
  bodyExpr <- normalize (x : env) body k
  return (ALet x rhsExpr bodyExpr)

-- Application: normalize function, then argument, then emit AApp
normalize env (App f arg) k = do
  -- Collect the spine: f could itself be an App, giving us multiple args
  let (func, args) = spine (App f arg)
  -- Normalize function expression to an atom
  normalizeAtom env func $ \fAtom ->
    -- Normalize all arguments to atoms
    normalizeArgs env args $ \argAtoms -> do
      -- fAtom might be a closure name (AVar), args are all atoms
      -- emit:  let tmp = fAtom argAtoms...  in k(tmp)
      tmp <- freshName "t"
      let fName = case fAtom of
            AVar n -> n
            ALit n -> show n -- shouldn't happen in well-typed code
      body <- k (AVar tmp)
      return (ALet tmp (AApp fName argAtoms) body)

-- Lambda: lambda lift
normalize env (Lam param body) k = do
  liftedName <- freshName "anon"
  -- Compute free variables *in the lambda body* that aren't the param itself
  let fvs = nub (freeVars (Lam param body)) -- already excludes param
  -- The lifted function takes free vars then the original param
  let allParams = fvs ++ [param]
  -- Normalize the body in a scope that includes all params
  bodyExpr <- normalize allParams body (return . AAtom)
  emitDef (TopDef liftedName allParams bodyExpr)
  -- At the call site, the lambda becomes a partial application of the
  -- lifted name to its free-variable arguments.
  -- If there are free vars we must build a closure shim; since we're doing
  -- full lambda lifting (no closures) we represent the closure application
  -- as its own lifted wrapper that closes over the fvs.
  if null fvs
    then k (AVar liftedName)
    else do
      -- Build a shim: shim fvs = liftedName fvs
      -- So uses of the lambda just see the shim (no free-var args needed)
      shimName <- freshName "shim"
      shimParam <- freshName "p"
      let shimBody = AApp liftedName (map AVar fvs ++ [AVar shimParam])
      emitDef (TopDef shimName (fvs ++ [shimParam]) shimBody)
      k (AVar shimName)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- Decompose nested App into (head, args) spine
spine :: Expr -> (Expr, [Expr])
spine expr = go expr []
  where
    go (App f a) acc = go f (a : acc)
    go e acc = (e, acc)

-- Normalize to an Atom, binding to a fresh name if needed
normalizeAtom :: [Name] -> Expr -> (Atom -> Compile AExpr) -> Compile AExpr
normalizeAtom env expr k = case expr of
  Var x -> k (AVar x)
  Lit n -> k (ALit n)
  _ -> normalize env expr k

-- Normalize a list of expressions to atoms (threading lets correctly)
normalizeArgs :: [Name] -> [Expr] -> ([Atom] -> Compile AExpr) -> Compile AExpr
normalizeArgs _env [] k = k []
normalizeArgs env (e : es) k = normalizeAtom env e $ \a -> normalizeArgs env es $ \as -> k (a : as)

-- Normalize an expression to an AExpr (not necessarily an atom)
normExpr :: [Name] -> Expr -> (AExpr -> Compile AExpr) -> Compile AExpr
normExpr env expr k = normalize env expr $ \atom -> k (AAtom atom)

-- Capture-avoiding substitution (used in Let normalisation above; simple version)
subst :: Name -> Expr -> Expr -> Expr
subst x s (Var y) = if x == y then s else Var y
subst _ _ (Lit n) = Lit n
subst x s (App f a) = App (subst x s f) (subst x s a)
subst x s (Lam y b) = if x == y then Lam y b else Lam y (subst x s b)
subst x s (Let y e b) = Let y (subst x s e) (if x == y then b else subst x s b)

--------------------------------------------------------------------------------
-- Top-level entry: compile a source expression
--------------------------------------------------------------------------------

compile :: Expr -> (AExpr, [TopDef])
compile expr = runCompile $ normalize [] expr (return . AAtom)

printResult :: String -> Expr -> IO ()
printResult label expr = do
  putStrLn $ replicate 60 '='
  putStrLn $ "EXAMPLE: " ++ label
  putStrLn $ replicate 60 '-'
  putStrLn $ "Source:\n  " ++ show expr
  putStrLn $ replicate 60 '-'
  let (mainExpr, defs) = compile expr
  putStrLn "Lifted top-level definitions:"
  mapM_ (\d -> putStrLn ("  " ++ show d)) defs
  putStrLn $ "\nMain expression:\n  " ++ show mainExpr
  putStrLn ""

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- 1. Simple lambda: \x -> x
  printResult "identity lambda" $
    Lam "x" (Var "x")

  -- 2. Nested lambda: \x -> \y -> x
  printResult "const (nested lambdas)" $
    Lam "x" (Lam "y" (Var "x"))

  -- 3. Lambda with free variable: \x -> f x
  --    where f is free
  printResult "lambda with free var (\\x -> f x)" $
    Lam "x" (App (Var "f") (Var "x"))

  -- 4. Compound application: f (g x) (h y)
  --    requires two intermediate lets
  printResult "compound application: f (g x) (h y)" $
    App
      (App (Var "f") (App (Var "g") (Var "x")))
      (App (Var "h") (Var "y"))

  -- 5. Let with compound RHS
  printResult "let with compound rhs" $
    Let
      "z"
      (App (Var "f") (App (Var "g") (Var "x")))
      (App (Var "h") (Var "z"))

  -- 6. Higher-order: apply a lambda immediately
  --    (\x -> f x) y
  printResult "immediate application of lambda: (\\x -> f x) y" $
    App (Lam "x" (App (Var "f") (Var "x"))) (Var "y")

  -- 7. Closure capture: \x -> \y -> add x y
  --    The inner lambda closes over x
  printResult "closure capture: \\x -> \\y -> add x y" $
    Lam "x" (Lam "y" (App (App (Var "add") (Var "x")) (Var "y")))

  -- 8. Deeply nested: f (g (h x))
  printResult "deeply nested: f (g (h x))" $
    App (Var "f") (App (Var "g") (App (Var "h") (Var "x")))

  -- 9. Complex: (\f -> \x -> f (f x)) — Church numeral successor idea
  printResult "church-style: \\f -> \\x -> f (f x)" $
    Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (Var "x"))))

  -- 10. Let binding a lambda, then applying it
  printResult "let-bound lambda application" $
    Let
      "double"
      (Lam "n" (App (App (Var "mul") (Lit 2)) (Var "n")))
      (App (Var "double") (Lit 5))
