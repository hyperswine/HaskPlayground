{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module FPipeline where

import Control.Monad.State
import Data.List (intercalate, nub)

-- ============================================================
-- SECTION 1: Source AST
-- ============================================================
-- This is what the parser would produce. Rich, nested, sugared.

data Expr
  = Var String
  | Lit Int
  | App Expr Expr -- f x
  | Lam [String] Expr -- λ params → body
  | Let String Expr Expr -- let name = rhs in body
  | Pipe Expr Expr -- x |> f
  | BinOp String Expr Expr -- x + y (sugared)
  | Case Expr [(Pat, Expr)] -- case e of { pat → e }
  deriving (Show)

data Pat
  = PVar String
  | PTuple [String]
  | PLit Int
  deriving (Show)

-- Top-level definition
data Def = Def String [String] Expr
  deriving (Show)

-- ============================================================
-- SECTION 2: Lambda Lifting
-- ============================================================
-- All local lets/lambdas become top-level functions.
-- Free variables become extra leading parameters.
-- Names: use the binding name if available, else "lambda",
-- with a global counter suffix: lambda.f'01, lambda.f'02 etc.

type LiftState = State (Int, [Def])

freshName :: String -> LiftState String
freshName hint = do
  (n, defs) <- get
  put (n + 1, defs)
  let suffix = if n < 10 then "0" ++ show n else show n
  return $ "lambda." ++ hint ++ suffix

emitDef :: Def -> LiftState ()
emitDef d = modify (\(n, ds) -> (n, ds ++ [d]))

-- Collect free variables in an expression given a set of bound names
freeVars :: [String] -> Expr -> [String]
freeVars bound expr = nub $ go bound expr
  where
    go b (Var x) = [x | x `notElem` b]
    go _ (Lit _) = []
    go b (App f x) = go b f ++ go b x
    go b (Lam ps e) = go (b ++ ps) e
    go b (Let n r body) = go b r ++ go (n : b) body
    go b (Pipe x f) = go b x ++ go b f
    go b (BinOp _ l r) = go b l ++ go b r
    go b (Case e alts) = go b e ++ concatMap (goAlt b) alts
    goAlt b (PVar v, e) = go (v : b) e
    goAlt b (PTuple vs, e) = go (b ++ vs) e
    goAlt b (PLit _, e) = go b e

-- Lift a single expression. Returns a (possibly simplified) expression
-- and emits lifted top-level Defs into state.
liftExpr :: [String] -> Expr -> LiftState Expr
liftExpr bound (Let name rhs body) = do
  -- Free vars of rhs = things it uses that aren't its own params
  -- (captured from enclosing scope, must become explicit params)
  let (params, innerBody) = case rhs of
        Lam ps e -> (ps, e)
        _ -> ([], rhs)
  let free = freeVars params innerBody
  liftedName <- freshName name
  let allParams = free ++ params
  innerBody' <- liftExpr (bound ++ allParams) innerBody
  emitDef (Def liftedName allParams innerBody')
  let callSite = foldl App (Var liftedName) (map Var free)
  body' <- liftExpr (name : bound) (substExpr name callSite body)
  return body'
liftExpr bound (Lam params body) = do
  -- Free vars = things used in body that aren't this lambda's own params
  -- These come from the enclosing scope and must become explicit params
  let free = freeVars params body
  liftedName <- freshName "anon"
  let allParams = free ++ params
  -- Lift body treating all (free captured + original params) as bound
  body' <- liftExpr (bound ++ allParams) body
  emitDef (Def liftedName allParams body')
  return $ foldl App (Var liftedName) (map Var free)
liftExpr bound (App f x) = App <$> liftExpr bound f <*> liftExpr bound x
liftExpr bound (Pipe x f) = Pipe <$> liftExpr bound x <*> liftExpr bound f
liftExpr bound (BinOp op l r) = BinOp op <$> liftExpr bound l <*> liftExpr bound r
liftExpr bound (Case e alts) = do
  e' <- liftExpr bound e
  alts' <- mapM (liftAlt bound) alts
  return $ Case e' alts'
liftExpr _ e = return e

liftAlt :: [String] -> (Pat, Expr) -> LiftState (Pat, Expr)
liftAlt bound (p, e) = (p,) <$> liftExpr (bound ++ patVars p) e

patVars :: Pat -> [String]
patVars (PVar v) = [v]
patVars (PTuple vs) = vs
patVars (PLit _) = []

substExpr :: String -> Expr -> Expr -> Expr
substExpr n repl (Var x) = if x == n then repl else Var x
substExpr n repl (App f x) = App (substExpr n repl f) (substExpr n repl x)
substExpr n repl (Lam ps e)
  | n `elem` ps = Lam ps e
  | otherwise = Lam ps (substExpr n repl e)
substExpr n repl (Let x r b)
  | x == n = Let x (substExpr n repl r) b
  | otherwise = Let x (substExpr n repl r) (substExpr n repl b)
substExpr n repl (Pipe x f) = Pipe (substExpr n repl x) (substExpr n repl f)
substExpr n repl (BinOp op l r) = BinOp op (substExpr n repl l) (substExpr n repl r)
substExpr n repl (Case e alts) = Case (substExpr n repl e) (map go alts)
  where
    go (p, ae) =
      if n `elem` patVars p
        then (p, ae)
        else (p, substExpr n repl ae)
substExpr _ _ e = e

liftDef :: Def -> LiftState Def
liftDef (Def name params body) = do
  body' <- liftExpr params body
  return $ Def name params body'

liftProgram :: [Def] -> [Def]
liftProgram defs =
  let (lifted, (_, emitted)) = runState (mapM liftDef defs) (0, [])
   in emitted ++ lifted

-- ============================================================
-- SECTION 3: Desugaring
-- ============================================================
-- Pipe: x |> f  →  f x
-- BinOp: x + y  →  add x y  (operators are just functions)

desugarExpr :: Expr -> Expr
desugarExpr (Pipe x f) = App (desugarExpr f) (desugarExpr x)
desugarExpr (BinOp op l r) = App (App (Var op) (desugarExpr l)) (desugarExpr r)
desugarExpr (App f x) = App (desugarExpr f) (desugarExpr x)
desugarExpr (Lam ps e) = Lam ps (desugarExpr e)
desugarExpr (Let n r b) = Let n (desugarExpr r) (desugarExpr b)
desugarExpr (Case e alts) = Case (desugarExpr e) (map (\(p, a) -> (p, desugarExpr a)) alts)
desugarExpr e = e

desugarDef :: Def -> Def
desugarDef (Def n ps e) = Def n ps (desugarExpr e)

desugarProgram :: [Def] -> [Def]
desugarProgram = map desugarDef

-- ============================================================
-- SECTION 4: ANF Transformation
-- ============================================================
-- Every nested call gets a fresh name.
-- Result: linear sequence of single-call let bindings.

type ANFState = State Int

freshANF :: ANFState String
freshANF = do
  n <- get
  put (n + 1)
  return $ "anf$" ++ show n

-- anf expr k: transform expr, pass the result name to continuation k
anfExpr :: Expr -> (Expr -> ANFState Expr) -> ANFState Expr
anfExpr (App f x) k = do
  anfExpr f $ \f' ->
    anfExpr x $ \x' -> do
      tmp <- freshANF
      body <- k (Var tmp)
      return $ Let tmp (App f' x') body
anfExpr (Lit n) k = k (Lit n)
anfExpr (Var x) k = k (Var x)
anfExpr (Case e alts) k = do
  anfExpr e $ \e' -> do
    alts' <- mapM (\(p, a) -> (p,) <$> anfExpr a k) alts
    return $ Case e' alts'
anfExpr (Let n r b) k = do
  anfExpr r $ \r' -> do
    b' <- anfExpr b k
    return $ Let n r' b'
anfExpr e k = k e -- Lam, etc — shouldn't appear after lifting but handle gracefully

anfDef :: Def -> ANFState Def
anfDef (Def n ps body) = do
  body' <- anfExpr body return
  return $ Def n ps body'

anfProgram :: [Def] -> [Def]
anfProgram defs = evalState (mapM anfDef defs) 0

-- ============================================================
-- SECTION 5: Pretty Printing
-- ============================================================

ppExpr :: Expr -> String
ppExpr (Var x) = x
ppExpr (Lit n) = show n
ppExpr (App f x) = ppExpr f ++ " " ++ ppArg x
ppExpr (Lam ps e) = "λ" ++ unwords ps ++ " → " ++ ppExpr e
ppExpr (Pipe x f) = ppExpr x ++ " |> " ++ ppExpr f
ppExpr (BinOp op l r) = ppExpr l ++ " " ++ op ++ " " ++ ppExpr r
ppExpr (Let n r b) = "\n    " ++ n ++ " = " ++ ppExpr r ++ ppLetBody b
ppExpr (Case e alts) = "case " ++ ppExpr e ++ " of" ++ concatMap ppAlt alts

ppLetBody :: Expr -> String
ppLetBody (Let n r b) = "\n    " ++ n ++ " = " ++ ppExpr r ++ ppLetBody b
ppLetBody e = "\n    " ++ ppExpr e

ppArg :: Expr -> String
ppArg e@(App _ _) = "(" ++ ppExpr e ++ ")"
ppArg e = ppExpr e

ppAlt :: (Pat, Expr) -> String
ppAlt (p, e) = "\n      " ++ ppPat p ++ " → " ++ ppExpr e

ppPat :: Pat -> String
ppPat (PVar v) = v
ppPat (PTuple vs) = "(" ++ intercalate ", " vs ++ ")"
ppPat (PLit n) = show n

ppDef :: Def -> String
ppDef (Def name params body) =
  name
    ++ (if null params then "" else " " ++ unwords params)
    ++ " ="
    ++ case body of
      Let {} -> ppExpr body
      _ -> " " ++ ppExpr body

ppProgram :: [Def] -> String
ppProgram = intercalate "\n" . map ppDef

-- ============================================================
-- SECTION 6: Example Programs
-- ============================================================

-- Example 1: local function with captured vars
--   f a b =
--     let f' x = a + b + x
--     in f' b
example1 :: [Def]
example1 =
  [ Def
      "f"
      ["a", "b"]
      ( Let
          "f'"
          (Lam ["x"] (BinOp "+" (BinOp "+" (Var "a") (Var "b")) (Var "x")))
          (App (Var "f'") (Var "b"))
      )
  ]

-- Example 2: pipeline with operator sugar
--   g x = x |> (* 2) |> (+ 10)
example2 :: [Def]
example2 =
  [ Def
      "g"
      ["x"]
      ( Pipe
          (Pipe (Var "x") (App (Var "*") (Lit 2)))
          (App (Var "+") (Lit 10))
      )
  ]

-- Example 3: nested lambdas
--   h = λx → (λy → y + x)
example3 :: [Def]
example3 =
  [ Def
      "h"
      []
      ( Lam
          ["x"]
          ( Lam
              ["y"]
              (BinOp "+" (Var "y") (Var "x"))
          )
      )
  ]

-- Example 4: pattern matching
--   fst p = case p of (x, y) → x
--   addPair p = case p of (x, y) → x + y
example4 :: [Def]
example4 =
  [ Def
      "fst'"
      ["p"]
      (Case (Var "p") [(PTuple ["x", "y"], Var "x")]),
    Def
      "addPair"
      ["p"]
      (Case (Var "p") [(PTuple ["x", "y"], BinOp "+" (Var "x") (Var "y"))])
  ]

-- ============================================================
-- SECTION 7: Main — show each pass
-- ============================================================

banner :: String -> IO ()
banner s = do
  putStrLn ""
  putStrLn $ replicate 60 '='
  putStrLn $ "  " ++ s
  putStrLn $ replicate 60 '='

showPass :: String -> [Def] -> IO ()
showPass label defs = do
  putStrLn $ "\n-- " ++ label ++ " --"
  putStrLn $ ppProgram defs

runPipeline :: String -> [Def] -> IO ()
runPipeline name defs = do
  banner name
  showPass "Source" defs
  let lifted = liftProgram defs
  showPass "After Lambda Lifting" lifted
  let desugared = desugarProgram lifted
  showPass "After Desugaring (pipes, operators)" desugared
  let anf = anfProgram desugared
  showPass "After ANF" anf

main :: IO ()
main = do
  runPipeline "Example 1: Local function capturing outer vars" example1
  runPipeline "Example 2: Pipeline with operator sugar" example2
  runPipeline "Example 3: Nested lambdas" example3
  runPipeline "Example 4: Pattern matching" example4

-- Debug: trace free var computation
debugFree :: IO ()
debugFree = do
  let innerLam = Lam ["y"] (BinOp "+" (Var "y") (Var "x"))
  let outerLam = Lam ["x"] innerLam
  putStrLn $ "Free vars of outer lam (bound=[]): " ++ show (freeVars [] outerLam)
  putStrLn $ "Free vars of inner lam (bound=[]): " ++ show (freeVars [] innerLam)
  putStrLn $ "Free vars of inner lam (bound=[x]): " ++ show (freeVars ["x"] innerLam)
