-- RC Insertion Pass over ANF IR
--
-- Takes an ANF program (as produced by a lambda lifter) and inserts
-- reference-counting operations according to these rules:
--
--   1. let x = Ctor fields  -> alloc, incref each field, assign
--   2. let x = f args       -> incref each arg before call
--   3. case x of Ctor a b   -> incref each bound field
--   4. function exit        -> decref params not returned/stored
--   5. dead let-bindings    -> immediate decref-and-free
--
-- No move semantics — Rc does everything. Every reference is either
-- increffed (new ownership) or decreffed (dropped ownership).
-- The output is a simple imperative RC-annotated IR that could be
-- straightforwardly lowered to C.

module Rc where

import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Input: ANF IR (post lambda-lift)
-- Extended with constructors, case expressions, and primitives.
--------------------------------------------------------------------------------

type Name = String

-- Atoms are always trivially cheap (no allocation)
data Atom
  = AVar Name
  | ALit Int -- primitive int: unboxed, never RC'd
  | AStr String -- SString (<=32 bytes): copied, never RC'd
  deriving (Eq, Ord)

instance Show Atom where
  show (AVar x) = x
  show (ALit n) = show n
  show (AStr s) = show s

isPrimitive :: Atom -> Bool
isPrimitive (ALit _) = True
isPrimitive (AStr _) = True
isPrimitive _ = False

-- ANF expressions
data AExpr
  = AAtom Atom
  | AApp Name [Atom] -- function call
  | ACtor Name [Atom] -- constructor application: Cons x y
  | ACase Atom [ABranch] -- case scrutinee of alts
  | ALet Name AExpr AExpr -- let x = e in body
  deriving (Eq)

data ABranch
  = ABranch Name [Name] AExpr -- Ctor bound_vars -> body
  deriving (Eq)

-- Top-level function (already lambda-lifted: no free vars)
data FunDef = FunDef
  { fnName :: Name,
    fnParams :: [Name],
    fnBody :: AExpr
  }
  deriving (Eq)

--------------------------------------------------------------------------------
-- Output: RC-annotated IR
-- A flat sequence of statements, easily lowered to C.
--------------------------------------------------------------------------------

data RStmt
  = RDecl Name RExpr -- Name *x = <expr>;  (declaration + assign)
  | RIncref Name -- incref(x);
  | RDecref Name -- decref(x);
  | RFree Name -- free(x);  (after rc reaches 0)
  | RCase Name [RBranch] -- switch on x's tag
  | RReturn RExpr -- return <expr>;
  deriving (Eq)

data RExpr
  = RAtom Atom
  | RAlloc Name [Atom] -- alloc_ctor("Ctor", fields...)
  | RCall Name [Atom] -- f(args...)
  deriving (Eq)

data RBranch = RBranch Name [Name] [RStmt]
  deriving (Eq)

-- A complete RC-annotated function
data RFunDef = RFunDef
  { rfName :: Name,
    rfParams :: [Name],
    rfBody :: [RStmt]
  }
  deriving (Eq)

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

indent :: Int -> String -> String
indent n s = replicate n ' ' ++ s

ppStmt :: Int -> RStmt -> String
ppStmt d (RDecl x e) = indent d $ "val " ++ x ++ " = " ++ ppExpr e ++ ";"
ppStmt d (RIncref x) = indent d $ "incref(" ++ x ++ ");"
ppStmt d (RDecref x) = indent d $ "decref(" ++ x ++ ");"
ppStmt d (RFree x) = indent d $ "free(" ++ x ++ ");"
ppStmt d (RReturn e) = indent d $ "return " ++ ppExpr e ++ ";"
ppStmt d (RCase x brs) =
  indent d ("case " ++ x ++ " of\n")
    ++ concatMap (ppBranch (d + 2)) brs

ppBranch :: Int -> RBranch -> String
ppBranch d (RBranch ctor fields stmts) =
  indent d (ctor ++ " " ++ unwords fields ++ " ->\n")
    ++ concatMap (\s -> ppStmt (d + 2) s ++ "\n") stmts

ppExpr :: RExpr -> String
ppExpr (RAtom a) = show a
ppExpr (RAlloc c args) = "alloc(" ++ c ++ ", " ++ intercalate ", " (map show args) ++ ")"
ppExpr (RCall f args) = f ++ "(" ++ intercalate ", " (map show args) ++ ")"

ppFun :: RFunDef -> String
ppFun (RFunDef name params body) =
  "fun "
    ++ name
    ++ "("
    ++ intercalate ", " params
    ++ ") {\n"
    ++ concatMap (\s -> ppStmt 2 s ++ "\n") body
    ++ "}"

--------------------------------------------------------------------------------
-- Liveness / use analysis on ANF
-- We need to know: which names are used in a subexpression?
-- A name whose last use is in a particular spot can be decreffed there.
--------------------------------------------------------------------------------

-- All names mentioned in an atom
atomUses :: Atom -> Set Name
atomUses (AVar x) = Set.singleton x
atomUses _ = Set.empty

-- All names used (free) in an AExpr
usesOf :: AExpr -> Set Name
usesOf (AAtom a) = atomUses a
usesOf (AApp _ args) = Set.unions (map atomUses args)
usesOf (ACtor _ args) = Set.unions (map atomUses args)
usesOf (ALet x e b) = usesOf e `Set.union` Set.delete x (usesOf b)
usesOf (ACase a brs) = atomUses a `Set.union` Set.unions (map brUses brs)
  where
    brUses (ABranch _ fields body) = usesOf body Set.\\ Set.fromList fields

-- Names live in the *continuation* after a let-binding
-- i.e. what does the body use?
liveInCont :: AExpr -> Set Name
liveInCont = usesOf

--------------------------------------------------------------------------------
-- RC Insertion
-- We walk the ANF expression, threading the set of names that are
-- currently "live" (have been declared and not yet decreffed).
-- At each point we emit the appropriate RC operations.
--------------------------------------------------------------------------------

type Live = Set Name -- names currently holding a live reference

-- Insert RC for a complete function definition
insertFun :: FunDef -> RFunDef
insertFun (FunDef name params body) =
  let live0 = Set.fromList params
      (stmts, ret) = insertExpr live0 params body
      -- params not mentioned in the return value need a final decref
      retUses = case ret of
        RAtom (AVar x) -> Set.singleton x
        _ -> Set.empty
      unusedParams = Set.fromList params Set.\\ retUses Set.\\ usedInStmts stmts
      finalDecrefs = map RDecref (Set.toList unusedParams)
   in RFunDef name params (stmts ++ finalDecrefs ++ [RReturn ret])

-- Very rough approximation: which names are decreffed or used in stmts
-- (used to avoid double-decref of params that were already handled)
usedInStmts :: [RStmt] -> Set Name
usedInStmts = Set.unions . map go
  where
    go (RDecl x _) = Set.singleton x
    go (RDecref x) = Set.singleton x
    go (RIncref x) = Set.singleton x
    go (RFree x) = Set.singleton x
    go (RCase x brs) = Set.singleton x
    go (RReturn _) = Set.empty

-- Insert RC for an expression.
-- Returns (statements-before-return, return-expression)
insertExpr :: Live -> [Name] -> AExpr -> ([RStmt], RExpr)
-- Base case: just an atom, nothing to emit
insertExpr _live _params (AAtom a) = ([], RAtom a)
-- Constructor: alloc + incref each non-primitive field
insertExpr _live _params (ACtor ctor args) =
  let increfs = [RIncref x | AVar x <- args] -- only heap values
   in (increfs, RAlloc ctor args)
-- Function call: incref each non-primitive arg before call
insertExpr _live _params (AApp f args) =
  let increfs = [RIncref x | AVar x <- args]
   in (increfs, RCall f args)
-- Let binding: the interesting case
insertExpr live params (ALet x rhs body) =
  let -- What does the body use? (for dead-binding detection)
      bodyUses = usesOf body
      isDead = not (Set.member x bodyUses)

      -- RC ops for the RHS
      (rhsStmts, rhsExpr) = insertExpr live params rhs
      declStmt = RDecl x rhsExpr

      -- If x is dead (never used in body), immediately free it
      deadStmts =
        if isDead
          then case rhs of
            ACtor _ args ->
              -- decref each field before freeing the allocation
              [RDecref f | AVar f <- args] ++ [RFree x]
            _ ->
              [RDecref x]
          else []

      -- Continue with body
      live' = if isDead then live else Set.insert x live
      (bodyStmts, retEx) = insertExpr live' params body

      allStmts = rhsStmts ++ [declStmt] ++ deadStmts ++ bodyStmts
   in (allStmts, retEx)
-- Case expression: incref each field bound in the matching branch
insertExpr live params (ACase scrutinee branches) =
  let scrutName = case scrutinee of AVar x -> x; _ -> "_lit"
      rBranches = map (insertBranch live params) branches
      caseStmt = RCase scrutName rBranches
   in ([caseStmt], RAtom (AVar "_case_result"))

-- Note: in a real impl the branches would each set a result slot;
-- here we simplify and emit the case as a single statement block.

insertBranch :: Live -> [Name] -> ABranch -> RBranch
insertBranch live params (ABranch ctor fields body) =
  let -- incref each bound field (pattern match creates new references)
      increfs = [RIncref f | f <- fields]
      live' = live `Set.union` Set.fromList fields
      (stmts, retEx) = insertExpr live' params body
      retStmt = RReturn retEx
      -- fields not used in body should be decreffed
      bodyUses = usesOf body
      unusedFields = filter (\f -> not (Set.member f bodyUses)) fields
      cleanupDecrefs = map RDecref unusedFields
   in RBranch ctor fields (increfs ++ stmts ++ cleanupDecrefs ++ [retStmt])

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

-- Helper to print an example
printExample :: String -> FunDef -> IO ()
printExample label fn = do
  putStrLn $ replicate 60 '='
  putStrLn $ "EXAMPLE: " ++ label
  putStrLn $ replicate 60 '-'
  putStrLn $
    "Source function: "
      ++ fnName fn
      ++ "("
      ++ intercalate ", " (fnParams fn)
      ++ ")"
  putStrLn $ replicate 60 '-'
  putStrLn "RC-annotated output:"
  putStrLn $ ppFun (insertFun fn)
  putStrLn ""

main :: IO ()
main = do
  -- 1. Identity: f x = x
  --    x is returned so no decref needed
  printExample "identity: f x = x" $
    FunDef
      "f"
      ["x"]
      (AAtom (AVar "x"))

  -- 2. Constant: f x = 42
  --    x is not returned → decref x at exit
  printExample "constant: f x = 42" $
    FunDef
      "f"
      ["x"]
      (AAtom (ALit 42))

  -- 3. Constructor: f x y = Cons x y
  --    Cons increfs x and y (fields), both params are stored → no exit decref
  printExample "constructor: f x y = Cons x y" $
    FunDef
      "f"
      ["x", "y"]
      (ACtor "Cons" [AVar "x", AVar "y"])

  -- 4. Constructor, param unused: f x = Cons 1 2
  --    All fields are primitives (no incref), x is not used → decref x
  printExample "constructor with primitives: f x = Cons 1 2" $
    FunDef
      "f"
      ["x"]
      (ACtor "Cons" [ALit 1, ALit 2])

  -- 5. Function call with args: f x y = g x y
  --    incref x and y before call, both params consumed by call
  printExample "call: f x y = g x y" $
    FunDef
      "f"
      ["x", "y"]
      (AApp "g" [AVar "x", AVar "y"])

  -- 6. Dead let: f x = let _ = Cons x Nil in Nil
  --    The Cons is immediately dead: decref x (field) + free the allocation
  printExample "dead let: f x = let _ = Cons x Nil in Nil" $
    FunDef
      "f"
      ["x"]
      ( ALet
          "_"
          (ACtor "Cons" [AVar "x", AVar "nil"])
          (AAtom (AVar "nil"))
      )

  -- 7. Live let chain: f x = let a = wrap x in let b = wrap a in b
  --    x incref'd into a, a incref'd into b, b returned
  --    x is now only held by a; a only by b — both correctly kept alive
  printExample "let chain: f x = let a = wrap x in let b = wrap a in b" $
    FunDef
      "f"
      ["x"]
      ( ALet
          "a"
          (ACtor "Box" [AVar "x"])
          ( ALet
              "b"
              (ACtor "Box" [AVar "a"])
              (AAtom (AVar "b"))
          )
      )

  -- 8. Call then return different: f x y = let r = g x in y
  --    x incref'd for call to g, r is dead (not returned) → decref r
  --    y is returned, x consumed by call, y not consumed → decref handled
  printExample "call + return other: f x y = let r = g x in y" $
    FunDef
      "f"
      ["x", "y"]
      ( ALet
          "r"
          (AApp "g" [AVar "x"])
          (AAtom (AVar "y"))
      )

  -- 9. Pattern match: f xs = case xs of { Cons h t -> h | Nil -> 0 }
  --    Matching Cons increfs h and t; h is returned, t unused → decref t
  printExample "case: f xs = case xs of Cons h t -> h; Nil -> 0" $
    FunDef
      "f"
      ["xs"]
      ( ACase
          (AVar "xs")
          [ ABranch
              "Cons"
              ["h", "t"]
              (AAtom (AVar "h")),
            ABranch
              "Nil"
              []
              (AAtom (ALit 0))
          ]
      )

  -- 10. Realistic: map step — f func xs = case xs of
  --       Nil       -> Nil
  --       Cons h t  -> let h' = func h
  --                        t' = f func t
  --                    in Cons h' t'
  printExample "map step: f func xs = case xs of ..." $
    FunDef
      "f"
      ["func", "xs"]
      ( ACase
          (AVar "xs")
          [ ABranch
              "Nil"
              []
              (ACtor "Nil" []),
            ABranch
              "Cons"
              ["h", "t"]
              ( ALet
                  "h'"
                  (AApp "func" [AVar "h"])
                  ( ALet
                      "t'"
                      (AApp "f" [AVar "func", AVar "t"])
                      (ACtor "Cons" [AVar "h'", AVar "t'"])
                  )
              )
          ]
      )
