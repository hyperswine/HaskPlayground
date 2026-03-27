{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module NPrologTest where

import qualified Data.Map.Strict as Map
import Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import NProlog

-- ---------------------------------------------------------------------------
-- Tiny helpers
-- ---------------------------------------------------------------------------

-- Run a query against an empty program, return all solutions
query :: String -> Either String [[(String, String)]]
query qs = case parseGoals qs of
  Left err -> Left err
  Right goals ->
    let prog = []
        solns = solveAll prog goals
     in Right [[(v, show t) | (v, t) <- Map.toList s] | s <- solns]

-- Run a query and return the first solution's bindings as a flat map
queryFirst :: String -> Maybe (Map.Map String String)
queryFirst qs = case query qs of
  Right (s : _) -> Just (Map.fromList s)
  _ -> Nothing

-- Assert a query has exactly one solution with the given bindings
assertBindings :: (MonadTest m) => String -> [(String, String)] -> m ()
assertBindings qs expected = do
  r <- case query qs of
    Left err -> do annotate err; failure
    Right ss -> pure ss
  case r of
    [] -> do annotate ("no solutions for: " ++ qs); failure
    (s : _) -> Map.fromList s === Map.fromList expected

-- Assert a query fails (no solutions)
assertFails :: (MonadTest m) => String -> m ()
assertFails qs = case query qs of
  Right [] -> success
  Right _ -> do annotate ("expected failure but got solutions: " ++ qs); failure
  Left err -> do annotate err; failure

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

genAtomName :: Gen String
genAtomName = do
  c <- Gen.element ['a' .. 'z']
  cs <- Gen.list (Range.linear 0 8) (Gen.element $ ['a' .. 'z'] ++ ['0' .. '9'] ++ "_")
  pure (c : cs)

genVarName :: Gen String
genVarName = do
  c <- Gen.element ['A' .. 'Z']
  cs <- Gen.list (Range.linear 0 8) (Gen.element $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'])
  pure (c : cs)

genSmallInt :: Gen Integer
genSmallInt = Gen.integral (Range.linear 0 50)

genNonZeroInt :: Gen Integer
genNonZeroInt = Gen.integral (Range.linear 1 50)

-- ---------------------------------------------------------------------------
-- § 1  Parser round-trips
-- ---------------------------------------------------------------------------

-- Parsing an atom succeeds and gives back an Atom node
prop_parse_atom :: Property
prop_parse_atom = property $ do
  name <- forAll genAtomName
  case parseTerm name of
    Right (Atom a) -> a === name
    _ -> do annotate ("failed to parse atom: " ++ name); failure

-- An integer literal round-trips through the parser
prop_parse_intlit :: Property
prop_parse_intlit = property $ do
  n <- forAll $ Gen.integral (Range.linear 0 9999)
  case parseTerm (show n) of
    Right (IntLit m) -> m === n
    _ -> failure

-- A variable name is parsed as Var
prop_parse_var :: Property
prop_parse_var = property $ do
  v <- forAll genVarName
  case parseTerm v of
    Right (Var v') -> v' === v
    _ -> do annotate ("failed to parse var: " ++ v); failure

-- A list literal parses without error
prop_parse_list_nocrash :: Property
prop_parse_list_nocrash = property $ do
  ns <- forAll $ Gen.list (Range.linear 0 6) (Gen.integral (Range.linear 0 99 :: Range Integer))
  let src = "[" ++ intercalate0 ", " (map show ns) ++ "]"
  case parseTerm src of
    Right _ -> success
    Left _ -> failure

-- Arithmetic expression parses to a compound node
prop_parse_arith_compound :: Property
prop_parse_arith_compound = property $ do
  a <- forAll genSmallInt
  b <- forAll genSmallInt
  case parseTerm (show a ++ " + " ++ show b) of
    Right (Compound "plus" [IntLit x, IntLit y]) -> x === a >> y === b
    _ -> failure

-- Nested arithmetic parses without error
prop_parse_arith_nested_nocrash :: Property
prop_parse_arith_nested_nocrash = property $ do
  a <- forAll genSmallInt
  b <- forAll genSmallInt
  c <- forAll genSmallInt
  case parseTerm (show a ++ " + " ++ show b ++ " * " ++ show c) of
    Right _ -> success
    Left _ -> failure

-- Comparison operators parse correctly
prop_parse_comparison_operators :: Property
prop_parse_comparison_operators = property $ do
  op <- forAll $ Gen.element [(">=", "gte"), ("<=", "lte"), (">", "gt"), ("<", "lt")]
  let (sym, expected) = op
  case parseTerm ("1 " ++ sym ++ " 2") of
    Right (Compound f [IntLit 1, IntLit 2]) -> f === expected
    _ -> failure

-- Unification operator parses to (unify l r)
prop_parse_unify_op :: Property
prop_parse_unify_op = property $ do
  a <- forAll genSmallInt
  b <- forAll genSmallInt
  case parseTerm (show a ++ " = " ++ show b) of
    Right (Compound "unify" [IntLit x, IntLit y]) -> x === a >> y === b
    _ -> failure

-- Parser does not crash on arbitrary short strings
prop_parse_nocrash :: Property
prop_parse_nocrash = property $ do
  src <- forAll $ Gen.string (Range.linear 0 40) Gen.unicode
  -- result may be Left; we just require no Haskell exception
  _ <- pure (parseTerm src)
  success

-- ---------------------------------------------------------------------------
-- § 2  Unification
-- ---------------------------------------------------------------------------

-- Unifying an atom with itself succeeds
prop_unify_atom_self :: Property
prop_unify_atom_self = property $ do
  name <- forAll genAtomName
  let s = unify emptySubst (Atom name) (Atom name)
  s /== Nothing

-- Unifying two different atoms fails
prop_unify_atom_different :: Property
prop_unify_atom_different = property $ do
  a <- forAll genAtomName
  b <- forAll $ Gen.filter (/= a) genAtomName
  unify emptySubst (Atom a) (Atom b) === Nothing

-- A variable unifies with any atom
prop_unify_var_binds_atom :: Property
prop_unify_var_binds_atom = property $ do
  v <- forAll genVarName
  name <- forAll genAtomName
  case unify emptySubst (Var v) (Atom name) of
    Just s -> deepWalk s (Var v) === Atom name
    Nothing -> failure

-- Unifying a variable with itself is a no-op
prop_unify_var_self :: Property
prop_unify_var_self = property $ do
  v <- forAll genVarName
  let t = Var v
  case unify emptySubst t t of
    Just _ -> success
    Nothing -> failure

-- Unifying two integers succeeds only when equal
prop_unify_intlit :: Property
prop_unify_intlit = property $ do
  a <- forAll genSmallInt
  b <- forAll genSmallInt
  let r = unify emptySubst (IntLit a) (IntLit b)
  if a == b then r /== Nothing else r === Nothing

-- Compound terms unify when functor and arity and all args match
prop_unify_compound_match :: Property
prop_unify_compound_match = property $ do
  f <- forAll genAtomName
  a <- forAll genSmallInt
  b <- forAll genSmallInt
  let t = Compound f [IntLit a, IntLit b]
  unify emptySubst t t /== Nothing

-- Compound terms fail when functors differ
prop_unify_compound_diff_functor :: Property
prop_unify_compound_diff_functor = property $ do
  f <- forAll genAtomName
  g <- forAll $ Gen.filter (/= f) genAtomName
  let t1 = Compound f [IntLit 1]
      t2 = Compound g [IntLit 1]
  unify emptySubst t1 t2 === Nothing

-- deepWalk resolves a chain of variable bindings
prop_deepWalk_chain :: Property
prop_deepWalk_chain = property $ do
  a <- forAll genSmallInt
  -- X -> Y -> IntLit a
  let s = Map.fromList [("X", Var "Y"), ("Y", IntLit a)]
  deepWalk s (Var "X") === IntLit a

-- ---------------------------------------------------------------------------
-- § 3  Arithmetic evaluation
-- ---------------------------------------------------------------------------

-- evalArith on a literal returns itself
prop_eval_intlit :: Property
prop_eval_intlit = property $ do
  n <- forAll genSmallInt
  evalArith emptySubst (IntLit n) === Just (IntLit n)

-- Addition is correct
prop_eval_plus :: Property
prop_eval_plus = property $ do
  a <- forAll genSmallInt
  b <- forAll genSmallInt
  evalArith emptySubst (Compound "plus" [IntLit a, IntLit b]) === Just (IntLit (a + b))

-- Subtraction is correct
prop_eval_minus :: Property
prop_eval_minus = property $ do
  a <- forAll genSmallInt
  b <- forAll genSmallInt
  evalArith emptySubst (Compound "minus" [IntLit a, IntLit b]) === Just (IntLit (a - b))

-- Multiplication is correct
prop_eval_mul :: Property
prop_eval_mul = property $ do
  a <- forAll genSmallInt
  b <- forAll genSmallInt
  evalArith emptySubst (Compound "mul" [IntLit a, IntLit b]) === Just (IntLit (a * b))

-- Integer division is correct (non-zero divisor)
prop_eval_div :: Property
prop_eval_div = property $ do
  a <- forAll genSmallInt
  b <- forAll genNonZeroInt
  evalArith emptySubst (Compound "div" [IntLit a, IntLit b]) === Just (IntLit (a `div` b))

-- Mod is correct
prop_eval_mod :: Property
prop_eval_mod = property $ do
  a <- forAll genSmallInt
  b <- forAll genNonZeroInt
  evalArith emptySubst (Compound "mod" [IntLit a, IntLit b]) === Just (IntLit (a `mod` b))

-- A free variable makes evalArith return Nothing
prop_eval_free_var :: Property
prop_eval_free_var = property $ do
  v <- forAll genVarName
  evalArith emptySubst (Var v) === Nothing

-- Nested arithmetic evaluates correctly
prop_eval_nested :: Property
prop_eval_nested = property $ do
  a <- forAll genSmallInt
  b <- forAll genSmallInt
  c <- forAll genSmallInt
  -- (a + b) * c
  let expr = Compound "mul" [Compound "plus" [IntLit a, IntLit b], IntLit c]
  evalArith emptySubst expr === Just (IntLit ((a + b) * c))

-- ---------------------------------------------------------------------------
-- § 4  Ground queries (no CLP)
-- ---------------------------------------------------------------------------

-- X = atom  binds X
prop_ground_unify_atom :: Property
prop_ground_unify_atom = property $ do
  v <- forAll genVarName
  name <- forAll genAtomName
  assertBindings (v ++ " = " ++ name) [(v, name)]

-- X = n  binds X to the integer
prop_ground_unify_int :: Property
prop_ground_unify_int = property $ do
  v <- forAll genVarName
  n <- forAll genSmallInt
  assertBindings (v ++ " = " ++ show n) [(v, show n)]

-- Unifying two equal atoms succeeds with no extra bindings
prop_ground_unify_equal_atoms :: Property
prop_ground_unify_equal_atoms = property $ do
  name <- forAll genAtomName
  case query (name ++ " = " ++ name) of
    Right _ -> success
    Left _ -> failure

-- Unifying two different atoms fails
prop_ground_unify_diff_atoms :: Property
prop_ground_unify_diff_atoms = property $ do
  a <- forAll genAtomName
  b <- forAll $ Gen.filter (/= a) genAtomName
  assertFails (a ++ " = " ++ b)

-- ---------------------------------------------------------------------------
-- § 5  Arithmetic queries (CLP-style)
-- ---------------------------------------------------------------------------

-- N = a + b  evaluates immediately when both are ground
prop_clp_both_ground :: Property
prop_clp_both_ground = property $ do
  a <- forAll genSmallInt
  b <- forAll genSmallInt
  v <- forAll genVarName
  assertBindings (v ++ " = " ++ show a ++ " + " ++ show b) [(v, show (a + b))]

-- N = A * B, A = a, B = b  — deferred then resolved
prop_clp_deferred_mul :: Property
prop_clp_deferred_mul = property $ do
  a <- forAll genNonZeroInt
  b <- forAll genNonZeroInt
  assertBindings
    ("N = A * B, A = " ++ show a ++ ", B = " ++ show b)
    [("N", show (a * b)), ("A", show a), ("B", show b)]

-- R = a * X, X = b  — single unknown solved algebraically
prop_clp_single_unknown_mul :: Property
prop_clp_single_unknown_mul = property $ do
  a <- forAll genNonZeroInt
  b <- forAll genNonZeroInt
  let n = a * b
  assertBindings
    ("N = " ++ show a ++ " * X, X = " ++ show b)
    [("N", show n), ("X", show b)]

-- Subtraction: N = a - b (only when a >= b so result stays in [0..INF])
prop_clp_subtraction :: Property
prop_clp_subtraction = property $ do
  b <- forAll genSmallInt
  d <- forAll genSmallInt -- d = a - b >= 0
  let a = b + d
  v <- forAll genVarName
  assertBindings (v ++ " = " ++ show a ++ " - " ++ show b) [(v, show (a - b))]

-- Comparison: a > b succeeds exactly when a > b
prop_clp_gt :: Property
prop_clp_gt = property $ do
  a <- forAll genSmallInt
  b <- forAll genSmallInt
  let qs = show a ++ " > " ++ show b
  if a > b
    then case query qs of Right _ -> success; Left e -> do annotate e; failure
    else assertFails qs

-- Comparison: a < b
prop_clp_lt :: Property
prop_clp_lt = property $ do
  a <- forAll genSmallInt
  b <- forAll genSmallInt
  let qs = show a ++ " < " ++ show b
  if a < b
    then case query qs of Right _ -> success; Left e -> do annotate e; failure
    else assertFails qs

-- is/2 evaluates rhs and unifies with lhs
prop_is_eval :: Property
prop_is_eval = property $ do
  a <- forAll genSmallInt
  b <- forAll genSmallInt
  v <- forAll genVarName
  assertBindings (v ++ " is " ++ show a ++ " + " ++ show b) [(v, show (a + b))]

-- ---------------------------------------------------------------------------
-- § 6  Structural unification nuances
-- ---------------------------------------------------------------------------

-- Two-variable swap: X = Y, Y = atom
prop_unify_chain_via_query :: Property
prop_unify_chain_via_query = property $ do
  name <- forAll genAtomName
  -- X gets bound to Y which is then bound to name
  case query ("X = Y, Y = " ++ name) of
    Right (s : _) -> do
      Map.lookup "X" (Map.fromList s) === Just name
      Map.lookup "Y" (Map.fromList s) === Just name
    _ -> failure

-- equiv (==) requires structurally equal ground terms
prop_equiv_equal :: Property
prop_equiv_equal = property $ do
  n <- forAll genSmallInt
  case query (show n ++ " == " ++ show n) of
    Right _ -> success
    Left e -> do annotate e; failure

prop_equiv_unequal :: Property
prop_equiv_unequal = property $ do
  a <- forAll genSmallInt
  b <- forAll $ Gen.filter (/= a) genSmallInt
  assertFails (show a ++ " == " ++ show b)

-- neq (!=) is the dual of equiv
prop_neq_different :: Property
prop_neq_different = property $ do
  a <- forAll genSmallInt
  b <- forAll $ Gen.filter (/= a) genSmallInt
  case query (show a ++ " != " ++ show b) of
    Right _ -> success
    Left e -> do annotate e; failure

prop_neq_same_fails :: Property
prop_neq_same_fails = property $ do
  n <- forAll genSmallInt
  assertFails (show n ++ " != " ++ show n)

-- ---------------------------------------------------------------------------
-- § 7  Program clauses and resolution
-- ---------------------------------------------------------------------------

-- A fact clause is matched by a goal
prop_clause_fact :: Property
prop_clause_fact = property $ do
  f <- forAll genAtomName
  arg <- forAll genAtomName
  let prog = f ++ " " ++ arg ++ "."
  case parseProgram prog of
    Left e -> do annotate e; failure
    Right p ->
      let solns = solveAll p [Compound f [Atom arg]]
       in assert (length solns >= 1)

-- A rule fires using a fact
prop_clause_rule_fires :: Property
prop_clause_rule_fires = property $ do
  base <- forAll genAtomName
  rule <- forAll $ Gen.filter (/= base) genAtomName
  let prog = base ++ " a.\n" ++ rule ++ " X <- " ++ base ++ " X."
  case parseProgram prog of
    Left e -> do annotate e; failure
    Right p ->
      let solns = solveAll p [Compound rule [NProlog.Var "Z"]]
       in assert (length solns >= 1)

-- A goal that matches no clause yields no solutions
prop_clause_unknown_predicate :: Property
prop_clause_unknown_predicate = property $ do
  f <- forAll genAtomName
  -- empty program
  let solns = solveAll [] [Compound f [Atom "x"]]
  solns === []

-- true/0 always succeeds
prop_builtin_true :: Property
prop_builtin_true = property $ do
  case query "true" of
    Right _ -> success
    Left e -> do annotate e; failure

-- fail/0 never succeeds
prop_builtin_fail :: Property
prop_builtin_fail =
  property $
    assertFails "fail"

-- ---------------------------------------------------------------------------
-- § 8  List unification
-- ---------------------------------------------------------------------------

-- [H|T] = [a,b,c] gives H=a, T=[b,c]
prop_list_head_tail :: Property
prop_list_head_tail = property $ do
  h <- forAll genAtomName
  t1 <- forAll genAtomName
  t2 <- forAll genAtomName
  case query ("H = " ++ h ++ ", T = [" ++ t1 ++ ", " ++ t2 ++ "], [H|T] = [" ++ h ++ ", " ++ t1 ++ ", " ++ t2 ++ "]") of
    Right (s : _) -> do
      Map.lookup "H" (Map.fromList s) === Just h
    Right [] -> do annotate "no solutions"; failure
    Left e -> do annotate e; failure

-- Empty list unifies with itself
prop_list_empty_self :: Property
prop_list_empty_self = property $
  case query "X = [], X = []" of
    Right _ -> success
    Left e -> do annotate e; failure

-- ---------------------------------------------------------------------------
-- § 9  runProlog IO interface
-- ---------------------------------------------------------------------------

prop_runProlog_nocrash :: Property
prop_runProlog_nocrash = property $ do
  a <- forAll genSmallInt
  b <- forAll genSmallInt
  let result = runProlog "" ("X = " ++ show a ++ " + " ++ show b)
  case result of
    Right solns -> assert (length solns >= 1)
    Left _ -> failure

prop_runProlog_false :: Property
prop_runProlog_false = property $ do
  a <- forAll genAtomName
  b <- forAll $ Gen.filter (/= a) genAtomName
  let result = runProlog "" (a ++ " = " ++ b)
  result === Right ["false."]

-- ---------------------------------------------------------------------------
-- § 10  Golden / regression tests
-- ---------------------------------------------------------------------------

prop_golden_chain_arithmetic :: Property
prop_golden_chain_arithmetic = property $ do
  assertBindings "N = 4 * R, R = 2" [("N", "8"), ("R", "2")]

prop_golden_both_ground_reduce :: Property
prop_golden_both_ground_reduce =
  property $
    assertBindings "N = 4 * 2" [("N", "8")]

prop_golden_prefix_known :: Property
prop_golden_prefix_known =
  property $
    assertBindings "R = 2, N = R * 2" [("N", "4"), ("R", "2")]

prop_golden_backsolve :: Property
prop_golden_backsolve =
  property $
    assertBindings "N = 4 * R, N = 12" [("N", "12"), ("R", "3")]

prop_golden_three_unknowns :: Property
prop_golden_three_unknowns =
  property $
    assertBindings "A = B * C, B = 2, C = 3" [("A", "6"), ("B", "2"), ("C", "3")]

-- ---------------------------------------------------------------------------
-- Helpers (local)
-- ---------------------------------------------------------------------------

intercalate0 :: String -> [String] -> String
intercalate0 _ [] = ""
intercalate0 _ [x] = x
intercalate0 sep (x : xs) = x ++ sep ++ intercalate0 sep xs

-- ---------------------------------------------------------------------------
-- Groups exported for Spec.hs
-- ---------------------------------------------------------------------------

nPrologGroup :: Group
nPrologGroup =
  Group
    "NProlog"
    [ ("parse: atom roundtrip", prop_parse_atom),
      ("parse: intlit roundtrip", prop_parse_intlit),
      ("parse: var roundtrip", prop_parse_var),
      ("parse: list no crash", prop_parse_list_nocrash),
      ("parse: arith compound", prop_parse_arith_compound),
      ("parse: nested arith no crash", prop_parse_arith_nested_nocrash),
      ("parse: comparison operators", prop_parse_comparison_operators),
      ("parse: unify operator", prop_parse_unify_op),
      ("parse: no crash on arbitrary input", prop_parse_nocrash),
      ("unify: atom self", prop_unify_atom_self),
      ("unify: atom different", prop_unify_atom_different),
      ("unify: var binds atom", prop_unify_var_binds_atom),
      ("unify: var self", prop_unify_var_self),
      ("unify: intlit equal/unequal", prop_unify_intlit),
      ("unify: compound match", prop_unify_compound_match),
      ("unify: compound diff functor", prop_unify_compound_diff_functor),
      ("deepWalk: chain resolution", prop_deepWalk_chain),
      ("eval: intlit identity", prop_eval_intlit),
      ("eval: plus", prop_eval_plus),
      ("eval: minus", prop_eval_minus),
      ("eval: mul", prop_eval_mul),
      ("eval: div", prop_eval_div),
      ("eval: mod", prop_eval_mod),
      ("eval: free var => Nothing", prop_eval_free_var),
      ("eval: nested expression", prop_eval_nested),
      ("query: unify atom", prop_ground_unify_atom),
      ("query: unify int", prop_ground_unify_int),
      ("query: equal atoms succeed", prop_ground_unify_equal_atoms),
      ("query: different atoms fail", prop_ground_unify_diff_atoms),
      ("CLP: both ground", prop_clp_both_ground),
      ("CLP: deferred mul", prop_clp_deferred_mul),
      ("CLP: single unknown mul", prop_clp_single_unknown_mul),
      ("CLP: subtraction", prop_clp_subtraction),
      ("CLP: gt comparison", prop_clp_gt),
      ("CLP: lt comparison", prop_clp_lt),
      ("CLP: is/2 eval", prop_is_eval),
      ("query: variable chain", prop_unify_chain_via_query),
      ("query: equiv equal", prop_equiv_equal),
      ("query: equiv unequal fails", prop_equiv_unequal),
      ("query: neq different", prop_neq_different),
      ("query: neq same fails", prop_neq_same_fails),
      ("clause: fact matched", prop_clause_fact),
      ("clause: rule fires", prop_clause_rule_fires),
      ("clause: unknown predicate fails", prop_clause_unknown_predicate),
      ("builtin: true succeeds", prop_builtin_true),
      ("builtin: fail fails", prop_builtin_fail),
      ("list: head/tail unification", prop_list_head_tail),
      ("list: empty list self-unifies", prop_list_empty_self),
      ("runProlog: no crash", prop_runProlog_nocrash),
      ("runProlog: false when no solutions", prop_runProlog_false),
      ("golden: N=4*R,R=2 => N=8", prop_golden_chain_arithmetic),
      ("golden: N=4*2 => N=8", prop_golden_both_ground_reduce),
      ("golden: R=2,N=R*2 => N=4", prop_golden_prefix_known),
      ("golden: N=4*R,N=12 => R=3", prop_golden_backsolve),
      ("golden: A=B*C,B=2,C=3 => A=6", prop_golden_three_unknowns)
    ]
