{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- ============================================================
-- VListDual.hs
--
-- Simulation of the "dual" mechanism for SoA VLists.
--
-- Idea: a VList Student is stored column-wise (struct-of-arrays).
-- Any function that does record field lookups, e.g.
--
--     ageInc s = s.age + 1
--
-- automatically gets a *dual* that operates on the column store
-- by index:
--
--     ageInc_dual i cols = (cols.age)[i] + 1
--
-- VList combinators (vmap, vfilter) never materialise a Student
-- record: they dualize the AST of the function they're given and
-- drive it with an index over the columns.
-- ============================================================

module VListDual where

import Data.List (intercalate)
import qualified Data.Map.Strict as M

type Name = String

-- ------------------------------------------------------------
-- The object-language AST
-- ------------------------------------------------------------

data Expr
  = IntE Int
  | StrE String
  | Var Name
  | Lam Name Expr
  | App Expr Expr
  | Add Expr Expr
  | Gt Expr Expr -- a > b  ==> 1 or 0 (toy booleans)
  | FieldGet Expr Name -- e.f            (AoS world: record field)
  | RecE [(Name, Expr)] -- {f = e, ...}   (record construction)
  | ColGet Expr Name -- cols.f         (SoA world: fetch a column)
  | IdxE Expr Expr -- arr[i]         (SoA world: index a column)
  deriving (Eq, Show)

-- ------------------------------------------------------------
-- Dualization: the heart of the mechanism
--
--   dualize (\s -> body)  ==>  \i -> \cols -> body'
--
-- where every occurrence of  (s.f)  in body is rewritten to
-- ((cols.f)[i]).  The record parameter itself vanishes; only
-- its field projections survive, re-routed through the columns.
-- ------------------------------------------------------------

iName, colsName :: Name
iName = "%i" -- '%' prefix guarantees no capture of user variables
colsName = "%cols"

dualize :: Expr -> Either String Expr
dualize (Lam s body) = do
  body' <- go body
  pure (Lam iName (Lam colsName body'))
  where
    go :: Expr -> Either String Expr
    go = \case
      -- the rewrite:  s.f  ==>  (cols.f)[i]
      FieldGet (Var v) f
        | v == s -> Right (IdxE (ColGet (Var colsName) f) (Var iName))
      -- the record parameter escaping *whole* has no columnar dual
      Var v
        | v == s ->
            Left
              ( "parameter '"
                  ++ s
                  ++ "' used as a whole record; no SoA dual exists"
              )
      -- shadowing: an inner \s -> ... rebinds s, stop rewriting under it
      Lam v b
        | v == s -> Right (Lam v b)
        | otherwise -> Lam v <$> go b
      -- everything else is structural recursion
      IntE n -> Right (IntE n)
      StrE t -> Right (StrE t)
      Var v -> Right (Var v)
      App f x -> App <$> go f <*> go x
      Add a b -> Add <$> go a <*> go b
      Gt a b -> Gt <$> go a <*> go b
      FieldGet e f -> FieldGet <$> go e <*> pure f
      RecE fs -> RecE <$> traverse (\(f, e) -> (,) f <$> go e) fs
      ColGet e f -> ColGet <$> go e <*> pure f
      IdxE a b -> IdxE <$> go a <*> go b
dualize e = Left ("dualize: expected a lambda, got: " ++ pretty e)

-- ------------------------------------------------------------
-- Values & evaluator
-- ------------------------------------------------------------

data Value
  = VInt Int
  | VStr String
  | VRec (M.Map Name Value)
  | VFun Env Name Expr -- closure: we keep the AST, so we can dualize it!
  | VList Int (M.Map Name [Value]) -- SoA VList: length + named columns
  | VArr [Value] -- a single column

type Env = M.Map Name Value

eval :: Env -> Expr -> Value
eval env = \case
  IntE n -> VInt n
  StrE t -> VStr t
  Var v -> maybe (error ("unbound: " ++ v)) id (M.lookup v env)
  Lam v b -> VFun env v b
  App f x -> apply (eval env f) (eval env x)
  Add a b -> case (eval env a, eval env b) of
    (VInt x, VInt y) -> VInt (x + y)
    _ -> error "Add: non-integers"
  Gt a b -> case (eval env a, eval env b) of
    (VInt x, VInt y) -> VInt (if x > y then 1 else 0)
    _ -> error "Gt: non-integers"
  FieldGet e f -> case eval env e of
    VRec m -> maybe (error ("no field " ++ f)) id (M.lookup f m)
    _ -> error ("FieldGet ." ++ f ++ " on non-record")
  RecE fs -> VRec (M.fromList [(f, eval env e) | (f, e) <- fs])
  ColGet e f -> case eval env e of
    VList _ cols ->
      maybe
        (error ("no column " ++ f))
        VArr
        (M.lookup f cols)
    _ -> error "ColGet on non-VList"
  IdxE a i -> case (eval env a, eval env i) of
    (VArr xs, VInt k) -> xs !! k
    _ -> error "IdxE: bad operands"

apply :: Value -> Value -> Value
apply (VFun env v b) x = eval (M.insert v x env) b
apply _ _ = error "apply: not a function"

-- ------------------------------------------------------------
-- VList construction and the VList.x combinators
-- ------------------------------------------------------------

-- Build a SoA VList from an AoS list of records (transpose)
fromRecords :: [Value] -> Value
fromRecords rs =
  let recs = [m | VRec m <- rs]
      fields = if null recs then [] else M.keys (head recs)
      col f = [m M.! f | m <- recs]
   in VList (length recs) (M.fromList [(f, col f) | f <- fields])

-- vmap: dualize the given function's AST, then drive it by index.
-- Note: no Student record is EVER materialised.  If the mapped
-- function *produces* records, the results are re-packed as SoA.
vmap :: Value -> Value -> Value
vmap (VFun cloEnv s body) vl@(VList n _) =
  case dualize (Lam s body) of
    Left err -> error ("vmap: " ++ err)
    Right dual ->
      let dualFn = eval cloEnv dual
          resAt i = apply (apply dualFn (VInt i)) vl
          results = map resAt [0 .. n - 1]
       in case results of
            (VRec _ : _) -> fromRecords results -- records out => SoA out
            _ -> VArr results
vmap _ _ = error "vmap: expected (function, VList)"

-- vfilter: same trick with a predicate; keeps rows by index,
-- slicing every column in lock-step.
vfilter :: Value -> Value -> Value
vfilter (VFun cloEnv s body) (VList n cols) =
  case dualize (Lam s body) of
    Left err -> error ("vfilter: " ++ err)
    Right dual ->
      let dualFn = eval cloEnv dual
          keep i = case apply (apply dualFn (VInt i)) (VList n cols) of
            VInt k -> k /= 0 -- toy language: 0 = false
            _ -> error "vfilter: predicate must return Int"
          idxs = filter keep [0 .. n - 1]
       in VList (length idxs) (M.map (\c -> [c !! i | i <- idxs]) cols)
vfilter _ _ = error "vfilter: expected (function, VList)"

-- ------------------------------------------------------------
-- Pretty printing (so we can SEE the generated duals)
-- ------------------------------------------------------------

pretty :: Expr -> String
pretty = \case
  IntE n -> show n
  StrE t -> show t
  Var v -> v
  Lam v b -> "\\" ++ v ++ " -> " ++ pretty b
  App f x -> paren (pretty f) ++ " " ++ paren (pretty x)
  Add a b -> paren (pretty a) ++ " + " ++ paren (pretty b)
  Gt a b -> paren (pretty a) ++ " > " ++ paren (pretty b)
  FieldGet e f -> paren (pretty e) ++ "." ++ f
  RecE fs ->
    "{"
      ++ intercalate
        ", "
        [f ++ " = " ++ pretty e | (f, e) <- fs]
      ++ "}"
  ColGet e f -> paren (pretty e) ++ "." ++ f
  IdxE a i -> paren (pretty a) ++ "[" ++ pretty i ++ "]"
  where
    paren s
      | any (== ' ') s = "(" ++ s ++ ")"
      | otherwise = s

showV :: Value -> String
showV = \case
  VInt n -> show n
  VStr t -> show t
  VRec m ->
    "{"
      ++ intercalate
        ", "
        [f ++ " = " ++ showV v | (f, v) <- M.toList m]
      ++ "}"
  VFun _ v b -> "<fun " ++ pretty (Lam v b) ++ ">"
  VArr xs -> "[" ++ intercalate ", " (map showV xs) ++ "]"
  VList n cols ->
    "VList(n="
      ++ show n
      ++ ")\n"
      ++ unlines
        [ "    ." ++ f ++ " = " ++ showV (VArr c)
          | (f, c) <- M.toList cols
        ]

-- ------------------------------------------------------------
-- Demo
-- ------------------------------------------------------------

student :: String -> Int -> Value
student nm ag = VRec (M.fromList [("name", VStr nm), ("age", VInt ag)])

main :: IO ()
main = do
  let students =
        fromRecords
          [ student "Ada" 36,
            student "Alan" 41,
            student "Grace" 29,
            student "Edsger" 55
          ]

  putStrLn "== SoA storage of VList Student =="
  putStrLn (showV students)

  -- ageInc s = s.age + 1
  let ageIncAST = Lam "student" (Add (FieldGet (Var "student") "age") (IntE 1))
      ageInc = eval M.empty ageIncAST

  putStrLn "== Dual generation =="
  putStrLn ("  source: ageInc      = " ++ pretty ageIncAST)
  case dualize ageIncAST of
    Right d -> putStrLn ("  dual:   ageInc_dual = " ++ pretty d)
    Left err -> putStrLn ("  ERROR: " ++ err)
  putStrLn ""

  putStrLn "== VList.map ageInc students  (runs the dual, no records built) =="
  putStrLn ("  " ++ showV (vmap ageInc students))
  putStrLn ""

  -- record-producing map: birthday s = {name = s.name, age = s.age + 1}
  let birthdayAST =
        Lam
          "s"
          ( RecE
              [ ("name", FieldGet (Var "s") "name"),
                ("age", Add (FieldGet (Var "s") "age") (IntE 1))
              ]
          )
      birthday = eval M.empty birthdayAST

  putStrLn "== Record-producing map re-packs as SoA =="
  putStrLn ("  source: birthday      = " ++ pretty birthdayAST)
  case dualize birthdayAST of
    Right d -> putStrLn ("  dual:   birthday_dual = " ++ pretty d)
    Left err -> putStrLn ("  ERROR: " ++ err)
  putStrLn ("  VList.map birthday students =\n  " ++ showV (vmap birthday students))

  -- vfilter with a dual predicate: isOver30 s = s.age > 30
  let isOver30AST = Lam "s" (Gt (FieldGet (Var "s") "age") (IntE 30))
      isOver30 = eval M.empty isOver30AST
  putStrLn ""
  putStrLn "== VList.filter isOver30 students (dual predicate, columns sliced in lock-step) =="
  putStrLn ("  source: isOver30      = " ++ pretty isOver30AST)
  case dualize isOver30AST of
    Right d -> putStrLn ("  dual:   isOver30_dual = " ++ pretty d)
    Left err -> putStrLn ("  ERROR: " ++ err)
  putStrLn ("  " ++ showV (vfilter isOver30 students))
  putStrLn ""

  -- Demonstrate the limitation: a function that uses the record as a whole
  let identAST = Lam "s" (Var "s")
  putStrLn "== Limitation: whole-record use has no dual =="
  putStrLn ("  source: ident = " ++ pretty identAST)
  case dualize identAST of
    Right d -> putStrLn ("  dual: " ++ pretty d)
    Left err -> putStrLn ("  cannot dualize: " ++ err)
