{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- ============================================================
-- StreamFusion.hs
--
-- Stream fusion for SoA VList pipelines by AST rewriting,
-- composed with the "dual" mechanism from VListDual.hs.
--
-- Pipelines are first-class syntax:
--
--     VList.map f (VList.map g xs)
--     VList.fold f z (VList.map g (VList.filter p xs))
--
-- A rewrite engine applies fusion rules to a fixpoint:
--
--   map/map        map f (map g xs)      --> map (f . g) xs
--   filter/filter  filter p (filter q x) --> filter (\v -> q v && p v) x
--   map/filter     map f (filter p xs)   --> filterMap p f xs
--   filter/map     filter q (map f xs)   --> filterMap (q . f) f xs
--   fold/map       fold f z (map g xs)   --> fold (\a v -> f a (g v)) z xs
--   fold/filter    fold f z (filter p x) --> fold (\a v -> if p v then f a v else a) z x
--   ... plus the filterMap absorption rules
--
-- Normal form: ANY map/filter chain collapses to at most one
-- filterMap over the source; any fold-terminated chain collapses
-- to a single fold.  Compositions like (f . g) are built at the
-- object-AST level and beta-reduced, so the fused code is readable.
--
-- Finally, the fused function is DUALIZED (s.f ==> cols.f[i]) and
-- executed in a single columnar pass -- no intermediate VLists, no
-- record materialisation.
-- ============================================================

module StreamFusion where

import Data.List (intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Name = String

-- ------------------------------------------------------------
-- Object-language expressions
-- ------------------------------------------------------------

data Expr
  = IntE Int
  | StrE String
  | Var Name
  | Lam Name Expr
  | App Expr Expr
  | Add Expr Expr
  | Gt Expr Expr -- a > b  ==> 1 or 0 (toy booleans)
  | If Expr Expr Expr -- if c then t else e   (c: 0 = false)
  | FieldGet Expr Name -- e.f            (AoS world)
  | RecE [(Name, Expr)] -- {f = e, ...}
  | ColGet Expr Name -- cols.f         (SoA world)
  | IdxE Expr Expr -- arr[i]
  deriving (Eq, Show)

-- map over immediate children (generic traversal helper)
descend :: (Expr -> Expr) -> Expr -> Expr
descend k = \case
  IntE n -> IntE n
  StrE t -> StrE t
  Var v -> Var v
  Lam v b -> Lam v (k b)
  App f x -> App (k f) (k x)
  Add a b -> Add (k a) (k b)
  Gt a b -> Gt (k a) (k b)
  If c t e -> If (k c) (k t) (k e)
  FieldGet e f -> FieldGet (k e) f
  RecE fs -> RecE [(f, k e) | (f, e) <- fs]
  ColGet e f -> ColGet (k e) f
  IdxE a i -> IdxE (k a) (k i)

-- ------------------------------------------------------------
-- Capture-avoiding substitution + a small simplifier
-- (this is what turns  (\y -> y+1) . (\s -> s.age)
--  into the readable   \v -> v.age + 1)
-- ------------------------------------------------------------

freeVars :: Expr -> S.Set Name
freeVars = \case
  Var v -> S.singleton v
  Lam v b -> S.delete v (freeVars b)
  IntE _ -> S.empty
  StrE _ -> S.empty
  App f x -> freeVars f <> freeVars x
  Add a b -> freeVars a <> freeVars b
  Gt a b -> freeVars a <> freeVars b
  If c t e -> freeVars c <> freeVars t <> freeVars e
  FieldGet e _ -> freeVars e
  RecE fs -> S.unions (map (freeVars . snd) fs)
  ColGet e _ -> freeVars e
  IdxE a i -> freeVars a <> freeVars i

freshName :: Name -> S.Set Name -> Name
freshName base used =
  head
    [ n | n <- base : [base ++ show k | k <- [1 :: Int ..]], n `S.notMember` used
    ]

-- subst x e body : replace free occurrences of x in body by e
subst :: Name -> Expr -> Expr -> Expr
subst x e = go
  where
    fvE = freeVars e
    go t = case t of
      Var v
        | v == x -> e
        | otherwise -> Var v
      Lam v b
        | v == x -> Lam v b -- shadowed
        | v `S.member` fvE -> -- would capture
            let v' = freshName v (fvE <> freeVars b <> S.singleton x)
             in Lam v' (go (subst v (Var v') b))
        | otherwise -> Lam v (go b)
      _ -> descend go t

-- one bottom-up pass of local rules
simplifyPass :: Expr -> Expr
simplifyPass = rule . descend simplifyPass
  where
    rule = \case
      App (Lam v b) a -> subst v a b -- beta
      FieldGet (RecE fs) f
        | Just x <- lookup f fs -> x -- {..}.f
      If (IntE n) t e -> if n /= 0 then t else e
      Add (IntE a) (IntE b) -> IntE (a + b)
      Gt (IntE a) (IntE b) -> IntE (if a > b then 1 else 0)
      t -> t

simplify :: Expr -> Expr
simplify = go (50 :: Int)
  where
    go 0 e = e
    go n e = let e' = simplifyPass e in if e' == e then e else go (n - 1) e'

-- ------------------------------------------------------------
-- AST-level combinators used by the fusion rules
-- ------------------------------------------------------------

-- compose f g  =  \v -> f (g v)      (beta-reduced for readability)
compose :: Expr -> Expr -> Expr
compose f g =
  let v = freshName "v" (freeVars f <> freeVars g)
   in simplify (Lam v (App f (App g (Var v))))

-- andBoth p q  =  \v -> if p v then q v else 0     (p checked first)
andBoth :: Expr -> Expr -> Expr
andBoth p q =
  let v = freshName "v" (freeVars p <> freeVars q)
   in simplify (Lam v (If (App p (Var v)) (App q (Var v)) (IntE 0)))

-- fold/map:      \acc v -> f acc (g v)
foldAfterMap :: Expr -> Expr -> Expr
foldAfterMap f g =
  let used = freeVars f <> freeVars g
      a = freshName "acc" used
      v = freshName "v" (S.insert a used)
   in simplify (Lam a (Lam v (App (App f (Var a)) (App g (Var v)))))

-- fold/filter:   \acc v -> if p v then f acc v else acc
foldAfterFilter :: Expr -> Expr -> Expr
foldAfterFilter f p =
  let used = freeVars f <> freeVars p
      a = freshName "acc" used
      v = freshName "v" (S.insert a used)
   in simplify
        ( Lam
            a
            ( Lam
                v
                ( If
                    (App p (Var v))
                    (App (App f (Var a)) (Var v))
                    (Var a)
                )
            )
        )

-- fold/filterMap: \acc v -> if p v then f acc (g v) else acc
foldAfterFilterMap :: Expr -> Expr -> Expr -> Expr
foldAfterFilterMap f p g =
  let used = freeVars f <> freeVars p <> freeVars g
      a = freshName "acc" used
      v = freshName "v" (S.insert a used)
   in simplify
        ( Lam
            a
            ( Lam
                v
                ( If
                    (App p (Var v))
                    (App (App f (Var a)) (App g (Var v)))
                    (Var a)
                )
            )
        )

-- ------------------------------------------------------------
-- Pipelines (the VList.x surface syntax) and the rewrite engine
-- ------------------------------------------------------------

data Pipe
  = Source Name -- a VList variable
  | PMap Expr Pipe -- VList.map f p
  | PFilter Expr Pipe -- VList.filter p pp
  | PFilterMap Expr Expr Pipe -- VList.filterMap pred f pp  (fused node:
  --   for each v: if pred v then yield f v)
  deriving (Eq, Show)

data Query
  = QCollect Pipe -- materialise the pipeline
  | QFold Expr Expr Pipe -- VList.fold f z p   (terminal)
  deriving (Eq, Show)

-- one outermost-first rewrite step, tagged with the rule that fired
stepPipe :: Pipe -> Maybe (String, Pipe)
stepPipe = \case
  PMap f (PMap g x) -> Just ("map/map", PMap (compose f g) x)
  PMap f (PFilter p x) -> Just ("map/filter", PFilterMap p f x)
  PMap f (PFilterMap p g x) -> Just ("map/filterMap", PFilterMap p (compose f g) x)
  PFilter q (PFilter p x) -> Just ("filter/filter", PFilter (andBoth p q) x)
  PFilter q (PMap f x) -> Just ("filter/map", PFilterMap (compose q f) f x)
  PFilter q (PFilterMap p g x) -> Just ("filter/filterMap", PFilterMap (andBoth p (compose q g)) g x)
  PFilterMap p f (PMap g x) -> Just ("filterMap/map", PFilterMap (compose p g) (compose f g) x)
  PFilterMap p f (PFilter q x) -> Just ("filterMap/filter", PFilterMap (andBoth q p) f x)
  PFilterMap p f (PFilterMap q g x) ->
    Just ("filterMap/filterMap", PFilterMap (andBoth q (compose p g)) (compose f g) x)
  -- no rule at this node: try the child
  PMap f x -> fmap (\(r, x') -> (r, PMap f x')) (stepPipe x)
  PFilter p x -> fmap (\(r, x') -> (r, PFilter p x')) (stepPipe x)
  PFilterMap p f x -> fmap (\(r, x') -> (r, PFilterMap p f x')) (stepPipe x)
  Source _ -> Nothing

stepQuery :: Query -> Maybe (String, Query)
stepQuery = \case
  QFold f z (PMap g x) -> Just ("fold/map", QFold (foldAfterMap f g) z x)
  QFold f z (PFilter p x) -> Just ("fold/filter", QFold (foldAfterFilter f p) z x)
  QFold f z (PFilterMap p g x) -> Just ("fold/filterMap", QFold (foldAfterFilterMap f p g) z x)
  QFold f z x -> fmap (\(r, x') -> (r, QFold f z x')) (stepPipe x)
  QCollect x -> fmap (\(r, x') -> (r, QCollect x')) (stepPipe x)

-- rewrite to fixpoint, collecting the trace of rules fired
fuse :: Query -> ([String], Query)
fuse q = case stepQuery q of
  Nothing -> ([], q)
  Just (r, q') -> let (rs, q'') = fuse q' in (r : rs, q'')

-- ------------------------------------------------------------
-- Values & evaluator
-- ------------------------------------------------------------

data Value
  = VInt Int
  | VStr String
  | VRec (M.Map Name Value)
  | VFun Env Name Expr -- closures keep their AST => dualizable
  | VList Int (M.Map Name [Value]) -- SoA VList
  | VArr [Value]

type Env = M.Map Name Value

eval :: Env -> Expr -> Value
eval env = \case
  IntE n -> VInt n
  StrE t -> VStr t
  Var v -> maybe (error ("unbound: " ++ v)) id (M.lookup v env)
  Lam v b -> VFun env v b
  App f x -> apply (eval env f) (eval env x)
  Add a b -> arith (+) (eval env a) (eval env b)
  Gt a b -> arith (\x y -> if x > y then 1 else 0) (eval env a) (eval env b)
  If c t e -> case eval env c of
    VInt 0 -> eval env e
    VInt _ -> eval env t
    _ -> error "If: non-integer condition"
  FieldGet e f -> case eval env e of
    VRec m -> maybe (error ("no field " ++ f)) id (M.lookup f m)
    _ -> error ("FieldGet ." ++ f ++ " on non-record")
  RecE fs -> VRec (M.fromList [(f, eval env e) | (f, e) <- fs])
  ColGet e f -> case eval env e of
    VList _ cols -> maybe (error ("no column " ++ f)) VArr (M.lookup f cols)
    _ -> error "ColGet on non-VList"
  IdxE a i -> case (eval env a, eval env i) of
    (VArr xs, VInt k) -> xs !! k
    _ -> error "IdxE: bad operands"
  where
    arith op (VInt x) (VInt y) = VInt (op x y)
    arith _ _ _ = error "arith: non-integers"

apply :: Value -> Value -> Value
apply (VFun env v b) x = eval (M.insert v x env) b
apply _ _ = error "apply: not a function"

-- ------------------------------------------------------------
-- The dual mechanism (from VListDual.hs, extended with Gt/If)
-- ------------------------------------------------------------

iName, colsName :: Name
iName = "%i"
colsName = "%cols"

dualize :: Expr -> Either String Expr
dualize (Lam s body) = Lam iName . Lam colsName <$> go body
  where
    go = \case
      FieldGet (Var v) f
        | v == s -> Right (IdxE (ColGet (Var colsName) f) (Var iName))
      Var v
        | v == s -> Left ("parameter '" ++ s ++ "' used as a whole value; no SoA dual exists")
      Lam v b
        | v == s -> Right (Lam v b) -- shadowed: stop rewriting
      t -> descendE go t
    -- Either-valued traversal of immediate children
    descendE k = \case
      IntE n -> Right (IntE n)
      StrE t -> Right (StrE t)
      Var v -> Right (Var v)
      Lam v b -> Lam v <$> k b
      App f x -> App <$> k f <*> k x
      Add a b -> Add <$> k a <*> k b
      Gt a b -> Gt <$> k a <*> k b
      If c t e -> If <$> k c <*> k t <*> k e
      FieldGet e f -> FieldGet <$> k e <*> pure f
      RecE fs -> RecE <$> traverse (\(f, e) -> (,) f <$> k e) fs
      ColGet e f -> ColGet <$> k e <*> pure f
      IdxE a i -> IdxE <$> k a <*> k i
dualize e = Left ("dualize: expected a lambda, got: " ++ pretty e)

-- dualize the ELEMENT parameter of a fold function \acc -> \v -> body
dualizeFoldFn :: Expr -> Either String Expr
dualizeFoldFn (Lam acc inner@(Lam _ _)) = Lam acc <$> dualize inner
dualizeFoldFn e = Left ("dualizeFoldFn: expected \\acc -> \\v -> ..., got: " ++ pretty e)

-- ------------------------------------------------------------
-- Fused columnar executors (single pass over the SoA store)
-- ------------------------------------------------------------

fromRecords :: [Value] -> Value
fromRecords rs =
  let recs = [m | VRec m <- rs]
      fields = if null recs then [] else M.keys (head recs)
   in VList (length recs) (M.fromList [(f, [m M.! f | m <- recs]) | f <- fields])

pack :: [Value] -> Value
pack results = case results of
  (VRec _ : _) -> fromRecords results
  _ -> VArr results

dualOf :: Env -> Expr -> Value
dualOf env fAst = case dualize fAst of
  Left err -> error ("cannot dualize: " ++ err)
  Right d -> eval env d

runFusedPipe :: Env -> Pipe -> Value
runFusedPipe env = \case
  Source nm -> src nm
  PMap fAst (Source nm)
    | vl@(VList n _) <- srcL nm,
      fd <- dualOf env fAst ->
        pack [at fd i vl | i <- [0 .. n - 1]]
  PFilter pAst (Source nm)
    | vl@(VList n cols) <- srcL nm,
      pd <- dualOf env pAst,
      keep <- [i | i <- [0 .. n - 1], truthy (at pd i vl)] ->
        VList (length keep) (M.map (\c -> [c !! i | i <- keep]) cols)
  PFilterMap pAst fAst (Source nm)
    | vl@(VList n _) <- srcL nm,
      pd <- dualOf env pAst,
      fd <- dualOf env fAst ->
        pack [at fd i vl | i <- [0 .. n - 1], truthy (at pd i vl)]
  p -> error ("runFusedPipe: pipeline not in normal form: " ++ prettyP p)
  where
    src nm = maybe (error ("unbound VList: " ++ nm)) id (M.lookup nm env)
    srcL nm = case src nm of
      vl@(VList _ _) -> vl
      _ -> error (nm ++ " is not a VList")
    at fd i vl = apply (apply fd (VInt i)) vl
    truthy (VInt k) = k /= 0
    truthy _ = error "predicate must return Int"

runFusedQuery :: Env -> Query -> Value
runFusedQuery env = \case
  QCollect p -> runFusedPipe env p
  QFold fAst zAst (Source nm) ->
    case M.lookup nm env of
      Just vl@(VList n _) ->
        let fd = case dualizeFoldFn fAst of
              Left err -> error ("cannot dualize fold fn: " ++ err)
              Right d -> eval env d
            z = eval env zAst
         in foldl (\acc i -> apply (apply (apply fd acc) (VInt i)) vl) z [0 .. n - 1]
      _ -> error ("unbound VList: " ++ nm)
  QFold _ _ p -> error ("runFusedQuery: fold not normalized over: " ++ prettyP p)

-- ------------------------------------------------------------
-- Naive (unfused) executor: materialises rows and intermediates.
-- Used as the baseline to show what fusion + duals avoid.
-- ------------------------------------------------------------

rowsOf :: Value -> [Value]
rowsOf (VList n cols) =
  [VRec (M.map (!! i) cols) | i <- [0 .. n - 1]] -- gather: materialise records!
rowsOf (VArr xs) = xs
rowsOf _ = error "rowsOf: not a sequence"

-- returns (rows, log lines, total element visits, values materialised)
runNaivePipe :: Env -> Pipe -> ([Value], [String], Int, Int)
runNaivePipe env = go
  where
    go (Source nm) =
      let vl = maybe (error ("unbound: " ++ nm)) id (M.lookup nm env)
          rs = rowsOf vl
          n = length rs
       in (rs, ["source " ++ nm ++ ": gathered " ++ show n ++ " records from SoA (materialised!)"], n, n)
    go (PMap fAst p) =
      let (rs, lg, vis, al) = go p
          fv = eval env fAst
          out = map (apply fv) rs
          n = length rs
       in (out, lg ++ ["map: visited " ++ show n ++ ", allocated " ++ show n ++ " (intermediate list)"], vis + n, al + n)
    go (PFilter pAst p) =
      let (rs, lg, vis, al) = go p
          pv = eval env pAst
          out = [r | r <- rs, case apply pv r of VInt k -> k /= 0; _ -> False]
          n = length rs
       in (out, lg ++ ["filter: visited " ++ show n ++ ", kept " ++ show (length out)], vis + n, al)
    go (PFilterMap pAst fAst p) =
      let (rs, lg, vis, al) = go p
          pv = eval env pAst
          fv = eval env fAst
          out = [apply fv r | r <- rs, case apply pv r of VInt k -> k /= 0; _ -> False]
          n = length rs
       in (out, lg ++ ["filterMap: visited " ++ show n ++ ", yielded " ++ show (length out)], vis + n, al + length out)

runNaiveQuery :: Env -> Query -> (Value, [String], Int, Int)
runNaiveQuery env = \case
  QCollect p -> let (rs, lg, v, a) = runNaivePipe env p in (VArr rs, lg, v, a)
  QFold fAst zAst p ->
    let (rs, lg, v, a) = runNaivePipe env p
        fv = eval env fAst
        z = eval env zAst
        res = foldl (\acc r -> apply (apply fv acc) r) z rs
     in (res, lg ++ ["fold: visited " ++ show (length rs)], v + length rs, a)

-- ------------------------------------------------------------
-- Pretty printing
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
  If c t e -> "if " ++ pretty c ++ " then " ++ pretty t ++ " else " ++ pretty e
  FieldGet e f -> paren (pretty e) ++ "." ++ f
  RecE fs -> "{" ++ intercalate ", " [f ++ " = " ++ pretty e | (f, e) <- fs] ++ "}"
  ColGet e f -> paren (pretty e) ++ "." ++ f
  IdxE a i -> paren (pretty a) ++ "[" ++ pretty i ++ "]"
  where
    paren s
      | any (== ' ') s = "(" ++ s ++ ")"
      | otherwise = s

prettyP :: Pipe -> String
prettyP = \case
  Source nm -> nm
  PMap f p -> "VList.map (" ++ pretty f ++ ")\n  $ " ++ prettyP p
  PFilter q p -> "VList.filter (" ++ pretty q ++ ")\n  $ " ++ prettyP p
  PFilterMap q f p ->
    "VList.filterMap\n      {pred = "
      ++ pretty q
      ++ ",\n       f    = "
      ++ pretty f
      ++ "}\n  $ "
      ++ prettyP p

prettyQ :: Query -> String
prettyQ = \case
  QCollect p -> prettyP p
  QFold f z p -> "VList.fold (" ++ pretty f ++ ") " ++ pretty z ++ "\n  $ " ++ prettyP p

showV :: Value -> String
showV = \case
  VInt n -> show n
  VStr t -> show t
  VRec m -> "{" ++ intercalate ", " [f ++ " = " ++ showV v | (f, v) <- M.toList m] ++ "}"
  VFun _ v b -> "<fun " ++ pretty (Lam v b) ++ ">"
  VArr xs -> "[" ++ intercalate ", " (map showV xs) ++ "]"
  VList n cols ->
    "VList(n="
      ++ show n
      ++ ")"
      ++ concat ["\n      ." ++ f ++ " = " ++ showV (VArr c) | (f, c) <- M.toList cols]

-- ------------------------------------------------------------
-- Demo
-- ------------------------------------------------------------

banner :: String -> IO ()
banner s = putStrLn ("\n======== " ++ s ++ " ========")

demo :: Env -> String -> Query -> IO ()
demo env title q = do
  banner title
  putStrLn "-- pipeline:"
  putStrLn (indent (prettyQ q))
  let (trace, q') = fuse q
  putStrLn
    ( "-- rewrite trace: "
        ++ if null trace
          then "(nothing to fuse)"
          else intercalate " ; " trace
    )
  putStrLn "-- fused:"
  putStrLn (indent (prettyQ q'))
  -- show the duals of the fused stage functions
  case q' of
    QCollect (PMap f _) -> showDual "map fn" f
    QCollect (PFilterMap p f _) -> showDual "pred" p >> showDual "map fn" f
    QCollect (PFilter p _) -> showDual "pred" p
    QFold f _ _ -> case dualizeFoldFn f of
      Right d -> putStrLn ("-- fold-fn dual:\n" ++ indent (pretty d))
      Left err -> putStrLn ("-- fold-fn dual: " ++ err)
    _ -> pure ()
  let (nv, lg, vis, al) = runNaiveQuery env q
  putStrLn "-- naive (unfused) execution:"
  mapM_ (putStrLn . ("     " ++)) lg
  putStrLn ("     TOTAL: " ++ show vis ++ " element visits, " ++ show al ++ " values materialised")
  let fv = runFusedQuery env q'
  putStrLn ("-- fused columnar execution: 1 pass, 0 records materialised")
  putStrLn ("-- naive result: " ++ oneLine (showV nv))
  putStrLn ("-- fused result: " ++ oneLine (showV fv))
  where
    showDual lbl f = case dualize f of
      Right d -> putStrLn ("-- " ++ lbl ++ " dual:\n" ++ indent (pretty d))
      Left err -> putStrLn ("-- " ++ lbl ++ " dual: CANNOT DUALIZE: " ++ err)
    indent = unlines . map ("     " ++) . lines
    oneLine = unwords . words

student :: String -> Int -> Value
student nm ag = VRec (M.fromList [("name", VStr nm), ("age", VInt ag)])

main :: IO ()
main = do
  let studentsV =
        fromRecords
          [ student "Ada" 36,
            student "Alan" 41,
            student "Grace" 29,
            student "Edsger" 55,
            student "Barbara" 19,
            student "Tony" 24
          ]
      env = M.fromList [("students", studentsV)]

  putStrLn "== source VList (SoA) =="
  putStrLn ("  " ++ showV studentsV)

  -- stage functions (plain ASTs)
  let ageOf = Lam "s" (FieldGet (Var "s") "age") -- \s -> s.age
      incr = Lam "x" (Add (Var "x") (IntE 1)) -- \x -> x + 1
      isAdultR = Lam "s" (Gt (FieldGet (Var "s") "age") (IntE 21)) -- \s -> s.age > 21
      gt30 = Lam "a" (Gt (Var "a") (IntE 30)) -- \a -> a > 30
      plus = Lam "acc" (Lam "x" (Add (Var "acc") (Var "x"))) -- \acc x -> acc + x

  -- 1. map/map: note that `incr` ALONE is not dualizable (it uses its
  --    whole parameter), but the FUSED \v -> v.age + 1 is.
  banner "aside: fusion rescues dualization"
  putStrLn ("  incr = " ++ pretty incr)
  case dualize incr of
    Left err -> putStrLn ("  dual of incr alone: CANNOT DUALIZE: " ++ err)
    Right d -> putStrLn ("  dual: " ++ pretty d)
  putStrLn "  (after map/map fusion with ageOf it becomes a record fn -> dualizable)"

  demo
    env
    "map/map"
    (QCollect (PMap incr (PMap ageOf (Source "students"))))

  -- 2. a four-stage chain: map . filter . map . filter --> one filterMap
  demo
    env
    "map . filter . map . filter  -->  one filterMap"
    ( QCollect
        ( PMap
            incr
            ( PFilter
                gt30
                ( PMap
                    ageOf
                    (PFilter isAdultR (Source "students"))
                )
            )
        )
    )

  -- 3. fold-terminated chain --> a single fold, single loop
  demo
    env
    "fold . map . filter  -->  one fold"
    ( QFold
        plus
        (IntE 0)
        ( PMap
            ageOf
            (PFilter isAdultR (Source "students"))
        )
    )
