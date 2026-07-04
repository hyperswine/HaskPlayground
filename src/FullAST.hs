{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- ============================================================================
-- A minimal end-to-end model of the FP-RISC/Sol compiler pipeline, stages 2-8:
--
--   2. HM inference + row-polymorphic records        (Expr -> Core, typed)
--   3. Type-shape-driven codegen: lens generation    (Core -> Core + lens defs)
--   4. Linearity checking (on structured typed core) (Handle must be used once)
--   5. Flatten to minimal core                       (lambda lift, records ->
--                                                     tuples + projections)
--   6. Lower to ANF                                  (explicit sequencing)
--   7. Fusion + inlining over ANF                    (mapV/mapV fusion, copy
--                                                     propagation, DCE)
--   8. RC insertion (Perceus-style dup/drop)         (per-branch drops,
--                                                     dup-before-non-last-use)
--
-- Deliberate simplifications, noted inline where relevant:
--   * Monomorphic let (real pipeline: generalize, then monomorphize row-poly
--     functions in stage 3/5). The row-polymorphic *inference* is still shown:
--     getx's definition-time type is printed before use-site unification.
--   * Lambdas must be closed at lift time (matches Sol's no-closures rule).
--   * `if` in non-tail position duplicates the continuation (real compiler:
--     join points).
--   * All Rhs forms are pure, so DCE needs no effect analysis.
--   * Handle is linear: freed by its consumer, invisible to RC insertion.
-- ============================================================================
module FullAST where

import Data.List (intercalate, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

-- ----------------------------------------------------------------------------
-- Tiny state monad (avoids any library dependence beyond base + containers)
-- ----------------------------------------------------------------------------
newtype St s a = St {runSt :: s -> (a, s)}

instance Functor (St s) where fmap f (St g) = St $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (St s) where
  pure a = St (a,)
  St f <*> St g = St $ \s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2)

instance Monad (St s) where
  St g >>= k = St $ \s -> let (a, s1) = g s in runSt (k a) s1

getS :: St s s
getS = St $ \s -> (s, s)

putS :: s -> St s ()
putS s = St $ const ((), s)

modS :: (s -> s) -> St s ()
modS f = St $ \s -> ((), f s)

evalSt :: St s a -> s -> a
evalSt m s = fst (runSt m s)

-- ----------------------------------------------------------------------------
-- Names, types, rows
-- ----------------------------------------------------------------------------
type Name = String

data Type
  = TInt
  | TBool
  | TVec
  | THandle
  | TFun Type Type
  | TVar Int
  | TRec Row
  | TTuple [Type] -- appears only after stage 5 (flattened records)
  deriving (Eq, Show)

data Row = REmpty | RVar Int | RExt Name Type Row
  deriving (Eq, Show)

isLinear :: Type -> Bool
isLinear THandle = True
isLinear _ = False

-- Heap-allocated (refcounted) types, for stage 8. Handle is linear: its
-- lifetime is statically exact, so it is freed by its consumer, not by RC.
isHeap :: Type -> Bool
isHeap (TTuple _) = True
isHeap TVec = True
isHeap (TRec _) = True -- shouldn't survive past stage 5, but be safe
isHeap _ = False

-- ----------------------------------------------------------------------------
-- Surface AST (post-desugar, stage 1 output)
-- ----------------------------------------------------------------------------
data Expr
  = EVar Name
  | ELit Int
  | ELam Name Expr
  | EApp Expr Expr
  | ELet Name Expr Expr
  | ERec [(Name, Expr)]
  | EProj Expr Name
  | EIf Expr Expr Expr
  | EPrim Name [Expr] -- saturated primitive application
  deriving (Show)

-- Primitive signatures (all monomorphic; only records are polymorphic here)
primSigs :: M.Map Name Type
primSigs =
  M.fromList
    [ ("add", TFun TInt (TFun TInt TInt)),
      ("mul", TFun TInt (TFun TInt TInt)),
      ("le", TFun TInt (TFun TInt TBool)),
      ("newHandle", TFun TInt THandle),
      ("closeHandle", TFun THandle TInt),
      ("fromTo", TFun TInt (TFun TInt TVec)),
      ("mapV", TFun (TFun TInt TInt) (TFun TVec TVec)),
      ("sumV", TFun TVec TInt)
    ]

-- ----------------------------------------------------------------------------
-- Typed core: same shape as Expr but binders and record nodes carry types
-- ----------------------------------------------------------------------------
data Core
  = CVar Name
  | CLit Int
  | CLam (Name, Type) Core
  | CApp Core Core
  | CLet (Name, Type) Core Core
  | CRec Type [(Name, Core)] -- annotated with its full TRec type
  | CProj Type Core Name -- annotated with the record's type
  | CIf Core Core Core
  | CPrim Name [Core]
  deriving (Show)

-- ============================================================================
-- STAGE 2: HM inference + row polymorphism
-- ============================================================================
data InfS = InfS
  { infN :: Int,
    subT :: M.Map Int Type,
    subR :: M.Map Int Row,
    defLog :: [(Name, Type)] -- definition-time (pre-use) types, for display
  }

type Infer a = St InfS a

freshT :: Infer Type
freshT = do s <- getS; putS s {infN = infN s + 1}; pure (TVar (infN s))

freshR :: Infer Row
freshR = do s <- getS; putS s {infN = infN s + 1}; pure (RVar (infN s))

-- Full zonk: chase substitutions to a fixed point
zT :: Type -> Infer Type
zT t = case t of
  TVar v -> do
    s <- getS
    case M.lookup v (subT s) of
      Just t' -> zT t'
      Nothing -> pure (TVar v)
  TFun a b -> TFun <$> zT a <*> zT b
  TRec r -> TRec <$> zR r
  TTuple ts -> TTuple <$> mapM zT ts
  _ -> pure t

zR :: Row -> Infer Row
zR r = case r of
  RVar v -> do
    s <- getS
    case M.lookup v (subR s) of
      Just r' -> zR r'
      Nothing -> pure (RVar v)
  RExt l t rest -> RExt l <$> zT t <*> zR rest
  REmpty -> pure REmpty

bindT :: Int -> Type -> Infer ()
bindT v t
  | t == TVar v = pure ()
  | occursT v t = error $ "occurs check failed: t" ++ show v ++ " in " ++ pT t
  | otherwise = modS $ \s -> s {subT = M.insert v t (subT s)}

bindR :: Int -> Row -> Infer ()
bindR v r
  | r == RVar v = pure ()
  | occursR v r = error $ "row occurs check failed"
  | otherwise = modS $ \s -> s {subR = M.insert v r (subR s)}

occursT :: Int -> Type -> Bool
occursT v = \case
  TVar x -> x == v
  TFun a b -> occursT v a || occursT v b
  TRec r -> occursTRow v r
  TTuple ts -> any (occursT v) ts
  _ -> False
  where
    occursTRow w = \case
      RExt _ t r -> occursT w t || occursTRow w r
      _ -> False

occursR :: Int -> Row -> Bool
occursR v = \case
  RVar x -> x == v
  RExt _ _ r -> occursR v r
  REmpty -> False

unify :: Type -> Type -> Infer ()
unify a0 b0 = do
  a <- zT a0
  b <- zT b0
  case (a, b) of
    _ | a == b -> pure ()
    (TVar v, t) -> bindT v t
    (t, TVar v) -> bindT v t
    (TFun p r, TFun q s) -> unify p q >> unify r s
    (TRec r1, TRec r2) -> unifyRow r1 r2
    _ -> error $ "type mismatch: " ++ pT a ++ " vs " ++ pT b

unifyRow :: Row -> Row -> Infer ()
unifyRow r10 r20 = do
  r1 <- zR r10
  r2 <- zR r20
  case (r1, r2) of
    (REmpty, REmpty) -> pure ()
    (RVar v, r) -> bindR v r
    (r, RVar v) -> bindR v r
    (RExt l t rest, r) -> do
      (t2, rest2) <- rewriteRow l r
      unify t t2
      unifyRow rest rest2
    (REmpty, RExt l _ _) -> error $ "record missing field: " ++ l

-- Expose field l at the head of a row, extending a row variable if needed.
rewriteRow :: Name -> Row -> Infer (Type, Row)
rewriteRow l = \case
  RExt l' t' rest
    | l' == l -> pure (t', rest)
    | otherwise -> do
        (t2, rest2) <- rewriteRow l rest
        pure (t2, RExt l' t' rest2)
  RVar v -> do
    t <- freshT
    rho <- freshR
    bindR v (RExt l t rho)
    pure (t, rho)
  REmpty -> error $ "record missing field: " ++ l

-- Monomorphic-let HM. Real pipeline: let-generalization here, then
-- monomorphization of row-polymorphic functions in stage 3/5. We log each
-- let-binding's definition-time type BEFORE use sites constrain it, so the
-- row polymorphism the inferencer discovered is still visible in the output.
infer :: M.Map Name Type -> Expr -> Infer (Type, Core)
infer env = \case
  EVar x -> case M.lookup x env of
    Just t -> pure (t, CVar x)
    Nothing -> error $ "unbound variable: " ++ x
  ELit n -> pure (TInt, CLit n)
  ELam x e -> do
    a <- freshT
    (tb, ce) <- infer (M.insert x a env) e
    pure (TFun a tb, CLam (x, a) ce)
  EApp f a -> do
    (tf, cf) <- infer env f
    (ta, ca) <- infer env a
    b <- freshT
    unify tf (TFun ta b)
    pure (b, CApp cf ca)
  ELet x e1 e2 -> do
    (t1, c1) <- infer env e1
    t1z <- zT t1
    modS $ \s -> s {defLog = defLog s ++ [(x, t1z)]} -- definition-time snapshot
    (t2, c2) <- infer (M.insert x t1 env) e2
    pure (t2, CLet (x, t1) c1 c2)
  ERec fields -> do
    tcs <- mapM (\(f, e) -> do (t, c) <- infer env e; pure (f, t, c)) fields
    let row = foldr (\(f, t, _) r -> RExt f t r) REmpty tcs
        ty = TRec row
    pure (ty, CRec ty [(f, c) | (f, _, c) <- tcs])
  EProj e f -> do
    (te, ce) <- infer env e
    t <- freshT
    rho <- freshR
    unify te (TRec (RExt f t rho))
    pure (t, CProj te ce f)
  EIf c t e -> do
    (tc, cc) <- infer env c
    unify tc TBool
    (tt, ct) <- infer env t
    (te, ce) <- infer env e
    unify tt te
    pure (tt, CIf cc ct ce)
  EPrim p args -> case M.lookup p primSigs of
    Nothing -> error $ "unknown primitive: " ++ p
    Just sig -> do
      tcs <- mapM (infer env) args
      let peel s' [] = pure s'
          peel (TFun q r) (ta : tas) = unify q ta >> peel r tas
          peel _ _ = error $ "primitive over-applied: " ++ p
      rt <- peel sig (map fst tcs)
      pure (rt, CPrim p (map snd tcs))

-- Zonk every annotation in the finished core
zonkCore :: Core -> Infer Core
zonkCore = \case
  CVar x -> pure (CVar x)
  CLit n -> pure (CLit n)
  CLam (x, t) b -> CLam . (x,) <$> zT t <*> zonkCore b
  CApp f a -> CApp <$> zonkCore f <*> zonkCore a
  CLet (x, t) r b -> CLet . (x,) <$> zT t <*> zonkCore r <*> zonkCore b
  CRec t fs -> CRec <$> zT t <*> mapM (\(f, c) -> (f,) <$> zonkCore c) fs
  CProj t e f -> CProj <$> zT t <*> zonkCore e <*> pure f
  CIf c t e -> CIf <$> zonkCore c <*> zonkCore t <*> zonkCore e
  CPrim p as -> CPrim p <$> mapM zonkCore as

stage2 :: Expr -> (Core, Type, [(Name, Type)])
stage2 e = evalSt go (InfS 0 M.empty M.empty [])
  where
    go = do
      (t, c) <- infer M.empty e
      tz <- zT t
      cz <- zonkCore c
      -- defLog entries were zonked at snapshot time; re-zonking now would erase
      -- the "polymorphic at definition" view, so keep them as recorded.
      s <- getS
      pure (cz, tz, defLog s)

-- ============================================================================
-- STAGE 3: type-shape-driven codegen -- lens (getter) generation
--
-- Same pipeline slot as VList duals / session dual()+automaton compilation /
-- functor application: reads fully-resolved types, emits structured code,
-- contributes nothing at runtime.
-- ============================================================================
sortedFields :: Row -> [(Name, Type)]
sortedFields r = sortOn fst (go r)
  where
    go REmpty = []
    go (RExt f t r') = (f, t) : go r'
    go (RVar _) = error "open row survived to stage 3 (would need monomorphization)"

-- Collect every (recordType, field) projected anywhere in the core
projSites :: Core -> [(Type, Name)]
projSites = \case
  CProj t e f -> (t, f) : projSites e
  CVar _ -> []
  CLit _ -> []
  CLam _ b -> projSites b
  CApp f a -> projSites f ++ projSites a
  CLet _ r b -> projSites r ++ projSites b
  CRec _ fs -> concatMap (projSites . snd) fs
  CIf c t e -> projSites c ++ projSites t ++ projSites e
  CPrim _ as -> concatMap projSites as

lensName :: Name -> Name
lensName f = "get_" ++ f

-- One generated def per (record shape, field). Demo has one record shape, so
-- plain "get_x"/"get_y" names suffice; multiple shapes would suffix a shape id.
stage3 :: Core -> ([(Name, Core, Type)], Core)
stage3 core = (lensDefs, rewrite core)
  where
    sites =
      M.toList . M.fromList $
        [((lensName f), (t, f)) | (t, f) <- projSites core]
    lensDefs =
      [ ( ln,
          CLam ("r", recTy) (CProj recTy (CVar "r") f), -- raw CProj lives ONLY here
          TFun recTy (fieldTy recTy f)
        )
        | (ln, (recTy, f)) <- sites
      ]
    fieldTy (TRec row) f = fromMaybe (error "no field") (lookup f (sortedFields row))
    fieldTy t _ = error $ "projection from non-record " ++ pT t
    rewrite = \case
      CProj _ e f -> CApp (CVar (lensName f)) (rewrite e)
      CVar x -> CVar x
      CLit n -> CLit n
      CLam b e -> CLam b (rewrite e)
      CApp f a -> CApp (rewrite f) (rewrite a)
      CLet b r e -> CLet b (rewrite r) (rewrite e)
      CRec t fs -> CRec t [(f, rewrite c) | (f, c) <- fs]
      CIf c t e -> CIf (rewrite c) (rewrite t) (rewrite e)
      CPrim p as -> CPrim p (map rewrite as)

-- ============================================================================
-- STAGE 4: linearity checking -- on the STRUCTURED typed core, before
-- flattening erases the binding structure the checker wants to see directly.
-- Every Handle-typed binder must be used exactly once on every path, and may
-- not be captured under a lambda.
-- ============================================================================
countL :: Name -> Core -> Either String Int
countL n = go
  where
    go = \case
      CVar x
        | x == n -> Right 1
        | otherwise -> Right 0
      CLit _ -> Right 0
      CLam (p, _) b
        | p == n -> Right 0 -- shadowed
        | otherwise -> do
            c <- go b
            if c > 0
              then Left $ "linear variable '" ++ n ++ "' captured under a lambda"
              else Right 0
      CApp f a -> (+) <$> go f <*> go a
      CLet (x, _) r b -> do
        cr <- go r
        cb <- if x == n then Right 0 else go b -- shadowed in body
        Right (cr + cb)
      CRec _ fs -> sum <$> mapM (go . snd) fs
      CProj _ e _ -> go e
      CIf c t e -> do
        cc <- go c
        ct <- go t
        ce <- go e
        if ct /= ce
          then
            Left $
              "linear variable '"
                ++ n
                ++ "' used unevenly across if-branches ("
                ++ show ct
                ++ " vs "
                ++ show ce
                ++ ")"
          else Right (cc + ct)
      CPrim _ as -> sum <$> mapM go as

stage4 :: [(Name, Core, Type)] -> Core -> Either String [String]
stage4 defs core = concat <$> mapM checkTop (map (\(_, c, _) -> c) defs ++ [core])
  where
    checkTop = go
    go = \case
      CLet (x, t) r b
        | isLinear t -> do
            c <- countL x b
            if c /= 1
              then
                Left $
                  "linear variable '"
                    ++ x
                    ++ "' used "
                    ++ show c
                    ++ " time(s), expected exactly 1"
              else
                (("  '" ++ x ++ "' : Handle consumed exactly once on every path") :)
                  <$> ((++) <$> go r <*> go b)
        | otherwise -> (++) <$> go r <*> go b
      CLam (p, t) b
        | isLinear t -> do
            c <- countL p b
            if c /= 1
              then Left ("linear parameter '" ++ p ++ "' used " ++ show c ++ " time(s)")
              else go b
        | otherwise -> go b
      CApp f a -> (++) <$> go f <*> go a
      CRec _ fs -> concat <$> mapM (go . snd) fs
      CProj _ e _ -> go e
      CIf c t e -> (\a b c' -> a ++ b ++ c') <$> go c <*> go t <*> go e
      CPrim _ as -> concat <$> mapM go as
      _ -> Right []

-- ============================================================================
-- STAGE 5: flatten to minimal core -- lambda lifting to top-level defs,
-- records -> tuples + positional projections. Obligated to PRESERVE the
-- linearity facts established in stage 4, not re-derive them.
-- ============================================================================
data Flat
  = FVar Name
  | FLit Int
  | FLet (Name, Type) Flat Flat
  | FIf Flat Flat Flat
  | FCall Name [Flat] -- saturated call to a top-level function
  | FPrim Name [Flat]
  | FMkTuple [Flat]
  | FProj Int Flat
  deriving (Show)

data FDef = FDef {fdName :: Name, fdParams :: [(Name, Type)], fdBody :: Flat, fdRet :: Type}

-- Record types -> tuple types (fields in sorted order)
trT :: Type -> Type
trT = \case
  TRec row -> TTuple (map (trT . snd) (sortedFields row))
  TFun a b -> TFun (trT a) (trT b)
  TTuple ts -> TTuple (map trT ts)
  t -> t

-- Type of a structured core term, given binder annotations + global sigs
typeOfCore :: M.Map Name Type -> Core -> Type
typeOfCore env = \case
  CVar x ->
    fromMaybe
      (error $ "typeOfCore: unbound " ++ x)
      (M.lookup x env `orElse` M.lookup x primSigs)
  CLit _ -> TInt
  CLam (x, t) b -> TFun t (typeOfCore (M.insert x t env) b)
  CApp f _ -> case typeOfCore env f of
    TFun _ r -> r
    t -> error $ "typeOfCore: applying non-function " ++ pT t
  CLet (x, t) _ b -> typeOfCore (M.insert x t env) b
  CRec t _ -> t
  CProj t _ f -> case t of
    TRec row -> fromMaybe (error "typeOfCore: field") (lookup f (sortedFields row))
    _ -> error "typeOfCore: proj from non-record"
  CIf _ t _ -> typeOfCore env t
  CPrim p as -> peel (fromMaybe (error "prim?") (M.lookup p primSigs)) (length as)
  where
    orElse (Just a) _ = Just a
    orElse Nothing b = b
    peel t 0 = t
    peel (TFun _ r) n = peel r (n - 1)
    peel _ _ = error "typeOfCore: prim over-applied"

data FlatS = FlatS
  { flN :: Int,
    flDefs :: [FDef],
    flArity :: M.Map Name Int,
    flSigs :: M.Map Name Type
  }

type FlatM a = St FlatS a

freshLam :: FlatM Name
freshLam = do s <- getS; putS s {flN = flN s + 1}; pure ("lam" ++ show (flN s))

addDef :: FDef -> Type -> FlatM ()
addDef d sig = modS $ \s ->
  s
    { flDefs = flDefs s ++ [d],
      flArity = M.insert (fdName d) (length (fdParams d)) (flArity s),
      flSigs = M.insert (fdName d) sig (flSigs s)
    }

-- Peel nested lambdas into a parameter list
peelLams :: Core -> ([(Name, Type)], Core)
peelLams (CLam b e) = let (ps, body) = peelLams e in (b : ps, body)
peelLams e = ([], e)

freeVarsC :: S.Set Name -> Core -> S.Set Name
freeVarsC bound = \case
  CVar x
    | x `S.member` bound -> S.empty
    | otherwise -> S.singleton x
  CLit _ -> S.empty
  CLam (x, _) b -> freeVarsC (S.insert x bound) b
  CApp f a -> freeVarsC bound f `S.union` freeVarsC bound a
  CLet (x, _) r b -> freeVarsC bound r `S.union` freeVarsC (S.insert x bound) b
  CRec _ fs -> S.unions (map (freeVarsC bound . snd) fs)
  CProj _ e _ -> freeVarsC bound e
  CIf c t e -> S.unions [freeVarsC bound c, freeVarsC bound t, freeVarsC bound e]
  CPrim _ as -> S.unions (map (freeVarsC bound) as)

flattenDef :: S.Set Name -> Name -> Core -> Type -> FlatM ()
flattenDef globals nm lam sig = do
  let (ps, body) = peelLams lam
      env = M.fromList ps
  fb <- flatten (globals `S.union` S.fromList (map fst ps)) env body
  s <- getS
  let envG = env `M.union` flSigs s -- locals shadow globals
  addDef (FDef nm [(p, trT t) | (p, t) <- ps] fb (trT (typeOfCore envG body))) (trT sig)

flatten :: S.Set Name -> M.Map Name Type -> Core -> FlatM Flat
flatten globals env core = case core of
  CVar x -> pure (FVar x)
  CLit n -> pure (FLit n)
  -- let-bound lambda: hoist directly under its own name (Sol lambda lifting)
  CLet (f, tf) lam@(CLam _ _) body -> do
    let fvs = freeVarsC (S.union globals (S.singleton f)) lam
    if not (S.null fvs)
      then
        error $
          "closure detected lifting '"
            ++ f
            ++ "' over "
            ++ show (S.toList fvs)
            ++ " (no closures: Sol requires closed lambdas)"
      else do
        flattenDef globals f lam tf
        flatten (S.insert f globals) env body

  -- anonymous lambda in expression position: lift under a fresh name
  CLam _ _ -> do
    let fvs = freeVarsC globals core
    if not (S.null fvs)
      then error $ "closure detected lifting anonymous lambda over " ++ show (S.toList fvs)
      else do
        nm <- freshLam
        s <- getS
        flattenDef globals nm core (typeOfCore (env `M.union` flSigs s) core)
        pure (FVar nm)
  CLet (x, t) r b ->
    FLet (x, trT t)
      <$> flatten globals env r
      <*> flatten globals (M.insert x t env) b
  CApp _ _ -> do
    let (hd, args) = spine core []
    case hd of
      CVar f -> do
        s <- getS
        case M.lookup f (flArity s) of
          Just ar
            | ar == length args ->
                FCall f <$> mapM (flatten globals env) args
          Just ar ->
            error $
              "partial/over application of '"
                ++ f
                ++ "' (arity "
                ++ show ar
                ++ ", got "
                ++ show (length args)
                ++ ")"
          Nothing -> error $ "higher-order local call to '" ++ f ++ "' unsupported"
      _ -> error "call head is not a top-level function"
    where
      spine (CApp f a) acc = spine f (a : acc)
      spine h acc = (h, acc)
  CRec t fs -> case t of
    TRec row ->
      FMkTuple
        <$> mapM
          ( \(f, _) ->
              flatten globals env (fromMaybe (error "field?") (lookup f fs))
          )
          (sortedFields row)
    _ -> error "CRec with non-record type"
  -- raw projections survive only inside generated lens bodies
  CProj t e f -> case t of
    TRec row -> do
      let idx = fromMaybe (error "field?") (lookup f (zip (map fst (sortedFields row)) [0 ..]))
      FProj idx <$> flatten globals env e
    _ -> error "projection from non-record"
  CIf c t e ->
    FIf
      <$> flatten globals env c
      <*> flatten globals env t
      <*> flatten globals env e
  CPrim p as -> FPrim p <$> mapM (flatten globals env) as

stage5 :: [(Name, Core, Type)] -> Core -> ([FDef], Flat)
stage5 lenses core = evalSt go (FlatS 0 [] M.empty M.empty)
  where
    go = do
      mapM_ (\(n, c, t) -> flattenDef (S.fromList lensNames) n c t) lenses
      fmain <- flatten (S.fromList lensNames) M.empty core
      s <- getS
      pure (flDefs s, fmain)
    lensNames = [n | (n, _, _) <- lenses]

-- ============================================================================
-- STAGE 6: lower to ANF -- every intermediate named, evaluation order total.
-- `if` in non-tail position duplicates the continuation (real: join points).
-- ============================================================================
data Atom = AVar Name | ALit Int deriving (Eq, Show)

data Rhs
  = RAtom Atom
  | RCall Name [Atom]
  | RPrim Name [Atom]
  | RTuple [Atom]
  | RProj Int Atom
  deriving (Show)

data ANF
  = ARet Atom
  | ALet (Name, Type) Rhs ANF
  | AIf Atom ANF ANF
  deriving (Show)

data ADef = ADef {adName :: Name, adParams :: [(Name, Type)], adBody :: ANF, adRet :: Type}

-- Type of a Flat term (globals: def sigs + prim sigs; locals: binders)
tof :: M.Map Name Type -> M.Map Name Type -> Flat -> Type
tof gsigs env = \case
  FVar x ->
    fromMaybe
      (error $ "tof: unbound " ++ x)
      (case M.lookup x env of Just t -> Just t; Nothing -> M.lookup x gsigs)
  FLit _ -> TInt
  FLet (x, t) _ b -> tof gsigs (M.insert x t env) b
  FIf _ t _ -> tof gsigs env t
  FCall f as -> peel (fromMaybe (error "tof: def sig") (M.lookup f gsigs)) (length as)
  FPrim p as -> peel (fromMaybe (error "tof: prim sig") (M.lookup p primSigs)) (length as)
  FMkTuple es -> TTuple (map (tof gsigs env) es)
  FProj i e -> case tof gsigs env e of
    TTuple ts -> ts !! i
    t -> error $ "tof: proj from " ++ pT t
  where
    peel t 0 = t
    peel (TFun _ r) n = peel r (n - 1)
    peel _ _ = error "tof: over-applied"

type AnfM a = St Int a

freshA :: AnfM Name
freshA = do n <- getS; putS (n + 1); pure ("t" ++ show n)

anf ::
  M.Map Name Type ->
  M.Map Name Type ->
  Flat ->
  (M.Map Name Type -> Atom -> AnfM ANF) ->
  AnfM ANF
anf gsigs env fl k = case fl of
  FVar x -> k env (AVar x)
  FLit n -> k env (ALit n)
  FLet (x, t) r b -> bindNamed gsigs env x t r (\env' -> anf gsigs env' b k)
  FIf c t e -> atom gsigs env c $ \env1 ca ->
    AIf ca <$> anf gsigs env1 t k <*> anf gsigs env1 e k -- continuation duplicated
  other -> do
    let t = tof gsigs env other
    x <- freshA
    bindNamed gsigs env x t other (\env' -> k env' (AVar x))

bindNamed ::
  M.Map Name Type ->
  M.Map Name Type ->
  Name ->
  Type ->
  Flat ->
  (M.Map Name Type -> AnfM ANF) ->
  AnfM ANF
bindNamed gsigs env x t fl kont = case fl of
  FVar y -> ALet (x, t) (RAtom (AVar y)) <$> kont (M.insert x t env)
  FLit n -> ALet (x, t) (RAtom (ALit n)) <$> kont (M.insert x t env)
  FCall f as -> atoms gsigs env as $ \env1 aas ->
    ALet (x, t) (RCall f aas) <$> kont (M.insert x t env1)
  FPrim p as -> atoms gsigs env as $ \env1 aas ->
    ALet (x, t) (RPrim p aas) <$> kont (M.insert x t env1)
  FMkTuple es -> atoms gsigs env es $ \env1 aas ->
    ALet (x, t) (RTuple aas) <$> kont (M.insert x t env1)
  FProj i e -> atom gsigs env e $ \env1 a ->
    ALet (x, t) (RProj i a) <$> kont (M.insert x t env1)
  FIf c th el -> atom gsigs env c $ \env1 ca ->
    AIf ca
      <$> bindNamed gsigs env1 x t th kont
      <*> bindNamed gsigs env1 x t el kont -- kont duplicated
  FLet (y, ty) r b -> bindNamed gsigs env y ty r $ \env1 ->
    bindNamed gsigs env1 x t b kont

atom ::
  M.Map Name Type ->
  M.Map Name Type ->
  Flat ->
  (M.Map Name Type -> Atom -> AnfM ANF) ->
  AnfM ANF
atom gsigs env fl k = case fl of
  FVar x -> k env (AVar x)
  FLit n -> k env (ALit n)
  _ -> anf gsigs env fl k

atoms ::
  M.Map Name Type ->
  M.Map Name Type ->
  [Flat] ->
  (M.Map Name Type -> [Atom] -> AnfM ANF) ->
  AnfM ANF
atoms _ env [] k = k env []
atoms gsigs env (f : fs) k = atom gsigs env f $ \env1 a ->
  atoms gsigs env1 fs (\env2 as -> k env2 (a : as))

defSig :: FDef -> Type
defSig (FDef _ ps _ rt) = foldr (TFun . snd) rt ps

stage6 :: [FDef] -> Flat -> ([ADef], ANF)
stage6 fdefs fmain = evalSt go 0
  where
    gsigs = M.fromList [(fdName d, defSig d) | d <- fdefs]
    go = do
      adefs <-
        mapM
          ( \d -> do
              body <- anf gsigs (M.fromList (fdParams d)) (fdBody d) (\_ a -> pure (ARet a))
              pure (ADef (fdName d) (fdParams d) body (fdRet d))
          )
          fdefs
      amain <- anf gsigs M.empty fmain (\_ a -> pure (ARet a))
      pure (adefs, amain)

-- ============================================================================
-- STAGE 7: fusion + inlining over ANF
--   * mapV f (mapV g xs) fuses to mapV (fused) xs when the intermediate
--     vector is used exactly once (synthesizes a fused top-level function)
--   * copy propagation of RAtom lets
--   * dead code elimination (all Rhs forms here are pure)
-- ============================================================================
atomVars :: [Atom] -> [Name]
atomVars as = [v | AVar v <- as]

rhsVars :: Rhs -> [Name]
rhsVars = \case
  RAtom a -> atomVars [a]
  RCall _ as -> atomVars as
  RPrim _ as -> atomVars as
  RTuple as -> atomVars as
  RProj _ a -> atomVars [a]

usesA :: Name -> ANF -> Int
usesA n = go
  where
    go = \case
      ARet a -> cnt [a]
      ALet _ r b -> length (filter (== n) (rhsVars r)) + go b
      AIf a t e -> cnt [a] + go t + go e
    cnt as = length (filter (== n) (atomVars as))

substAtom :: Name -> Atom -> Atom -> Atom
substAtom n a (AVar v) | v == n = a
substAtom _ _ x = x

substRhs :: Name -> Atom -> Rhs -> Rhs
substRhs n a = \case
  RAtom x -> RAtom (s x)
  RCall f as -> RCall f (map s as)
  RPrim p as -> RPrim p (map s as)
  RTuple as -> RTuple (map s as)
  RProj i x -> RProj i (s x)
  where
    s = substAtom n a

substANF :: Name -> Atom -> ANF -> ANF
substANF n a = go
  where
    go = \case
      ARet x -> ARet (substAtom n a x)
      ALet b@(x, _) r e
        | x == n -> ALet b (substRhs n a r) e -- shadowed below this binder
        | otherwise -> ALet b (substRhs n a r) (go e)
      AIf x t e -> AIf (substAtom n a x) (go t) (go e)

data FuseS = FuseS {fuN :: Int, fuDefs :: [ADef]}

fusePass :: ANF -> St FuseS ANF
fusePass = go M.empty
  where
    -- pending: intermediate vec -> (mapper atom, source atom), single-use only
    go pend = \case
      ARet a -> pure (ARet a)
      ALet (x, t) rhs rest -> case rhs of
        RPrim "mapV" [f, AVar v]
          | Just (g, src0) <- M.lookup v pend -> do
              fname <- synthFused g f
              let rhs' = RPrim "mapV" [AVar fname, src0]
                  pend' =
                    if usesA x rest == 1 -- chainable, same guard
                      then M.insert x (AVar fname, src0) pend
                      else pend
              ALet (x, t) rhs' <$> go pend' rest
        RPrim "mapV" [f, src]
          | usesA x rest == 1 ->
              ALet (x, t) rhs <$> go (M.insert x (f, src) pend) rest
        _ -> ALet (x, t) rhs <$> go pend rest
      AIf a t e -> AIf a <$> go pend t <*> go pend e

    synthFused g f = do
      s <- getS
      let nm = "fused" ++ show (fuN s)
          body =
            ALet ("p", TInt) (callOf g [AVar "x"]) $
              ALet ("q", TInt) (callOf f [AVar "p"]) $
                ARet (AVar "q")
          d = ADef nm [("x", TInt)] body TInt
      putS s {fuN = fuN s + 1, fuDefs = fuDefs s ++ [d]}
      pure nm
    callOf (AVar fn) as = RCall fn as
    callOf (ALit _) _ = error "function atom is a literal?"

-- copy propagation: substitute every RAtom-bound name within its scope
copyProp :: ANF -> ANF
copyProp = \case
  ALet (x, _) (RAtom a) rest -> copyProp (substANF x a rest)
  ALet b r rest -> ALet b r (copyProp rest)
  AIf a t e -> AIf a (copyProp t) (copyProp e)
  ARet a -> ARet a

-- DCE: all Rhs forms are pure here; a real compiler consults an effect analysis
dce :: ANF -> ANF
dce = \case
  ALet b@(x, _) r rest ->
    let rest' = dce rest
     in if usesA x rest' == 0 then rest' else ALet b r rest'
  AIf a t e -> AIf a (dce t) (dce e)
  ARet a -> ARet a

stage7 :: [ADef] -> ANF -> ([ADef], ANF)
stage7 adefs amain =
  let (mains, FuseS _ newDefs) = runSt (fusePass amain) (FuseS 0 [])
      clean = dce . copyProp
   in ( [d {adBody = clean (adBody d)} | d <- adefs] ++ newDefs,
        clean mains
      )

-- ============================================================================
-- STAGE 8: RC insertion (Perceus-style, ownership-passing convention)
--   * every heap-typed binding/param is owned exactly once
--   * callees consume owned arguments
--   * dup before any use that is not the last use
--   * per-branch drops at `if` for values dead in that branch
--   * drop-all-but-result at return
--   * Handle is linear: freed by its consumer, never refcounted
-- ============================================================================
data RC
  = RCRet Atom
  | RCLet (Name, Type) Rhs RC
  | RCIf Atom RC RC
  | RCDup Name RC
  | RCDrop Name RC

fvANF :: ANF -> S.Set Name
fvANF = \case
  ARet a -> fvAtoms [a]
  ALet (x, _) r b -> S.fromList (rhsVars r) `S.union` S.delete x (fvANF b)
  AIf a t e -> S.unions [fvAtoms [a], fvANF t, fvANF e]
  where
    fvAtoms as = S.fromList (atomVars as)

perceus :: M.Map Name Type -> S.Set Name -> ANF -> RC
perceus env owned = \case
  ARet a ->
    let keep = case a of AVar v | v `S.member` owned -> S.singleton v; _ -> S.empty
        drops = S.toList (owned `S.difference` keep)
     in foldr RCDrop (RCRet a) drops
  AIf a t e ->
    let fvt = fvANF t
        fve = fvANF e
        deadT = S.toList (owned `S.difference` fvt)
        deadE = S.toList (owned `S.difference` fve)
        t' = foldr RCDrop (perceus env (owned `S.intersection` fvt) t) deadT
        e' = foldr RCDrop (perceus env (owned `S.intersection` fve) e) deadE
     in RCIf a t' e'
  ALet (x, tx) rhs body ->
    let liveAfter = fvANF body
        heapUses =
          [ v | v <- rhsVars rhs, v `S.member` owned -- owned => heap
          ]
        counts = M.fromListWith (+) [(v, 1 :: Int) | v <- heapUses]
        dupsFor v n =
          let needed = n + (if v `S.member` liveAfter then 1 else 0)
           in replicate (needed - 1) v
        dups = concat [dupsFor v n | (v, n) <- M.toList counts]
        -- owned vars neither used here nor later: drop now (rare; safety net)
        dead =
          [ v | v <- S.toList owned, v `notElem` heapUses, not (v `S.member` liveAfter)
          ]
        owned1 =
          S.filter (`S.member` liveAfter) owned
            `S.union` (if isHeap tx then S.singleton x else S.empty)
        body0 = perceus (M.insert x tx env) owned1 body
        body1 =
          if isHeap tx && not (x `S.member` fvANF body)
            then RCDrop x body0
            else body0
     in foldr RCDup (foldr RCDrop (RCLet (x, tx) rhs body1) dead) dups

rcOfDef :: ADef -> (Name, [(Name, Type)], RC)
rcOfDef (ADef n ps body _) =
  let env = M.fromList ps
      owned = S.fromList [p | (p, t) <- ps, isHeap t]
   in (n, ps, perceus env owned body)

stage8 :: [ADef] -> ANF -> ([(Name, [(Name, Type)], RC)], RC)
stage8 adefs amain = (map rcOfDef adefs, perceus M.empty S.empty amain)

-- ============================================================================
-- Pretty printers
-- ============================================================================
pT :: Type -> String
pT = \case
  TInt -> "Int"
  TBool -> "Bool"
  TVec -> "Vec"
  THandle -> "Handle"
  TVar v -> "t" ++ show v
  TFun a b -> pTa a ++ " -> " ++ pT b
  TRec r -> "{" ++ pRow r ++ "}"
  TTuple ts -> "(" ++ intercalate ", " (map pT ts) ++ ")"
  where
    pTa t@(TFun _ _) = "(" ++ pT t ++ ")"
    pTa t = pT t

pRow :: Row -> String
pRow REmpty = ""
pRow (RVar v) = "rho" ++ show v
pRow (RExt f t r) =
  f ++ " : " ++ pT t ++ case r of
    REmpty -> ""
    _ -> case r of RVar _ -> " | " ++ pRow r; _ -> ", " ++ pRow r

ind :: Int -> String
ind n = replicate n ' '

pCore :: Int -> Core -> String
pCore i = \case
  CVar x -> x
  CLit n -> show n
  CLam (x, t) b -> "\\(" ++ x ++ " : " ++ pT t ++ ") -> " ++ pCore i b
  CApp f a -> pCoreA i f ++ " " ++ pCoreA i a
  CLet (x, t) r b ->
    "let "
      ++ x
      ++ " : "
      ++ pT t
      ++ " = "
      ++ pCore (i + 2) r
      ++ " in\n"
      ++ ind i
      ++ pCore i b
  CRec _ fs -> "{" ++ intercalate ", " [f ++ " = " ++ pCore i c | (f, c) <- fs] ++ "}"
  CProj _ e f -> pCoreA i e ++ "." ++ f
  CIf c t e ->
    "if "
      ++ pCore i c
      ++ "\n"
      ++ ind (i + 2)
      ++ "then "
      ++ pCore (i + 2) t
      ++ "\n"
      ++ ind (i + 2)
      ++ "else "
      ++ pCore (i + 2) e
  CPrim p as -> p ++ "(" ++ intercalate ", " (map (pCore i) as) ++ ")"
  where
    pCoreA j e = case e of
      CApp _ _ -> "(" ++ pCore j e ++ ")"
      CLam _ _ -> "(" ++ pCore j e ++ ")"
      _ -> pCore j e

pFlat :: Int -> Flat -> String
pFlat i = \case
  FVar x -> x
  FLit n -> show n
  FLet (x, t) r b ->
    "let "
      ++ x
      ++ " : "
      ++ pT t
      ++ " = "
      ++ pFlat (i + 2) r
      ++ " in\n"
      ++ ind i
      ++ pFlat i b
  FIf c t e ->
    "if "
      ++ pFlat i c
      ++ "\n"
      ++ ind (i + 2)
      ++ "then "
      ++ pFlat (i + 2) t
      ++ "\n"
      ++ ind (i + 2)
      ++ "else "
      ++ pFlat (i + 2) e
  FCall f as -> f ++ "(" ++ intercalate ", " (map (pFlat i) as) ++ ")"
  FPrim p as -> p ++ "(" ++ intercalate ", " (map (pFlat i) as) ++ ")"
  FMkTuple es -> "mktuple(" ++ intercalate ", " (map (pFlat i) es) ++ ")"
  FProj n e -> "proj_" ++ show n ++ "(" ++ pFlat i e ++ ")"

pFDef :: FDef -> String
pFDef (FDef n ps b rt) =
  n
    ++ "("
    ++ intercalate ", " [p ++ " : " ++ pT t | (p, t) <- ps]
    ++ ") : "
    ++ pT rt
    ++ " =\n  "
    ++ pFlat 2 b

pAtom :: Atom -> String
pAtom (AVar v) = v
pAtom (ALit n) = show n

pRhs :: Rhs -> String
pRhs = \case
  RAtom a -> pAtom a
  RCall f as -> f ++ "(" ++ intercalate ", " (map pAtom as) ++ ")"
  RPrim p as -> p ++ "(" ++ intercalate ", " (map pAtom as) ++ ")"
  RTuple as -> "mktuple(" ++ intercalate ", " (map pAtom as) ++ ")"
  RProj i a -> "proj_" ++ show i ++ "(" ++ pAtom a ++ ")"

pANF :: Int -> ANF -> String
pANF i = \case
  ARet a -> ind i ++ "ret " ++ pAtom a
  ALet (x, t) r b ->
    ind i ++ "let " ++ x ++ " : " ++ pT t ++ " = " ++ pRhs r ++ "\n" ++ pANF i b
  AIf a t e ->
    ind i
      ++ "if "
      ++ pAtom a
      ++ "\n"
      ++ ind i
      ++ "then\n"
      ++ pANF (i + 2) t
      ++ "\n"
      ++ ind i
      ++ "else\n"
      ++ pANF (i + 2) e

pADef :: ADef -> String
pADef (ADef n ps b rt) =
  n
    ++ "("
    ++ intercalate ", " [p ++ " : " ++ pT t | (p, t) <- ps]
    ++ ") : "
    ++ pT rt
    ++ " =\n"
    ++ pANF 2 b

pRC :: Int -> RC -> String
pRC i = \case
  RCRet a -> ind i ++ "ret " ++ pAtom a
  RCLet (x, t) r b ->
    ind i ++ "let " ++ x ++ " : " ++ pT t ++ " = " ++ pRhs r ++ "\n" ++ pRC i b
  RCIf a t e ->
    ind i
      ++ "if "
      ++ pAtom a
      ++ "\n"
      ++ ind i
      ++ "then\n"
      ++ pRC (i + 2) t
      ++ "\n"
      ++ ind i
      ++ "else\n"
      ++ pRC (i + 2) e
  RCDup v b -> ind i ++ "dup " ++ v ++ "\n" ++ pRC i b
  RCDrop v b -> ind i ++ "drop " ++ v ++ "\n" ++ pRC i b

banner :: String -> IO ()
banner s = putStrLn $ "\n" ++ replicate 76 '=' ++ "\n" ++ s ++ "\n" ++ replicate 76 '='

-- ============================================================================
-- Demo program (as post-desugar surface AST):
--
--   let getx = \r -> r.x                       -- row-polymorphic getter
--   let rec1 = {x = 10, y = 20}                -- heap record, shared uses
--   let h    = newHandle (getx rec1)           -- LINEAR handle
--   let v1   = fromTo 1 rec1.y
--   let v2   = mapV (\a -> mul a 2) v1         -- } fusion candidates
--   let v3   = mapV (\b -> add b 1) v2         -- }
--   let s    = sumV v3
--   let w    = {x = add s 1, y = 0}            -- heap, used in ONE branch only
--   let t    = if le s 100 then getx rec1
--                          else add (getx rec1) w.x
--   in add t (closeHandle h)                   -- handle consumed exactly once
-- ============================================================================
demo :: Expr
demo =
  ELet "getx" (ELam "r" (EProj (EVar "r") "x"))
    $ ELet "rec1" (ERec [("x", ELit 10), ("y", ELit 20)])
    $ ELet "h" (EPrim "newHandle" [EApp (EVar "getx") (EVar "rec1")])
    $ ELet "v1" (EPrim "fromTo" [ELit 1, EProj (EVar "rec1") "y"])
    $ ELet "v2" (EPrim "mapV" [ELam "a" (EPrim "mul" [EVar "a", ELit 2]), EVar "v1"])
    $ ELet "v3" (EPrim "mapV" [ELam "b" (EPrim "add" [EVar "b", ELit 1]), EVar "v2"])
    $ ELet "s" (EPrim "sumV" [EVar "v3"])
    $ ELet "w" (ERec [("x", EPrim "add" [EVar "s", ELit 1]), ("y", ELit 0)])
    $ ELet
      "t"
      ( EIf
          (EPrim "le" [EVar "s", ELit 100])
          (EApp (EVar "getx") (EVar "rec1"))
          ( EPrim
              "add"
              [ EApp (EVar "getx") (EVar "rec1"),
                EProj (EVar "w") "x"
              ]
          )
      )
    $ EPrim "add" [EVar "t", EPrim "closeHandle" [EVar "h"]]

-- A deliberately broken program: linear handle consumed twice
badDemo :: Expr
badDemo =
  ELet "h" (EPrim "newHandle" [ELit 1]) $
    EPrim
      "add"
      [ EPrim "closeHandle" [EVar "h"],
        EPrim "closeHandle" [EVar "h"]
      ]

main :: IO ()
main = do
  -- ------------------------------------------------------------- stage 2
  banner "STAGE 2: HM inference + row polymorphism"
  let (core2, mainTy, dlog) = stage2 demo
  putStrLn "definition-time types (snapshotted before use sites constrain them):"
  mapM_ (\(n, t) -> putStrLn $ "  " ++ n ++ " : " ++ pT t) dlog
  putStrLn $ "\nmain : " ++ pT mainTy
  putStrLn "\ntyped core (zonked):"
  putStrLn (pCore 0 core2)

  -- ------------------------------------------------------------- stage 3
  banner
    "STAGE 3: type-shape-driven codegen (lens generation)\n\
    \  [same slot as: VList duals, session dual()+automaton, functors]"
  let (lenses, core3) = stage3 core2
  putStrLn "generated lens definitions (raw projections live only here):"
  mapM_
    ( \(n, c, t) ->
        putStrLn $
          "  "
            ++ n
            ++ " : "
            ++ pT t
            ++ "\n  "
            ++ n
            ++ " = "
            ++ pCore 2 c
    )
    lenses
  putStrLn "\ncore with r.f rewritten to get_f r:"
  putStrLn (pCore 0 core3)

  -- ------------------------------------------------------------- stage 4
  banner "STAGE 4: linearity checking (on structured core, BEFORE flattening)"
  case stage4 lenses core3 of
    Left err -> putStrLn $ "REJECTED: " ++ err
    Right ok -> mapM_ putStrLn ("linearity OK:" : ok)
  putStrLn "\nnegative test (same handle consumed twice):"
  let (badCore, _, _) = stage2 badDemo
      (badL, badC) = stage3 badCore
  case stage4 badL badC of
    Left err -> putStrLn $ "  REJECTED (as it should be): " ++ err
    Right _ -> putStrLn "  BUG: bad program passed the linearity check"

  -- ------------------------------------------------------------- stage 5
  banner
    "STAGE 5: flatten to minimal core\n\
    \  [lambda lift -> top-level defs, records -> tuples + projections]"
  let (fdefs, fmain) = stage5 lenses core3
  mapM_ (putStrLn . (++ "\n") . pFDef) fdefs
  putStrLn "main =" >> putStrLn ("  " ++ pFlat 2 fmain)

  -- ------------------------------------------------------------- stage 6
  banner "STAGE 6: ANF (if-in-let duplicates the continuation; real: join points)"
  let (adefs, amain) = stage6 fdefs fmain
  mapM_ (putStrLn . (++ "\n") . pADef) adefs
  putStrLn "main =" >> putStrLn (pANF 2 amain)

  -- ------------------------------------------------------------- stage 7
  banner
    "STAGE 7: fusion + inlining over ANF\n\
    \  [mapV/mapV fusion synthesizing fused0; copy propagation; DCE]"
  let (adefs7, amain7) = stage7 adefs amain
  mapM_ (putStrLn . (++ "\n") . pADef) adefs7
  putStrLn "main =" >> putStrLn (pANF 2 amain7)

  -- ------------------------------------------------------------- stage 8
  banner
    "STAGE 8: RC insertion (Perceus-style dup/drop)\n\
    \  [dup before non-last use; per-branch drops; Handle is linear -> no RC]"
  let (rcdefs, rcmain) = stage8 adefs7 amain7
  mapM_
    ( \(n, ps, b) -> do
        putStrLn $
          n
            ++ "("
            ++ intercalate
              ", "
              [p ++ " : " ++ pT t | (p, t) <- ps]
            ++ ") ="
        putStrLn (pRC 2 b)
        putStrLn ""
    )
    rcdefs
  putStrLn "main =" >> putStrLn (pRC 2 rcmain)
