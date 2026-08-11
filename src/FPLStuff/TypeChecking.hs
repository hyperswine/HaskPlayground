{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- CheckPoC.hs
--
-- A proof-of-concept of the "conservative static checking with dynamic
-- insertion" design discussed for Sol:
--
--   * Functions carry preconditions (interval refinements per parameter)
--     and a postcondition (interval refinement on the result).
--   * A conservative ABSTRACT INTERPRETER over the interval domain tries
--     to discharge every precondition at every call site, and every
--     postcondition per function, statically.  Anything it cannot prove
--     gets a DYNAMIC CHECK inserted at exactly that site -- the program
--     still runs, it just pays a runtime guard where the proof failed.
--   * A conservative TERMINATION / MEASURE checker: a function declared
--     `safe` may only self-recurse if (a) every self-call is in tail
--     position, (b) some parameter syntactically decreases (p - k, k>=1)
--     at every self-call, and (c) that parameter's precondition gives it
--     a finite lower bound.  This yields a static call-count bound
--     (WCET shape).  Anything else must be declared `unsafe`.
--   * `unsafe` is TRANSITIVE: a safe function calling an unsafe one is a
--     compile error.  Mutual recursion between safe functions is
--     conservatively rejected.
--   * A builtin bounded recursion scheme, Fold lo hi f acc, whose
--     iteration count is bounded by the range -- no per-use termination
--     proof needed (the generic proof is done once, here, in the
--     checker's transfer function for Fold).
--
-- Run:  runghc CheckPoC.hs        (or ghc -O0 CheckPoC.hs && ./CheckPoC)

module FPLStuff.TypeChecking where

import Data.List (intercalate, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import System.IO (hSetEncoding, stdout, utf8)

--------------------------------------------------------------------------
-- 1. The interval abstract domain
--------------------------------------------------------------------------

data Ext = NegInf | Fin Integer | PosInf deriving (Eq, Ord)

instance Show Ext where
  show NegInf = "-inf"
  show PosInf = "+inf"
  show (Fin n) = show n

-- An interval [lo, hi]; Bot is the unreachable/empty interval.
data Itv = Bot | Itv Ext Ext deriving (Eq)

instance Show Itv where
  show Bot = "_|_"
  show (Itv a b) = "[" ++ show a ++ "," ++ show b ++ "]"

top = Itv NegInf PosInf

itv :: Integer -> Integer -> Itv
itv a b = Itv (Fin a) (Fin b)

atLeast, atMost :: Integer -> Itv
atLeast n = Itv (Fin n) PosInf
atMost n = Itv NegInf (Fin n)

exactly :: Integer -> Itv
exactly n = itv n n

mkItv :: Ext -> Ext -> Itv
mkItv a b = if a > b then Bot else Itv a b

-- subset: is every value of the first inside the second?
subItv :: Itv -> Itv -> Bool
subItv Bot _ = True
subItv _ Bot = False
subItv (Itv a b) (Itv c d) = c <= a && b <= d

joinItv :: Itv -> Itv -> Itv
joinItv Bot x = x
joinItv x Bot = x
joinItv (Itv a b) (Itv c d) = Itv (min a c) (max b d)

meetItv :: Itv -> Itv -> Itv
meetItv Bot _ = Bot
meetItv _ Bot = Bot
meetItv (Itv a b) (Itv c d) = mkItv (max a c) (min b d)

member :: Integer -> Itv -> Bool
member _ Bot = False
member n (Itv a b) = Fin n >= a && Fin n <= b

-- extended arithmetic ------------------------------------------------------

addE :: Ext -> Ext -> Ext
addE (Fin a) (Fin b) = Fin (a + b)
addE PosInf NegInf = error "addE: inf - inf"
addE NegInf PosInf = error "addE: inf - inf"
addE PosInf _ = PosInf
addE _ PosInf = PosInf
addE NegInf _ = NegInf
addE _ NegInf = NegInf

negE :: Ext -> Ext
negE NegInf = PosInf
negE PosInf = NegInf
negE (Fin n) = Fin (negate n)

-- 0 * inf = 0 (standard for interval-bound candidates)
mulE :: Ext -> Ext -> Ext
mulE (Fin 0) _ = Fin 0
mulE _ (Fin 0) = Fin 0
mulE (Fin a) (Fin b) = Fin (a * b)
mulE a b =
  let sgn NegInf = -1 :: Int
      sgn PosInf = 1
      sgn (Fin n) = if n < 0 then -1 else 1
   in if sgn a * sgn b > 0 then PosInf else NegInf

iAdd, iSub, iMul :: Itv -> Itv -> Itv
iAdd Bot _ = Bot
iAdd _ Bot = Bot
iAdd (Itv a b) (Itv c d) = Itv (addE a c) (addE b d)
iSub x y = iAdd x (iNeg y)

iNeg :: Itv -> Itv
iNeg Bot = Bot
iNeg (Itv a b) = Itv (negE b) (negE a)

iMul Bot _ = Bot
iMul _ Bot = Bot
iMul (Itv a b) (Itv c d) =
  let cs = [mulE a c, mulE a d, mulE b c, mulE b d]
   in Itv (minimum cs) (maximum cs)

-- Fully conservative division: we make no attempt to be clever.  This is exactly the "won't go further than the domains we defined" boundary -- div simply loses all information.  (A logHalf-style function is the documented false-rejection class.)
iDiv :: Itv -> Itv -> Itv
iDiv Bot _ = Bot
iDiv _ Bot = Bot
iDiv _ _ = top

--------------------------------------------------------------------------
-- 2. The AST
--------------------------------------------------------------------------

data AOp = Add | Sub | Mul | Div deriving (Eq, Show)

data COp = CLt | CLe | CGt | CGe | CEq | CNe deriving (Eq, Show)

-- Fold id lo hi f acc: builtin bounded recursion scheme. iterates i from lo..hi calling f i acc; iteration count is statically bounded by (hi - lo + 1).  The Int is a site id for the implicit calls to f.
data Expr
  = Lit Integer
  | Var String
  | Bin AOp Expr Expr
  | If CExpr Expr Expr
  | Let String Expr Expr
  | Call Int String [Expr] -- Int = call-site id (unique per program)
  | Fold Int Expr Expr String Expr
  deriving (Eq, Show)

-- conditions are comparisons (kept separate so If can do path refinement)
data CExpr = Cmp COp Expr Expr deriving (Eq, Show)

data Safety = SafeFn | UnsafeFn deriving (Eq, Show)

data Param = Param {pName :: String, pPre :: Itv} deriving (Eq, Show)

data FnDef = FnDef {fName :: String, fParams :: [Param], fPost :: Itv, fSafety :: Safety, fBody :: Expr} deriving (Eq, Show)

type Prog = [FnDef]

-- small builders so the demo programs below read cleanly
v :: String -> Expr
v = Var

n :: Integer -> Expr
n = Lit

(.+.), (.-.), (.*.), (./.) :: Expr -> Expr -> Expr
a .+. b = Bin Add a b
a .-. b = Bin Sub a b
a .*. b = Bin Mul a b
a ./. b = Bin Div a b

--------------------------------------------------------------------------
-- 3. Compile-time results
--------------------------------------------------------------------------

-- Where the checker inserted a dynamic guard.
data DynCheck
  = DynPre Int String Int -- call-site id, callee, param index
  | DynPost String -- function needing a runtime post check
  deriving (Eq, Ord, Show)

data Note
  = NStaticPre Int String Int Itv Itv -- site, callee, idx, got, need
  | NDynPre Int String Int Itv Itv
  | NPostProven String Itv Itv -- fn, derived, declared
  | NPostDyn String Itv Itv
  | NTermination String String Integer Integer -- fn, param, step k, lower bound
  | NFoldBound Int String -- site, description
  | NInduction String -- self-call used declared post as IH
  deriving (Eq, Show)

data CompileError
  = ENotTail String Int -- fn, site id of non-tail self-call
  | ENoMeasure String String -- fn, why
  | ESafeCallsUnsafe String String -- caller, callee
  | EMutualRec [String]
  | EUnknownFn String String
  | EArity Int String Int Int
  deriving (Eq, Show)

data Compiled = Compiled {cProg :: Map String FnDef, cDyn :: Set DynCheck, cNotes :: [Note]} deriving (Show)

--------------------------------------------------------------------------
-- 4. Termination / measure / safety checking
--------------------------------------------------------------------------

callsIn :: Expr -> [(Int, String, [Expr])]
callsIn e = case e of
  Lit {} -> []
  Var {} -> []
  Bin _ a b -> callsIn a ++ callsIn b
  If (Cmp _ a b) t f -> callsIn a ++ callsIn b ++ callsIn t ++ callsIn f
  Let _ a b -> callsIn a ++ callsIn b
  Call i f as -> (i, f, as) : concatMap callsIn as
  Fold i lo hi f acc -> (i, f, []) : callsIn lo ++ callsIn hi ++ callsIn acc

-- self-calls of fn in tail position within e?  Returns the set of site
-- ids of self-calls that ARE in tail position.
tailSelfCalls :: String -> Expr -> Set Int
tailSelfCalls me = go
  where
    go e = case e of
      If _ t f -> go t `S.union` go f
      Let _ _ b -> go b -- binding value is NOT tail
      Call i f _ | f == me -> S.singleton i
      _ -> S.empty

allSelfCalls :: String -> Expr -> [(Int, [Expr])]
allSelfCalls me e = [(i, as) | (i, f, as) <- callsIn e, f == me]

-- direct callees (for the transitive-safety / mutual-recursion pass)
callees :: FnDef -> [String]
callees f = nub [g | (_, g, _) <- callsIn (fBody f), g /= fName f]

-- reachable set in the callee graph
reachable :: Map String FnDef -> String -> Set String
reachable env from = go S.empty [from]
  where
    go seen [] = seen
    go seen (x : xs)
      | x `S.member` seen = go seen xs
      | otherwise = case M.lookup x env of
          Nothing -> go (S.insert x seen) xs
          Just fd -> go (S.insert x seen) (callees fd ++ xs)

-- The conservative measure shape, shared by the safety checker and the
-- WCET pass: some param with a finite lower-bound precondition decreases
-- syntactically (p - k, k >= 1) at every self-call.
findMeasure :: FnDef -> Maybe (String, Integer, Integer) -- (param, step k, lower bound)
findMeasure fd =
  let selfs = allSelfCalls (fName fd) (fBody fd)
      ps = fParams fd
      decreasesAt idx (Param nm pre) =
        let steps =
              [ k | (_, as) <- selfs, length as == length ps, Bin Sub (Var x) (Lit k) <- [as !! idx], x == nm, k >= 1
              ]
         in if not (null selfs) && length steps == length selfs
              then case pre of
                Itv (Fin lo) _ -> Just (nm, minimum steps, lo)
                _ -> Nothing
              else Nothing
   in case mapMaybe (\(i, p) -> decreasesAt i p) (zip [0 ..] ps) of
        (m : _) -> Just m
        [] -> Nothing

checkSafety :: Map String FnDef -> FnDef -> Either CompileError [Note]
checkSafety env fd
  | fSafety fd == UnsafeFn = Right [] -- unsafe: no termination obligation
  | otherwise = do
      -- 1. transitive unsafe
      let reach = S.delete (fName fd) (reachable env (fName fd))
      case [g | g <- S.toList reach, Just gd <- [M.lookup g env], fSafety gd == UnsafeFn] of
        (g : _) -> Left (ESafeCallsUnsafe (fName fd) g)
        [] -> Right ()
      -- 2. mutual recursion (cycle through another function)
      let mutual = [g | g <- S.toList reach, fName fd `S.member` reachable env g]
      case mutual of
        (_ : _) -> Left (EMutualRec (fName fd : mutual))
        [] -> Right ()
      -- 3. self-recursion: measure + tail
      let selfs = allSelfCalls (fName fd) (fBody fd)
      if null selfs
        then Right []
        else do
          let tails = tailSelfCalls (fName fd) (fBody fd)
          case [i | (i, _) <- selfs, not (i `S.member` tails)] of
            (i : _) -> Left (ENotTail (fName fd) i)
            [] -> Right ()
          -- find a param that decreases syntactically at EVERY self-call
          case findMeasure fd of
            Just (nm, k, lo) -> Right [NTermination (fName fd) nm k lo]
            Nothing -> Left (ENoMeasure (fName fd) "no parameter with a finite lower-bound precondition decreases syntactically (p - k, k>=1) at every self-call")

--------------------------------------------------------------------------
-- 5. Abstract interpretation: discharge pre/post, insert dynamic checks
--------------------------------------------------------------------------

type AEnv = Map String Itv

data ChkSt = ChkSt {stDyn :: Set DynCheck, stNotes :: [Note]}

emptySt :: ChkSt
emptySt = ChkSt S.empty []

note :: Note -> ChkSt -> ChkSt
note nt st = st {stNotes = stNotes st ++ [nt]}

dyn :: DynCheck -> ChkSt -> ChkSt
dyn d st = st {stDyn = S.insert d (stDyn st)}

-- refine env from a condition (path sensitivity), for the True branch
-- and the False branch respectively.  Only Var-vs-Lit shapes refine.
refine :: CExpr -> AEnv -> (AEnv, AEnv)
refine (Cmp op (Var x) (Lit k)) env = refineVar x op k env
refine (Cmp op (Lit k) (Var x)) env = refineVar x (flipC op) k env
refine _ env = (env, env)

flipC :: COp -> COp
flipC CLt = CGt
flipC CLe = CGe
flipC CGt = CLt
flipC CGe = CLe
flipC CEq = CEq
flipC CNe = CNe

refineVar :: String -> COp -> Integer -> AEnv -> (AEnv, AEnv)
refineVar x op k env =
  let cur = fromMaybe top (M.lookup x env)
      -- x /= k is not an interval in general, but when k is an endpoint
      -- of the CURRENT interval, excluding it tightens the bound:
      -- x in [k,b] /\ x /= k  ==>  x in [k+1,b].
      excl = case cur of
        Itv (Fin a) b | a == k -> mkItv (Fin (k + 1)) b
        Itv a (Fin b) | b == k -> mkItv a (Fin (k - 1))
        _ -> cur
      (tI, fI) = case op of
        CLt -> (atMost (k - 1), atLeast k)
        CLe -> (atMost k, atLeast (k + 1))
        CGt -> (atLeast (k + 1), atMost k)
        CGe -> (atLeast k, atMost (k - 1))
        CEq -> (exactly k, excl)
        CNe -> (excl, exactly k)
      upd i = M.insert x (meetItv cur i) env
   in (upd tI, upd fI)

absEval :: Map String FnDef -> String -> AEnv -> Expr -> ChkSt -> (Itv, ChkSt)
absEval env me aenv e st = case e of
  Lit k -> (exactly k, st)
  Var x -> (fromMaybe top (M.lookup x aenv), st)
  Bin op a b ->
    let (ia, st1) = absEval env me aenv a st
        (ib, st2) = absEval env me aenv b st1
        f = case op of Add -> iAdd; Sub -> iSub; Mul -> iMul; Div -> iDiv
     in (f ia ib, st2)
  If c t f ->
    let (envT, envF) = refine c aenv
        st0 = evalCond c st
        (it, st1) =
          if anyReach envT
            then absEval env me envT t st0
            else (Bot, st0)
        (iff, st2) =
          if anyReach envF
            then absEval env me envF f st1
            else (Bot, st1)
     in (joinItv it iff, st2)
    where
      anyReach en = all (/= Bot) (M.elems en)
      evalCond (Cmp _ a b) s =
        let (_, s1) = absEval env me aenv a s
            (_, s2) = absEval env me aenv b s1
         in s2
  Let x a b ->
    let (ia, st1) = absEval env me aenv a st
     in absEval env me (M.insert x ia aenv) b st1
  Call site f as ->
    case M.lookup f env of
      Nothing -> (top, st) -- unknown fn: arity pass catches it earlier
      Just fd ->
        let step (i, (arg, Param _ pre)) (accSt :: ChkSt) =
              let (ia, s1) = absEval env me aenv arg accSt
               in if ia `subItv` pre
                    then note (NStaticPre site f i ia pre) s1
                    else dyn (DynPre site f i) (note (NDynPre site f i ia pre) s1)
            st1 = foldr step st (reverse (zip [0 ..] (zip as (fParams fd))))
            st2 = if f == me then note (NInduction me) st1 else st1
         in (fPost fd, st2)
  -- KEY MOVE: the call's abstract result is the callee's DECLARED post. This is sound regardless of whether the callee's post was proven statically, because an unproven post got a runtime check inserted -- if control returns, the post held.
  Fold site lo hi f acc ->
    case M.lookup f env of
      Nothing -> (top, st)
      Just fd | length (fParams fd) /= 2 -> (top, st)
      Just fd ->
        let (ilo, st1) = absEval env me aenv lo st
            (ihi, st2) = absEval env me aenv hi st1
            (iac, st3) = absEval env me aenv acc st2
            idxI = case (ilo, ihi) of
              (Itv a _, Itv _ b) -> mkItv a b
              _ -> Bot
            [p0, p1] = fParams fd
            chk i got pre s =
              if got `subItv` pre
                then note (NStaticPre site f i got pre) s
                else dyn (DynPre site f i) (note (NDynPre site f i got pre) s)
            accIn = joinItv iac (fPost fd) -- acc is either initial or a prior result
            st4 = chk 0 idxI (pPre p0) st3
            st5 = chk 1 accIn (pPre p1) st4
            st6 = note (NFoldBound site ("iterations <= hi - lo + 1; bound follows from the range, proven once for Fold, not per use")) st5
         in (joinItv iac (fPost fd), st6)

checkFn :: Map String FnDef -> FnDef -> ChkSt -> ChkSt
checkFn env fd st =
  let aenv = M.fromList [(pName p, pPre p) | p <- fParams fd]
      (ir, st1) = absEval env (fName fd) aenv (fBody fd) st
   in if ir `subItv` fPost fd
        then note (NPostProven (fName fd) ir (fPost fd)) st1
        else
          dyn
            (DynPost (fName fd))
            (note (NPostDyn (fName fd) ir (fPost fd)) st1)

--------------------------------------------------------------------------
-- 5b. Symbolic WCET equations
--------------------------------------------------------------------------
--
-- Every function gets a symbolic worst-case cost equation in units of
-- abstract ops (each arithmetic op, comparison, and call = 1 unit).
--
--   * SAFE functions get a CLOSED FORM in their own parameters.  For a
--     self-recursive safe function the equation is
--         (ceil((p - lo)/k) + 1) * per-call-body-cost
--     -- the call-count factor comes straight from the measure the
--     termination checker already certified (sound because the measure
--     param only decreases, so the initial per-call cost dominates).
--   * UNSAFE functions contribute an OPAQUE term ω(f).  Composition is
--     algebraic: sequencing ADDS ω, iteration (Fold) MULTIPLIES it by
--     the range -- so the equation shows exactly where uncertainty
--     enters and how it scales, instead of collapsing everything to
--     "unknown".

data Cost
  = CN Integer -- constant
  | CP String -- parameter of the function under analysis
  | CO String -- opaque ω(fn): an unsafe function's unknown cost
  | CAdd [Cost]
  | CMul [Cost]
  | CMax [Cost]
  | CDivC Cost Integer -- ceiling-division by a positive constant
  deriving (Eq, Show)

simp :: Cost -> Cost
simp c = case c of
  CAdd xs ->
    let ys = concatMap (flatA . simp) xs
        k = sum [x | CN x <- ys]
        rest = [y | y <- ys, notCN y]
     in wrap (rest ++ [CN k | k /= 0]) (CN 0) CAdd
  CMul xs ->
    let ys = concatMap (flatM . simp) xs
        k = product [x | CN x <- ys]
        rest = [y | y <- ys, notCN y]
     in if k == 0
          then CN 0
          else wrap (rest ++ [CN k | k /= 1]) (CN 1) CMul
  CMax xs ->
    let ys = nub (concatMap (flatX . simp) xs)
        ks = [x | CN x <- ys]
        rest = [y | y <- ys, notCN y]
        kept =
          rest
            ++ [ CN (maximum ks)
                 | not (null ks),
                   null rest || maximum ks > 0
               ]
     in -- costs are nonnegative, so a 0 constant never wins a max
        wrap kept (CN 0) CMax
  CDivC e 1 -> simp e
  CDivC e k -> case simp e of
    CN x -> CN ((x + k - 1) `div` k)
    e' -> CDivC e' k
  _ -> c
  where
    notCN (CN _) = False
    notCN _ = True
    flatA (CAdd ys) = ys; flatA y = [y]
    flatM (CMul ys) = ys; flatM y = [y]
    flatX (CMax ys) = ys; flatX y = [y]
    wrap [] z _ = z
    wrap [y] _ _ = y
    wrap ys _ kf = kf ys

prettyC :: Cost -> String
prettyC = go False
  where
    go _ (CN k) = show k
    go _ (CP p) = p
    go _ (CO f) = "\969(" ++ f ++ ")" -- ω(f)
    go par (CAdd xs) = paren par (intercalate " + " (map (go False) xs))
    go _ (CMul xs) = intercalate "\183" (map (go True) xs) -- ·
    go _ (CMax xs) = "max(" ++ intercalate ", " (map (go False) xs) ++ ")"
    go _ (CDivC e k) = "\8968" ++ go False e ++ "/" ++ show k ++ "\8969"
    paren True s = "(" ++ s ++ ")"
    paren False s = s

substC :: Map String Cost -> Cost -> Cost
substC m = go
  where
    go (CP p) = fromMaybe (CP p) (M.lookup p m)
    go (CAdd xs) = CAdd (map go xs)
    go (CMul xs) = CMul (map go xs)
    go (CMax xs) = CMax (map go xs)
    go (CDivC e k) = CDivC (go e) k
    go x = x

hasParam :: Cost -> Bool
hasParam c = case c of
  CP _ -> True
  CAdd xs -> any hasParam xs
  CMul xs -> any hasParam xs
  CMax xs -> any hasParam xs
  CDivC e _ -> hasParam e
  _ -> False

-- symbolic rendering of an argument expression in terms of the caller's
-- parameters (affine-ish shapes only; anything else is opaque)
symOfE :: Map String Cost -> Expr -> Maybe Cost
symOfE sub e = case e of
  Lit k -> Just (CN k)
  Var x -> M.lookup x sub
  Bin Add a b -> add <$> symOfE sub a <*> symOfE sub b
  Bin Sub a b ->
    (\x y -> add x (CMul [CN (-1), y]))
      <$> symOfE sub a
      <*> symOfE sub b
  Bin Mul a b -> (\x y -> CMul [x, y]) <$> symOfE sub a <*> symOfE sub b
  _ -> Nothing
  where
    add x y = CAdd [x, y]

wcetAll :: Map String FnDef -> Map String Cost
wcetAll env = M.fromList [(nm, wcetFn S.empty fd) | (nm, fd) <- M.toList env]
  where
    wcetFn vis fd
      | fName fd `S.member` vis = CO (fName fd) -- cycle: opaque
      | fSafety fd == UnsafeFn,
        not (null (allSelfCalls (fName fd) (fBody fd))) =
          CO (fName fd) -- uncertified recursion: the whole cost is opaque
      | otherwise =
          let me = fName fd
              vis' = S.insert me vis
              sub0 = M.fromList [(pName p, CP (pName p)) | p <- fParams fd]
              body = costE vis' me sub0 (fBody fd)
           in case (fSafety fd, findMeasure fd) of
                (SafeFn, Just (nm, k, lo)) ->
                  let calls = CAdd [CDivC (CAdd [CP nm, CN (negate lo)]) k, CN 1]
                   in simp (CMul [calls, body])
                _ -> simp body

    costE vis me sub e = case e of
      Lit _ -> CN 0
      Var _ -> CN 0
      Bin _ a b -> CAdd [CN 1, costE vis me sub a, costE vis me sub b]
      If (Cmp _ a b) t f -> CAdd [CN 1, costE vis me sub a, costE vis me sub b, CMax [costE vis me sub t, costE vis me sub f]]
      Let x a b ->
        let xa = fromMaybe (CO "?") (symOfE sub a)
         in CAdd [costE vis me sub a, costE vis me (M.insert x xa sub) b]
      Call _ g as
        | g == me -> CAdd (CN 1 : map (costE vis me sub) as)
        -- self-call: overhead only; the call-count factor multiplies in
        | otherwise -> CAdd (CN 1 : map (costE vis me sub) as ++ [calleeCost vis sub g as])
      Fold _ lo hi g acc ->
        let iters = case (symOfE sub lo, symOfE sub hi) of
              (Just l, Just h) -> CAdd [h, CMul [CN (-1), l], CN 1]
              _ -> CO "?range"
            per = CAdd [CN 1, opaqueParams (calleeW vis g)]
         in CAdd [costE vis me sub lo, costE vis me sub hi, costE vis me sub acc, CMul [iters, per]]

    calleeW vis g = case M.lookup g env of
      Nothing -> CN 0
      Just gd -> wcetFn vis gd

    calleeCost vis sub g as = case M.lookup g env of
      Nothing -> CN 0
      Just gd ->
        let w = wcetFn vis gd
            m = M.fromList (zip (map pName (fParams gd)) (map (\a -> fromMaybe (CO "?") (symOfE sub a)) as))
         in substC m w

    -- Fold applies its callee to loop-varying arguments; if the callee's
    -- cost depends on its own params we can't name them from here, so
    -- they become opaque rather than leaking foreign parameter names.
    opaqueParams w = if hasParam w then opq w else w
      where
        opq (CP _) = CO "?"
        opq (CAdd xs) = CAdd (map opq xs)
        opq (CMul xs) = CMul (map opq xs)
        opq (CMax xs) = CMax (map opq xs)
        opq (CDivC e k) = CDivC (opq e) k
        opq x = x

arityCheck :: Map String FnDef -> FnDef -> Either CompileError ()
arityCheck env fd =
  mapM_ chk (callsIn (fBody fd))
  where
    chk (site, g, as) = case M.lookup g env of
      Nothing -> Left (EUnknownFn (fName fd) g)
      Just gd ->
        let want = length (fParams gd)
            got = length as
         in case fBody gd of
              _
                | isFoldSite site (fBody fd) -> Right () -- Fold passes args implicitly
                | got /= want -> Left (EArity site g want got)
                | otherwise -> Right ()
    isFoldSite site = go
      where
        go e = case e of
          Fold i _ _ _ _ | i == site -> True
          Bin _ a b -> go a || go b
          If (Cmp _ a b) t f -> go a || go b || go t || go f
          Let _ a b -> go a || go b
          Call _ _ as -> any go as
          Fold _ lo hi _ acc -> go lo || go hi || go acc
          _ -> False

compile :: Prog -> Either [CompileError] Compiled
compile prog =
  let env = M.fromList [(fName f, f) | f <- prog]
      arityErrs = [e | f <- prog, Left e <- [arityCheck env f]]
      safeRes = [(f, checkSafety env f) | f <- prog]
      safeErrs = [e | (_, Left e) <- safeRes]
      termNotes = concat [ns | (_, Right ns) <- safeRes]
   in case arityErrs ++ safeErrs of
        (_ : _) -> Left (arityErrs ++ safeErrs)
        [] ->
          let st = foldl (\s f -> checkFn env f s) emptySt {stNotes = termNotes} prog
           in Right (Compiled env (stDyn st) (stNotes st))

--------------------------------------------------------------------------
-- 7. The interpreter (with fuel + inserted dynamic checks)
--------------------------------------------------------------------------

data RtErr
  = RPreViolation String Int Integer Itv -- callee, param idx, value, need
  | RPostViolation String Integer Itv
  | RFuel
  | ROther String
  deriving (Eq, Show)

type VEnv = Map String Integer

runFn :: Compiled -> String -> [Integer] -> Either RtErr Integer
runFn c f args = snd <$> callF (100000 :: Int) f args
  where
    env = cProg c
    dset = cDyn c

    callF fuel g as
      | fuel <= 0 = Left RFuel
      | otherwise = case M.lookup g env of
          Nothing -> Left (ROther ("unknown fn " ++ g))
          Just gd -> do
            let venv = M.fromList (zip (map pName (fParams gd)) as)
            (fuel', r) <- eval (fuel - 1) gd venv (fBody gd)
            -- dynamic postcondition, if the checker inserted one
            if DynPost g `S.member` dset && not (r `member` fPost gd)
              then Left (RPostViolation g r (fPost gd))
              else Right (fuel', r)

    -- dynamic precondition at a specific call site, if inserted
    preGuard site g as = case M.lookup g env of
      Nothing -> Right ()
      Just gd ->
        let ps = fParams gd
            bad = [(i, x, pPre p) | (i, (x, p)) <- zip [0 ..] (zip as ps), DynPre site g i `S.member` dset, not (x `member` pPre p)]
         in case bad of
              ((i, x, need) : _) -> Left (RPreViolation g i x need)
              [] -> Right ()

    eval fuel fd venv e
      | fuel <= 0 = Left RFuel
      | otherwise = case e of
          Lit k -> Right (fuel, k)
          Var x -> case M.lookup x venv of
            Just k -> Right (fuel, k)
            Nothing -> Left (ROther ("unbound " ++ x))
          Bin op a b -> do
            (f1, x) <- eval fuel fd venv a
            (f2, y) <- eval f1 fd venv b
            r <- case op of
              Add -> Right (x + y)
              Sub -> Right (x - y)
              Mul -> Right (x * y)
              Div ->
                if y == 0
                  then Left (ROther "div by zero")
                  else Right (x `div` y)
            Right (f2, r)
          If (Cmp op a b) t f -> do
            (f1, x) <- eval fuel fd venv a
            (f2, y) <- eval f1 fd venv b
            let cond = case op of
                  CLt -> x < y
                  CLe -> x <= y
                  CGt -> x > y
                  CGe -> x >= y
                  CEq -> x == y
                  CNe -> x /= y
            eval f2 fd venv (if cond then t else f)
          Let x a b -> do
            (f1, xv) <- eval fuel fd venv a
            eval f1 fd (M.insert x xv venv) b
          Call site g as -> do
            (f1, vs) <- evalArgs fuel as
            preGuard site g vs
            callF f1 g vs
            where
              evalArgs fu [] = Right (fu, [])
              evalArgs fu (x : xs) = do
                (fu1, xv) <- eval fu fd venv x
                (fu2, rest) <- evalArgs fu1 xs
                Right (fu2, xv : rest)
          Fold site lo hi g acc -> do
            (f1, lov) <- eval fuel fd venv lo
            (f2, hiv) <- eval f1 fd venv hi
            (f3, a0) <- eval f2 fd venv acc
            let go fu i a
                  | i > hiv = Right (fu, a)
                  | otherwise = do
                      preGuard site g [i, a]
                      (fu1, a') <- callF fu g [i, a]
                      go fu1 (i + 1) a'
            go f3 lov a0

--------------------------------------------------------------------------
-- 8. Report printing
--------------------------------------------------------------------------

showParam :: Param -> String
showParam (Param nm i) = nm ++ " : " ++ show i

report :: Prog -> Compiled -> String
report prog c = unlines (concatMap perFn prog)
  where
    wmap = wcetAll (cProg c)
    perFn fd =
      let nm = fName fd
          hdr = nm ++ " (" ++ intercalate ", " (map showParam (fParams fd)) ++ ") -> " ++ show (fPost fd) ++ "   [" ++ (case fSafety fd of SafeFn -> "safe"; UnsafeFn -> "UNSAFE") ++ "]"
          wline = case M.lookup nm wmap of
            Nothing -> []
            Just w -> ["  wcet: " ++ prettyC w ++ " ops" ++ (if hasOmega w then "   (\969 = opaque unsafe cost)" else "")]
          hasOmega x = case x of
            CO _ -> True
            CAdd xs -> any hasOmega xs
            CMul xs -> any hasOmega xs
            CMax xs -> any hasOmega xs
            CDivC e _ -> hasOmega e
            _ -> False
          mine nt = case nt of
            NStaticPre _ g _ _ _ -> ownsCall g nt
            NDynPre _ g _ _ _ -> ownsCall g nt
            NPostProven f _ _ -> f == nm
            NPostDyn f _ _ -> f == nm
            NTermination f _ _ _ -> f == nm
            NInduction f -> f == nm
            NFoldBound site _ -> siteIn site
          ownsCall _ nt' = case nt' of
            NStaticPre site _ _ _ _ -> siteIn site
            NDynPre site _ _ _ _ -> siteIn site
            _ -> False
          siteIn site = site `elem` [i | (i, _, _) <- callsIn (fBody fd)]
          fmt nt = case nt of
            NTermination _ p k lo -> "  termination: measure " ++ p ++ " decreases by >= " ++ show k ++ " per self-call, precondition lower bound " ++ show lo ++ "  ==>  call count <= (" ++ p ++ " - " ++ show lo ++ ")/" ++ show k ++ " + 1   (static WCET shape)"
            NInduction _ -> "  self-call result taken from declared post (induction hypothesis)"
            NStaticPre site g i got need -> "  pre  @site " ++ show site ++ " " ++ g ++ "#arg" ++ show i ++ ": derived " ++ show got ++ " <= required " ++ show need ++ "   STATIC, no runtime check"
            NDynPre site g i got need -> "  pre  @site " ++ show site ++ " " ++ g ++ "#arg" ++ show i ++ ": derived " ++ show got ++ " NOT <= " ++ show need ++ "   ==> DYNAMIC check inserted"
            NPostProven _ got need -> "  post: derived " ++ show got ++ " <= declared " ++ show need ++ "   PROVEN statically"
            NPostDyn _ got need -> "  post: derived " ++ show got ++ " NOT <= declared " ++ show need ++ "   ==> DYNAMIC post check inserted"
            NFoldBound site d -> "  fold @site " ++ show site ++ ": " ++ d
       in (hdr : wline ++ map fmt (filter mine (cNotes c))) ++ [""]

showErr :: CompileError -> String
showErr = \case
  ENotTail f site -> "error[" ++ f ++ "]: self-call at site " ++ show site ++ " is NOT in tail position -- pending work per frame means the cost model is not `calls x per-call`; mark unsafe or rewrite with an accumulator / recursion scheme"
  ENoMeasure f why -> "error[" ++ f ++ "]: cannot certify termination: " ++ why ++ " -- mark unsafe or rewrite as a builtin recursion scheme (Fold)"
  ESafeCallsUnsafe f g -> "error[" ++ f ++ "]: declared safe but (transitively) calls UNSAFE `" ++ g ++ "` -- unsafety is infectious upward; mark " ++ f ++ " unsafe"
  EMutualRec fs -> "error: mutual recursion among safe functions " ++ show fs ++ " -- conservatively rejected (measure would need to be checked across the group)"
  EUnknownFn f g -> "error[" ++ f ++ "]: unknown function " ++ g
  EArity site g w g' -> "error: call at site " ++ show site ++ " to " ++ g ++ ": expected " ++ show w ++ " args, got " ++ show g'

--------------------------------------------------------------------------
-- 9. Demo programs
--------------------------------------------------------------------------

-- (a) REJECTED: classic naive factorial, declared safe.  Two problems:
--     non-tail self-call, and no lower-bound precondition.
factNaiveBad :: Prog
factNaiveBad =
  [ FnDef "fact" [Param "x" top] (atLeast 1) SafeFn $
      If
        (Cmp CEq (v "x") (n 0))
        (n 1)
        (v "x" .*. Call 1 "fact" [v "x" .-. n 1])
  ]

-- (b) REJECTED: safe function calling an unsafe one.
safeCallsUnsafe :: Prog
safeCallsUnsafe =
  [ FnDef "danger" [Param "x" top] top UnsafeFn $
      If (Cmp CEq (v "x") (n 0)) (n 0) (Call 1 "danger" [v "x" ./. n 2]),
    FnDef "wrapper" [Param "x" top] top SafeFn $
      Call 2 "danger" [v "x"]
  ]

-- (c) The main program: everything compiles; the report shows exactly
--     which obligations were proven and which got dynamic checks.
mainProg :: Prog
mainProg =
  [ -- tail-recursive factorial with accumulator.
    -- pre: x >= 0, acc >= 1.  post: result >= 1.
    -- Fully static: termination measure on x, both self-call
    -- preconditions and the post are discharged by the intervals.
    FnDef
      "fact_acc"
      [Param "x" (atLeast 0), Param "acc" (atLeast 1)]
      (atLeast 1)
      SafeFn
      $ If
        (Cmp CEq (v "x") (n 0))
        (v "acc")
        (Call 1 "fact_acc" [v "x" .-. n 1, v "acc" .*. v "x"]),
    -- factorial via the builtin bounded recursion scheme: no
    -- termination analysis needed at all -- Fold's bound is generic.
    FnDef
      "mul_step"
      [Param "i" (atLeast 1), Param "acc" (atLeast 1)]
      (atLeast 1)
      SafeFn
      $ v "i" .*. v "acc",
    FnDef "fact_fold" [Param "x" (atLeast 0)] (atLeast 1) SafeFn $
      Fold 2 (n 1) (v "x") "mul_step" (n 1),
    -- a postcondition the intervals CANNOT prove: claims result >= 10
    -- but the body only supports >= 5.  Compiles -- with a dynamic
    -- post check inserted.  (mystery 20 passes, mystery 3 aborts.)
    FnDef "mystery" [Param "x" (atLeast 0)] (atLeast 10) SafeFn $
      v "x" .+. n 5,
    -- caller with an UNCONSTRAINED parameter calling fact_acc:
    -- x's interval (top) is not <= [0,+inf), so a dynamic
    -- precondition check is inserted at the call site.
    FnDef "use_fact" [Param "x" top] (atLeast 1) SafeFn $
      Call 3 "fact_acc" [v "x", n 1],
    -- caller that ESTABLISHES the precondition by branching first:
    -- the else-branch refines x to [0,+inf) and the same call is
    -- discharged statically.  Path sensitivity doing the work.
    FnDef "use_fact_guarded" [Param "x" top] (atLeast 1) SafeFn $
      If
        (Cmp CLt (v "x") (n 0))
        (n 1)
        (Call 4 "fact_acc" [v "x", n 1]),
    -- constraint propagation through a let: x >= 1 gives y = x + 1 >= 2,
    -- which satisfies fact_acc's acc-precondition (>= 1) statically.
    FnDef "propagate" [Param "x" (atLeast 1)] (atLeast 1) SafeFn $
      Let "y" (v "x" .+. n 1) $
        Call 5 "fact_acc" [v "x", v "y"],
    -- log2-by-halving: genuinely bounded, but n/2 is not the p-k shape
    -- the conservative checker accepts, so it MUST be unsafe.  Its
    -- postcondition is still PROVEN statically -- unsafe only forfeits
    -- the termination/WCET claim, not the pre/post machinery.
    FnDef "log_half" [Param "x" (atLeast 1)] (atLeast 0) UnsafeFn $
      If
        (Cmp CLe (v "x") (n 1))
        (n 0)
        (n 1 .+. Call 6 "log_half" [v "x" ./. n 2]),
    -- unsafe caller of an unsafe function: fine, unsafety only needs
    -- to be acknowledged, not forbidden.
    FnDef "use_log" [Param "x" (atLeast 1)] (atLeast 0) UnsafeFn $
      Call 7 "log_half" [v "x"],
    -- the naive factorial again, this time HONESTLY marked unsafe:
    -- compiles; fact_unsafe (-1) demonstrates what unsafe permits
    -- (divergence -- caught here by fuel).
    FnDef "fact_unsafe" [Param "x" top] (atLeast 1) UnsafeFn $
      If
        (Cmp CEq (v "x") (n 0))
        (n 1)
        (v "x" .*. Call 8 "fact_unsafe" [v "x" .-. n 1]),
    -- an unsafe function INSIDE a Fold: sequencing ADDS the opaque
    -- cost, iteration MULTIPLIES it by the range.  sum_logs' WCET
    -- equation shows x scaling the uncertainty: x*(c + w(log_half)).
    FnDef
      "log_step"
      [Param "i" (atLeast 1), Param "acc" (atLeast 0)]
      (atLeast 0)
      UnsafeFn
      $ v "acc" .+. Call 9 "log_half" [v "i"],
    FnDef "sum_logs" [Param "x" (atLeast 1)] (atLeast 0) UnsafeFn $
      Fold 10 (n 1) (v "x") "log_step" (n 0)
  ]

--------------------------------------------------------------------------
-- 10. main: compile both bad programs (show errors), then the good one
--------------------------------------------------------------------------

runCase :: Compiled -> String -> [Integer] -> String
runCase c f as =
  let call = f ++ "(" ++ intercalate ", " (map show as) ++ ")"
   in case runFn c f as of
        Right r -> "  " ++ call ++ " = " ++ show r
        Left err -> "  " ++ call ++ "  =>  RUNTIME ABORT: " ++ pretty err
  where
    pretty (RPreViolation g i x need) = "dynamic PREcondition check: " ++ g ++ "#arg" ++ show i ++ " = " ++ show x ++ " not in " ++ show need
    pretty (RPostViolation g r need) = "dynamic POSTcondition check: " ++ g ++ " returned " ++ show r ++ " not in " ++ show need
    pretty RFuel = "fuel exhausted (unbounded recursion -- this is what `unsafe` permits)"
    pretty (ROther s) = s

main :: IO ()
main = do
  hSetEncoding stdout utf8
  let line = putStrLn (replicate 72 '-')

  line
  putStrLn "1. naive factorial declared safe  ==>  REJECTED"
  line
  case compile factNaiveBad of
    Left errs -> mapM_ (putStrLn . showErr) errs
    Right _ -> putStrLn "unexpectedly compiled!"

  putStrLn ""
  line
  putStrLn "2. safe function calling an unsafe one  ==>  REJECTED"
  line
  case compile safeCallsUnsafe of
    Left errs -> mapM_ (putStrLn . showErr) errs
    Right _ -> putStrLn "unexpectedly compiled!"

  putStrLn ""
  line
  putStrLn "3. the real program  ==>  compiles; obligations report:"
  line
  case compile mainProg of
    Left errs -> mapM_ (putStrLn . showErr) errs
    Right c -> do
      putStr (report mainProg c)
      line
      putStrLn "runtime demos (fuel = 100000 calls)"
      line
      putStrLn "fully static path (no runtime checks executed at all):"
      putStrLn (runCase c "fact_acc" [10, 1])
      putStrLn (runCase c "fact_fold" [10])
      putStrLn (runCase c "propagate" [5])
      putStrLn ""
      putStrLn "dynamic PREcondition (call-site guard inserted by checker):"
      putStrLn (runCase c "use_fact" [5])
      putStrLn (runCase c "use_fact" [-3])
      putStrLn "  (same call, but guarded by a branch: static, no guard)"
      putStrLn (runCase c "use_fact_guarded" [-3])
      putStrLn ""
      putStrLn "dynamic POSTcondition (unprovable claim, checked at return):"
      putStrLn (runCase c "mystery" [20])
      putStrLn (runCase c "mystery" [3])
      putStrLn ""
      putStrLn "unsafe functions run, but the guarantees are gone:"
      putStrLn (runCase c "use_log" [1024])
      putStrLn (runCase c "sum_logs" [16])
      putStrLn (runCase c "fact_unsafe" [10])
      putStrLn (runCase c "fact_unsafe" [-1])
