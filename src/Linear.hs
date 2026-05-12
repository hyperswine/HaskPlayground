{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Linear where

import Data.Map (Map)
import qualified Data.Map as Map

data Mult = One | Many deriving (Show, Eq)

-- type vars like a, constructors, arrays, pairs, Result, File 1
data Ty = TVar String | TCon String | TArr Ty Ty | TLinear Ty | TPair Ty Ty | TResult Ty deriving (Eq)

instance Show Ty where
  show (TVar v) = v
  show (TCon c) = c
  show (TArr a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (TLinear t) = show t ++ " 1"
  show (TPair a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
  show (TResult t) = "Result " ++ show t

-- Smart-X constructors
tFile, tBytes, tBool, tUnit, tValue :: Ty
tFile = TCon "File"
tBytes = TCon "Bytes"
tBool = TCon "Bool"
tUnit = TCon "()"
tValue = TCon "Value"

tFileLinear :: Ty
tFileLinear = TLinear tFile

tLinearPair :: Ty -> Ty
tLinearPair a = TPair tFileLinear $ TResult a

type Subst = Map String Ty

emptySubst :: Subst
emptySubst = Map.empty

-- Take a substitution env, take a type, return the substituted type like a --> Int
applySubst :: Subst -> Ty -> Ty
applySubst s (TVar v) = maybe (TVar v) (applySubst s) $ Map.lookup v s -- map.lookup returns something --> apply subst to it, otherwise just return a normal type variable
applySubst s (TArr a b) = TArr (applySubst s a) $ applySubst s b -- substitute each element in the array
applySubst s (TLinear t) = TLinear (applySubst s t) -- for linear checking, just normal substitution
applySubst s (TPair a b) = TPair (applySubst s a) $ applySubst s b -- same as TArr incidentally, but TArr may have more recursive cases like more TArr inside b
applySubst s (TResult t) = TResult $ applySubst s t -- Result a --> basically Ok a | Err String
applySubst s t = t -- degenerate case, should never happen

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (applySubst s1) s2 `Map.union` s1 -- subst s1 into s2 + s1 --> gives you s2[s1] + s1

type TyError = String

-- unify two types for checking + inference. Doing most of the equality testing against a and b for any a and b
unify :: Ty -> Ty -> Either TyError Subst
unify (TVar v) t = bindVar v t
unify t (TVar v) = bindVar v t
unify (TCon a) (TCon b) | a == b = Right emptySubst
unify (TCon a) (TCon b) = Left $ "Cannot unify " ++ a ++ " with " ++ b
-- use monadic bind to sequence ops in Either monad, short circuit fast with TyError String
unify (TArr a1 b1) (TArr a2 b2) = do
  s1 <- unify a1 a2
  s2 <- unify (applySubst s1 b1) $ applySubst s1 b2
  Right $ composeSubst s2 s1
-- the unification itself isnt too complex, but the ref checking semantics inside the code is a different problem.
unify (TLinear a) (TLinear b) = unify a b
-- consume linear into unrestricted
unify (TLinear a) b = unify a b
unify a (TLinear b) = unify a b
unify (TPair a1 b1) (TPair a2 b2) = do
  s1 <- unify a1 a2
  s2 <- unify (applySubst s1 b1) $ applySubst s1 b2
  Right (composeSubst s2 s1)
-- Ok a is guarenteed to be the same a, Err String is always String
unify (TResult a) (TResult b) = unify a b
unify a b = Left $ "Cannot unify " ++ show a ++ " with " ++ show b

-- given a type var like "a", bind to some specific type
bindVar :: String -> Ty -> Either TyError Subst
bindVar v (TVar u) | v == u = Right emptySubst
bindVar v t | occursIn v t = Left $ "Infinite type: " ++ v ++ " in " ++ show t -- occurs check, no cycle
bindVar v t = Right $ Map.singleton v t

occursIn :: String -> Ty -> Bool
occursIn v (TVar u) = v == u -- so if "a" == a
occursIn v (TArr a b) = occursIn v a || occursIn v b
occursIn v (TLinear t) = occursIn v t
occursIn v (TPair a b) = occursIn v a || occursIn v b
occursIn v (TResult t) = occursIn v t
occursIn _ _ = False

-- variable reference, literal (always Many), smart |> threading, named function: name, argTy, retTy, with varName : ty { pipeline }, if _ then e1 else e2
data Expr = Var String | Lit Ty | Pipe Expr Expr | PipeFn String Ty Ty | With String Ty Expr | IfExpr Expr Expr Expr deriving (Show)

-- e.g. (File, One, _)
type LinearEnv = Map String (Ty, Mult, Int)

emptyEnv :: LinearEnv
emptyEnv = Map.empty

declare :: String -> Ty -> Mult -> LinearEnv -> LinearEnv
declare name ty mult = Map.insert name (ty, mult, 0)

useVar :: String -> LinearEnv -> Either TyError (Ty, LinearEnv)
useVar name env = case Map.lookup name env of
  Nothing -> Left $ "Unbound variable: '" ++ name ++ "'"
  Just (ty, One, 0) -> Right (ty, Map.insert name (ty, One, 1) env)
  Just (_, One, n) -> Left $ "Linear variable '" ++ name ++ "' used " ++ show (n + 1) ++ " times (must be exactly 1)"
  -- when var has a reference, bump up by one, just that simple
  Just (ty, Many, n) -> Right (ty, Map.insert name (ty, Many, n + 1) env)

-- check references are exactly 1 in scope for linear value
checkConsumed :: [String] -> LinearEnv -> Either TyError ()
checkConsumed names env = mapM_ go names
  where
    go name = case Map.lookup name env of
      Just (_, One, 0) -> Left $ "Linear variable '" ++ name ++ "' was never consumed"
      Just (_, One, n) | n > 1 -> Left $ "Linear variable '" ++ name ++ "' used " ++ show n ++ " times"
      -- Only right if thing was used exactly once, aka was bumped up by 1
      _ -> Right ()

-- notice the do's, a lot of Either plumbing. Map lookup instead of list lookup just to push refs in current scope and check
check :: LinearEnv -> Subst -> Expr -> Either TyError (Ty, LinearEnv, Subst)
check env s (Lit ty) = Right (applySubst s ty, env, s)
check env s (Var name) = do
  (ty, env') <- useVar name env
  Right (applySubst s ty, env', s)
-- the interesting case, for pipes, where we have to ensure it actually works properly
check env s (PipeFn _name argTy retTy) = Right (TArr (applySubst s argTy) $ applySubst s retTy, env, s)
check env s (Pipe e1 e2) = do
  (t1, env1, s1) <- check env s e1
  (t2, env2, s2) <- check env1 s1 e2
  rt <- thread t1 t2
  Right (applySubst s2 rt, env2, s2)
  where
    thread (TPair (TLinear h) (TResult _)) (TArr _ b) = Right $ TPair (TLinear h) $ TResult b -- handle rides, Result threads
    thread (TPair (TLinear h) _) (TArr _ b) = Right $ TPair (TLinear h) b -- handle rides, plain data
    thread (TResult _) (TArr _ b) = Right $ TResult b -- Result short-circuit
    thread _ (TArr _ b) = Right b -- simple pipeline
    thread a b = Left $ "Cannot pipe " ++ show a ++ " into " ++ show b
check env s (With varName resTy pipeline) = do
  let env' = declare varName resTy One env
  (pTy, env'', s') <- check env' s pipeline
  checkConsumed [varName] env''
  Right (stripLinear pTy, env'', s')
  where
    stripLinear (TPair (TLinear _) r) = r
    stripLinear t = t
-- check both branches independently, both need to consume once exactly
check env s (IfExpr _cond e1 e2) = do
  (t1, env1, s1) <- check env s e1
  (t2, env2, s2) <- check env s e2 -- same starting env for both branches
  s3 <- unify (applySubst s2 t1) (applySubst s2 t2)
  checkBranchLinear env env1 env2
  Right (applySubst s3 t1, mergeEnvs env1 env2, composeSubst s3 s2)

-- core logic for branch checking
checkBranchLinear :: LinearEnv -> LinearEnv -> LinearEnv -> Either TyError ()
checkBranchLinear orig e1 e2 = mapM_ go (Map.keys orig)
  where
    go name = case Map.lookup name orig of
      Just (_, One, _) ->
        let u1 = usageIn name e1
            u2 = usageIn name e2
         in case (u1, u2) of
              -- both branches bump by 1
              (1, 1) -> Right ()
              (0, _) -> Left $ "Linear var '" ++ name ++ "' not consumed in then-branch"
              (_, 0) -> Left $ "Linear var '" ++ name ++ "' not consumed in else-branch"
              (a, b) -> Left $ "Linear var '" ++ name ++ "' used " ++ show a ++ " vs " ++ show b ++ " times across branches"
      _ -> Right ()
    usageIn name e = case Map.lookup name e of
      Just (_, _, n) -> n
      Nothing -> 0

mergeEnvs :: LinearEnv -> LinearEnv -> LinearEnv
mergeEnvs = Map.unionWith (\(ty, m, n1) (_, _, n2) -> (ty, m, max n1 n2))

run :: String -> LinearEnv -> Expr -> IO ()
run label env expr = do
  putStrLn $ "── " ++ label
  case check env emptySubst expr of
    Left err -> putStrLn $ "  TYPE ERROR : " ++ err
    Right (ty, _, _) -> putStrLn $ "  OK         : " ++ show ty
  putStrLn ""

runU :: String -> Ty -> Ty -> IO ()
runU label a b = do
  putStrLn $ "── " ++ label
  case unify a b of
    Left err -> putStrLn $ "  UNIFY ERROR: " ++ err
    Right s -> putStrLn $ "  OK, subst  : " ++ if Map.null s then "{}" else show s
  putStrLn ""

main :: IO ()
main = do
  putStrLn "Sol Linear HM Type Checker Demo"
  putStrLn "================================\n"

  -- 1. Simple pipeline
  run "bytes |> json.parse  =>  Result Value" (declare "bytes" tBytes Many emptyEnv) (Pipe (Var "bytes") (PipeFn "json.parse" tBytes (TResult tValue)))

  -- 2. Linear pair threading: handle rides, data transforms
  -- json.parse here returns plain Value; the Result wrapper comes from the pair
  run "(File 1, Result Bytes) |> json.parse  =>  (File 1, Value)" (declare "fp" (tLinearPair tBytes) One emptyEnv) (Pipe (Var "fp") (PipeFn "json.parse" tBytes tValue))

  -- 3. with block: f introduced as linear, read consumes it, produces pair,
  --    json.parse transforms data component, with closes handle  =>  OK
  run
    "with f { f |> read |> json.parse }  =>  OK"
    emptyEnv
    ( With
        "f"
        tFileLinear
        ( Pipe
            ( Pipe
                (Var "f")
                (PipeFn "read" tFileLinear (tLinearPair tBytes))
            )
            (PipeFn "json.parse" tBytes tValue)
        )
    )

  -- 4. with block: f declared but never used  =>  FAIL
  run
    "with f { bytes |> json.parse }  =>  f unconsumed, FAIL"
    (declare "bytes" tBytes Many emptyEnv)
    ( With
        "f"
        tFileLinear
        (Pipe (Var "bytes") (PipeFn "json.parse" tBytes (TResult tValue)))
    )

  -- 5. Linear var used twice  =>  FAIL
  run
    "f |> close, then f again  =>  used twice, FAIL"
    (declare "f" tFileLinear One emptyEnv)
    ( Pipe
        (Pipe (Var "f") (PipeFn "close" tFileLinear tUnit))
        (Pipe (Var "f") (PipeFn "close" tFileLinear tUnit))
    )

  -- 6. Unification: File 1 with File  =>  OK (consuming linear)
  runU "File 1  ~  File  (consuming linear, OK)" tFileLinear tFile

  -- 7. Unification: File with Bytes  =>  FAILG
  runU "File  ~  Bytes  (should fail)" tFile tBytes

  -- 8. Unification with TVar: (File 1, Result a) ~ (File 1, Result Bytes)
  runU "(File 1, Result a)  ~  (File 1, Result Bytes)  =>  a := Bytes" (tLinearPair (TVar "a")) (tLinearPair tBytes)

  -- 9. IfExpr: both branches consume f  =>  OK
  run
    "if _ then close f else close f  =>  OK"
    (declare "f" tFileLinear One emptyEnv)
    ( IfExpr
        (Lit tBool)
        (Pipe (Var "f") (PipeFn "close" tFileLinear tUnit))
        (Pipe (Var "f") (PipeFn "close" tFileLinear tUnit))
    )

  -- 10. IfExpr: else branch forgets f  =>  FAIL
  run
    "if _ then close f else ()  =>  else forgets f, FAIL"
    (declare "f" tFileLinear One emptyEnv)
    ( IfExpr
        (Lit tBool)
        (Pipe (Var "f") (PipeFn "close" tFileLinear tUnit))
        (Lit tUnit)
    )

  putStrLn "Done."
