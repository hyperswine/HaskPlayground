{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-
  N Prolog syntax
  lambda prolog like, period terminator
  Strings are "" which are just list of char codes
  Chars with ''
  identifiers lower case
  variables upper case

  Clauses:
    pred Var1 Var2 <- goal1 Var1 (compoundterm1 Var2 Var2), goal2.
    color red.

  Operators:
    Infix only for most, except negative numbers

    +, -, /, *, %, ^, =, !=, ., ==
    1 + 1 --> (plus 1 1)
    x.y --> (access x y)
    x = y --> (unify x y)
    x == y --> (equiv x y)

    etc.

  Numbers:
    1, 3.3, -4.4. Integers and decimals

  Lists:
    [1, 2, 3], [X | Y]

  DCGs:
    mypred A B C -> p A, d B, c C.

  Comments:
    #

  Special things:
    call/2: call(Pred, Args)
    phrase_from_file/2
    phrase_to_file/2

  Semantics:
    Universal modus ponens, typical prolog style semantics of unification and backtracking, DFS search of goals
    Nothing too different
    The + and other things use a CLP like thing by default

  NOTES:
    dont add the other things like assert/1 and cuts or anything
    X + Y is actually meant to be more like X #+ Y across the FD of Integer from 0..INF
-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-# HLINT ignore "Replace case with fromMaybe" #-}

module NProlog where

import qualified Control.Exception as CE
import Control.Monad (foldM)
import Control.Monad.Combinators.Expr
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (dropWhileEnd, intercalate, isPrefixOf, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Data.Void
import Prettyprinter
import Prettyprinter.Render.Terminal
import System.Console.Haskeline
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Term = Atom String | Var String | IntLit Integer | FloatLit Double | CharLit Char | StrLit String | Compound String [Term] | TList [Term] (Maybe Term) deriving (Ord, Eq)

data Clause = Clause {clauseHead :: Term, clauseBody :: [Term]} deriving (Show, Eq)

type Program = [Clause]

instance Show Term where
  show (Atom s) = s
  show (Var s) = s
  show (IntLit n) = show n
  show (FloatLit d) = show d
  show (CharLit c) = ['\'', c, '\'']
  show (StrLit s) = show s
  show (Compound f as) = f ++ " " ++ unwords (map showArg as)
    where
      showArg t@(Compound _ (_ : _)) = "(" ++ show t ++ ")"
      showArg t = show t
  show (TList xs rest) = "[" ++ intercalate ", " (map show xs) ++ maybe "" (\r -> " | " ++ show r) rest ++ "]"

type Parser = Parsec Void String

-- TYPICAL TOKEN - SYMBOL STUFF

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme = L.lexeme sc

symbol = L.symbol sc

-- PARSE

pAtomIdent = lexeme $ do
  c <- lowerChar <|> char '_'
  cs <- many $ alphaNumChar <|> char '_'

  let name = c : cs
  if name == "_" then fail "underscore is a variable" else return name

pVarIdent = lexeme $ do
  c <- upperChar
  cs <- many $ alphaNumChar <|> char '_'

  return $ c : cs

-- notfollowedby returns unit value so it doesnt complain about unused bind
pAnon = lexeme $ do
  char '_'
  notFollowedBy alphaNumChar

  return "_"

pNumber = lexeme $ do
  neg <- option False $ True <$ char '-'

  choice
    [ try $ do
        -- DECIMAL x.y
        whole <- some digitChar
        char '.'
        frac <- some digitChar

        let d = read $ whole ++ "." ++ frac :: Double

        return $ FloatLit $ if neg then negate d else d,
      do
        -- WHOLE NUMBER n
        digits <- some digitChar

        let n = read digits :: Integer

        return $ IntLit $ if neg then negate n else n
    ]

pCharLit = lexeme $ do
  char '\''
  c <- L.charLiteral
  char '\''

  return $ CharLit c

pStrLit = lexeme $ do
  char '"'
  cs <- manyTill L.charLiteral $ char '"'

  return $ StrLit cs

pList = do
  symbol "["

  -- Either another ] or , ... or | ...
  choice
    [ symbol "]" >> return (TList [] Nothing),
      do
        first <- pExpr
        rest <- many $ symbol "," *> pExpr

        let elems = first : rest

        tail' <- optional $ symbol "|" *> pExpr
        symbol "]"

        return $ TList elems tail'
    ]

pAtomicTerm = choice [try pNumber, pCharLit, pStrLit, pList, between (symbol "(") (symbol ")") pExpr, try pCompound, Var <$> (pAnon <|> pVarIdent), Atom <$> pAtomIdent]

pCompound = do
  f <- pAtomIdent
  args <- some pArgTerm

  return $ Compound f args

pArgTerm = choice [try pNumber, pCharLit, pStrLit, pList, between (symbol "(") (symbol ")") pExpr, Var <$> (pAnon <|> pVarIdent), Atom <$> pAtomIdent]

pExpr = makeExprParser pAtomicTerm operatorTable

operatorTable =
  [ [ InfixL $ binOp "^" <$ try (symbol "^")
    ],
    [ InfixL $ binOp "mul" <$ try (symbol "*"),
      InfixL $ binOp "div" <$ try (symbol "/"),
      InfixL $ binOp "mod" <$ try (symbol "%")
    ],
    [ InfixL $ binOp "plus" <$ try (symbol "+"),
      InfixL $ binOp "minus" <$ try (symbol "-")
    ],
    [ InfixN $ binOp "access" <$ try (char '.' *> lookAhead (letterChar <|> char '_'))
    ],
    [ InfixN $ binOp "gte" <$ try (symbol ">="),
      InfixN $ binOp "lte" <$ try (symbol "<="),
      InfixN $ binOp "gt" <$ try (symbol ">" <* notFollowedBy (char '=' <|> char '-')),
      InfixN $ binOp "lt" <$ try (symbol "<" <* notFollowedBy (char '=' <|> char '-'))
    ],
    [ InfixN $ binOp "equiv" <$ try (symbol "=="),
      InfixN $ binOp "neq" <$ try (symbol "!="),
      InfixN $ binOp "unify" <$ try (symbol "=" <* notFollowedBy (char '=')),
      InfixN $ binOp "is" <$ try (string "is" <* notFollowedBy alphaNumChar <* sc)
    ]
  ]
  where
    binOp name l r = Compound name [l, r]

pGoal = pExpr

pGoals = pGoal `sepBy1` symbol ","

pClause = do
  hd <- pExpr

  choice
    [ do
        symbol "->"
        body <- pGoals
        symbol "."

        return $ desugarDCG hd body,
      do
        symbol "<-"
        body <- pGoals
        symbol "."

        return $ Clause hd body,
      do
        symbol "."

        return $ Clause hd []
    ]

desugarDCG hd body = Clause hd' body'
  where
    n = length body
    sVars = [Var $ "S" ++ show i | i <- [0 .. n]]

    addArgs t sa sb = case t of
      Atom f -> Compound f [sa, sb]
      Compound f args -> Compound f $ args ++ [sa, sb]
      _ -> Compound "call" [t, sa, sb]

    s0 = sVars !! 0
    sN = sVars !! n

    hd' = case hd of
      Atom f -> Compound f [s0, sN]
      Compound f args -> Compound f $ args ++ [s0, sN]
      _ -> hd
    body' = zipWith3 addArgs body sVars $ drop 1 sVars

pProgram = sc *> many pClause <* eof

-- CONVERSION FROM PARSEC TO AST

parseProgram input = case parse pProgram "<input>" input of
  Left err -> Left $ errorBundlePretty err
  Right p -> Right p

parseTerm input = case parse (sc *> pExpr <* eof) "<input>" input of
  Left err -> Left $ errorBundlePretty err
  Right t -> Right t

parseGoals input = case parse (sc *> pGoals <* eof) "<input>" input of
  Left err -> Left $ errorBundlePretty err
  Right gs -> Right gs

-- UNIFY

type Subst = Map String Term

emptySubst :: Subst
emptySubst = Map.empty

-- REQUIRED BECAUSE MAP IS RECURSIVE for VARS
walk s (Var v) = case Map.lookup v s of
  Just t -> walk s t
  Nothing -> Var v
walk _ t = t

-- COMPOUNDS AND LISTS can involve vars
deepWalk s t = case walk s t of
  Var v -> Var v
  Atom a -> Atom a
  IntLit n -> IntLit n
  FloatLit d -> FloatLit d
  CharLit c -> CharLit c
  StrLit str -> StrLit str
  Compound f as -> Compound f $ map (deepWalk s) as
  TList xs rest -> TList (map (deepWalk s) xs) (fmap (deepWalk s) rest)

-- OCCURS CHECK TO PREVENT INFINITE LOOPS ON UNIFICATION
occurs s v t = case walk s t of
  Var v' -> v == v'
  Compound _ as -> any (occurs s v) as
  TList xs rest -> any (occurs s v) xs || maybe False (occurs s v) rest
  _ -> False

expandList [] (Just t) = t
expandList [] Nothing = Atom "nil"
expandList (h : hs) r = Compound "cons" [h, expandList hs r]

unify s t1 t2 = unify' s (walk s t1) (walk s t2)

unify' :: Subst -> Term -> Term -> Maybe Subst
-- Variable cases. Anonymous var matches anything
unify' s (Var "_") _ = Just s
unify' s _ (Var "_") = Just s
unify' s (Var v) t | Var v == t = Just s
-- Occurs check
unify' s (Var v) t | occurs s v t = Nothing
unify' s (Var v) t = Just $ Map.insert v t s
unify' s t (Var v) | occurs s v t = Nothing
unify' s t (Var v) = Just $ Map.insert v t s
-- Atom / literal cases
unify' s (Atom a) (Atom b) | a == b = Just s
unify' s (IntLit a) (IntLit b) | a == b = Just s
unify' s (FloatLit a) (FloatLit b) | a == b = Just s
unify' s (CharLit a) (CharLit b) | a == b = Just s
unify' s (StrLit a) (StrLit b) | a == b = Just s
-- Compound terms
unify' s (Compound f1 as1) (Compound f2 as2) | f1 == f2 && length as1 == length as2 = foldM (\s' (a, b) -> unify s' a b) s (zip as1 as2)
-- List unification
unify' s (TList [] Nothing) (TList [] Nothing) = Just s
unify' s (TList (x : xs) rest) (TList (y : ys) rest2) = do s' <- unify s x y; unify s' (TList xs rest) (TList ys rest2)
unify' s (TList [] Nothing) (TList [] (Just t)) = unify s t (TList [] Nothing)
unify' s (TList [] (Just t)) (TList [] Nothing) = unify s t (TList [] Nothing)
unify' s (TList [] (Just t)) (TList ys rest) = unify s t (TList ys rest)
unify' s (TList xs rest) (TList [] (Just t)) = unify s (TList xs rest) t
unify' s (TList xs rest) t2 = unify s (expandList xs rest) t2
unify' s t1 (TList xs rest) = unify s t1 (expandList xs rest)
-- OTHERWISE fail
unify' _ _ _ = Nothing

type FreshCounter = Int

-- Rename all variables in a clause to fresh ones to avoid capture.
freshenClause counter (Clause hd body) = (Clause hd' body', counter')
  where
    allVars = nub $ concatMap termVars (hd : body)
    mapping = zip allVars [counter ..]
    rename = renameVars $ Map.fromList [(v, Var ("_G" ++ show i)) | (v, i) <- mapping]

    counter' = counter + length allVars
    hd' = rename hd
    body' = map rename body

-- COLLECT ALL VAR NAMES IN A TERM
termVars (Var "_") = []
termVars (Var v) = [v]
termVars (Compound _ as) = concatMap termVars as
termVars (TList xs rest) = concatMap termVars xs ++ maybe [] termVars rest
termVars _ = []

renameVars m (Var v) = fromMaybe (Var v) (Map.lookup v m)
renameVars m (Compound f as) = Compound f $ map (renameVars m) as
renameVars m (TList xs rest) = TList (map (renameVars m) xs) (fmap (renameVars m) rest)
renameVars _ t = t

solve prog goals = solveGoals prog emptySubst goals 0

-- all goals satisfied
solveGoals _ s [] _ = [s]
solveGoals prog s (g : gs) c = case walk s g of
  -- Built-in: unify
  Compound "unify" [a, b] -> clpfdUnify prog s a b gs c
  Compound "equiv" [a, b] -> if deepWalk s a == deepWalk s b then solveGoals prog s gs c else []
  Compound "neq" [a, b] -> if deepWalk s a /= deepWalk s b then solveGoals prog s gs c else []
  Compound "gt" [a, b] -> numCompareGoal (>) s a b gs prog c
  Compound "lt" [a, b] -> numCompareGoal (<) s a b gs prog c
  Compound "gte" [a, b] -> numCompareGoal (>=) s a b gs prog c
  Compound "lte" [a, b] -> numCompareGoal (<=) s a b gs prog c
  Compound "is" [lhs, rhs] ->
    let val = evalArith s rhs
     in case val of
          Just v -> case unify s lhs v of Just s' -> solveGoals prog s' gs c; Nothing -> []
          Nothing -> []
  Compound "call" (pred' : args) ->
    let goal = case pred' of
          Atom f -> Compound f args
          Compound f as' -> Compound f (as' ++ args)
          _ -> Compound "call" (pred' : args)
     in solveGoals prog s (goal : gs) c
  Compound "write" [_] -> solveGoals prog s gs c
  Atom "nl" -> solveGoals prog s gs c
  Atom "true" -> solveGoals prog s gs c
  Atom "fail" -> []
  -- Normal goal: try each clause
  goal -> concatMap (tryClause prog s goal gs c) prog

tryClause prog s goal restGoals counter clause
  | Just s' <- unify s (normArithTerm s goal) hd = solveGoals prog s' (body ++ restGoals) counter'
  | otherwise = []
  where
    (Clause hd body, counter') = freshenClause counter clause

-- Eagerly reduce any fully-ground arithmetic sub-expressions in a term.
-- This ensures goals like `factorial (N - 1) R` with N=5 evaluate
-- the sub-expression to `factorial 4 R` before head unification.
normArithTerm :: Subst -> Term -> Term
normArithTerm s t = case deepWalk s t of
  Compound f as
    | f `elem` ["plus", "minus", "mul", "div", "mod", "^"] ->
        let as' = map (normArithTerm s) as
            t' = Compound f as'
         in case evalArith s t' of
              Just v -> v
              Nothing -> t'
  Compound f as -> Compound f (map (normArithTerm s) as)
  TList xs rest -> TList (map (normArithTerm s) xs) (fmap (normArithTerm s) rest)
  t' -> t'

toDouble (IntLit n) = fromIntegral n
toDouble (FloatLit d) = d
toDouble _ = 0 / 0

numCompareGoal cmp s a b gs prog c = case (evalArith s a, evalArith s b) of
  (Just va, Just vb) | cmp (toDouble va) (toDouble vb) -> solveGoals prog s gs c
  _ -> []

evalArith s t = case deepWalk s t of
  IntLit n -> Just $ IntLit n
  FloatLit d -> Just $ FloatLit d
  Compound "plus" [a, b] -> numBinOp (+) (+) s a b
  Compound "minus" [a, b] -> numBinOp (-) (-) s a b
  Compound "mul" [a, b] -> numBinOp (*) (*) s a b
  Compound "div" [a, b] -> numBinOp div (/) s a b
  Compound "mod" [a, b] -> case (evalArith s a, evalArith s b) of
    (Just (IntLit x), Just (IntLit y)) | y /= 0 -> Just $ IntLit $ x `mod` y
    _ -> Nothing
  Compound "^" [a, b] -> case (evalArith s a, evalArith s b) of
    (Just (IntLit x), Just (IntLit y)) | y >= 0 -> Just $ IntLit $ x ^ y
    (Just (FloatLit x), Just (FloatLit y)) -> Just $ FloatLit $ x ** y
    (Just (IntLit x), Just (FloatLit y)) -> Just $ FloatLit $ fromIntegral x ** y
    (Just (FloatLit x), Just (IntLit y)) -> Just $ FloatLit $ x ** fromIntegral y
    _ -> Nothing
  _ -> Nothing

numBinOp iop fop s a b = case (evalArith s a, evalArith s b) of
  (Just (IntLit x), Just (IntLit y)) -> Just $ IntLit $ iop x y
  (Just (FloatLit x), Just (FloatLit y)) -> Just $ FloatLit $ fop x y
  (Just (IntLit x), Just (FloatLit y)) -> Just $ FloatLit $ fop (fromIntegral x) y
  (Just (FloatLit x), Just (IntLit y)) -> Just $ FloatLit $ fop x (fromIntegral y)
  _ -> Nothing

-- ─── CLP(FD) over Integer [0..INF] ──────────────────────────────────────────────────

arithOps :: [String]
arithOps = ["plus", "minus", "mul", "div", "mod", "^"]

-- True when the term is structurally an arithmetic expression (not a bare variable or atom)
isArithTerm :: Term -> Bool
isArithTerm (IntLit _) = True
isArithTerm (FloatLit _) = True
isArithTerm (Compound f [_, _]) = f `elem` arithOps
isArithTerm _ = False

-- Free variable names within an arithmetic expression (call on a deepWalked term)
arithFreeVars :: Term -> [String]
arithFreeVars (Var v) | v /= "_" = [v]
arithFreeVars (Compound f [a, b]) | f `elem` arithOps = arithFreeVars a ++ arithFreeVars b
arithFreeVars _ = []

-- Invert expr to solve for variable v given a target integer. expr must be deepWalked.  Returns Just val if solvable.
invertArith :: Subst -> Term -> String -> Integer -> Maybe Integer
invertArith s t v target = case walk s t of
  Var x | x == v -> Just target
  Var _ -> Nothing
  IntLit _ -> Nothing
  Compound "plus" [a, b] -> case (evalArith s a, evalArith s b) of
    (Just (IntLit va), _) -> invertArith s b v (target - va)
    (_, Just (IntLit vb)) -> invertArith s a v (target - vb)
    _ -> Nothing
  Compound "minus" [a, b] -> case (evalArith s a, evalArith s b) of
    (Just (IntLit va), _) -> invertArith s b v (va - target)
    (_, Just (IntLit vb)) -> invertArith s a v (target + vb)
    _ -> Nothing
  Compound "mul" [a, b] -> case (evalArith s a, evalArith s b) of
    (Just (IntLit va), _) | va /= 0, target `mod` va == 0 -> invertArith s b v (target `div` va)
    (_, Just (IntLit vb)) | vb /= 0, target `mod` vb == 0 -> invertArith s a v (target `div` vb)
    _ -> Nothing
  Compound "div" [a, b] -> case (evalArith s a, evalArith s b) of
    (_, Just (IntLit vb)) | vb /= 0 -> invertArith s a v (target * vb)
    (Just (IntLit va), _) | target /= 0, va `mod` target == 0 -> invertArith s b v (va `div` target)
    _ -> Nothing
  _ -> Nothing

-- Fallback enumeration bound when the range cannot be derived analytically
clpMaxDomain :: Integer
clpMaxDomain = 100

-- Range for the first free variable when the constraint has two or more unknowns
enumerationRange :: Term -> Integer -> [Integer]
enumerationRange expr target = case expr of
  Compound "plus" _ -> [0 .. target]
  Compound "minus" _ -> [0 .. target + clpMaxDomain]
  Compound "mul" _ | target > 0 -> [i | i <- [1 .. target], target `mod` i == 0]
  Compound "mul" _ -> [0 .. clpMaxDomain]
  _ -> [0 .. clpMaxDomain]

-- Resolve the arithmetic constraint  expr = target  against the domain. Returns all solutions via the existing backtracking DFS.
solveArithConstraint :: Program -> Subst -> Term -> Integer -> [Term] -> FreshCounter -> [Subst]
-- solveArithConstraint prog s expr target restGoals c =
--   let expr' = deepWalk s expr
--       fvs = nub (arithFreeVars expr')
--    in case fvs of
--         [] ->
--           case evalArith s expr' of
--             Just (IntLit n) | n == target -> solveGoals prog s restGoals c
--             _ -> []
--         [v] ->
--           case invertArith s expr' v target of
--             Just val | val >= 0 -> solveGoals prog (Map.insert v (IntLit val) s) restGoals c
--             _ -> []
--         _ ->
--           -- Multiple unknowns: enumerate the first, recurse with one fewer unknown
--           let v = head fvs
--               range = enumerationRange expr' target
--            in concatMap
--                 ( \i ->
--                     let s' = Map.insert v (IntLit i) s
--                      in solveArithConstraint prog s' (deepWalk s' expr') target restGoals c
--                 )
--                 range
solveArithConstraint prog s expr target restGoals c = case fvs of
  [] -> case evalArith s expr' of
    Just (IntLit n) | n == target -> solveGoals prog s restGoals c
    _ -> []
  [v] -> case invertArith s expr' v target of
    Just val | val >= 0 -> solveGoals prog (Map.insert v (IntLit val) s) restGoals c
    _ -> []
  -- Multiple unknowns: enumerate the first, recurse with one fewer unknown
  _ -> let v = head fvs; range = enumerationRange expr' target in concatMap (\i -> let s' = Map.insert v (IntLit i) s in solveArithConstraint prog s' (deepWalk s' expr') target restGoals c) range
  where
    expr' = deepWalk s expr
    fvs = nub $ arithFreeVars expr'

-- Smart arithmetic unification: replaces plain structural unify for = goals.
-- - Both sides ground and arithmetic → evaluate and check equality
-- - One side ground integer, other has free vars → algebraic solve (single var)
--   or enumeration over [0..INF] (multiple vars)
-- - Deferred constraints (both sides non-ground) fall back to structural unify,
--   which binds the variable to the expression; the constraint is re-evaluated
--   whenever a subsequent = goal fully resolves both sides.
-- - Non-arithmetic terms fall back to structural unify unchanged.
clpfdUnify :: Program -> Subst -> Term -> Term -> [Term] -> FreshCounter -> [Subst]
clpfdUnify prog s lhs rhs restGoals c =
  let wa = deepWalk s lhs
      wb = deepWalk s rhs
   in if not (isArithTerm wa) && not (isArithTerm wb)
        then case unify s wa wb of
          Just s' -> solveGoals prog s' restGoals c
          Nothing -> []
        else case (evalArith s wa, evalArith s wb) of
          (Just tv1, Just tv2) ->
            if tv1 == tv2 then solveGoals prog s restGoals c else []
          (Nothing, Just (IntLit n)) ->
            solveArithConstraint prog s wa n restGoals c
          (Just (IntLit n), Nothing) ->
            solveArithConstraint prog s wb n restGoals c
          _ ->
            -- Deferred or float: fall back to structural unification
            case unify s wa wb of
              Just s' -> solveGoals prog s' restGoals c
              Nothing -> []

-- solveAll prog goals = map normalizeSoln rawSolutions
--   if not (isArithTerm wa) && not (isArithTerm wb)
--     then case unify s wa wb of
--       Just s' -> solveGoals prog s' restGoals c
--       Nothing -> []
--     else case (evalArith s wa, evalArith s wb) of
--       (Just tv1, Just tv2) -> if tv1 == tv2 then solveGoals prog s restGoals c else []
--       (Nothing, Just (IntLit n)) -> solveArithConstraint prog s wa n restGoals c
--       (Just (IntLit n), Nothing) -> solveArithConstraint prog s wb n restGoals c
--       -- Deferred or float: fall back to structural unification
--       _ -> case unify s wa wb of
--         Just s' -> solveGoals prog s' restGoals c
--         Nothing -> []
--   where
--     wa = deepWalk s lhs
--     wb = deepWalk s rhs

-- Deep-walk all variable references AND evaluate any fully-ground arithmetic
-- subterms, so that deferred constraints like N = 4 * R (solved after R = 2)
-- display as N = 8 rather than N = mul 4 2.
deepEval :: Subst -> Term -> Term
deepEval s t = tryEval (deepWalk s t)
  where
    tryEval t' = case evalArith emptySubst t' of
      Just v  -> v
      Nothing -> case t' of
        Compound f as -> Compound f (map tryEval as)
        TList xs r    -> TList (map tryEval xs) (fmap tryEval r)
        _             -> t'

solveAll prog goals = map (\s -> Map.filterWithKey (\k _ -> k `elem` queryVars) (fmap (deepEval s) s)) rawSolutions
  where
    queryVars = nub $ concatMap termVars goals
    rawSolutions = solve prog goals
    normalizeSoln s = Map.filterWithKey (\k _ -> k `elem` queryVars) $ fmap (evalFully s . deepWalk s) s
    -- Recursively evaluate any ground arithmetic sub-expressions in a value.
    evalFully s t = case t of
      Compound f as
        | f `elem` ["plus", "minus", "mul", "div", "mod", "^"] ->
            let t' = Compound f (map (evalFully s) as)
             in case evalArith s t' of
                  Just v -> v
                  Nothing -> t'
      Compound f as -> Compound f (map (evalFully s) as)
      TList xs rest -> TList (map (evalFully s) xs) (fmap (evalFully s) rest)
      _ -> t

showSolution s | Map.null s = "true."
showSolution s = intercalate ", " [v ++ " = " ++ show t | (v, t) <- Map.toList s]

runProlog programSrc querySrc = do
  prog <- parseProgram programSrc
  goals <- parseGoals querySrc

  let solns = solveAll prog goals

  if null solns then return ["false."] else return $ map showSolution solns

-- ─── Pretty Printing ─────────────────────────────────────────────────────────

renderDoc = unpack . renderStrict . layoutPretty defaultLayoutOptions

docErr msg = annotate (bold <> color Red) (pretty "Error: ") <> annotate (color Red) (pretty msg)

docInfo = annotate (colorDull Cyan) . pretty

docSolution s
  | Map.null s = annotate (bold <> color Green) (pretty "true.")
  | otherwise = hsep $ punctuate comma [annotate (color Yellow) (pretty v) <+> annotate (colorDull White) (pretty "=") <+> annotate (color Cyan) (pretty (show t)) | (v, t) <- Map.toList s]

-- ─── Completion ───────────────────────────────────────────────────────────────

progPreds prog = nub [name | c <- prog, let name = case clauseHead c of Atom f -> f; Compound f _ -> f; _ -> "", not $ null name]

prologComplete progRef input@(leftRev, _) = do
  let left = reverse leftRev
  if ":load " `isPrefixOf` left then completeFilename input else completeWord Nothing " \t,(.?" completeIdent input
  where
    completeIdent word = do
      prog <- readIORef progRef
      let preds = nub (progPreds prog ++ [":quit", ":load"])
      return [simpleCompletion p | p <- preds, word `isPrefixOf` p]

-- ─── REPL ─────────────────────────────────────────────────────────────────────

runPrologRepl = do
  progRef <- newIORef ([] :: Program)

  let settings = (defaultSettings :: Settings IO) {complete = prologComplete progRef, historyFile = Just ".nprolog_history"}

  runInputT settings $ do
    outputStrLn $ renderDoc $ annotate (bold <> color Cyan) (pretty "NewProlog") <> annotate (colorDull White) (pretty " — clauses: ") <> annotate (colorDull Yellow) (pretty "color red.") <+> annotate (colorDull White) (pretty "queries: ") <> annotate (colorDull Yellow) (pretty "color X?")
    outputStrLn $ renderDoc $ annotate (colorDull White) (pretty "Commands: :load <file>   :quit   Ctrl+D")
    loop progRef
  where
    prompt = renderDoc $ annotate (bold <> color Blue) (pretty "?> ")

    loop progRef = do
      mline <- getInputLine prompt

      case mline of
        Nothing -> outputStrLn (renderDoc $ annotate (colorDull Cyan) (pretty "Bye!"))
        Just line -> handleLine progRef (dropWhileEnd (== ' ') line)

    handleLine progRef trimmed = case trimmed of
      ":quit" -> outputStrLn (renderDoc $ annotate (colorDull Cyan) (pretty "Bye!"))
      _
        | take 5 trimmed == ":load" -> do
            let file = dropWhile (== ' ') (drop 5 trimmed)
            result <- liftIO (CE.try (readFile file) :: IO (Either CE.SomeException String))
            case result of
              Left ex -> outputStrLn (renderDoc $ docErr (show ex)) >> loop progRef
              Right contents -> case parseProgram contents of
                Left err -> outputStrLn (renderDoc $ docErr err) >> loop progRef
                Right prog' -> do
                  liftIO $ modifyIORef progRef (++ prog')
                  outputStrLn (renderDoc $ docInfo ("Loaded " ++ show (length prog') ++ " clause(s) from " ++ show file ++ "."))
                  loop progRef
        | not (null trimmed) && last trimmed == '?' -> do
            let queryStr = init trimmed
            case parseGoals queryStr of
              Left err -> outputStrLn (renderDoc $ docErr err) >> loop progRef
              Right goals -> do
                prog <- liftIO (readIORef progRef)
                let solns = solveAll prog goals
                if null solns
                  then outputStrLn (renderDoc $ annotate (bold <> color Red) (pretty "false."))
                  else mapM_ (outputStrLn . renderDoc . docSolution) solns
                loop progRef
        | null trimmed -> loop progRef
        | otherwise -> do
            case parseProgram trimmed of
              Left err -> outputStrLn (renderDoc $ docErr err) >> loop progRef
              Right clauses -> do
                liftIO $ modifyIORef progRef (++ clauses)
                outputStrLn (renderDoc $ docInfo ("Added " ++ show (length clauses) ++ " clause(s)."))
                loop progRef
