{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module NProlog where

import Control.Monad (foldM)
import Control.Monad.Combinators.Expr
import Data.List (intercalate, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Void
import System.IO (BufferMode (..), hFlush, hIsEOF, hSetBuffering, isEOF, stdin, stdout)
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
  Compound "unify" [a, b] -> case unify s a b of
    Just s' -> solveGoals prog s' gs c
    Nothing -> []
  Compound "equiv" [a, b] -> if deepWalk s a == deepWalk s b then solveGoals prog s gs c else []
  Compound "neq" [a, b] -> if deepWalk s a /= deepWalk s b then solveGoals prog s gs c else []
  Compound "gt" [a, b] -> numCompareGoal (>) s a b gs prog c
  Compound "lt" [a, b] -> numCompareGoal (<) s a b gs prog c
  Compound "gte" [a, b] -> numCompareGoal (>=) s a b gs prog c
  Compound "lte" [a, b] -> numCompareGoal (<=) s a b gs prog c
  Compound "is" [lhs, rhs] ->
    let val = evalArith s rhs
     in case val of
          Just v -> case unify s lhs v of
            Just s' -> solveGoals prog s' gs c
            Nothing -> []
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
  | Just s' <- unify s goal hd = solveGoals prog s' (body ++ restGoals) counter'
  | otherwise = []
  where
    (Clause hd body, counter') = freshenClause counter clause

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

solveAll prog goals = map (\s -> Map.filterWithKey (\k _ -> k `elem` queryVars) (fmap (deepWalk s) s)) rawSolutions
  where
    queryVars = nub $ concatMap termVars goals
    rawSolutions = solve prog goals

showSolution s | Map.null s = "true."
showSolution s = intercalate ", " [v ++ " = " ++ show t | (v, t) <- Map.toList s]

runProlog programSrc querySrc = do
  prog <- parseProgram programSrc
  goals <- parseGoals querySrc

  let solns = solveAll prog goals

  if null solns then return ["false."] else return $ map showSolution solns

runPrologRepl = do
  hSetBuffering stdout LineBuffering

  putStrLn "NewProlog REPL. Enter clauses, then query with ?- goal1, goal2."
  putStrLn "Commands: :load <file>, :quit"

  repl []
  where
    repl prog = do
      putStr "?> "
      hFlush stdout

      done <- isEOF

      if done
        then putStrLn "Bye!"
        else do
          line <- getLine

          case line of
            ":quit" -> putStrLn "Bye!"
            _
              | take 5 line == ":load" -> do
                  let file = dropWhile (== ' ') (drop 5 line)
                  contents <- readFile file
                  case parseProgram contents of
                    Left err -> putStrLn ("Parse error: " ++ err) >> repl prog
                    Right prog' -> do
                      putStrLn ("Loaded " ++ show (length prog') ++ " clauses.")
                      repl (prog ++ prog')
              | take 2 line == "?-" -> do
                  let queryStr = dropWhile (== ' ') (drop 2 line)
                  case parseGoals queryStr of
                    Left err -> putStrLn ("Parse error: " ++ err)
                    Right goals -> do
                      let solns = solveAll prog goals
                      if null solns
                        then putStrLn "false."
                        else mapM_ (putStrLn . showSolution) solns
                  repl prog
              | null line -> repl prog
              | otherwise -> do
                  -- Try to parse as a clause
                  case parseProgram line of
                    Left err -> putStrLn ("Parse error: " ++ err)
                    Right clauses -> do
                      putStrLn ("Added " ++ show (length clauses) ++ " clause(s).")
                      repl (prog ++ clauses)
                  repl prog
