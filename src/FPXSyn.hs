{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- Main.hs: FP-RISC parser + desugarer driver
-- Runs the full pipeline: source text → surface AST → core IR

module FPXSyn where

import Data.Void
import Data.Char (isAlphaNum, isAlpha, isUpper, isLower, isDigit)
import Data.List (intercalate, nub, (\\), foldl')
import Data.Maybe (fromMaybe)
import Control.Monad (void, when, foldM)
import qualified Control.Monad.State as S
import qualified Data.Map.Strict as Map
import qualified System.Environment
import qualified System.IO.Error
import qualified System.IO

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- ============================================================
-- Surface AST  (same as before)
-- ============================================================

type Name  = String
type QName = [Name]

data Lit
  = LInt    Integer
  | LFloat  Double
  | LString String
  deriving (Show, Eq)

data Pat
  = PVar    Name
  | PWild
  | PLit    Lit
  | PCon    QName [Pat]
  | PTuple  [Pat]
  | PList   [Pat]
  | PCons   Pat Pat
  | PAs     Name Pat
  deriving (Show, Eq)

data Expr
  = EVar    QName
  | ELit    Lit
  | EApp    Expr Expr
  | EFn     [Pat] Expr
  | ELet    [(Name, Expr)] Expr
  | ECase   Expr [(Pat, Expr)]
  | EIf     Expr Expr Expr
  | ETuple  [Expr]
  | EList   [Expr]
  | ECons   Expr Expr
  | EPipe   Expr Expr
  | EBinOp  Name Expr Expr
  | EUnary  Name Expr
  | ETypeof Expr
  | EAnn    Expr Type
  deriving (Show, Eq)

data Type
  = TVar    Name
  | TCon    QName
  | TApp    Type Type
  | TFun    Type Type
  | TTuple  [Type]
  | TList   Type
  deriving (Show, Eq)

data Decl
  = DUse    Name String
  | DAlias  Name [Name] Type
  | DType   Name [Name] Type
  | DData   Name [Name] [(Name, [Type])]
  | DSig    Name Type
  | DFunc   Name [Pat] Expr
  | DIso    Name Name (Expr, Expr)
  | DEval   Expr
  deriving (Show, Eq)

type Module = [Decl]

-- ============================================================
-- Parser  (same as before)
-- ============================================================

type Parser = Parsec Void String

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

replOutput :: Parser ()
replOutput = do
  _ <- string "#:"
  _ <- takeWhileP Nothing (/= '\n')
  return ()

sc :: Parser ()
sc = L.space space1 lineComment empty

scFull :: Parser ()
scFull = L.space space1 (lineComment <|> replOutput) empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

dot :: Parser ()
dot = void $ symbol "."

semi :: Parser ()
semi = void $ symbol ";"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

keywords :: [String]
keywords =
  [ "fn", "let", "in", "case", "of", "if", "then", "else"
  , "use", "alias", "type", "iso", "typeof"
  ]

keyword :: String -> Parser ()
keyword w = lexeme $ do
  _ <- string w
  notFollowedBy (alphaNumChar <|> char '\'' <|> char '_')

lident :: Parser Name
lident = lexeme $ do
  first <- lowerChar <|> char '_'
  rest  <- many (alphaNumChar <|> char '\'' <|> char '_')
  let w = first : rest
  when (w `elem` keywords) $
    fail $ "keyword " ++ show w ++ " used as identifier"
  return w

uident :: Parser Name
uident = lexeme $ do
  first <- upperChar
  rest  <- many (alphaNumChar <|> char '\'' <|> char '_')
  return (first : rest)

wild :: Parser ()
wild = lexeme $ do
  _ <- char '_'
  notFollowedBy (alphaNumChar <|> char '\'' <|> char '_')

qname :: Parser QName
qname = lexeme $ do
  first <- nameSegment
  rest  <- many (try $ char '.' *> nameSegment)
  return (first : rest)
  where
    nameSegment = do
      h <- letterChar
      t <- many (alphaNumChar <|> char '\'' <|> char '_')
      let w = h : t
      when (w `elem` keywords) $
        fail $ "keyword in qualified name"
      return w

uqname :: Parser QName
uqname = lexeme $ do
  first <- uident'
  rest  <- many (try $ char '.' *> uident')
  return (first : rest)
  where
    uident' = do
      h <- upperChar
      t <- many (alphaNumChar <|> char '\'' <|> char '_')
      return (h : t)

negSign :: Parser Bool
negSign = option False (True <$ char '-' <* notFollowedBy spaceChar)

numLit :: Parser Lit
numLit = lexeme $ do
  neg <- negSign
  try (floatP neg) <|> intP neg

floatP :: Bool -> Parser Lit
floatP neg = do
  n <- L.decimal :: Parser Integer
  _ <- char '.'
  d <- some digitChar
  let f = read (show n ++ "." ++ d) :: Double
  return $ LFloat (if neg then negate f else f)

intP :: Bool -> Parser Lit
intP neg = do
  n <- L.decimal
  return $ LInt (if neg then negate n else n)

strLit :: Parser Lit
strLit = lexeme $ do
  _ <- char '"'
  s <- many strChar
  _ <- char '"'
  return $ LString s
  where
    strChar = (char '\\' *> escChar) <|> satisfy (/= '"')
    escChar = choice
      [ '\n' <$ char 'n', '\t' <$ char 't'
      , '\\' <$ char '\\', '"' <$ char '"' ]

lit :: Parser Lit
lit = try numLit <|> strLit

pType :: Parser Type
pType = do
  t <- pTypeApp
  option t $ do
    _ <- symbol "->"
    TFun t <$> pType

pTypeApp :: Parser Type
pTypeApp = do
  ts <- some pTypeAtom
  return $ foldl1 TApp ts

pTypeAtom :: Parser Type
pTypeAtom = choice
  [ try (TTuple <$> parens ((:) <$> pType <*> some (symbol "," *> pType)))
  , parens pType
  , TList <$> brackets pType
  , try (TCon <$> uqname)
  , TVar <$> lident
  ]

pPat :: Parser Pat
pPat = pPatCons

pPatCons :: Parser Pat
pPatCons = do
  p <- pPatApp
  option p $ do
    _ <- symbol "::"
    PCons p <$> pPatCons

pPatApp :: Parser Pat
pPatApp =
  try (do
    con <- uqname
    args <- many pPatAtom
    return $ PCon con args)
  <|> pPatAtom

pPatAtom :: Parser Pat
pPatAtom = choice
  [ PWild  <$ try wild
  , PLit   <$> try lit
  , try (PTuple <$> parens ((:) <$> pPat <*> some (symbol "," *> pPat)))
  , try (parens pPat)
  , PList  <$> brackets (pPat `sepBy` symbol ",")
  , try (PCon <$> uqname <*> pure [])
  , PVar   <$> lident
  ]

pExpr :: Parser Expr
pExpr = pPipe

pPipe :: Parser Expr
pPipe = do
  e <- pBinOp
  rest e
  where
    rest e = option e $ do
      _ <- symbol "|>"
      e' <- pBinOp
      rest (EPipe e e')

pBinOp :: Parser Expr
pBinOp = pCons

pCons :: Parser Expr
pCons = do
  e <- pOr
  option e $ do
    _ <- symbol "::"
    ECons e <$> pCons

pOr :: Parser Expr
pOr = chainL pAnd (EBinOp "||" <$ symbol "||")

pAnd :: Parser Expr
pAnd = chainL pCmp (EBinOp "&&" <$ symbol "&&")

pCmp :: Parser Expr
pCmp = do
  e <- pAdd
  option e $ do
    op <- choice (map (\s -> s <$ try (symbol s)) ["==", "/=", "<=", ">=", "<", ">"])
    EBinOp op e <$> pAdd

pAdd :: Parser Expr
pAdd = chainL pMul $ choice
  [ EBinOp "+" <$ try (symbol "+" <* notFollowedBy (char '+'))
  , EBinOp "-" <$ symbol "-"
  , EBinOp "++" <$ symbol "++"
  ]

pMul :: Parser Expr
pMul = chainL pApp $ choice
  [ EBinOp "*" <$ symbol "*"
  , EBinOp "/" <$ symbol "/"
  ]

chainL :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr
chainL term op = do
  e <- term
  rest e
  where
    rest e = option e $ do
      f <- op
      e' <- term
      rest (f e e')

pApp :: Parser Expr
pApp = do
  es <- some pAtom
  return $ foldl1 EApp es

pAtom :: Parser Expr
pAtom = choice
  [ pFn, pLet, pCase, pIf, pTypeof
  , try pOpSection
  , try (ETuple <$> parens ((:) <$> pExpr <*> some (symbol "," *> pExpr)))
  , try (parens pExpr)
  , EList <$> brackets (pExpr `sepBy` symbol ",")
  , ELit  <$> try lit
  , EVar  <$> try qname
  ]

pOpSection :: Parser Expr
pOpSection = parens $ do
  op <- choice $ map (\s -> s <$ try (symbol s))
    [ "++", "==", "/=", "<=", ">=", "&&", "||", "::"
    , "+", "-", "*", "/", "<", ">" ]
  -- Optional right argument: (* 2) means (fn x => x * op 2), sugar for partial app
  marg <- optional pAtom
  return $ case marg of
    Nothing  -> EVar [op]
    Just arg -> EApp (EVar [op]) arg

pFn :: Parser Expr
pFn = do
  keyword "fn"
  pats <- some pPatAtom
  _ <- symbol "=>"
  EFn pats <$> pExpr

pLet :: Parser Expr
pLet = do
  keyword "let"
  bindings <- some $ try $ do
    n <- lident
    _ <- symbol "="
    e <- pExpr
    semi
    return (n, e)
  body <- pExpr
  return $ ELet bindings body

pCase :: Parser Expr
pCase = do
  keyword "case"
  e <- pExpr
  keyword "of"
  optional (symbol "|")
  first <- pCaseArm
  rest  <- many (symbol "|" *> pCaseArm)
  return $ ECase e (first : rest)

pCaseArm :: Parser (Pat, Expr)
pCaseArm = do
  p <- pPat
  _ <- symbol "->"
  body <- pExpr
  return (p, body)

pIf :: Parser Expr
pIf = do
  keyword "if"
  c <- pExpr
  keyword "then"
  t <- pExpr
  keyword "else"
  EIf c t <$> pExpr

pTypeof :: Parser Expr
pTypeof = do
  keyword "typeof"
  ETypeof <$> pAtom

pDecl :: Parser Decl
pDecl = choice
  [ try pDeclEval, try pDeclUse, try pDeclAlias, try pDeclType
  , try pDeclData, try pDeclIso, try pDeclSig, pDeclFunc
  ]

pDeclEval :: Parser Decl
pDeclEval = do
  _ <- symbol ">"
  e <- pExpr
  dot
  return $ DEval e

pDeclUse :: Parser Decl
pDeclUse = do
  n <- lident
  _ <- symbol "="
  keyword "use"
  s <- lexeme $ do
    _ <- char '"'
    v <- many (satisfy (/= '"'))
    _ <- char '"'
    return v
  dot
  return $ DUse n s

pDeclAlias :: Parser Decl
pDeclAlias = do
  keyword "alias"
  name <- uident
  tvars <- many lident
  _ <- symbol "="
  t <- pType
  dot
  return $ DAlias name tvars t

pDeclType :: Parser Decl
pDeclType = do
  keyword "type"
  name <- uident
  tvars <- many lident
  _ <- symbol "="
  t <- pType
  dot
  return $ DType name tvars t

pDeclData :: Parser Decl
pDeclData = do
  name  <- uident
  tvars <- many lident
  _ <- symbol "="
  cons <- pConDef `sepBy1` symbol "|"
  dot
  return $ DData name tvars cons
  where
    pConDef = do
      con  <- uident
      args <- many pTypeAtom
      return (con, args)

pDeclIso :: Parser Decl
pDeclIso = do
  keyword "iso"
  a <- uident
  b <- uident
  _ <- symbol "="
  (ea, eb) <- parens $ do
    x <- pExpr
    _ <- symbol ","
    y <- pExpr
    return (x, y)
  dot
  return $ DIso a b (ea, eb)

pDeclSig :: Parser Decl
pDeclSig = do
  n <- lident
  _ <- symbol ":"
  t <- pType
  dot
  return $ DSig n t

pDeclFunc :: Parser Decl
pDeclFunc = do
  name <- lident
  pats <- many pPatAtom
  _ <- symbol "="
  body <- pExpr
  dot
  return $ DFunc name pats body

pModule :: Parser Module
pModule = do
  scFull
  decls <- many (scFull *> pDecl <* scFull)
  eof
  return decls

-- ============================================================
-- Core IR
-- ============================================================

data CoreLit = CLInt Integer | CLFloat Double | CLString String
  deriving (Show, Eq)

data CorePat
  = CPVar  Name
  | CPWild
  | CPLit  CoreLit
  | CPCon  QName [Name]
  | CPTuple [Name]
  deriving (Show, Eq)

data CoreExpr
  = CVar    QName
  | CLit    CoreLit
  | CApp    CoreExpr CoreExpr
  | CLam    Name CoreExpr
  | CLet    Name CoreExpr CoreExpr
  | CCase   CoreExpr [(CorePat, CoreExpr)]
  | CTypeof CoreExpr
  | CAnn    CoreExpr CoreType
  deriving (Show, Eq)

data CoreType
  = CTVar  Name
  | CTCon  QName
  | CTApp  CoreType CoreType
  | CTFun  CoreType CoreType
  | CTTuple [CoreType]
  deriving (Show, Eq)

data CoreDecl
  = CDData  Name [Name] [(Name, [CoreType])]
  | CDAlias Name [Name] CoreType
  | CDType  Name [Name] CoreType
  | CDSig   Name CoreType
  | CDDef   Name CoreExpr
  | CDIso   Name Name (CoreExpr, CoreExpr)
  | CDUse   Name String
  | CDEval  CoreExpr
  deriving (Show, Eq)

type CoreModule = [CoreDecl]

-- ============================================================
-- Fresh name supply
-- ============================================================

type DS a = S.State Int a

fresh :: String -> DS Name
fresh hint = do
  n <- S.get
  S.put (n + 1)
  return $ "$" ++ hint ++ show n

runDS :: DS a -> a
runDS m = S.evalState m 0

-- ============================================================
-- Type desugaring
-- ============================================================

desugarType :: Type -> CoreType
desugarType (TVar n)    = CTVar n
desugarType (TCon q)    = CTCon q
desugarType (TApp f x)  = CTApp (desugarType f) (desugarType x)
desugarType (TFun a b)  = CTFun (desugarType a) (desugarType b)
desugarType (TTuple ts) = CTTuple (map desugarType ts)
desugarType (TList t)   = CTApp (CTCon ["List"]) (desugarType t)

-- ============================================================
-- Literal conversion
-- ============================================================

desugarLit :: Lit -> CoreLit
desugarLit (LInt n)    = CLInt n
desugarLit (LFloat f)  = CLFloat f
desugarLit (LString s) = CLString s

-- ============================================================
-- Pattern desugaring
-- ============================================================

-- Returns (flat CorePat, wrapper that handles sub-pattern matching)
desugarPat :: Pat -> DS (CorePat, CoreExpr -> DS CoreExpr)

desugarPat (PVar n) = return (CPVar n, return)

desugarPat PWild = do
  v <- fresh "w"
  return (CPVar v, return)

desugarPat (PLit l) = return (CPLit (desugarLit l), return)

desugarPat (PCon q subpats) = do
  args <- mapM (const (fresh "p")) subpats
  wrappers <- mapM (uncurry makeSubWrapper) (zip subpats args)
  let wrap body = foldrM applyWrapper body (reverse wrappers)
  return (CPCon q args, wrap)
  where
    makeSubWrapper sp arg = do
      (cp, w) <- desugarPat sp
      return $ \body -> do
        inner <- w body
        return $ CCase (CVar [arg]) [(cp, inner)]
    applyWrapper f acc = f acc

desugarPat (PTuple subpats) = do
  args <- mapM (const (fresh "t")) subpats
  wrappers <- mapM (uncurry makeSubWrapper) (zip subpats args)
  let wrap body = foldrM applyWrapper body (reverse wrappers)
  return (CPTuple args, wrap)
  where
    makeSubWrapper sp arg = do
      (cp, w) <- desugarPat sp
      return $ \body -> do
        inner <- w body
        return $ CCase (CVar [arg]) [(cp, inner)]
    applyWrapper f acc = f acc

desugarPat (PList []) =
  return (CPCon ["List","Nil"] [], return)
desugarPat (PList (p:ps)) =
  desugarPat (PCons p (PList ps))

desugarPat (PCons hpat tpat) = do
  hname <- fresh "h"
  tname <- fresh "t"
  (hcp, hwrap) <- desugarPat hpat
  (tcp, twrap) <- desugarPat tpat
  let wrap body = do
        hbody  <- hwrap body
        let hcase = CCase (CVar [hname]) [(hcp, hbody)]
        tbodied <- twrap hcase
        return $ CCase (CVar [tname]) [(tcp, tbodied)]
  return (CPCon ["List","Cons"] [hname, tname], wrap)

desugarPat (PAs n pat) = do
  (cp, wrap) <- desugarPat pat
  let wrap' body = do
        inner <- wrap body
        v <- fresh "as"
        return $ CCase (CVar [v]) [(cp, CLet n (CVar [v]) inner)]
  return (CPVar n, wrap')

-- ============================================================
-- Expression desugaring
-- ============================================================

desugarExpr :: Expr -> DS CoreExpr
desugarExpr (EVar q)     = return $ CVar q
desugarExpr (ELit l)     = return $ CLit (desugarLit l)
desugarExpr (EAnn e t)   = CAnn <$> desugarExpr e <*> pure (desugarType t)
desugarExpr (ETypeof e)  = CTypeof <$> desugarExpr e

-- Application
desugarExpr (EApp f x)   = CApp <$> desugarExpr f <*> desugarExpr x

-- Pipe: e |> f  →  f e
desugarExpr (EPipe e f)  = do
  e' <- desugarExpr e
  f' <- desugarExpr f
  return $ CApp f' e'

-- BinOp: x + y  →  CApp (CApp (CVar ["+"]) x) y
desugarExpr (EBinOp op l r) = do
  l' <- desugarExpr l
  r' <- desugarExpr r
  return $ CApp (CApp (CVar [op]) l') r'

-- Unary: -x  →  negate x
desugarExpr (EUnary op e) =
  CApp (CVar [op]) <$> desugarExpr e

-- Tuple: (e1,e2,e3)  →  Tuple.3 e1 e2 e3
desugarExpr (ETuple es) = do
  es' <- mapM desugarExpr es
  return $ foldl CApp (CVar ["Tuple", show (length es)]) es'

-- List: [a,b,c]  →  List.Cons a (List.Cons b (List.Cons c List.Nil))
desugarExpr (EList es) = do
  es' <- mapM desugarExpr es
  return $ foldr (\x acc -> CApp (CApp (CVar ["List","Cons"]) x) acc)
                 (CVar ["List","Nil"])
                 es'

-- Cons: h :: t  →  List.Cons h t
desugarExpr (ECons h t) = do
  h' <- desugarExpr h
  t' <- desugarExpr t
  return $ CApp (CApp (CVar ["List","Cons"]) h') t'

-- if c then t else e  →  case $v of True -> t | False -> e
desugarExpr (EIf c t e) = do
  c' <- desugarExpr c
  t' <- desugarExpr t
  e' <- desugarExpr e
  v  <- fresh "if"
  return $ CLet v c' $
    CCase (CVar [v])
      [ (CPCon ["True"]  [], t')
      , (CPCon ["False"] [], e')
      ]

-- let bindings  →  nested single CLet bindings
desugarExpr (ELet bindings body) = do
  body' <- desugarExpr body
  foldrM (\(n, e) acc -> do
    e' <- desugarExpr e
    return $ CLet n e' acc)
    body' bindings

-- fn pats => body  →  nested single-arg lambdas with case for patterns
desugarExpr (EFn pats body) = do
  body' <- desugarExpr body
  foldrM desugarFnArg body' pats

-- case e of arms
desugarExpr (ECase scrut arms) = do
  scrut' <- desugarExpr scrut
  v      <- fresh "scrut"
  arms'  <- mapM (desugarArm (CVar [v])) arms
  return $ CLet v scrut' (CCase (CVar [v]) arms')

-- ---- Helpers ----

desugarFnArg :: Pat -> CoreExpr -> DS CoreExpr
desugarFnArg (PVar n) body = return $ CLam n body
desugarFnArg PWild    body = CLam <$> fresh "w" <*> pure body
desugarFnArg pat      body = do
  v <- fresh "arg"
  (cp, wrap) <- desugarPat pat
  wrapped <- wrap body
  return $ CLam v (CCase (CVar [v]) [(cp, wrapped)])

desugarArm :: CoreExpr -> (Pat, Expr) -> DS (CorePat, CoreExpr)
desugarArm _ (pat, body) = do
  (cp, wrap) <- desugarPat pat
  body'      <- desugarExpr body
  wrapped    <- wrap body'
  return (cp, wrapped)

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ z []     = return z
foldrM f z (x:xs) = do { rest <- foldrM f z xs; f x rest }

-- ============================================================
-- Multi-clause function merging
-- ============================================================

groupClauses :: Module -> [Either Decl [(Name, [Pat], Expr)]]
groupClauses [] = []
groupClauses (DFunc n pats body : rest) =
  let (same, other) = span (isClause n (length pats)) rest
      clauses = (n, pats, body) : [(n', ps, b) | DFunc n' ps b <- same]
  in Right clauses : groupClauses other
groupClauses (d:rest) = Left d : groupClauses rest

isClause :: Name -> Int -> Decl -> Bool
isClause n arity (DFunc n' pats _) = n' == n && length pats == arity
isClause _ _ _                     = False

mergeClauses :: [(Name, [Pat], Expr)] -> DS CoreDecl
mergeClauses [] = error "mergeClauses: empty"
mergeClauses clauses@((name, pats, _):_) = do
  let arity = length pats
  if arity == 0
    then do
      let (_, _, body) = head clauses
      CDDef name <$> desugarExpr body
    else do
      argNames <- mapM (\i -> fresh ("a" ++ show i)) [0..arity-1]
      -- Each clause becomes a row of patterns → body.
      -- We build a single case per arg position, nesting left to right.
      -- For each clause: desugar all pats against the argNames, nest cases.
      arms <- mapM (clauseToArm argNames) clauses
      -- Outermost case is on the first arg; inner cases are nested in bodies.
      let body = CCase (CVar [head argNames]) arms
      return $ CDDef name (foldr CLam body argNames)

-- Turn one clause into a case arm for the first argument,
-- with the remaining argument patterns handled by nested cases in the body.
clauseToArm :: [Name] -> (Name, [Pat], Expr) -> DS (CorePat, CoreExpr)
clauseToArm argNames (_, pats, body) = do
  body' <- desugarExpr body
  -- Process args right-to-left: innermost case first
  let (firstPat:restPats) = pats
      (firstArg:restArgs) = argNames
  -- Wrap body with nested cases for args 1..n
  inner <- foldrM (\(arg, pat) acc -> do
    (cp, wrap) <- desugarPat pat
    wrapped <- wrap acc
    return $ CCase (CVar [arg]) [(cp, wrapped)])
    body'
    (zip restArgs restPats)
  -- The outermost arm is on the first arg
  (cp, wrap) <- desugarPat firstPat
  finalBody <- wrap inner
  return (cp, finalBody)

-- ============================================================
-- Top-level desugaring
-- ============================================================

desugarDecl :: Decl -> DS [CoreDecl]
desugarDecl (DUse n s)         = return [CDUse n s]
desugarDecl (DAlias n vs t)    = return [CDAlias n vs (desugarType t)]
desugarDecl (DType n vs t)     = return [CDType n vs (desugarType t)]
desugarDecl (DData n vs cons)  = return [CDData n vs (map (fmap (map desugarType)) cons)]
desugarDecl (DSig n t)         = return [CDSig n (desugarType t)]
desugarDecl (DIso a b (e1,e2)) = do
  e1' <- desugarExpr e1
  e2' <- desugarExpr e2
  return [CDIso a b (e1', e2')]
desugarDecl (DEval e)          = (:[]) . CDEval <$> desugarExpr e
desugarDecl (DFunc {})         = error "desugarDecl: DFunc via groupClauses only"

desugarModule :: Module -> CoreModule
desugarModule decls = runDS $ do
  let groups = groupClauses decls
  fmap concat $ mapM processGroup groups
  where
    processGroup (Left d)        = desugarDecl d
    processGroup (Right clauses) = (:[]) <$> mergeClauses clauses

-- ============================================================
-- PASS 1: Simplify
-- Eliminate trivial identity-case and dead/inlineable lets
-- ============================================================

simplifyExpr :: CoreExpr -> CoreExpr
simplifyExpr (CCase scrut [(CPVar n, body)]) =
  simplifyExpr (CLet n scrut body)
simplifyExpr (CLet n rhs body)
  | CVar [n'] <- rhs, n' == n  = simplifyExpr body
  | not (occursIn n body)       = simplifyExpr body
  | CVar _ <- rhs               = substExpr n rhs (simplifyExpr body)
  | CLit _ <- rhs               = substExpr n rhs (simplifyExpr body)
  | otherwise                   = CLet n (simplifyExpr rhs) (simplifyExpr body)
simplifyExpr (CApp f x)         = CApp (simplifyExpr f) (simplifyExpr x)
simplifyExpr (CLam n e)         = CLam n (simplifyExpr e)
simplifyExpr (CCase e arms)     = CCase (simplifyExpr e)
                                    [(p, simplifyExpr b) | (p,b) <- arms]
simplifyExpr (CTypeof e)        = CTypeof (simplifyExpr e)
simplifyExpr (CAnn e t)         = CAnn (simplifyExpr e) t
simplifyExpr e                  = e

occursIn :: Name -> CoreExpr -> Bool
occursIn n (CVar q)         = q == [n]
occursIn n (CApp f x)       = occursIn n f || occursIn n x
occursIn n (CLam v e)       = v /= n && occursIn n e
occursIn n (CLet v r b)     = occursIn n r || (v /= n && occursIn n b)
occursIn n (CCase e arms)   = occursIn n e ||
  any (\(p,b) -> n `notElem` patBound p && occursIn n b) arms
occursIn n (CTypeof e)      = occursIn n e
occursIn n (CAnn e _)       = occursIn n e
occursIn _ _                = False

patBound :: CorePat -> [Name]
patBound (CPVar n)    = [n]
patBound (CPCon _ ns) = ns
patBound (CPTuple ns) = ns
patBound _            = []

substExpr :: Name -> CoreExpr -> CoreExpr -> CoreExpr
substExpr n repl (CVar q)
  | q == [n]  = repl
  | otherwise = CVar q
substExpr n repl (CApp f x)     = CApp (substExpr n repl f) (substExpr n repl x)
substExpr n repl (CLam v e)
  | v == n    = CLam v e
  | otherwise = CLam v (substExpr n repl e)
substExpr n repl (CLet v r b)
  | v == n    = CLet v (substExpr n repl r) b
  | otherwise = CLet v (substExpr n repl r) (substExpr n repl b)
substExpr n repl (CCase e arms) = CCase (substExpr n repl e)
  [(p, if n `elem` patBound p then b else substExpr n repl b) | (p,b) <- arms]
substExpr n repl (CTypeof e)    = CTypeof (substExpr n repl e)
substExpr n repl (CAnn e t)     = CAnn (substExpr n repl e) t
substExpr _ _ e                 = e

simplifyDecl :: CoreDecl -> CoreDecl
simplifyDecl (CDDef n e) = CDDef n (simplifyExpr e)
simplifyDecl (CDEval e)  = CDEval (simplifyExpr e)
simplifyDecl d           = d

simplifyModule :: CoreModule -> CoreModule
simplifyModule = map simplifyDecl

-- ============================================================
-- PASS 2: Type Inference (Hindley-Milner Algorithm W)
-- ============================================================

-- Type representation
type TyVar = Name

data Ty
  = TyVar   TyVar
  | TyCon   QName
  | TyApp   Ty Ty
  | TyFun   Ty Ty
  | TyTuple [Ty]
  deriving (Show, Eq)

-- Type scheme
data Scheme = Scheme [TyVar] Ty deriving (Show, Eq)

-- Typed expression — every node carries its type
data TExpr
  = TEVar    QName               Ty
  | TELit    CoreLit             Ty
  | TEApp    TExpr TExpr         Ty
  | TELam    Name  TExpr         Ty
  | TELet    Name  TExpr TExpr   Ty
  | TECase   TExpr [(CorePat, TExpr)] Ty
  | TETypeof TExpr               Ty
  deriving (Show, Eq)

typeOf :: TExpr -> Ty
typeOf (TEVar  _ t)     = t
typeOf (TELit  _ t)     = t
typeOf (TEApp  _ _ t)   = t
typeOf (TELam  _ _ t)   = t
typeOf (TELet  _ _ _ t) = t
typeOf (TECase _ _ t)   = t
typeOf (TETypeof _ t)   = t

-- Smart constructors
tyInt, tyFloat, tyString, tyBool :: Ty
tyInt    = TyCon ["Int"]
tyFloat  = TyCon ["Float"]
tyString = TyCon ["String"]
tyBool   = TyCon ["Bool"]

tyList :: Ty -> Ty
tyList t = TyApp (TyCon ["List"]) t

tyFun :: [Ty] -> Ty -> Ty
tyFun []     r = r
tyFun (a:as) r = TyFun a (tyFun as r)

coreTypeToTy :: CoreType -> Ty
coreTypeToTy (CTVar n)    = TyVar n
coreTypeToTy (CTCon q)    = TyCon q
coreTypeToTy (CTApp f x)  = TyApp (coreTypeToTy f) (coreTypeToTy x)
coreTypeToTy (CTFun a b)  = TyFun (coreTypeToTy a) (coreTypeToTy b)
coreTypeToTy (CTTuple ts) = TyTuple (map coreTypeToTy ts)

ppTy :: Ty -> String
ppTy (TyVar v)    = v
ppTy (TyCon q)    = intercalate "." q
ppTy (TyApp f x)  = ppTy f ++ " " ++ ppTyA x
ppTy (TyFun a b)  = ppTyA a ++ " -> " ++ ppTy b
ppTy (TyTuple ts) = "(" ++ intercalate ", " (map ppTy ts) ++ ")"

ppTyA :: Ty -> String
ppTyA t@(TyFun _ _) = "(" ++ ppTy t ++ ")"
ppTyA t@(TyApp _ _) = "(" ++ ppTy t ++ ")"
ppTyA t              = ppTy t

-- Substitution
type Subst = Map.Map TyVar Ty

emptySubst :: Subst
emptySubst = Map.empty

applySubst :: Subst -> Ty -> Ty
applySubst s (TyVar v)    = case Map.lookup v s of
  Just t  -> applySubst s t
  Nothing -> TyVar v
applySubst s (TyApp f x)  = TyApp (applySubst s f) (applySubst s x)
applySubst s (TyFun a b)  = TyFun (applySubst s a) (applySubst s b)
applySubst s (TyTuple ts) = TyTuple (map (applySubst s) ts)
applySubst _ t            = t

composeSubst :: Subst -> Subst -> Subst
composeSubst s2 s1 = Map.map (applySubst s2) s1 `Map.union` s2

freeTyVars :: Ty -> [TyVar]
freeTyVars (TyVar v)    = [v]
freeTyVars (TyApp f x)  = nub $ freeTyVars f ++ freeTyVars x
freeTyVars (TyFun a b)  = nub $ freeTyVars a ++ freeTyVars b
freeTyVars (TyTuple ts) = nub $ concatMap freeTyVars ts
freeTyVars _            = []

freeTyVarsScheme :: Scheme -> [TyVar]
freeTyVarsScheme (Scheme vs t) = freeTyVars t \\ vs

type TyEnv = Map.Map Name Scheme

-- Type inference monad
data TIState = TIState
  { tiCounter :: Int
  , tiSubst   :: Subst
  , tiErrors  :: [String]
  }

type TI a = S.State TIState a

runTI :: TI a -> (a, [String])
runTI m =
  let (a, st) = S.runState m (TIState 0 emptySubst [])
  in (a, tiErrors st)

freshTy :: TI Ty
freshTy = do
  st <- S.get
  S.put st { tiCounter = tiCounter st + 1 }
  return $ TyVar ("t" ++ show (tiCounter st))

getSubst :: TI Subst
getSubst = S.gets tiSubst

extendSubst :: Subst -> TI ()
extendSubst s = S.modify $ \st ->
  st { tiSubst = composeSubst s (tiSubst st) }

typeError :: String -> TI ()
typeError msg = S.modify $ \st ->
  st { tiErrors = tiErrors st ++ [msg] }

applyCurrentSubst :: Ty -> TI Ty
applyCurrentSubst t = do
  s <- getSubst
  return $ applySubst s t

-- Unification
unify :: Ty -> Ty -> TI ()
unify t1 t2 = do
  s <- getSubst
  let t1' = applySubst s t1
      t2' = applySubst s t2
  case (t1', t2') of
    _ | t1' == t2' -> return ()
    (TyVar v, t)   -> bindTyVar v t
    (t, TyVar v)   -> bindTyVar v t
    (TyFun a1 b1, TyFun a2 b2) -> unify a1 a2 >> unify b1 b2
    (TyApp f1 x1, TyApp f2 x2) -> unify f1 f2 >> unify x1 x2
    (TyTuple ts1, TyTuple ts2)
      | length ts1 == length ts2 -> mapM_ (uncurry unify) (zip ts1 ts2)
    -- Allow bare TyCon to unify with TyApp of same constructor by binding fresh vars
    (TyCon q, TyApp (TyCon q') _) | q == q' -> return ()  -- structural match, ignore arg
    (TyApp (TyCon q) _, TyCon q') | q == q' -> return ()
    _ -> typeError $ "Type mismatch: " ++ ppTy t1' ++ "  vs  " ++ ppTy t2'

bindTyVar :: TyVar -> Ty -> TI ()
bindTyVar v t
  | TyVar v' <- t, v' == v = return ()
  | v `elem` freeTyVars t  = typeError $ "Occurs check: " ++ v ++ " in " ++ ppTy t
  | otherwise               = extendSubst (Map.singleton v t)

-- Instantiate and generalise
instantiate :: Scheme -> TI Ty
instantiate (Scheme vs t) = do
  fresh <- mapM (\_ -> freshTy) vs
  return $ applySubst (Map.fromList (zip vs fresh)) t

generalise :: TyEnv -> Ty -> Scheme
generalise env t =
  let envFree  = nub $ concatMap freeTyVarsScheme (Map.elems env)
      quantify = freeTyVars t \\ envFree
  in Scheme quantify t

applySubstScheme :: Subst -> Scheme -> Scheme
applySubstScheme s (Scheme vs t) =
  Scheme vs (applySubst (foldr Map.delete s vs) t)

applySubstTExpr :: Subst -> TExpr -> TExpr
applySubstTExpr s (TEVar q t)       = TEVar q (applySubst s t)
applySubstTExpr s (TELit l t)       = TELit l (applySubst s t)
applySubstTExpr s (TEApp f x t)     =
  TEApp (applySubstTExpr s f) (applySubstTExpr s x) (applySubst s t)
applySubstTExpr s (TELam n e t)     =
  TELam n (applySubstTExpr s e) (applySubst s t)
applySubstTExpr s (TELet n r b t)   =
  TELet n (applySubstTExpr s r) (applySubstTExpr s b) (applySubst s t)
applySubstTExpr s (TECase e arms t) =
  TECase (applySubstTExpr s e)
    [(p, applySubstTExpr s b) | (p,b) <- arms]
    (applySubst s t)
applySubstTExpr s (TETypeof e t)    = TETypeof (applySubstTExpr s e) (applySubst s t)

-- Built-in environment
builtinEnv :: TyEnv
builtinEnv = Map.fromList
  [ ("+",   Scheme [] $ tyFun [tyInt, tyInt] tyInt)
  , ("-",   Scheme [] $ tyFun [tyInt, tyInt] tyInt)
  , ("*",   Scheme [] $ tyFun [tyInt, tyInt] tyInt)
  , ("/",   Scheme [] $ tyFun [tyInt, tyInt] tyInt)
  , ("+.",  Scheme [] $ tyFun [tyFloat, tyFloat] tyFloat)
  , ("-.",  Scheme [] $ tyFun [tyFloat, tyFloat] tyFloat)
  , ("==",  Scheme ["a"] $ tyFun [TyVar "a", TyVar "a"] tyBool)
  , ("/=",  Scheme ["a"] $ tyFun [TyVar "a", TyVar "a"] tyBool)
  , ("<",   Scheme ["a"] $ tyFun [TyVar "a", TyVar "a"] tyBool)
  , (">",   Scheme ["a"] $ tyFun [TyVar "a", TyVar "a"] tyBool)
  , ("<=",  Scheme ["a"] $ tyFun [TyVar "a", TyVar "a"] tyBool)
  , (">=",  Scheme ["a"] $ tyFun [TyVar "a", TyVar "a"] tyBool)
  , ("&&",  Scheme [] $ tyFun [tyBool, tyBool] tyBool)
  , ("||",  Scheme [] $ tyFun [tyBool, tyBool] tyBool)
  , ("++",  Scheme [] $ tyFun [tyString, tyString] tyString)
  , ("List.Cons", Scheme ["a"] $
      tyFun [TyVar "a", tyList (TyVar "a")] (tyList (TyVar "a")))
  , ("List.Nil",  Scheme ["a"] $ tyList (TyVar "a"))
  , ("True",      Scheme [] tyBool)
  , ("False",     Scheme [] tyBool)
  , ("negate",    Scheme [] $ tyFun [tyInt] tyInt)
  , ("print",     Scheme ["a"] $ tyFun [TyVar "a"] (TyCon ["Unit"]))
  -- Tuple constructors for common arities
  , ("Tuple.2",   Scheme ["a","b"] $
      tyFun [TyVar "a", TyVar "b"] (TyTuple [TyVar "a", TyVar "b"]))
  , ("Tuple.3",   Scheme ["a","b","c"] $
      tyFun [TyVar "a", TyVar "b", TyVar "c"] (TyTuple [TyVar "a", TyVar "b", TyVar "c"]))
  , ("Tuple.4",   Scheme ["a","b","c","d"] $
      tyFun [TyVar "a", TyVar "b", TyVar "c", TyVar "d"]
            (TyTuple [TyVar "a", TyVar "b", TyVar "c", TyVar "d"]))
  ]

-- Build constructor env from data decls
buildConEnv :: CoreModule -> TyEnv
buildConEnv decls = Map.fromList $ concatMap go decls
  where
    go (CDData typeName tyVars cons) = map (mkCon typeName tyVars) cons
    go _                             = []
    mkCon typeName tyVars (conName, argTypes) =
      let ret    = foldl TyApp (TyCon [typeName]) (map TyVar tyVars)
          conTy  = tyFun (map coreTypeToTy argTypes) ret
      in (conName, Scheme tyVars conTy)

-- Algorithm W
infer :: TyEnv -> CoreExpr -> TI TExpr

infer env (CVar q) = do
  let name = intercalate "." q
  case Map.lookup name env `mplus` Map.lookup (last q) env of
    Nothing -> do
      t <- freshTy
      typeError $ "Unknown: " ++ name
      return $ TEVar q t
    Just sc -> TEVar q <$> instantiate sc

infer _   (CLit l@(CLInt _))    = return $ TELit l tyInt
infer _   (CLit l@(CLFloat _))  = return $ TELit l tyFloat
infer _   (CLit l@(CLString _)) = return $ TELit l tyString

infer env (CApp f x) = do
  tf   <- infer env f
  tx   <- infer env x
  tret <- freshTy
  unify (typeOf tf) (TyFun (typeOf tx) tret)
  s <- getSubst
  return $ TEApp (applySubstTExpr s tf) (applySubstTExpr s tx) (applySubst s tret)

infer env (CLam n body) = do
  targ  <- freshTy
  let env' = Map.insert n (Scheme [] targ) env
  tbody <- infer env' body
  s     <- getSubst
  return $ TELam n tbody (TyFun (applySubst s targ) (typeOf tbody))

infer env (CLet n rhs body) = do
  trhs <- infer env rhs
  s    <- getSubst
  let rhsTy  = applySubst s (typeOf trhs)
      scheme = generalise (Map.map (applySubstScheme s) env) rhsTy
      env'   = Map.insert n scheme env
  tbody <- infer env' body
  s2    <- getSubst
  return $ TELet n (applySubstTExpr s2 trhs)
                  (applySubstTExpr s2 tbody)
                  (applySubst s2 (typeOf tbody))

infer env (CCase scrut arms) = do
  tscrut <- infer env scrut
  tret   <- freshTy
  arms'  <- mapM (inferArm env (typeOf tscrut) tret) arms
  s      <- getSubst
  return $ TECase (applySubstTExpr s tscrut) arms' (applySubst s tret)

infer env (CTypeof e) = TETypeof <$> infer env e <*> pure tyString

infer env (CAnn e ct) = do
  te <- infer env e
  unify (typeOf te) (coreTypeToTy ct)
  s <- getSubst
  return $ applySubstTExpr s te

inferArm :: TyEnv -> Ty -> Ty -> (CorePat, CoreExpr) -> TI (CorePat, TExpr)
inferArm env scrutTy retTy (pat, body) = do
  (env', patTy) <- inferPat env pat
  unify scrutTy patTy
  tbody <- infer env' body
  unify (typeOf tbody) retTy
  s <- getSubst
  return (pat, applySubstTExpr s tbody)

inferPat :: TyEnv -> CorePat -> TI (TyEnv, Ty)
inferPat env (CPVar n) = do
  t <- freshTy
  return (Map.insert n (Scheme [] t) env, t)
inferPat env CPWild = do
  t <- freshTy
  return (env, t)
inferPat env (CPLit (CLInt _))    = return (env, tyInt)
inferPat env (CPLit (CLFloat _))  = return (env, tyFloat)
inferPat env (CPLit (CLString _)) = return (env, tyString)
inferPat env (CPCon q argNames) = do
  let name = intercalate "." q
  case Map.lookup name env of
    Nothing -> do
      argTys <- mapM (\_ -> freshTy) argNames
      retTy  <- freshTy
      typeError $ "Unknown constructor in pattern: " ++ name
      let env' = foldl (\e (n,t) -> Map.insert n (Scheme [] t) e) env
                       (zip argNames argTys)
      return (env', retTy)
    Just sc -> do
      conTy <- instantiate sc
      let (argTys, retTy) = unpackFunTy (length argNames) conTy
      let env' = foldl (\e (n,t) -> Map.insert n (Scheme [] t) e) env
                       (zip argNames argTys)
      return (env', retTy)
inferPat env (CPTuple argNames) = do
  argTys <- mapM (\_ -> freshTy) argNames
  let env' = foldl (\e (n,t) -> Map.insert n (Scheme [] t) e) env
                   (zip argNames argTys)
  return (env', TyTuple argTys)

unpackFunTy :: Int -> Ty -> ([Ty], Ty)
unpackFunTy 0 t           = ([], t)
unpackFunTy n (TyFun a b) = let (as, r) = unpackFunTy (n-1) b in (a:as, r)
unpackFunTy n t           = (replicate n (TyCon ["?"]), t)

mplus :: Maybe a -> Maybe a -> Maybe a
mplus Nothing y = y
mplus x       _ = x

-- Typed declaration
data TypedDecl
  = TDData  Name [Name] [(Name, [CoreType])]
  | TDAlias Name [Name] CoreType
  | TDType  Name [Name] CoreType
  | TDSig   Name Ty
  | TDDef   Name TExpr Ty
  | TDIso   Name Name (TExpr, TExpr)
  | TDUse   Name String
  | TDEval  TExpr Ty
  deriving (Show)

type TypedModule = [TypedDecl]

typeCheckModule :: CoreModule -> (TypedModule, [String])
typeCheckModule decls = runTI $ do
  let conEnv  = buildConEnv decls
      initEnv = Map.union builtinEnv conEnv
      sigs    = Map.fromList
                  [ (n, Scheme (freeTyVars (coreTypeToTy t)) (coreTypeToTy t))
                  | CDSig n t <- decls ]
  -- Seed environment with user signatures for recursive defs
  recEnv <- foldM (\env d -> case d of
    CDDef n _ -> do
      t <- freshTy
      let sc = fromMaybe (Scheme [] t) (Map.lookup n sigs)
      return $ Map.insert n sc env
    _ -> return env)
    (Map.union initEnv sigs) decls
  mapM (checkDecl recEnv sigs) decls

checkDecl :: TyEnv -> Map.Map Name Scheme -> CoreDecl -> TI TypedDecl
checkDecl _   _    (CDData  n vs c) = return $ TDData  n vs c
checkDecl _   _    (CDAlias n vs t) = return $ TDAlias n vs t
checkDecl _   _    (CDType  n vs t) = return $ TDType  n vs t
checkDecl _   _    (CDSig   n t)    = return $ TDSig   n (coreTypeToTy t)
checkDecl _   _    (CDUse   n s)    = return $ TDUse   n s
checkDecl env sigs (CDDef   n e)    = do
  te <- infer env e
  s  <- getSubst
  let finalTy = applySubst s (typeOf te)
  case Map.lookup n sigs of
    Just (Scheme vs sigTy) -> do
      freshSig <- instantiate (Scheme vs sigTy)
      unify finalTy freshSig
    Nothing -> return ()
  s2 <- getSubst
  return $ TDDef n (applySubstTExpr s2 te) (applySubst s2 finalTy)
checkDecl env _ (CDIso a b (e1,e2)) = do
  te1 <- infer env e1
  te2 <- infer env e2
  return $ TDIso a b (te1, te2)
checkDecl env _ (CDEval e) = do
  te <- infer env e
  s  <- getSubst
  return $ TDEval (applySubstTExpr s te) (applySubst s (typeOf te))

-- ============================================================
-- PASS 3: Lambda Lifting
-- ============================================================

data LLState = LLState
  { llCounter :: Int
  , llLifted  :: [(Name, CoreExpr)]
  }

type LL a = S.State LLState a

runLL :: LL a -> (a, [(Name, CoreExpr)])
runLL m =
  let (a, st) = S.runState m (LLState 0 [])
  in (a, llLifted st)

freshLL :: String -> LL Name
freshLL hint = do
  n <- S.gets llCounter
  S.modify $ \st -> st { llCounter = n + 1 }
  return $ hint ++ "$lam" ++ show n

emitLifted :: Name -> CoreExpr -> LL ()
emitLifted n e = S.modify $ \st ->
  st { llLifted = llLifted st ++ [(n, e)] }

-- Free variables that a lambda CAPTURES from its enclosing scope.
-- `outer`: names bound in enclosing lambdas (these are captures if referenced)
-- `inner`: names bound by let/case/lambda within the current scope (not captures)
-- Returns names from `outer` that are referenced in the expression.
freeVarsCore :: [Name]   -- globally bound (top-level, never captured)
             -> [Name]   -- outer scope: lambda params from enclosing lambdas
             -> [Name]   -- inner scope: let/case/lambda bindings within this body
             -> CoreExpr
             -> [Name]
freeVarsCore gl outer inner (CVar [n])
  | n `elem` gl    = []        -- top-level, not captured
  | n `elem` inner = []        -- locally bound within this body, not captured
  | n `elem` outer = [n]       -- captured from enclosing scope
  | otherwise      = []        -- unknown/qualified
freeVarsCore _  _     _     (CVar _)      = []
freeVarsCore _  _     _     (CLit _)      = []
freeVarsCore gl outer inner (CApp f x)    =
  nub $ freeVarsCore gl outer inner f ++ freeVarsCore gl outer inner x
freeVarsCore gl outer inner (CLam n e)    =
  -- New lambda: n moves to outer (it IS a capture for deeper lambdas)
  -- but for our purposes we want captures of THIS lambda, so n is inner here
  freeVarsCore gl outer (n:inner) e
freeVarsCore gl outer inner (CLet n r b)  =
  nub $ freeVarsCore gl outer inner r ++
        freeVarsCore gl outer (n:inner) b
freeVarsCore gl outer inner (CCase e arms) =
  nub $ freeVarsCore gl outer inner e ++
        concatMap (\(p,b) -> freeVarsCore gl outer (inner ++ patBound p) b) arms
freeVarsCore gl outer inner (CTypeof e)   = freeVarsCore gl outer inner e
freeVarsCore gl outer inner (CAnn e _)    = freeVarsCore gl outer inner e

liftExpr :: Name    -- enclosing top-level def name (for naming)
         -> [Name]  -- global names
         -> [Name]  -- locally bound names in scope
         -> CoreExpr
         -> LL CoreExpr

liftExpr top gl loc (CLam n body) = do
  -- Lift body first (inside-out), passing n as part of outer scope
  body' <- liftExpr top gl (n:loc) body
  -- Captured vars: things referenced in body' that come from enclosing lambdas (loc)
  -- `outer` = loc (enclosing lambda params), `inner` = [] (body's own bindings found by recursion)
  let free = nub $ freeVarsCore gl loc [] body'
  liftedName <- freshLL top
  -- Emit: liftedName = λfree1 → λfree2 → ... → λn → body'
  let lifted = foldr CLam body' (free ++ [n])
  emitLifted liftedName lifted
  -- Call site: partially apply to captured free vars
  return $ foldl CApp (CVar [liftedName]) (map (\v -> CVar [v]) free)

liftExpr top gl loc (CApp f x) =
  CApp <$> liftExpr top gl loc f <*> liftExpr top gl loc x

liftExpr top gl loc (CLet n rhs body) = do
  rhs'  <- liftExpr top gl loc rhs
  -- n is a let binding — it goes into the body's local scope, not the lambda-capture scope
  -- so we do NOT add n to loc (which tracks lambda params for capture detection)
  body' <- liftExpr top gl loc body
  return $ CLet n rhs' body'

liftExpr top gl loc (CCase e arms) = do
  e'    <- liftExpr top gl loc e
  arms' <- forM arms $ \(p, b) ->
    -- pattern-bound names are also local, not added to loc
    (p,) <$> liftExpr top gl loc b
  return $ CCase e' arms'

liftExpr top gl loc (CTypeof e) = CTypeof <$> liftExpr top gl loc e
liftExpr top gl loc (CAnn e t)  = (`CAnn` t) <$> liftExpr top gl loc e
liftExpr _   _  _   e           = return e

liftDecl :: [Name] -> CoreDecl -> LL [CoreDecl]
liftDecl gl (CDDef n e) = do
  e' <- liftExpr n gl [] e
  return [CDDef n e']
liftDecl gl (CDEval e) = do
  e' <- liftExpr "$eval" gl [] e
  return [CDEval e']
liftDecl gl (CDIso a b (e1, e2)) = do
  e1' <- liftExpr a gl [] e1
  e2' <- liftExpr b gl [] e2
  return [CDIso a b (e1', e2')]
liftDecl _ d = return [d]

lambdaLiftModule :: CoreModule -> CoreModule
lambdaLiftModule decls =
  let globals          = [n | CDDef n _ <- decls]
      (decls', lifted) = runLL $ fmap concat $ mapM (liftDecl globals) decls
      liftedDecls      = [CDDef n e | (n, e) <- lifted]
  in decls' ++ liftedDecls

-- ============================================================
-- PASS 4: ANF (Administrative Normal Form)
-- ============================================================

type ANF a = S.State Int a

freshANF :: ANF Name
freshANF = do
  n <- S.get
  S.put (n + 1)
  return $ "$anf" ++ show n

runANF :: ANF a -> a
runANF m = S.evalState m 0

isAtom :: CoreExpr -> Bool
isAtom (CVar _) = True
isAtom (CLit _) = True
isAtom _        = False

-- CPS-style ANF transform.
-- anfK e k: normalise e, pass the atom result to continuation k.
anfK :: CoreExpr -> (CoreExpr -> ANF CoreExpr) -> ANF CoreExpr
anfK e@(CVar _) k = k e
anfK e@(CLit _) k = k e

anfK (CApp f x) k =
  anfK f $ \f' ->
  anfK x $ \x' -> do
    tmp  <- freshANF
    rest <- k (CVar [tmp])
    return $ CLet tmp (CApp f' x') rest

anfK (CLam n body) k = do
  body' <- anfK body return
  k (CLam n body')

anfK (CLet n rhs body) k =
  anfK rhs $ \rhs' -> do
    body' <- anfK body k
    return $ CLet n rhs' body'

anfK (CCase scrut arms) k =
  anfK scrut $ \scrut' -> do
    arms' <- mapM (\(p, b) -> (p,) <$> anfK b k) arms
    return $ CCase scrut' arms'

anfK (CTypeof e) k =
  anfK e $ \e' -> do
    tmp  <- freshANF
    rest <- k (CVar [tmp])
    return $ CLet tmp (CTypeof e') rest

anfK (CAnn e t) k = anfK e (\e' -> k (CAnn e' t))

anfDecl :: CoreDecl -> ANF CoreDecl
anfDecl (CDDef n e)          = CDDef n <$> anfK e return
anfDecl (CDEval e)           = CDEval  <$> anfK e return
anfDecl (CDIso a b (e1, e2)) = do
  e1' <- anfK e1 return
  e2' <- anfK e2 return
  return $ CDIso a b (e1', e2')
anfDecl d = return d

anfModule :: CoreModule -> CoreModule
anfModule decls = runANF $ mapM anfDecl decls

-- forM for use in LL
forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM = flip mapM


ppLit :: Lit -> String
ppLit (LInt n)    = show n
ppLit (LFloat f)  = show f
ppLit (LString s) = show s

ppQName :: QName -> String
ppQName = intercalate "."

ppPat :: Pat -> String
ppPat (PVar n)     = n
ppPat PWild        = "_"
ppPat (PLit l)     = ppLit l
ppPat (PCon q [])  = ppQName q
ppPat (PCon q ps)  = "(" ++ ppQName q ++ " " ++ unwords (map ppPat ps) ++ ")"
ppPat (PTuple ps)  = "(" ++ intercalate ", " (map ppPat ps) ++ ")"
ppPat (PList ps)   = "[" ++ intercalate ", " (map ppPat ps) ++ "]"
ppPat (PCons p q)  = "(" ++ ppPat p ++ " :: " ++ ppPat q ++ ")"
ppPat (PAs n p)    = n ++ "@" ++ ppPat p

ppSExpr :: Int -> Expr -> String
ppSExpr _ (EVar q)        = ppQName q
ppSExpr _ (ELit l)        = ppLit l
ppSExpr d (EApp f x)      = paren (d>10) $ ppSExpr 10 f ++ " " ++ ppSExpr 11 x
ppSExpr d (EFn ps e)      = paren (d>0)  $ "fn " ++ unwords (map ppPat ps) ++ " => " ++ ppSExpr 0 e
ppSExpr d (ELet bs e)     = paren (d>0)  $
  "let " ++ intercalate "; " (map (\(n,v) -> n ++ " = " ++ ppSExpr 0 v) bs) ++ "; " ++ ppSExpr 0 e
ppSExpr d (ECase e arms)  = paren (d>0) $
  "case " ++ ppSExpr 0 e ++ " of" ++
  concatMap (\(p,b) -> "\n  " ++ ppPat p ++ " -> " ++ ppSExpr 0 b) arms
ppSExpr d (EIf c t e)     = paren (d>0) $
  "if " ++ ppSExpr 0 c ++ " then " ++ ppSExpr 0 t ++ " else " ++ ppSExpr 0 e
ppSExpr _ (ETuple es)     = "(" ++ intercalate ", " (map (ppSExpr 0) es) ++ ")"
ppSExpr _ (EList es)      = "[" ++ intercalate ", " (map (ppSExpr 0) es) ++ "]"
ppSExpr d (ECons h t)     = paren (d>5) $ ppSExpr 6 h ++ " :: " ++ ppSExpr 5 t
ppSExpr d (EPipe e f)     = paren (d>1) $ ppSExpr 1 e ++ " |> " ++ ppSExpr 2 f
ppSExpr d (EBinOp op l r) = paren (d>6) $ ppSExpr 6 l ++ " " ++ op ++ " " ++ ppSExpr 7 r
ppSExpr d (EUnary op e)   = paren (d>10) $ op ++ ppSExpr 11 e
ppSExpr _ (ETypeof e)     = "typeof " ++ ppSExpr 11 e
ppSExpr d (EAnn e t)      = paren (d>0) $ ppSExpr 1 e ++ " : " ++ ppSType t

paren :: Bool -> String -> String
paren True  s = "(" ++ s ++ ")"
paren False s = s

ppSType :: Type -> String
ppSType (TVar n)      = n
ppSType (TCon q)      = ppQName q
ppSType (TApp f x)    = ppSType f ++ " " ++ ppSTypeAtom x
ppSType (TFun a b)    = ppSTypeAtom a ++ " -> " ++ ppSType b
ppSType (TTuple ts)   = "(" ++ intercalate ", " (map ppSType ts) ++ ")"
ppSType (TList t)     = "[" ++ ppSType t ++ "]"

ppSTypeAtom :: Type -> String
ppSTypeAtom t@(TFun _ _) = "(" ++ ppSType t ++ ")"
ppSTypeAtom t@(TApp _ _) = "(" ++ ppSType t ++ ")"
ppSTypeAtom t              = ppSType t

ppSDecl :: Decl -> String
ppSDecl (DUse n s)         = n ++ " = use \"" ++ s ++ "\"."
ppSDecl (DAlias n vs t)    = "alias " ++ unwords (n:vs) ++ " = " ++ ppSType t ++ "."
ppSDecl (DType n vs t)     = "type " ++ unwords (n:vs) ++ " = " ++ ppSType t ++ "."
ppSDecl (DData n vs cons)  =
  n ++ (if null vs then "" else " " ++ unwords vs) ++ " =\n" ++
  intercalate "\n| " (map ppCon cons) ++ "."
  where ppCon (c,[]) = c; ppCon (c,ts) = c ++ " " ++ unwords (map ppSTypeAtom ts)
ppSDecl (DSig n t)         = n ++ " : " ++ ppSType t ++ "."
ppSDecl (DFunc n [] e)     = n ++ " = " ++ ppSExpr 0 e ++ "."
ppSDecl (DFunc n ps e)     = n ++ " " ++ unwords (map ppPat ps) ++ " = " ++ ppSExpr 0 e ++ "."
ppSDecl (DIso a b (e1,e2)) = "iso " ++ a ++ " " ++ b ++ " = (" ++ ppSExpr 0 e1 ++ ", " ++ ppSExpr 0 e2 ++ ")."
ppSDecl (DEval e)          = "> " ++ ppSExpr 0 e ++ "."

-- ============================================================
-- Pretty printing: Core IR
-- ============================================================

ppCoreLit :: CoreLit -> String
ppCoreLit (CLInt n)    = show n
ppCoreLit (CLFloat f)  = show f
ppCoreLit (CLString s) = show s

ppCorePat :: CorePat -> String
ppCorePat (CPVar n)      = n
ppCorePat CPWild         = "_"
ppCorePat (CPLit l)      = ppCoreLit l
ppCorePat (CPCon q [])   = intercalate "." q
ppCorePat (CPCon q ns)   = "(" ++ intercalate "." q ++ " " ++ unwords ns ++ ")"
ppCorePat (CPTuple ns)   = "(" ++ intercalate ", " ns ++ ")"

ppCoreType :: CoreType -> String
ppCoreType (CTVar n)      = n
ppCoreType (CTCon q)      = intercalate "." q
ppCoreType (CTApp f x)    = ppCoreType f ++ " " ++ ppCoreTypeA x
ppCoreType (CTFun a b)    = ppCoreTypeA a ++ " -> " ++ ppCoreType b
ppCoreType (CTTuple ts)   = "(" ++ intercalate ", " (map ppCoreType ts) ++ ")"

ppCoreTypeA :: CoreType -> String
ppCoreTypeA t@(CTFun _ _) = "(" ++ ppCoreType t ++ ")"
ppCoreTypeA t@(CTApp _ _) = "(" ++ ppCoreType t ++ ")"
ppCoreTypeA t               = ppCoreType t

ppCoreExpr :: Int -> CoreExpr -> String
ppCoreExpr _ (CVar q)         = intercalate "." q
ppCoreExpr _ (CLit l)         = ppCoreLit l
ppCoreExpr d (CApp f x)       = parenC (d>10) $ ppCoreExpr 10 f ++ " " ++ ppCoreExpr 11 x
ppCoreExpr d (CLam n e)       = parenC (d>0)  $ ppLamChain n e
ppCoreExpr d (CLet n e body)  = parenC (d>0)  $
  "let " ++ n ++ " = " ++ ppCoreExpr 0 e ++ "\n" ++
  replicate (d*2) ' ' ++ "in  " ++ ppCoreExpr 0 body
ppCoreExpr d (CCase e arms)   = parenC (d>0) $
  "case " ++ ppCoreExpr 0 e ++ " of" ++
  concatMap (\(p,b) -> "\n    " ++ ppCorePat p ++ " ->\n      " ++ ppCoreExpr 0 b) arms
ppCoreExpr _ (CTypeof e)      = "typeof " ++ ppCoreExpr 11 e
ppCoreExpr d (CAnn e t)       = parenC (d>0) $ ppCoreExpr 1 e ++ " : " ++ ppCoreType t

ppLamChain :: Name -> CoreExpr -> String
ppLamChain n (CLam n' e) = "λ" ++ n ++ " " ++ ppLamChain n' e
ppLamChain n e           = "λ" ++ n ++ " →\n    " ++ ppCoreExpr 0 e

parenC :: Bool -> String -> String
parenC True  s = "(" ++ s ++ ")"
parenC False s = s

ppCoreDecl :: CoreDecl -> String
ppCoreDecl (CDUse n s)         = n ++ " = use \"" ++ s ++ "\"."
ppCoreDecl (CDAlias n vs t)    = "alias " ++ unwords (n:vs) ++ " = " ++ ppCoreType t ++ "."
ppCoreDecl (CDType n vs t)     = "type " ++ unwords (n:vs) ++ " = " ++ ppCoreType t ++ "."
ppCoreDecl (CDData n vs cons)  =
  n ++ (if null vs then "" else " " ++ unwords vs) ++ " =\n" ++
  intercalate "\n| " (map ppCon cons) ++ "."
  where ppCon (c,[]) = c; ppCon (c,ts) = c ++ " " ++ unwords (map ppCoreTypeA ts)
ppCoreDecl (CDSig n t)         = n ++ " : " ++ ppCoreType t ++ "."
ppCoreDecl (CDDef n e)         = n ++ " =\n  " ++ ppCoreExpr 0 e ++ "."
ppCoreDecl (CDIso a b (e1,e2)) = "iso " ++ a ++ " " ++ b ++
  " = (" ++ ppCoreExpr 0 e1 ++ ", " ++ ppCoreExpr 0 e2 ++ ")."
ppCoreDecl (CDEval e)          = "> " ++ ppCoreExpr 0 e ++ "."

-- ============================================================
-- Test program
-- ============================================================

testSrc :: String
testSrc = unlines
  [ "std = use \"std@v1\"."
  , ""
  , "# Data types"
  , "MyType a = A a | B Int | C (String, Int)."
  , "alias MyInt = Int."
  , "type MyString = String."
  , ""
  , "# Type signature + multi-clause function"
  , "f : MyType -> String."
  , "f (A a) = \"A\"."
  , "f (B i) = \"B\"."
  , "f _ = \"None\"."
  , ""
  , "# Simple arithmetic"
  , "> 1 + 2 * 3."
  , ""
  , "# Pipe desugaring"
  , "> 5 |> (* 2) |> (+ 1)."
  , ""
  , "# if/then/else"
  , "safediv x y = if y == 0 then 0 else x / y."
  , ""
  , "# let block"
  , "calc x = let a = x + 1; b = a * 2; b - 1."
  , ""
  , "# Multi-arg fn with nested pattern"
  , "addPair p = case p of"
  , "  (A a, B i) -> i"
  , "| _ -> 0."
  , ""
  , "# List sugar"
  , "mylist = [1, 2, 3]."
  , "head' (x :: xs) = x."
  , "head' [] = 0."
  , ""
  , "# Tuple"
  , "swap p = case p of (x, y) -> (y, x)."
  , ""
  , "# Recursive type + iso"
  , "Queue a = Node a (Queue a) | End."
  , "queuetolist (Node x xs) = x :: queuetolist xs."
  , "queuetolist End = []."
  , "iso Queue List = (queuetolist, queuetolist)."
  , ""
  , "# Nested fn"
  , "adder x = fn y => x + y."
  ]

-- ============================================================
-- Pretty printing: Typed IR
-- ============================================================

ppTE :: Int -> TExpr -> String
ppTE _ (TEVar q t)       = intercalate "." q ++ " : " ++ ppTy t
ppTE _ (TELit l t)       = ppCoreLit l ++ " : " ++ ppTy t
ppTE d (TEApp f x t)     = parenC (d>10) $
  ppTEInner 10 f ++ " " ++ ppTEInner 11 x ++ "  {" ++ ppTy t ++ "}"
ppTE d (TELam n e t)     = parenC (d>0) $
  "\\" ++ n ++ " -> " ++ ppTE 0 e ++ "  {" ++ ppTy t ++ "}"
ppTE d (TELet n r b t)   = parenC (d>0) $
  "let " ++ n ++ " = " ++ ppTE 0 r ++ "\n  in " ++ ppTE 0 b ++ "  {" ++ ppTy t ++ "}"
ppTE d (TECase e arms t) = parenC (d>0) $
  "case " ++ ppTE 0 e ++ " of" ++
  concatMap (\(p,b) -> "\n  | " ++ ppCorePat p ++ " -> " ++ ppTE 1 b) arms ++
  "  {" ++ ppTy t ++ "}"
ppTE _ (TETypeof e t)    = "typeof " ++ ppTE 11 e ++ " : " ++ ppTy t

ppTEInner :: Int -> TExpr -> String
ppTEInner d e = parenC (d > 0 && not (isSimple e)) (ppTEBare e)
  where
    isSimple (TEVar _ _) = True
    isSimple (TELit _ _) = True
    isSimple _          = False
    ppTEBare (TEVar q _) = intercalate "." q
    ppTEBare (TELit l _) = ppCoreLit l
    ppTEBare e'         = ppTE d e'

ppTypedDecl :: TypedDecl -> String
ppTypedDecl (TDData n vs cons)   = ppCoreDecl (CDData n vs cons)
ppTypedDecl (TDAlias n vs t)     = ppCoreDecl (CDAlias n vs t)
ppTypedDecl (TDType n vs t)      = ppCoreDecl (CDType n vs t)
ppTypedDecl (TDSig n t)          = n ++ " : " ++ ppTy t
ppTypedDecl (TDUse n s)          = n ++ " = use \"" ++ s ++ "\""
ppTypedDecl (TDDef n te ty)      =
  n ++ " : " ++ ppTy ty ++ "\n" ++ n ++ " =\n  " ++ ppTE 1 te
ppTypedDecl (TDIso a b (e1,e2))  =
  "iso " ++ a ++ " " ++ b ++ " = (" ++ ppTE 0 e1 ++ ", " ++ ppTE 0 e2 ++ ")"
ppTypedDecl (TDEval te ty)       =
  "> " ++ ppTE 0 te ++ "\n  : " ++ ppTy ty

-- ============================================================
-- PASS 4b: Saturation
-- Collapse curried known-arity calls back into fully-applied form.
-- After ANF, (f x y) becomes (let a = f x in a y).
-- For known primitives, constructors, and top-level functions
-- we want to emit a single call with all args loaded at once.
--
-- This pass walks the let-chain and recognises patterns like:
--   let a = f x        -- f is a known 2-arg prim / fn
--   let b = a y        -- a is the partial result
-- and rewrites to a single SatCall node that codegen handles directly.
--
-- We use a small extension to CoreExpr for saturated calls.
-- Rather than extend the ADT, we represent them as a special
-- CApp chain that the codegen recognises by inspecting arity.
-- The saturation pass just ensures the args are collected so codegen
-- can emit a single call instruction.
--
-- Implementation: track which let-bound names are "partial applications"
-- of known functions, and when a subsequent let completes them, emit
-- directly. This is done at codegen time by looking through the let chain.
-- ============================================================

-- Known arities for primitives and builtins
knownArity :: Name -> Maybe Int
knownArity n = Map.lookup n arityTable

arityTable :: Map.Map Name Int
arityTable = Map.fromList $
  -- 2-arg primitives
  [ (op, 2) | op <- ["+","-","*","/","+.","-.","+.","*.","/.","==","/=","<",">","<=",">=","&&","||","++"] ] ++
  -- 1-arg primitives
  [ (op, 1) | op <- ["negate"] ] ++
  -- List constructors
  [ ("List.Cons", 2), ("List.Nil", 0) ] ++
  -- Tuple constructors
  [ ("Tuple." ++ show n, n) | n <- [2..8] ]

-- Saturation pass: rewrite the let-chain to group partial applications.
-- We tag partial-application bindings in a Map so codegen can see through them.
-- Returns: (expression with inlined partials where profitable, partial-app map)
-- A "partial app map" maps a let-bound name to (fn, [args so far]).
type PartialMap = Map.Map Name (CoreExpr, [CoreExpr])

saturateExpr :: SymTable -> PartialMap -> CoreExpr -> CoreExpr
saturateExpr sym pm (CLet n rhs body) =
  let rhs' = saturateExpr sym pm rhs
      pm'  = updatePartials sym pm n rhs'
  in CLet n rhs' (saturateExpr sym pm' body)
saturateExpr sym pm (CCase e arms) =
  CCase (saturateExpr sym pm e)
    [(p, saturateExpr sym pm b) | (p,b) <- arms]
saturateExpr sym pm (CLam n body) =
  CLam n (saturateExpr sym Map.empty body)
saturateExpr sym pm (CApp f x) =
  CApp (saturateExpr sym pm f) (saturateExpr sym pm x)
saturateExpr _ _ e = e

-- When we bind `n = rhs`, check if rhs is a partial application
-- of something with known arity so we can track it.
updatePartials :: SymTable -> PartialMap -> Name -> CoreExpr -> PartialMap
updatePartials sym pm n rhs =
  case collectAppsS rhs of
    (CVar [fn], args) ->
      let arity = case Map.lookup fn arityTable of
                    Just a  -> Just a
                    Nothing -> case Map.lookup fn sym of
                                 Just fi -> Just (fiArity fi)
                                 Nothing -> Nothing
      in case arity of
           Just a | length args < a ->
             Map.insert n (CVar [fn], args) pm
           _ -> pm
    (CVar q, args) | isListFnS q ->
      -- List functions: treat as known arity 2 for map/filter, 3 for fold
      let a = listFnArity q
      in if length args < a then Map.insert n (CVar q, args) pm
         else pm
    _ -> pm
  where
    -- Resolve through the partial map: if rhs is `CVar partial_name`, look it up
    collectAppsS (CApp f x) =
      let (fn, args) = collectAppsS f
          x' = case x of
                 CVar [vn] -> case Map.lookup vn pm of
                                Just _  -> x  -- partial itself, not an arg
                                Nothing -> x
                 _ -> x
      in (fn, args ++ [x'])
    collectAppsS e = (e, [])

isListFnS :: QName -> Bool
isListFnS ("List":_) = True
isListFnS _          = False

listFnArity :: QName -> Int
listFnArity ["List","map"]    = 2
listFnArity ["List","filter"] = 2
listFnArity ["List","fold"]   = 3
listFnArity ["List","foldl"]  = 3
listFnArity ["List","foldr"]  = 3
listFnArity ["List","zip"]    = 2
listFnArity ["List","zipWith"]= 3
listFnArity ["List","take"]   = 2
listFnArity ["List","drop"]   = 2
listFnArity ["List","Cons"]   = 2
listFnArity _                 = 2

saturateDecl :: SymTable -> CoreDecl -> CoreDecl
saturateDecl sym (CDDef n e) = CDDef n (saturateExpr sym Map.empty e)
saturateDecl sym (CDEval e)  = CDEval (saturateExpr sym Map.empty e)
saturateDecl _   d           = d

saturateModule :: SymTable -> CoreModule -> CoreModule
saturateModule sym = map (saturateDecl sym)

-- ============================================================
-- PASS 5: RISC-V Code Generation
-- ============================================================

-- Symbol table: function names to labels + arities
data FunInfo = FunInfo
  { fiLabel :: String
  , fiArity :: Int
  } deriving (Show)

type SymTable = Map.Map Name FunInfo

countArity :: CoreExpr -> Int
countArity (CLam _ body) = 1 + countArity body
countArity _             = 0

-- Mangle a name to a valid assembly label
mangle :: Name -> String
mangle = concatMap mangleChar
  where
    mangleChar c
      | (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') = [c]
      | c == '_'  = "_"
      | c == '\'' = "_q"
      | c == '$'  = "_D"
      | c == '.'  = "_d"
      | otherwise = "_x" ++ show (fromEnum c) ++ "_"

buildSymTable :: CoreModule -> SymTable
buildSymTable decls = Map.fromList
  [ (n, FunInfo (mangle n) (countArity e))
  | CDDef n e <- decls ]

-- Assembly line
type Asm = String

-- CG state
data CGState = CGState
  { cgCounter  :: Int
  , cgStrings  :: [(String, String)]
  , cgSymTable :: SymTable
  }

type CG a = CGState -> (a, CGState)

cgReturn :: a -> CG a
cgReturn x s = (x, s)

cgBind :: CG a -> (a -> CG b) -> CG b
cgBind m f s = let (a, s') = m s in f a s'

cgGet :: CG CGState
cgGet s = (s, s)

cgModify :: (CGState -> CGState) -> CG ()
cgModify f s = ((), f s)

freshLabel :: String -> CG String
freshLabel hint s =
  let n = cgCounter s
  in ("_L" ++ hint ++ show n, s { cgCounter = n + 1 })

internString :: String -> CG String
internString str s =
  case lookup str (cgStrings s) of
    Just lbl -> (lbl, s)
    Nothing  ->
      let lbl = ".Lstr" ++ show (length (cgStrings s))
      in (lbl, s { cgStrings = cgStrings s ++ [(lbl, str)] })

runCG :: SymTable -> CG a -> (a, CGState)
runCG sym m = m (CGState 0 [] sym)

-- Frame: maps local names to stack offsets from fp (s0)
data Frame = Frame
  { frLocals :: Map.Map Name Int
  , frSize   :: Int
  , frArgs   :: [Name]
  }

buildFrame :: [Name] -> [Name] -> Frame
buildFrame args locals =
  let names   = args ++ locals
      offsets = Map.fromList (zip names (map (\i -> -(i+1)*8) [0..]))
      -- negative offsets from fp: fp-8, fp-16, ...
      size    = align16 (8 * (length names + 2))  -- +2 for ra, s0
  in Frame offsets size args
  where
    align16 n = ((n + 15) `div` 16) * 16

collectLocals :: CoreExpr -> [Name]
collectLocals (CLet n _ body) = n : collectLocals body
collectLocals (CCase e arms)  = collectLocals e ++
  concatMap (\(p,b) -> cgPatBound p ++ collectLocals b) arms
collectLocals (CApp f x)      = collectLocals f ++ collectLocals x
collectLocals _               = []

cgPatBound :: CorePat -> [Name]
cgPatBound (CPVar n)    = [n]
cgPatBound (CPCon _ ns) = ns
cgPatBound (CPTuple ns) = ns
cgPatBound _            = []

-- Constructor tag table
builtinTags :: Map.Map String Int
builtinTags = Map.fromList
  [ ("List.Nil",  0), ("List.Cons", 1)
  , ("True", 1),      ("False", 0)
  ]

buildTagTable :: CoreModule -> Map.Map String Int
buildTagTable decls = foldl' addData builtinTags decls
  where
    addData tbl (CDData _ _ cons) =
      foldl' (\t (i,(n,_)) -> Map.insert n i t) tbl (zip [0..] cons)
    addData tbl _ = tbl

-- ---- Top-level entry point ----

generateModule :: CoreModule -> String
generateModule decls =
  let sym  = buildSymTable decls
      tags = buildTagTable decls
      (sections, st) = runCG sym (cgModule tags decls)
  in unlines $ asmHeader ++ sections ++ dataSection (cgStrings st) ++ runtimeStubs

asmHeader :: [Asm]
asmHeader =
  [ "# FP-RISC generated RISC-V assembly (RV64GC)"
  , "# Every value is a heap pointer in a0..a7."
  , "# Primitives unbox, compute, rebox."
  , ""
  , "    .option nopic"
  , "    .text"
  , ""
  ]

dataSection :: [(String, String)] -> [Asm]
dataSection [] = []
dataSection strs =
  "" : "    .section .rodata" :
  concatMap (\(lbl, s) ->
    [ lbl ++ ":"
    , "    .quad " ++ show (length s)
    , "    .string " ++ show s
    ]) strs

cgModule :: Map.Map String Int -> CoreModule -> CG [Asm]
cgModule tags [] = cgReturn []
cgModule tags (d:ds) =
  cgDecl tags d `cgBind` \a ->
  cgModule tags ds `cgBind` \b ->
  cgReturn (a ++ b)

cgDecl :: Map.Map String Int -> CoreDecl -> CG [Asm]
cgDecl tags (CDDef name expr) = cgFunction tags name expr
cgDecl tags (CDEval expr)     =
  cgGet `cgBind` \st ->
  let n = cgCounter st in
  cgModify (\s -> s { cgCounter = n + 1 }) `cgBind` \_ ->
  cgFunction tags ("__eval_" ++ show n) expr
cgDecl _ _ = cgReturn []

cgFunction :: Map.Map String Int -> Name -> CoreExpr -> CG [Asm]
cgFunction tags name expr =
  let args   = lambdaArgs expr
      body   = lambdaBody expr
      locals = nub (collectLocals body)
      frame  = buildFrame args locals
      lbl    = mangle name
  in
  cgBody tags frame body "a0" `cgBind` \bodyAsm ->
  cgReturn $
    [ "", "    .globl " ++ lbl
    , "    .type  " ++ lbl ++ ", @function"
    , lbl ++ ":"
    , "    # prologue: frame=" ++ show (frSize frame) ++ " bytes"
    , "    addi  sp, sp, -" ++ show (frSize frame)
    , "    sd    ra, " ++ show (frSize frame - 8) ++ "(sp)"
    , "    sd    s0, " ++ show (frSize frame - 16) ++ "(sp)"
    , "    addi  s0, sp, " ++ show (frSize frame)
    ]
    ++ saveArgs frame args
    ++ bodyAsm
    ++
    [ "    # epilogue"
    , "    ld    ra, " ++ show (frSize frame - 8) ++ "(sp)"
    , "    ld    s0, " ++ show (frSize frame - 16) ++ "(sp)"
    , "    addi  sp, sp, " ++ show (frSize frame)
    , "    ret"
    , "    .size " ++ lbl ++ ", .-" ++ lbl
    ]

lambdaArgs :: CoreExpr -> [Name]
lambdaArgs (CLam n body) = n : lambdaArgs body
lambdaArgs _             = []

lambdaBody :: CoreExpr -> CoreExpr
lambdaBody (CLam _ body) = lambdaBody body
lambdaBody e             = e

-- Save incoming a0..a7 to stack
saveArgs :: Frame -> [Name] -> [Asm]
saveArgs frame args =
  concat
    [ case Map.lookup name (frLocals frame) of
        Just off -> [ "    sd    a" ++ show i ++ ", " ++ show off ++ "(s0)  # " ++ name ]
        Nothing  -> [ "    # no slot for arg " ++ name ]
    | (i, name) <- zip [0..] args
    ]

loadVar :: Frame -> Name -> String -> [Asm]
loadVar frame name reg =
  case Map.lookup name (frLocals frame) of
    Just off -> [ "    ld    " ++ reg ++ ", " ++ show off ++ "(s0)  # " ++ name ]
    Nothing  -> [ "    la    " ++ reg ++ ", " ++ mangle name ++ "  # global " ++ name ]

storeVar :: Frame -> Name -> String -> [Asm]
storeVar frame name reg =
  case Map.lookup name (frLocals frame) of
    Just off -> [ "    sd    " ++ reg ++ ", " ++ show off ++ "(s0)  # " ++ name ]
    Nothing  -> [ "    # WARNING: no frame slot for " ++ name ]

-- ---- Expression codegen ----

-- Detect: rhs = CApp (known-fn) arg1 (1 arg when fn needs >=2)
-- Then check if body starts with: let m = CApp (CVar n) arg2
-- If so, generate the full 2-arg call directly into m, skip n.
tryFusePartial :: Map.Map String Int -> Frame -> Name -> CoreExpr -> CoreExpr -> String
               -> Maybe (CG [Asm])
tryFusePartial tags frame n rhs (CLet m (CApp (CVar [n']) arg2) body2) dest
  | n' == n
  , Just (fn, [arg1]) <- isPartialKnown rhs
  = Just $
      -- Load arg2 into a1 FIRST (may box through a0),
      -- then load arg1 into a0 (pure ld, won't clobber a1)
      cgAtomLoad tags frame arg2 "a1" `cgBind` \a1 ->
      cgAtomLoad tags frame arg1 "a0" `cgBind` \a0 ->
      let callAsm = case fn of
            Left prim    -> [ "    call  " ++ prim ]
            Right "cons" -> [ "    call  rt_list_cons" ]
            Right direct -> [ "    call  " ++ direct ]
      in cgBody tags frame body2 dest `cgBind` \bodyAsm ->
         cgReturn $ a1 ++ a0 ++ callAsm ++
           storeVar frame m "a0" ++
           bodyAsm
tryFusePartial _ _ _ _ _ _ = Nothing

-- Recognise a 1-of-2-arg partial application.
isPartialKnown :: CoreExpr -> Maybe (Either String String, [CoreExpr])
isPartialKnown (CApp (CVar [op]) arg)
  | Just prim <- Map.lookup op primOps
  = Just (Left prim, [arg])
isPartialKnown (CApp (CVar ["List","Cons"]) arg)
  = Just (Right "cons", [arg])
isPartialKnown (CApp (CVar q) arg)
  | isListFn q
  = Just (Right (listStubName q 2), [arg])
isPartialKnown _ = Nothing

cgBody :: Map.Map String Int -> Frame -> CoreExpr -> String -> CG [Asm]

cgBody _ frame (CVar [n]) dest =
  cgReturn $ loadVar frame n dest

cgBody _ _ (CVar q) dest =
  cgReturn [ "    la    " ++ dest ++ ", " ++ mangle (intercalate "." q) ]

cgBody _ _ (CLit (CLInt n)) dest =
  cgReturn
    [ "    li    a0, " ++ show n
    , "    call  rt_box_int"
    , if dest /= "a0" then "    mv    " ++ dest ++ ", a0" else "    # -> a0"
    ]

cgBody _ _ (CLit (CLFloat f)) dest =
  cgReturn
    [ "    # float " ++ show f
    , "    call  rt_box_float  # TODO: pass float via fa0"
    , if dest /= "a0" then "    mv    " ++ dest ++ ", a0" else "    # -> a0"
    ]

cgBody _ _ (CLit (CLString s)) dest =
  internString s `cgBind` \lbl ->
  cgReturn
    [ "    la    a0, " ++ lbl
    , "    call  rt_box_string_static"
    , if dest /= "a0" then "    mv    " ++ dest ++ ", a0" else "    # -> a0"
    ]

cgBody tags frame (CLet n rhs body) dest =
  case tryFusePartial tags frame n rhs body dest of
    Just fused -> fused
    Nothing ->
      cgBody tags frame rhs "a0" `cgBind` \rhsAsm ->
      let store = storeVar frame n "a0"
      in cgBody tags frame body dest `cgBind` \bodyAsm ->
         cgReturn $ rhsAsm ++ store ++ bodyAsm

cgBody tags frame (CCase scrut arms) dest =
  cgBody tags frame scrut "t0" `cgBind` \scrutAsm ->
  freshLabel "cend" `cgBind` \lEnd ->
  cgArms tags frame arms dest lEnd `cgBind` \armsAsm ->
  cgReturn $ scrutAsm ++ armsAsm ++ [ lEnd ++ ":" ]

cgBody tags frame app dest | not (isAtomE app) =
  let (fn, args) = collectApps app
  in cgCallExpr tags frame fn args dest

cgBody _ _ e dest =
  cgReturn [ "    # unhandled: " ++ show e ]

isAtomE :: CoreExpr -> Bool
isAtomE (CVar _) = True
isAtomE (CLit _) = True
isAtomE _        = False

collectApps :: CoreExpr -> (CoreExpr, [CoreExpr])
collectApps = go []
  where
    go acc (CApp f x) = go (x:acc) f
    go acc e          = (e, acc)

-- Generate a function call / constructor application
-- fn is the head, args is the full arg list (already collected by collectApps)
cgCallExpr :: Map.Map String Int -> Frame -> CoreExpr -> [CoreExpr] -> String -> CG [Asm]
cgCallExpr tags frame fn args dest =
  cgGet `cgBind` \st ->
  let sym   = cgSymTable st
      nargs = length args
  in
  case fn of
    -- Known 2-arg primitive — load arg1 into a0, arg2 into a1
    -- Load arg2 first (may use a0 as scratch), then arg1 into a0
    CVar [op] | Just prim <- Map.lookup op primOps, nargs == 2 ->
      cgAtomLoad tags frame (args !! 1) "a1" `cgBind` \a1 ->
      cgAtomLoad tags frame (args !! 0) "a0" `cgBind` \a0 ->
      cgReturn $ a1 ++ a0 ++
        [ "    call  " ++ prim
        , mv dest "a0"
        ]

    -- Known 1-arg primitive
    CVar [op] | Just prim <- Map.lookup op primOps, nargs == 1 ->
      cgAtomLoad tags frame (args !! 0) "a0" `cgBind` \a0 ->
      cgReturn $ a0 ++
        [ "    call  " ++ prim
        , mv dest "a0"
        ]

    -- List.Nil — no args, call rt_list_nil
    CVar ["List","Nil"] ->
      cgReturn [ "    call  rt_list_nil", mv dest "a0" ]

    -- List.Cons — 2 args
    CVar ["List","Cons"] | nargs == 2 ->
      cgAtomLoad tags frame (args !! 1) "a1" `cgBind` \a1 ->
      cgAtomLoad tags frame (args !! 0) "a0" `cgBind` \a0 ->
      cgReturn $ a1 ++ a0 ++
        [ "    call  rt_list_cons", mv dest "a0" ]

    -- Other List function — load all args and call stub
    CVar q | isListFn q ->
      cgLoadArgsList tags frame args `cgBind` \argAsms ->
      cgReturn $ argAsms ++
        [ "    call  " ++ listStubName q nargs, mv dest "a0" ]

    -- Known constructor — fully saturated
    CVar q | isConstructor q ->
      let tag = fromMaybe 99 (Map.lookup (intercalate "." q) tags)
      in cgLoadArgsList tags frame args `cgBind` \argAsms ->
         cgReturn $ argAsms ++ allocCon tag nargs dest

    -- Tuple.N — N args
    CVar ["Tuple", ns] ->
      let n = read ns :: Int
      in cgLoadArgsList tags frame args `cgBind` \argAsms ->
         cgReturn $ argAsms ++
           [ "    li    a" ++ show n ++ ", " ++ show n
           , "    call  rt_alloc_tuple"
           , mv dest "a0"
           ]

    -- Known top-level function — direct call, no closure dispatch
    CVar [fname] | Map.member fname sym ->
      cgLoadArgsList tags frame args `cgBind` \argAsms ->
      cgReturn $ argAsms ++
        [ "    call  " ++ mangle fname, mv dest "a0" ]
    -- Partial application lookup: if fn is CVar that resolves to a
    -- known partial in the frame, we should have already inlined it.
    -- Fall through to general indirect call.
    _ ->
      cgAtomLoad tags frame fn "t0" `cgBind` \fnAsm ->
      cgLoadArgsList tags frame args `cgBind` \argAsms ->
      freshLabel "icall" `cgBind` \icallLbl ->
      cgReturn $ fnAsm ++ argAsms ++
        [ "    # indirect call"
        , "    lw    t1, 0(t0)          # load object tag"
        , "    li    t2, 5              # TAG_CLOSURE"
        , "    beq   t1, t2, " ++ icallLbl ++ "_clos"
        , "    jalr  ra, t0, 0          # plain function pointer"
        , "    j     " ++ icallLbl ++ "_done"
        , icallLbl ++ "_clos:"
        , "    ld    t0, 16(t0)         # fn_ptr from closure header"
        , "    jalr  ra, t0, 0"
        , icallLbl ++ "_done:"
        , mv dest "a0"
        ]

-- Helper: emit mv only when needed
mv :: String -> String -> String
mv dest src = if dest == src then "    # result in " ++ src
              else "    mv    " ++ dest ++ ", " ++ src

-- Load a list of args into a0..aN, evaluating in order.
-- We must be careful: later args might clobber registers used by earlier ones.
-- Strategy: evaluate all into temp stack slots, then load into arg regs.
cgLoadArgsList :: Map.Map String Int -> Frame -> [CoreExpr] -> CG [Asm]
cgLoadArgsList tags frame args = loadRev (length args - 1) (reverse args)
  where
    loadRev _ []     = cgReturn []
    loadRev i (a:as) =
      cgAtomLoad tags frame a ("a" ++ show i) `cgBind` \aAsm ->
      loadRev (i-1) as `cgBind` \rest ->
      cgReturn (aAsm ++ rest)

-- Load a single atom into a register.
-- For literals, we box into a0 then move — but if dest != a0 we must
-- be careful not to clobber a0 if it already holds something useful.
-- Since we load args in order (a0 first, then a1 etc.), loading into a1
-- will go through a0 and we must save a0 first.
cgAtomLoad :: Map.Map String Int -> Frame -> CoreExpr -> String -> CG [Asm]
cgAtomLoad _ frame (CVar [n]) reg = cgReturn $ loadVar frame n reg
cgAtomLoad _ _     (CVar q)   reg =
  cgReturn [ "    la    " ++ reg ++ ", " ++ mangle (intercalate "." q) ]
-- Literal into a non-a0 register: save a0, box lit, move, restore a0
cgAtomLoad _ _ (CLit (CLInt n)) "a0" =
  cgReturn [ "    li    a0, " ++ show n, "    call  rt_box_int" ]
cgAtomLoad _ _ (CLit (CLInt n)) reg =
  cgReturn
    [ "    # box int " ++ show n ++ " into " ++ reg
    , "    addi  sp, sp, -16"
    , "    sd    a0, 0(sp)          # save a0"
    , "    li    a0, " ++ show n
    , "    call  rt_box_int"
    , "    mv    " ++ reg ++ ", a0"
    , "    ld    a0, 0(sp)          # restore a0"
    , "    addi  sp, sp, 16"
    ]
cgAtomLoad _ _ (CLit (CLFloat f)) "a0" =
  cgReturn [ "    # box float " ++ show f, "    call  rt_box_float" ]
cgAtomLoad _ _ (CLit (CLFloat f)) reg =
  cgReturn
    [ "    addi  sp, sp, -16"
    , "    sd    a0, 0(sp)"
    , "    # box float " ++ show f
    , "    call  rt_box_float"
    , "    mv    " ++ reg ++ ", a0"
    , "    ld    a0, 0(sp)"
    , "    addi  sp, sp, 16"
    ]
cgAtomLoad tags frame e reg = cgBody tags frame e reg

-- Case arms
cgArms :: Map.Map String Int -> Frame -> [(CorePat, CoreExpr)] -> String -> String -> CG [Asm]
cgArms _ _ [] _ _ = cgReturn []
cgArms tags frame ((pat, body):rest) dest lEnd =
  freshLabel "arm" `cgBind` \lNext ->
  cgBody tags frame body dest `cgBind` \bodyAsm ->
  cgArms tags frame rest dest lEnd `cgBind` \restAsm ->
  cgReturn $
    cgPatMatch tags frame pat "t0" lNext ++
    bodyAsm ++
    [ "    j     " ++ lEnd
    , lNext ++ ":"
    ] ++ restAsm

-- Pattern matching: test "t0", branch to lNext if no match, bind vars on match
cgPatMatch :: Map.Map String Int -> Frame -> CorePat -> String -> String -> [Asm]

cgPatMatch _ frame (CPVar n) scrutReg _ =
  storeVar frame n scrutReg

cgPatMatch _ _ CPWild _ _ = []

cgPatMatch _ frame (CPLit (CLInt n)) scrutReg lNext =
  [ "    ld    t1, 8(" ++ scrutReg ++ ")   # unbox Int"
  , "    li    t2, " ++ show n
  , "    bne   t1, t2, " ++ lNext
  ]

cgPatMatch _ frame (CPLit (CLFloat _)) scrutReg lNext =
  [ "    # float pattern match (TODO: unbox and compare)"
  , "    j     " ++ lNext  -- stub: always fall through
  ]

cgPatMatch _ frame (CPLit (CLString _)) scrutReg lNext =
  [ "    # string pattern match (TODO: call rt_prim_eq)"
  , "    j     " ++ lNext  -- stub
  ]

cgPatMatch tags frame (CPCon q fieldNames) scrutReg lNext =
  let tag = fromMaybe 99 (Map.lookup (intercalate "." q) tags)
  in
  [ "    lw    t1, 0(" ++ scrutReg ++ ")   # load tag"
  , "    li    t2, " ++ show tag
  , "    bne   t1, t2, " ++ lNext
  ]
  -- Bind fields: Con header is [tag:u32|refcnt:u32|con_id:u32|pad:u32|field0|field1...]
  -- fields start at offset 16
  ++ concat
    [ [ "    ld    t3, " ++ show (16 + 8*i) ++ "(" ++ scrutReg ++ ")   # field " ++ fn ]
      ++ storeVar frame fn "t3"
    | (i, fn) <- zip [0..] fieldNames
    ]

cgPatMatch _ frame (CPTuple fieldNames) scrutReg lNext =
  [ "    lw    t1, 0(" ++ scrutReg ++ ")   # load tag"
  , "    li    t2, 4                        # TAG_TUPLE"
  , "    bne   t1, t2, " ++ lNext
  ]
  -- Tuple layout: [tag:u32|refcnt:u32|field0|field1...]  fields at offset 8
  ++ concat
    [ [ "    ld    t3, " ++ show (8 + 8*i) ++ "(" ++ scrutReg ++ ")   # tuple[" ++ show i ++ "]" ]
      ++ storeVar frame fn "t3"
    | (i, fn) <- zip [0..] fieldNames
    ]

cgPatMatch _ _ _ _ lNext =
  [ "    j     " ++ lNext  -- unhandled pattern, skip arm
  ]

-- ---- Allocation ----

allocCon :: Int -> Int -> String -> [Asm]
allocCon tag nFields dest =
  -- Convention: a0=tag, a1=nfields, then a2..aN have field pointers
  -- We shift current args: they are already in a0..a(N-1),
  -- so we move them up to a2..a(N+1), then set a0=tag, a1=nfields
  [ "    # alloc constructor tag=" ++ show tag ++ " nfields=" ++ show nFields ]
  ++ [ "    mv    a" ++ show (nFields+1-i) ++ ", a" ++ show (nFields-i)
     | i <- [1..nFields] ]
  ++
  [ "    li    a0, " ++ show tag
  , "    li    a1, " ++ show nFields
  , "    call  rt_alloc_con"
  , if dest /= "a0" then "    mv    " ++ dest ++ ", a0" else ""
  ]

-- ---- Primitives and stubs ----

primOps :: Map.Map String String
primOps = Map.fromList
  [ ("+",  "rt_prim_add_int"),  ("-",  "rt_prim_sub_int")
  , ("*",  "rt_prim_mul_int"),  ("/",  "rt_prim_div_int")
  , ("+.", "rt_prim_add_float"),("-.", "rt_prim_sub_float")
  , ("*.", "rt_prim_mul_float"),("/.", "rt_prim_div_float")
  , ("==", "rt_prim_eq"),       ("/=", "rt_prim_neq")
  , ("<",  "rt_prim_lt"),       (">",  "rt_prim_gt")
  , ("<=", "rt_prim_le"),       (">=", "rt_prim_ge")
  , ("&&", "rt_prim_and"),      ("||", "rt_prim_or")
  , ("++", "rt_prim_str_concat"),("negate","rt_prim_negate_int")
  ]

isListFn :: QName -> Bool
isListFn ("List":_) = True
isListFn _          = False

isConstructor :: QName -> Bool
isConstructor q = case last q of { (c:_) -> c >= 'A' && c <= 'Z'; _ -> False }

listStubName :: QName -> Int -> String
listStubName q _ = "rt_list_" ++ map toLower' (intercalate "_" (tail q))
  where toLower' c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

runtimeStubs :: [Asm]
runtimeStubs =
  [ "", "# ---- Runtime extern declarations ----"
  , "    .extern rt_alloc"
  , "    .extern rt_alloc_con"
  , "    .extern rt_alloc_tuple"
  , "    .extern rt_rc_inc"
  , "    .extern rt_rc_dec"
  , "    .extern rt_box_int"
  , "    .extern rt_box_float"
  , "    .extern rt_box_string_static"
  , "    .extern rt_prim_add_int"
  , "    .extern rt_prim_sub_int"
  , "    .extern rt_prim_mul_int"
  , "    .extern rt_prim_div_int"
  , "    .extern rt_prim_add_float"
  , "    .extern rt_prim_sub_float"
  , "    .extern rt_prim_mul_float"
  , "    .extern rt_prim_div_float"
  , "    .extern rt_prim_eq"
  , "    .extern rt_prim_neq"
  , "    .extern rt_prim_lt"
  , "    .extern rt_prim_gt"
  , "    .extern rt_prim_le"
  , "    .extern rt_prim_ge"
  , "    .extern rt_prim_and"
  , "    .extern rt_prim_or"
  , "    .extern rt_prim_str_concat"
  , "    .extern rt_prim_negate_int"
  , "    # List tier 1 (Cons cells)"
  , "    .extern rt_list_cons"
  , "    .extern rt_list_nil"
  , "    .extern rt_list_map"
  , "    .extern rt_list_filter"
  , "    .extern rt_list_fold"
  , "    .extern rt_list_foldl"
  , "    .extern rt_list_foldr"
  , "    .extern rt_list_head"
  , "    .extern rt_list_tail"
  , "    .extern rt_list_length"
  , "    .extern rt_list_append"
  , "    .extern rt_list_reverse"
  , "    .extern rt_list_zip"
  , "    .extern rt_list_zip_with"
  , "    .extern rt_list_any"
  , "    .extern rt_list_all"
  , "    .extern rt_list_concat"
  , "    # List tier 2 (SoA)"
  , "    .extern rt_list_soa_create"
  , "    .extern rt_list_soa_get"
  , "    .extern rt_list_soa_set"
  , "    .extern rt_list_soa_map"
  , "    .extern rt_list_soa_map_all"
  , "    .extern rt_list_soa_filter"
  , "    .extern rt_list_soa_fold"
  , "    .extern rt_list_soa_to_cons"
  , "    .extern rt_list_cons_to_soa"
  , "    # List tier 3 (streams / fusion)"
  , "    .extern rt_stream_from_list"
  , "    .extern rt_stream_map"
  , "    .extern rt_stream_filter"
  , "    .extern rt_stream_fold"
  , "    .extern rt_stream_zip_with"
  , "    .extern rt_stream_take"
  , "    .extern rt_stream_force"
  , "    # SIMD (RVV numeric SoA)"
  , "    .extern rt_simd_map_add_int"
  , "    .extern rt_simd_map_mul_int"
  , "    .extern rt_simd_map_add_float"
  , "    .extern rt_simd_map_mul_float"
  , "    .extern rt_simd_fold_sum_int"
  , "    .extern rt_simd_fold_sum_float"
  , "    .extern rt_simd_zip_add_int"
  , "    .extern rt_simd_zip_mul_float"
  , "    .extern rt_simd_dot_product"
  , "    # Closures"
  , "    .extern rt_make_closure"
  , "    .extern rt_apply_closure"
  , "    .extern rt_apply_closure_n"
  ]

-- Runtime C header comment
runtimeHeaderComment :: [Asm]
runtimeHeaderComment =
  [ ""
  , "# ============================================================"
  , "# C RUNTIME INTERFACE SUMMARY (implement in runtime.c)"
  , "# All values are void* (heap pointers). Object layout:"
  , "#   [tag:u32 | refcnt:u32 | payload...]"
  , "# Tags: 0=Int 1=Float 2=String 3=Con 4=Tuple 5=Closure"
  , "#        6=SoA 7=Stream"
  , "# List tiers:"
  , "#   Tier1 = Cons cells  (rt_list_*)"
  , "#   Tier2 = SoA arrays  (rt_list_soa_*)  — SIMD-friendly"
  , "#   Tier3 = Streams     (rt_stream_*)    — zero-copy fusion"
  , "#   SIMD  = RVV ops on numeric SoA (rt_simd_*)"
  , "# ============================================================"
  ]

banner :: String -> IO ()
banner s = do
  putStrLn ""
  putStrLn $ replicate 64 '='
  putStrLn $ "  " ++ s
  putStrLn $ replicate 64 '='

-- ============================================================
-- Registry integration: read use.json sidecar if present
-- ============================================================

-- Parse a use.json file: {"std": "/abs/path", "mymod": "/abs/path"}
-- Returns a map from module name to resolved source path.
readUseJson :: FilePath -> IO (Map.Map String String)
readUseJson path = do
  exists <- doesFileExist path
  if not exists
    then return Map.empty
    else do
      content <- readFile path
      -- Simple JSON parsing without a library: expect {"k":"v",...}
      return $ parseSimpleJsonMap content

doesFileExist :: FilePath -> IO Bool
doesFileExist p = do
  r <- System.IO.Error.tryIOError (readFile p)
  case r of
    Left _  -> return False
    Right _ -> return True

-- Parse {"key": "value", ...} — simple, no nesting, no escapes
parseSimpleJsonMap :: String -> Map.Map String String
parseSimpleJsonMap s =
  let stripped = filter (/= '\n') s
      pairs    = extractPairs stripped
  in Map.fromList pairs
  where
    extractPairs str =
      case dropWhile (/= '"') str of
        [] -> []
        (_:rest) ->
          let (k, rest1) = span (/= '"') rest
          in case dropWhile (\c -> c `elem` (" \t:\"" :: String)) rest1 of
               [] -> []
               rest2 ->
                 let (v, rest3) = span (/= '"') rest2
                 in (k, v) : extractPairs (dropWhile (/= ',') rest3)

-- Rewrite DUse paths using the resolved map.
-- use "std@v1"  →  if "std" in map, rewrite spec to resolved path.
-- The compiler then reads that file as the module source.
applyUseResolution :: Map.Map String String -> [Decl] -> [Decl]
applyUseResolution useMap = map rewrite
  where
    rewrite (DUse alias spec) =
      let name = takeWhile (/= '@') spec
      in case Map.lookup name useMap of
           Just path -> DUse alias path  -- resolved to absolute snapshot path
           Nothing   -> DUse alias spec  -- unresolved, keep as-is
    rewrite d = d

-- ============================================================
-- Main: demo mode or file compile mode
-- ============================================================

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case args of
    -- File mode: fpr_compiler <file.fpr> [<file.use.json>]
    (srcFile : rest) -> do
      let useJsonFile = case rest of
                          (j:_) -> j
                          []    -> srcFile ++ ".use.json"  -- convention
      h   <- System.IO.openFile srcFile System.IO.ReadMode
      System.IO.hSetEncoding h System.IO.utf8
      src <- System.IO.hGetContents h
      useMap <- readUseJson useJsonFile
      runPipeline srcFile src useMap

    -- Demo mode: no args, run on built-in test source
    [] -> do
      putStrLn "# fpr compiler — demo mode (no file specified)"
      putStrLn "# Pass a .fpr file as argument to compile it."
      putStrLn ""
      runPipeline "<demo>" testSrc Map.empty

runPipeline :: String -> String -> Map.Map String String -> IO ()
runPipeline srcName src useMap = do
  case parse pModule srcName src of
    Left err -> putStrLn $ "PARSE ERROR:\n" ++ errorBundlePretty err
    Right surfaceDecls0 -> do
      let surfaceDecls = applyUseResolution useMap surfaceDecls0

      -- Surface
      banner "1. Surface AST"
      mapM_ (\d -> putStrLn $ "  " ++ ppSDecl d) surfaceDecls

      -- Desugar
      let core0 = desugarModule surfaceDecls
      banner "2. Core IR (desugared)"
      mapM_ (\d -> putStrLn $ ppCoreDecl d) core0

      -- Simplify
      let core1 = simplifyModule core0
      banner "3. Core IR (simplified)"
      mapM_ (\d -> putStrLn $ ppCoreDecl d) core1

      -- Type check
      let (typed, errs) = typeCheckModule core1
      banner "4. Type Checking"
      if null errs
        then putStrLn "  No errors."
        else do
          putStrLn "  ERRORS:"
          mapM_ (\e -> putStrLn $ "    " ++ e) errs
      putStrLn ""
      mapM_ (\d -> putStrLn $ ppTypedDecl d ++ "\n") typed

      -- Lambda lift
      let core2 = lambdaLiftModule core1
      banner "5. After Lambda Lifting"
      mapM_ (\d -> putStrLn $ ppCoreDecl d) core2

      -- ANF
      let core3 = anfModule core2
      banner "6. After ANF"
      mapM_ (\d -> putStrLn $ ppCoreDecl d) core3

      -- Codegen
      let sym   = buildSymTable core3
      let core4 = saturateModule sym core3
      banner "7. RISC-V Assembly"
      let asm = generateModule core4
      putStrLn asm
