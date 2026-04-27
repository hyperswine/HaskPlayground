{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module FPXSyn where

import Data.Void
import Data.List (intercalate)
import Control.Monad (void, when)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- ============================================================
-- AST
-- ============================================================

type Name = String   -- e.g. "x", "myFunc", "f'"
type QName = [Name]  -- e.g. ["std","println"] or ["MyType","A"]

data Lit
  = LInt    Integer
  | LFloat  Double
  | LString String
  deriving (Show, Eq)

-- Patterns (LHS of function clauses and case arms)
data Pat
  = PVar    Name           -- x
  | PWild                  -- _
  | PLit    Lit            -- 1, 1.0, "hi"
  | PCon    QName [Pat]    -- A p1 p2 / MyType.A p1 p2
  | PTuple  [Pat]          -- (p1, p2, p3)
  | PList   [Pat]          -- [p1, p2]
  | PCons   Pat Pat        -- p :: ps
  | PAs     Name Pat       -- x@pat  (bonus)
  deriving (Show, Eq)

-- Expressions
data Expr
  = EVar    QName            -- x / std.println / MyType.A
  | ELit    Lit              -- 1, 1.0, "hi"
  | EApp    Expr Expr        -- f x
  | EFn     [Pat] Expr       -- fn x y => body
  | ELet    [(Name, Expr)] Expr  -- let x = e; y = e; body
  | ECase   Expr [(Pat, Expr)]   -- case e of p -> e | p -> e
  | EIf     Expr Expr Expr   -- if c then t else e
  | ETuple  [Expr]           -- (e1, e2, e3)
  | EList   [Expr]           -- [e1, e2, e3]
  | ECons   Expr Expr        -- e :: es
  | EPipe   Expr Expr        -- e |> f
  | EBinOp  Name Expr Expr   -- e + e
  | EUnary  Name Expr        -- -e (negative literal desugared)
  | ETypeof Expr             -- typeof e
  | EAnn    Expr Type        -- e : T  (inline annotation, rare)
  deriving (Show, Eq)

-- Types
data Type
  = TVar    Name             -- a, b
  | TCon    QName            -- Int, MyType, List
  | TApp    Type Type        -- List Int, Maybe a
  | TFun    Type Type        -- a -> b
  | TTuple  [Type]           -- (Int, String)
  | TList   Type             -- [a]  (sugar for List a)
  deriving (Show, Eq)

-- Top-level declarations
data Decl
  = DUse    Name String          -- std = use "std@v1"
  | DAlias  Name [Name] Type     -- alias MyInt = Int
  | DType   Name [Name] Type     -- type MyString = String
  | DData   Name [Name] [(Name, [Type])]  -- MyType a = A a | B Int
  | DSig    Name Type            -- f : MyType -> String
  | DFunc   Name [Pat] Expr      -- f (A a) = "A"  (one clause)
  | DIso    Name Name (Expr, Expr) -- iso Queue List = (toList, fromList)
  | DEval   Expr                 -- > expr
  deriving (Show, Eq)

-- A module is a list of declarations
type Module = [Decl]

-- ============================================================
-- Parser type
-- ============================================================

type Parser = Parsec Void String

-- ============================================================
-- Lexer primitives
-- ============================================================

-- Line comments: # ...
-- No block comments for now
lineComment :: Parser ()
lineComment = L.skipLineComment "#"

-- But #: is a REPL output marker, not a comment — skip it as a line
replOutput :: Parser ()
replOutput = do
  _ <- string "#:"
  _ <- takeWhileP Nothing (/= '\n')
  return ()

sc :: Parser ()
sc = L.space space1 lineComment empty

-- sc but skipping repl output markers too
scFull :: Parser ()
scFull = L.space space1 (lineComment <|> replOutput) empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- Period terminator (top-level)
dot :: Parser ()
dot = void $ symbol "."

-- Semicolon (let separator)
semi :: Parser ()
semi = void $ symbol ";"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- ============================================================
-- Keywords and reserved words
-- ============================================================

keywords :: [String]
keywords =
  [ "fn", "let", "in", "case", "of", "if", "then", "else"
  , "use", "alias", "type", "iso", "typeof"
  ]

-- Parse a specific keyword
keyword :: String -> Parser ()
keyword w = lexeme $ do
  _ <- string w
  notFollowedBy (alphaNumChar <|> char '\'' <|> char '_')

-- ============================================================
-- Identifiers
-- ============================================================

-- Lower-case identifier (variables, function names, module aliases)
-- Allows letters, digits, underscore, apostrophe
lident :: Parser Name
lident = lexeme $ do
  first <- lowerChar <|> char '_'
  rest  <- many (alphaNumChar <|> char '\'' <|> char '_')
  let w = first : rest
  when (w `elem` keywords) $
    fail $ "keyword " ++ show w ++ " used as identifier"
  return w

-- Upper-case identifier (types, constructors, modules)
uident :: Parser Name
uident = lexeme $ do
  first <- upperChar
  rest  <- many (alphaNumChar <|> char '\'' <|> char '_')
  return (first : rest)

-- Wildcard
wild :: Parser ()
wild = lexeme $ do
  _ <- char '_'
  notFollowedBy (alphaNumChar <|> char '\'' <|> char '_')

-- Qualified name: segments separated by '.'
-- e.g. std.println, MyType.A, List.map
-- We parse greedily: Seg.Seg.seg
-- Upper segments = module/type qualifiers, final = constructor or var
qname :: Parser QName
qname = lexeme $ do
  first <- nameSegment
  rest  <- many (try $ char '.' *> nameSegment)
  return (first : rest)
  where
    -- A segment must start with a letter (alpha, not digit)
    nameSegment = do
      h <- letterChar
      t <- many (alphaNumChar <|> char '\'' <|> char '_')
      let w = h : t
      when (w `elem` keywords) $
        fail $ "keyword in qualified name"
      return w

-- A qualified name that must start with uppercase (type/constructor)
-- Used in type positions
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

-- ============================================================
-- Literals
-- ============================================================

-- Negative numeric literals: '-' immediately followed by digit (no space)
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
  d <- some digitChar  -- requires at least one digit after dot
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
      [ '\n' <$ char 'n'
      , '\t' <$ char 't'
      , '\\' <$ char '\\'
      , '"'  <$ char '"'
      ]

lit :: Parser Lit
lit = try numLit <|> strLit

-- ============================================================
-- Type parser
-- ============================================================

-- type = typeAtom -> type   (right-assoc arrow)
--      | typeAtom
pType :: Parser Type
pType = do
  t <- pTypeApp
  option t $ do
    _ <- symbol "->"
    TFun t <$> pType

-- type application: left-assoc
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

-- ============================================================
-- Pattern parser
-- ============================================================

pPat :: Parser Pat
pPat = pPatCons

-- :: is right-associative cons pattern
pPatCons :: Parser Pat
pPatCons = do
  p <- pPatApp
  option p $ do
    _ <- symbol "::"
    PCons p <$> pPatCons

-- Constructor application pattern
-- Con p p p ... where args are atoms only
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
  , try (PCon <$> uqname <*> pure [])   -- bare constructor, no args
  , PVar   <$> lident
  ]

-- ============================================================
-- Expression parser
-- ============================================================

-- Entry point: pipe expressions
pExpr :: Parser Expr
pExpr = pPipe

-- |> is left-associative, lowest precedence
pPipe :: Parser Expr
pPipe = do
  e <- pBinOp
  rest e
  where
    rest e = option e $ do
      _ <- symbol "|>"
      e' <- pBinOp
      rest (EPipe e e')

-- Binary operators: +, -, *, /, ++, ==, /=, <, >, <=, >=, &&, ||, ::
-- Simple precedence climbing
pBinOp :: Parser Expr
pBinOp = pCons

-- :: right-assoc cons
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

-- Function application: left-assoc, high precedence
pApp :: Parser Expr
pApp = do
  es <- some pAtom
  return $ foldl1 EApp es

pAtom :: Parser Expr
pAtom = choice
  [ pFn
  , pLet
  , pCase
  , pIf
  , pTypeof
  , try pOpSection                  -- (+), (*), (++) etc
  , try (ETuple <$> parens ((:) <$> pExpr <*> some (symbol "," *> pExpr)))
  , try (parens pExpr)
  , EList <$> brackets (pExpr `sepBy` symbol ",")
  , ELit  <$> try lit
  , EVar  <$> try qname
  ]

-- Operator sections: (+), (*), (++), (==), etc.
-- These become EVar [opName] — the operator as a first-class value
pOpSection :: Parser Expr
pOpSection = parens $ do
  op <- choice $ map (\s -> s <$ try (symbol s))
    [ "++", "==", "/=", "<=", ">=", "&&", "||", "::"
    , "+", "-", "*", "/"
    , "<", ">"
    ]
  return $ EVar [op]

-- fn pat+ => expr
pFn :: Parser Expr
pFn = do
  keyword "fn"
  pats <- some pPatAtom
  _ <- symbol "=>"
  EFn pats <$> pExpr

-- let x = e; y = e; body
-- body is the last expression (no semicolon after it)
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

-- case e of p -> e | p -> e
-- First arm may optionally be preceded by |
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

-- if e then e else e
pIf :: Parser Expr
pIf = do
  keyword "if"
  c <- pExpr
  keyword "then"
  t <- pExpr
  keyword "else"
  EIf c t <$> pExpr

-- typeof e
pTypeof :: Parser Expr
pTypeof = do
  keyword "typeof"
  ETypeof <$> pAtom

-- ============================================================
-- Top-level declaration parser
-- ============================================================

pDecl :: Parser Decl
pDecl = choice
  [ try pDeclEval      -- > expr.
  , try pDeclUse       -- name = use "...".
  , try pDeclAlias     -- alias Name tvars = Type.
  , try pDeclType      -- type Name tvars = Type.
  , try pDeclData      -- Name tvars = Con ... | Con ....
  , try pDeclIso       -- iso Name Name = (e, e).
  , try pDeclSig       -- name : Type.
  , pDeclFunc          -- name pats = expr.
  ]

-- > expr.
pDeclEval :: Parser Decl
pDeclEval = do
  _ <- symbol ">"
  e <- pExpr
  dot
  return $ DEval e

-- std = use "std@v1".
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

-- alias MyInt = Int.
pDeclAlias :: Parser Decl
pDeclAlias = do
  keyword "alias"
  name <- uident
  tvars <- many lident
  _ <- symbol "="
  t <- pType
  dot
  return $ DAlias name tvars t

-- type MyString = String.
pDeclType :: Parser Decl
pDeclType = do
  keyword "type"
  name <- uident
  tvars <- many lident
  _ <- symbol "="
  t <- pType
  dot
  return $ DType name tvars t

-- MyType a = A a | B Int | C (String, Int).
-- Only if the RHS contains '|'-separated constructors
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

-- iso Queue List = (toList, fromList).
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

-- f : MyType -> String.
pDeclSig :: Parser Decl
pDeclSig = do
  n <- lident
  _ <- symbol ":"
  t <- pType
  dot
  return $ DSig n t

-- f pat* = expr.
-- Also handles operators as function names: (+) = ...
pDeclFunc :: Parser Decl
pDeclFunc = do
  name <- lident
  pats <- many pPatAtom
  _ <- symbol "="
  body <- pExpr
  dot
  return $ DFunc name pats body

-- ============================================================
-- Module parser
-- ============================================================

pModule :: Parser Module
pModule = do
  scFull
  decls <- many (scFull *> pDecl <* scFull)
  eof
  return decls

-- ============================================================
-- Pretty printing
-- ============================================================

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

ppExpr :: Int -> Expr -> String
ppExpr _ (EVar q)       = ppQName q
ppExpr _ (ELit l)       = ppLit l
ppExpr d (EApp f x)     = paren (d > 10) $ ppExpr 10 f ++ " " ++ ppExpr 11 x
ppExpr d (EFn ps e)     = paren (d > 0) $ "fn " ++ unwords (map ppPat ps) ++ " => " ++ ppExpr 0 e
ppExpr d (ELet bs e)    = paren (d > 0) $
  "let " ++ intercalate "; " (map (\(n,v) -> n ++ " = " ++ ppExpr 0 v) bs) ++ "; " ++ ppExpr 0 e
ppExpr d (ECase e arms) = paren (d > 0) $
  "case " ++ ppExpr 0 e ++ " of\n" ++
  intercalate "\n" (zipWith ppArm [0..] arms)
  where
    ppArm 0 (p, b) = "  "  ++ ppPat p ++ " -> " ++ ppExpr 0 b
    ppArm _ (p, b) = "| "  ++ ppPat p ++ " -> " ++ ppExpr 0 b
ppExpr d (EIf c t e)    = paren (d > 0) $
  "if " ++ ppExpr 0 c ++ " then " ++ ppExpr 0 t ++ " else " ++ ppExpr 0 e
ppExpr _ (ETuple es)    = "(" ++ intercalate ", " (map (ppExpr 0) es) ++ ")"
ppExpr _ (EList es)     = "[" ++ intercalate ", " (map (ppExpr 0) es) ++ "]"
ppExpr d (ECons h t)    = paren (d > 5) $ ppExpr 6 h ++ " :: " ++ ppExpr 5 t
ppExpr d (EPipe e f)    = paren (d > 1) $ ppExpr 1 e ++ " |> " ++ ppExpr 2 f
ppExpr d (EBinOp op l r)= paren (d > 6) $ ppExpr 6 l ++ " " ++ op ++ " " ++ ppExpr 7 r
ppExpr d (EUnary op e)  = paren (d > 10) $ op ++ ppExpr 11 e
ppExpr _ (ETypeof e)    = "typeof " ++ ppExpr 11 e
ppExpr d (EAnn e t)     = paren (d > 0) $ ppExpr 1 e ++ " : " ++ ppType t

paren :: Bool -> String -> String
paren True s  = "(" ++ s ++ ")"
paren False s = s

ppType :: Type -> String
ppType (TVar n)      = n
ppType (TCon q)      = ppQName q
ppType (TApp f x)    = ppType f ++ " " ++ ppTypeAtom x
ppType (TFun a b)    = ppTypeAtom a ++ " -> " ++ ppType b
ppType (TTuple ts)   = "(" ++ intercalate ", " (map ppType ts) ++ ")"
ppType (TList t)     = "[" ++ ppType t ++ "]"

ppTypeAtom :: Type -> String
ppTypeAtom t@(TFun _ _) = "(" ++ ppType t ++ ")"
ppTypeAtom t@(TApp _ _) = "(" ++ ppType t ++ ")"
ppTypeAtom t             = ppType t

ppDecl :: Decl -> String
ppDecl (DUse n s)          = n ++ " = use \"" ++ s ++ "\"."
ppDecl (DAlias n vs t)     = "alias " ++ unwords (n:vs) ++ " = " ++ ppType t ++ "."
ppDecl (DType n vs t)      = "type " ++ unwords (n:vs) ++ " = " ++ ppType t ++ "."
ppDecl (DData n vs cons)   =
  n ++ (if null vs then "" else " " ++ unwords vs) ++ " =\n" ++
  intercalate "\n| " (map ppCon cons) ++ "."
  where ppCon (c, []) = c
        ppCon (c, ts) = c ++ " " ++ unwords (map ppTypeAtom ts)
ppDecl (DSig n t)          = n ++ " : " ++ ppType t ++ "."
ppDecl (DFunc n [] e)      = n ++ " = " ++ ppExpr 0 e ++ "."
ppDecl (DFunc n ps e)      = n ++ " " ++ unwords (map ppPat ps) ++ " = " ++ ppExpr 0 e ++ "."
ppDecl (DIso a b (e1,e2))  = "iso " ++ a ++ " " ++ b ++ " = (" ++ ppExpr 0 e1 ++ ", " ++ ppExpr 0 e2 ++ ")."
ppDecl (DEval e)           = "> " ++ ppExpr 0 e ++ "."

-- ============================================================
-- Test programs
-- ============================================================

testModule :: String
testModule = unlines
  [ "# MyMod.fpr"
  , ""
  , "std = use \"std@v1\"."
  , ""
  , "# Tagged union"
  , "MyType a = A a | B Int | C (String, Int)."
  , ""
  , "# Newtype and alias"
  , "alias MyInt = Int."
  , "type MyString = String."
  , ""
  , "# Type signature"
  , "f : MyType -> String."
  , ""
  , "# Clause-based definition with constructor patterns"
  , "f (A a) = \"A\"."
  , "f _ = \"None\"."
  , ""
  , "# REPL eval"
  , "> 1.0 + 2.0."
  , "#: 3.0"
  , ""
  , "# Negative literal"
  , "> -1.0."
  , ""
  , "# Let binding"
  , "myfunc x ="
  , "  let z = (let x = x;"
  , "               y = x + 1;"
  , "               y - 1);"
  , "      z - 1."
  , ""
  , "# Anonymous fn"
  , "myfunc' y = (fn x => x + ((fn y => y - 1) x))."
  , ""
  , "# Module-qualified call"
  , "println = std.println."
  , ""
  , "> println \"Hello FPR syntax\"."
  , "#: Output: Hello FPR syntax"
  , ""
  , "# if then else"
  , "safediv x y = if y == 0 then 0 else x / y."
  , ""
  , "# Recursive type"
  , "Queue a = Node a (Queue a) | End."
  , ""
  , "# List sugar"
  , "queuetolist (Node x xs) = x :: queuetolist xs."
  , "queuetolist End = []."
  , ""
  , "listtoqueue (x :: xs) = Node x (listtoqueue xs)."
  , "listtoqueue [] = End."
  , ""
  , "# Iso declaration"
  , "iso Queue List = (queuetolist, listtoqueue)."
  , ""
  , "# Pipeline"
  , "> (Node 1 (Node 2 (Node 3 End))) |> List.map ((+) 1)."
  ]

-- Additional focused tests
tests :: [(String, String)]
tests =
  [ ("fn expression",
     "f = fn x y => x + y.")

  , ("nested fn",
     "f = fn x => fn y => x + y.")

  , ("case expression",
     unlines
       [ "descr x ="
       , "  case x of"
       , "    A a -> \"got A\""
       , "  | B i -> \"got B\""
       , "  | C (s, i) -> s."
       ])

  , ("let expression",
     "f x = let a = x + 1; b = a * 2; b - 1.")

  , ("list literals and cons",
     "f = [1, 2, 3].")

  , ("tuple",
     "f = (1, \"hi\", 3.0).")

  , ("pipeline",
     "> xs |> List.map (fn x => x + 1) |> List.filter (fn x => x > 0).")

  , ("typeof",
     "> typeof myfunc.")

  , ("negative literal",
     "> -42.")

  , ("qualified name",
     "f = std.io.println.")

  , ("type sig complex",
     "map : (a -> b) -> List a -> List b.")
  ]

-- ============================================================
-- Main: run parser on tests and display results
-- ============================================================

banner :: String -> IO ()
banner s = do
  putStrLn ""
  putStrLn $ replicate 60 '='
  putStrLn $ "  " ++ s
  putStrLn $ replicate 60 '='

runTest :: String -> String -> IO ()
runTest label src = do
  putStrLn $ "\n[" ++ label ++ "]"
  putStrLn $ "  Input:  " ++ take 60 (filter (/= '\n') src)
  case parse (scFull *> pDecl <* scFull <* eof) "" src of
    Left err -> putStrLn $ "  ERROR:  " ++ errorBundlePretty err
    Right d  -> putStrLn $ "  Parsed: " ++ ppDecl d

main :: IO ()
main = do
  banner "Full module parse"
  case parse pModule "" testModule of
    Left err -> putStrLn $ "PARSE ERROR:\n" ++ errorBundlePretty err
    Right ds -> do
      putStrLn $ "Parsed " ++ show (length ds) ++ " declarations:\n"
      mapM_ (\d -> putStrLn ("  " ++ ppDecl d)) ds

  banner "Debug: failing eval line"
  let evalLine = "> (Node 1 (Node 2 (Node 3 End))) |> List.map ((+) 1)."
  runTest "eval pipeline" evalLine

  banner "Debug: iso line"
  let isoLine = "iso Queue List = (queuetolist, listtoqueue)."
  runTest "iso decl" isoLine
  mapM_ (uncurry runTest) tests
