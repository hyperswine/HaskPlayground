{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- Sol surface syntax parser (sliced from the interpreter PoC).
module SolParser where

import Control.Monad (void, when)
import Data.Char (isAlphaNum, isLetter, isLower, isUpper)
import Data.List (foldl', intercalate)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Name = String

--------------------------------------------------------------------------------
-- Surface AST
--------------------------------------------------------------------------------

data SExpr
  = SVar Name
  | SInt Integer
  | SAtom Name -- :nextid  (interned symbol)
  | SStrI [Seg] -- interpolated string
  | SApp SExpr SExpr
  | SLam [Name] SExpr -- fn x y -> e
  | SBlock [SStmt] SExpr -- a = 1; b = 2; e     (let sugar)
  | SCase SExpr [(SPat, SExpr)]
  | SBin Name SExpr SExpr -- a + b  ==>  (+) a b
  | SProj SExpr [Name] -- e.f.g   (dotted projection chain)
  | SRec [(Name, SExpr)] -- {a = 1, b = 2}
  | SUpd SExpr [([Name], SExpr)] -- {m | b = 2, p.q = 1}
  | STup [SExpr]
  | SList [SExpr]
  deriving (Show)

data Seg = SegStr String | SegExpr SExpr deriving (Show)

data SStmt
  = SBind Name [Name] SExpr -- name params = expr ;
  | SBindPat SPat SExpr -- (i1, i2) :: rest = expr ;   (refutable, crash on mismatch)
  deriving (Show)

data SPat
  = PVar Name
  | PWild
  | PInt Integer
  | PStr String
  | PCon Name [SPat]
  | PTup [SPat]
  | PRec [Name] -- {a, b} field punning (row-polymorphic)
  deriving (Show)

data STop
  = TBind Name [SPat] (Maybe SExpr) SExpr -- one clause: name pats [| guard] = body .
  | TType Name Bool [(Name, [Ty])] -- MyType a b = Type (Con tys | ...) . Bool = linear (`Name 1 = ...`)
  | TShape [Name] -- MyRecord = {a : T, b : U} .  (register shape)
  | TSig Name ([Ty], Ty) -- name : T1 -> T2 -> Tr .  (params, return)
  | TSkip -- unparseable signatures / other aliases: skipped
  deriving (Show)

-- Minimal type expressions: just enough for constructor args and signatures.
data Ty = TCon Name [Ty] | TVarT Name | TTup [Ty] | TOther
  deriving (Show)

--------------------------------------------------------------------------------
-- Lexer
--------------------------------------------------------------------------------

type P = Parsec Void String

sc :: P ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: P a -> P a
lexeme = L.lexeme sc

symbol :: String -> P String
symbol = L.symbol sc

identChar :: Char -> Bool
identChar c = isAlphaNum c || c == '_' || c == '\''

reserved :: [String]
reserved = ["fn", "case", "of", "Type"]

-- A dotted identifier: seg(.seg)* where '.' continues the chain only if the
-- next char is a letter. This is what disambiguates projection ("x.f") from
-- the binding terminator ("x.<newline>") with zero whitespace sensitivity.
dottedIdent :: P [String]
dottedIdent = lexeme $ do
  first <- rawSeg
  rest <- many (try (char '.' *> rawSeg))
  let n = first : rest
  when (first `elem` reserved) $ fail ("reserved word: " ++ first)
  pure n
  where
    rawSeg = (:) <$> satisfy isLetter <*> takeWhileP Nothing identChar

lowerName :: P Name
lowerName = try $ do
  segs <- dottedIdent
  case segs of
    [s] | isLower (head s) -> pure s
    _ -> fail "expected simple lowercase identifier"

-- Binding/field position name: identifier or parenthesized operator, e.g. (+)
pName :: P Name
pName = try (parens operatorName) <|> lowerName
  where
    operatorName = lexeme (some (oneOf "+-*/=!<>|$?"))

upperName :: P Name -- possibly qualified: String.len stays one name
upperName = try $ do
  segs <- dottedIdent
  case segs of
    (s : _) | isUpper (head s) -> pure (intercalate "." segs)
    _ -> fail "expected uppercase identifier"

integer :: P Integer
integer = lexeme L.decimal

parens :: P a -> P a
parens = between (symbol "(") (symbol ")")

braces :: P a -> P a
braces = between (symbol "{") (symbol "}")

brackets :: P a -> P a
brackets = between (symbol "[") (symbol "]")

-- '.' terminator: a dot NOT continuing an identifier chain.
dotTerm :: P ()
dotTerm = lexeme . try $ void (char '.' <* notFollowedBy (satisfy isLetter))

-- '|' as clause/arm/guard separator: must not be |> or |>?
pipeSep :: P ()
pipeSep = lexeme . try $ void (char '|' <* notFollowedBy (oneOf ">?"))

-- '=' in bindings: must not be ==
eqSign :: P ()
eqSign = lexeme . try $ void (char '=' <* notFollowedBy (char '='))

--------------------------------------------------------------------------------
-- Expression parser
--------------------------------------------------------------------------------

-- Operator table is CLOSED. Each binary op desugars to SBin op a b, which in
-- turn becomes application of the name "op" -- resolved lexically at eval time
-- (locals shadow globals shadow Prelude prims), exactly per the design.

expr :: P SExpr
expr = lamE <|> opExpr
  where
    lamE = do
      _ <- symbol "fn"
      ps <- some lowerName
      _ <- symbol "->"
      SLam ps <$> expr

opExpr :: P SExpr
opExpr = dollarChain
  where
    -- \$ : lowest, right assoc, pure application sugar
    dollarChain = do
      a <- pipeChain
      option a (symbol "$" *> (SApp a <$> dollarChain))

    -- \|> |>? >> : left assoc pipeline layer; a lambda may appear as an operand
    pipeChain = chainl1' pipeOperand pipeOp
    pipeOperand =
      ( do
          _ <- symbol "fn"
          ps <- some lowerName
          _ <- symbol "->"
          SLam ps <$> expr
      )
        <|> cmpLayer
    pipeOp =
      choice
        [ SBin "|>?" <$ try (symbol "|>?"),
          SBin "|>" <$ try (symbol "|>"),
          SBin ">>" <$ try (lexeme (string ">>" <* notFollowedBy (char '=')))
        ]

    cmpLayer = chainl1' consLayer cmpOp
    cmpOp =
      choice
        [ SBin "==" <$ try (symbol "=="),
          SBin "!=" <$ try (symbol "!="),
          SBin "<=" <$ try (symbol "<="),
          SBin ">=" <$ try (symbol ">="),
          SBin "<" <$ try (lexeme (char '<' <* notFollowedBy (oneOf "="))),
          SBin ">" <$ try (lexeme (char '>' <* notFollowedBy (oneOf ">=")))
        ]

    -- :: cons sugar, right assoc: x :: xs ==> Cons x xs
    consLayer = do
      a <- addLayer
      option a (try (symbol "::") *> (SBin "::" a <$> consLayer))

    addLayer = chainl1' mulLayer addOp
    addOp =
      choice
        [ SBin "+" <$ try (symbol "+"),
          SBin "-" <$ try (lexeme (char '-' <* notFollowedBy (char '>')))
        ]

    mulLayer = chainl1' bangLayer mulOp
    mulOp =
      choice
        [ SBin "*" <$ symbol "*",
          SBin "/" <$ symbol "/"
        ]

    -- ! : lookup operator (1-indexed in Sol)
    bangLayer = chainl1' btLayer bangOp
    bangOp = SBin "!" <$ try (lexeme (char '!' <* notFollowedBy (char '=')))

    -- backtick infix: a `f` b  ==>  f a b   (tightest binary layer; the
    -- symbol set stays closed -- only already-named functions, no new tokens)
    btLayer = chainl1' appLayer btOp
    btOp =
      (\f a b -> SApp (SApp (SVar f) a) b)
        <$> try (lexeme (char '`' *> ident' <* char '`'))
      where
        ident' = (:) <$> satisfy isLower <*> takeWhileP Nothing identChar

    appLayer = do
      f <- term
      as <- many term
      pure (foldl' SApp f as)

chainl1' :: P a -> P (a -> a -> a) -> P a
chainl1' p op = p >>= rest
  where
    rest a = (do f <- op; b <- p; rest (f a b)) <|> pure a

term :: P SExpr
term =
  choice
    [ SInt <$> integer,
      atomLit,
      stringLit,
      caseE,
      listLit,
      recordish,
      parensOrTuple,
      SVar <$> upperName,
      varOrProj
    ]

-- :nextid  -- ':' immediately followed by a lowercase letter
atomLit :: P SExpr
atomLit = lexeme . try $ do
  _ <- char ':'
  s <- (:) <$> satisfy isLower <*> takeWhileP Nothing identChar
  pure (SAtom s)

varOrProj :: P SExpr
varOrProj = try $ do
  segs <- dottedIdent
  case segs of
    [s] | isLower (head s) -> pure (SVar s)
    (s : rest) | isLower (head s) -> pure (SProj (SVar s) rest)
    _ -> fail "not a lowercase name"

parensOrTuple :: P SExpr
parensOrTuple = parens $ do
  e <- expr
  es <- many (symbol "," *> expr)
  pure $ if null es then e else STup (e : es)

listLit :: P SExpr
listLit = SList <$> brackets (expr `sepBy` symbol ",")

-- {a = 1, b = 2}  vs  {m | b = 2, p.q = 1}
recordish :: P SExpr
recordish = braces (try litRec <|> updRec)
  where
    litRec = SRec <$> (fieldAssign `sepBy1` symbol ",")
    fieldAssign = do n <- pName; eqSign; (n,) <$> expr
    updRec = do
      m <- expr
      pipeSep
      as <- pathAssign `sepBy1` symbol ","
      pure (SUpd m as)
    pathAssign = do
      path <- dottedIdent
      eqSign
      (path,) <$> expr

caseE :: P SExpr
caseE = do
  _ <- symbol "case"
  scrut <- expr
  _ <- symbol "of"
  arms <- arm `sepBy1` pipeSep
  pure (SCase scrut arms)
  where
    arm = do p <- pattern'; _ <- symbol "->"; (p,) <$> expr

-- String literals with {expr} interpolation. \{ \\ \" \n escapes.
stringLit :: P SExpr
stringLit = lexeme $ do
  _ <- char '"'
  segs <- manyTill seg (char '"')
  pure (SStrI (mergeSegs segs))
  where
    seg =
      choice
        [ SegExpr <$> (char '{' *> sc *> expr <* char '}'),
          SegStr . pure <$> (char '\\' *> escaped),
          SegStr . pure <$> satisfy (\c -> c /= '"' && c /= '{' && c /= '\\')
        ]
    escaped =
      choice
        [ '\n' <$ char 'n',
          '\t' <$ char 't',
          '{' <$ char '{',
          '}' <$ char '}',
          '"' <$ char '"',
          '\\' <$ char '\\'
        ]
    mergeSegs (SegStr a : SegStr b : r) = mergeSegs (SegStr (a ++ b) : r)
    mergeSegs (x : r) = x : mergeSegs r
    mergeSegs [] = []

--------------------------------------------------------------------------------
-- Patterns
--------------------------------------------------------------------------------

pattern' :: P SPat -- full pattern (constructors may take args, :: cons)
pattern' = do
  p <- patApp
  option p (try (symbol "::") *> (PCon "Cons" . (p :) . pure <$> pattern'))

patApp :: P SPat
patApp =
  choice
    [ do c <- upperName; args <- many patAtom; pure (PCon c args),
      patAtom
    ]

patAtom :: P SPat
patAtom =
  choice
    [ PWild <$ symbol "_",
      PInt <$> integer,
      strPat,
      PRec <$> braces (lowerName `sepBy1` symbol ","),
      parens patInParens,
      flip PCon [] <$> upperName, -- 0-ary constructor as atom
      PVar <$> lowerName
    ]
  where
    strPat = lexeme $ do
      _ <- char '"'
      s <- manyTill (satisfy (/= '"')) (char '"')
      pure (PStr s)
    patInParens = do
      p <- pattern'
      ps <- many (symbol "," *> pattern')
      pure $ if null ps then p else PTup (p : ps)

--------------------------------------------------------------------------------
-- Top level
--------------------------------------------------------------------------------

program :: P [STop]
program = sc *> many topDecl <* eof

topDecl :: P STop
topDecl =
  choice
    [ try typeDecl,
      try shapeAlias,
      try otherAlias,
      try signature,
      binding
    ]

-- name : <type> .   Parsed for real when possible (feeds the linearity checker);
-- signatures using record rows etc. fall back to being skipped.
signature :: P STop
signature = do
  n <- (pName <|> upperName)
  _ <- lexeme (char ':' <* notFollowedBy (char ':'))
  try (fullSig n) <|> (skipTillDot >> pure TSkip)
  where
    fullSig n = do
      parts <- tyApp `sepBy1` symbol "->"
      dotTerm
      case parts of
        [] -> fail "empty signature"
        ps -> pure (TSig n (init ps, last ps))

-- minimal type expressions -----------------------------------------------

tyApp :: P Ty
tyApp = do
  atoms <- some tyAtom
  pure $ case atoms of
    [t] -> t
    (TCon n [] : args) -> TCon n args
    _ -> TOther

tyAtom :: P Ty
tyAtom =
  choice
    [ parens tyTuple,
      flip TCon [] <$> upperName,
      TVarT <$> lowerName
    ]
  where
    tyTuple = do
      t <- tyApp
      ts <- many (symbol "," *> tyApp)
      pure $ if null ts then t else TTup (t : ts)

skipTillDot :: P ()
skipTillDot = void (skipManyTill anySingle dotTerm)

-- MyType a b = Type (Con T1 T2 | ...) .        parameterized tagged union
-- File 1 = Type Int .                          linear newtype: constructor = type name
-- Multiplicity `1` sits right after the name; type args are erased at runtime.
typeDecl :: P STop
typeDecl = do
  n <- upperName
  mult <- optional integer
  _params <- many lowerName -- type parameters: erased, arity unchecked in PoC
  eqSign
  _ <- symbol "Type"
  cons <-
    parens (conDecl `sepBy1` pipeSep)
      <|> newtypeCon n -- Type Int  ==>  single constructor named n
  dotTerm
  pure (TType n (mult == Just 1) cons)
  where
    conDecl = do
      c <- upperName
      args <- many tyAtom
      pure (c, args)
    newtypeCon n = do
      args <- some tyAtom
      pure [(n, args)]

-- MyRecord = {a : String, b : Int} .   (register the shape's field set)
shapeAlias :: P STop
shapeAlias = do
  _ <- upperName
  eqSign
  fs <- braces (fieldDecl `sepBy1` symbol ",")
  dotTerm
  pure (TShape fs)
  where
    fieldDecl = do
      f <- pName
      _ <- lexeme (char ':')
      _ <- some (noneOf ",}") -- type expr: skipped
      pure f

-- Length = Nat .  and other aliases we don't model: skip
otherAlias :: P STop
otherAlias = do
  _ <- upperName
  eqSign
  _ <- upperName
  dotTerm
  pure TSkip

-- One function/constant clause. Body is a block (let-sugar with ';').
binding :: P STop
binding = do
  n <- pName
  pats <- many patAtom
  g <- optional (pipeSep *> expr)
  eqSign
  body <- block
  dotTerm
  pure (TBind n pats g body)

block :: P SExpr
block = do
  stmts <- many (try stmt)
  e <- expr
  pure $ if null stmts then e else SBlock stmts e
  where
    stmt = try nameStmt <|> patStmt
    nameStmt = do
      n <- pName
      ps <- many lowerName
      eqSign
      e <- expr
      _ <- symbol ";"
      pure (SBind n ps e)
    patStmt = do
      -- refutable let: crash on mismatch
      p <- pattern'
      eqSign
      e <- expr
      _ <- symbol ";"
      pure (SBindPat p e)
