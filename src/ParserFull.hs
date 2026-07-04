{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

-- Core has ONLY:
--   variables, int/string literals, application, let, if-else,
--   tagged product construction (typeid + variant id + fields),
--   tag tests, positional projections, primitive references.
--
-- Everything else is eliminated by desugaring:
--   * operators           -> function application (names resolved env -> global -> prelude prim)
--   * pipelines |> |>? >> -> application / Result-case / let-sequencing
--   * pattern matching    -> if-else trees on runtime (typeid, variant) tags + projections
--   * guards              -> if-else inside the clause chain
--   * records             -> products with shape typeids; field access = typeid-dispatched
--                            positional projection; {m | a.b = e} = dispatch + rebuild
--   * lambdas             -> lambda-lifted to top-level supercombinators (no closures:
--                            free vars become leading parameters, call sites partially apply)
--   * string interp       -> strcat/str prim applications
--   * lists, tuples       -> pre-registered constructors (Nil/Cons, Tup2/Tup3)

module ParserFull where

import Control.Monad (foldM, unless, void, when)
import Control.Monad.State.Strict
import Data.Char (isAlphaNum, isLetter, isLower, isUpper)
import Data.List (foldl', intercalate, nub, sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Void (Void)
import System.Environment (getArgs)
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

data SStmt = SStmt Name [Name] SExpr deriving (Show) -- name params = expr ;

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
  | TType Name [(Name, Int)] -- MyType = Type (Con arity | ...) .
  | TShape [Name] -- MyRecord = {a : T, b : U} .  (register shape)
  | TSkip -- signatures / other aliases: parsed, ignored
  deriving (Show)

--------------------------------------------------------------------------------
-- Core AST (the target: nothing exotic left)
--------------------------------------------------------------------------------

data Core
  = CVar Name
  | CInt Integer
  | CStr String
  | CApp Core Core
  | CLam [Name] Core -- transient: guaranteed absent after lifting
  | CLet Name Core Core
  | CIf Core Core Core
  | CMk Int Int [Core] -- typeid, variant id, fields (the ONLY data former)
  | CTagEq Int Int Core -- runtime (typeid, variant) test
  | CProj Int Core -- positional projection
  | CErr String -- let-it-crash
  deriving (Show)

-- A compiled program: supercombinators only. (name, params, body)
type Prog = M.Map Name ([Name], Core)

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

    cmpLayer = chainl1' addLayer cmpOp
    cmpOp =
      choice
        [ SBin "==" <$ try (symbol "=="),
          SBin "!=" <$ try (symbol "!="),
          SBin "<=" <$ try (symbol "<="),
          SBin ">=" <$ try (symbol ">="),
          SBin "<" <$ try (lexeme (char '<' <* notFollowedBy (oneOf "="))),
          SBin ">" <$ try (lexeme (char '>' <* notFollowedBy (oneOf ">=")))
        ]

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
    bangLayer = chainl1' appLayer bangOp
    bangOp = SBin "!" <$ try (lexeme (char '!' <* notFollowedBy (char '=')))

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
      stringLit,
      caseE,
      listLit,
      recordish,
      parensOrTuple,
      SVar <$> upperName,
      varOrProj
    ]

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

pattern' :: P SPat -- full pattern (constructors may take args)
pattern' =
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

-- name : <type stuff> .        (parsed and skipped -- no type checking in PoC)
signature :: P STop
signature = do
  _ <- (pName <|> upperName)
  _ <- lexeme (char ':' <* notFollowedBy (char ':'))
  skipTillDot
  pure TSkip

skipTillDot :: P ()
skipTillDot = void (skipManyTill anySingle dotTerm)

-- MyType = Type (Con T1 T2 | Con2 ...) .
typeDecl :: P STop
typeDecl = do
  n <- upperName
  eqSign
  _ <- symbol "Type"
  cons <- parens (conDecl `sepBy1` pipeSep)
  dotTerm
  pure (TType n cons)
  where
    conDecl = do
      c <- upperName
      arity <- length <$> many typeAtom
      pure (c, arity)
    typeAtom = void upperName <|> void (parens skipBalanced)
    skipBalanced = void (many (void (noneOf "()") <|> void (parens skipBalanced)))

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
    stmt = do
      n <- pName
      ps <- many lowerName
      eqSign
      e <- expr
      _ <- symbol ";"
      pure (SStmt n ps e)

--------------------------------------------------------------------------------
-- Desugaring
--------------------------------------------------------------------------------

data DEnv = DEnv
  { dFresh :: Int,
    dCons :: M.Map Name (Int, Int, Int), -- constructor -> (typeid, variant, arity)
    dShapes :: M.Map [Name] Int, -- sorted field set -> shape typeid
    dLifted :: [(Name, [Name], Core)] -- lambda-lifted supercombinators
  }

type D = Control.Monad.State.Strict.State DEnv

fresh :: String -> D Name
fresh pre = do
  s <- get
  put s {dFresh = dFresh s + 1}
  pure (pre ++ "_" ++ show (dFresh s))

-- Builtin constructors (typeids 0..9 reserved, user types from 10, shapes from 100)
builtinCons :: M.Map Name (Int, Int, Int)
builtinCons =
  M.fromList
    [ ("Unit", (0, 0, 0)),
      ("False", (1, 0, 0)),
      ("True", (1, 1, 0)),
      ("Nil", (2, 0, 0)),
      ("Cons", (2, 1, 2)),
      ("Ok", (3, 0, 1)),
      ("Err", (3, 1, 1)),
      ("Tup2", (4, 0, 2)),
      ("Tup3", (5, 0, 3))
    ]

boolT, listT :: Int
boolT = 1
listT = 2

-- Pass 0: registries from the surface program --------------------------------

collectCons :: [STop] -> M.Map Name (Int, Int, Int)
collectCons tops = M.union builtinCons (M.fromList user)
  where
    tdecls = [cs | TType _ cs <- tops]
    user = concat (zipWith one [10 ..] tdecls)
    one tid cs = [(c, (tid, v, ar)) | ((c, ar), v) <- zip cs [0 ..]]

collectShapes :: [STop] -> M.Map [Name] Int
collectShapes tops = M.fromList (zip allShapes [100 ..])
  where
    allShapes = nub (concatMap topShapes tops)
    topShapes (TShape fs) = [sort fs]
    topShapes (TBind _ ps g b) =
      concatMap patShapes ps
        ++ maybe [] exprShapes g
        ++ exprShapes b
    topShapes _ = []
    patShapes = \case
      PCon _ ps -> concatMap patShapes ps
      PTup ps -> concatMap patShapes ps
      _ -> []
    exprShapes = \case
      SRec fs -> [sort (map fst fs)] ++ concatMap (exprShapes . snd) fs
      SApp a b -> exprShapes a ++ exprShapes b
      SLam _ e -> exprShapes e
      SBlock ss e -> concatMap (\(SStmt _ _ x) -> exprShapes x) ss ++ exprShapes e
      SCase s as -> exprShapes s ++ concatMap (\(p, e) -> patShapes p ++ exprShapes e) as
      SBin _ a b -> exprShapes a ++ exprShapes b
      SProj e _ -> exprShapes e
      SUpd m as -> exprShapes m ++ concatMap (exprShapes . snd) as
      STup es -> concatMap exprShapes es
      SList es -> concatMap exprShapes es
      SStrI segs -> concat [exprShapes e | SegExpr e <- segs]
      _ -> []

-- Expression desugaring -------------------------------------------------------

dExpr :: SExpr -> D Core
dExpr = \case
  SVar n -> do
    cons <- gets dCons
    pure $ case M.lookup n cons of
      Just (_, _, _) -> CVar n -- constructors are ordinary globals
      Nothing -> CVar n
  SInt i -> pure (CInt i)
  SApp a b -> CApp <$> dExpr a <*> dExpr b
  SLam ps e -> CLam ps <$> dExpr e
  SBlock stmts e -> go stmts
    where
      go [] = dExpr e
      go (SStmt n ps rhs : rest) = do
        rhs' <- dExpr rhs
        let rhs'' = if null ps then rhs' else CLam ps rhs'
        CLet n rhs'' <$> go rest
  SCase scrut arms -> do
    s <- fresh "scrut"
    sc' <- dExpr scrut
    body <-
      compileArms
        (CVar s)
        [(p, Nothing, e) | (p, e) <- arms]
        (CErr "case: no matching pattern")
    pure (CLet s sc' body)

  -- pipelines / operators
  SBin "|>" a f -> CApp <$> dExpr f <*> dExpr a
  SBin ">>" a b -> do
    a' <- dExpr a
    b' <- dExpr b
    u <- fresh "seq"
    pure (CLet u a' b')
  SBin "|>?" a f -> do
    -- case a of Ok v -> f v | Err e -> Err e
    v <- fresh "ok"
    e <- fresh "err"
    dExpr
      ( SCase
          a
          [ (PCon "Ok" [PVar v], SApp f (SVar v)),
            (PCon "Err" [PVar e], SApp (SVar "Err") (SVar e))
          ]
      )
  SBin op a b -> do
    -- a + b ==> (+) a b, name resolved lexically
    a' <- dExpr a
    b' <- dExpr b
    pure (CApp (CApp (CVar op) a') b')
  SProj e path -> do
    e' <- dExpr e
    foldM projField e' path
  SRec fs -> do
    let sorted = sortOn fst fs
    tid <- shapeId (map fst sorted)
    CMk tid 0 <$> mapM (dExpr . snd) sorted
  SUpd m assigns -> do
    m' <- dExpr m
    v <- fresh "rec"
    body <- updateRecord (CVar v) assigns
    pure (CLet v m' body)
  STup es -> do
    let con = if length es == 3 then "Tup3" else "Tup2"
    (tid, var, _) <- conInfo con
    CMk tid var <$> mapM dExpr es
  SList es ->
    dExpr
      ( foldr
          (\x acc -> SApp (SApp (SVar "Cons") x) acc)
          (SVar "Nil")
          es
      )
  SStrI segs -> do
    parts <- mapM segCore segs
    pure $ case parts of
      [] -> CStr ""
      (p : ps) -> foldl' (\acc x -> CApp (CApp (CVar "strcat") acc) x) p ps
    where
      segCore (SegStr s) = pure (CStr s)
      segCore (SegExpr e) = CApp (CVar "str") <$> dExpr e

conInfo :: Name -> D (Int, Int, Int)
conInfo c = do
  cons <- gets dCons
  case M.lookup c cons of
    Just i -> pure i
    Nothing -> error ("unknown constructor: " ++ c)

shapeId :: [Name] -> D Int
shapeId fs = do
  shapes <- gets dShapes
  case M.lookup (sort fs) shapes of
    Just i -> pure i
    Nothing -> error ("unknown record shape: " ++ show fs)

-- Field projection: typeid-dispatched positional projection.
-- Single candidate shape -> direct CProj. Several -> if-else chain on tag.
projField :: Core -> Name -> D Core
projField scrut f = do
  shapes <- gets dShapes
  let cands =
        [ (tid, idx) | (fs, tid) <- M.toList shapes, f `elem` fs, let idx = length (takeWhile (/= f) (sort fs))
        ]
  case cands of
    [] -> error ("no record shape has field ." ++ f)
    [(_, idx)] -> pure (CProj idx scrut)
    many' -> do
      v <- fresh "r"
      let chain =
            foldr
              ( \(tid, idx) rest ->
                  CIf (CTagEq tid 0 (CVar v)) (CProj idx (CVar v)) rest
              )
              (CErr ("no shape with field ." ++ f ++ " matched"))
              many'
      pure (CLet v scrut chain)

-- {m | b = 2, p.q = 1} : dispatch on m's shape, rebuild the product,
-- recursing for nested paths.
updateRecord :: Core -> [([Name], SExpr)] -> D Core
updateRecord scrut assigns = do
  shapes <- gets dShapes
  let roots = nub (map (head . fst) assigns)
      cands =
        [ (fs, tid) | (fs, tid) <- M.toList shapes, all (`elem` fs) roots
        ]
  when (null cands) $ error ("no record shape has fields " ++ show roots)
  arms <- mapM rebuild cands
  pure $ case arms of
    [(_, body)] -> body
    _ ->
      foldr
        (\(tid, body) rest -> CIf (CTagEq tid 0 scrut) body rest)
        (CErr "record update: no shape matched")
        arms
  where
    rebuild (fs, tid) = do
      let sorted = sort fs
      fields <- mapM (fieldValue tid) (zip [0 ..] sorted)
      pure (tid, CMk tid 0 fields)
      where
        fieldValue _ (idx, f) =
          case [(path, e) | (path, e) <- assigns, head path == f] of
            [] -> pure (CProj idx scrut)
            [([_], e)] -> dExpr e -- direct replace
            deeper -> do
              -- nested path(s)
              let subAssigns = [(tail p, e) | (p, e) <- deeper]
              updateRecord (CProj idx scrut) subAssigns

-- Pattern compilation ---------------------------------------------------------
-- clauses: (patterns matched against scrutinee(s), optional guard, body)
-- Compiles to an if-else chain on (typeid, variant) tags with projections.

compileArms :: Core -> [(SPat, Maybe SExpr, SExpr)] -> Core -> D Core
compileArms scrut arms fallback = go arms
  where
    go [] = pure fallback
    go ((p, g, body) : rest) = do
      nxt <- go rest
      body' <- dExpr body
      inner <- case g of
        Nothing -> pure body'
        Just ge -> do
          ge' <- dExpr ge
          pure (CIf ge' body' nxt)
      matchPat scrut p inner nxt

-- matchPat scrut pat onSuccess onFail
matchPat :: Core -> SPat -> Core -> Core -> D Core
matchPat scrut p ok fail' = case p of
  PWild -> pure ok
  PVar x -> pure (CLet x scrut ok)
  PInt n -> pure (CIf (CApp (CApp (CVar "==") scrut) (CInt n)) ok fail')
  PStr s -> pure (CIf (CApp (CApp (CVar "==") scrut) (CStr s)) ok fail')
  PCon c ps -> do
    (tid, var, ar) <- conInfo c
    unless (length ps == ar) $
      error ("constructor " ++ c ++ " arity mismatch in pattern")
    inner <- matchFields scrut ps ok fail'
    pure (CIf (CTagEq tid var scrut) inner fail')
  PTup ps -> do
    let con = if length ps == 3 then "Tup3" else "Tup2"
    (tid, var, _) <- conInfo con
    inner <- matchFields scrut ps ok fail'
    pure (CIf (CTagEq tid var scrut) inner fail')
  PRec fs -> do
    -- row-polymorphic punning: just bind, no tag test
    let bind [] = pure ok
        bind (f : rest) = do
          proj <- projField scrut f
          CLet f proj <$> bind rest
    bind fs

matchFields :: Core -> [SPat] -> Core -> Core -> D Core
matchFields scrut ps ok fail' = go (zip [0 ..] ps)
  where
    go [] = pure ok
    go ((i, p) : rest) = do
      inner <- go rest
      matchPat (CProj i scrut) p inner fail'

-- Top-level compilation -------------------------------------------------------

-- Merge consecutive clauses of the same name, compile pattern/guard chains.
compileTop :: [STop] -> D Prog
compileTop tops = do
  cons <- gets dCons
  let conGlobals =
        [ (c, (params, CMk tid var (map CVar params)))
          | (c, (tid, var, ar)) <- M.toList cons,
            let params = ["x" ++ show i | i <- [1 .. ar]]
        ]
  binds <- mapM compileGroup (groupClauses [b | b@TBind {} <- tops])
  pure (M.fromList (conGlobals ++ binds))
  where
    groupClauses [] = []
    groupClauses (TBind n ps g b : rest) =
      let (same, others) = span (\(TBind n' _ _ _) -> n' == n) rest
       in (n, (ps, g, b) : [(ps', g', b') | TBind _ ps' g' b' <- same])
            : groupClauses others
    groupClauses (_ : rest) = groupClauses rest

compileGroup :: (Name, [([SPat], Maybe SExpr, SExpr)]) -> D (Name, ([Name], Core))
compileGroup (n, clauses@((ps0, _, _) : _)) = do
  let arity = length ps0
  args <- mapM (\i -> fresh ("a" ++ show i)) [1 .. arity]
  body <- goClauses args clauses
  pure (n, (args, body))
  where
    goClauses _ [] = pure (CErr ("no matching clause for " ++ n))
    goClauses args ((ps, g, b) : rest) = do
      nxt <- goClauses args rest
      b' <- dExpr b
      inner <- case g of
        Nothing -> pure b'
        Just ge -> do ge' <- dExpr ge; pure (CIf ge' b' nxt)
      matchMany (zip (map CVar args) ps) inner nxt
    matchMany [] ok _ = pure ok
    matchMany ((s, p) : rest) ok fail' = do
      inner <- matchMany rest ok fail'
      matchPat s p inner fail'
compileGroup (n, []) = error ("empty clause group: " ++ n)

--------------------------------------------------------------------------------
-- Lambda lifting: eliminate CLam entirely. Free vars become leading params;
-- the lambda becomes a top-level supercombinator, the use site a partial app.
--------------------------------------------------------------------------------

liftProg :: Prog -> D Prog
liftProg prog = do
  let globalNames = M.keysSet prog
  lifted <- M.traverseWithKey (\_ (ps, b) -> (ps,) <$> liftC globalNames ps b) prog
  extra <- gets dLifted
  extra' <-
    mapM
      ( \(n, ps, b) -> do
          b' <- liftC globalNames ps b
          pure (n, (ps, b'))
      )
      extra
  pure (M.union lifted (M.fromList extra'))
  where
    liftC globals bound = go (foldr (:) [] bound)
      where
        go env = \case
          CLam ps body -> do
            body' <- go (ps ++ env) body
            let fvs =
                  nub
                    [ v | v <- freeVars body', v `notElem` ps, not (v `M.member` prog), v `notElem` primNames
                    ]
                capture = filter (`elem` (env :: [Name])) fvs
            nm <- fresh "lifted"
            modify (\s -> s {dLifted = (nm, capture ++ ps, body') : dLifted s})
            pure (foldl' CApp (CVar nm) (map CVar capture))
          CApp a b -> CApp <$> go env a <*> go env b
          CLet x a b -> CLet x <$> go env a <*> go (x : env) b
          CIf c t e -> CIf <$> go env c <*> go env t <*> go env e
          CMk t v fs -> CMk t v <$> mapM (go env) fs
          CTagEq t v e -> CTagEq t v <$> go env e
          CProj i e -> CProj i <$> go env e
          other -> pure other

freeVars :: Core -> [Name]
freeVars = \case
  CVar n -> [n]
  CApp a b -> freeVars a ++ freeVars b
  CLam ps b -> filter (`notElem` ps) (freeVars b)
  CLet x a b -> freeVars a ++ filter (/= x) (freeVars b)
  CIf c t e -> freeVars c ++ freeVars t ++ freeVars e
  CMk _ _ fs -> concatMap freeVars fs
  CTagEq _ _ e -> freeVars e
  CProj _ e -> freeVars e
  _ -> []

-- Iterate lifting until no CLam remains (lifted bodies may contain lambdas).
liftFix :: Prog -> D Prog
liftFix p = do
  modify (\s -> s {dLifted = []})
  p' <- liftProg p
  if any (hasLam . snd . snd) (M.toList p') then liftFix p' else pure p'
  where
    hasLam = \case
      CLam _ _ -> True
      CApp a b -> hasLam a || hasLam b
      CLet _ a b -> hasLam a || hasLam b
      CIf c t e -> hasLam c || hasLam t || hasLam e
      CMk _ _ fs -> any hasLam fs
      CTagEq _ _ e -> hasLam e
      CProj _ e -> hasLam e
      _ -> False

--------------------------------------------------------------------------------
-- Pretty printer for Core
--------------------------------------------------------------------------------

pretty :: Core -> String
pretty = go 0
  where
    ind n = replicate (n * 2) ' '
    go _ (CVar n) = n
    go _ (CInt i) = show i
    go _ (CStr s) = show s
    go _ (CErr m) = "error " ++ show m
    go d e@CApp {} =
      let (f, as) = spine e
       in "(" ++ unwords (map (go d) (f : as)) ++ ")"
    go d (CLam ps b) = "(\\" ++ unwords ps ++ " -> " ++ go d b ++ ")"
    go d (CLet x a b) =
      "let " ++ x ++ " = " ++ go (d + 1) a ++ " in\n" ++ ind (d + 1) ++ go (d + 1) b
    go d (CIf c t e) =
      "if "
        ++ go d c
        ++ "\n"
        ++ ind (d + 1)
        ++ "then "
        ++ go (d + 1) t
        ++ "\n"
        ++ ind (d + 1)
        ++ "else "
        ++ go (d + 1) e
    go d (CMk t v fs) =
      "mk["
        ++ show t
        ++ "."
        ++ show v
        ++ "]("
        ++ intercalate ", " (map (go d) fs)
        ++ ")"
    go d (CTagEq t v e) = "tag?(" ++ go d e ++ " == " ++ show t ++ "." ++ show v ++ ")"
    go d (CProj i e) = "proj." ++ show i ++ "(" ++ go d e ++ ")"
    spine (CApp a b) = let (f, as) = spine a in (f, as ++ [b])
    spine f = (f, [])

prettyProg :: Prog -> [Name] -> String
prettyProg prog names =
  unlines
    [ n ++ " " ++ unwords ps ++ " =\n  " ++ pretty b ++ "\n"
      | n <- names,
        Just (ps, b) <- [M.lookup n prog]
    ]

--------------------------------------------------------------------------------
-- Evaluator (strict). Values: ints, strings, tagged products, and partial
-- applications of NAMED code (global or prim) -- i.e. "an address plus args
-- collected so far", per the no-closures calling convention.
--------------------------------------------------------------------------------

data Value
  = VInt Integer
  | VStr String
  | VData Int Int [Value]
  | VPap PapRef [Value] Int -- code ref, args so far, remaining arity
  deriving (Show)

data PapRef = PGlobal Name | PPrim Name deriving (Show)

primNames :: [Name]
primNames =
  [ "+",
    "-",
    "*",
    "/",
    "==",
    "!=",
    "<",
    ">",
    "<=",
    ">=",
    "!",
    "print",
    "String.len",
    "str",
    "strcat",
    "error"
  ]

primArity :: Name -> Int
primArity n
  | n `elem` ["print", "String.len", "str", "error"] = 1
  | otherwise = 2

vTrue, vFalse, vUnit :: Value
vTrue = VData boolT 1 []
vFalse = VData boolT 0 []
vUnit = VData 0 0 []

eval :: Prog -> M.Map Name Value -> Core -> IO Value
eval prog env = go
  where
    go = \case
      CInt i -> pure (VInt i)
      CStr s -> pure (VStr s)
      CErr m -> errorWithoutStackTrace ("sol: " ++ m)
      CVar n -> case M.lookup n env of
        Just v -> pure v
        Nothing -> case M.lookup n prog of
          Just ([], body) -> eval prog M.empty body -- 0-ary global
          Just (ps, _) -> pure (VPap (PGlobal n) [] (length ps))
          Nothing
            | n `elem` primNames -> pure (VPap (PPrim n) [] (primArity n))
            | otherwise -> errorWithoutStackTrace ("unbound: " ++ n)
      CApp f a -> do
        fv <- go f
        av <- go a
        apply prog fv av
      CLam {} -> errorWithoutStackTrace "internal: CLam survived lifting"
      CLet x a b -> do
        av <- go a
        eval prog (M.insert x av env) b
      CIf c t e ->
        go c >>= \case
          VData t' v' _ | t' == boolT -> if v' == 1 then go t else go e
          other -> errorWithoutStackTrace ("if: non-bool " ++ show other)
      CMk t v fs -> VData t v <$> mapM go fs
      CTagEq t v e ->
        go e >>= \case
          VData t' v' _ -> pure (if t == t' && v == v' then vTrue else vFalse)
          _ -> pure vFalse
      CProj i e ->
        go e >>= \case
          VData _ _ fs | i < length fs -> pure (fs !! i)
          other -> errorWithoutStackTrace ("proj: bad value " ++ show other)

apply :: Prog -> Value -> Value -> IO Value
apply prog (VPap ref args 1) a = call prog ref (reverse (a : args))
apply prog (VPap ref args n) a = pure (VPap ref (a : args) (n - 1))
apply _ v _ = errorWithoutStackTrace ("apply: not a function: " ++ show v)

call :: Prog -> PapRef -> [Value] -> IO Value
call prog (PGlobal n) args = case M.lookup n prog of
  Just (ps, body) -> eval prog (M.fromList (zip ps args)) body
  Nothing -> errorWithoutStackTrace ("no global: " ++ n)
call prog (PPrim n) args = prim prog n args

prim :: Prog -> Name -> [Value] -> IO Value
prim _ "print" [v] = putStrLn (render v) >> pure vUnit
prim _ "str" [v] = pure (VStr (render v))
prim _ "strcat" [VStr a, VStr b] = pure (VStr (a ++ b))
prim _ "String.len" [VStr s] = pure (VInt (fromIntegral (length s)))
prim _ "error" [VStr m] = errorWithoutStackTrace ("sol: " ++ m)
prim _ "+" [VInt a, VInt b] = pure (VInt (a + b))
prim _ "-" [VInt a, VInt b] = pure (VInt (a - b))
prim _ "*" [VInt a, VInt b] = pure (VInt (a * b))
prim _ "/" [VInt a, VInt b] =
  if b == 0
    then errorWithoutStackTrace "sol: division by zero"
    else pure (VInt (a `div` b))
prim _ "==" [a, b] = pure (bool (veq a b))
prim _ "!=" [a, b] = pure (bool (not (veq a b)))
prim _ "<" [VInt a, VInt b] = pure (bool (a < b))
prim _ ">" [VInt a, VInt b] = pure (bool (a > b))
prim _ "<=" [VInt a, VInt b] = pure (bool (a <= b))
prim _ ">=" [VInt a, VInt b] = pure (bool (a >= b))
prim _ "!" [VData t v fs, VInt i] -- 1-indexed lookup (Sol)
  | t == listT = index (VData t v fs) i
  where
    index (VData _ 1 [x, rest]) 1 = pure x
    index (VData _ 1 [_, rest]) k = index rest (k - 1)
    index _ _ = errorWithoutStackTrace "sol: index out of range"
prim _ n args =
  errorWithoutStackTrace
    ( "prim "
        ++ n
        ++ ": bad args (typeid dispatch found no case): "
        ++ show args
    )

bool :: Bool -> Value
bool b = if b then vTrue else vFalse

veq :: Value -> Value -> Bool
veq (VInt a) (VInt b) = a == b
veq (VStr a) (VStr b) = a == b
veq (VData t v fs) (VData t' v' fs') =
  t == t' && v == v' && length fs == length fs' && and (zipWith veq fs fs')
veq _ _ = False

render :: Value -> String
render (VInt i) = show i
render (VStr s) = s
render (VData t 1 [x, rest])
  | t == listT =
      "[" ++ intercalate ", " (renderList (VData t 1 [x, rest])) ++ "]"
  where
    renderList (VData _ 1 [y, r]) = render y : renderList r
    renderList _ = []
render (VData t 0 []) | t == listT = "[]"
render (VData 1 0 []) = "False"
render (VData 1 1 []) = "True"
render (VData 0 0 []) = "()"
render (VData t v fs) =
  "<"
    ++ show t
    ++ "."
    ++ show v
    ++ (if null fs then "" else " " ++ unwords (map render fs))
    ++ ">"
render (VPap ref args n) = "<fn " ++ show ref ++ "/" ++ show n ++ ">"

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  let file = case args of (f : _) -> f; [] -> "example.sol"
  src <- readFile file
  case parse program file src of
    Left err -> putStrLn (errorBundlePretty err)
    Right tops -> do
      let cons = collectCons tops
          shapes = collectShapes tops
          st0 = DEnv 0 cons shapes []
          (prog, _) = runState (compileTop tops >>= liftFix) st0
      putStrLn "=== PARSE: OK ==="
      putStrLn ""
      putStrLn ("type constructors: " ++ show (M.toList (M.difference cons builtinCons)))
      putStrLn ("record shapes (sorted fields -> typeid): " ++ show (M.toList shapes))
      putStrLn ""
      putStrLn "=== DESUGARED CORE (selected bindings) ==="
      putStrLn ""
      let interesting =
            [ n | n <- M.keys prog, n `notElem` M.keys builtinCons
            ]
      putStrLn (prettyProg prog interesting)
      putStrLn "=== EVAL main ==="
      _ <- eval prog M.empty (CVar "main")
      pure ()
