{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module FParser (parseExpr, parseProgram, parseFile) where

import Control.Monad (void)
import Data.Void (Void)
import FPInterpreter (Expr (..), Pattern (..), Value (..))
import Text.Megaparsec (MonadParsec (eof, notFollowedBy, try), Parsec, choice, empty, errorBundlePretty, many, manyTill, option, optional, runParser, sepBy, sepBy1, sepEndBy1, (<|>))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- ─────────────────────────────────────────────────────────────────────────────
-- PARSER TYPE
-- ─────────────────────────────────────────────────────────────────────────────

type Parser = Parsec Void String

-- ─────────────────────────────────────────────────────────────────────────────
-- WHITESPACE
--
--   sc    skip spaces/tabs only, line comments ok  (NEVER eats newlines)
--   scn   skip any whitespace including newlines
--
-- Rule: every `lexeme` uses `sc`.  Positions that allow the next token to
-- start on the next line (after `=>`, `=`, `then`, `else`, `{`, `,`) call
-- `scn` explicitly after the delimiter.
-- ─────────────────────────────────────────────────────────────────────────────

sc :: Parser ()
sc = L.space hspace1 (L.skipLineComment "--") empty

scn :: Parser ()
scn = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sym :: String -> Parser ()
sym s = void (lexeme (string s))

-- sym then allow newline continuation
symN :: String -> Parser ()
symN s = sym s *> scn

kw :: String -> Parser ()
kw s = lexeme $ try (string s *> notFollowedBy (alphaNumChar <|> char '_'))

kwN :: String -> Parser ()
kwN s = kw s *> scn

-- ─────────────────────────────────────────────────────────────────────────────
-- KEYWORDS
-- ─────────────────────────────────────────────────────────────────────────────

keywords :: [String]
keywords = ["let", "in", "if", "then", "else", "fn", "iso", "send", "receive", "spawn", "type", "function", "alloc", "dealloc", "getref", "true", "false", "Tag", "match"]

-- ─────────────────────────────────────────────────────────────────────────────
-- IDENTIFIERS
-- ─────────────────────────────────────────────────────────────────────────────

identChar :: Parser Char
identChar = alphaNumChar <|> char '_' <|> char '\''

lowerIdent :: Parser String
lowerIdent = lexeme . try $ do
  c <- lowerChar
  cs <- many identChar
  let name = c : cs
  if name `elem` keywords then fail (show name ++ " is a keyword") else return name

upperIdent :: Parser String
upperIdent = lexeme $ (:) <$> upperChar <*> many identChar

-- ─────────────────────────────────────────────────────────────────────────────
-- LITERALS
-- ─────────────────────────────────────────────────────────────────────────────

parseInt :: Parser Value
parseInt = VInt <$> lexeme (L.signed sc L.decimal)

parseStr :: Parser Value
parseStr = VStr <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

parseBool :: Parser Value
parseBool = (VBool True <$ kw "true") <|> (VBool False <$ kw "false")

parseUnit :: Parser Value
parseUnit = VUnit <$ lexeme (try (string "()"))

parseListLit :: Parser Value
parseListLit = do
  sym "["
  vs <- sepBy atomicVal (sym ",")
  sym "]"
  return (VList vs)
  where
    atomicVal = parseInt <|> parseStr <|> parseBool <|> parseUnit

parseLit :: Parser Expr
parseLit = Lit <$> choice [parseListLit, parseUnit, parseBool, parseStr, parseInt]

-- ─────────────────────────────────────────────────────────────────────────────
-- PATTERNS
-- ─────────────────────────────────────────────────────────────────────────────

parsePattern :: Parser Pattern
parsePattern =
  choice [PWild <$ lexeme (try (string "_" <* notFollowedBy identChar)), PLit (VBool True) <$ kw "true", PLit (VBool False) <$ kw "false", PLit VUnit <$ lexeme (try (string "()")), PLit . VInt <$> lexeme (L.signed sc L.decimal), tagPat, PVar <$> lowerIdent]
  where
    tagPat = do
      t <- upperIdent
      args <- option [] (sym "(" *> sepBy1 parsePattern (sym ",") <* sym ")")
      return (PTagged t args)

-- ─────────────────────────────────────────────────────────────────────────────
-- EXPRESSIONS
-- ─────────────────────────────────────────────────────────────────────────────

parseExpr :: Parser Expr
parseExpr = choice [parseLet, try parseIsoDecl, parseIf, parseLam, parseSend, parseReceive, parseMatch, parseSpawn, parseAtom]

-- let x = rhs              open — body filled in by chainLets
-- let x = rhs in body      closed
parseLet :: Parser Expr
parseLet = do
  kw "let"
  name <- lowerIdent
  symN "="
  val <- parseExpr
  body <- optional (kw "in" *> scn *> parseExpr)
  return $ case body of
    Just b -> Let name val b
    Nothing -> Let name val (Seq [])

-- if cond then t else f
-- `then` and `else` may be on next line
parseIf :: Parser Expr
parseIf = do
  kw "if"
  cond <- parseExpr
  scn *> kwN "then"
  t <- parseExpr
  scn *> kwN "else"
  f <- parseExpr
  return (If cond t f)

-- fn(x, y) => body
parseLam :: Parser Expr
parseLam = do
  kw "fn"
  params <- sym "(" *> sepBy lowerIdent (sym ",") <* sym ")"
  symN "=>"
  body <- parseExpr
  return (Lam params body)

-- send targetAtom msgExpr
parseSend :: Parser Expr
parseSend = do
  kw "send"
  target <- parseAtom
  msg <- parseExpr
  return (Send (Lit VUnit) target msg)

-- match(expr) { Pat => body | Pat => body }
-- Clauses are separated by '|', newlines, or commas.
parseMatch :: Parser Expr
parseMatch = do
  kw "match"
  scrutinee <- sym "(" *> parseExpr <* sym ")"
  sym "{"
  scn
  clauses <- sepEndBy1 clause sep
  scn *> sym "}"
  return (Match scrutinee clauses)
  where
    clause = do
      optional (void (char '|') *> sc)  -- allow leading | after a newline sep
      pat <- parsePattern
      symN "=>"
      body <- parseExpr
      return (pat, body)
    sep = sc *> (void (char '|') <|> void (char ',') <|> void (char '\n')) *> scn

-- receive { Pat => expr, ... }   or   { Pat => expr \n Pat => expr }
parseReceive :: Parser Expr
parseReceive = do
  kw "receive"
  sym "{"
  scn
  clauses <- sepEndBy1 clause sep
  scn *> sym "}"
  return (Receive clauses)
  where
    clause = do
      pat <- parsePattern
      symN "=>"
      body <- parseExpr
      return (pat, body)
    sep = sc *> (void (char ',') <|> void (char '\n')) *> scn

-- spawn "hint" fnExpr  or  spawn "hint" fnExpr(args)
parseSpawn :: Parser Expr
parseSpawn = do
  kw "spawn"
  hint <- lexeme (char '"' *> manyTill L.charLiteral (char '"'))
  fn <- parseExpr
  args <- option [] (sym "(" *> sepBy parseExpr (sym ",") <* sym ")")
  return (Spawn hint fn args)

-- iso A B fwdExpr bkwdExpr
parseIsoDecl :: Parser Expr
parseIsoDecl = do
  kw "iso"
  notFollowedBy (char '?')
  a <- upperIdent
  b <- upperIdent
  fwd <- parseExpr
  scn
  bkwd <- parseExpr
  return (IsoDecl a b fwd bkwd)

-- ─────────────────────────────────────────────────────────────────────────────
-- ATOMS   (self-delimiting, safe as function arguments)
-- After the base atom is parsed, chainCalls handles any trailing (args)
-- so that expressions like f(x)(y) or tagPayload(b)(v) work correctly.
-- ─────────────────────────────────────────────────────────────────────────────

parseAtom :: Parser Expr
parseAtom = chainCalls parseAtomBase

parseAtomBase :: Parser Expr
parseAtomBase = choice [parseLit, parseBlock, TypeOf <$> (kw "type" *> sym "(" *> parseExpr <* sym ")"), FnOf <$> (kw "function" *> sym "(" *> parseExpr <* sym ")"), Alloc <$> (kw "alloc" *> sym "(" *> parseExpr <* sym ")"), Dealloc <$> (kw "dealloc" *> sym "(" *> parseExpr <* sym ")"), GetRef <$> (kw "getref" *> sym "(" *> parseExpr <* sym ")"), parseLookupIso, parseTagExpr, parseVarOrApp, parseParens]

-- { stmt \n stmt \n stmt }  or  { stmt; stmt; stmt }
-- chainLets is applied so that `let` bindings are visible to later
-- statements in the same block, just as at the top-level program.
parseBlock :: Parser Expr
parseBlock = do
  sym "{"
  scn
  exprs <- sepEndBy1 parseExpr sep
  scn *> sym "}"
  return (chainLets exprs)
  where
    sep = sc *> (void (char ';') <|> void (char '\n')) *> scn

-- iso?(A, B)
parseLookupIso :: Parser Expr
parseLookupIso = do
  void $ lexeme (string "iso?")
  a <- sym "(" *> upperIdent
  b <- sym "," *> upperIdent <* sym ")"
  return (LookupIso a b)

-- Tag Ctor  or  Tag Ctor(e, ...)
parseTagExpr :: Parser Expr
parseTagExpr = do
  kw "Tag"
  tag <- upperIdent
  args <- option [] (sym "(" *> sepBy1 parseExpr (sym ",") <* sym ")")
  return (Tag tag args)

-- name or name(e, ...)  — bare variable; call-chains are handled by
-- chainCalls in parseAtom.
parseVarOrApp :: Parser Expr
parseVarOrApp = Var <$> lowerIdent

-- (expr)  grouping
parseParens :: Parser Expr
parseParens = sym "(" *> parseExpr <* sym ")"

-- After any atom, repeatedly consume "(args)" to form chained calls:
--   f(x)(y)  →  App (App (Var "f") [x]) [y]
--   tagPayload(b)(5)  →  App (App ...) [5]
-- Each extra application is wrapped in `try` so a bare '(' that belongs
-- to the surrounding grammar is never consumed.
chainCalls :: Parser Expr -> Parser Expr
chainCalls p = do
  base <- p
  chains <- many (try (sym "(" *> scn *> sepBy parseExpr (sym "," *> scn) <* scn <* sym ")"))
  return $ foldl App base chains

-- ─────────────────────────────────────────────────────────────────────────────
-- PROGRAM
-- Sequence of expressions separated by newlines/semicolons.
-- Open `let x = v` bindings are chained: the rest of the program becomes
-- their body, turning flat statement lists into proper nested Let trees.
-- ─────────────────────────────────────────────────────────────────────────────

chainLets :: [Expr] -> Expr
chainLets [] = Seq []
chainLets [e] = e
chainLets (Let x v (Seq []) : rest) = Let x v (chainLets rest)
chainLets (e : rest) = Seq (e : flattenSeq (chainLets rest))
  where
    flattenSeq (Seq es) = es
    flattenSeq e' = [e']

parseProgram :: Parser Expr
parseProgram = do
  scn
  exprs <- sepEndBy1 parseExpr progSep
  eof
  return (chainLets exprs)
  where
    -- After sc-based lexemes we're always sitting AT the newline/semicolon. Consume it plus any surrounding whitespace.
    progSep = sc *> (void (char '\n') <|> void (char ';')) *> scn

parseFile :: String -> String -> Either String Expr
parseFile fname src = case runParser parseProgram fname src of
  Left err -> Left (errorBundlePretty err)
  Right ast -> Right ast
