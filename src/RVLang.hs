{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use if" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use isAsciiLower" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use isAsciiUpper" #-}
{-# HLINT ignore "Use isDigit" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-
  NEW FEATURES NEED(ED):
    - simple conditions. no nested exprs. basically select/3 with true = 0 and function calls in bodies
    - pluggable FFI
-}

module RVLang where

import Control.Monad.Combinators.Expr
import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data VarSource = VarData Text | VarParam deriving (Show)

-- TOP DOWN, propagate env for searching up vars and funcs. envFunDefs now just tracks which functions exist (no body storage needed)
data Env = Env {envVars :: M.Map Text VarSource, envFunDefs :: S.Set Text} deriving (Show)

data MyError = UndefinedVar Text deriving (Eq, Ord, Show)

instance ShowErrorComponent MyError where
  showErrorComponent (UndefinedVar name) = "Semantic Error: The variable '" <> T.unpack name <> "' is not in the environment!"

-- BOTTOM UP, tells us how many instructions and what kinds in subtree
data Attr = Attr {aInstrs :: [Text], aData :: M.Map Text Text} deriving (Show)

type Parser = ReaderT Env (Parsec MyError Text)

sc :: Parser ()
sc = L.space space1 empty empty

lexeme = L.lexeme sc

symbol = L.symbol sc

parens = between (symbol "(") (symbol ")")

-- STORE a0 to stack. Store the 64-bit double
pushFa0 :: [Text]
pushFa0 = ["  addi sp, sp, -8", "  fsd fa0, 0(sp)"]

-- pop a0 to t0
popToFt0 :: [Text]
popToFt0 = ["  fld ft0, 0(sp)", "  addi sp, sp, 8"]

binaryOp instr (Attr cl dl) (Attr cr dr) = Attr (cl ++ pushFa0 ++ cr ++ popToFt0 ++ ["  " <> instr]) (M.union dl dr)

-- a0 = t0 + a0
addA = binaryOp "fadd.d fa0, ft0, fa0"

subA = binaryOp "fsub.d fa0, ft0, fa0"

mulA = binaryOp "fmul.d fa0, ft0, fa0"

divA = binaryOp "fdiv.d fa0, ft0, fa0"

-- WHEN SEE A POW OP, CREATE SYNTHESIZED ATTRIBUTE based on left and right operands x ^ n. x in fa1, n in fa0
powA (Attr cl dl) (Attr cr dr) = Attr (cl ++ pushFa0 ++ cr ++ ["  fmv.d fa1, fa0"] ++ ["  fld fa0, 0(sp)"] ++ ["  call pow"]) (M.union dl dr)

modA (Attr cl dl) (Attr cr dr) = Attr (cl ++ pushFa0 ++ cr ++ ["  fcvt.l.d a1, fa0, rtz"] ++ ["  fld fa0, 0(sp)", "  addi sp, sp, 8"] ++ ["  fcvt.l.d a0, fa0, rtz"] ++ ["  rem a0, a0, a1"] ++ ["  fcvt.d.l fa0, a0"]) (M.union dl dr)

-- USER DEFINED FUNCTIONS
applyNative instr (Attr instrs d) = Attr (instrs ++ ["  " <> instr]) d

-- LIBRARY BUILTIN FUNCTIONS
applyLibm fname (Attr instrs d) = Attr (instrs ++ ["  call" <> " " <> fname]) d

-- USE <$ to simply replace the parsed symbol "*" char with the mulA function. A fmap style function on the terms on the left and right which are guarenteed to be Attr
operatorTable = [[InfixR $ powA <$ symbol "^"], [InfixL $ mulA <$ symbol "*", InfixL $ divA <$ symbol "/", InfixL $ modA <$ symbol "%"], [InfixL $ addA <$ symbol "+", InfixL $ subA <$ symbol "-"]]

pExpr = makeExprParser pTerm operatorTable

pTerm = parens pExpr <|> pFunc <|> pUserFuncCall <|> pNumber <|> pVariable

sanitize c
  | (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') = c
  | otherwise = '_'

-- create a local label of the form .L(literal constant) for constants so dont duplicate
constLabel :: Double -> Text
constLabel d = ".Llit_" <> T.map sanitize (T.pack (show d))

i2d :: Integer -> Double
i2d = fromIntegral

pNumber = do
  val <- (try $ lexeme L.float) <|> (i2d <$> lexeme L.decimal)
  let lbl = constLabel val

  return $ Attr ["  la t0, " <> lbl, "  fld fa0, 0(t0)"] $ M.singleton lbl $ ".double " <> T.pack (show val)

-- PARSING A FUNCTION call such as sin(x)
pFunc = do
  fname <- try $ do
    name <- lexeme $ T.pack <$> some letterChar
    symbol "("
    return name
  arg <- pExpr
  symbol ")"

  -- MAGIC HAPPENS HERE, generate the right function
  dispatchFunc fname arg

-- NOT GOING TO do push here
pUserFuncCall = try $ do
  fname <- lexeme $ T.pack <$> some letterChar
  env <- ask

  case S.member fname $ envFunDefs env of
    True -> do
      arg <- pTerm
      return $ emitCall fname arg
    False -> fail "not a user defined fn"

dispatchFunc :: Text -> Attr -> Parser Attr
dispatchFunc "sin" arg = return $ applyLibm "sin" arg
dispatchFunc "cos" arg = return $ applyLibm "cos" arg
dispatchFunc "tan" arg = return $ applyLibm "tan" arg
dispatchFunc "log" arg = return $ applyLibm "log" arg
-- MOST FUNCTIONS NEED TO STORE THING INTO a0 or ra. Thing is always in a0 and ra due to essentially a kind of currying
dispatchFunc "abs" arg = return $ applyNative "fabs.d fa0, fa0" arg
dispatchFunc "sqrt" arg = return $ applyNative "fsqrt.d fa0, fa0" arg
dispatchFunc other arg = do
  env <- ask

  case S.member other $ envFunDefs env of
    True -> return $ emitCall other arg
    False -> customFailure $ UndefinedVar other

emitCall fname argAttr = Attr (aInstrs argAttr ++ ["  call fn_" <> fname]) (aData argAttr)

-- WHEN WE ENCOUNTER VARIABLE USAGE like x = y for y, and y + 1
pVariable = do
  -- all this is doing is mostly alphanum+
  varName <- lexeme $ T.pack <$> some letterChar
  env <- ask

  case M.lookup varName (envVars env) of
    -- MOST VARIABLE USES synthesize instructions to load. VARIABLES are technically always just loaded in tandem. Typically only around 7 things can be in regs before call, 7 primitives.
    Just (VarData lbl) -> return $ Attr ["  la t0, " <> lbl, "  fld fa0, 0(t0)"] M.empty
    -- IF JUST A PARAM like f x = x + 1, should not typically be 'used directly but incorporated into FnCall
    Just VarParam -> return $ Attr [] M.empty
    Nothing -> customFailure (UndefinedVar varName)

pBinding :: Parser (Text, Text, Text)
pBinding = do
  name <- lexeme $ T.pack <$> some letterChar
  symbol "="
  -- NOTICE ALREADY MAKING SEMANTIC ACTIONS BY READILY PARSING A DECIMAL AND CONVERTING IT INTO PROGRAM DECIMAL
  val <- (try $ lexeme L.float) <|> (i2d <$> lexeme L.decimal)
  symbol ","
  let lbl = "var_" <> name

  return (name, lbl, ".double" <> T.pack (show val))

-- Parse and compile a function definition immediately (single pass)
pFnDef :: Parser (Text, [Text], M.Map Text Text)
pFnDef = do
  symbol "fn"
  fname <- lexeme $ T.pack <$> some letterChar
  param <- lexeme $ T.pack <$> some letterChar
  symbol "="

  -- Compile body immediately with parameter in scope
  env <- ask
  let fnEnv = env {envVars = M.insert param VarParam (envVars env)}
  bodyAttr <- local (const fnEnv) pExpr

  symbol ","

  -- Generate function block
  let hasCall = any (T.isInfixOf "call") (aInstrs bodyAttr)
      prologue = if hasCall then ["  addi sp, sp, -16", "  sd ra, 8(sp)"] else []
      epilogue = if hasCall then ["  ld ra, 8(sp)", "  addi sp, sp, 16"] else []
      block = ("fn_" <> fname <> ":") : prologue ++ aInstrs bodyAttr ++ epilogue ++ ["  ret"]

  return (fname, block, aData bodyAttr)

pProgram = do
  baseEnv <- ask

  -- Parse function definitions one by one, accumulating names
  let parseFns env = do
        mFn <- optional (try pFnDef)
        case mFn of
          Nothing -> return []
          Just (fname, block, dataMap) -> do
            let env' = env {envFunDefs = S.insert fname (envFunDefs env)}
            rest <- local (const env') (parseFns env')
            return ((fname, block, dataMap) : rest)

  fnResults <- parseFns baseEnv

  -- THEN VAR BINDINGS
  bindings <- many (try pBinding)

  -- Build environment with all functions and variables
  let fnNames = S.fromList [fname | (fname, _, _) <- fnResults]
      withFns = baseEnv {envFunDefs = S.union fnNames (envFunDefs baseEnv)}
      withVars = foldl (\e (name, lbl, _) -> e {envVars = M.insert name (VarData lbl) (envVars e)}) withFns bindings

  -- FINAL EXPRESSION COMPILE
  mainAttr <- local (const withVars) pExpr

  -- CREATE REGISTRY FOR .data
  let builtins = M.fromList [(".Lconst_pi", ".double 3.141592653589793"), (".Lconst_e", ".double 2.718281828459045"), (".Lfmt", ".asciz \"Result: %.10g\\n\"")]
      varEntries = M.fromList [(lbl, entry) | (_, lbl, entry) <- bindings]
      fnData = [dataMap | (_, _, dataMap) <- fnResults]
      allData = foldl M.union (M.union builtins varEntries) (aData mainAttr : fnData)
      dataLines = [lbl <> ": " <> def | (lbl, def) <- M.toAscList allData]

  -- CREATE REGISTRY FOR .code
  let fnCode = concatMap (\(_, block, _) -> block) fnResults
      mainBlock = ["main:", "  addi sp, sp, -16", "  sd ra, 8(sp)"] ++ aInstrs mainAttr ++ ["  fmv.x.d a1, fa0", "  la a0, .Lfmt", "  call printf", "  ld ra, 8(sp)", "  addi sp, sp, 16", "  li a0, 0", "  ret"]

  -- T.unlines does most of the work with new lines
  let assembly = T.unlines $ [".data"] ++ dataLines ++ ["", ".text", ".global main", ""] ++ fnCode ++ (if null fnCode then [] else [""]) ++ mainBlock

  return assembly

runCodeGen input =
  let baseEnv = Env {envVars = M.fromList [("pi", VarData ".Lconst_pi"), ("e", VarData ".Lconst_e")], envFunDefs = S.empty}
      parser = runReaderT (pProgram <* eof) baseEnv
   in case parse parser "" input of
        Left bundle -> Left $ errorBundlePretty bundle
        Right asm -> Right asm

--- >>> either id T.unpack $ runCodeGen "1 + 2 * 4 + 6 / 2"

--- >>> either id T.unpack $ runCodeGen "fn f x = x * 2, x = 2, y = 4444, x * y"
