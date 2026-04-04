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
{-# HLINT ignore "Use :" #-}

module RVLang where

import Control.Monad.Combinators.Expr
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- VarFloatParam n: float parameter at position n, lives in fa<n> (fa0, fa1, ...)
-- VarFnParam: function-pointer parameter passed in a1; saved into s1 at function entry
data VarSource = VarData Text | VarFloatParam Int | VarFnParam deriving (Eq, Show)

-- TOP DOWN env. envFunDefs maps function name -> float arity (fn-ptr params not counted)
data Env = Env {envVars :: M.Map Text VarSource, envFunDefs :: M.Map Text Int} deriving (Show)

data MyError = UndefinedVar Text deriving (Eq, Ord, Show)

instance ShowErrorComponent MyError where
  showErrorComponent (UndefinedVar name) = "Semantic Error: The variable '" <> T.unpack name <> "' is not in the environment!"

-- BOTTOM UP, tells us how many instructions and what kinds in subtree
data Attr = Attr {aInstrs :: [Text], aData :: M.Map Text Text} deriving (Show)

-- StateT Int carries a counter for generating unique branch labels in a single pass
type Parser = ReaderT Env (StateT Int (Parsec MyError Text))

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

-- WHEN SEE A POW OP, CREATE SYNTHESIZED ATTRIBUTE based on left and right operands x ^ n. x in fa0, n in fa1
-- pushFa0 saves x; cr evaluates n into fa0; fmv moves n to fa1; then pop x into fa0; call pow.
powA (Attr cl dl) (Attr cr dr) = Attr (cl ++ pushFa0 ++ cr ++ ["  fmv.d fa1, fa0", "  fld fa0, 0(sp)", "  addi sp, sp, 8", "  call pow"]) (M.union dl dr)

modA (Attr cl dl) (Attr cr dr) = Attr (cl ++ pushFa0 ++ cr ++ ["  fcvt.l.d a1, fa0, rtz"] ++ ["  fld fa0, 0(sp)", "  addi sp, sp, 8"] ++ ["  fcvt.l.d a0, fa0, rtz"] ++ ["  rem a0, a0, a1"] ++ ["  fcvt.d.l fa0, a0"]) (M.union dl dr)

-- USER DEFINED FUNCTIONS
applyNative instr (Attr instrs d) = Attr (instrs ++ ["  " <> instr]) d

-- LIBRARY BUILTIN FUNCTIONS
applyLibm fname (Attr instrs d) = Attr (instrs ++ ["  call" <> " " <> fname]) d

-- USE <$ to simply replace the parsed symbol "*" char with the mulA function. A fmap style function on the terms on the left and right which are guarenteed to be Attr
operatorTable = [[InfixR $ powA <$ symbol "^"], [InfixL $ mulA <$ symbol "*", InfixL $ divA <$ symbol "/", InfixL $ modA <$ symbol "%"], [InfixL $ addA <$ symbol "+", InfixL $ subA <$ symbol "-"]]

pExpr = makeExprParser pTerm operatorTable

-- pTerm tries if/then/else first, then parens, stdlib, fix builtin, user calls, literals, variable refs
pTerm = pIfExpr <|> parens pExpr <|> pFunc <|> pFix <|> pUserFuncCall <|> pNumber <|> pVariable

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
  dispatchFunc fname arg

-- Allocate a fresh label counter value for if/then/else branch labels
freshLabel :: Parser Int
freshLabel = do
  n <- lift get
  lift $ put (n + 1)
  return n

-- if <cond> then <e1> else <e2>
-- Zero-check: cond == 0.0 means false (jump to else). Non-zero means true (fall through).
-- feq.d produces 1 when equal, 0 when not equal; beqz branches when feq result == 0 (i.e. not-equal-to-zero, so cond != 0 → take then branch). Wait:
-- We want: if cond is non-zero → then branch. if cond is zero → else branch.
-- feq.d t0, fa0, ft1  (ft1 = 0.0): t0=1 iff fa0==0, t0=0 iff fa0!=0
-- bnez t0, elseLabel  → jump to else when t0=1, i.e. when fa0==0 (false)
-- So: bnez t0, elseLabel gives: "if fa0==0 then goto else"
pIfExpr :: Parser Attr
pIfExpr = do
  try $ symbol "if"
  condAttr <- pTerm
  symbol "then"
  thenAttr <- pExpr
  symbol "else"
  elseAttr <- pExpr
  n <- freshLabel
  let ns = T.pack (show n)
      elseLabel = ".Lcond_else_" <> ns
      endLabel = ".Lcond_end_" <> ns
      zeroLabel = ".Llit_0_0"
      zeroData = M.singleton zeroLabel ".double 0.0"
      -- evaluate cond; feq with 0; if equal (cond==0) jump to else
      condCode = aInstrs condAttr ++ ["  la t0, " <> zeroLabel, "  fld ft1, 0(t0)", "  feq.d t0, fa0, ft1", "  bnez t0, " <> elseLabel]
      thenCode = aInstrs thenAttr ++ ["  j " <> endLabel]
      elseCode = [elseLabel <> ":"] ++ aInstrs elseAttr
      endCode = [endLabel <> ":"]
      allInstrs = condCode ++ thenCode ++ elseCode ++ endCode
      allData = M.unions [zeroData, aData condAttr, aData thenAttr, aData elseAttr]
  return $ Attr allInstrs allData

-- fix <fname> : self-recursive fixpoint.
-- Loads the address of fn_fname into a1 (the fn-ptr arg register), then calls fn_fname.
-- fa0 must already contain the first float argument (set by the caller before fix).
pFix :: Parser Attr
pFix = do
  try $ symbol "fix"
  fname <- lexeme $ T.pack <$> some letterChar
  env <- ask
  case M.lookup fname (envFunDefs env) of
    Nothing -> customFailure $ UndefinedVar fname
    Just _ -> return $ Attr ["  la a1, fn_" <> fname, "  call fn_" <> fname] M.empty

-- User-defined function call with juxtaposed arguments.
-- Collects exactly `arity` float argument terms.
pUserFuncCall = try $ do
  fname <- lexeme $ T.pack <$> some letterChar
  env <- ask
  case M.lookup fname (envFunDefs env) of
    Nothing -> fail "not a user defined fn"
    Just arity -> do
      args <- mapM (\_ -> pTerm) [1 .. arity]
      return $ emitMultiCall fname args

-- Emit a multi-argument call using fa0..fa(n-1) for arguments.
-- Strategy: evaluate each arg into fa0 and push; then pop in reverse order into fa0..fa(n-1).
emitMultiCall :: Text -> [Attr] -> Attr
emitMultiCall fname [] = Attr ["  call fn_" <> fname] M.empty
emitMultiCall fname args =
  let pushAll = concatMap (\a -> aInstrs a ++ pushFa0) args
      n = length args
      -- pop in reverse so deepest (first arg) ends up in fa0
      popInto i = ["  fld fa" <> T.pack (show i) <> ", 0(sp)", "  addi sp, sp, 8"]
      popAll = concatMap popInto (reverse [0 .. n - 1])
      callInstr = ["  call fn_" <> fname]
      allData = M.unions (map aData args)
   in Attr (pushAll ++ popAll ++ callInstr) allData

dispatchFunc :: Text -> Attr -> Parser Attr
dispatchFunc "sin" arg = return $ applyLibm "sin" arg
dispatchFunc "cos" arg = return $ applyLibm "cos" arg
dispatchFunc "tan" arg = return $ applyLibm "tan" arg
dispatchFunc "log" arg = return $ applyLibm "log" arg
dispatchFunc "abs" arg = return $ applyNative "fabs.d fa0, fa0" arg
dispatchFunc "sqrt" arg = return $ applyNative "fsqrt.d fa0, fa0" arg
dispatchFunc other arg = do
  env <- ask
  case M.lookup other (envFunDefs env) of
    Just _ -> return $ emitMultiCall other [arg]
    Nothing -> case M.lookup other (envVars env) of
      Just VarFnParam -> return $ applyNative "jalr ra, s1, 0" arg
      _ -> customFailure $ UndefinedVar other

-- VARIABLE usage
pVariable = do
  varName <- lexeme $ T.pack <$> some letterChar
  env <- ask
  case M.lookup varName (envVars env) of
    Just (VarData lbl) -> return $ Attr ["  la t0, " <> lbl, "  fld fa0, 0(t0)"] M.empty
    -- Float param 0 is already in fa0 (no-op); param n moves from fa<n> to fa0
    Just (VarFloatParam 0) -> return $ Attr [] M.empty
    Just (VarFloatParam n) -> return $ Attr ["  fmv.d fa0, fa" <> T.pack (show n)] M.empty
    -- VarFnParam: address is in s1; call it with current fa0 as argument
    Just VarFnParam -> return $ Attr ["  jalr ra, s1, 0"] M.empty
    Nothing -> customFailure (UndefinedVar varName)

pBinding :: Parser (Text, Text, Text)
pBinding = do
  name <- lexeme $ T.pack <$> some letterChar
  symbol "="
  val <- (try $ lexeme L.float) <|> (i2d <$> lexeme L.decimal)
  symbol ","
  let lbl = "var_" <> name
  return (name, lbl, ".double " <> T.pack (show val))

-- Parse function parameters, supporting '#' prefix for fn-ptr params.
-- e.g. `fn myfunc x #f = ...` makes x a VarFloatParam 0 and f a VarFnParam.
-- Returns tagged [(paramName, VarSource)] and the float arity.
pParams :: Parser ([(Text, VarSource)], Int)
pParams = do
  rawParams <- many $ try $ do
    isFn <- option False (True <$ symbol "#")
    name <- lexeme $ T.pack <$> some letterChar
    return (name, isFn)
  let assign _ [] = []
      assign floatIdx ((name, True) : rest) = (name, VarFnParam) : assign floatIdx rest
      assign floatIdx ((name, False) : rest) = (name, VarFloatParam floatIdx) : assign (floatIdx + 1) rest
      tagged = assign 0 rawParams
      floatAr = length (filter (\(_, vs) -> case vs of VarFloatParam _ -> True; _ -> False) tagged)
  return (tagged, floatAr)

-- Parse and compile a function definition (single pass), returning (name, arity, codeBlock, dataMap)
pFnDef :: Parser (Text, Int, [Text], M.Map Text Text)
pFnDef = do
  symbol "fn"
  fname <- lexeme $ T.pack <$> some letterChar
  (params, arity) <- pParams
  symbol "="

  env <- ask
  let paramMap = M.fromList params
      fnEnv = env {envVars = M.union paramMap (envVars env)}
  bodyAttr <- local (const fnEnv) pExpr

  symbol ","

  let usesFnPtr = any (\(_, vs) -> vs == VarFnParam) params
      hasCall = any (T.isInfixOf "call") (aInstrs bodyAttr) || any (T.isInfixOf "jalr") (aInstrs bodyAttr)
      needsFrame = hasCall || usesFnPtr
      -- 16-byte frame: slot 8(sp) = ra (saved when hasCall), slot 0(sp) = s1 (saved when usesFnPtr)
      prologue = if needsFrame then ["  addi sp, sp, -16"] ++ (if hasCall then ["  sd ra, 8(sp)"] else []) ++ (if usesFnPtr then ["  sd s1, 0(sp)"] else []) else []
      -- Save a1 into s1 right after prologue
      fnPtrInit = if usesFnPtr then ["  mv s1, a1"] else []
      epilogue = if needsFrame then (if usesFnPtr then ["  ld s1, 0(sp)"] else []) ++ (if hasCall then ["  ld ra, 8(sp)"] else []) ++ ["  addi sp, sp, 16"] else []
      block = ("fn_" <> fname <> ":") : prologue ++ fnPtrInit ++ aInstrs bodyAttr ++ epilogue ++ ["  ret"]

  return (fname, arity, block, aData bodyAttr)

pProgram = do
  baseEnv <- ask

  -- Parse function definitions one by one, accumulating names+arities in env
  let parseFns env = do
        mFn <- optional (try pFnDef)
        case mFn of
          Nothing -> return []
          Just (fname, arity, block, dataMap) -> do
            let env' = env {envFunDefs = M.insert fname arity (envFunDefs env)}
            rest <- local (const env') (parseFns env')
            return ((fname, arity, block, dataMap) : rest)

  fnResults <- parseFns baseEnv

  -- VAR BINDINGS
  bindings <- many (try pBinding)

  -- Build environment with all functions and variables known
  let fnMap = M.fromList [(fname, ar) | (fname, ar, _, _) <- fnResults]
      withFns = baseEnv {envFunDefs = fnMap}
      withVars = foldl (\e (name, lbl, _) -> e {envVars = M.insert name (VarData lbl) (envVars e)}) withFns bindings

  mainAttr <- local (const withVars) pExpr

  -- Assemble .data and .text sections
  let builtins =
        M.fromList
          [ (".Lconst_pi", ".double 3.141592653589793"),
            (".Lconst_e", ".double 2.718281828459045"),
            (".Lfmt", ".asciz \"Result: %.10g\\n\""),
            (".Llit_0_0", ".double 0.0")
          ]
      varEntries = M.fromList [(lbl, entry) | (_, lbl, entry) <- bindings]
      fnData = [dataMap | (_, _, _, dataMap) <- fnResults]
      allData = foldl M.union (M.union builtins varEntries) (aData mainAttr : fnData)
      dataLines = [lbl <> ": " <> def | (lbl, def) <- M.toAscList allData]
      fnCode = concatMap (\(_, _, block, _) -> block) fnResults
      mainBlock =
        ["main:", "  addi sp, sp, -16", "  sd ra, 8(sp)"]
          ++ aInstrs mainAttr
          ++ ["  fmv.x.d a1, fa0", "  la a0, .Lfmt", "  call printf", "  ld ra, 8(sp)", "  addi sp, sp, 16", "  li a0, 0", "  ret"]
      assembly =
        T.unlines $
          [".data"]
            ++ dataLines
            ++ ["", ".text", ".global main", ""]
            ++ fnCode
            ++ (if null fnCode then [] else [""])
            ++ mainBlock

  return assembly

runCodeGen input =
  let baseEnv = Env {envVars = M.fromList [("pi", VarData ".Lconst_pi"), ("e", VarData ".Lconst_e")], envFunDefs = M.empty}
      parser = runReaderT (pProgram <* eof) baseEnv
      stParser = runStateT parser 0
   in case parse stParser "" input of
        Left bundle -> Left $ errorBundlePretty bundle
        Right (asm, _) -> Right asm

--- >>> either id T.unpack $ runCodeGen "1 + 2 * 4 + 6 / 2"

--- >>> either id T.unpack $ runCodeGen "fn f x = x * 2, x = 2, y = 4444, x * y"

--- >>> either id T.unpack $ runCodeGen "fn inc x = x + 1, fn add a b = a + b, add 3 4"

--- >>> either id T.unpack $ runCodeGen "if 1 then 42 else 99"

--- >>> either id T.unpack $ runCodeGen "fn myfunc x #f = if x then x else x + f (x - 1), fn myrecfunc x = fix myfunc, myrecfunc 5"
