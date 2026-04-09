{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use isAsciiLower" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use isAsciiUpper" #-}
{-# HLINT ignore "Use isDigit" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-
  Add:

  Expressions. Expressions just get desugared into something like ANF

  if x then y else z

-}

module DirectLang where

import Control.Monad (forM)
import Control.Monad.Combinators.Expr
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- INHERITED Attribute: symbol tables passed down to sub-parsers. envVars: where each variable lives — in a .data label, a float register, or a spilled slot. envFunDefs: function name → (param names, raw body text) for later re-parsing. -- variable lives in .data at this label; load with la/fld. parameter lives in a float register (e.g. "fa0", "fa1"). parameter spilled to stack; offset is s0-relative (e.g. -24 means fld fa0,-24(s0))
data VarSource = VarData Text | VarReg Text | VarFrame Int deriving (Show)

-- envFunDefs: fname → (paramNames, bodyText)
data Env = Env {envVars :: M.Map Text VarSource, envFunDefs :: M.Map Text ([Text], Text)} deriving (Show)

-- Our Parser Stack: ReaderT provides the Env, Parsec handles the Text.
type Parser = ReaderT Env (Parsec MyError Text)

-- SYNTHESIZED Attribute: code emitted for an expression sub-tree. Convention: all expression code leaves its double result in register fa0. aData uses a Map so duplicate constant labels are automatically deduplicated. aInst: instructions for the .text section. aData: label → directive for the .data section
data Attr = Attr {aInstrs :: [Text], aData :: M.Map Text Text} deriving (Show)

-- Custom error type for semantic errors (e.g. unbound variables)
data MyError = UndefinedVar Text deriving (Eq, Ord, Show)

instance ShowErrorComponent MyError where
  showErrorComponent (UndefinedVar name) = "Semantic Error: The variable '" <> T.unpack name <> "' is not in the environment!"

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- fa0 is the primary result register throughout. Binary ops: evaluate left (→ fa0), push to stack, evaluate right (→ fa0), pop left into ft0, then operate: fa0 = ft0 OP fa0.
pushFa0 :: [Text]
pushFa0 = ["  addi sp, sp, -8", "  fsd fa0, 0(sp)"]

popToFt0 :: [Text]
popToFt0 = ["  fld ft0, 0(sp)", "  addi sp, sp, 8"]

binaryOp :: Text -> Attr -> Attr -> Attr
binaryOp instr (Attr cl dl) (Attr cr dr) = Attr (cl ++ pushFa0 ++ cr ++ popToFt0 ++ ["  " <> instr]) (M.union dl dr)

-- addA, subA, mulA, divA :: Attr -> Attr -> Attr
addA = binaryOp "fadd.d fa0, ft0, fa0" -- ft0 + fa0 = left + right

subA = binaryOp "fsub.d fa0, ft0, fa0" -- ft0 - fa0 = left - right

mulA = binaryOp "fmul.d fa0, ft0, fa0"

divA = binaryOp "fdiv.d fa0, ft0, fa0" -- ft0 / fa0 = left / right

-- pow: libm pow(x, y); calling convention fa0 = x, fa1 = y, result in fa0.
powA :: Attr -> Attr -> Attr
powA (Attr cl dl) (Attr cr dr) =
  Attr
    ( cl
        ++ pushFa0 -- save left
        ++ cr -- right → fa0
        ++ ["  fmv.d fa1, fa0"] -- right → fa1 (second arg)
        ++ ["  fld fa0, 0(sp)", "  addi sp, sp, 8"] -- restore left → fa0
        ++ ["  call pow"]
    )
    (M.union dl dr)

-- modA: round both to integers via fcvt, use rem, convert back to double.
modA :: Attr -> Attr -> Attr
modA (Attr cl dl) (Attr cr dr) =
  Attr
    ( cl
        ++ pushFa0 -- save left
        ++ cr -- right → fa0
        ++ ["  fcvt.l.d a1, fa0, rtz"] -- right → a1 (int, round-to-zero)
        ++ ["  fld fa0, 0(sp)", "  addi sp, sp, 8"] -- restore left → fa0
        ++ ["  fcvt.l.d a0, fa0, rtz"] -- left → a0 (int)
        ++ ["  rem a0, a0, a1"] -- integer remainder
        ++ ["  fcvt.d.l fa0, a0"] -- back to double
    )
    (M.union dl dr)

-- Apply a native single-instruction unary op (arg and result in fa0).
applyNative :: Text -> Attr -> Attr
applyNative instr (Attr instrs d) = Attr (instrs ++ ["  " <> instr]) d

-- Apply a libm unary function (arg in fa0, result in fa0).
applyLibm :: Text -> Attr -> Attr
applyLibm fname (Attr instrs d) = Attr (instrs ++ ["  call " <> fname]) d

operatorTable :: [[Operator Parser Attr]]
operatorTable =
  [ [InfixR (powA <$ symbol "^")],
    [InfixL (mulA <$ symbol "*"), InfixL (divA <$ symbol "/"), InfixL (modA <$ symbol "%")],
    [InfixL (addA <$ symbol "+"), InfixL (subA <$ symbol "-")]
  ]

pExpr :: Parser Attr
pExpr = makeExprParser pTerm operatorTable

pTerm :: Parser Attr
pTerm = parens pExpr <|> pFunc <|> pUserFuncCall <|> pNumber <|> pVariable

-- Load a double literal via a .data label. Labels are derived from the value so identical constants share one entry.
constLabel :: Double -> Text
constLabel d = ".Llit_" <> T.map sanitize (T.pack (show d))
  where
    sanitize c
      | (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') = c
      | otherwise = '_'

pNumber :: Parser Attr
pNumber = do
  val <- try (lexeme L.float) <|> ((fromIntegral :: Integer -> Double) <$> lexeme L.decimal)
  let lbl = constLabel val
  return $ Attr ["  la t0, " <> lbl, "  fld fa0, 0(t0)"] (M.singleton lbl (".double " <> T.pack (show val)))

-- Emit code for a user function call with any number of args. Single arg: evaluate → fa0, call (no stack traffic). Multi arg : evaluate each → fa0 and push; then load fa0..fa(n-1) from stack; call.
emitFuncCall :: Text -> [Attr] -> Attr
emitFuncCall fname [argAttr] =
  Attr (aInstrs argAttr ++ ["  call fn_" <> fname]) (aData argAttr)
emitFuncCall fname argAttrs =
  let n = length argAttrs
      pushAll = concatMap (\a -> aInstrs a ++ pushFa0) argAttrs
      -- After pushAll: sp+0 = argN (last), sp+(n-1)*8 = arg1 (first) fa0 gets arg1 (deepest), fa(n-1) gets argN (shallowest)
      loadArgs = ["  fld fa" <> T.pack (show i) <> ", " <> T.pack (show ((n - 1 - i) * 8)) <> "(sp)" | i <- [0 .. n - 1]]
      cleanup = ["  addi sp, sp, " <> T.pack (show (n * 8))]
      allData = foldl M.union M.empty (map aData argAttrs)
   in Attr (pushAll ++ loadArgs ++ cleanup ++ ["  call fn_" <> fname]) allData

-- Built-in and user-defined functions called with parentheses: fname(arg) or fname(e1, e2, ...)
pFunc :: Parser Attr
pFunc = do
  fname <- try $ do
    name <- lexeme (T.pack <$> some letterChar)
    _ <- symbol "("
    return name
  dispatchFuncInParens fname

-- Dispatch for fname(...) calls: builtins are fixed single-arg; user functions use their arity.
dispatchFuncInParens :: Text -> Parser Attr
dispatchFuncInParens fname = case fname of
  "sin" -> pExpr >>= \a -> symbol ")" >> return (applyLibm "sin" a)
  "cos" -> pExpr >>= \a -> symbol ")" >> return (applyLibm "cos" a)
  "tan" -> pExpr >>= \a -> symbol ")" >> return (applyLibm "tan" a)
  "abs" -> pExpr >>= \a -> symbol ")" >> return (applyNative "fabs.d fa0, fa0" a)
  "sqrt" -> pExpr >>= \a -> symbol ")" >> return (applyNative "fsqrt.d fa0, fa0" a)
  "log" -> pExpr >>= \a -> symbol ")" >> return (applyLibm "log" a)
  other -> do
    env <- ask
    case M.lookup other (envFunDefs env) of
      Nothing -> customFailure (UndefinedVar other)
      Just (params, _) -> do
        -- Parse comma-separated args; arity must match definition.
        args <- pExpr `sepBy1` symbol ","
        _ <- symbol ")"
        if length args == length params then return $ emitFuncCall other args else fail $ "Wrong number of arguments for '" ++ T.unpack other ++ "'"

-- User-defined functions called with juxtaposition: fname e1 e2 ... en. Each argument is a pTerm to avoid operator ambiguity.
pUserFuncCall :: Parser Attr
pUserFuncCall = try $ do
  fname <- lexeme $ T.pack <$> some letterChar
  env <- ask
  case M.lookup fname (envFunDefs env) of
    Nothing -> fail "not a user-defined function"
    Just (params, _) -> do
      args <- count (length params) pTerm
      return $ emitFuncCall fname args

pVariable :: Parser Attr
pVariable = do
  varName <- lexeme (T.pack <$> some letterChar)
  env <- ask
  case M.lookup varName (envVars env) of
    Just (VarData lbl) -> return $ Attr ["  la t0, " <> lbl, "  fld fa0, 0(t0)"] M.empty
    -- already in result register or move from param register
    Just (VarReg reg) -> return $ if reg == "fa0" then Attr [] M.empty else Attr ["  fmv.d fa0, " <> reg] M.empty
    Just (VarFrame offset) -> return $ Attr ["  fld fa0, " <> T.pack (show offset) <> "(s0)"] M.empty
    Nothing -> customFailure (UndefinedVar varName)

-- Parse "x = 1.5," → returns (varName, .data label, .double directive)
pBinding :: Parser (Text, Text, Text)
pBinding = do
  name <- lexeme (T.pack <$> some letterChar)
  _ <- symbol "="
  val <- try (lexeme L.float) <|> ((fromIntegral :: Integer -> Double) <$> lexeme L.decimal)
  _ <- symbol ","
  let lbl = "var_" <> name
  return (name, lbl, ".double " <> T.pack (show val))

-- Parse "fn f a b c = <body>," — multi-parameter function definition (1–7 params).
-- Params are collected with manyTill until "=" is consumed; body captured as raw Text.
pFnDef :: Parser (Text, [Text], Text)
pFnDef = do
  _ <- symbol "fn"
  fname <- lexeme (T.pack <$> some letterChar)
  first <- lexeme (T.pack <$> some letterChar) -- require at least one param
  rest <- manyTill (lexeme (T.pack <$> some letterChar)) (symbol "=")
  bodyText <- T.strip <$> takeWhileP (Just "function body") (/= ',')
  _ <- symbol ","
  return (fname, first : rest, bodyText)

-- Temporarily swap the parser's input stream, run a sub-parser to completion,
-- then restore the original stream. This lets us parse a function body (captured
-- as Text) directly inside the Parser monad — no nested `parse` call needed.
withInput :: Text -> Parser a -> Parser a
withInput txt p = do
  saved <- getInput
  setInput txt
  result <- p <* eof
  setInput saved
  return result

-- Top-level parser: emits a complete RISC-V assembly program.
-- Grammar: (fn f p1 p2... = body,)* (x = lit,)* expr
-- Variables → .data double entries; functions → labeled basic blocks; expr → main body.
pProgram :: Parser Text
pProgram = do
  fnDefs <- many (try pFnDef)
  bindings <- many (try pBinding)
  baseEnv <- ask

  -- Build the complete environment once all definitions are known.
  let withFns = foldl (\e (fn, ps, b) -> e {envFunDefs = M.insert fn (ps, b) (envFunDefs e)}) baseEnv fnDefs
      withVars = foldl (\e (name, lbl, _) -> e {envVars = M.insert name (VarData lbl) (envVars e)}) withFns bindings
      fullEnv = withVars

  -- Compile the final expression (the program's result).
  mainAttr <- local (const fullEnv) pExpr

  -- Compile each function body by parsing it directly in the Parser monad.
  -- Frame layout (multi-arg, s0 = entry sp):
  --   s0-8: ra, s0-16: s0, s0-(24+k*8): fa{k} (all params spilled).
  fnResults <- forM fnDefs $ \(fname, params, bodyText) -> do
    let n = length params
        isMultiArg = n > 1
        frameSize = ((16 + n * 8 + 15) `div` 16) * 16
        paramSrcs
          | isMultiArg = [VarFrame (-(24 + k * 8)) | k <- [0 .. n - 1]]
          | otherwise = [VarReg "fa0"]
        fnEnv = fullEnv {envVars = foldl (\m (p, src) -> M.insert p src m) (envVars fullEnv) (zip params paramSrcs)}
    bodyAttr <- withInput bodyText (local (const fnEnv) pExpr)
    let hasCall = any (T.isInfixOf "call") (aInstrs bodyAttr)
        (prologue, epilogue)
          | isMultiArg =
              ( [ "  addi sp, sp, -" <> T.pack (show frameSize),
                  "  sd ra, " <> T.pack (show (frameSize - 8)) <> "(sp)",
                  "  sd s0, " <> T.pack (show (frameSize - 16)) <> "(sp)",
                  "  addi s0, sp, " <> T.pack (show frameSize)
                ]
                  ++ ["  fsd fa" <> T.pack (show k) <> ", " <> T.pack (show (frameSize - 24 - k * 8)) <> "(sp)" | k <- [0 .. n - 1]],
                [ "  ld ra, " <> T.pack (show (frameSize - 8)) <> "(sp)",
                  "  ld s0, " <> T.pack (show (frameSize - 16)) <> "(sp)",
                  "  addi sp, sp, " <> T.pack (show frameSize)
                ]
              )
          | hasCall =
              ( ["  addi sp, sp, -16", "  sd ra, 8(sp)"],
                ["  ld ra, 8(sp)", "  addi sp, sp, 16"]
              )
          | otherwise = ([], [])
        block = ("fn_" <> fname <> ":") : prologue ++ aInstrs bodyAttr ++ epilogue ++ ["  ret"]
    return (block, aData bodyAttr)

  -- Assemble the .data section: builtins + variables + inline constants from expressions.
  let builtins =
        M.fromList
          [ (".Lconst_pi", ".double 3.141592653589793"),
            (".Lconst_e", ".double 2.718281828459045"),
            (".Lfmt", ".asciz \"Result: %.10g\\n\"")
          ]
      varEntries = M.fromList [(lbl, entry) | (_, lbl, entry) <- bindings]
      allData = foldl M.union (M.union builtins varEntries) (aData mainAttr : map snd fnResults)
      dataLines = [lbl <> ": " <> def | (lbl, def) <- M.toAscList allData]

  -- Assemble the .text section: function blocks then main.
  let fnCode = concatMap fst fnResults
      mainBlock =
        [ "main:",
          "  addi sp, sp, -16",
          "  sd ra, 8(sp)"
        ]
          ++ aInstrs mainAttr
          ++
          -- RISC-V variadic ABI: floating-point variadic args go in integer registers, not float registers. Move the double result (fa0) to a1 as raw bits before calling printf.
          [ "  fmv.x.d a1, fa0",
            "  la a0, .Lfmt",
            "  call printf",
            "  ld ra, 8(sp)",
            "  addi sp, sp, 16",
            "  li a0, 0",
            "  ret"
          ]

  let assembly =
        T.unlines $
          [".data"]
            ++ dataLines
            ++ ["", ".text", ".global main", ""]
            ++ fnCode
            ++ (if null fnCode then [] else [""])
            ++ mainBlock

  return assembly

runCodeGen :: Text -> Either String Text
runCodeGen input =
  let baseEnv = Env {envVars = M.fromList [("pi", VarData ".Lconst_pi"), ("e", VarData ".Lconst_e")], envFunDefs = M.empty}
      parser = runReaderT (pProgram <* eof) baseEnv
   in case parse parser "" input of
        Left bundle -> Left (errorBundlePretty bundle)
        Right asm -> Right asm

--- >>> either id T.unpack $ runCodeGen "1 + 2 * 4 + 6 / 2"
--- >>> either id T.unpack $ runCodeGen "fn square x = x * x, square 5"
--- >>> either id T.unpack $ runCodeGen "fn double x = x + x, fn quad x = double(double x), quad 3"
--- >>> either id T.unpack $ runCodeGen "r = 5, pi * r ^ 2"
--- >>> either id T.unpack $ runCodeGen "x = 1, y = 2, z = 3, x + y + sin(z)"
-- ".data\n.Lconst_e: .double 2.718281828459045\n.Lconst_pi: .double 3.141592653589793\n.Lfmt: .asciz \"Result: %.10g\\n\"\n.Llit_1_0: .double 1.0\n.Llit_2_0: .double 2.0\n.Llit_4_0: .double 4.0\n.Llit_6_0: .double 6.0\n\n.text\n.global main\n\nmain:\n  addi sp, sp, -16\n  sd ra, 8(sp)\n  la t0, .Llit_1_0\n  fld fa0, 0(t0)\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  la t0, .Llit_2_0\n  fld fa0, 0(t0)\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  la t0, .Llit_4_0\n  fld fa0, 0(t0)\n  fld ft0, 0(sp)\n  addi sp, sp, 8\n  fmul.d fa0, ft0, fa0\n  fld ft0, 0(sp)\n  addi sp, sp, 8\n  fadd.d fa0, ft0, fa0\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  la t0, .Llit_6_0\n  fld fa0, 0(t0)\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  la t0, .Llit_2_0\n  fld fa0, 0(t0)\n  fld ft0, 0(sp)\n  addi sp, sp, 8\n  fdiv.d fa0, ft0, fa0\n  fld ft0, 0(sp)\n  addi sp, sp, 8\n  fadd.d fa0, ft0, fa0\n  fmv.x.d a1, fa0\n  la a0, .Lfmt\n  call printf\n  ld ra, 8(sp)\n  addi sp, sp, 16\n  li a0, 0\n  ret\n"
-- ".data\n.Lconst_e: .double 2.718281828459045\n.Lconst_pi: .double 3.141592653589793\n.Lfmt: .asciz \"Result: %.10g\\n\"\n.Llit_5_0: .double 5.0\n\n.text\n.global main\n\nfn_square:\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  fld ft0, 0(sp)\n  addi sp, sp, 8\n  fmul.d fa0, ft0, fa0\n  ret\n\nmain:\n  addi sp, sp, -16\n  sd ra, 8(sp)\n  la t0, .Llit_5_0\n  fld fa0, 0(t0)\n  call fn_square\n  fmv.x.d a1, fa0\n  la a0, .Lfmt\n  call printf\n  ld ra, 8(sp)\n  addi sp, sp, 16\n  li a0, 0\n  ret\n"
-- ".data\n.Lconst_e: .double 2.718281828459045\n.Lconst_pi: .double 3.141592653589793\n.Lfmt: .asciz \"Result: %.10g\\n\"\n.Llit_3_0: .double 3.0\n\n.text\n.global main\n\nfn_double:\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  fld ft0, 0(sp)\n  addi sp, sp, 8\n  fadd.d fa0, ft0, fa0\n  ret\nfn_quad:\n  addi sp, sp, -16\n  sd ra, 8(sp)\n  call fn_double\n  call fn_double\n  ld ra, 8(sp)\n  addi sp, sp, 16\n  ret\n\nmain:\n  addi sp, sp, -16\n  sd ra, 8(sp)\n  la t0, .Llit_3_0\n  fld fa0, 0(t0)\n  call fn_quad\n  fmv.x.d a1, fa0\n  la a0, .Lfmt\n  call printf\n  ld ra, 8(sp)\n  addi sp, sp, 16\n  li a0, 0\n  ret\n"
-- ".data\n.Lconst_e: .double 2.718281828459045\n.Lconst_pi: .double 3.141592653589793\n.Lfmt: .asciz \"Result: %.10g\\n\"\n.Llit_2_0: .double 2.0\nvar_r: .double 5.0\n\n.text\n.global main\n\nmain:\n  addi sp, sp, -16\n  sd ra, 8(sp)\n  la t0, .Lconst_pi\n  fld fa0, 0(t0)\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  la t0, var_r\n  fld fa0, 0(t0)\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  la t0, .Llit_2_0\n  fld fa0, 0(t0)\n  fmv.d fa1, fa0\n  fld fa0, 0(sp)\n  addi sp, sp, 8\n  call pow\n  fld ft0, 0(sp)\n  addi sp, sp, 8\n  fmul.d fa0, ft0, fa0\n  fmv.x.d a1, fa0\n  la a0, .Lfmt\n  call printf\n  ld ra, 8(sp)\n  addi sp, sp, 16\n  li a0, 0\n  ret\n"
-- ".data\n.Lconst_e: .double 2.718281828459045\n.Lconst_pi: .double 3.141592653589793\n.Lfmt: .asciz \"Result: %.10g\\n\"\nvar_x: .double 1.0\nvar_y: .double 2.0\nvar_z: .double 3.0\n\n.text\n.global main\n\nmain:\n  addi sp, sp, -16\n  sd ra, 8(sp)\n  la t0, var_x\n  fld fa0, 0(t0)\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  la t0, var_y\n  fld fa0, 0(t0)\n  fld ft0, 0(sp)\n  addi sp, sp, 8\n  fadd.d fa0, ft0, fa0\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  la t0, var_z\n  fld fa0, 0(t0)\n  call sin\n  fld ft0, 0(sp)\n  addi sp, sp, 8\n  fadd.d fa0, ft0, fa0\n  fmv.x.d a1, fa0\n  la a0, .Lfmt\n  call printf\n  ld ra, 8(sp)\n  addi sp, sp, 16\n  li a0, 0\n  ret\n"

--- >>> either id T.unpack $ runCodeGen "fn add a b = a + b, add 3 5"
--- >>> either id T.unpack $ runCodeGen "fn fma a b c = a * b + c, fma 2 3 4"
--- >>> either id T.unpack $ runCodeGen "fn dot a b c = a * b + b * c + a * c, dot 2 3 4"
-- ".data\n.Lconst_e: .double 2.718281828459045\n.Lconst_pi: .double 3.141592653589793\n.Lfmt: .asciz \"Result: %.10g\\n\"\n.Llit_3_0: .double 3.0\n.Llit_5_0: .double 5.0\n\n.text\n.global main\n\nfn_add:\n  addi sp, sp, -32\n  sd ra, 24(sp)\n  sd s0, 16(sp)\n  addi s0, sp, 32\n  fsd fa0, 8(sp)\n  fsd fa1, 0(sp)\n  fld fa0, -24(s0)\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  fld fa0, -32(s0)\n  fld ft0, 0(sp)\n  addi sp, sp, 8\n  fadd.d fa0, ft0, fa0\n  ld ra, 24(sp)\n  ld s0, 16(sp)\n  addi sp, sp, 32\n  ret\n\nmain:\n  addi sp, sp, -16\n  sd ra, 8(sp)\n  la t0, .Llit_3_0\n  fld fa0, 0(t0)\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  la t0, .Llit_5_0\n  fld fa0, 0(t0)\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  fld fa0, 8(sp)\n  fld fa1, 0(sp)\n  addi sp, sp, 16\n  call fn_add\n  fmv.x.d a1, fa0\n  la a0, .Lfmt\n  call printf\n  ld ra, 8(sp)\n  addi sp, sp, 16\n  li a0, 0\n  ret\n"
-- ".data\n.Lconst_e: .double 2.718281828459045\n.Lconst_pi: .double 3.141592653589793\n.Lfmt: .asciz \"Result: %.10g\\n\"\n.Llit_2_0: .double 2.0\n.Llit_3_0: .double 3.0\n.Llit_4_0: .double 4.0\n\n.text\n.global main\n\nfn_fma:\n  addi sp, sp, -48\n  sd ra, 40(sp)\n  sd s0, 32(sp)\n  addi s0, sp, 48\n  fsd fa0, 24(sp)\n  fsd fa1, 16(sp)\n  fsd fa2, 8(sp)\n  fld fa0, -24(s0)\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  fld fa0, -32(s0)\n  fld ft0, 0(sp)\n  addi sp, sp, 8\n  fmul.d fa0, ft0, fa0\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  fld fa0, -40(s0)\n  fld ft0, 0(sp)\n  addi sp, sp, 8\n  fadd.d fa0, ft0, fa0\n  ld ra, 40(sp)\n  ld s0, 32(sp)\n  addi sp, sp, 48\n  ret\n\nmain:\n  addi sp, sp, -16\n  sd ra, 8(sp)\n  la t0, .Llit_2_0\n  fld fa0, 0(t0)\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  la t0, .Llit_3_0\n  fld fa0, 0(t0)\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  la t0, .Llit_4_0\n  fld fa0, 0(t0)\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  fld fa0, 16(sp)\n  fld fa1, 8(sp)\n  fld fa2, 0(sp)\n  addi sp, sp, 24\n  call fn_fma\n  fmv.x.d a1, fa0\n  la a0, .Lfmt\n  call printf\n  ld ra, 8(sp)\n  addi sp, sp, 16\n  li a0, 0\n  ret\n"
-- ".data\n.Lconst_e: .double 2.718281828459045\n.Lconst_pi: .double 3.141592653589793\n.Lfmt: .asciz \"Result: %.10g\\n\"\n.Llit_2_0: .double 2.0\n.Llit_3_0: .double 3.0\n.Llit_4_0: .double 4.0\n\n.text\n.global main\n\nfn_dot:\n  addi sp, sp, -48\n  sd ra, 40(sp)\n  sd s0, 32(sp)\n  addi s0, sp, 48\n  fsd fa0, 24(sp)\n  fsd fa1, 16(sp)\n  fsd fa2, 8(sp)\n  fld fa0, -24(s0)\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  fld fa0, -32(s0)\n  fld ft0, 0(sp)\n  addi sp, sp, 8\n  fmul.d fa0, ft0, fa0\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  fld fa0, -32(s0)\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  fld fa0, -40(s0)\n  fld ft0, 0(sp)\n  addi sp, sp, 8\n  fmul.d fa0, ft0, fa0\n  fld ft0, 0(sp)\n  addi sp, sp, 8\n  fadd.d fa0, ft0, fa0\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  fld fa0, -24(s0)\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  fld fa0, -40(s0)\n  fld ft0, 0(sp)\n  addi sp, sp, 8\n  fmul.d fa0, ft0, fa0\n  fld ft0, 0(sp)\n  addi sp, sp, 8\n  fadd.d fa0, ft0, fa0\n  ld ra, 40(sp)\n  ld s0, 32(sp)\n  addi sp, sp, 48\n  ret\n\nmain:\n  addi sp, sp, -16\n  sd ra, 8(sp)\n  la t0, .Llit_2_0\n  fld fa0, 0(t0)\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  la t0, .Llit_3_0\n  fld fa0, 0(t0)\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  la t0, .Llit_4_0\n  fld fa0, 0(t0)\n  addi sp, sp, -8\n  fsd fa0, 0(sp)\n  fld fa0, 16(sp)\n  fld fa1, 8(sp)\n  fld fa2, 0(sp)\n  addi sp, sp, 24\n  call fn_dot\n  fmv.x.d a1, fa0\n  la a0, .Lfmt\n  call printf\n  ld ra, 8(sp)\n  addi sp, sp, 16\n  li a0, 0\n  ret\n"

-- USE spike /opt/homebrew/opt/riscv-pk/riscv64-unknown-elf/bin/pk to test
