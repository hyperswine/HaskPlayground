{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use elemIndex" #-}

-- FPL v1 Compiler
-- Single-pass: ReaderT Env (StateT CompState (Parsec ...))
--
-- Features:
--   * Immutable modules (Sig / Impl, access via dot)
--   * Tagged unions with if-chain desugaring
--   * Pattern matching -> if/else + field projections
--   * SoA VList (struct-of-arrays for records, SIMD-friendly layout)
--   * Isomorphisms: iso TypeName <-> VList FieldType
--   * Actors (spawn / send / self) with RC-style message passing
--   * fix as a tail-jump (warns if not in tail position)
--   * Higher-order via function pointers (no closures)
--   * Pure strict expression language
--   * UNIX interface: open/close/read/write/seek as runtime calls
--   * Codegen to RISC-V RV64GV (integer + vector extension for SIMD)

module FPL1 where

import Control.Monad.Reader
import Control.Monad.State
import Data.Char (isUpper)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------
-- Source types
--------------------------------------------------------------------------------

-- A field in a record/union variant
data Field = Field {fieldName :: Text, fieldType :: FPType} deriving (Show, Eq)

-- Types in FP-RISC v1
data FPType
  = TInt -- i64
  | TFloat -- f64
  | TBool -- i64 0/1
  | TVList FPType -- VList T  (flat array)
  | TRecord Text -- named record type
  | TUnion Text -- named tagged union
  | TFnPtr -- function pointer (no closures)
  | TActor -- actor reference
  | TUnit -- ()
  deriving (Show, Eq)

-- A variant of a tagged union
data Variant = Variant
  { variantName :: Text,
    variantFields :: [Field] -- may be empty
  }
  deriving (Show, Eq)

-- An isomorphism declaration
data IsoDecl = IsoDecl
  { isoTypeName :: Text, -- e.g. "Vec3"
    isoVListType :: FPType, -- e.g. TVList TFloat
    isoToVList :: Text, -- function name: TypeName -> VList FieldType
    isoFromVList :: Text -- function name: VList FieldType -> TypeName
  }
  deriving (Show, Eq)

-- Top-level declarations
data Decl
  = DRecord Text [Field] -- record typename fields
  | DUnion Text [Variant] -- union  typename variants
  | DIso IsoDecl -- iso declaration
  | DFn Text [(Text, ParamKind)] FPType Expr -- fn name params rettype body
  | DModule Text [Decl] -- module name { decls }
  | DLet Text FPType Expr -- top-level let binding
  deriving (Show)

data ParamKind
  = PInt
  | PFloat
  | PBool
  | PFnPtr
  | PActor
  | PLet  -- let-bound value: stored in callee-saved reg s1..s8 (idx selects which)
  deriving (Show, Eq)

-- Expressions
data Expr
  = EInt Integer
  | EFloat Double
  | EBool Bool
  | EVar Text
  | EAccess Expr Text -- e.field  / module.fn
  | EApp Expr [Expr] -- f args...
  | EBinOp Text Expr Expr
  | EUnOp Text Expr
  | EIf Expr Expr Expr
  | ECase Expr [(Pat, Expr)] -- case e of alts
  | EFix Text [Expr] -- fix fname args  (tail-jump)
  | ELet Text Expr Expr -- let x = e1 in e2
  | ESpawn Text [Expr] -- spawn actor_fn args
  | ESend Expr Expr -- send actor_ref msg
  | ESelf -- self (current actor ref)
  | EVList [Expr] -- VList literal [e1, e2, ...]
  | EIndex Expr Expr -- vlist[i]
  | EOpen Expr -- open path
  | ERead Expr Expr -- read fd n
  | EWrite Expr Expr -- write fd buf
  | EClose Expr -- close fd
  | ESeek Expr Expr Text -- seek fd offset whence
  | EUnit
  deriving (Show)

data Pat
  = PVariant Text [Text] -- VariantName field_bindings...
  | PWild -- _
  | PVar Text -- x  (bind whole value)
  deriving (Show)

--------------------------------------------------------------------------------
-- Compiler environment and state
--------------------------------------------------------------------------------

data TypeDef
  = TDRecord [Field]
  | TDUnion [Variant]
  deriving (Show)

data FnSig = FnSig
  { fnParams :: [(Text, ParamKind)],
    fnRetType :: FPType
  }
  deriving (Show)

data Env = Env
  { envTypes :: M.Map Text TypeDef, -- type name -> definition
    envFns :: M.Map Text FnSig, -- fn name   -> signature
    envVars :: M.Map Text (Int, ParamKind), -- var name -> (param index, kind)
    envLetTypes :: M.Map Text FPType, -- let-bound var name -> its type
    envModules :: M.Map Text (M.Map Text FnSig), -- module -> exported fns
    envIsos :: M.Map Text IsoDecl, -- type name -> iso
    envCurrentFn :: Maybe Text -- for fix validation
  }
  deriving (Show)

emptyEnv :: Env
emptyEnv = Env M.empty M.empty M.empty M.empty M.empty M.empty Nothing

data CompState = CompState
  { csLabel :: Int, -- unique label counter
    csWarnings :: [Text], -- non-fatal warnings
    csData :: M.Map Text Text, -- .data section entries
    csLetDepth :: Int -- current nesting depth of live let-bindings (selects s1..s8)
  }
  deriving (Show)

emptyState :: CompState
emptyState = CompState 0 [] M.empty 0

type Parser = ReaderT Env (StateT CompState (Parsec Void Text))

-- Code is just lines of RISC-V assembly
type Code = [Text]

--------------------------------------------------------------------------------
-- Codegen attribute (bottom-up synthesised)
--------------------------------------------------------------------------------

data Attr = Attr
  { atCode :: Code, -- instructions
    atReg :: Text, -- register holding result (a0, fa0, etc.)
    atType :: FPType -- inferred type
  }
  deriving (Show)

simpleAttr :: Code -> Text -> FPType -> Attr
simpleAttr c r t = Attr c r t

--------------------------------------------------------------------------------
-- Parser utilities
--------------------------------------------------------------------------------

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

keyword :: Text -> Parser Text
keyword w = lexeme (string w <* notFollowedBy alphaNumChar)

identifier :: Parser Text
identifier = lexeme $ do
  c <- letterChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_' <|> char '\'')
  let name = T.pack (c : cs)
  if name `elem` reservedWords then fail ("reserved: " ++ T.unpack name) else return name

upperIdent :: Parser Text
upperIdent = lexeme $ do
  c <- upperChar
  cs <- many (alphaNumChar <|> char '_')
  return $ T.pack (c : cs)

reservedWords :: [Text]
reservedWords =
  [ "fn",
    "let",
    "in",
    "if",
    "then",
    "else",
    "case",
    "of",
    "fix",
    "spawn",
    "send",
    "self",
    "module",
    "sig",
    "impl",
    "record",
    "union",
    "iso",
    "VList",
    "open",
    "read",
    "write",
    "close",
    "seek",
    "true",
    "false",
    "type",
    "print"
  ]

freshLabel :: Parser Int
freshLabel = do
  n <- gets csLabel
  modify (\s -> s {csLabel = n + 1})
  return n

addData :: Text -> Text -> Parser ()
addData lbl def = modify (\s -> s {csData = M.insert lbl def (csData s)})

warn :: Text -> Parser ()
warn w = modify (\s -> s {csWarnings = csWarnings s ++ [w]})

--------------------------------------------------------------------------------
-- Type parser
--------------------------------------------------------------------------------

pType :: Parser FPType
pType =
  (keyword "Int" >> return TInt)
    <|> (keyword "Float" >> return TFloat)
    <|> (keyword "Bool" >> return TBool)
    <|> (keyword "Actor" >> return TActor)
    <|> (keyword "FnPtr" >> return TFnPtr)
    <|> (keyword "Unit" >> return TUnit)
    <|> (TVList <$> (keyword "VList" >> pType))
    <|> (TRecord <$> upperIdent)

--------------------------------------------------------------------------------
-- Expression parser
--------------------------------------------------------------------------------

pExpr :: Parser Attr
pExpr = pLetExpr <|> pIfExpr <|> pCaseExpr <|> pBinOp

pLetExpr :: Parser Attr
pLetExpr = do
  keyword "let"
  name <- identifier
  symbol "="
  rhs <- pExpr
  keyword "in"
  depth <- gets csLetDepth
  if depth >= 8 then warn ("too many nested lets (>8); " <> name <> " may alias s8") else return ()
  modify (\s -> s {csLetDepth = depth + 1})
  let letReg = "s" <> T.pack (show (depth + 1))  -- s1..s8
  -- Extend env: let-bound variable looks up depth in PLet logic of pVarOrAccess
  body <- local (\e -> e { envVars = M.insert name (depth, PLet) (envVars e)
                          , envLetTypes = M.insert name (atType rhs) (envLetTypes e)
                          }) pExpr
  modify (\s -> s {csLetDepth = depth})
  let code =
        atCode rhs
          ++ ["  mv   " <> letReg <> ", a0  # let " <> name]
          ++ atCode body
  return $ Attr code (atReg body) (atType body)

pIfExpr :: Parser Attr
pIfExpr = do
  keyword "if"
  cond <- pExpr
  keyword "then"
  thn <- pExpr
  keyword "else"
  els <- pExpr
  n <- freshLabel
  let ns = T.pack (show n)
      elseL = ".Lelse_" <> ns
      endL = ".Lend_" <> ns
      condCode =
        atCode cond
          ++ ["  beqz a0, " <> elseL] -- branch if false (0)
      thenCode =
        atCode thn
          ++ ["  j " <> endL]
      elseCode = [elseL <> ":"] ++ atCode els
      endCode = [endL <> ":"]
  -- result type from then branch
  return $ Attr (condCode ++ thenCode ++ elseCode ++ endCode) "a0" (atType thn)

-- case expr of { VariantName fields -> body | ... }
-- Desugars to if/else chain on tag comparisons
pCaseExpr :: Parser Attr
pCaseExpr = do
  keyword "case"
  scrut <- pExpr
  keyword "of"
  alts <- some pAlt
  env <- ask
  n <- freshLabel
  let ns = T.pack (show n)
      endL = ".Lcase_end_" <> ns

  -- Emit: evaluate scrutinee into a0, then chain of tag checks
  -- Tag is at offset 0, fields follow
  -- For each alt: compare tag, if match bind fields and eval body
  altCodes <- mapM (emitAlt ns scrut env) (zip [0 ..] alts)
  let code =
        atCode scrut
          ++ concat altCodes
          ++ [endL <> ":"]
  return $ Attr code "a0" TInt -- type would be inferred properly in a full impl

pAlt :: Parser (Pat, Expr -> Parser Attr)
pAlt = do
  symbol "|"
  pat <- pPat
  symbol "->"
  -- We need to parse the body in an extended env.
  -- We defer: return the pat and a function that takes scrut attr.
  bodyStr <- pExprRaw
  return (pat, \_ -> return bodyStr)

-- Simplified: parse expression for alt body
pExprRaw :: Parser Attr
pExprRaw = pBinOp

pPat :: Parser Pat
pPat =
  (PWild <$ symbol "_")
    <|> ( do
            name <- upperIdent
            fields <- many identifier
            return $ PVariant name fields
        )
    <|> (PVar <$> identifier)

emitAlt :: Text -> Attr -> Env -> (Int, (Pat, Expr -> Parser Attr)) -> Parser Code
emitAlt ns _scrut _env (i, (pat, mkBody)) = do
  let nextL = ".Lalt_" <> ns <> "_" <> T.pack (show (i + 1))
  body <- mkBody (EUnit)
  case pat of
    PWild -> return $ atCode body
    PVar _ -> return $ atCode body
    PVariant vname _fields ->
      -- tag is at 0(a0) — compare with variant index
      -- We'd look up variant index in env; simplified: use hash of name
      let tagVal = variantTag vname
       in return $
            [ "  # case alt: " <> vname,
              "  lw t0, 0(a0)", -- load tag field
              "  li t1, " <> T.pack (show tagVal),
              "  bne t0, t1, " <> nextL -- skip if tag doesn't match
            ]
              ++ atCode body
              ++ ["  j .Lcase_end_" <> ns]
              ++ [nextL <> ":"]

-- Simple stable tag assignment: index in sorted order (full impl uses union def)
variantTag :: Text -> Int
variantTag name = fromMaybe 0 $ lookup name $ zip knownVariants [0 ..]
  where
    knownVariants = ["None", "Some", "Ok", "Err", "Nil", "Cons", "True", "False"]

--------------------------------------------------------------------------------
-- Binary / unary operators
--------------------------------------------------------------------------------

pBinOp :: Parser Attr
pBinOp = do
  lhs <- pUnary
  rest lhs
  where
    rest lhs = do
      mop <- optional $ choice $ map (\op -> (op,) <$> symbol op) ["==", "!=", "<=", ">=", "<", ">", "&&", "||", "+", "-", "*", "/", "%"]
      case mop of
        Nothing -> return lhs
        Just (op, _) -> do
          rhs <- pUnary
          attr <- emitBinOp op lhs rhs
          rest attr

pUnary :: Parser Attr
pUnary =
  ( do
      symbol "!"
      a <- pApp
      return $ Attr (atCode a ++ ["  seqz a0, a0"]) "a0" TBool
  )
    <|> ( do
            symbol "-"
            a <- pApp
            return $ Attr (atCode a ++ ["  neg a0, a0"]) "a0" (atType a)
        )
    <|> pApp

pApp :: Parser Attr
pApp = do
  fn <- pAtom
  args <- many (try pAtom)
  case args of
    [] -> return fn
    _ -> emitCall fn args

pAtom :: Parser Attr
pAtom =
  parens pExpr
    <|> pVListLit
    <|> pFixExpr
    <|> pSpawnExpr
    <|> pSendExpr
    <|> (ESelf <$ keyword "self" >>= \_ -> return (Attr ["  mv a0, s10  # self actor ref"] "a0" TActor))
    <|> pUnixOp
    <|> pPrintExpr
    <|> pBoolLit
    <|> pNumLit
    <|> pVarOrAccess

pBoolLit :: Parser Attr
pBoolLit =
  (keyword "true" >> return (Attr ["  li a0, 1"] "a0" TBool))
    <|> (keyword "false" >> return (Attr ["  li a0, 0"] "a0" TBool))

pNumLit :: Parser Attr
pNumLit = do
  val <- try (Left <$> lexeme L.float) <|> (Right <$> lexeme L.decimal)
  case val of
    Left d -> do
      let lbl = ".Lf_" <> T.map sanitizeC (T.pack (show d))
      addData lbl (".double " <> T.pack (show d))
      return $ Attr ["  la t0, " <> lbl, "  fld fa0, 0(t0)"] "fa0" TFloat
    Right i ->
      return $ Attr ["  li a0, " <> T.pack (show i)] "a0" TInt

sanitizeC :: Char -> Char
sanitizeC c
  | c `elem` (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']) = c
  | otherwise = '_'

pVarOrAccess :: Parser Attr
pVarOrAccess = do
  name <- identifier
  -- check for dot-access (module.fn or record.field)
  -- Only uppercase-starting names can be module refs; also wrap in try so a
  -- failed identifier after '.' doesn't consume the '.' function terminator.
  mDot <-
    if not (T.null name) && isUpper (T.head name)
      then optional (try (symbol "." >> identifier))
      else return Nothing
  env <- ask
  case mDot of
    Just field -> do
      -- module access: emit call to module_field
      let qualName = name <> "_" <> field
      return $ Attr ["  call fn_" <> qualName] "a0" TInt
    Nothing ->
      case M.lookup name (envVars env) of
        Just (idx, PLet) ->
          -- let-binding stored in callee-saved reg s{idx+1}; preserve original type
          let letReg  = "s" <> T.pack (show (idx + 1))
              letType = fromMaybe TInt (M.lookup name (envLetTypes env))
           in return $ Attr ["  mv   a0, " <> letReg] "a0" letType
        Just (idx, kind) -> return $ emitParamLoad name idx kind
        Nothing ->
          case M.lookup name (envFns env) of
            -- 0-param functions: emit a direct call rather than loading the fn ptr.
            -- This lets them be used as expressions (thunks) without explicit ().
            Just sig | null (fnParams sig) ->
              return $ Attr ["  call fn_" <> name] "a0" (fnRetType sig)
            Just _ -> return $ Attr ["  la a0, fn_" <> name] "a0" TFnPtr
            Nothing -> return $ Attr ["  # unresolved: " <> name, "  li a0, 0"] "a0" TInt

emitParamLoad :: Text -> Int -> ParamKind -> Attr
-- Always load from the stable s0-relative frame slot so that
-- prior expression evaluation (which clobbers a0) does not corrupt param access.
emitParamLoad _name idx kind = case kind of
  PFloat ->
    Attr ["  fld  fa0, " <> T.pack (show (idx * 8)) <> "(s0)"] "fa0" TFloat
  _ ->
    Attr ["  ld   a0, " <> T.pack (show (idx * 8)) <> "(s0)"] "a0" TInt

-- VList literal: [e1, e2, e3]
-- Emits heap allocation for n elements, then stores each
pVListLit :: Parser Attr
pVListLit = do
  elems <- brackets (pExpr `sepBy` symbol ",")
  let elemCount = length elems
      -- Allocate count*8 bytes via a simple bump-allocator runtime call
      allocCode =
        [ "  li a0, " <> T.pack (show (elemCount * 8)),
          "  call fpr_alloc   # VList alloc",
          "  mv s2, a0        # base ptr"
        ]
      storeElem (i, elemAttr) =
        atCode elemAttr
          ++ ["  sd a0, " <> T.pack (show (i * 8)) <> "(s2)  # VList[" <> T.pack (show i) <> "]"]
      storeAll = concatMap storeElem (zip [(0 :: Int) ..] elems)
      finalise = ["  mv a0, s2"]
  return $ Attr (allocCode ++ storeAll ++ finalise) "a0" (TVList TInt)

-- fix fname arg1 arg2 ...  — tail jump
pFixExpr :: Parser Attr
pFixExpr = do
  keyword "fix"
  fname <- identifier
  args <- many (try pAtom)
  env <- ask
  -- Warn if fix is not the outermost expression (we can't easily check here,
  -- so we emit a comment; real impl would check in a post-pass)
  case envCurrentFn env of
    Just cur | cur == fname -> return ()
    _ -> warn $ "fix " <> fname <> ": ensure this is in tail position"
  -- Load updated params into a0..aN, then jump to the entry label (after frame
  -- setup) so the function re-saves params to its frame without re-adjusting sp.
  let numArgs  = length args
      pushArg a = atCode a ++ ["  addi sp, sp, -8", "  sd   a0, 0(sp)"]
      popArg i  = "  ld   a" <> T.pack (show (i :: Int)) <> ", " <> T.pack (show ((numArgs - 1 - i) * 8)) <> "(sp)"
      pushCode  = concatMap pushArg args
      popCode   = map popArg [0 .. numArgs - 1]
      cleanup   = ["  addi sp, sp, " <> T.pack (show (numArgs * 8))]
      jumpCode  = ["  j fn_" <> fname <> "_entry  # fix tail-jump"]
  return $ Attr (pushCode ++ popCode ++ cleanup ++ jumpCode) "a0" TUnit

-- spawn actor_fn arg1 arg2
pSpawnExpr :: Parser Attr
pSpawnExpr = do
  keyword "spawn"
  fname <- identifier
  args <- many (try pAtom)
  let setupArgs = concatMap (\(i, a) -> atCode a ++ ["  mv a" <> T.pack (show (i :: Int)) <> ", a0"]) (zip [0 ..] args)
      spawnCode =
        [ "  la a0, fn_" <> fname, -- actor entry fn
          "  call fpr_spawn          # -> actor ref in a0"
        ]
  return $ Attr (setupArgs ++ spawnCode) "a0" TActor

-- send actor_ref msg_expr
pSendExpr :: Parser Attr
pSendExpr = do
  keyword "send"
  ref <- pAtom
  msg <- pAtom
  let code =
        atCode ref
          ++ ["  mv a2, a0          # actor ref"]
          ++ atCode msg
          ++ ["  mv a1, a0          # message"]
          ++ [ "  mv a0, a2",
               "  call fpr_send       # send msg to actor"
             ]
  return $ Attr code "a0" TUnit

-- UNIX interface: open/read/write/close/seek
pUnixOp :: Parser Attr
pUnixOp =
  ( do
      keyword "open"
      path <- pAtom
      return $ Attr (atCode path ++ ["  call fpr_open"]) "a0" TInt
  )
    <|> ( do
            keyword "read"
            fd <- pAtom
            n <- pAtom
            return $ Attr (atCode fd ++ ["  mv a0, a0"] ++ atCode n ++ ["  call fpr_read"]) "a0" (TVList TInt)
        )
    <|> ( do
            keyword "write"
            fd <- pAtom
            buf <- pAtom
            return $ Attr (atCode fd ++ atCode buf ++ ["  call fpr_write"]) "a0" TInt
        )
    <|> ( do
            keyword "close"
            fd <- pAtom
            return $ Attr (atCode fd ++ ["  call fpr_close"]) "a0" TInt
        )
    <|> ( do
            keyword "seek"
            fd <- pAtom
            off <- pAtom
            wh <- identifier
            let whCode = case wh of
                  "set" -> "  li a2, 0"
                  "cur" -> "  li a2, 1"
                  "end" -> "  li a2, 2"
                  _ -> "  li a2, 0"
            return $ Attr (atCode fd ++ atCode off ++ [whCode, "  call fpr_seek"]) "a0" TInt
        )

--------------------------------------------------------------------------------
-- print / println
--------------------------------------------------------------------------------

pPrintExpr :: Parser Attr
pPrintExpr = do
  keyword "print"
  arg <- pAtom
  env <- ask
  n <- freshLabel
  let ns = T.pack (show n)
  code <- emitPrint ns (atCode arg) (atType arg) env
  return $ Attr code "a0" TUnit

emitPrint :: Text -> Code -> FPType -> Env -> Parser Code
emitPrint _ argCode TInt _ =
  return $
    argCode
      ++ [ "  mv   a1, a0"
         , "  la   a0, fpr_fmt_int"
         , "  call printf"
         ]
emitPrint _ argCode TFloat _ =
  return $
    argCode
      ++ [ "  la   a0, fpr_fmt_float"
         , "  call printf"
         ]
emitPrint ns argCode TBool _ =
  return $
    argCode
      ++ [ "  beqz a0, .Lpbf_" <> ns
         , "  la   a0, fpr_fmt_true"
         , "  call printf"
         , "  j    .Lpbe_" <> ns
         , ".Lpbf_" <> ns <> ":"
         , "  la   a0, fpr_fmt_false"
         , "  call printf"
         , ".Lpbe_" <> ns <> ":"
         ]
emitPrint _ argCode TUnit _ =
  return $
    argCode
      ++ [ "  la   a0, fpr_fmt_unit"
         , "  call printf"
         ]
emitPrint ns argCode (TUnion tyname) env =
  case M.lookup tyname (envTypes env) of
    Just (TDUnion variants) -> do
      let ptrReg = "s9"  -- callee-saved; printf will not clobber it
          endL   = ".Lpe_" <> ns
          emitVariant (i, Variant vname fields) = do
            let nextL    = ".Lpv_" <> ns <> "_" <> T.pack (show (i + 1 :: Int))
                fmtL     = ".Lfmt_pv_" <> ns <> "_" <> T.pack (show (i :: Int))
                nameStr  = vname <> if null fields then "\\n" else "("
                tagCheck =
                  [ "  lw   t0, 0(" <> ptrReg <> ")  # union tag"
                  , "  li   t1, " <> T.pack (show i)
                  , "  bne  t0, t1, " <> nextL
                  ]
                printName = ["  la   a0, " <> fmtL, "  call printf"]
                printFlds = concatMap (emitFieldPrint ptrReg) (zip [0 ..] fields)
                closeParen =
                  if null fields
                    then []
                    else ["  la   a0, fpr_fmt_closeparen", "  call printf"]
                done = ["  j    " <> endL, nextL <> ":"]
            addData fmtL (".string \"" <> nameStr <> "\"")
            return $ tagCheck ++ printName ++ printFlds ++ closeParen ++ done
      variantCodes <- mapM emitVariant (zip [0 ..] variants)
      let lastNext  = ".Lpv_" <> ns <> "_" <> T.pack (show (length variants))
          unknownFall =
            [ lastNext <> ":"
            , "  la   a0, fpr_fmt_unknown"
            , "  call printf"
            ]
      return $ argCode ++ ["  mv   " <> ptrReg <> ", a0"] ++ concat variantCodes ++ unknownFall ++ [endL <> ":"]
    _ ->
      return $
        argCode
          ++ [ "  mv   a1, a0"
             , "  la   a0, fpr_fmt_int"
             , "  call printf"
             ]
emitPrint _ argCode _ _ =
  return $
    argCode
      ++ [ "  mv   a1, a0"
         , "  la   a0, fpr_fmt_int"
         , "  call printf"
         ]

emitFieldPrint :: Text -> (Int, Field) -> Code
emitFieldPrint ptrReg (i, Field _fname fty) =
  let offset = 8 + i * 8  -- tag at byte 0; fields from byte 8
      sep
        | i == 0    = []
        | otherwise = ["  la   a0, fpr_fmt_comma", "  call printf"]
      body = case fty of
        TFloat ->
          [ "  fld  fa0, " <> T.pack (show offset) <> "(" <> ptrReg <> ")"
          , "  la   a0, fpr_fmt_float"
          , "  call printf"
          ]
        _ ->
          [ "  ld   a1, " <> T.pack (show offset) <> "(" <> ptrReg <> ")"
          , "  la   a0, fpr_fmt_intfld"
          , "  call printf"
          ]
   in sep ++ body

--------------------------------------------------------------------------------
-- Binary op codegen
--------------------------------------------------------------------------------

emitBinOp :: Text -> Attr -> Attr -> Parser Attr
emitBinOp op lhs rhs = do
  -- Evaluate lhs first, push, evaluate rhs, pop lhs into t0, combine
  let isFloat = atType lhs == TFloat || atType rhs == TFloat
  n <- freshLabel
  let _ns = T.pack (show n)
  if isFloat
    then do
      let pushCode = atCode lhs ++ ["  addi sp, sp, -8", "  fsd fa0, 0(sp)"]
          popCode = ["  fld ft0, 0(sp)", "  addi sp, sp, 8"]
          instr = floatOp op
          code = pushCode ++ atCode rhs ++ popCode ++ ["  " <> instr]
      return $ Attr code "fa0" TFloat
    else do
      let pushCode = atCode lhs ++ ["  addi sp, sp, -8", "  sd a0, 0(sp)"]
          popCode = ["  ld t0, 0(sp)", "  addi sp, sp, 8"]
          (instr, rtype) = intOp op
          code = pushCode ++ atCode rhs ++ popCode ++ ["  " <> instr]
      return $ Attr code "a0" rtype

floatOp :: Text -> Text
floatOp "+" = "fadd.d fa0, ft0, fa0"
floatOp "-" = "fsub.d fa0, ft0, fa0"
floatOp "*" = "fmul.d fa0, ft0, fa0"
floatOp "/" = "fdiv.d fa0, ft0, fa0"
floatOp _ = "fadd.d fa0, ft0, fa0"

intOp :: Text -> (Text, FPType)
intOp "+" = ("add  a0, t0, a0", TInt)
intOp "-" = ("sub  a0, t0, a0", TInt)
intOp "*" = ("mul  a0, t0, a0", TInt)
intOp "/" = ("div  a0, t0, a0", TInt)
intOp "%" = ("rem  a0, t0, a0", TInt)
intOp "==" = ("sub  t1, t0, a0\n  seqz a0, t1", TBool)
intOp "!=" = ("sub  t1, t0, a0\n  snez a0, t1", TBool)
intOp "<" = ("slt  a0, t0, a0", TBool)
intOp "<=" = ("sgt  t1, t0, a0\n  xori a0, t1, 1", TBool)
intOp ">" = ("sgt  a0, t0, a0", TBool)
intOp ">=" = ("slt  t1, t0, a0\n  xori a0, t1, 1", TBool)
intOp "&&" = ("and  a0, t0, a0", TBool)
intOp "||" = ("or   a0, t0, a0", TBool)
intOp _ = ("add  a0, t0, a0", TInt)

--------------------------------------------------------------------------------
-- Function calls
--------------------------------------------------------------------------------

emitCall :: Attr -> [Attr] -> Parser Attr
emitCall fn args = do
  -- Evaluate each arg into a0/fa0, push to stack.  Pop in order into a0..aN.
  -- This avoids register clobbering when arg N overwrites a0 before arg 0 is saved.
  let numArgs = length args
      pushArg a =
        atCode a
          ++ [ if atType a == TFloat
                 then "  addi sp, sp, -8\n  fsd  fa0, 0(sp)"
                 else "  addi sp, sp, -8\n  sd   a0, 0(sp)"
             ]
      -- After pushing numArgs items, arg i is at (numArgs-1-i)*8(sp)
      popArg (i, a)
        | atType a == TFloat =
            "  fld  fa" <> T.pack (show (i :: Int)) <> ", " <> T.pack (show ((numArgs - 1 - i) * 8)) <> "(sp)"
        | otherwise =
            "  ld   a" <> T.pack (show (i :: Int)) <> ", " <> T.pack (show ((numArgs - 1 - i) * 8)) <> "(sp)"
      pushCode  = concatMap pushArg args
      popCode   = map popArg (zip [0 ..] args)
      cleanupSp = ["  addi sp, sp, " <> T.pack (show (numArgs * 8))]
      argSetup  = pushCode ++ popCode ++ cleanupSp
  let callCode =
        case atCode fn of
          -- Direct named call: fn_XXX is statically known
          [t] | "  la a0, fn_" `T.isPrefixOf` t ->
                let target = T.drop (T.length "  la a0, ") t
                 in argSetup ++ ["  call " <> target]
          -- Indirect fn-ptr call: address computed into a0
          _ -> atCode fn ++ ["  mv t1, a0"] ++ argSetup ++ ["  jalr ra, t1, 0  # fn-ptr call"]
  return $ Attr callCode "a0" TInt

--------------------------------------------------------------------------------
-- Declaration parsers
--------------------------------------------------------------------------------

pDecl :: Parser (Env -> Env, Code, M.Map Text Text)
pDecl = pRecordDecl <|> pUnionDecl <|> pIsoDecl <|> pFnDecl <|> pModuleDecl

-- record Vec3 { x: Float, y: Float, z: Float }
pRecordDecl :: Parser (Env -> Env, Code, M.Map Text Text)
pRecordDecl = do
  keyword "record"
  name <- upperIdent
  fields <- braces (pField `sepBy` symbol ",")
  -- Emit struct layout comment; actual memory layout is just sequential 8-byte slots
  let comment =
        [ "  # record "
            <> name
            <> ": "
            <> T.intercalate ", " (map (\f -> fieldName f <> ":" <> showType (fieldType f)) fields)
        ]
      extend e = e {envTypes = M.insert name (TDRecord fields) (envTypes e)}
  return (extend, comment, M.empty)

pField :: Parser Field
pField = do
  name <- identifier
  symbol ":"
  ty <- pType
  return $ Field name ty

showType :: FPType -> Text
showType TInt = "Int"
showType TFloat = "Float"
showType TBool = "Bool"
showType TActor = "Actor"
showType TFnPtr = "FnPtr"
showType TUnit = "Unit"
showType (TVList t) = "VList " <> showType t
showType (TRecord n) = n
showType (TUnion n) = n

-- union Option { None | Some(value: Int) }
pUnionDecl :: Parser (Env -> Env, Code, M.Map Text Text)
pUnionDecl = do
  keyword "union"
  name <- upperIdent
  variants <- braces (pVariant `sepBy` symbol "|")
  let extend e = e {envTypes = M.insert name (TDUnion variants) (envTypes e)}
      comment =
        [ "  # union "
            <> name
            <> ": "
            <> T.intercalate " | " (map variantName variants)
        ]
  return (extend, comment, M.empty)

pVariant :: Parser Variant
pVariant = do
  name <- upperIdent
  fields <- option [] $ parens (pField `sepBy` symbol ",")
  return $ Variant name fields

-- iso Vec3 <-> VList Float = { to = vec3_to_vlist, from = vec3_from_vlist }
-- Also emits SoA layout helper comment for SIMD codegen
pIsoDecl :: Parser (Env -> Env, Code, M.Map Text Text)
pIsoDecl = do
  keyword "iso"
  name <- upperIdent
  symbol "<->"
  vlt <- pType
  symbol "="
  braces $ do
    keyword "to"
    symbol "="
    toFn <- identifier
    symbol ","
    keyword "from"
    symbol "="
    fromFn <- identifier
    let iso = IsoDecl name vlt toFn fromFn
        extend e = e {envIsos = M.insert name iso (envIsos e)}
        -- SoA comment: tells the backend this type participates in SIMD layout
        soaComment =
          [ "  # iso " <> name <> " <-> " <> showType vlt,
            "  # SoA layout eligible: VList "
              <> name
              <> " -> struct { "
              <> showType vlt
              <> "* xs; i64 len; }",
            "  # SIMD via RVV: vle64.v / vse64.v on each field array"
          ]
    return (extend, soaComment, M.empty)

-- fn name param1 param2 ... : RetType = body .
pFnDecl :: Parser (Env -> Env, Code, M.Map Text Text)
pFnDecl = do
  keyword "fn"
  name <- identifier
  params <- many pParam
  retTy <- option TInt (try (symbol ":" >> pType))
  symbol "="
  -- Build inner env with params
  env <- ask
  let paramMap = M.fromList [(pname, (idx, kind)) | (idx, (pname, kind)) <- zip [0 ..] params]
      innerEnv = env {envVars = M.union paramMap (envVars env), envFns = M.insert name (FnSig params retTy) (envFns env), envCurrentFn = Just name}
  outerLetDepth <- gets csLetDepth
  modify (\s -> s {csLetDepth = 0})  -- fresh let-reg scope for this function
  bodyAttr <- local (const innerEnv) pExpr
  modify (\s -> s {csLetDepth = outerLetDepth})
  symbol "."

  -- Frame layout (from sp = s0 at entry):
  --   s0 + 0          .. s0 + (n-1)*8   : param slots (n = numParams)
  --   s0 + n*8                          : saved s0
  --   s0 + (n+1)*8                      : saved ra
  -- Frame size is rounded up to 16-byte alignment.
  let numParams  = length params
      -- Frame saves: ra + s0..s8 = 10 slots; plus param slots
      numSaved   = 10  -- ra, s0, s1..s8
      rawSize    = (numParams + numSaved) * 8
      frameSize  = ((rawSize + 15) `div` 16) * 16
      raOff      = frameSize - 8
      -- Saved register offsets from sp (= s0 at entry)
      -- ra at raOff, s0 at raOff-8, s1 at raOff-16, ..., s8 at raOff-72
      savedRegs  = ["s0","s1","s2","s3","s4","s5","s6","s7","s8"]

      -- Prologue: allocate frame, save ra + s0-s8, set s0 = sp.
      prologue =
        ["  addi sp, sp, -" <> T.pack (show frameSize),
         "  sd   ra, " <> T.pack (show raOff) <> "(sp)"
        ]
        ++ [ "  sd   " <> reg <> ", " <> T.pack (show (raOff - (i + 1) * 8)) <> "(sp)"
           | (i, reg) <- zip [0 ..] savedRegs
           ]
        ++ ["  mv   s0, sp"]

      -- fn_name_entry: label for fix tail-jumps (frame already set up).
      -- Re-saves updated params from incoming a0..aN.
      entryLabel  = ["fn_" <> name <> "_entry:"]
      paramSaves  =
        [ case k of
            PFloat -> "  fsd  fa" <> T.pack (show i) <> ", " <> T.pack (show (i * 8)) <> "(s0)"
            _      -> "  sd   a"  <> T.pack (show i) <> ", " <> T.pack (show (i * 8)) <> "(s0)"
        | (i, (_, k)) <- zip [(0 :: Int) ..] params
        ]

      -- Epilogue: restore all saved regs via the stable s0.
      epilogue =
        ["  ld   ra, " <> T.pack (show raOff) <> "(s0)",
         "  addi sp, s0, " <> T.pack (show frameSize)
        ]
        ++ [ "  ld   " <> reg <> ", -" <> T.pack (show ((i + 2) * 8)) <> "(sp)"
           | (i, reg) <- zip [0 ..] savedRegs
           ]

      block =
        ["", "fn_" <> name <> ":"]
          ++ prologue
          ++ [""] ++ entryLabel
          ++ paramSaves
          ++ ["  # params: " <> T.intercalate ", " (map (\(p, k) -> p <> ":" <> showKind k) params)]
          ++ atCode bodyAttr
          ++ epilogue
          ++ ["  ret"]

  let sig = FnSig params retTy
      extend e = e {envFns = M.insert name sig (envFns e)}

  return (extend, block, M.empty)

pParam :: Parser (Text, ParamKind)
pParam = parenParam <|> plainParam
  where
    -- (name : Type) syntax — explicit per-param annotation
    parenParam = between (symbol "(") (symbol ")") $ do
      kind <- option PInt $ (PFnPtr <$ symbol "#") <|> (PActor <$ symbol "@")
      name <- identifier
      _ <- symbol ":"
      ty <- pType
      let k = case ty of
            TFloat -> PFloat
            TActor -> PActor
            TFnPtr -> PFnPtr
            _ -> kind
      return (name, k)
    -- plain name / #name / @name / name : Type syntax
    plainParam = do
      kind <- option PInt $ (PFnPtr <$ symbol "#") <|> (PActor <$ symbol "@")
      name <- identifier
      -- Don't consume ': Type' if '=' immediately follows — that's the return-type annotation
      optTy <- optional (try (symbol ":" >> pType <* notFollowedBy (char '=')))
      let k = case optTy of
            Just TFloat -> PFloat
            Just TActor -> PActor
            Just TFnPtr -> PFnPtr
            _ -> kind
      return (name, k)

showKind :: ParamKind -> Text
showKind PInt = "Int"
showKind PFloat = "Float"
showKind PBool = "Bool"
showKind PFnPtr = "FnPtr"
showKind PActor = "Actor"
showKind PLet = "Let"

-- module MyMod { decl1 decl2 ... }
pModuleDecl :: Parser (Env -> Env, Code, M.Map Text Text)
pModuleDecl = do
  keyword "module"
  name <- upperIdent
  decls <- braces (many (try pDecl))
  -- Prefix all function names with ModuleName_
  let code = concatMap (\(_, c, _) -> c) decls
      dataMap = M.unions (map (\(_, _, d) -> d) decls)
      extend e = foldr (\(ext, _, _) acc -> ext acc) e decls
  return (extend, ["  # module " <> name] ++ code, dataMap)

--------------------------------------------------------------------------------
-- Program parser
--------------------------------------------------------------------------------

pProgram :: Parser Text
pProgram = do
  sc
  env0 <- ask

  -- Parse declarations accumulating env; returns (code, warns, data, finalEnv)
  let parseDecls env = do
        md <- optional (try pDecl)
        case md of
          Nothing -> return ([], [], M.empty, env)
          Just (ext, code, dat) -> do
            let env' = ext env
            (restCode, restWarns, restDat, finalEnv) <- local (const env') (parseDecls env')
            return (code ++ restCode, restWarns, M.union dat restDat, finalEnv)

  (declCode, _ws, declData, finalEnv) <- parseDecls env0

  -- Main expression (entry point) — parsed in the fully-extended env so all
  -- declared functions are in scope.
  mainAttr <- local (const finalEnv) pExpr
  optional (symbol ".")
  eof

  st <- get
  let allData = M.union declData (csData st)
      warnings = csWarnings st
      dataLines = map (\(l, d) -> l <> ": " <> d) (M.toAscList allData)

      -- Runtime stubs (would be linked from fpr_runtime.s in real build)
      runtimeStubs =
        [ "",
          "# --- FP-RISC v1 runtime stubs (replace with real impl) ---",
          "fpr_alloc:",
          "  # a0 = size in bytes -> a0 = ptr (bump allocator)",
          "  ret",
          "fpr_spawn:",
          "  # a0 = fn_ptr, a1..aN = initial args -> a0 = actor_ref",
          "  ret",
          "fpr_send:",
          "  # a0 = actor_ref, a1 = msg_ptr -> enqueue",
          "  ret",
          "fpr_open:",
          "  # a0 = path_ptr -> a0 = fd",
          "  li a7, 56   # openat syscall",
          "  ecall",
          "  ret",
          "fpr_read:",
          "  # a0 = fd, a1 = buf, a2 = count -> a0 = bytes_read",
          "  li a7, 63   # read syscall",
          "  ecall",
          "  ret",
          "fpr_write:",
          "  # a0 = fd, a1 = buf, a2 = count -> a0 = bytes_written",
          "  li a7, 64   # write syscall",
          "  ecall",
          "  ret",
          "fpr_close:",
          "  # a0 = fd -> a0 = 0",
          "  li a7, 57   # close syscall",
          "  ecall",
          "  ret",
          "fpr_seek:",
          "  # a0 = fd, a1 = offset, a2 = whence",
          "  li a7, 62   # lseek syscall",
          "  ecall",
          "  ret"
        ]

      warnLines = map (\w -> "# WARNING: " <> w) warnings

      -- main: full callee-saved frame so let-bindings in s1-s8 survive calls.
      -- Frame: ra + s0..s8 = 10 * 8 = 80, rounded to 96 for alignment.
      mainSavedRegs = ["s0","s1","s2","s3","s4","s5","s6","s7","s8"]
      mainFrameSize = 96  -- 9 saved regs * 8 + ra * 8 = 80, padded to 96
      mainRaOff     = mainFrameSize - 8  -- 88
      mainBlock =
        [ ""
        , "main:"
        , "  addi sp, sp, -" <> T.pack (show mainFrameSize)
        , "  sd   ra, " <> T.pack (show mainRaOff) <> "(sp)"
        ]
        ++ [ "  sd   " <> reg <> ", " <> T.pack (show (mainRaOff - (i + 1) * 8)) <> "(sp)"
           | (i, reg) <- zip [0 ..] mainSavedRegs
           ]
        ++ ["  mv   s0, sp"]
        ++ atCode mainAttr
        ++ ["  ld   ra, " <> T.pack (show mainRaOff) <> "(s0)"
           , "  addi sp, s0, " <> T.pack (show mainFrameSize)
           ]
        ++ [ "  ld   " <> reg <> ", -" <> T.pack (show ((i + 2) * 8)) <> "(sp)"
           | (i, reg) <- zip [0 ..] mainSavedRegs
           ]
        ++ ["  li   a0, 0", "  ret"]

      -- Always-present format strings for the print builtin
      printFmts =
        [ "fpr_fmt_int:        .string \"%ld\\n\""
        , "fpr_fmt_float:      .string \"%f\\n\""
        , "fpr_fmt_true:       .string \"true\\n\""
        , "fpr_fmt_false:      .string \"false\\n\""
        , "fpr_fmt_unit:       .string \"()\\n\""
        , "fpr_fmt_unknown:    .string \"<unknown>\\n\""
        , "fpr_fmt_comma:      .string \", \""
        , "fpr_fmt_closeparen: .string \")\\n\""
        , "fpr_fmt_intfld:     .string \"%ld\""
        ]

      assembly =
        T.unlines $
          warnLines
            ++ [".data"]
            ++ printFmts
            ++ dataLines
            ++ ["", ".text", ".global main"]
            ++ declCode
            ++ runtimeStubs
            ++ mainBlock

  return assembly

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

runCompiler :: Text -> Either Text (Text, [Text])
runCompiler input =
  let parser = runReaderT (pProgram) emptyEnv
      stParser = runStateT parser emptyState
   in case parse stParser "<fpr>" input of
        Left bundle -> Left $ T.pack (errorBundlePretty bundle)
        Right (asm, st) -> Right (asm, csWarnings st)
