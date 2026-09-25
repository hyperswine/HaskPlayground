{-# LANGUAGE LambdaCase #-}

-- ============================================================================
-- WatParser (v2): WebAssembly Text Format -> WasmVM.Module
--
-- Supported module fields:
--   (type $t (func (param ..) (result ..)))
--   (import "mod" "name" (func $f (type $t) | (param ..)* (result ..)*))
--   (memory [$m] min [max])            (table [$t] n funcref)
--   (elem (i32.const n) [func] $f ...) (global $g (mut T)|T (T.const c))
--   (data (i32.const n) "...")         (export "n" (func $f))   (start $f)
--   (func $f (export "n")? (type $t)? (param $p T)* (result T)* (local $l T)* body)
--
-- Instructions: everything in WasmVM.Instr, by its standard name, including
-- memargs (offset= align=), br_table, call_indirect (type $t), folded and
-- flat forms mixed, named labels resolved to de Bruijn depths.
--
-- Not supported: flat `block ... end` syntax, typed select, multiple
-- memories/tables, (elem) with expressions other than i32.const.
-- ============================================================================

module WASM.WatParser (parseWat, ParsedModule (..)) where

import Data.Char (chr, digitToInt, isDigit, isHexDigit, isSpace)
import Data.Int (Int32, Int64)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Word (Word32, Word64, Word8)
import Numeric (readHex)
import WASM.WasmVM

data ParsedModule = ParsedModule
  { pmModule :: Module,
    pmFuncNames :: M.Map String Int -- $name -> funcidx (for un-exported entry points)
  }

-- ---------------------------------------------------------------------------
-- S-expressions
-- ---------------------------------------------------------------------------

data SExp = Atom String | Str String | List [SExp]
  deriving (Show)

tokenize :: String -> Either String [SExp]
tokenize = fmap fst . sexps
  where
    sexps s = case skipWs s of
      [] -> Right ([], [])
      (')' : _) -> Right ([], s)
      s' -> do
        (e, r) <- sexp s'
        (es, r') <- sexps r
        pure (e : es, r')
    sexp ('(' : s) = do
      (es, r) <- sexps s
      case skipWs r of
        (')' : r') -> Right (List es, r')
        _ -> Left "unclosed '('"
    sexp ('"' : s) = str s ""
    sexp s =
      let (a, rest) = span (\c -> not (isSpace c) && c `notElem` "()\";") s
       in if null a then Left ("unexpected input: " ++ take 20 s) else Right (Atom a, rest)
    str ('"' : s) acc = Right (Str (reverse acc), s)
    str ('\\' : c : s) acc = case c of
      'n' -> str s ('\n' : acc)
      't' -> str s ('\t' : acc)
      'r' -> str s ('\r' : acc)
      '"' -> str s ('"' : acc)
      '\'' -> str s ('\'' : acc)
      '\\' -> str s ('\\' : acc)
      _
        | isHexDigit c,
          (h : s') <- s,
          isHexDigit h ->
            str s' (chr (16 * digitToInt c + digitToInt h) : acc)
      _ -> Left ("bad string escape: \\" ++ [c])
    str (c : s) acc = str s (c : acc)
    str [] _ = Left "unterminated string"
    skipWs (';' : ';' : s) = skipWs (dropWhile (/= '\n') s)
    skipWs ('(' : ';' : s) = skipWs (blockComment s)
    skipWs (c : s) | isSpace c = skipWs s
    skipWs s = s
    blockComment (';' : ')' : s) = s
    blockComment (_ : s) = blockComment s
    blockComment [] = []

-- ---------------------------------------------------------------------------
-- Module assembly
-- ---------------------------------------------------------------------------

data Names = Names
  { nTypes :: M.Map String Int,
    nFuncs :: M.Map String Int,
    nGlobals :: M.Map String Int
  }

parseWat :: String -> Either String ParsedModule
parseWat src = do
  es <- tokenize src
  fields <- case es of
    [List (Atom "module" : fs)] -> Right (dropWhile isName fs)
    _ -> Left "expected a single (module ...)"
  -- pass 1: explicit types, then function index space (imports first, in order)
  let typeDecls = [(nm, ft) | List (Atom "type" : rest) <- fields, Right (nm, ft) <- [typeDecl rest]]
      explicitTypes = map snd typeDecls
      typeNames = M.fromList [(n, i) | ((Just n, _), i) <- zip typeDecls [0 ..]]
      importFields = [rest | List (Atom "import" : rest) <- fields]
      funcFields = [rest | List (Atom "func" : rest) <- fields]
      importNames = M.fromList [(n, i) | (rest, i) <- zip importFields [0 ..], Just n <- [importFuncName rest]]
      nImp = length importFields
      funcNames =
        M.fromList
          [(n, nImp + i) | (rest, i) <- zip funcFields [0 ..], (Atom n@('$' : _) : _) <- [rest]]
      globalFields = [rest | List (Atom "global" : rest) <- fields]
      globalNames = M.fromList [(n, i) | (Atom n@('$' : _) : _, i) <- zip globalFields [0 ..]]
      names0 = Names typeNames (M.union importNames funcNames) globalNames
  -- pass 2: resolve function types (interning synthesized ones)
  (imports, types1) <- foldM' (\(acc, ts) rest -> importDecl names0 ts rest >>= \(im, ts') -> Right (acc ++ [im], ts')) ([], explicitTypes) importFields
  (funcs, types2) <- foldM' (\(acc, ts) rest -> funcDecl names0 ts rest >>= \(f, ts') -> Right (acc ++ [f], ts')) ([], types1) funcFields
  globals <- mapM globalDecl globalFields
  datas <- mapM dataSeg [rest | List (Atom "data" : rest) <- fields]
  (tblSize, elems) <- tableAndElems names0 fields
  let memPages = case [rest | List (Atom "memory" : rest) <- fields] of
        (rest : _) ->
          let nums = [n | Atom n <- rest, all isDigit n]
           in case nums of
                [a] -> Just (read a, Nothing)
                [a, b] -> Just (read a, Just (read b))
                _ -> Just (0, Nothing)
        [] -> Nothing
      exports =
        M.fromList $
          [(n, i) | List [Atom "export", Str n, List [Atom "func", Atom f]] <- fields, Just i <- [resolveFunc names0 f]]
            ++ [(n, nImp + i) | (rest, i) <- zip funcFields [0 ..], n <- inlineExports rest]
      start = case [f | List [Atom "start", Atom f] <- fields] of
        (f : _) -> resolveFunc names0 f
        [] -> Nothing
      m =
        Module
          { mTypes = IM.fromList (zip [0 ..] types2),
            mImports = imports,
            mFuncs = IM.fromList (zip [0 ..] funcs),
            mTableSize = tblSize,
            mTable = elems,
            mMemPages = memPages,
            mGlobals = globals,
            mData = datas,
            mExports = exports,
            mStart = start
          }
  pure (ParsedModule m (nFuncs names0))
  where
    isName (Atom ('$' : _)) = True
    isName _ = False
    inlineExports rest = [n | List [Atom "export", Str n] <- rest]
    importFuncName rest = case rest of
      [Str _, Str _, List (Atom "func" : Atom n@('$' : _) : _)] -> Just n
      _ -> Nothing

foldM' :: (b -> a -> Either String b) -> b -> [a] -> Either String b
foldM' _ z [] = Right z
foldM' f z (x : xs) = f z x >>= \z' -> foldM' f z' xs

resolveFunc :: Names -> String -> Maybe Int
resolveFunc names f
  | all isDigit f = Just (read f)
  | otherwise = M.lookup f (nFuncs names)

valType :: String -> Either String ValType
valType = \case
  "i32" -> Right TI32
  "i64" -> Right TI64
  "f32" -> Right TF32
  "f64" -> Right TF64
  t -> Left ("unknown value type " ++ t)

-- (type $t (func (param ..) (result ..)))
typeDecl :: [SExp] -> Either String (Maybe String, FuncType)
typeDecl rest = case rest of
  (Atom n@('$' : _) : [List (Atom "func" : sig)]) -> (,) (Just n) <$> sigOf sig
  [List (Atom "func" : sig)] -> (,) Nothing <$> sigOf sig

  _ -> Left "bad (type ...)"

-- (param $x i32) (param i32 i32) (result i32) ...  -> FuncType, plus param names
sigOf :: [SExp] -> Either String FuncType
sigOf sig = do
  ps <- concat <$> mapM paramTypes [r | List (Atom "param" : r) <- sig]
  rs <- concat <$> mapM (mapM valType) [[t | Atom t <- r] | List (Atom "result" : r) <- sig]
  pure (FuncType ps rs)
  where
    paramTypes r = mapM valType [t | Atom t <- r, take 1 t /= "$"]

paramNames :: [SExp] -> [String]
paramNames sig = concat [names r | List (Atom "param" : r) <- sig]
  where
    names r = case r of
      (Atom n@('$' : _) : _) -> [n]
      ts -> map (const "") [t | Atom t <- ts] -- anonymous params still occupy indices

-- Resolve a signature that is either (type $t) or inline; intern inline ones.
resolveSig :: Names -> [FuncType] -> [SExp] -> Either String (FuncType, Int, [FuncType])
resolveSig names types sig = case [t | List [Atom "type", Atom t] <- sig] of
  (t : _) -> do
    idx <- if all isDigit t then Right (read t) else maybe (Left ("unknown type " ++ t)) Right (M.lookup t (nTypes names))
    ft <- maybe (Left "type index out of range") Right (lookupL idx types)
    pure (ft, idx, types)
  [] -> do
    ft <- sigOf sig
    case lookup ft (zip types [0 ..]) of
      Just idx -> pure (ft, idx, types)
      Nothing -> pure (ft, length types, types ++ [ft])
  where
    lookupL i xs = if i < length xs then Just (xs !! i) else Nothing

importDecl :: Names -> [FuncType] -> [SExp] -> Either String (Import, [FuncType])
importDecl names types rest = case rest of
  [Str md, Str nm, List (Atom "func" : r)] -> do
    let sig = dropWhile isName r
    (ft, _, types') <- resolveSig names types sig
    pure (Import md nm ft, types')
  _ -> Left "only function imports are supported"
  where
    isName (Atom ('$' : _)) = True
    isName _ = False

funcDecl :: Names -> [FuncType] -> [SExp] -> Either String (Func, [FuncType])
funcDecl names types rest = do
  let r0 = dropWhile isName rest
      r1 = filter (not . isExport) r0
      (sig, r2) = span isSig r1
      (localDecls, body) = span isLocal r2
  (ft, _, types') <- resolveSig names types sig
  let pnames = paramNames sig
      lnames = concat [localNames l | List (Atom "local" : l) <- localDecls]
  ltypes <- concat <$> mapM (\l -> mapM valType [t | Atom t <- l, take 1 t /= "$"]) [l | List (Atom "local" : l) <- localDecls]
  let allNames = pnames ++ lnames
      localMap = M.fromList [(n, i) | (n, i) <- zip allNames [0 ..], n /= ""]
      env = Env names localMap []
  code <- compileSeq env body
  pure (Func ft ltypes code, types')
  where
    isName (Atom ('$' : _)) = True
    isName _ = False
    isExport (List (Atom "export" : _)) = True
    isExport _ = False
    isSig (List (Atom k : _)) = k `elem` ["type", "param", "result"]
    isSig _ = False
    isLocal (List (Atom "local" : _)) = True
    isLocal _ = False
    localNames l = case l of
      (Atom n@('$' : _) : _) -> [n]
      ts -> map (const "") [t | Atom t <- ts]

globalDecl :: [SExp] -> Either String Global
globalDecl rest = do
  let r = dropWhile isName rest
  case r of
    [List [Atom "mut", Atom t], initE] -> mk True t initE
    [Atom t, initE] -> mk False t initE
    _ -> Left "bad (global ...)"
  where
    isName (Atom ('$' : _)) = True
    isName _ = False
    mk mut t initE = do
      ty <- valType t
      v <- constExpr initE
      pure (Global ty mut v)

constExpr :: SExp -> Either String Val
constExpr = \case
  List [Atom "i32.const", Atom n] -> VI32 . fromIntegral <$> readInt n
  List [Atom "i64.const", Atom n] -> VI64 . fromIntegral <$> readInt n
  List [Atom "f32.const", Atom n] -> VF32 . realToFrac <$> readFloat n
  List [Atom "f64.const", Atom n] -> VF64 <$> readFloat n
  e -> Left ("unsupported constant expression: " ++ show e)

dataSeg :: [SExp] -> Either String (Int, [Word8])
dataSeg rest = case dropWhile isName rest of
  (List [Atom "i32.const", Atom n] : strs) -> do
    off <- readInt n
    pure (fromIntegral off, concat [map (fromIntegral . fromEnum) s | Str s <- strs])
  _ -> Left "unsupported (data ...) form"
  where
    isName (Atom ('$' : _)) = True
    isName _ = False

tableAndElems :: Names -> [SExp] -> Either String (Int, IM.IntMap Int)
tableAndElems names fields = do
  let tblSize = case [rest | List (Atom "table" : rest) <- fields] of
        (rest : _) -> case [n | Atom n <- rest, all isDigit n] of
          (a : _) -> read a
          [] -> 0
        [] -> 0
  elemsL <- concat <$> mapM elemSeg [rest | List (Atom "elem" : rest) <- fields]
  let maxIdx = if null elemsL then 0 else maximum (map fst elemsL) + 1
  pure (max tblSize maxIdx, IM.fromList elemsL)
  where
    elemSeg rest = case dropWhile isName rest of
      (List [Atom "i32.const", Atom n] : fs) -> do
        off <- readInt n
        idxs <- mapM (\f -> maybe (Left ("unknown func in elem: " ++ f)) Right (resolveFunc names f)) [f | Atom f <- fs, f /= "func"]
        pure (zip [fromIntegral off ..] idxs)
      _ -> Left "unsupported (elem ...) form"
    isName (Atom ('$' : _)) = True
    isName _ = False

-- ---------------------------------------------------------------------------
-- Instruction compilation
-- ---------------------------------------------------------------------------

data Env = Env
  { eNames :: Names,
    eLocals :: M.Map String Int,
    eLabels :: [Maybe String] -- innermost first
  }

compileSeq :: Env -> [SExp] -> Either String [Instr]
compileSeq _ [] = Right []
compileSeq env (Atom a : rest) = do
  (ins, rest') <- plainInstr env a rest
  (ins ++) <$> compileSeq env rest'
compileSeq env (List l : rest) = do
  ins <- compileFolded env l
  (ins ++) <$> compileSeq env rest
compileSeq _ (e : _) = Left ("unexpected: " ++ show e)

-- immediates are consumed from the token stream that follows the opcode
plainInstr :: Env -> String -> [SExp] -> Either String ([Instr], [SExp])
plainInstr env a rest = case a of
  "i32.const" -> immAtom rest $ \n r -> readInt n >>= \v -> Right ([Const (VI32 (fromIntegral v))], r)
  "i64.const" -> immAtom rest $ \n r -> readInt n >>= \v -> Right ([Const (VI64 (fromIntegral v))], r)
  "f32.const" -> immAtom rest $ \n r -> readFloat n >>= \v -> Right ([Const (VF32 (realToFrac v))], r)
  "f64.const" -> immAtom rest $ \n r -> readFloat n >>= \v -> Right ([Const (VF64 v)], r)
  "local.get" -> immLocal env rest $ \k r -> Right ([LocalGet k], r)
  "local.set" -> immLocal env rest $ \k r -> Right ([LocalSet k], r)
  "local.tee" -> immLocal env rest $ \k r -> Right ([LocalTee k], r)
  "global.get" -> immGlobal env rest $ \k r -> Right ([GlobalGet k], r)
  "global.set" -> immGlobal env rest $ \k r -> Right ([GlobalSet k], r)
  "br" -> immLabel env rest $ \d r -> Right ([Br d], r)
  "br_if" -> immLabel env rest $ \d r -> Right ([BrIf d], r)
  "br_table" ->
    let (labels, r) = span isLabelAtom rest
     in do
          ds <- mapM (\l -> immLabel env [l] (\d _ -> Right d)) labels
          case reverse ds of
            (d : revTs) -> Right ([BrTable (reverse revTs) d], r)
            [] -> Left "br_table needs a default label"
  "call" -> immAtom rest $ \f r -> case resolveFunc (eNames env) f of
    Just i -> Right ([Call i], r)
    Nothing -> Left ("unknown function: " ++ f)
  "call_indirect" ->
    let r0 = case rest of (Atom t : r) | not (isTypeUse (Atom t)) && take 1 t == "$" -> r; _ -> rest -- optional table name
        (tyUse, r1) = span isTypeUse r0
     in case [t | List [Atom "type", Atom t] <- tyUse] of
          (t : _) ->
            let idx = if all isDigit t then Just (read t) else M.lookup t (nTypes (eNames env))
             in maybe (Left ("unknown type " ++ t)) (\i -> Right ([CallIndirect i], r1)) idx
          [] -> Left "call_indirect requires (type $t)"
  _
    | Just (ty, narrow) <- M.lookup a loadTable ->
        let (off, r) = memarg rest in Right ([Load ty narrow off], r)
    | Just (ty, narrow) <- M.lookup a storeTable ->
        let (off, r) = memarg rest in Right ([Store ty narrow off], r)
    | Just i <- M.lookup a opTable -> Right ([i], rest)
    | otherwise -> Left ("unknown instruction: " ++ a)
  where
    isLabelAtom (Atom ('$' : _)) = True
    isLabelAtom (Atom n) = all isDigit n
    isLabelAtom _ = False
    isTypeUse (List (Atom k : _)) = k `elem` ["type", "param", "result"]
    isTypeUse _ = False

memarg :: [SExp] -> (Int, [SExp])
memarg (Atom a : rest)
  | Just v <- stripPrefix' "offset=" a = let (o, r) = memarg rest in (either (const 0) fromIntegral (readInt v) + o, r)
  | Just _ <- stripPrefix' "align=" a = memarg rest
memarg rest = (0, rest)

stripPrefix' :: String -> String -> Maybe String
stripPrefix' p s = if take (length p) s == p then Just (drop (length p) s) else Nothing

compileFolded :: Env -> [SExp] -> Either String [Instr]
compileFolded env l = case l of
  (Atom "block" : rest) -> do
    let (lbl, r) = takeLabel rest
        (bt, body) = blockType env r
    inner <- compileSeq env {eLabels = lbl : eLabels env} body
    pure [Block bt inner]
  (Atom "loop" : rest) -> do
    let (lbl, r) = takeLabel rest
        (bt, body) = blockType env r
    inner <- compileSeq env {eLabels = lbl : eLabels env} body
    pure [Loop bt inner]
  (Atom "if" : rest) -> do
    let (lbl, r) = takeLabel rest
        (bt, r') = blockType env r
        (condEs, thenE, elseE) = splitIf r'
        env' = env {eLabels = lbl : eLabels env}
    cond <- compileSeq env condEs
    t <- compileSeq env' thenE
    e <- compileSeq env' elseE
    pure (cond ++ [If bt t e])
  (Atom a : rest) -> do
    (opIns, rest') <- plainInstr env a rest
    operands <- compileSeq env rest'
    pure (operands ++ opIns)
  _ -> Left ("cannot compile: " ++ show (List l))
  where
    takeLabel (Atom n@('$' : _) : r) = (Just n, r)
    takeLabel r = (Nothing, r)
    splitIf es =
      let isArm (List (Atom k : _)) = k `elem` ["then", "else"]
          isArm _ = False
          conds = takeWhile (not . isArm) es
          arms = drop (length conds) es
       in (conds, concat [b | List (Atom "then" : b) <- arms], concat [b | List (Atom "else" : b) <- arms])

-- (type $t) | (param ..)* (result ..)*  at the head of a block body
blockType :: Env -> [SExp] -> (BlockType, [SExp])
blockType env r =
  let (sig, body) = span isSig r
      bt = case [t | List [Atom "type", Atom t] <- sig] of
        (t : _) ->
          let idx = if all isDigit t then Just (read t) else M.lookup t (nTypes (eNames env))
           in maybe (BT [] []) (const (BT [] [])) idx -- (typed blocks are resolved by the driver; rare)
        [] -> case sigOf sig of
          Right (FuncType ps rs) -> BT ps rs
          Left _ -> BT [] []
   in (bt, body)
  where
    isSig (List (Atom k : _)) = k `elem` ["type", "param", "result"]
    isSig _ = False

-- immediate readers ----------------------------------------------------------

immAtom :: [SExp] -> (String -> [SExp] -> Either String a) -> Either String a
immAtom (Atom n : r) k = k n r
immAtom _ _ = Left "expected immediate"

immLocal :: Env -> [SExp] -> (Int -> [SExp] -> Either String a) -> Either String a
immLocal env (Atom n : r) k
  | Just i <- M.lookup n (eLocals env) = k i r
  | all isDigit n = k (read n) r
  | otherwise = Left ("unknown local: " ++ n)
immLocal _ _ _ = Left "expected local immediate"

immGlobal :: Env -> [SExp] -> (Int -> [SExp] -> Either String a) -> Either String a
immGlobal env (Atom n : r) k
  | Just i <- M.lookup n (nGlobals (eNames env)) = k i r
  | all isDigit n = k (read n) r
  | otherwise = Left ("unknown global: " ++ n)
immGlobal _ _ _ = Left "expected global immediate"

immLabel :: Env -> [SExp] -> (Int -> [SExp] -> Either String a) -> Either String a
immLabel env (Atom n : r) k
  | all isDigit n = k (read n) r
  | otherwise = case lookup (Just n) (zip (eLabels env) [0 ..]) of
      Just d -> k d r
      Nothing -> Left ("unknown label: " ++ n)
immLabel _ _ _ = Left "expected label immediate"

readInt :: String -> Either String Integer
readInt s0 = case s0 of
  ('-' : s) -> negate <$> mag s
  ('+' : s) -> mag s
  s -> mag s
  where
    mag s = case filter (/= '_') s of
      ('0' : 'x' : h) | not (null h), all isHexDigit h -> Right (fst (head (readHex h)))
      d | not (null d), all isDigit d -> Right (read d)
      _ -> Left ("bad integer: " ++ s0)

readFloat :: String -> Either String Double
readFloat s0 = case s0 of
  "inf" -> Right (1 / 0)
  "-inf" -> Right (-1 / 0)
  "+inf" -> Right (1 / 0)
  s | take 3 s == "nan" || take 4 s == "-nan" -> Right (0 / 0)
  ('-' : s) -> negate <$> readFloat s
  ('+' : s) -> readFloat s
  s -> case readInt s of
    Right i -> Right (fromInteger i)
    Left _ ->
      let s' = fixup (filter (/= '_') s)
       in case reads s' :: [(Double, String)] of
            [(v, "")] -> Right v
            _ -> Left ("bad float: " ++ s0)
  where
    fixup s = let s1 = if take 1 s == "." then '0' : s else s
                  s2 = case break (== 'e') s1 of
                    (m, e) | last m == '.' -> m ++ "0" ++ e
                    _ -> s1
               in case break (== 'e') s2 of
                    (m, 'e' : e) | not ('.' `elem` m) -> m ++ ".0e" ++ e
                    _ -> s2

-- ---------------------------------------------------------------------------
-- The opcode name tables (generated combinatorially)
-- ---------------------------------------------------------------------------

isizes :: [(String, ISz)]
isizes = [("i32", S32), ("i64", S64)]

fsizes :: [(String, FSz)]
fsizes = [("f32", P32), ("f64", P64)]

opTable :: M.Map String Instr
opTable =
  M.fromList $
    [ ("unreachable", Unreachable),
      ("nop", Nop),
      ("return", Return),
      ("drop", Drop),
      ("select", Select),
      ("memory.size", MemSize),
      ("memory.grow", MemGrow),
      ("memory.fill", MemFill),
      ("memory.copy", MemCopy),
      ("i32.wrap_i64", Cvtop Wrap),
      ("i64.extend_i32_s", Cvtop ExtendS),
      ("i64.extend_i32_u", Cvtop ExtendU),
      ("f32.demote_f64", Cvtop Demote),
      ("f64.promote_f32", Cvtop Promote),
      ("i32.reinterpret_f32", Cvtop (Reinterpret TI32)),
      ("i64.reinterpret_f64", Cvtop (Reinterpret TI64)),
      ("f32.reinterpret_i32", Cvtop (Reinterpret TF32)),
      ("f64.reinterpret_i64", Cvtop (Reinterpret TF64))
    ]
      ++ [(s ++ "." ++ n, IBinop sz c) | (s, sz) <- isizes, (n, c) <- ibins]
      ++ [(s ++ "." ++ n, IUnop sz c) | (s, sz) <- isizes, (n, c) <- iuns]
      ++ [(s ++ ".eqz", IEqz sz) | (s, sz) <- isizes]
      ++ [(s ++ "." ++ n, IRelop sz c) | (s, sz) <- isizes, (n, c) <- irels]
      ++ [(s ++ "." ++ n, FBinop sz c) | (s, sz) <- fsizes, (n, c) <- fbins]
      ++ [(s ++ "." ++ n, FUnop sz c) | (s, sz) <- fsizes, (n, c) <- funs]
      ++ [(s ++ "." ++ n, FRelop sz c) | (s, sz) <- fsizes, (n, c) <- frels]
      ++ [ (is ++ ".trunc_" ++ sat ++ fs ++ sg, Cvtop (mk isz fsz))
           | (is, isz) <- isizes,
             (fs, fsz) <- fsizes,
             (sat, sg, mk) <-
               [ ("", "_s", TruncS),
                 ("", "_u", TruncU),
                 ("sat_", "_s", TruncSatS),
                 ("sat_", "_u", TruncSatU)
               ]
         ]
      ++ [ (fs ++ ".convert_" ++ is ++ sg, Cvtop (mk fsz isz))
           | (fs, fsz) <- fsizes,
             (is, isz) <- isizes,
             (sg, mk) <- [("_s", ConvertS), ("_u", ConvertU)]
         ]
      ++ [("i64.extend32_s", IUnop S64 IExt32S)]
  where
    ibins =
      [ ("add", IAdd), ("sub", ISub), ("mul", IMul), ("div_s", IDivS), ("div_u", IDivU),
        ("rem_s", IRemS), ("rem_u", IRemU), ("and", IAnd), ("or", IOr), ("xor", IXor),
        ("shl", IShl), ("shr_s", IShrS), ("shr_u", IShrU), ("rotl", IRotl), ("rotr", IRotr)
      ]
    iuns = [("clz", IClz), ("ctz", ICtz), ("popcnt", IPopcnt), ("extend8_s", IExt8S), ("extend16_s", IExt16S)]
    irels =
      [ ("eq", IEq), ("ne", INe), ("lt_s", ILtS), ("lt_u", ILtU), ("gt_s", IGtS),
        ("gt_u", IGtU), ("le_s", ILeS), ("le_u", ILeU), ("ge_s", IGeS), ("ge_u", IGeU)
      ]
    fbins = [("add", FAdd), ("sub", FSub), ("mul", FMul), ("div", FDiv), ("min", FMin), ("max", FMax), ("copysign", FCopysign)]
    funs = [("abs", FAbs), ("neg", FNeg), ("ceil", FCeil), ("floor", FFloor), ("trunc", FTrunc), ("nearest", FNearest), ("sqrt", FSqrt)]
    frels = [("eq", FEq), ("ne", FNe), ("lt", FLt), ("gt", FGt), ("le", FLe), ("ge", FGe)]

loadTable :: M.Map String (ValType, Maybe (Int, Bool))
loadTable =
  M.fromList
    [ ("i32.load", (TI32, Nothing)),
      ("i64.load", (TI64, Nothing)),
      ("f32.load", (TF32, Nothing)),
      ("f64.load", (TF64, Nothing)),
      ("i32.load8_s", (TI32, Just (1, True))),
      ("i32.load8_u", (TI32, Just (1, False))),
      ("i32.load16_s", (TI32, Just (2, True))),
      ("i32.load16_u", (TI32, Just (2, False))),
      ("i64.load8_s", (TI64, Just (1, True))),
      ("i64.load8_u", (TI64, Just (1, False))),
      ("i64.load16_s", (TI64, Just (2, True))),
      ("i64.load16_u", (TI64, Just (2, False))),
      ("i64.load32_s", (TI64, Just (4, True))),
      ("i64.load32_u", (TI64, Just (4, False)))
    ]

storeTable :: M.Map String (ValType, Maybe Int)
storeTable =
  M.fromList
    [ ("i32.store", (TI32, Nothing)),
      ("i64.store", (TI64, Nothing)),
      ("f32.store", (TF32, Nothing)),
      ("f64.store", (TF64, Nothing)),
      ("i32.store8", (TI32, Just 1)),
      ("i32.store16", (TI32, Just 2)),
      ("i64.store8", (TI64, Just 1)),
      ("i64.store16", (TI64, Just 2)),
      ("i64.store32", (TI64, Just 4))
    ]

-- silence unused-import warnings for types referenced only in signatures
_unused :: (Int32, Int64, Word32, Word64, [Int] -> Int, Maybe Int -> Int)
_unused = (0, 0, 0, 0, foldl' (+) 0, fromMaybe 0)
