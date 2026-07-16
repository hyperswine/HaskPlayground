{-# LANGUAGE LambdaCase #-}
-- ============================================================================
-- WatParser: parse a (subset of) WebAssembly Text Format into WasmModel.Instr
--
-- Supported:
--   * (module ...) with (import ...), (memory ...), (data (i32.const N) "s"),
--     and exactly one (func ...) which becomes the actor body
--   * named locals: (local $x i32), resolved to indices in declaration order
--   * named labels on block/loop and named br/br_if targets, resolved to
--     de Bruijn depths (exactly wasm's label model, which Instr already uses)
--   * folded control flow: (block ...) (loop ...) (if <cond>* (then..)(else..))
--   * plain and folded instructions freely mixed, e.g. both
--       (local.get $i) (i32.load8_u)          and
--       (i32.add (local.get $a) (local.get $b))
--     compile to the same post-order instruction sequence
--   * string escapes in data segments: \n \t \r \" \\ \NN (hex)
--
-- Deliberately NOT supported (documented gaps vs real wasm):
--   * multiple functions / call to non-host functions (Instr has no Call;
--     the model has block frames but no function frames)
--   * typed value stack (f64/i64), memarg offsets, globals, tables
-- ============================================================================

module WatParser (parseWat) where

import Data.Char (chr, digitToInt, isDigit, isHexDigit, isSpace)
import qualified Data.Map.Strict as M
import WasmModel (HostFn (..), Instr (..))

-- ---------------------------------------------------------------------------
-- S-expressions
-- ---------------------------------------------------------------------------

data SExp = Atom String | Str String | List [SExp]
  deriving (Show)

tokenize :: String -> Either String [SExp]
tokenize = fmap fst . sexps
  where
    sexps :: String -> Either String ([SExp], String)
    sexps s = case skipWs s of
      [] -> Right ([], [])
      (')' : _) -> Right ([], s) -- caller consumes ')'
      _ -> do
        (e, s') <- sexp (skipWs s)
        (es, s'') <- sexps s'
        pure (e : es, s'')

    sexp :: String -> Either String (SExp, String)
    sexp ('(' : s) = do
      (es, s') <- sexps s
      case skipWs s' of
        (')' : s'') -> Right (List es, s'')
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
    skipWs (c : s) | isSpace c = skipWs s
    skipWs s = s

-- ---------------------------------------------------------------------------
-- Compilation environment
-- ---------------------------------------------------------------------------

data Env = Env
  { eLocals :: M.Map String Int, -- $name -> local index
    eLabels :: [Maybe String] -- innermost first; Nothing = unnamed (if)
  }

hostMap :: M.Map String HostFn
hostMap =
  M.fromList
    [ ("$fd_write", FdWrite),
      ("$fd_read", FdRead),
      ("$path_open", PathOpen)
    ]

-- ---------------------------------------------------------------------------
-- Top level: module -> (body, data segments)
-- ---------------------------------------------------------------------------

parseWat :: String -> Either String ([Instr], [(Int, String)])
parseWat src = do
  es <- tokenize src
  m <- case es of
    [List (Atom "module" : fields)] -> Right fields
    _ -> Left "expected a single (module ...)"
  segs <- mapM dataSeg [f | f@(List (Atom "data" : _)) <- m]
  body <- case [f | List (Atom "func" : f) <- m] of
    [f] -> compileFunc f
    [] -> Left "no (func ...) in module"
    _ -> Left "only one (func ...) supported (Instr has no Call)"
  pure (body, segs)
  where
    dataSeg (List [Atom "data", List [Atom "i32.const", Atom n], Str s]) = do
      off <- readNum n
      pure (off, s)
    dataSeg _ = Left "unsupported (data ...) form"

compileFunc :: [SExp] -> Either String [Instr]
compileFunc parts = do
  let isMeta (List (Atom k : _)) = k `elem` ["export", "param", "result", "type"]
      isMeta (Atom ('$' : _)) = True -- function name
      isMeta _ = False
      parts' = dropWhile isMeta parts
      (localDecls, body) = span isLocal parts'
      isLocal (List (Atom "local" : _)) = True
      isLocal _ = False
  locals <- localEnv (concatMap unLocal localDecls)
  compileSeq (Env locals []) body
  where
    unLocal (List (Atom "local" : rest)) = [n | Atom n@('$' : _) <- rest]
    unLocal _ = []
    localEnv names = Right (M.fromList (zip names [0 ..]))

-- ---------------------------------------------------------------------------
-- Instruction compilation (post-order flatten of folded forms)
-- ---------------------------------------------------------------------------

compileSeq :: Env -> [SExp] -> Either String [Instr]
compileSeq _ [] = Right []
compileSeq env (Atom a : rest) = do
  (ins, rest') <- plainInstr env a rest
  (ins ++) <$> compileSeq env rest'
compileSeq env (List l : rest) = do
  ins <- compileFolded env l
  (ins ++) <$> compileSeq env rest
compileSeq _ (e : _) = Left ("unexpected: " ++ show e)

-- plain (unparenthesised) instruction, consuming immediates from the stream
plainInstr :: Env -> String -> [SExp] -> Either String ([Instr], [SExp])
plainInstr env a rest = case a of
  "i32.const" -> immNum rest $ \n r -> Right ([Const n], r)
  "local.get" -> immLocal env rest $ \k r -> Right ([LGet k], r)
  "local.set" -> immLocal env rest $ \k r -> Right ([LSet k], r)
  "local.tee" -> immLocal env rest $ \k r -> Right ([LTee k], r)
  "br" -> immLabel env rest $ \d r -> Right ([Br d], r)
  "br_if" -> immLabel env rest $ \d r -> Right ([BrIf d], r)
  "call" -> immCall rest $ \h r -> Right ([Host h], r)
  _ -> case simpleOp a of
    Just i -> Right ([i], rest)
    Nothing -> Left ("unknown instruction: " ++ a)

simpleOp :: String -> Maybe Instr
simpleOp = \case
  "i32.add" -> Just Add
  "i32.sub" -> Just Sub
  "i32.mul" -> Just Mul
  "i32.div_s" -> Just DivS
  "i32.rem_s" -> Just RemS
  "i32.lt_s" -> Just LtS
  "i32.eqz" -> Just Eqz
  "i32.load8_u" -> Just Load8U
  "i32.store8" -> Just Store8
  "drop" -> Just Drp
  _ -> Nothing

-- folded form: (op immediates* operand-exprs*)
compileFolded :: Env -> [SExp] -> Either String [Instr]
compileFolded env l = case l of
  (Atom "block" : rest) -> do
    let (lbl, body) = takeLabel rest
    inner <- compileSeq env {eLabels = lbl : eLabels env} (dropResult body)
    pure [Blk inner]
  (Atom "loop" : rest) -> do
    let (lbl, body) = takeLabel rest
    inner <- compileSeq env {eLabels = lbl : eLabels env} (dropResult body)
    pure [Lp inner]
  (Atom "if" : rest) -> do
    let (lbl, rest') = takeLabel rest
        (condEs, thenE, elseE) = splitIf (dropResult rest')
        env' = env {eLabels = lbl : eLabels env}
    cond <- compileSeq env condEs -- condition evaluated OUTSIDE the if label
    t <- compileSeq env' thenE
    e <- compileSeq env' elseE
    pure (cond ++ [Ift t e])
  (Atom a : rest) -> do
    -- generic folded instruction: immediates first, then operand exprs
    (opIns, rest') <- plainInstr env a rest
    operands <- compileSeq env rest'
    pure (operands ++ opIns)
  _ -> Left ("cannot compile: " ++ show (List l))
  where
    takeLabel (Atom n@('$' : _) : r) = (Just n, r)
    takeLabel r = (Nothing, r)

    dropResult (List (Atom "result" : _) : r) = r
    dropResult r = r

    splitIf es =
      let isThen (List (Atom "then" : _)) = True; isThen _ = False
          isElse (List (Atom "else" : _)) = True; isElse _ = False
          conds = takeWhile (\e -> not (isThen e || isElse e)) es
          arms = drop (length conds) es
          thenB = concat [b | List (Atom "then" : b) <- arms]
          elseB = concat [b | List (Atom "else" : b) <- arms]
       in (conds, thenB, elseB)

-- immediate readers ----------------------------------------------------------

immNum :: [SExp] -> (Int -> [SExp] -> Either String a) -> Either String a
immNum (Atom n : r) k = readNum n >>= \v -> k v r
immNum _ _ = Left "expected numeric immediate"

immLocal :: Env -> [SExp] -> (Int -> [SExp] -> Either String a) -> Either String a
immLocal env (Atom n : r) k
  | Just i <- M.lookup n (eLocals env) = k i r
  | all isDigit n = k (read n) r
  | otherwise = Left ("unknown local: " ++ n)
immLocal _ _ _ = Left "expected local immediate"

immLabel :: Env -> [SExp] -> (Int -> [SExp] -> Either String a) -> Either String a
immLabel env (Atom n : r) k
  | all isDigit n = k (read n) r
  | otherwise = case lookup (Just n) (zip (eLabels env) [0 ..]) of
      Just d -> k d r
      Nothing -> Left ("unknown label: " ++ n)
immLabel _ _ _ = Left "expected label immediate"

immCall :: [SExp] -> (HostFn -> [SExp] -> Either String a) -> Either String a
immCall (Atom n : r) k = case M.lookup n hostMap of
  Just h -> k h r
  Nothing -> Left ("call target not a known host fn (no user Call support): " ++ n)
immCall _ _ = Left "expected call target"

readNum :: String -> Either String Int
readNum s = case s of
  ('-' : d) | all isDigit d, not (null d) -> Right (negate (read d))
  d | all isDigit d, not (null d) -> Right (read d)
  _ -> Left ("bad number: " ++ s)
