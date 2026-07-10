{-# LANGUAGE LambdaCase #-}

-- ============================================================================
-- Driver: Sol source text -> parse (SolParser) -> convert to pipeline Expr ->
-- stages 2..8 (FullAST) -> RC-annotated IR, the last stop before codegen.
--
-- The converter accepts the statically-typed pipeline SUBSET of Sol:
--   * top-level single-clause functions, plain-name params, defined before use,
--     `main` (0 params) last
--   * blocks with `;`, lambdas, application, |>, $
--   * closed operator set:  + - * <= < ==   (desugar to saturated prims)
--   * True/False literals, case-on-Bool  ==>  if
--   * records, projection chains, record update incl. nested paths
--     ({m | p.q = e} rewrites to {m | p = {m.p | q = e}} -- note m twice:
--      stage 8 inserts the dup)
--   * prims by name: newHandle closeHandle fromTo mapV sumV  (Handle is linear)
--
-- Everything outside the subset is rejected with a message naming the feature
-- (variant cases, lists, strings, atoms, guards, multi-clause, |>?, ::, !).
-- Full Sol lowers those in stage 1 (see the interpreter PoC); this driver
-- deliberately keeps the typed pipeline's surface small.
-- ============================================================================
module SolcMain where

import Control.Exception (SomeException, displayException, evaluate, try)
import Data.List (foldl', intercalate)
import qualified Data.Map.Strict as M
import FullAST
import SolParser
  ( SExpr (..),
    SPat (..),
    SStmt (..),
    STop (..),
    Seg (..),
    program,
  )
import qualified SolParser as SP
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty, parse)

-- ----------------------------------------------------------------------------
-- Surface -> pipeline Expr
-- ----------------------------------------------------------------------------

primArities :: M.Map SP.Name Int
primArities =
  M.fromList
    [ ("newHandle", 1),
      ("closeHandle", 1),
      ("fromTo", 2),
      ("mapV", 2),
      ("sumV", 1),
      ("zipV", 2),
      ("fstV", 1),
      ("sndV", 1),
      ("mapFstV", 2),
      ("mapSndV", 2)
    ]

opPrim :: SP.Name -> Maybe SP.Name
opPrim = \case
  "+" -> Just "add"
  "-" -> Just "sub"
  "*" -> Just "mul"
  "<=" -> Just "le"
  "<" -> Just "lt"
  "==" -> Just "eq"
  _ -> Nothing

conv :: SExpr -> Either String Expr
conv = \case
  SInt n -> Right (ELit (fromIntegral n))
  SVar "True" -> Right (EPrim "true" [])
  SVar "False" -> Right (EPrim "false" [])
  SVar x
    | M.member x primArities ->
        Left $ "primitive '" ++ x ++ "' must be fully applied (no first-class prims in the typed subset)"
    | otherwise -> Right (EVar x)
  e@(SApp _ _) -> convSpine e
  SBin "|>" a f -> conv (SApp f a)
  SBin op a b -> case opPrim op of
    Just p -> do a' <- conv a; b' <- conv b; Right (EPrim p [a', b'])
    Nothing -> Left $ "operator '" ++ op ++ "' is outside the typed pipeline subset"
  SLam ps e -> do
    e' <- conv e
    Right (foldr ELam e' ps)
  SBlock stmts final -> goBlock stmts
    where
      goBlock [] = conv final
      goBlock (SBind n ps rhs : rest) = do
        rhs' <- conv rhs
        body <- goBlock rest
        Right (ELet n (foldr ELam rhs' ps) body)
      goBlock (SBindPat {} : _) =
        Left "refutable let-patterns are outside the typed pipeline subset"
  SCase scrut arms -> do
    s' <- conv scrut
    case arms of
      [(PCon "True" [], a), (PCon "False" [], b)] -> EIf s' <$> conv a <*> conv b
      [(PCon "False" [], b), (PCon "True" [], a)] -> EIf s' <$> conv a <*> conv b
      [(PCon "True" [], a), (PWild, b)] -> EIf s' <$> conv a <*> conv b
      [(PCon "False" [], b), (PWild, a)] -> EIf s' <$> conv a <*> conv b
      _ -> Left "only case-on-Bool (True/False arms) is in the typed pipeline subset"
  SProj e path -> do
    e' <- conv e
    Right (foldl' EProj e' path)
  SRec fs -> ERec <$> mapM (\(f, e) -> (,) f <$> conv e) fs
  SUpd m assigns -> do
    -- nested paths: {m | p.q = e}  ==>  {m | p = {m.p | q = e}}
    let norm ([f], e) = Right (f, e)
        norm (f : rest, e) = Right (f, SUpd (SProj m [f]) [(rest, e)])
        norm ([], _) = Left "empty update path"
    roots <- mapM norm assigns
    let names = map fst roots
    if length names /= length (M.keys (M.fromList roots))
      then Left "duplicate root fields in a single record update"
      else do
        m' <- conv m
        fs <- mapM (\(f, e) -> (,) f <$> conv e) roots
        Right (EUpd m' fs)
  SStrI _ -> Left "strings are outside the typed pipeline subset"
  SAtom _ -> Left "atoms are outside the typed pipeline subset"
  STup _ -> Left "tuple expressions are outside the typed pipeline subset"
  SList _ -> Left "list literals are outside the typed pipeline subset (use fromTo)"

convSpine :: SExpr -> Either String Expr
convSpine e0 = do
  let (h, args) = spine e0 []
  case h of
    SVar p
      | Just ar <- M.lookup p primArities ->
          if length args == ar
            then EPrim p <$> mapM conv args
            else
              Left $
                "primitive '"
                  ++ p
                  ++ "' expects "
                  ++ show ar
                  ++ " argument(s), got "
                  ++ show (length args)
    _ -> do
      h' <- conv h
      as <- mapM conv args
      Right (foldl' EApp h' as)
  where
    spine (SApp f a) acc = spine f (a : acc)
    spine h acc = (h, acc)

-- Top-level program: single-clause defs in order, main (0 params) last.
convProgram :: [STop] -> Either String (Expr, [String])
convProgram tops = do
  let binds = [(n, ps, g, b) | TBind n ps g b <- tops]
      ignored =
        [ "signature/type/shape declarations ignored by the typed subset driver"
          | any (\case TSig {} -> True; TType {} -> True; TShape {} -> True; _ -> False) tops
        ]
  defs <- mapM oneClause binds
  let names = map (\(n, _, _) -> n) defs
  if length names /= length (M.keys (M.fromList (zip names (repeat ()))))
    then Left "multi-clause definitions are outside the typed pipeline subset"
    else case reverse defs of
      (("main", [], body) : restRev) -> do
        let others = reverse restRev
        Right
          ( foldr (\(n, ps, b) acc -> ELet n (foldr ELam b ps) acc) body others,
            ignored
          )
      _ -> Left "expected a 0-parameter 'main' as the last definition"
  where
    oneClause (n, ps, g, b) = do
      case g of
        Just _ -> Left ("guards are outside the typed pipeline subset (in '" ++ n ++ "')")
        Nothing -> pure ()
      names <-
        mapM
          ( \case
              PVar v -> Right v
              _ -> Left ("only plain-name parameters in the typed subset (in '" ++ n ++ "')")
          )
          ps
      b' <- conv b
      Right (n, names, b')

-- ----------------------------------------------------------------------------
-- Staged runner with rejection reporting (type errors etc. are thrown lazily
-- from pure code; force the rendered output to surface them per stage)
-- ----------------------------------------------------------------------------

forced :: String -> IO (Either String String)
forced s =
  fmap
    (either (Left . trim . displayException) Right)
    (try (evaluate (length s) >> pure s) :: IO (Either SomeException String))
  where
    trim = takeWhile (/= '\n') -- ErrorCall includes a callstack; keep line 1

banner :: String -> IO ()
banner s = putStrLn $ "\n" ++ replicate 76 '=' ++ "\n" ++ s ++ "\n" ++ replicate 76 '='

compileFile :: FilePath -> IO ()
compileFile path = do
  src <- readFile path
  banner ("FILE: " ++ path)
  case parse program path src of
    Left err -> putStrLn (errorBundlePretty err)
    Right tops -> case convProgram tops of
      Left err -> putStrLn ("REJECTED (subset conversion): " ++ err)
      Right (expr, notes) -> do
        mapM_ (putStrLn . ("note: " ++)) notes
        run expr

run :: Expr -> IO ()
run expr = do
  -- stage 2
  s2 <-
    forced $
      let (core2, mainTy, dlog) = stage2 expr
       in unlines $
            ["definition-time types (snapshotted before use sites constrain them):"]
              ++ ["  " ++ n ++ " : " ++ pT t | (n, t) <- dlog]
              ++ ["", "main : " ++ pT mainTy, "", "typed core (zonked):", pCore 0 core2]
  case s2 of
    Left err -> putStrLn ("\n-- STAGE 2 (HM + rows): REJECTED: " ++ err)
    Right out2 -> do
      putStrLn "\n-- STAGE 2: HM inference + row polymorphism"
      putStrLn out2
      let (core2, _, _) = stage2 expr
          (lenses, core3) = stage3 core2
      s3 <-
        forced $
          unlines $
            ["generated lens definitions (raw proj/update live only here):"]
              ++ concat
                [ ["  " ++ n ++ " : " ++ pT t, "  " ++ n ++ " = " ++ pCore 2 c]
                  | (n, c, t) <- lenses
                ]
              ++ ["", "core with r.f / {r | f = e} rewritten to lens calls:", pCore 0 core3]
      case s3 of
        Left err -> putStrLn ("\n-- STAGE 3 (lenses): REJECTED: " ++ err)
        Right out3 -> do
          putStrLn "-- STAGE 3: type-shape-driven codegen (getter + setter lenses)"
          putStrLn out3
          putStrLn "-- STAGE 4: linearity checking (structured core, before flattening)"
          case stage4 lenses core3 of
            Left err -> putStrLn ("REJECTED: " ++ err ++ "\n\n(pipeline stops here)")
            Right oks -> do
              mapM_ putStrLn ("linearity OK:" : oks)
              s5 <-
                forced $
                  let (fdefs, fmain) = stage5 lenses core3
                   in unlines (map pFDef fdefs ++ ["main =", "  " ++ pFlat 2 fmain])
              case s5 of
                Left err -> putStrLn ("\n-- STAGE 5 (flatten): REJECTED: " ++ err ++ "\n\n(pipeline stops here)")
                Right out5 -> do
                  putStrLn "\n-- STAGE 5: flatten (lambda lift, records -> tuples)"
                  putStrLn out5
                  let (fdefs, fmain) = stage5 lenses core3
                      (adefs, amain) = stage6 fdefs fmain
                  putStrLn "-- STAGE 6: ANF"
                  mapM_ (putStrLn . (++ "\n") . pADef) adefs
                  putStrLn "main =" >> putStrLn (pANF 2 amain)
                  let (adefs7, amain7) = stage7 adefs amain
                  putStrLn "\n-- STAGE 7: fusion + copy propagation + DCE"
                  mapM_ (putStrLn . (++ "\n") . pADef) adefs7
                  putStrLn "main =" >> putStrLn (pANF 2 amain7)
                  let (rcdefs, rcmain) = stage8 adefs7 amain7
                  putStrLn "\n-- STAGE 8: RC insertion (Perceus dup/drop) -- final IR before codegen"
                  mapM_
                    ( \(n, ps, b) -> do
                        putStrLn $
                          n
                            ++ "("
                            ++ intercalate ", " [p ++ " : " ++ pT t | (p, t) <- ps]
                            ++ ") ="
                        putStrLn (pRC 2 b)
                        putStrLn ""
                    )
                    rcdefs
                  putStrLn "main =" >> putStrLn (pRC 2 rcmain)

main args = do
  mapM_ compileFile (if null args then ["demo.sol"] else [args])
