{-# OPTIONS_GHC -Wno-missing-export-lists #-}
-- Demonstrates producer/consumer fusion in ANF form. In ANF every intermediate collection is a named let-binding, making producer-consumer edges trivially visible:

--   let xs = map f input     -- producer
--       ys = map g xs        -- consumer of xs (xs used exactly once -> fuse)

-- The pass walks the let-chain collecting a "pipeline context" (name -> Pipeline). When a binding is consumed by a single downstream combinator, it is fused in and the intermediate binding is erased. When used more than once, it must materialise.

--   FUSED:   map.map, filter.map, map.filter, foldl.map, foldl.filter, take.map, range as source, long chains
--   BLOCKED: use-count > 1, zip with opaque input, opaque function call
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Fusion where

import Data.Map (Map)
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- ANF IR
--------------------------------------------------------------------------------

type Name = String

data Atom = AVar Name | ALit Int deriving (Eq, Ord)

instance Show Atom where
  show (AVar x) = x
  show (ALit n) = show n

data Rhs = RCall Name [Atom] | RAtom Atom deriving (Eq)

instance Show Rhs where
  show (RCall f args) = unwords (f : map show args)
  show (RAtom a) = show a

data Binding = Binding Name Rhs deriving (Eq)

data Program = Program [Binding] Atom deriving (Eq)

ppProgram :: Program -> String
ppProgram (Program binds result) = concatMap ppB binds ++ show result where ppB (Binding x r) = "let " ++ x ++ " = " ++ show r ++ "\n"

--------------------------------------------------------------------------------
-- Pipeline IR
--------------------------------------------------------------------------------

data Source = SVar Name | SRange Atom Atom deriving (Eq, Show)

data Stage = SMap Name | SFilter Name | STake Atom deriving (Eq, Show)

data Terminal = TToList | TFoldl Name Atom deriving (Eq, Show)

data Pipeline = Pipeline {plSource :: Source, plStages :: [Stage], plTerminal :: Terminal} deriving (Eq, Show)

ppPipeline :: Pipeline -> String
ppPipeline (Pipeline src stages term) = ppSrc src ++ concatMap ppStage stages ++ ppTerm term
  where
    ppSrc (SVar n) = n
    ppSrc (SRange lo hi) = "range(" ++ show lo ++ ".." ++ show hi ++ ")"
    ppStage (SMap f) = " |> map(" ++ f ++ ")"
    ppStage (SFilter p) = " |> filter(" ++ p ++ ")"
    ppStage (STake n) = " |> take(" ++ show n ++ ")"
    ppTerm TToList = ""
    ppTerm (TFoldl step z) = " |> foldl(" ++ step ++ ", " ++ show z ++ ")"

--------------------------------------------------------------------------------
-- Use-count: how many times does 'x' appear in the remaining bindings
--------------------------------------------------------------------------------

atomUses :: Name -> Atom -> Int
atomUses x (AVar y) = if x == y then 1 else 0
atomUses _ _ = 0

rhsUses :: Name -> Rhs -> Int
rhsUses x (RAtom a) = atomUses x a
rhsUses x (RCall _ args) = sum (map (atomUses x) args)

usesAfter :: Name -> [Binding] -> Atom -> Int
usesAfter x bs result = sum [rhsUses x r | Binding _ r <- bs] + atomUses x result

--------------------------------------------------------------------------------
-- Recognise a Rhs as a pipeline node
--------------------------------------------------------------------------------

type FusionCtx = Map Name Pipeline

recognise :: FusionCtx -> Rhs -> Maybe Pipeline
recognise _ (RCall "range" [lo, hi]) = Just $ Pipeline (SRange lo hi) [] TToList
recognise ctx (RCall "map" [AVar f, AVar xs]) = let base = Map.findWithDefault (Pipeline (SVar xs) [] TToList) xs ctx in Just base {plStages = plStages base ++ [SMap f]}
recognise ctx (RCall "filter" [AVar p, AVar xs]) = let base = Map.findWithDefault (Pipeline (SVar xs) [] TToList) xs ctx in Just base {plStages = plStages base ++ [SFilter p]}
recognise ctx (RCall "take" [n, AVar xs]) = let base = Map.findWithDefault (Pipeline (SVar xs) [] TToList) xs ctx in Just base {plStages = plStages base ++ [STake n]}
recognise ctx (RCall "foldl" [AVar step, z, AVar xs]) = let base = Map.findWithDefault (Pipeline (SVar xs) [] TToList) xs ctx in Just base {plTerminal = TFoldl step z}
recognise _ _ = Nothing

--------------------------------------------------------------------------------
-- Fusion notes
--------------------------------------------------------------------------------

data Note
  = NFused String String -- what fused, resulting pipeline string
  | NBlocked String String -- reason, binding description
  | NThrough String -- passed through unchanged
  | NZip String -- zip handling
  deriving (Eq)

instance Show Note where
  show (NFused d p) = "[FUSED]   " ++ d ++ "\n              => " ++ p
  show (NBlocked r n) = "[BLOCKED] " ++ n ++ "\n              reason: " ++ r
  show (NThrough s) = "[THROUGH] " ++ s
  show (NZip s) = "[ZIP]     " ++ s

--------------------------------------------------------------------------------
-- Fusion pass: left-to-right over flat binding list
--------------------------------------------------------------------------------

fusionPass :: Program -> (Program, [Note])
fusionPass (Program binds result) = let (outBinds, notes) = go binds result Map.empty [] [] in (Program (reverse outBinds) result, reverse notes)
  where
    go :: [Binding] -> Atom -> FusionCtx -> [Binding] -> [Note] -> ([Binding], [Note])
    go [] _ _ out notes = (out, notes)
    go (Binding x rhs : rest) result ctx out notes =
      let uc = usesAfter x rest result
       in case () of
            -- ── zip special case ──────────────────────────────────────────────────
            _ | RCall "zip" [AVar a, AVar b] <- rhs -> case (Map.lookup a ctx, Map.lookup b ctx) of
              (Just plA, Just plB) ->
                -- Both inputs are fused pipelines: emit a zipFused node. In a real backend this becomes a paired-pull loop.
                let outRhs = RCall "zipFused" [AVar ("(" ++ ppPipeline plA ++ ")"), AVar ("(" ++ ppPipeline plB ++ ")")]
                    note = NZip $ "both inputs fused:\n" ++ "                  L: " ++ ppPipeline plA ++ "\n                  R: " ++ ppPipeline plB
                    -- Remove consumed pipeline entries from ctx
                    ctx' = Map.delete b (Map.delete a ctx)
                 in go rest result ctx' (Binding x outRhs : out) (note : notes)
              (Just _, Nothing) -> let note = NBlocked "right input of zip is opaque" (x ++ " = " ++ show rhs) in go rest result ctx (Binding x rhs : out) (note : notes)
              (Nothing, Just _) -> let note = NBlocked "left input of zip is opaque" (x ++ " = " ++ show rhs) in go rest result ctx (Binding x rhs : out) (note : notes)
              _ -> let note = NThrough (x ++ " = " ++ show rhs ++ "  (both opaque)") in go rest result ctx (Binding x rhs : out) (note : notes)
            -- ── recognised pipeline node ──────────────────────────────────────────
            _ | Just pl <- recognise ctx rhs ->
                  -- Dead: drop the binding entirely
                  if uc == 0 then
                      let note = NBlocked "dead (use-count=0), dropped" (x ++ " = " ++ show rhs) in go rest result ctx out (note : notes)
                  else
                      -- Safe to fuse: suppress binding, record in ctx
                      if uc == 1 then
                          let note = NFused (x ++ " = " ++ show rhs) (ppPipeline pl)
                              ctx' = Map.insert x pl ctx
                           in go rest result ctx' out (note : notes)
                      -- Must materialise: emit as a pipeline{...} call so it's clear. A single fused loop runs but the result list exists in memory
                      else
                          let note = NBlocked ("use-count=" ++ show uc ++ ", must materialise as list") (x ++ " = " ++ show rhs)
                              outRhs = RCall ("pipeline{" ++ ppPipeline pl ++ "}") []
                           in go rest result ctx (Binding x outRhs : out) (note : notes)
            -- ── opaque: pass through unchanged ───────────────────────────────────
            _ -> let note = NThrough (x ++ " = " ++ show rhs) in go rest result ctx (Binding x rhs : out) (note : notes)

--------------------------------------------------------------------------------
-- Builders
--------------------------------------------------------------------------------

p :: [Binding] -> Atom -> Program
p = Program

b :: Name -> Rhs -> Binding
b = Binding

call :: Name -> [Atom] -> Rhs
call = RCall

v :: Name -> Atom
v = AVar

i :: Int -> Atom
i = ALit

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------

printExample :: String -> Program -> IO ()
printExample label prog = do
  putStrLn $ replicate 66 '='
  putStrLn $ "EXAMPLE: " ++ label
  putStrLn $ replicate 66 '-'
  putStrLn "Input ANF:"
  mapM_ (putStrLn . ("  " ++)) (lines (ppProgram prog))
  putStrLn $ replicate 66 '-'
  let (outProg, notes) = fusionPass prog
  putStrLn "Fusion log:"
  mapM_ (\n -> putStrLn ("  " ++ show n)) notes
  putStrLn ""
  putStrLn "Output ANF:"
  mapM_ (putStrLn . ("  " ++)) (lines (ppProgram outProg))
  putStrLn ""

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- 1. map . map . foldl — fully fused, no intermediates
  printExample "map |> map |> foldl  (fully fused)" $
    p
      [ b "a" (call "map" [v "f", v "input"]),
        b "b" (call "map" [v "g", v "a"]),
        b "r" (call "foldl" [v "h", i 0, v "b"])
      ]
      (v "r")

  -- 2. Long pipeline: range . map . filter . map . foldl
  printExample "range |> map |> filter |> map |> foldl  (fully fused)" $
    p
      [ b "r0" (call "range" [i 0, i 100]),
        b "r1" (call "map" [v "double", v "r0"]),
        b "r2" (call "filter" [v "isEven", v "r1"]),
        b "r3" (call "map" [v "addOne", v "r2"]),
        b "r4" (call "foldl" [v "sum", i 0, v "r3"])
      ]
      (v "r4")

  -- 3. take . map — list result (TToList), no fold terminal
  printExample "map |> take  (materialises once, no fold)" $
    p
      [ b "a" (call "map" [v "f", v "input"]),
        b "b" (call "take" [i 10, v "a"])
      ]
      (v "b")

  -- 4. BLOCKED: intermediate 'a' used by two consumers
  --    Both folds need the same list; it must exist in memory.
  printExample "BLOCKED: shared intermediate — two consumers" $
    p
      [ b "a" (call "map" [v "f", v "input"]),
        b "b" (call "foldl" [v "sum", i 0, v "a"]),
        b "c" (call "foldl" [v "product", i 1, v "a"])
      ]
      (v "b")

  -- 5. zip: both inputs are single-use pipelines -> fused zip
  --    In a real backend: paired pull-loop, no intermediate lists.
  printExample "zip: both inputs are fused pipelines  (zipFused)" $
    p
      [ b "a" (call "map" [v "f", v "xs"]),
        b "b" (call "map" [v "g", v "ys"]),
        b "ab" (call "zip" [v "a", v "b"]),
        b "r" (call "foldl" [v "h", i 0, v "ab"])
      ]
      (v "r")

  -- 6. zip: right input opaque -> blocked, left pipeline still noted
  printExample "zip: right input opaque  (BLOCKED)" $
    p
      [ b "a" (call "map" [v "f", v "xs"]),
        b "ab" (call "zip" [v "a", v "ys"]), -- ys is not a pipeline
        b "r" (call "foldl" [v "h", i 0, v "ab"])
      ]
      (v "r")

  -- 7. Opaque function breaks the chain
  --    'a' is fused (single use into mystery), but mystery is opaque.
  --    'b' starts a fresh pipeline from mystery's output.
  printExample "opaque call in the middle breaks chain" $
    p
      [ b "a" (call "map" [v "f", v "input"]),
        b "b" (call "mystery" [v "a"]), -- opaque
        b "c" (call "map" [v "g", v "b"]),
        b "r" (call "foldl" [v "h", i 0, v "c"])
      ]
      (v "r")

  -- 8. Dead intermediate dropped
  printExample "dead intermediate dropped entirely" $
    p
      [ b "a" (call "map" [v "f", v "input"]),
        b "dead" (call "map" [v "g", v "a"]), -- never used
        b "r" (call "foldl" [v "h", i 0, v "a"])
      ]
      (v "r")

  -- 9. Two independent pipelines — both fuse independently
  printExample "two independent pipelines" $
    p
      [ b "a" (call "map" [v "f", v "xs"]),
        b "ra" (call "foldl" [v "sum", i 0, v "a"]),
        b "b" (call "map" [v "g", v "ys"]),
        b "b2" (call "map" [v "h", v "b"]),
        b "rb" (call "foldl" [v "product", i 1, v "b2"])
      ]
      (v "ra")

  -- 10. Shared map prefix, two downstream consumers
  --     The map can't fuse into either fold because it feeds both.
  --     Result: map materialises once, both folds see an opaque list.
  printExample "shared map prefix — two folds  (BLOCKED on map)" $
    p
      [ b "mapped" (call "map" [v "f", v "input"]),
        b "total" (call "foldl" [v "sum", i 0, v "mapped"]),
        b "count" (call "foldl" [v "countIf", i 0, v "mapped"])
      ]
      (v "total")
