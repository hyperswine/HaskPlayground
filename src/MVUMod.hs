{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- gen_view MVU simulation
--
-- Models the proposed architecture:
--   * A gen_view DSL where dynamic holes are lens-like projections (m -> a).
--     (Stands in for Sol's `:model.field` atom-lens syntax; here the "compile
--     time verification" is just Haskell's type checker on record selectors.)
--   * COMPILE: split the view into a Static template (shipped to client once,
--     including *all* Cond branch templates and the single ForEach item
--     template) and an extraction function Model -> Dyn.
--   * RUNTIME: server extracts Dyn per update, diffs against previous Dyn,
--     ships a Patch pruned to only changed parts.
--   * Patch keeps ForEach structural moves (key order) ORTHOGONAL to per-row
--     content patches, and recurses uniformly (a hole can hold a scalar, a
--     Cond tag+branch-dyn, or a nested keyed collection).
--   * CLIENT: holds Static + Dyn, applies patches, renders. Invariant checked:
--     patched client Dyn == fresh server extraction, at every step.

module MVUMod where

import Data.Bits (shiftR, xor)
import Data.List (foldl', intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

type Key = Int

--------------------------------------------------------------------------------
-- 1. The gen_view DSL
--------------------------------------------------------------------------------

data View m
  = VStack [View m]
  | TextS String -- static text
  | TextD (m -> String) -- dynamic hole  (:model.field)
  | Button String [View m] -- static attr (msg name) + kids
  | Cond (m -> Int) [View m] -- branch selector + ALL branches
  | forall i. ForEach (m -> [(Key, i)]) (View i) -- keyed rows, one item template

--------------------------------------------------------------------------------
-- 2. COMPILE TIME: the static template (this is what ships to the client once)
--------------------------------------------------------------------------------

data Static
  = SStack [Static]
  | SText String
  | SHole -- a dynamic scalar slot
  | SButton String [Static]
  | SCond [Static] -- every branch's template, shipped up front
  | SForEach Static -- the single item template
  deriving (Eq, Show)

staticOf :: View m -> Static
staticOf = \case
  VStack ks -> SStack (map staticOf ks)
  TextS s -> SText s
  TextD _ -> SHole
  Button n ks -> SButton n (map staticOf ks)
  Cond _ bs -> SCond (map staticOf bs)
  ForEach _ t -> SForEach (staticOf t)

--------------------------------------------------------------------------------
-- 3. RUNTIME VALUES: the dynamics tree (parallel to Static, DNone = no holes)
--------------------------------------------------------------------------------

data Dyn
  = DStack [Dyn]
  | DNone -- pure-static subtree: nothing ever shipped
  | DScalar String
  | DCond Int Dyn -- active branch tag + that branch's dynamics
  | DRows [(Key, Dyn)] -- keyed rows, order-significant
  deriving (Eq, Show)

extract :: View m -> m -> Dyn
extract v m = case v of
  VStack ks -> DStack [extract k m | k <- ks]
  TextS _ -> DNone
  TextD f -> DScalar (f m)
  Button _ ks -> DStack [extract k m | k <- ks]
  Cond sel bs -> let t = sel m in DCond t (extract (bs !! t) m)
  ForEach f t -> DRows [(k, extract t i) | (k, i) <- f m]

--------------------------------------------------------------------------------
-- 4. THE PATCH: pruned tree, moves orthogonal to content
--------------------------------------------------------------------------------

data Patch
  = PStack [(Int, Patch)] -- sparse: only changed children, by index
  | PScalar String
  | PSwitch Int Dyn -- Cond: new tag + full dynamics of new branch
  | PInner Patch -- Cond: same tag, patch inside active branch
  | PRows RowsPatch
  deriving (Eq, Show)

data RowsPatch = RowsPatch
  { rpOrder :: Maybe [Key], -- STRUCTURAL: new key order (Nothing = unchanged;
  --   implies inserts/deletes/moves)
    rpInserted :: [(Key, Dyn)], -- full dynamics for rows the client hasn't seen
    rpChanged :: [(Key, Patch)] -- CONTENT: per-surviving-row patches, by key
  }
  deriving (Eq, Show)

-- diff old new = Nothing  <=>  nothing to send
diff :: Dyn -> Dyn -> Maybe Patch
diff old new = case (old, new) of
  (DNone, DNone) -> Nothing
  (DScalar a, DScalar b)
    | a == b -> Nothing
    | otherwise -> Just (PScalar b)
  (DStack as, DStack bs) ->
    let ps = [(i, p) | (i, a, b) <- zip3 [0 ..] as bs, Just p <- [diff a b]]
     in if null ps then Nothing else Just (PStack ps)
  (DCond t1 d1, DCond t2 d2)
    | t1 /= t2 -> Just (PSwitch t2 d2) -- branch swap: ship branch dyns
    | otherwise -> PInner <$> diff d1 d2 -- same branch: recurse
  (DRows as, DRows bs) ->
    let oldKeys = map fst as
        newKeys = map fst bs
        oldMap = M.fromList as
        order = if oldKeys == newKeys then Nothing else Just newKeys
        ins = [(k, d) | (k, d) <- bs, not (M.member k oldMap)]
        chg =
          [ (k, p) | (k, d) <- bs, Just o <- [M.lookup k oldMap], Just p <- [diff o d]
          ]
     in case (order, ins, chg) of
          (Nothing, [], []) -> Nothing
          _ -> Just (PRows (RowsPatch order ins chg))
  _ -> error "static shape mismatch: compile-time bug"

applyPatch :: Dyn -> Patch -> Dyn
applyPatch d p = case (d, p) of
  (DScalar _, PScalar s) -> DScalar s
  (DStack ks, PStack ps) ->
    let pm = M.fromList ps
     in DStack [maybe k (applyPatch k) (M.lookup i pm) | (i, k) <- zip [0 ..] ks]
  (DCond _ _, PSwitch t nd) -> DCond t nd
  (DCond t bd, PInner ip) -> DCond t (applyPatch bd ip)
  (DRows rs, PRows rp) ->
    let base = M.fromList rs
        patched =
          foldl'
            (\m (k, q) -> M.adjust (`applyPatch` q) k m)
            base
            (rpChanged rp)
        withIns =
          foldl'
            (\m (k, nd) -> M.insert k nd m)
            patched
            (rpInserted rp)
        order = fromMaybe (map fst rs) (rpOrder rp)
     in DRows [(k, withIns M.! k) | k <- order]
  _ -> error "patch shape mismatch"

--------------------------------------------------------------------------------
-- 5. CLIENT-SIDE RENDER: Static + Dyn -> output (fake HTML)
--------------------------------------------------------------------------------

render :: Static -> Dyn -> String
render s d = case (s, d) of
  (SText t, _) -> t
  (SHole, DScalar v) -> "{" ++ v ++ "}"
  (SStack ks, DStack ds) -> "<v>" ++ concatMap (uncurry render) (zip ks ds) ++ "</v>"
  (SStack ks, DNone) -> "<v>" ++ concatMap (`render` DNone) ks ++ "</v>"
  (SButton n ks, DStack ds) ->
    "<btn:"
      ++ n
      ++ ">"
      ++ concatMap (uncurry render) (zip ks ds)
      ++ "</btn>"
  (SButton n ks, DNone) ->
    "<btn:"
      ++ n
      ++ ">"
      ++ concatMap (`render` DNone) ks
      ++ "</btn>"
  (SCond bs, DCond t bd) -> render (bs !! t) bd
  (SForEach t, DRows rs) ->
    "<ul>"
      ++ concat
        [ "<li#"
            ++ show k
            ++ ">"
            ++ render t rd
            ++ "</li>"
          | (k, rd) <- rs
        ]
      ++ "</ul>"
  (_, DNone) -> case s of
    SText t -> t
    _ -> error "DNone under dynamic template"
  _ -> error "render shape mismatch"

--------------------------------------------------------------------------------
-- 6. Size accounting (wire-cost proxy: count constructors + string chars)
--------------------------------------------------------------------------------

dynSize :: Dyn -> Int
dynSize = \case
  DStack ds -> 1 + sum (map dynSize ds)
  DNone -> 0
  DScalar s -> 1 + length s
  DCond _ d -> 1 + dynSize d
  DRows rs -> 1 + sum [1 + dynSize d | (_, d) <- rs]

patchSize :: Patch -> Int
patchSize = \case
  PStack ps -> 1 + sum [1 + patchSize p | (_, p) <- ps]
  PScalar s -> 1 + length s
  PSwitch _ d -> 1 + dynSize d
  PInner p -> 1 + patchSize p
  PRows rp ->
    1
      + maybe 0 length (rpOrder rp)
      + sum [1 + dynSize d | (_, d) <- rpInserted rp]
      + sum [1 + patchSize p | (_, p) <- rpChanged rp]

--------------------------------------------------------------------------------
-- 7. The example app: nested model, Cond, nested ForEach
--------------------------------------------------------------------------------

data Item = Item
  { itemId :: Key,
    label :: String,
    subs :: [(Key, String)] -- nested keyed collection
  }
  deriving (Eq, Show)

data Model = Model
  { name :: String,
    clicked :: Bool,
    items :: [Item]
  }
  deriving (Eq, Show)

-- gen_view = vstack {} [ text :model.name
--                      , button {onClick=Click}
--                          [Cond :model.clicked (text "Click Me") (text "Clicked")]
--                      , ForEach :model.items \item ->
--                          vstack {} [ text :item.label
--                                    , ForEach :item.subs \s -> text :s ] ]
appView :: View Model
appView =
  VStack
    [ TextD name,
      Button
        "Click"
        [ Cond
            (\m -> fromEnum (clicked m))
            [TextS "Click Me", TextS "Clicked"]
        ],
      ForEach
        (\m -> [(itemId i, i) | i <- items m])
        ( VStack
            [ TextD label,
              ForEach subsKeyed (TextD snd)
            ]
        )
    ]
  where
    subsKeyed i = [(k, (k, s)) | (k, s) <- subs i]

--------------------------------------------------------------------------------
-- 8. Server/client step with invariant checks
--------------------------------------------------------------------------------

data Client = Client {cDyn :: Dyn}

-- returns (newClient, description of what went over the wire)
step :: Static -> Model -> Client -> (Client, String)
step st newModel (Client oldDyn) =
  let newDyn = extract appView newModel
   in case diff oldDyn newDyn of
        Nothing -> (Client oldDyn, "  wire: NOTHING (0)")
        Just p ->
          let applied = applyPatch oldDyn p
              ok1 = applied == newDyn
              ok2 = render st applied == render st newDyn
           in if ok1 && ok2
                then
                  ( Client applied,
                    "  wire: patch size "
                      ++ show (patchSize p)
                      ++ " vs full "
                      ++ show (dynSize newDyn)
                      ++ "   "
                      ++ summarize p
                  )
                else
                  error
                    ( "INVARIANT VIOLATED:\n  patched: "
                        ++ show applied
                        ++ "\n  fresh:   "
                        ++ show newDyn
                    )

summarize :: Patch -> String
summarize = \case
  PScalar s -> "scalar=" ++ show s
  PSwitch t _ -> "cond-switch->" ++ show t
  PInner p -> "cond-inner(" ++ summarize p ++ ")"
  PStack ps ->
    "stack["
      ++ intercalate
        ", "
        [show i ++ ":" ++ summarize p | (i, p) <- ps]
      ++ "]"
  PRows rp ->
    "rows{order="
      ++ maybe "unchanged" show (rpOrder rp)
      ++ ", ins="
      ++ show (map fst (rpInserted rp))
      ++ ", chg=["
      ++ intercalate
        ", "
        [show k ++ ":" ++ summarize p | (k, p) <- rpChanged rp]
      ++ "]}"

--------------------------------------------------------------------------------
-- 9. Scenario walkthrough
--------------------------------------------------------------------------------

m0 :: Model
m0 =
  Model
    "Jasen"
    False
    [ Item 1 "alpha" [(10, "a1"), (11, "a2")],
      Item 2 "beta" [(20, "b1")],
      Item 3 "gamma" [(30, "c1"), (31, "c2"), (32, "c3")]
    ]

scenarios :: [(String, Model -> Model)]
scenarios =
  [ ("no-op update (nothing dirty)", id),
    ("scalar: rename model.name", \m -> m {name = "Jasen K"}),
    ("cond: toggle clicked (branch switch)", \m -> m {clicked = True}),
    ("rows: edit one row's label", editLabel 2 "BETA"),
    ("rows: reorder only (no content change)", \m -> m {items = rot (items m)}),
    ("rows: reorder + nested edit, orthogonal", \m -> m {items = rot (editSub 3 31 "C2!" <$> items m)}),
    ("rows: insert a row", \m -> m {items = items m ++ [Item 4 "delta" [(40, "d1")]]}),
    ("rows: delete a row", \m -> m {items = filter ((/= 1) . itemId) (items m)}),
    ("nested rows: insert a sub-item only", onItem 2 (\i -> i {subs = subs i ++ [(21, "b2")]}))
  ]
  where
    rot [] = []
    rot (x : xs) = xs ++ [x]
    editLabel k l m =
      m
        { items =
            [ if itemId i == k then i {label = l} else i
              | i <- items m
            ]
        }
    editSub k sk v i
      | itemId i == k = i {subs = [(s, if s == sk then v else t) | (s, t) <- subs i]}
      | otherwise = i
    onItem k f m = m {items = [if itemId i == k then f i else i | i <- items m]}

runScenarios :: IO ()
runScenarios = do
  let st = staticOf appView
      d0 = extract appView m0
  putStrLn "== STATIC TEMPLATE (shipped once, includes all Cond branches) =="
  print st
  putStrLn $ "\n== FIRST RENDER ==  full dyn size " ++ show (dynSize d0)
  putStrLn $ "  " ++ render st d0
  putStrLn "\n== INCREMENTAL UPDATES =="
  let go (m, c) (nm, f) = do
        let m' = f m
            (c', wire) = step st m' c
        putStrLn $ "* " ++ nm
        putStrLn wire
        putStrLn $ "  " ++ render st (cDyn c')
        pure (m', c')
  _ <- foldM go (m0, Client d0) scenarios
  pure ()
  where
    foldM f z (x : xs) = f z x >>= \z' -> foldM f z' xs
    foldM _ z [] = pure z

--------------------------------------------------------------------------------
-- 10. Randomized invariant check (hand-rolled PRNG; no external deps)
--------------------------------------------------------------------------------

-- splitmix-ish step
next :: Int -> (Int, Int)
next s =
  let s' = s + 0x9e3779b97f4a7c15
      z0 = s'
      z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
      z3 = z2 `xor` (z2 `shiftR` 31)
   in (abs z3, s')

rint :: Int -> Int -> (Int, Int) -- rint bound seed = (value in [0,bound), seed')
rint b s = let (v, s') = next s in (v `mod` max 1 b, s')

-- one random model mutation
mutate :: Int -> Model -> (Model, Int)
mutate s0 m =
  let (op, s1) = rint 8 s0
   in case op of
        0 -> let (n, s2) = rint 1000 s1 in (m {name = "user" ++ show n}, s2)
        1 -> (m {clicked = not (clicked m)}, s1)
        2
          | not (null (items m)) -> -- edit a label
              let (ix, s2) = rint (length (items m)) s1
                  (n, s3) = rint 1000 s2
               in ( m
                      { items =
                          [ if j == ix then i {label = "L" ++ show n} else i
                            | (j, i) <- zip [0 ..] (items m)
                          ]
                      },
                    s3
                  )
        3
          | length (items m) > 1 -> -- swap two rows
              let (a, s2) = rint (length (items m)) s1
                  (b, s3) = rint (length (items m)) s2
                  xs = items m
                  sw =
                    [ if j == a then xs !! b else if j == b then xs !! a else x
                      | (j, x) <- zip [0 ..] xs
                    ]
               in (m {items = sw}, s3)
        4 ->
          -- insert a row
          let (n, s2) = rint 100000 s1
              k = 1000 + n
           in (m {items = items m ++ [Item k ("new" ++ show n) [(k * 10, "s")]]}, s2)
        5
          | length (items m) > 1 -> -- delete a row
              let (ix, s2) = rint (length (items m)) s1
               in (m {items = [i | (j, i) <- zip [0 ..] (items m), j /= ix]}, s2)
        6
          | not (null (items m)) -> -- insert a sub-item
              let (ix, s2) = rint (length (items m)) s1
                  (n, s3) = rint 100000 s2
               in ( m
                      { items =
                          [ if j == ix
                              then i {subs = subs i ++ [(10000 + n, "sub" ++ show n)]}
                              else i
                            | (j, i) <- zip [0 ..] (items m)
                          ]
                      },
                    s3
                  )
        7
          | not (null (items m)) -> -- edit a sub-item
              let (ix, s2) = rint (length (items m)) s1
                  i = items m !! ix
               in if null (subs i)
                    then (m, s2)
                    else
                      let (sx, s3) = rint (length (subs i)) s2
                          (n, s4) = rint 1000 s3
                          i' =
                            i
                              { subs =
                                  [ (k, if j == sx then "e" ++ show n else v)
                                    | (j, (k, v)) <- zip [0 ..] (subs i)
                                  ]
                              }
                       in ( m
                              { items =
                                  [ if j == ix then i' else x
                                    | (j, x) <- zip [0 ..] (items m)
                                  ]
                              },
                            s4
                          )
        _ -> (m, s1)

randomCheck :: Int -> IO ()
randomCheck steps = do
  let loop 0 _ _ d acc = pure (d, acc)
      loop k s m d acc = do
        let (m', s') = mutate s m
            d' = extract appView m'
        case diff d d' of
          Nothing -> loop (k - 1) s' m' d' acc
          Just p -> do
            let applied = applyPatch d p
            if applied /= d'
              then
                error
                  ( "random check FAILED at step "
                      ++ show (steps - k)
                      ++ "\n model: "
                      ++ show m'
                  )
              else
                loop
                  (k - 1)
                  s'
                  m'
                  d'
                  ((patchSize p, dynSize d') : acc)
  (_, sizes) <- loop steps 42 m0 (extract appView m0) []
  let n = length sizes
      ps = sum (map fst sizes)
      fs = sum (map snd sizes)
  putStrLn $ "\n== RANDOM INVARIANT CHECK: " ++ show steps ++ " mutations =="
  putStrLn $ "  all " ++ show n ++ " non-trivial patches applied correctly \\o/"
  putStrLn $
    "  cumulative wire cost: patches "
      ++ show ps
      ++ " vs full re-sends "
      ++ show fs
      ++ "  ("
      ++ show
        ( round
            ( 100
                * fromIntegral ps
                / fromIntegral fs ::
                Double
            )
        )
      ++ "%)"

main :: IO ()
main = do
  runScenarios
  randomCheck 3000
