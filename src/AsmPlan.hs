{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- AsmPlan: simplified end-to-end assembly-sequence planner demo.
--
-- Pipeline (each layer from the design discussion):
--   1. Part library + Feature/Joint model  (what can connect to what)
--   2. Burial DAG                          (joint precedence from buried features)
--   3. Greedy topo-sort with staging check (space-aware instruction ordering)
--   4. Delivery feasibility                (does the product fit through the door;
--                                           if not, defer joints to after the move)
--   5. Rendered instruction sequence
--
-- Run:  runghc AsmPlan.hs

module AsmPlan where

import Data.List (partition, sortOn)
import qualified Data.Map.Strict as M

-- ---------------------------------------------------------------------------
-- 1. Part library + Feature/Joint model
-- ---------------------------------------------------------------------------

type Mm = Int

data V3 = V3 {vx, vy, vz :: Mm} deriving (Eq, Show)

data PartKind
  = Extrusion2020 Mm -- length
  | CornerBracket Int -- angle in degrees (45 / 90 / 135)
  | PanelBracket -- Jasen's panel-mount bracket
  | Panel Mm Mm -- w x d, 6mm thick
  deriving (Eq, Show)

data Part = Part
  { pId :: String,
    pKind :: PartKind
  }
  deriving (Eq, Show)

bboxOf :: PartKind -> V3
bboxOf (Extrusion2020 l) = V3 l 20 20
bboxOf (CornerBracket _) = V3 20 20 20
bboxOf PanelBracket = V3 30 20 20
bboxOf (Panel w d) = V3 w d 6

-- A named mating feature on a part (simplified to a label; a real version
-- carries local position + normal + compatibility rules).
data Feature = Feature {fPart :: String, fName :: String}
  deriving (Eq, Ord, Show)

data Joint = Joint
  { jId :: String,
    jParts :: [String], -- part ids united by this joint
    jUses :: [Feature], -- features this joint needs access to
    jBuries :: [Feature], -- features inaccessible after this joint
    jAfter :: [String], -- explicit structural prerequisites (joint ids)
    jDesc :: String, -- human instruction text
    jGravity :: Bool -- True = gravity-assisted in natural orientation
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- 2. Burial DAG: joint A must precede joint B if B uses a feature A buries.
-- ---------------------------------------------------------------------------

-- edges: (before, after)
burialEdges :: [Joint] -> [(String, String)]
burialEdges js =
  [ (jId user, jId burier)
    | burier <- js,
      user <- js,
      jId burier /= jId user,
      any (`elem` jBuries burier) (jUses user)
  ]

structuralEdges :: [Joint] -> [(String, String)]
structuralEdges js = [(pre, jId j) | j <- js, pre <- jAfter j]

allEdges :: [Joint] -> [(String, String)]
allEdges js = burialEdges js ++ structuralEdges js

predsOf :: [(String, String)] -> String -> [String]
predsOf es j = [a | (a, b) <- es, b == j]

-- ---------------------------------------------------------------------------
-- 3. Staging areas + space-aware greedy ordering
-- ---------------------------------------------------------------------------

data Area = Area
  { aId :: String,
    aFootprint :: (Mm, Mm)
  }
  deriving (Show)

fits :: (Mm, Mm) -> Area -> Bool
fits (w, d) Area {..} =
  let (aw, ad) = aFootprint
   in (w <= aw && d <= ad) || (d <= aw && w <= ad)

-- Union-find-lite: map part -> component id, plus component bbox (naive: sum
-- of footprints along x, max along y — crude but demonstrates the check).
type Components = M.Map String (String, V3)

componentBBoxAfter :: M.Map String Part -> Components -> Joint -> V3
componentBBoxAfter lib comps Joint {..} =
  let boxes =
        [ maybe (bboxOf . pKind $ lib M.! p) snd (M.lookup p comps)
          | p <- jParts
        ]
      -- toy composition rule: side-by-side along x for extrusion joins,
      -- stacked in z for panel mounting
      w = maximum (map vx boxes)
      d = sum (map vy boxes)
      h = maximum (map vz boxes)
   in V3 w d h

mergeComponents :: Components -> Joint -> V3 -> Components
mergeComponents comps Joint {..} box =
  let cid = jId
   in foldr (\p m -> M.insert p (cid, box) m) comps jParts

-- Pick next joint from the ready set: prefer gravity-assisted, then smallest
-- resulting bbox (keeps work on the bench as long as possible).
data Placed = Placed {plJoint :: Joint, plArea :: String, plBox :: V3}

planOrder :: M.Map String Part -> [Area] -> [Joint] -> Either String [Placed]
planOrder lib areas js = go js M.empty []
  where
    edges = allEdges js
    go [] _ acc = Right (reverse acc)
    go pending comps acc =
      let done = map (jId . plJoint) acc
          ready = [j | j <- pending, all (`elem` done) (predsOf edges (jId j))]
       in case ready of
            [] -> Left "cycle in burial constraints (a needed face is buried by a prerequisite)"
            _ ->
              let scored = sortOn score ready
                  score j =
                    ( not (jGravity j),
                      let V3 w d _ = componentBBoxAfter lib comps j in w * d
                    )
                  j = head scored
                  box = componentBBoxAfter lib comps j
                  V3 w d _ = box
                  area = case filter (fits (w, d)) areas of
                    (a : _) -> aId a
                    [] -> "!! NO AREA FITS !!"
               in go
                    (filter ((/= jId j) . jId) pending)
                    (mergeComponents comps j box)
                    (Placed j area box : acc)

-- ---------------------------------------------------------------------------
-- 4. Delivery feasibility: final bbox vs doorway; propose deferring joints.
-- ---------------------------------------------------------------------------

data Doorway = Doorway {dwId :: String, dwW, dwH :: Mm} deriving (Show)

-- Can the box pass the doorway in some axis-aligned orientation?
passes :: V3 -> Doorway -> Bool
passes (V3 x y z) Doorway {..} =
  any
    (\(a, b) -> a <= dwW && b <= dwH)
    [(x, y), (y, x), (x, z), (z, x), (y, z), (z, y)]

-- Joints tagged deferrable (e.g. legs) can be moved to after transport.
deliveryPlan :: V3 -> [(Joint, V3)] -> Doorway -> ([String], [String])
deliveryPlan finalBox deferrable dw
  | finalBox `passes` dw = ([], [])
  | otherwise =
      let (ok, _) = partition (\(_, boxWithout) -> boxWithout `passes` dw) deferrable
       in ( map (jId . fst) ok,
            if null ok
              then ["Product cannot pass " ++ dwId dw ++ " and no deferrable joints fix it."]
              else []
          )

-- ---------------------------------------------------------------------------
-- 5. Demo model: the desk (2 quadrants for brevity), panels, legs
-- ---------------------------------------------------------------------------

lib :: M.Map String Part
lib = M.fromList [(pId p, p) | p <- ps]
  where
    ps =
      [ Part "ext-A1" (Extrusion2020 600),
        Part "ext-A2" (Extrusion2020 600),
        Part "ext-B1" (Extrusion2020 600),
        Part "ext-B2" (Extrusion2020 600),
        Part "cb-1" (CornerBracket 90),
        Part "cb-2" (CornerBracket 90),
        Part "cb-3" (CornerBracket 90),
        Part "pb-1" PanelBracket,
        Part "pb-2" PanelBracket,
        Part "panel-1" (Panel 600 600),
        Part "panel-2" (Panel 600 600),
        Part "leg-1" (Extrusion2020 700),
        Part "leg-2" (Extrusion2020 700),
        Part "leg-3" (Extrusion2020 700),
        Part "leg-4" (Extrusion2020 700)
      ]

topFace, cornerNE :: String -> Feature
topFace p = Feature p "top-slot"
cornerNE p = Feature p "corner"

joints :: [Joint]
joints =
  [ Joint
      "J1-quadA"
      ["ext-A1", "ext-A2", "cb-1"]
      [cornerNE "ext-A1"]
      []
      []
      "Build quadrant A: join ext-A1 + ext-A2 with 90-deg bracket cb-1 (flat on bench, brackets up)."
      True,
    Joint
      "J2-quadB"
      ["ext-B1", "ext-B2", "cb-2"]
      [cornerNE "ext-B1"]
      []
      []
      "Build quadrant B: join ext-B1 + ext-B2 with 90-deg bracket cb-2 (flat on bench, brackets up)."
      True,
    Joint
      "J3-frame"
      ["ext-A1", "ext-B1", "cb-3"]
      [cornerNE "ext-A1", cornerNE "ext-B1"]
      []
      ["J1-quadA", "J2-quadB"]
      "Join quadrant A to quadrant B along shared rail with bracket cb-3. Check diagonals, then torque."
      True,
    Joint
      "J4-panels"
      ["ext-A1", "panel-1", "panel-2", "pb-1", "pb-2"]
      []
      [topFace "ext-A1", topFace "ext-B1"] -- burying the frame top face!
      ["J3-frame"]
      "Bolt panels onto frame top slots using panel brackets pb-1/pb-2. (Buries frame top face.)"
      True,
    Joint
      "J5-square"
      ["ext-A1"]
      [topFace "ext-A1"]
      [] -- needs the face J4 buries
      ["J3-frame"]
      "Straightedge squareness check across bare frame top rails; shim/adjust corners."
      True,
    Joint
      "J6-legs"
      ["ext-A1", "leg-1", "leg-2", "leg-3", "leg-4"]
      []
      []
      ["J4-panels"]
      "Invert frame, bolt 4 legs pointing up at corners, single final flip."
      False
  ]

areas :: [Area]
areas = [Area "bench" (900, 600), Area "floor-zone" (2000, 2000)]

door :: Doorway
door = Doorway "office-door" 690 2040 -- narrow door: with-legs desk won't pass

main :: IO ()
main = do
  putStrLn "== AsmPlan: desk build =="
  putStrLn ""
  putStrLn "-- Burial constraints (must-precede edges) --"
  mapM_ (\(a, b) -> putStrLn $ "  " ++ a ++ "  before  " ++ b) (allEdges joints)
  putStrLn ""
  case planOrder lib areas joints of
    Left err -> putStrLn $ "PLANNING FAILED: " ++ err
    Right steps -> do
      putStrLn "-- Instruction sequence --"
      mapM_ render (zip [1 :: Int ..] steps)
      putStrLn ""
      -- Delivery: final desk bbox with legs vs without legs
      let deskWithLegs = V3 1200 700 720
          deskWithoutLegs = V3 1200 700 46
          (defer, errs) = deliveryPlan deskWithLegs [(last joints, deskWithoutLegs)] door
      putStrLn "-- Delivery feasibility --"
      putStrLn $
        "  Final bbox "
          ++ showV3 deskWithLegs
          ++ " vs "
          ++ dwId door
          ++ " ("
          ++ show (dwW door)
          ++ "x"
          ++ show (dwH door)
          ++ "): "
          ++ (if deskWithLegs `passes` door then "PASSES" else "BLOCKED")
      mapM_ (putStrLn . ("  " ++)) errs
      mapM_
        ( \j ->
            putStrLn $
              "  -> Defer joint "
                ++ j
                ++ " until after transport (frame-only bbox "
                ++ showV3 deskWithoutLegs
                ++ " passes)."
        )
        defer
  where
    render (n, Placed {..}) =
      putStrLn $
        "  Step "
          ++ show n
          ++ " ["
          ++ plArea
          ++ ", bbox "
          ++ showV3 plBox
          ++ "]"
          ++ (if jGravity plJoint then "" else " (against gravity: invert first)")
          ++ "\n    "
          ++ jDesc plJoint
    showV3 (V3 x y z) = show x ++ "x" ++ show y ++ "x" ++ show z
