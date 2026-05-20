{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module TerraII.World where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Random (mkStdGen)
import TerraII.Types

-- Map layout (20 areas):
--  Row0: [0]BaseCamp [1]MtPearson [2]Ashfield  [3]NWastes
--  Row1: [4]RvPearson[5]PrsnDelta [6]CraggdPass[7]IronrkRdg
--  Row2: [8]NeoSea   [9]CntrlPlns [10]Rustfld  [11]EmpStage
--  Row3: [12]KelpRns [13]MidlndFst[14]TheRot   [15]HivePrime
--  Row4:             [16]Dustbowl  [17]ThfWarrn [18]SiltSwamp
--  Row5:                           [18]EastFrng (shared row w/19)

buildWorld :: Map AreaId Area
buildWorld = Map.fromList [(areaId a, a) | a <- allAreas]

allAreas :: [Area]
allAreas =
  [ mk
      0
      "Base Camp"
      Grassland
      [1, 4, 8]
      (Just NeoRepublic)
      3
      (r 3 1 1 0)
      0
      ["Fertile volcanic soil", "River Pearson access", "Ruined Rep.V outpost"],
    mk
      1
      "Mt. Pearson"
      Volcanic
      [0, 2, 5]
      Nothing
      2
      (r 2 2 0 1)
      1
      ["Dormant volcano", "Mineral-rich lava flows", "Sulphur vents"],
    mk
      2
      "Ashfield"
      Volcanic
      [1, 3, 6]
      Nothing
      2
      (r 1 2 0 0)
      2
      ["Ash-blanketed slopes", "Occasional tremors"],
    mk
      3
      "N. Wastes"
      Desert
      [2, 7]
      Nothing
      3
      (r 0 1 2 0)
      3
      ["Irradiated flats", "Rusted Rep.V hulks", "Extreme UV exposure"],
    mk
      4
      "Rv. Pearson"
      Riverland
      [0, 5, 8, 9]
      Nothing
      3
      (r 4 0 1 0)
      1
      ["Fresh water source", "Fishing stocks", "Soft riverbank soil"],
    mk
      5
      "Prsn Delta"
      Riverland
      [1, 4, 6, 9]
      Nothing
      3
      (r 3 1 1 0)
      1
      ["Fertile floodplain", "Ford crossing"],
    mk
      6
      "Cragd Pass"
      Mountains
      [2, 5, 7, 10]
      Nothing
      2
      (r 0 3 1 0)
      2
      ["Narrow mountain pass", "Defensible cliffs", "Old watchtower ruins"],
    mk
      7
      "Ironrk Rdg"
      Mountains
      [3, 6, 11]
      Nothing
      2
      (r 0 4 1 0)
      2
      ["Iron ore seams", "High vantage", "Rockfall hazards"],
    mk
      8
      "Neo Sea Shr"
      Coast
      [0, 4, 9, 12]
      Nothing
      3
      (r 2 1 0 0)
      1
      ["Neo Sea coastline", "Salvage from sunken vessels", "Thick fog banks"],
    mk
      9
      "Cntrl Plains"
      Grassland
      [4, 5, 8, 10, 13]
      Nothing
      4
      (r 2 1 1 0)
      1
      ["Open terrain", "Rep.V road network", "Exposed on all sides"],
    mk
      10
      "Rustfield"
      Industrial
      [6, 9, 11, 14]
      Nothing
      3
      (r 0 3 2 1)
      2
      ["Ruined Rep.V factory", "Scrap metal abundant", "Rogue robot activity"],
    mk
      11
      "Emp. Staging"
      Grassland
      [7, 10, 18]
      (Just SouthernEmpire)
      3
      (r 2 2 2 0)
      0
      ["Military depot", "Hardened bunkers", "Maximilian's banner flies here"],
    mk
      12
      "Kelp Ruins"
      Ruins
      [8, 13]
      Nothing
      2
      (r 1 2 0 2)
      2
      ["Submerged Rep.V ruins", "Strange energy readings", "Kelp-choked channels"],
    mk
      13
      "Midlnd Frst"
      Forest
      [9, 12, 14, 16]
      Nothing
      3
      (r 2 1 0 1)
      2
      ["Dense fungal forest", "Mutant animal packs", "Poor sight lines"],
    mk
      14
      "The Rot"
      Forest
      [10, 13, 15, 17]
      Nothing
      2
      (r 1 0 0 0)
      4
      ["Heavily contaminated", "Insect spore clouds", "Near-zero visibility"],
    mk
      15
      "Hive Prime"
      Forest
      [14, 18]
      (Just Insects)
      3
      (r 2 0 0 0)
      0
      ["Insect hive core", "Pheromone towers", "Constant chittering"],
    mk
      16
      "Dustbowl"
      Desert
      [13, 17]
      Nothing
      3
      (r 0 2 3 0)
      2
      ["Abandoned settlement", "High sandstorm frequency", "Old fuel caches"],
    mk
      17
      "Thief Warren"
      Ruins
      [14, 16, 18, 19]
      (Just Thieves)
      3
      (r 1 1 1 1)
      0
      ["Maze of tunnels", "Many bolt-holes", "Nothing is as it seems"],
    mk
      18
      "Siltmre Swp"
      Riverland
      [11, 15, 17, 19]
      Nothing
      2
      (r 2 0 0 0)
      3
      ["Thick black silt", "Sand-sludge entities", "Treacherous footing"],
    mk
      19
      "East Fringe"
      Desert
      [17, 18]
      Nothing
      3
      (r 0 2 2 0)
      2
      ["Edge of known territory", "Strange lights at night", "Binary star rises here"]
  ]

mk :: AreaId -> String -> AreaTerrain -> [AreaId] -> Maybe FactionId -> Int -> Resources -> Int -> [String] -> Area
mk aid nm ter adj owner cap yld haz feats = Area {areaId = aid, areaName = nm, areaTerrain = ter, areaAdj = adj, areaOwner = owner, areaBase = fmap mkBase owner, areaUnits = Map.empty, areaExplored = maybe Set.empty Set.singleton owner, areaCapacity = cap, areaYield = yld, areaHazard = haz, areaFeatures = feats} where mkBase fid = Base fid 1 30 30 3

r :: Int -> Int -> Int -> Int -> Resources
r f m fu rs = Resources f m fu rs

startFaction :: FactionId -> Faction
startFaction NeoRepublic = Faction NeoRepublic "Neo Republican Empire" "\ESC[94m" 'N' (Resources 20 15 10 5) Set.empty 70 "Commander Jake" False
startFaction SouthernEmpire = Faction SouthernEmpire "Southern Empire" "\ESC[91m" 'S' (Resources 15 20 15 0) (Set.fromList [CoilgunRifles]) 80 "Lt. Maximilian" False
startFaction Insects = Faction Insects "The Hiveform" "\ESC[92m" 'I' (Resources 30 5 5 0) (Set.fromList [MutantAnalysis]) 90 "The Overmind" False
startFaction Thieves = Faction Thieves "The Thieves" "\ESC[93m" 'T' (Resources 10 10 10 10) (Set.fromList [ReconDrones, TradeNetworks]) 65 "Analyst Jack" False

mkUnit :: UnitId -> UnitType -> FactionId -> String -> AreaId -> Unit
mkUnit uid ut owner nm loc = Unit uid ut owner nm (unitMaxHP ut) 0 Green loc False

startingUnits :: [(UnitId, Unit)]
startingUnits =
  [ (1, mkUnit 1 Recon NeoRepublic "Scout Alpha" 0),
    (2, mkUnit 2 Laborer NeoRepublic "Work Team 1" 0),
    (3, mkUnit 3 Militia NeoRepublic "Home Guard" 0),
    (4, mkUnit 4 LightInfantry SouthernEmpire "Iron Rifles" 11),
    (5, mkUnit 5 Militia SouthernEmpire "Vanguard" 11),
    (6, mkUnit 6 Militia SouthernEmpire "Shock Troop" 11),
    (7, mkUnit 7 Militia Insects "Brood Alpha" 15),
    (8, mkUnit 8 Militia Insects "Brood Beta" 15),
    (9, mkUnit 9 Recon Insects "Spore Scout" 15),
    (10, mkUnit 10 Recon Thieves "Shadow 1" 17),
    (11, mkUnit 11 Militia Thieves "Street Runner" 17),
    (12, mkUnit 12 Laborer Thieves "Scavenger" 17)
  ]

populateUnits :: Map AreaId Area -> Map UnitId Unit -> Map AreaId Area
populateUnits areas units = Map.foldl' add areas units
  where
    add acc u = Map.adjust (\a -> a {areaUnits = Map.insertWith (++) (unitOwner u) [unitId u] (areaUnits a)}) (unitLoc u) acc

initGame :: Int -> GameState
initGame seed =
  let areas = populateUnits buildWorld (Map.fromList startingUnits)
      units = Map.fromList startingUnits
      facs = Map.fromList [(f, startFaction f) | f <- [minBound .. maxBound]]
   in GameState {gsTurn = 1, gsAreas = areas, gsFacs = facs, gsUnits = units, gsEnv = Env ENormal 8 EUVHigh, gsLog = [LogEntry 0 Nothing "Welcome to Terra II. Only misery and war." LInfo], gsNextUID = 13, gsRng = mkStdGen seed, gsPhase = PlayerOrders, gsSel = SelNone, gsPlayer = NeoRepublic, gsOver = Nothing, gsMsg = Just intro}

intro :: String
intro =
  unlines
    [ "\ESC[1m\ESC[94mNEO REPUBLICAN BASE CAMP -- TURN 1\ESC[0m",
      "",
      "Population: ~5000 survivors. Resources scarce.",
      "Commander Jake surveys the horizon from the camp perimeter.",
      "To the north: Mt. Pearson, dormant but fertile.",
      "To the south: River Pearson. To the west: the Neo Sea.",
      "",
      "\ESC[93mIntel: Three rival factions are establishing themselves",
      "on this continent. Only one will remain.\ESC[0m",
      "",
      "Press \ESC[96m[N]\ESC[0m to cycle units  |  \ESC[96m[T]\ESC[0m to end turn  |  \ESC[96m[?]\ESC[0m for help"
    ]
