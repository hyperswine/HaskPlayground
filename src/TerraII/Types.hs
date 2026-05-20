module TerraII.Types where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (StdGen, mkStdGen)

-- Factions
data FactionId = NeoRepublic | SouthernEmpire | Insects | Thieves
  deriving (Show, Eq, Ord, Enum, Bounded)

data Faction = Faction
  { factionId        :: FactionId
  , factionName      :: String
  , factionFgColor   :: String   -- ANSI color code
  , factionSym       :: Char
  , factionResources :: Resources
  , factionTech      :: Set TechId
  , factionMorale    :: Int
  , factionLeader    :: String
  , factionDefeated  :: Bool
  } deriving (Show, Eq)

-- Resources
data Resources = Resources
  { resFood  :: Int
  , resMat   :: Int
  , resFuel  :: Int
  , resResearch :: Int
  } deriving (Show, Eq)

emptyRes :: Resources
emptyRes = Resources 0 0 0 0

addRes :: Resources -> Resources -> Resources
addRes a b = Resources (resFood a + resFood b) (resMat a + resMat b) (resFuel a + resFuel b) (resResearch a + resResearch b)

subRes :: Resources -> Resources -> Resources
subRes a b = Resources (resFood a - resFood b) (resMat a - resMat b) (resFuel a - resFuel b) (resResearch a - resResearch b)

canAfford :: Resources -> Resources -> Bool
canAfford have cost = resFood have >= resFood cost && resMat have >= resMat cost
                   && resFuel have >= resFuel cost && resResearch have >= resResearch cost

-- Tech
data TechId
  = BasicFarming | BasicMining | CoilgunRifles | RPGDesign
  | LightVehicles | ArmouredVehicles | RayWeapons | RailgunDesign
  | ReconDrones | FuelOpt | MutantAnalysis | TradeNetworks
  deriving (Show, Eq, Ord, Enum, Bounded)

data Tech = Tech
  { techId     :: TechId
  , techName   :: String
  , techDesc   :: String
  , techCost   :: Int
  , techPrereqs :: [TechId]
  , techEffect :: String
  } deriving (Show, Eq)

allTechs :: [Tech]
allTechs =
  [ Tech BasicFarming    "Basic Farming"       "Cultivate fertile soil."              80  []              "+2 food/turn on farm areas"
  , Tech BasicMining     "Basic Mining"        "Extract raw materials."               80  []              "+2 mat/turn on mine areas"
  , Tech CoilgunRifles   "Coilgun Rifles"      "EM propulsion infantry rifles."       120 []              "LightInf +2 attack"
  , Tech RPGDesign       "RPG Ordnance"        "Anti-armour rockets."                 150 [CoilgunRifles] "LightInf gains anti-armour"
  , Tech LightVehicles   "Light Vehicles"      "Scout cars and transports."           180 [BasicMining]   "Unlocks MechInf"
  , Tech ArmouredVehicles "Armoured Vehicles"  "Heavy plating and tanks."             250 [LightVehicles] "MechInf +3 defence"
  , Tech RayWeapons      "Ray Weapons"         "Directed energy from RepV schematics."300 [CoilgunRifles] "Unlocks HeavyInf"
  , Tech RailgunDesign   "Railgun Artillery"   "EM mass drivers."                     350 [RayWeapons]    "HeavyInf siege capability"
  , Tech ReconDrones     "Recon Drones"        "Automated aerial scouts."             100 []              "Recon reveals +1 area/turn"
  , Tech FuelOpt         "Fuel Optimisation"   "Refined fuel synthesis."              120 []              "-30% fuel cost"
  , Tech MutantAnalysis  "Mutant Analysis"     "Study of local fauna."                100 []              "Units -1 dmg from neutrals"
  , Tech TradeNetworks   "Trade Networks"      "Barter routes between areas."         150 []              "+1 mat/turn per trade route"
  ]

-- Unit types
data UnitType = Recon | Laborer | Militia | LightInfantry | MechInfantry | HeavyInfantry
  deriving (Show, Eq, Ord, Enum, Bounded)

data UnitStats = UnitStats
  { usAtk     :: Int
  , usDef     :: Int
  , usFoodUp  :: Int
  , usFuelUp  :: Int
  , usBuildMat :: Int
  , usBuildFuel :: Int
  , usBuildFood :: Int
  } deriving (Show, Eq)

unitStats :: UnitType -> UnitStats
unitStats Recon         = UnitStats 1 1 1 1 10 5  0
unitStats Laborer       = UnitStats 0 1 1 0 10 0  10
unitStats Militia       = UnitStats 2 2 2 1 5  0  5
unitStats LightInfantry = UnitStats 4 3 2 2 20 10 0
unitStats MechInfantry  = UnitStats 5 5 3 4 40 20 0
unitStats HeavyInfantry = UnitStats 8 6 4 6 60 30 0

unitTypeName :: UnitType -> String
unitTypeName Recon         = "Recon"
unitTypeName Laborer       = "Laborer"
unitTypeName Militia       = "Militia"
unitTypeName LightInfantry = "Light Infantry"
unitTypeName MechInfantry  = "Mech. Infantry"
unitTypeName HeavyInfantry = "Heavy Infantry"

unitTypeShort :: UnitType -> String
unitTypeShort Recon         = "RCN"
unitTypeShort Laborer       = "LAB"
unitTypeShort Militia       = "MIL"
unitTypeShort LightInfantry = "LIN"
unitTypeShort MechInfantry  = "MCH"
unitTypeShort HeavyInfantry = "HVY"

unitMaxHP :: UnitType -> Int
unitMaxHP ut = let s = unitStats ut in (usAtk s + usDef s) * 5

unitTypeDesc :: UnitType -> String
unitTypeDesc Recon         = "Fast scouts. Explore, spy, trade."
unitTypeDesc Laborer       = "Build bases, exploit resources, clear hostiles."
unitTypeDesc Militia       = "Citizen fighters. Good for clearing and home defence."
unitTypeDesc LightInfantry = "Coilguns, RPGs, LMGs. Capable PvP fighters."
unitTypeDesc MechInfantry  = "Vehicle-mounted. Anti-armour. Heavy assault."
unitTypeDesc HeavyInfantry = "Elite. Ray weapons, railgun artillery. Siege capable."

-- Unit instance
type UnitId = Int

data Veterancy = Green | Regular | Veteran | Elite
  deriving (Show, Eq, Ord, Enum, Bounded)

vetBonus :: Veterancy -> Int
vetBonus Green   = 0
vetBonus Regular = 1
vetBonus Veteran = 2
vetBonus Elite   = 4

xpThreshold :: Veterancy -> Int
xpThreshold Green   = 0
xpThreshold Regular = 10
xpThreshold Veteran = 30
xpThreshold Elite   = 70

data Unit = Unit
  { unitId       :: UnitId
  , unitType     :: UnitType
  , unitOwner    :: FactionId
  , unitName     :: String
  , unitHP       :: Int
  , unitXP       :: Int
  , unitVet      :: Veterancy
  , unitLoc      :: AreaId
  , unitOrdered  :: Bool
  } deriving (Show, Eq)

-- Areas
type AreaId = Int

data AreaTerrain
  = Grassland | Forest | Mountains | Ruins | Volcanic
  | Desert | Riverland | Coast | Industrial
  deriving (Show, Eq, Ord)

terrainChar :: AreaTerrain -> String
terrainChar Grassland  = c 32 "~"
terrainChar Forest     = c 32 "*"
terrainChar Mountains  = c 37 "^"
terrainChar Ruins      = c 33 "H"
terrainChar Volcanic   = c 31 "O"
terrainChar Desert     = c 33 "~"
terrainChar Riverland  = c 34 "~"
terrainChar Coast      = c 36 "~"
terrainChar Industrial = c 37 "#"

c :: Int -> String -> String
c n s = "\ESC[" ++ show n ++ "m" ++ s ++ "\ESC[0m"

data Area = Area
  { areaId       :: AreaId
  , areaName     :: String
  , areaTerrain  :: AreaTerrain
  , areaAdj      :: [AreaId]
  , areaOwner    :: Maybe FactionId
  , areaBase     :: Maybe Base
  , areaUnits    :: Map FactionId [UnitId]
  , areaExplored :: Set FactionId
  , areaCapacity :: Int
  , areaYield    :: Resources
  , areaHazard   :: Int    -- 0-5
  , areaFeatures :: [String]
  } deriving (Show, Eq)

data Base = Base
  { baseOwner   :: FactionId
  , baseLevel   :: Int
  , baseHP      :: Int
  , baseMaxHP   :: Int
  , baseDef     :: Int
  } deriving (Show, Eq)

-- Environment
data EnvPhase = ENormal | EUVHigh | EFreeze | EAshStorm
  deriving (Show, Eq)

envPhaseName :: EnvPhase -> String
envPhaseName ENormal   = "Normal"
envPhaseName EUVHigh   = "UV HIGH"
envPhaseName EFreeze   = "FREEZE"
envPhaseName EAshStorm = "ASH STORM"

envPhaseColor :: EnvPhase -> String
envPhaseColor ENormal   = "\ESC[32m"
envPhaseColor EUVHigh   = "\ESC[91m"
envPhaseColor EFreeze   = "\ESC[96m"
envPhaseColor EAshStorm = "\ESC[90m"

data Env = Env
  { envPhase    :: EnvPhase
  , envTurnLeft :: Int   -- turns until phase change
  , envNext     :: EnvPhase
  } deriving (Show, Eq)

-- Log
data LogSev = LInfo | LWarn | LCombat | LDiscover | LDefeat
  deriving (Show, Eq)

data LogEntry = LogEntry
  { leTurn  :: Int
  , leFac   :: Maybe FactionId
  , leMsg   :: String
  , leSev   :: LogSev
  } deriving (Show, Eq)

-- Game state
data Phase = PlayerOrders | Resolution | SitRep
  deriving (Show, Eq)

data Sel = SelNone | SelUnit UnitId | SelArea AreaId
  deriving (Show, Eq)

data GameResult = GWin FactionId | GLose String
  deriving (Show, Eq)

data GameState = GameState
  { gsTurn    :: Int
  , gsAreas   :: Map AreaId Area
  , gsFacs    :: Map FactionId Faction
  , gsUnits   :: Map UnitId Unit
  , gsEnv     :: Env
  , gsLog     :: [LogEntry]
  , gsNextUID :: UnitId
  , gsRng     :: StdGen
  , gsPhase   :: Phase
  , gsSel     :: Sel
  , gsPlayer  :: FactionId
  , gsOver    :: Maybe GameResult
  , gsMsg     :: Maybe String
  } deriving (Show)

-- ANSI helpers
reset :: String
reset = "\ESC[0m"

bold :: String -> String
bold s = "\ESC[1m" ++ s ++ reset

dim :: String -> String
dim s = "\ESC[2m" ++ s ++ reset

col :: Int -> String -> String
col n s = "\ESC[" ++ show n ++ "m" ++ s ++ reset

factionCol :: FactionId -> Int
factionCol NeoRepublic    = 94  -- bright blue
factionCol SouthernEmpire = 91  -- bright red
factionCol Insects        = 92  -- bright green
factionCol Thieves        = 93  -- bright yellow

factionColored :: FactionId -> String -> String
factionColored fid s = col (factionCol fid) s

factionShort :: FactionId -> String
factionShort NeoRepublic    = "NeoRep"
factionShort SouthernEmpire = "S.Emp"
factionShort Insects        = "Hivfrm"
factionShort Thieves        = "Thieves"


padR :: Int -> String -> String
padR n s
  | vlen s >= n = take n (stripA s)
  | otherwise   = s ++ replicate (n - vlen s) ' '

padL :: Int -> String -> String
padL n s
  | vlen s >= n = take n (stripA s)
  | otherwise   = replicate (n - vlen s) ' ' ++ s

stripA :: String -> String
stripA [] = []
stripA ('\ESC':'[':rest) = stripA (drop 1 (dropWhile (/= 'm') rest))
stripA (x:xs) = x : stripA xs

vlen :: String -> Int
vlen = length . stripA
