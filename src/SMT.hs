{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SMT where

import Brick hiding (clamp)
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Array (Array, Ix, bounds, elems, listArray, (!))
import Data.Binary (Binary, decode, decodeFileOrFail, encode, encodeFile)
import qualified Data.Binary as Bin
import Data.Bits (shiftR, xor)
import Data.List (find, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, listToMaybe, mapMaybe)
import GHC.Generics (Generic)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import System.Directory (doesFileExist)

-- ===========================================================================
-- ANIMATION HELPERS
-- ===========================================================================

sinApprox :: Int -> Int
sinApprox deg =
  let d = deg `mod` 360
   in if d < 90
        then d * 1000 `div` 90
        else
          if d < 180
            then (180 - d) * 1000 `div` 90
            else
              if d < 270
                then negate $ (d - 180) * 1000 `div` 90
                else negate $ (360 - d) * 1000 `div` 90

breathe :: Int -> Int -> Int
breathe speed tick = (sinApprox ((tick * speed) `mod` 360) + 1000) `div` 2

lerpRGB :: (Int, Int, Int) -> (Int, Int, Int) -> Int -> (Int, Int, Int)
lerpRGB (r1, g1, b1) (r2, g2, b2) t =
  ( r1 + (r2 - r1) * t `div` 1000,
    g1 + (g2 - g1) * t `div` 1000,
    b1 + (b2 - b1) * t `div` 1000
  )

hueRGB :: Int -> Int -> (Int, Int, Int)
hueRGB sat tick =
  let h = (tick * 7) `mod` 360
      seg = h `div` 60
      f = (h `mod` 60) * 255 `div` 60
      s = sat
      (r, g, b) = case seg of
        0 -> (255, f, 0)
        1 -> (255 - f, 255, 0)
        2 -> (0, 255, f)
        3 -> (0, 255 - f, 255)
        4 -> (f, 0, 255)
        _ -> (255, 0, 255 - f)
      desat c = c + (255 - c) * (255 - s) `div` 255
   in (desat r, desat g, desat b)

hpColour :: Int -> Int -> (Int, Int, Int)
hpColour hp maxHp =
  let pct = hp * 100 `div` max 1 maxHp
   in if pct > 60
        then (60, 200, 80)
        else
          if pct > 30
            then lerpRGB (220, 200, 40) (60, 200, 80) ((pct - 30) * 33)
            else lerpRGB (220, 50, 50) (220, 200, 40) (pct * 33)

pulseRGB :: (Int, Int, Int) -> (Int, Int, Int) -> Int -> Int -> (Int, Int, Int)
pulseRGB c1 c2 speed tick = lerpRGB c1 c2 (breathe speed tick)

flashTint :: Int -> (Int, Int, Int) -> (Int, Int, Int)
flashTint i (r, g, b) = (r + (255 - r) * i `div` 255, g + (255 - g) * i `div` 255, b + (255 - b) * i `div` 255)

-- ===========================================================================
-- SPRITE SYSTEM
-- ===========================================================================

sw, sh :: Int
sw = 5
sh = 3

data SCell = SCell Char (Int, Int, Int)

type Sprite = [[SCell]]

sc :: Char -> (Int, Int, Int) -> SCell
sc = SCell

largeSpr :: Sprite -> Sprite
largeSpr = concatMap (\row -> let r2 = concatMap (\c -> [c, c]) row in [r2, r2])

tintSprite :: ((Int, Int, Int) -> (Int, Int, Int)) -> Sprite -> Sprite
tintSprite f = map (map (\(SCell ch col) -> SCell ch (f col)))

-- ===========================================================================
-- TERRAIN SPRITES  (swamp + cavern)
-- ===========================================================================

bogSpr :: Int -> Sprite
bogSpr tick =
  let p = tick `mod` 4; (c1, c2) = case p of 0 -> ('~', '\''); 1 -> ('\'', '~'); 2 -> ('-', '~'); _ -> ('~', '-')
   in [[sc c1 d, sc c2 l, sc c1 d, sc c2 l, sc c1 d], [sc c2 l, sc c1 d, sc c2 l, sc c1 d, sc c2 l], [sc c1 d, sc c2 l, sc c1 d, sc c2 l, sc c1 d]]
  where
    d = (50, 90, 60); l = (70, 120, 80)

mudSpr :: Sprite
mudSpr = [[sc '.' mg, sc ',' mg, sc '.' dg, sc '\'' mg, sc '.' mg], [sc ',' dg, sc '.' mg, sc ',' mg, sc '.' dg, sc '\'' mg], [sc '.' mg, sc '\'' mg, sc '.' mg, sc ',' dg, sc '.' mg]]
  where
    mg = (90, 75, 50); dg = (70, 58, 35)

deadTreeSpr :: Sprite
deadTreeSpr = [[sc ' ' k, sc 'Y' b, sc ' ' k, sc '/' b, sc ' ' k], [sc ' ' k, sc '|' b, sc ' ' k, sc ' ' k, sc ' ' k], [sc '.' mg, sc '|' d, sc '.' mg, sc ' ' k, sc ' ' k]]
  where
    b = (80, 65, 40); d = (60, 48, 28); mg = (80, 68, 45); k = (0, 0, 0)

reedsSpr :: Int -> Sprite
reedsSpr tick =
  let sway = if tick `mod` 6 < 3 then '|' else '/'
   in [[sc sway r, sc ' ' k, sc '|' r, sc ' ' k, sc sway r], [sc '|' d, sc '\'' g, sc '|' d, sc '\'' g, sc '|' d], [sc '.' mg, sc '.' mg, sc ',' mg, sc '.' mg, sc '.' mg]]
  where
    r = (60, 120, 50); d = (45, 95, 38); g = (80, 150, 60); mg = (80, 70, 45); k = (0, 0, 0)

puddleSpr :: Sprite
puddleSpr = [[sc '.' mg, sc '.' mg, sc '.' mg, sc '.' mg, sc '.' mg], [sc '.' mg, sc '~' w, sc '~' w, sc '.' mg, sc '.' mg], [sc '.' mg, sc '.' mg, sc '.' mg, sc '.' mg, sc '.' mg]]
  where
    mg = (85, 72, 48); w = (60, 100, 75)

stoneSpr :: Sprite
stoneSpr = [[sc '#' s, sc ' ' k, sc '#' d, sc ' ' k, sc '#' s], [sc ' ' k, sc '#' d, sc '#' s, sc '#' d, sc ' ' k], [sc '#' d, sc '#' s, sc ' ' k, sc '#' d, sc '#' s]]
  where
    s = (110, 105, 100); d = (80, 76, 72); k = (0, 0, 0)

-- Cavern terrain sprites
lavaS :: Int -> Sprite
lavaS tick =
  let p = tick `mod` 4; (c1, c2) = case p of 0 -> ('~', '\''); 1 -> ('\'', '~'); 2 -> ('-', '~'); _ -> ('~', '-')
   in [[sc c1 d, sc c2 l, sc c1 d, sc c2 l, sc c1 d], [sc c2 l, sc c1 d, sc c2 l, sc c1 d, sc c2 l], [sc c1 d, sc c2 l, sc c1 d, sc c2 l, sc c1 d]]
  where
    d = (180, 40, 10); l = (220, 80, 20)

rockFloorS :: Sprite
rockFloorS = [[sc '.' dg, sc ':' dg, sc '.' mg, sc ',' dg, sc '.' mg], [sc ':' mg, sc '.' dg, sc ':' mg, sc '.' dg, sc ':' mg], [sc '.' mg, sc ',' dg, sc '.' mg, sc ':' dg, sc '.' mg]]
  where
    dg = (70, 65, 80); mg = (90, 85, 100)

stalactiteS :: Sprite
stalactiteS = [[sc ' ' k, sc 'V' s, sc ' ' k, sc 'v' d, sc ' ' k], [sc ' ' k, sc '|' d, sc ' ' k, sc ' ' k, sc ' ' k], [sc '.' rk, sc '.' rk, sc '.' rk, sc '.' rk, sc '.' rk]]
  where
    s = (160, 155, 175); d = (120, 115, 135); rk = (90, 85, 100); k = (0, 0, 0)

crystalS :: Int -> Sprite
crystalS tick =
  let (cr, cg, cb) = lerpRGB (100, 180, 255) (180, 100, 255) (breathe 8 tick)
   in [[sc ' ' k, sc '/' (cr, cg, cb), sc '*' (255, 240, 255), sc '\\' (cr, cg, cb), sc ' ' k], [sc '/' (cr, cg, cb), sc '|' (cr `div` 2, cg `div` 2, cb `div` 2), sc '|' (cr, cg, cb), sc '\\' (cr, cg, cb), sc ' ' k], [sc '.' rk, sc '|' (cr `div` 2, cg `div` 2, cb `div` 2), sc '.' rk, sc ' ' k, sc ' ' k]]
  where
    k = (0, 0, 0); rk = (80, 75, 95)

caveWallS :: Sprite
caveWallS = [[sc '█' d, sc '█' s, sc '█' d, sc '█' s, sc '█' d], [sc '█' s, sc '▓' d, sc '█' s, sc '▓' d, sc '█' s], [sc '▓' d, sc '█' s, sc '▓' d, sc '█' s, sc '▓' d]]
  where
    d = (55, 50, 65); s = (75, 70, 85)

-- Healer NPC sprite
healerSpr :: Sprite
healerSpr =
  [ [sc ' ' k, sc 'o' sk, sc ' ' k, sc '+' h, sc ' ' k],
    [sc '(' rb, sc ':' rb, sc ')' rb, sc ' ' k, sc ' ' k],
    [sc '/' rb, sc ' ' k, sc '\\' rb, sc ' ' k, sc ' ' k]
  ]
  where
    sk = (235, 210, 180); h = (80, 220, 120); rb = (80, 140, 210); k = (0, 0, 0)

-- Portal sprite (animated, used as overlay)
portalSpr :: Int -> Sprite
portalSpr tick =
  let (pr, pg, pb) = hueRGB 200 (tick * 4)
      (ir, ig, ib) = lerpRGB (pr, pg, pb) (255, 255, 255) (breathe 15 tick)
   in [[sc ' ' k, sc '(' (pr, pg, pb), sc 'Ω' (ir, ig, ib), sc ')' (pr, pg, pb), sc ' ' k], [sc '(' (pr, pg, pb), sc ':' (ir, ig, ib), sc ':' (ir, ig, ib), sc ')' (pr, pg, pb), sc ' ' k], [sc ' ' k, sc '\\' (pr, pg, pb), sc '_' (pr, pg, pb), sc '/' (pr, pg, pb), sc ' ' k]]
  where
    k = (0, 0, 0)

terrainSpr :: Int -> Terrain -> Sprite
terrainSpr tick t = case t of
  Bog -> bogSpr tick
  Mud -> mudSpr
  DeadTree -> deadTreeSpr
  Reeds -> reedsSpr tick
  Puddle -> puddleSpr
  Stone -> stoneSpr
  Lava -> lavaS tick
  RockFloor -> rockFloorS
  Stalactite -> stalactiteS
  Crystal -> crystalS tick
  CaveWall -> caveWallS

-- ===========================================================================
-- ENTITY SPRITES
-- ===========================================================================

playerSpr :: Sprite
playerSpr = [[sc ' ' k, sc '(' p, sc 'o' p, sc ')' p, sc ' ' k], [sc ' ' k, sc '[' c, sc '|' c, sc ']' c, sc ' ' k], [sc ' ' k, sc '/' c, sc ' ' c, sc '\\' c, sc ' ' k]]
  where
    p = (210, 195, 170); c = (60, 90, 160); k = (0, 0, 0)

swordDemonSpr :: Int -> Sprite
swordDemonSpr frame =
  let eyeChar = if frame == 0 then '@' else '*'
      hornCol = if frame == 0 then (200, 60, 60) else (220, 80, 40)
   in [[sc '/' hornCol, sc 'D' r, sc 'D' r, sc '\\' hornCol, sc '|' s], [sc ' ' k, sc '(' r, sc eyeChar e, sc ')' r, sc '-' s], [sc ' ' k, sc '|' d, sc '|' d, sc ' ' k, sc ' ' k]]
  where
    r = (170, 40, 40); e = (255, 120, 50); d = (130, 30, 30); s = (190, 190, 210); k = (0, 0, 0)

plagueDemonSpr :: Int -> Sprite
plagueDemonSpr frame =
  let bubbleCol = if frame == 0 then (90, 170, 70) else (110, 190, 90)
   in [[sc ' ' k, sc 'o' g, sc 'O' bubbleCol, sc 'o' g, sc ' ' k], [sc '(' g, sc '*' bubbleCol, sc '$' y, sc '*' bubbleCol, sc ')' g], [sc ' ' k, sc '~' g, sc '~' dg, sc '~' g, sc ' ' k]]
  where
    g = (60, 130, 50); dg = (40, 100, 35); y = (180, 200, 50); k = (0, 0, 0)

shadowDemonSpr :: Int -> Sprite
shadowDemonSpr frame =
  let wisp = if frame == 0 then '~' else '-'
      vCol = if frame == 0 then (120, 80, 180) else (140, 100, 200)
   in [[sc ' ' k, sc ')' vCol, sc 'o' p, sc '(' vCol, sc ' ' k], [sc wisp vCol, sc ' ' k, sc '|' dv, sc ' ' k, sc wisp vCol], [sc ' ' k, sc wisp dv, sc ' ' k, sc wisp dv, sc ' ' k]]
  where
    dv = (80, 50, 130); p = (210, 170, 255); k = (0, 0, 0)

-- Cavern-exclusive demons
infernoDemonSpr :: Int -> Sprite
infernoDemonSpr frame =
  let (fr, fg, fb) = if frame == 0 then (255, 100, 20) else (255, 160, 40)
   in [[sc '/' (fr, fg `div` 2, 0), sc 'A' (fr, fg, fb), sc 'A' (fr, fg, fb), sc '\\' (fr, fg `div` 2, 0), sc '|' (200, 200, 220)], [sc ' ' (0, 0, 0), sc '(' (200, 50, 10), sc '@' (255, 200, 50), sc ')' (200, 50, 10), sc '=' (200, 200, 220)], [sc ' ' (0, 0, 0), sc '|' (150, 30, 10), sc '|' (150, 30, 10), sc ' ' (0, 0, 0), sc ' ' (0, 0, 0)]]

voidDemonSpr :: Int -> Sprite
voidDemonSpr frame =
  let (vr, vg, vb) = if frame == 0 then (60, 20, 120) else (90, 30, 160)
   in [[sc '(' (vr, vg, vb), sc '*' (200, 150, 255), sc 'O' (220, 180, 255), sc '*' (200, 150, 255), sc ')' (vr, vg, vb)], [sc '~' (vr, vg, vb), sc ' ' (0, 0, 0), sc '|' (vr, vg, vb), sc ' ' (0, 0, 0), sc '~' (vr, vg, vb)], [sc ' ' (0, 0, 0), sc '~' (vr `div` 2, vg `div` 2, vb `div` 2), sc ' ' (0, 0, 0), sc '~' (vr `div` 2, vg `div` 2, vb `div` 2), sc ' ' (0, 0, 0)]]

demonSpriteFramed :: DemonKind -> Int -> Sprite
demonSpriteFramed SwordDemon f = swordDemonSpr f
demonSpriteFramed PlagueDemon f = plagueDemonSpr f
demonSpriteFramed ShadowDemon f = shadowDemonSpr f
demonSpriteFramed InfernoDemon f = infernoDemonSpr f
demonSpriteFramed VoidDemon f = voidDemonSpr f

overlayEntity :: Sprite -> Sprite -> Sprite
overlayEntity entity terrain = zipWith (zipWith merge) entity terrain
  where
    merge (SCell _ (0, 0, 0)) t = t; merge e _ = e

-- ===========================================================================
-- TYPES
-- ===========================================================================

type Pos = (Int, Int)

data MapId = SwampMap | CavernMap deriving (Eq, Show, Ord, Generic)

instance Binary MapId

data Terrain = Bog | Mud | DeadTree | Reeds | Puddle | Stone | Lava | RockFloor | Stalactite | Crystal | CaveWall
  deriving (Eq, Show, Generic)

instance Binary Terrain

data DemonKind = SwordDemon | PlagueDemon | ShadowDemon | InfernoDemon | VoidDemon
  deriving (Eq, Show, Ord, Generic)

instance Binary DemonKind

data Move = SwordAttack | FireAttack | PoisonCloud | DarkSlash | InfernoBlast | VoidRend | ThunderStrike | IceSpear
  deriving (Eq, Show, Ord, Generic)

instance Binary Move

data DamageType = Physical | Magical deriving (Eq, Show, Generic)

instance Binary DamageType

moveInfo :: Move -> (String, DamageType, Int)
moveInfo SwordAttack = ("Sword Atk", Physical, 8)
moveInfo FireAttack = ("Fire Atk", Magical, 10)
moveInfo PoisonCloud = ("Poison", Magical, 10)
moveInfo DarkSlash = ("Dark Slash", Physical, 8)
moveInfo InfernoBlast = ("Inferno", Magical, 14)
moveInfo VoidRend = ("Void Rend", Magical, 12)
moveInfo ThunderStrike = ("Thunder", Magical, 13)
moveInfo IceSpear = ("Ice Spear", Magical, 11)

-- XP thresholds and stat gains per level
xpThresholds :: [Int]
xpThresholds = [50, 150, 350]

xpForLevel :: Int -> Int
xpForLevel lvl = if lvl <= 0 then 0 else xpThresholds !! min (lvl - 1) (length xpThresholds - 1)

-- Moves unlocked at each level per demon kind
levelUpMoves :: DemonKind -> Int -> Maybe Move
levelUpMoves SwordDemon 2 = Just ThunderStrike
levelUpMoves SwordDemon 3 = Just InfernoBlast
levelUpMoves PlagueDemon 2 = Just IceSpear
levelUpMoves PlagueDemon 3 = Just VoidRend
levelUpMoves ShadowDemon 2 = Just VoidRend
levelUpMoves ShadowDemon 3 = Just DarkSlash
levelUpMoves InfernoDemon 2 = Just InfernoBlast
levelUpMoves InfernoDemon 3 = Just VoidRend
levelUpMoves VoidDemon 2 = Just VoidRend
levelUpMoves VoidDemon 3 = Just ThunderStrike
levelUpMoves _ _ = Nothing

-- Per-demon persistent stats (survives between battles)
data DemonStats = DemonStats {dsLevel :: Int, dsXP :: Int, dsMoves :: [Move]} deriving (Eq, Show, Generic)

instance Binary DemonStats

defaultDemonStats :: DemonKind -> DemonStats
defaultDemonStats k = DemonStats 1 0 (dMoves (demonTemplates Map.! k))

data DemonTemplate = DemonTemplate {dKind :: DemonKind, dName :: String, dMaxHP :: Int, dPhyAtk :: Int, dMagAtk :: Int, dPhyDef :: Int, dMagDef :: Int, dMoves :: [Move], dXPReward :: Int}

demonTemplates :: Map DemonKind DemonTemplate
demonTemplates =
  Map.fromList
    [ (SwordDemon, DemonTemplate SwordDemon "Kelpie" 80 35 20 30 15 [SwordAttack, FireAttack] 25),
      (PlagueDemon, DemonTemplate PlagueDemon "Boggart" 70 25 30 20 30 [PoisonCloud, FireAttack] 20),
      (ShadowDemon, DemonTemplate ShadowDemon "Umbra" 65 30 35 20 35 [DarkSlash, FireAttack] 30),
      (InfernoDemon, DemonTemplate InfernoDemon "Infernal" 90 40 45 25 25 [InfernoBlast, FireAttack] 45),
      (VoidDemon, DemonTemplate VoidDemon "Voidling" 75 35 50 20 40 [VoidRend, DarkSlash] 40)
    ]

data Combatant = Combatant {combName :: String, combHP :: Int, combMaxHP :: Int, combPhyAtk :: Int, combMagAtk :: Int, combPhyDef :: Int, combMagDef :: Int, combMoves :: [Move], combKind :: Maybe DemonKind, combLevel :: Int, combXP :: Int} deriving (Eq, Show, Generic)

instance Binary Combatant

playerCombatant :: Combatant
playerCombatant = Combatant "Hayato" 120 120 40 20 25 20 [SwordAttack] Nothing 1 0

fromTemplateWithStats :: DemonTemplate -> DemonStats -> Combatant
fromTemplateWithStats t ds =
  Combatant
    { combName = dName t,
      combHP = dMaxHP t + (dsLevel ds - 1) * 8,
      combMaxHP = dMaxHP t + (dsLevel ds - 1) * 8,
      combPhyAtk = dPhyAtk t + (dsLevel ds - 1) * 3,
      combMagAtk = dMagAtk t + (dsLevel ds - 1) * 3,
      combPhyDef = dPhyDef t,
      combMagDef = dMagDef t,
      combMoves = dsMoves ds,
      combKind = Just (dKind t),
      combLevel = dsLevel ds,
      combXP = dsXP ds
    }

fromTemplate :: DemonTemplate -> Combatant
fromTemplate t = fromTemplateWithStats t (defaultDemonStats (dKind t))

data WorldDemon = WorldDemon
  { wdId :: Int,
    wdKind :: DemonKind,
    wdPos :: Pos,
    wdSpawn :: Pos,
    wdFacing :: (Int, Int),
    wdStepCtr :: Int
  }
  deriving (Eq, Show, Generic)

instance Binary WorldDemon

data Portal = Portal {portalPos :: Pos, portalDest :: MapId, portalDestPos :: Pos}
  deriving (Eq, Show, Generic)

instance Binary Portal

data MapData = MapData
  { mdTerrain :: Array Pos Terrain,
    mdPortals :: [Portal],
    mdHealers :: [Pos], -- positions of healer NPCs on this map
    mdSpawnKinds :: [DemonKind], -- kinds that can spawn on this map
    mdSpawnCap :: Int,
    mdAmbient :: (Int, Int, Int) -- colour tint for sidebar/UI accent
  }

data BattleMenu = MenuFight | MenuSummon | MenuRecruit | MenuBag | MenuRun
  deriving (Eq, Show, Enum, Bounded)

data BattlePhase
  = PlayerMenu Int
  | PlayerFight Int Int
  | PlayerSummon Int
  | EnemyTurn Int String
  | BattleOver Bool
  deriving (Eq, Show, Generic)

instance Binary BattlePhase

data BattleState = BattleState
  { bPlayer :: Combatant,
    bAllies :: [Combatant],
    bEnemy :: Combatant,
    bEnemyAllies :: [Combatant],
    bEnemyId :: Int,
    bPhase :: BattlePhase,
    bMenuSel :: BattleMenu,
    bLog :: [String],
    hitFlash :: Int,
    allyFlash :: Int
  }
  deriving (Generic)

instance Binary BattleState

instance Binary BattleMenu where
  put m = Bin.put (fromEnum m)
  get = fmap toEnum Bin.get

data Screen = Exploring | Fighting BattleState

data Player = Player
  { playerPos :: Pos,
    playerFacing :: (Int, Int),
    playerHP :: Int,
    playerMaxHP :: Int,
    playerParty :: [DemonKind],
    playerLevel :: Int,
    playerXP :: Int,
    partyStats :: Map DemonKind DemonStats, -- persistent per-demon stats
    playerPotions :: Int
  }
  deriving (Eq, Show, Generic)

instance Binary Player

-- Save/load struct — excludes worldMaps (regenerated) and Screen (always Exploring on load)
data SaveData = SaveData
  { savePlayer :: Player,
    saveDemons :: Map Int WorldDemon,
    saveCurrentMap :: MapId,
    saveNextDmnId :: Int,
    saveSpawnTimer :: Int,
    saveMsgLog :: [String]
  }
  deriving (Generic)

instance Binary SaveData

-- Wrap Array for Binary
newtype ArrWrap i e = ArrWrap {unArr :: Array i e}

instance (Binary i, Binary e, Ix i) => Binary (ArrWrap i e) where
  put (ArrWrap a) = Bin.put (bounds a) >> Bin.put (elems a)
  get = do bnds <- Bin.get; els <- Bin.get; return $ ArrWrap (listArray bnds els)

data GameState = GameState
  { maps :: Map MapId MapData,
    currentMap :: MapId,
    player :: Player,
    demons :: Map Int WorldDemon,
    screen :: Screen,
    animTick :: Int,
    msgLog :: [String],
    nextDmnId :: Int,
    spawnTimer :: Int -- ticks since last spawn attempt
  }

data AppEvent = Tick | DemonTick

type Name = ()

-- ===========================================================================
-- MAP GENERATION
-- ===========================================================================

mapW, mapH :: Int
mapW = 40
mapH = 30

hash2 :: Int -> Int -> Int
hash2 x y =
  let h = x * 374761393 + y * 1103515245; h2 = h `xor` (h `shiftR` 13); h3 = h2 * 1664525
   in h3 `xor` (h3 `shiftR` 16)

valueNoise :: Int -> Int -> Int
valueNoise x y = abs (hash2 x y) `mod` 1001

smoothNoise :: Int -> Int -> Int -> Int
smoothNoise scale x y =
  let gx = x `div` scale
      gy = y `div` scale
      tx = (x `mod` scale) * 1000 `div` scale
      ty = (y `mod` scale) * 1000 `div` scale
      v00 = valueNoise gx gy
      v10 = valueNoise (gx + 1) gy
      v01 = valueNoise gx (gy + 1)
      v11 = valueNoise (gx + 1) (gy + 1)
      lerp a b t = a + (b - a) * t `div` 1000
   in lerp (lerp v00 v10 tx) (lerp v01 v11 tx) ty

isPassable :: Terrain -> Bool
isPassable Bog = False; isPassable Stone = False; isPassable CaveWall = False; isPassable _ = True

genSwamp :: Int -> Int -> Terrain
genSwamp x y =
  let e = (smoothNoise 8 x y * 600 + smoothNoise 4 x y * 300 + smoothNoise 2 x y * 100) `div` 1000
      d = valueNoise (x + 37) (y + 91) `mod` 100
   in if e < 300
        then Bog
        else
          if e < 500
            then if d < 15 then Puddle else Mud
            else
              if e < 700
                then if d < 10 then Reeds else if d < 20 then DeadTree else Mud
                else if d < 20 then Stone else Reeds

genCavern :: Int -> Int -> Terrain
genCavern x y =
  let e = (smoothNoise 6 (x + 50) (y + 50) * 600 + smoothNoise 3 (x + 50) (y + 50) * 300 + smoothNoise 2 (x + 50) (y + 50) * 100) `div` 1000
      d = valueNoise (x + 113) (y + 217) `mod` 100
   in if e < 250
        then Lava
        else
          if e < 400
            then RockFloor
            else
              if e < 650
                then if d < 8 then Crystal else if d < 18 then Stalactite else RockFloor
                else CaveWall

buildSwampMap :: Array Pos Terrain
buildSwampMap =
  listArray
    ((0, 0), (mapW - 1, mapH - 1))
    [genSwamp x y | y <- [0 .. mapH - 1], x <- [0 .. mapW - 1]]

buildCavernMap :: Array Pos Terrain
buildCavernMap =
  listArray
    ((0, 0), (mapW - 1, mapH - 1))
    [genCavern x y | y <- [0 .. mapH - 1], x <- [0 .. mapW - 1]]

buildMaps :: Map MapId MapData
buildMaps =
  Map.fromList
    [ ( SwampMap,
        MapData
          { mdTerrain = buildSwampMap,
            mdPortals = [Portal (20, 15) CavernMap (5, 5)],
            mdHealers = [(8, 5)],
            mdSpawnKinds = [SwordDemon, PlagueDemon, ShadowDemon],
            mdSpawnCap = 6,
            mdAmbient = (60, 100, 70)
          }
      ),
      ( CavernMap,
        MapData
          { mdTerrain = buildCavernMap,
            mdPortals = [Portal (5, 5) SwampMap (20, 15)],
            mdHealers = [(8, 8)],
            mdSpawnKinds = [InfernoDemon, VoidDemon, ShadowDemon],
            mdSpawnCap = 5,
            mdAmbient = (90, 50, 130)
          }
      )
    ]

initialDemons :: Map Int WorldDemon
initialDemons =
  Map.fromList $
    zip
      [0 ..]
      [ WorldDemon i k p p (1, 0) 0
        | (i, k, p) <- [(0, SwordDemon, (15, 10)), (1, PlagueDemon, (25, 18)), (2, ShadowDemon, (10, 22)), (3, SwordDemon, (30, 8)), (4, PlagueDemon, (20, 25))]
      ]

initialPlayer :: Player
initialPlayer = Player (5, 5) (1, 0) 120 120 [] 1 0 Map.empty 3

initialState :: Map MapId MapData -> GameState
initialState ms =
  GameState
    { maps = ms,
      currentMap = SwampMap,
      player = initialPlayer,
      demons = initialDemons,
      screen = Exploring,
      animTick = 0,
      msgLog = ["You enter the fetid swamp...", "SPACE: attack/portal/heal · +: healer NPC"],

      nextDmnId = 5,
      spawnTimer = 0
    }

savePath :: FilePath
savePath = "daemon_hunt.sav"

toSaveData :: GameState -> SaveData
toSaveData gs =
  SaveData
    { savePlayer = player gs,
      saveDemons = demons gs,
      saveCurrentMap = currentMap gs,
      saveNextDmnId = nextDmnId gs,
      saveSpawnTimer = spawnTimer gs,
      saveMsgLog = take 10 (msgLog gs)
    }

fromSaveData :: Map MapId MapData -> SaveData -> GameState
fromSaveData ms sd =
  GameState
    { maps = ms,
      currentMap = saveCurrentMap sd,
      player = savePlayer sd,
      demons = saveDemons sd,
      screen = Exploring,
      animTick = 0,
      msgLog = "(save loaded)" : saveMsgLog sd,
      nextDmnId = saveNextDmnId sd,
      spawnTimer = saveSpawnTimer sd
    }

doSave :: GameState -> IO ()
doSave gs = encodeFile savePath (toSaveData gs)

doLoad :: Map MapId MapData -> IO (Maybe GameState)
doLoad ms = do
  exists <- doesFileExist savePath
  if not exists
    then return Nothing
    else do
      r <- decodeFileOrFail savePath
      case r of
        Left _ -> return Nothing
        Right sd -> return (Just (fromSaveData ms sd))

-- ===========================================================================
-- SPAWN SYSTEM
-- ===========================================================================

spawnThreshold :: Int
spawnThreshold = 12 -- DemonTicks between spawn attempts

trySpawnDemon :: GameState -> GameState
trySpawnDemon gs =
  let md = maps gs Map.! currentMap gs
      cap = mdSpawnCap md
      alive = Map.size (demons gs)
      pp = playerPos (player gs)
   in if alive >= cap || spawnTimer gs < spawnThreshold
        then gs {spawnTimer = spawnTimer gs + 1}
        else
          let kinds = mdSpawnKinds md
              tm = animTick gs
              kind = kinds !! (abs (hash2 tm (alive * 7)) `mod` length kinds)
              -- pick a passable tile far from player
              candidates =
                [ (x, y) | x <- [0 .. mapW - 1], y <- [0 .. mapH - 1], let t = mdTerrain md ! (x, y), isPassable t, abs (x - fst pp) + abs (y - snd pp) > 8, not (any (\wd -> wdPos wd == (x, y)) (Map.elems (demons gs)))
                ]
              idx = abs (hash2 (tm * 3) (alive * 11)) `mod` max 1 (length candidates)
              pos = if null candidates then (10, 10) else candidates !! idx
              wid = nextDmnId gs
              wd = WorldDemon wid kind pos pos (1, 0) 0
           in gs {demons = Map.insert wid wd (demons gs), nextDmnId = wid + 1, spawnTimer = 0}

-- ===========================================================================
-- XP / LEVELING
-- ===========================================================================

-- Award XP to a demon kind in partyStats; return (updated stats, [level-up messages])
awardXP :: DemonKind -> Int -> Map DemonKind DemonStats -> (Map DemonKind DemonStats, [String])
awardXP kind xp statsMap =
  let old = fromMaybe (defaultDemonStats kind) (Map.lookup kind statsMap)
      newXP = dsXP old + xp
      newLvl = length (takeWhile (<= newXP) xpThresholds) + 1
      lvlUp = newLvl > dsLevel old
      -- unlock moves for each new level
      newMoves = nub $ dsMoves old ++ [mv | lv <- [dsLevel old + 1 .. newLvl], Just mv <- [levelUpMoves kind lv]]
      msgs = if lvlUp then (dName (demonTemplates Map.! kind) ++ " reached level " ++ show newLvl ++ "!") : ["  Learned " ++ fst3 (moveInfo mv) ++ "!" | mv <- newMoves, mv `notElem` dsMoves old] else []
      newStats = old {dsLevel = newLvl, dsXP = newXP, dsMoves = newMoves}
   in (Map.insert kind newStats statsMap, msgs)
  where
    fst3 (a, _, _) = a

-- Award XP to player (stat bump on level)
awardPlayerXP :: Int -> Player -> (Player, [String])
awardPlayerXP xp pl =
  let newXP = playerXP pl + xp
      newLvl = length (takeWhile (<= newXP) xpThresholds) + 1
      lvlUp = newLvl > playerLevel pl
      bonus = if lvlUp then 15 else 0
      msgs = if lvlUp then ["Hayato reached level " ++ show newLvl ++ "! HP and ATK up!"] else []
   in ( pl
          { playerXP = newXP,
            playerLevel = newLvl,
            playerMaxHP = playerMaxHP pl + bonus,
            playerHP = playerHP pl + bonus
          },
        msgs
      )

-- After a won battle: distribute XP to active ally (if any) and player
distributeXP :: BattleState -> Player -> (Player, [String])
distributeXP bs pl =
  let baseXP = dXPReward (demonTemplates Map.! (fromMaybe SwordDemon (combKind (bEnemy bs))))
      allyXP = baseXP
      plXP = baseXP `div` 2
      -- award to first alive ally's kind
      (pl1, msgs1) = case listToMaybe (filter (\c -> combHP c > 0) (bAllies bs)) of
        Nothing -> (pl, [])
        Just a -> case combKind a of
          Nothing -> (pl, [])
          Just k ->
            let (newStats, lmsgs) = awardXP k allyXP (partyStats pl)
             in (pl {partyStats = newStats}, lmsgs)
      -- award to player
      (pl2, msgs2) = awardPlayerXP plXP pl1
   in (pl2, msgs1 ++ msgs2)

-- ===========================================================================
-- RENDERING UTILITIES
-- ===========================================================================

clamp :: Int -> Int -> Int -> Int
clamp lo hi x = max lo (min hi x)

rgb :: Int -> Int -> Int -> V.Attr
rgb r g b = V.withForeColor V.defAttr (V.rgbColor r g b)

boldA :: V.Attr -> V.Attr
boldA = flip V.withStyle V.bold

dimA :: V.Attr
dimA = rgb 120 120 130

boxedPanel :: (Int, Int, Int) -> Int -> String -> [V.Image] -> V.Image
boxedPanel (br, bg, bb) innerW label rows =
  let borderA = rgb br bg bb
      labelA = boldA (rgb (min 255 (br + 50)) (min 255 (bg + 50)) (min 255 (bb + 50)))
      padL = (innerW - length label) `div` 2
      padR = innerW - padL - length label
      top =
        V.string borderA "╔"
          V.<|> V.string borderA (replicate padL '═')
          V.<|> V.string labelA label
          V.<|> V.string borderA (replicate padR '═')
          V.<|> V.string borderA "╗"
      bot = V.string borderA $ "╚" ++ replicate innerW '═' ++ "╝"
      wrapRow r = V.string borderA "║" V.<|> padTo innerW r V.<|> V.string borderA "║"
   in V.vertCat $ [top] ++ map wrapRow rows ++ [bot]

padTo :: Int -> V.Image -> V.Image
padTo w img = img V.<|> V.string V.defAttr (replicate (max 0 (w - V.imageWidth img)) ' ')

spriteImage :: Int -> Sprite -> V.Image
spriteImage flash spr = V.vertCat $ map renderRow spr
  where
    renderRow cells =
      V.horizCat $
        map
          ( \(SCell ch (r, g, b)) ->
              let (r', g', b') = flashTint flash (r, g, b) in V.char (rgb r' g' b') ch
          )
          cells

hpBarImg :: Int -> Int -> Int -> V.Image
hpBarImg tick hp maxHp =
  let pct = (hp * 16) `div` max 1 maxHp
      (r, g, b) = hpColour hp maxHp
      flicker = hp * 100 `div` max 1 maxHp < 20 && tick `mod` 2 == 0
      (r', g', b') = if flicker then flashTint 130 (r, g, b) else (r, g, b)
   in V.string (rgb r' g' b') (replicate pct 'o')
        V.<|> V.string (rgb 50 50 60) (replicate (16 - pct) '.')
        V.<|> V.string dimA (" " ++ show hp ++ "/" ++ show maxHp)

-- XP bar image
xpBarImg :: Int -> Int -> V.Image
xpBarImg xp lvl =
  let threshold = if lvl <= length xpThresholds then xpThresholds !! (lvl - 1) else xpThresholds !! (length xpThresholds - 1)
      pct = min 12 (xp * 12 `div` max 1 threshold)
   in V.string (rgb 180 160 255) (replicate pct '▪')
        V.<|> V.string (rgb 60 55 80) (replicate (12 - pct) '▫')
        V.<|> V.string dimA (" Lv" ++ show lvl)

-- ===========================================================================
-- EXPLORE RENDERING
-- ===========================================================================

vTilesW, vTilesH :: Int
vTilesW = 27
vTilesH = 22

camOrigin :: Pos -> (Int, Int)
camOrigin (px, py) =
  (clamp 0 (mapW - vTilesW) (px - vTilesW `div` 2), clamp 0 (mapH - vTilesH) (py - vTilesH `div` 2))

renderExplore :: GameState -> [Widget Name]
renderExplore gs =
  [raw $ V.horizCat [mapImg, V.string dimA " ", sidebarImg]]
  where
    pl = (player gs)
    (cx, cy) = camOrigin (playerPos pl)
    tick = animTick gs
    mapImg = V.vertCat [buildMapRow gs tick cx (cy + ty) | ty <- [0 .. vTilesH - 1]]
    sidebarImg = renderExploreSidebar gs

buildMapRow :: GameState -> Int -> Int -> Int -> V.Image
buildMapRow gs tick cx wy =
  let sprites = [getTileSprite gs tick (cx + tx) wy | tx <- [0 .. vTilesW - 1]]
      rowsPerSpr = [map (!! r) sprites | r <- [0 .. sh - 1]]
      scImg (SCell ch (r, g, b)) = V.char (rgb r g b) ch
      rowImg = V.horizCat . map scImg
      tileRowImg = V.horizCat . map rowImg
   in V.vertCat (map tileRowImg rowsPerSpr)

getTileSprite :: GameState -> Int -> Int -> Int -> Sprite
getTileSprite gs tick wx wy
  | wx < 0 || wy < 0 || wx >= mapW || wy >= mapH = replicate sh (replicate sw (SCell ' ' (5, 5, 15)))
  | otherwise =
      let pos = (wx, wy)
          pl = player gs
          md = maps gs Map.! currentMap gs
          terrain = mdTerrain md ! pos
          mPortal = find (\p -> portalPos p == pos) (mdPortals md)
          isHealer = pos `elem` mdHealers md
          mDemon = listToMaybe [d | d <- Map.elems (demons gs), wdPos d == pos]
          isPlayer = playerPos pl == pos
          base = terrainSpr tick terrain
          frame wd = (tick + wdId wd * 3) `mod` 2
       in if isPlayer
            then overlayEntity playerSpr base
            else case mPortal of
              Just _ -> overlayEntity (portalSpr tick) base
              Nothing ->
                if isHealer
                  then overlayEntity healerSpr base
                  else case mDemon of
                    Just wd -> overlayEntity (demonSpriteFramed (wdKind wd) (frame wd)) base
                    Nothing -> base

renderExploreSidebar :: GameState -> V.Image
renderExploreSidebar gs =
  let tick = animTick gs
      pl = player gs
      md = maps gs Map.! currentMap gs
      (ar, ag, ab) = mdAmbient md
      titleImg =
        V.horizCat $
          zipWith
            ( \i ch ->
                let (r, g, b) = pulseRGB (ar `div` 2, ag `div` 2, ab `div` 2) (min 255 ar, min 255 ag, min 255 ab) 12 (tick + i * 8)
                 in V.char (boldA (rgb r g b)) ch
            )
            [0 ..]
            ("  DAEMON HUNT  " :: String)
      mapName = case currentMap gs of SwampMap -> "The Fetid Swamp"; CavernMap -> "The Deep Cavern"
      mapImg = V.string (rgb ar ag ab) (" Map: " ++ mapName)
      sep = V.string dimA (replicate 26 '─')
      sp = V.string V.defAttr " "
      hpImg = V.string (rgb 140 140 155) " HP " V.<|> hpBarImg tick (playerHP pl) (playerMaxHP pl)
      xpImg = V.string (rgb 140 130 175) " XP " V.<|> xpBarImg (playerXP pl) (playerLevel pl)
      partyHdr = V.string (boldA (rgb 180 160 220)) " Party"
      partyLines =
        if null (playerParty pl)
          then [V.string dimA "  (no demons)"]
          else
            zipWith
              ( \i k ->
                  let stats = fromMaybe (defaultDemonStats k) (Map.lookup k (partyStats pl))
                      (r, g, b) = pulseRGB (ar `div` 2, ag `div` 2, ab `div` 2) (ar, ag, ab) 10 (tick + i * 25)
                      lvlStr = " Lv" ++ show (dsLevel stats) ++ " (" ++ show (dsXP stats) ++ "xp)"
                   in V.string (rgb r g b) $ "  " ++ show i ++ ". " ++ dName (demonTemplates Map.! k) ++ lvlStr
              )
              [1 ..]
              (playerParty pl)
      potionImg = V.string (rgb 100 210 130) " +" V.<|> V.string (rgb 155 155 165) " Potions: " V.<|> V.string (boldA (rgb 100 220 140)) (show (playerPotions pl))
      ctrlHdr = V.string (boldA (rgb 180 160 220)) " Controls"
      ctrls =
        [ V.string (rgb 155 155 165) "  WASD   move",
          V.string (rgb 155 155 165) "  SPACE  attack/portal/heal",
          V.string (rgb 155 155 165) "  +      healer NPC",
          V.string (rgb 155 155 165) "  ESC    quit"
        ]
      logHdr = V.string (boldA (rgb 180 160 220)) " Log"
      logLines = zipWith (\i l -> let br = max 80 (160 - i * 20) in V.string (rgb br br (min 255 (br + 20))) ("  " ++ take 24 l)) [0 ..] (take 5 (msgLog gs))
   in V.vertCat $
        [titleImg, sep, mapImg, hpImg, xpImg, potionImg, sp, sep, partyHdr]
          ++ partyLines
          ++ [sp, sep, ctrlHdr]
          ++ ctrls
          ++ [sp, sep, logHdr]
          ++ logLines

-- ===========================================================================
-- BATTLE RENDERING
-- ===========================================================================

panelW :: Int
panelW = 36

-- Width for each compact RPG-style combat card
combCardW :: Int
combCardW = 20

renderBattle :: GameState -> BattleState -> [Widget Name]
renderBattle gs bs = [raw $ V.vertCat [titleBar, rowSep "ENEMIES", enemiesRow, rowSep "YOUR PARTY", partyRow, bottomArea]]
  where
    tick = animTick gs
    flash = hitFlash bs
    aflash = allyFlash bs
    md = maps gs Map.! currentMap gs
    (ar, ag, ab) = mdAmbient md
    totalW = panelW * 2 + 1

    rowSep lbl =
      let s = "═[ " ++ lbl ++ " ]"
          padLen = max 0 (totalW - length s)
       in V.string dimA (s ++ replicate padLen '═')

    titleBar =
      let titleImg =
            V.horizCat $
              zipWith
                ( \i ch ->
                    let (r, g, b) = pulseRGB (ar `div` 2, ag `div` 2, ab `div` 2) (ar, ag, ab) 10 (tick * 2 + i * 10)
                     in V.char (boldA (rgb r g b)) ch
                )
                [0 ..]
                (" ◈  DAEMON HUNT  ◈ " :: String)
          (tr, tg, tb) = case bPhase bs of
            EnemyTurn _ _ -> pulseRGB (220, 80, 60) (255, 130, 100) 15 tick
            PlayerMenu i | i > 0 -> pulseRGB (80, 180, 220) (120, 230, 255) 15 tick
            BattleOver True -> pulseRGB (60, 160, 130) (100, 210, 170) 8 tick
            BattleOver False -> (150, 150, 160)
            _ -> pulseRGB (180, 170, 220) (230, 220, 255) 8 tick
          phaseStr = case bPhase bs of
            PlayerMenu 0 -> "  YOUR TURN  "
            PlayerMenu i -> "  SLOT " ++ show i ++ "  "
            PlayerFight _ _ -> "  CHOOSE MOVE  "
            PlayerSummon _ -> "  SUMMON  "
            EnemyTurn _ _ -> "  ENEMY TURN  "
            BattleOver True -> "  VICTORY!  "
            BattleOver False -> "  RETREATED  "
       in titleImg V.<|> V.string (boldA (rgb tr tg tb)) phaseStr

    -- Enemies laid out horizontally at the top
    enemiesRow =
      let enemies = bEnemy bs : bEnemyAllies bs
       in V.horizCat (zipWith mkEnemyCard [0 ..] enemies)

    mkEnemyCard idx e =
      let kind = fromMaybe SwordDemon (combKind e)
          frame = tick `mod` 2
          spr = largeSpr (demonSpriteFramed kind frame)
          (bcr, bcg, bcb) =
            if flash > 0 && idx == 0
              then flashTint (min 255 (flash * 55)) (150, 30, 30)
              else (100, 35, 45)
          nameImg =
            V.horizCat $
              zipWith
                (\i ch -> let (r, g, b) = hueRGB 240 (tick * 2 + idx * 30 + i * 15) in V.char (boldA (rgb r g b)) ch)
                [0 ..]
                (take (combCardW - 7) (combName e))
          lvlImg = V.string dimA $ " Lv" ++ show (combLevel e)
          sprPad = replicate ((combCardW - 10) `div` 2) ' '
          sprImg = V.string V.defAttr sprPad V.<|> spriteImage (min 255 (flash * 64)) spr
          rows = [nameImg V.<|> lvlImg, hpBarImg tick (combHP e) (combMaxHP e), V.string V.defAttr " ", sprImg]
       in boxedPanel (bcr, bcg, bcb) combCardW " ENEMY " rows

    -- Player party laid out horizontally at the bottom
    partyRow =
      let actSlot = case bPhase bs of
            PlayerMenu i -> i
            PlayerFight i _ -> i
            _ -> -1
          party = bPlayer bs : bAllies bs
       in V.horizCat (zipWith (mkPartyCard actSlot) [0 ..] party)

    mkPartyCard actSlot slotIdx c =
      let isActive = actSlot == slotIdx
          isPlayerSlot = slotIdx == 0
          kind = fromMaybe SwordDemon (combKind c)
          spr = largeSpr (if isPlayerSlot then playerSpr else demonSpriteFramed kind (tick `mod` 2))
          (bcr, bcg, bcb)
            | aflash > 0 = flashTint (min 255 (aflash * 55)) (30, 50, 150)
            | isActive = (60, 90, 160)
            | otherwise = (40, 60, 120)
          (nr, ng, nb)
            | isActive = pulseRGB (120, 220, 255) (180, 255, 255) 15 tick
            | isPlayerSlot = (100, 190, 240)
            | otherwise = hueRGB 200 (tick * 2 + slotIdx * 90)
          statusStr = if combHP c <= 0 then " [X]" else ""
          nameStr = take (combCardW - 7) (combName c) ++ statusStr
          nameImg = V.string (boldA (rgb nr ng nb)) nameStr
          lvlImg = V.string dimA $ " Lv" ++ show (combLevel c)
          sprPad = replicate ((combCardW - 10) `div` 2) ' '
          sprImg = V.string V.defAttr sprPad V.<|> spriteImage (min 255 (aflash * 64)) spr
          rows = [nameImg V.<|> lvlImg, hpBarImg tick (combHP c) (combMaxHP c), V.string V.defAttr " ", sprImg]
       in boxedPanel (bcr, bcg, bcb) combCardW " " rows

    bottomArea = V.horizCat [logPanel, V.string dimA " ", menuPanel]

    logPanel =
      let ls = take 5 (bLog bs) ++ repeat ""
          rows = zipWith (\i l -> let br = max 80 (190 - i * 25) in V.string (rgb br br (min 255 (br + 25))) (" " ++ take (panelW - 2) l)) [0 .. 4] ls
       in boxedPanel (70, 55, 95) panelW " BATTLE LOG " rows

    cursor isSel =
      if isSel
        then let (r, g, b) = pulseRGB (200, 180, 50) (255, 240, 100) 18 tick in V.string (boldA (rgb r g b)) " ▸ "
        else V.string dimA "   "
    selLabel isSel lbl =
      let (r, g, b) = if isSel then pulseRGB (220, 215, 255) (255, 252, 255) 18 tick else (145, 140, 160)
       in V.string (boldA (rgb r g b)) lbl

    menuPanel = case bPhase bs of
      PlayerMenu slot ->
        let sel = bMenuSel bs
            isHayato = slot == 0
            actor = maybe "???" combName (playerSlotCombatant bs slot)
            potions = playerPotions (player gs)
            items =
              if isHayato
                then
                  [ (MenuFight, "FIGHT", "Attack the enemy"),
                    (MenuSummon, "SUMMON", "Call a party demon"),
                    (MenuRecruit, "RECRUIT", "Recruit at low HP"),
                    (MenuBag, "BAG", "Potion (" ++ show potions ++ " left)"),
                    (MenuRun, "RUN", "Flee")
                  ]
                else [(MenuFight, "FIGHT", "Attack"), (MenuBag, "BAG", "Potion (" ++ show potions ++ ")"), (MenuRun, "PASS", "Skip turn")]
            rows =
              V.string dimA (" " ++ actor ++ "'s action:")
                : map (\(m, lbl, hint) -> cursor (m == sel) V.<|> selLabel (m == sel) lbl V.<|> V.string dimA ("  " ++ hint)) items
         in boxedPanel (80, 65, 115) panelW " ACTION " rows
      PlayerFight slot idx ->
        let attacker = fromJust (playerSlotCombatant bs slot)
            moves = combMoves attacker
            rows =
              V.string dimA " Choose attack:"
                : zipWith
                  ( \i mv ->
                      let (nm, dt, pw) = moveInfo mv
                          isSel = i == idx
                          dtStr = case dt of Physical -> "PHY"; Magical -> "MAG"
                       in cursor isSel V.<|> selLabel isSel nm V.<|> V.string dimA (" [" ++ dtStr ++ " " ++ show pw ++ "]")
                  )
                  [0 ..]
                  moves
                ++ [V.string dimA "   <- back"]
         in boxedPanel (80, 65, 115) panelW " FIGHT " rows
      PlayerSummon idx ->
        let party = playerParty (player gs)
            rows =
              V.string dimA " Choose demon:"
                : if null party
                  then [V.string dimA "   (party empty)"]
                  else
                    zipWith
                      ( \i k ->
                          let t = demonTemplates Map.! k
                              isSel = i == idx
                              stats = fromMaybe (defaultDemonStats k) (Map.lookup k (partyStats (player gs)))
                              lvlStr = " Lv" ++ show (dsLevel stats)
                           in cursor isSel V.<|> selLabel isSel (dName t) V.<|> V.string dimA lvlStr
                      )
                      [0 ..]
                      party
                      ++ [V.string dimA "   <- back"]
         in boxedPanel (80, 65, 115) panelW " SUMMON " rows
      EnemyTurn _ msg ->
        let (r, g, b) = pulseRGB (230, 80, 60) (255, 140, 100) 12 tick
         in boxedPanel
              (125, 35, 35)
              panelW
              " ENEMY TURN "
              [V.string (boldA (rgb r g b)) (" " ++ msg), V.string dimA " (press any key)"]
      BattleOver True ->
        let (r, g, b) = pulseRGB (60, 160, 130) (100, 210, 170) 8 tick
         in boxedPanel
              (50, 120, 55)
              panelW
              " RESULT "
              [V.string (boldA (rgb r g b)) "  VICTORY!", V.string dimA "  Press any key"]
      BattleOver False ->
        boxedPanel
          (75, 75, 90)
          panelW
          " RESULT "
          [V.string dimA "  You retreated safely.", V.string dimA "  Press any key"]

-- ===========================================================================
-- DEMON WANDERING
-- ===========================================================================

addMsg :: String -> GameState -> GameState
addMsg msg gs = gs {msgLog = msg : msgLog gs}

addMsgs :: [String] -> GameState -> GameState
addMsgs msgs gs = foldr addMsg gs (reverse msgs)

wanderDemon :: Array Pos Terrain -> Int -> WorldDemon -> WorldDemon
wanderDemon wm tick wd =
  let (sx, sy) = wdSpawn wd
      (px, py) = wdPos wd
      tgt = (sx + ((hash2 (wdId wd) tick `mod` 9) - 4), sy + ((hash2 (wdId wd * 7) tick `mod` 9) - 4))
      (tx, ty) = (clamp (sx - 4) (sx + 4) (fst tgt), clamp (sy - 4) (sy + 4) (snd tgt))
      stepX = signum (tx - px)
      stepY = signum (ty - py)
      (nx, ny)
        | stepX /= 0 && inB (px + stepX, py) && isPassable (wm ! (px + stepX, py)) = (px + stepX, py)
        | stepY /= 0 && inB (px, py + stepY) && isPassable (wm ! (px, py + stepY)) = (px, py + stepY)
        | otherwise = (px, py)
   in wd {wdPos = (nx, ny)}
  where
    inB (x, y) = x >= 0 && y >= 0 && x < mapW && y < mapH

stepDemons :: GameState -> GameState
stepDemons gs =
  let tick = animTick gs
      md = maps gs Map.! currentMap gs
      wm = mdTerrain md
      fid = case screen gs of Fighting bs -> Just (bEnemyId bs); _ -> Nothing
      moved = Map.map (\wd -> if Just (wdId wd) == fid then wd else wanderDemon wm (tick + wdId wd * 13) wd) (demons gs)
   in gs {demons = moved}

-- ===========================================================================

-- BATTLE LOGIC
-- ===========================================================================

calcDamage :: Combatant -> Combatant -> Move -> Int
calcDamage atk def mv =
  let (_, dt, pw) = moveInfo mv
      a = case dt of Physical -> combPhyAtk atk; Magical -> combMagAtk atk
      d = case dt of Physical -> combPhyDef def; Magical -> combMagDef def
   in max 1 $ pw * a `div` max 1 d

dealToEnemy :: Int -> BattleState -> BattleState
dealToEnemy dmg bs = bs {bEnemy = (bEnemy bs) {combHP = max 0 (combHP (bEnemy bs) - dmg)}, hitFlash = 5}

dealToPlayerSlot :: Int -> Int -> BattleState -> BattleState
dealToPlayerSlot 0 dmg bs = bs {bPlayer = (bPlayer bs) {combHP = max 0 (combHP (bPlayer bs) - dmg)}, allyFlash = 5}
dealToPlayerSlot i dmg bs =
  let j = i - 1
      allies = bAllies bs
      a' = (allies !! j) {combHP = max 0 (combHP (allies !! j) - dmg)}
   in bs {bAllies = take j allies ++ [a'] ++ drop (j + 1) allies, allyFlash = 5}

blogMsg :: String -> BattleState -> BattleState
blogMsg msg bs = bs {bLog = msg : bLog bs}

-- Update a combatant in a given slot (0 = player, 1+ = allies)
updateCombatantInSlot :: Int -> Combatant -> BattleState -> BattleState
updateCombatantInSlot 0 c bs = bs {bPlayer = c}
updateCombatantInSlot i c bs =
  let j = i - 1
      allies = bAllies bs
   in bs {bAllies = take j allies ++ [c] ++ drop (j + 1) allies}

checkBattleEnd :: BattleState -> BattleState
checkBattleEnd bs
  | combHP (bEnemy bs) <= 0 = bs {bPhase = BattleOver True}
  | combHP (bPlayer bs) <= 0 = bs {bPhase = BattleOver False}
  | otherwise = bs

playerSlotCombatant :: BattleState -> Int -> Maybe Combatant
playerSlotCombatant bs 0 = Just (bPlayer bs)
playerSlotCombatant bs i | i >= 1 && i - 1 < length (bAllies bs) = Just (bAllies bs !! (i - 1)) | otherwise = Nothing

enemySlotCombatant :: BattleState -> Int -> Maybe Combatant
enemySlotCombatant bs 0 = Just (bEnemy bs)
enemySlotCombatant bs i | i >= 1 && i - 1 < length (bEnemyAllies bs) = Just (bEnemyAllies bs !! (i - 1)) | otherwise = Nothing

nextPlayerSlot :: BattleState -> Int -> BattlePhase
nextPlayerSlot bs slot =
  let maxSlot = length (bAllies bs)
      nextI = find (\i -> maybe False (\c -> combHP c > 0) (playerSlotCombatant bs i)) [(slot + 1) .. maxSlot]
   in case nextI of Just i -> PlayerMenu i; Nothing -> EnemyTurn 0 (combName (bEnemy bs) ++ "'s turn!")

nextEnemySlot :: BattleState -> Int -> BattlePhase
nextEnemySlot bs slot =
  let maxSlot = length (bEnemyAllies bs)
      nextI = find (\i -> maybe False (\c -> combHP c > 0) (enemySlotCombatant bs i)) [(slot + 1) .. maxSlot]
   in case nextI of Just i -> EnemyTurn i (combName (fromJust (enemySlotCombatant bs i)) ++ "'s turn!"); Nothing -> PlayerMenu 0

enemyAction :: Int -> BattleState -> BattleState
enemyAction slot bs = case enemySlotCombatant bs slot of
  Nothing -> bs {bPhase = nextEnemySlot bs slot}
  Just e | combHP e <= 0 -> bs {bPhase = nextEnemySlot bs slot}
  Just e ->
    let mv = combMoves e !! (length (bLog bs) `mod` length (combMoves e))
        (nm, _, _) = moveInfo mv
        aliveSlots = filter (\i -> maybe False (\c -> combHP c > 0) (playerSlotCombatant bs i)) (0 : [1 .. length (bAllies bs)])
        tgtSlot = head aliveSlots
        tgt = fromJust (playerSlotCombatant bs tgtSlot)
        dmg = calcDamage e tgt mv
        bs1 = blogMsg (combName e ++ " uses " ++ nm ++ "! " ++ show dmg ++ " dmg") bs
        bs2 = checkBattleEnd (dealToPlayerSlot tgtSlot dmg bs1)
     in case bPhase bs2 of BattleOver _ -> bs2; _ -> bs2 {bPhase = nextEnemySlot bs2 slot}

cycleNext :: (Eq a, Enum a, Bounded a) => a -> a
cycleNext x = if x == maxBound then minBound else succ x

cyclePrev :: (Eq a, Enum a, Bounded a) => a -> a
cyclePrev x = if x == minBound then maxBound else pred x

handleMenuSelect :: GameState -> BattleState -> Int -> GameState
handleMenuSelect gs bs slot = case bMenuSel bs of
  MenuFight -> gs {screen = Fighting bs {bPhase = PlayerFight slot 0}}
  MenuSummon
    | slot /= 0 -> gs
    | null (playerParty (player gs)) -> gs {screen = Fighting (blogMsg "No demons in party!" bs)}
    | length (bAllies bs) >= 3 -> gs {screen = Fighting (blogMsg "Demon slots full! (max 3)" bs)}
    | otherwise -> gs {screen = Fighting bs {bPhase = PlayerSummon 0}}
  MenuRecruit | slot /= 0 -> gs | otherwise -> tryRecruit gs bs
  MenuBag ->
    let pl = player gs
        potions = playerPotions pl
     in if potions <= 0
          then gs {screen = Fighting (blogMsg "No potions left!" bs)}
          else
            let healAmt = 50
                actor = fromJust (playerSlotCombatant bs slot)
                healed = min healAmt (combMaxHP actor - combHP actor)
                actor' = actor {combHP = combHP actor + healed}
                bs1 = blogMsg (combName actor ++ " used a Potion! +" ++ show healed ++ " HP") (updateCombatantInSlot slot actor' bs)
                bs2 = bs1 {bPhase = nextPlayerSlot bs1 slot}
                pl' = pl {playerPotions = potions - 1}
             in gs {player = pl', screen = Fighting bs2}
  MenuRun
    | slot == 0 -> gs {screen = Fighting bs {bPhase = BattleOver False}}
    | otherwise -> gs {screen = Fighting bs {bPhase = nextPlayerSlot bs slot}}

tryRecruit :: GameState -> BattleState -> GameState
tryRecruit gs bs =
  let e = bEnemy bs; hpPct = combHP e * 100 `div` combMaxHP e; party = playerParty (player gs)
   in if hpPct > 30
        then gs {screen = Fighting (blogMsg (combName e ++ " won't join yet!") bs)}
        else
          if length party >= 4
            then gs {screen = Fighting (blogMsg "Party full! (max 4)" bs)}
            else
              let kind = fromMaybe SwordDemon (combKind e)
                  -- give the demon its current stats from the battle
                  newStats = DemonStats (combLevel e) (combXP e) (combMoves e)
                  msg = combName e ++ " joined your party!"
               in addMsg msg $
                    gs
                      { player =
                          (player gs)
                            { playerParty = playerParty (player gs) ++ [kind],
                              partyStats = Map.insert kind newStats (partyStats (player gs))
                            },
                        screen = Fighting (blogMsg msg bs) {bPhase = BattleOver True}
                      }

executePlayerMove :: GameState -> BattleState -> Int -> Int -> GameState
executePlayerMove gs bs slot idx =
  let attacker = fromJust (playerSlotCombatant bs slot)
      mv = combMoves attacker !! idx
      (nm, _, _) = moveInfo mv
      dmg = calcDamage attacker (bEnemy bs) mv
      bs1 = dealToEnemy dmg $ blogMsg (combName attacker ++ " uses " ++ nm ++ "! " ++ show dmg ++ " dmg") bs
      bs2 = checkBattleEnd bs1
      bs3 = case bPhase bs2 of BattleOver _ -> bs2; _ -> bs2 {bPhase = nextPlayerSlot bs2 slot}
   in gs {screen = Fighting bs3}

executeSummon :: GameState -> BattleState -> Int -> GameState
executeSummon gs bs idx =
  let kind = playerParty (player gs) !! idx
      tmpl = demonTemplates Map.! kind
      stats = fromMaybe (defaultDemonStats kind) (Map.lookup kind (partyStats (player gs)))
      newDemon = fromTemplateWithStats tmpl stats
      msg = dName tmpl ++ " (Lv" ++ show (dsLevel stats) ++ ") summoned!"
      allies' = bAllies bs ++ [newDemon]
      bs' = (blogMsg msg bs) {bAllies = allies', bPhase = nextPlayerSlot (bs {bAllies = allies'}) 0}
   in gs {screen = Fighting bs'}

-- After battle ends: apply XP, sync player HP, remove dead demon
applyBattleResults :: BattleState -> GameState -> GameState
applyBattleResults bs gs =
  let won = case bPhase bs of BattleOver True -> True; _ -> False
   in if not won
        then gs
        else
          let (pl1, xpMsgs) = distributeXP bs (player gs)
              -- sync player HP back from battle
              pl2 = pl1 {playerHP = combHP (bPlayer bs)}
              gs1 = gs {player = pl2, demons = Map.delete (bEnemyId bs) (demons gs)}
           in addMsgs xpMsgs gs1

handleBattleKey :: V.Key -> GameState -> BattleState -> GameState
handleBattleKey key gs bs = case bPhase bs of
  BattleOver won ->
    let gs1 = gs {screen = Exploring}
        gs2 = applyBattleResults bs gs1
        gs3 = if won then gs2 else addMsg "You fled the battle." gs2
     in gs3
  EnemyTurn slot _ -> gs {screen = Fighting (enemyAction slot bs)}
  PlayerMenu slot -> case key of
    V.KChar 'w' -> gs {screen = Fighting bs {bMenuSel = cyclePrev (bMenuSel bs)}}
    V.KChar 's' -> gs {screen = Fighting bs {bMenuSel = cycleNext (bMenuSel bs)}}
    V.KUp -> gs {screen = Fighting bs {bMenuSel = cyclePrev (bMenuSel bs)}}
    V.KDown -> gs {screen = Fighting bs {bMenuSel = cycleNext (bMenuSel bs)}}
    V.KEnter -> handleMenuSelect gs bs slot
    V.KChar ' ' -> handleMenuSelect gs bs slot
    _ -> gs
  PlayerFight slot idx ->
    let attacker = fromJust (playerSlotCombatant bs slot); maxIdx = length (combMoves attacker) - 1
     in case key of
          V.KUp -> gs {screen = Fighting bs {bPhase = PlayerFight slot (max 0 (idx - 1))}}
          V.KDown -> gs {screen = Fighting bs {bPhase = PlayerFight slot (min maxIdx (idx + 1))}}
          V.KChar 'w' -> gs {screen = Fighting bs {bPhase = PlayerFight slot (max 0 (idx - 1))}}
          V.KChar 's' -> gs {screen = Fighting bs {bPhase = PlayerFight slot (min maxIdx (idx + 1))}}
          V.KLeft -> gs {screen = Fighting bs {bPhase = PlayerMenu slot}}
          V.KEnter -> executePlayerMove gs bs slot idx
          V.KChar ' ' -> executePlayerMove gs bs slot idx
          _ -> gs
  PlayerSummon idx -> case key of
    V.KUp -> gs {screen = Fighting bs {bPhase = PlayerSummon (max 0 (idx - 1))}}
    V.KDown -> gs {screen = Fighting bs {bPhase = PlayerSummon (min (length (playerParty (player gs)) - 1) (idx + 1))}}
    V.KChar 'w' -> gs {screen = Fighting bs {bPhase = PlayerSummon (max 0 (idx - 1))}}
    V.KChar 's' -> gs {screen = Fighting bs {bPhase = PlayerSummon (min (length (playerParty (player gs)) - 1) (idx + 1))}}
    V.KLeft -> gs {screen = Fighting bs {bPhase = PlayerMenu 0}}
    V.KEnter -> executeSummon gs bs idx
    V.KChar ' ' -> executeSummon gs bs idx
    _ -> gs

-- ===========================================================================
-- EXPLORE INPUT
-- ===========================================================================

tryMove :: (Int, Int) -> GameState -> GameState
tryMove (dx, dy) gs =
  let pl = player gs
      (px, py) = playerPos pl
      np = (px + dx, py + dy)
      md = maps gs Map.! currentMap gs
      inB (x, y) = x >= 0 && y >= 0 && x < mapW && y < mapH
   in if not (inB np) || not (isPassable (mdTerrain md ! np))
        then gs {player = pl {playerFacing = (dx, dy)}}
        else gs {player = pl {playerPos = np, playerFacing = (dx, dy)}}

-- Attack a demon, enter a portal, or receive healing from a healer NPC,
-- whichever is in the tile the player is currently facing.
trySpaceAction :: GameState -> GameState
trySpaceAction gs =
  let pl = player gs
      (px, py) = playerPos pl
      (fx, fy) = playerFacing pl
      fp = (px + fx, py + fy)
      md = maps gs Map.! currentMap gs
      mDemon = find (\d -> wdPos d == fp) (Map.elems (demons gs))
      mPortal = find (\p -> portalPos p == fp) (mdPortals md)
      isHealer = fp `elem` mdHealers md
   in case mDemon of
        Just wd -> startBattle gs pl wd
        Nothing -> case mPortal of
          Just portal -> doPortalTransition gs portal
          Nothing ->
            if isHealer
              then tryHealFromNPC gs
              else addMsg "Nothing here to interact with." gs

startBattle :: GameState -> Player -> WorldDemon -> GameState
startBattle gs pl wd =
  let tmpl = demonTemplates Map.! wdKind wd
      bs =
        BattleState
          { bPlayer =
              playerCombatant
                { combHP = playerHP pl,
                  combMaxHP = playerMaxHP pl,
                  combLevel = playerLevel pl,
                  combXP = playerXP pl
                },
            bAllies = [],
            bEnemy = fromTemplate tmpl,
            bEnemyAllies = [],
            bEnemyId = wdId wd,
            bPhase = PlayerMenu 0,
            bMenuSel = MenuFight,
            bLog = ["Encountered " ++ dName tmpl ++ "!"],
            hitFlash = 0,
            allyFlash = 0
          }
   in gs {screen = Fighting bs}

doPortalTransition :: GameState -> Portal -> GameState
doPortalTransition gs portal =
  let destMd = maps gs Map.! portalDest portal
      destPos = portalDestPos portal
      newDemons =
        Map.fromList $
          zip [0 ..] $
            take
              (mdSpawnCap destMd)
              [ WorldDemon i k p p (1, 0) 0
                | (i, (k, p)) <-
                    zip
                      [0 ..]
                      ( zip
                          (cycle (mdSpawnKinds destMd))
                          [(10, 10), (25, 15), (15, 22), (30, 8), (20, 20), (8, 18)]
                      )
              ]
      mapName = case portalDest portal of SwampMap -> "the Fetid Swamp"; CavernMap -> "the Deep Cavern"
   in addMsg ("Entered " ++ mapName ++ "!") $
        gs
          { currentMap = portalDest portal,
            player = (player gs) {playerPos = destPos},
            demons = newDemons,
            nextDmnId = length newDemons,
            spawnTimer = 0
          }

tryHealFromNPC :: GameState -> GameState
tryHealFromNPC gs =
  let pl = player gs
      fullHP = playerMaxHP pl
      healed = fullHP - playerHP pl
      -- Also heal each party demon's persistent stats (reflected in partyStats; combat HP is separate)
      msg = if healed > 0 then "The healer restores your HP to full! (+" ++ show healed ++ ")" else "You are already at full HP."
   in addMsg msg gs {player = pl {playerHP = fullHP}}

-- ===========================================================================
-- EVENT HANDLING
-- ===========================================================================

tickGame :: GameState -> GameState
tickGame gs =
  let gs' = gs {animTick = animTick gs + 1}
   in case screen gs' of
        Fighting bs -> gs' {screen = Fighting bs {hitFlash = max 0 (hitFlash bs - 1), allyFlash = max 0 (allyFlash bs - 1)}}
        _ -> gs'

handleEvent :: BrickEvent Name AppEvent -> EventM Name GameState ()
handleEvent (AppEvent Tick) = modify tickGame
handleEvent (AppEvent DemonTick) = do
  gs <- Brick.get
  let gs1 = case screen gs of
        Fighting bs -> case bPhase bs of
          EnemyTurn slot _ -> gs {screen = Fighting (enemyAction slot bs)}
          _ -> stepDemons gs
        _ -> stepDemons gs
      gs2 = trySpawnDemon gs1
  Brick.put gs2
  -- autosave
  void $ liftIO (doSave gs2)
handleEvent (VtyEvent (V.EvKey V.KEsc _)) = halt
handleEvent (VtyEvent (V.EvKey key _)) = do
  gs <- Brick.get
  case screen gs of
    Exploring -> handleExploreKey key gs
    Fighting bs -> Brick.put $ handleBattleKey key gs bs
handleEvent _ = pure ()

handleExploreKey :: V.Key -> GameState -> EventM Name GameState ()
handleExploreKey key gs = case key of
  V.KChar 'q' -> halt
  V.KChar 'w' -> Brick.put (tryMove (0, -1) gs)
  V.KChar 's' -> Brick.put (tryMove (0, 1) gs)
  V.KChar 'a' -> Brick.put (tryMove (-1, 0) gs)
  V.KChar 'd' -> Brick.put (tryMove (1, 0) gs)
  V.KChar ' ' -> Brick.put (trySpaceAction gs)
  _ -> pure ()

-- ===========================================================================
-- APP
-- ===========================================================================

drawGame :: GameState -> [Widget Name]
drawGame gs = case screen gs of
  Exploring -> renderExplore gs
  Fighting bs -> renderBattle gs bs

app :: App GameState AppEvent Name
app =
  App
    { appDraw = drawGame,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const (attrMap V.defAttr [])
    }

main :: IO ()
main = do
  chan <- newBChan 20
  let ms = buildMaps
  -- try to load save, fall back to fresh state
  mSaved <- doLoad ms
  let gs = fromMaybe (initialState ms) mSaved
  case mSaved of Just _ -> putStrLn "(save loaded)"; Nothing -> pure ()
  forkIO $ forever $ do writeBChan chan Tick; threadDelay 120000
  forkIO $ forever $ do writeBChan chan DemonTick; threadDelay 800000
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app gs
