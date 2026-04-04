{-# LANGUAGE OverloadedStrings #-}

module SMT where

import Brick hiding (clamp)
import Brick.BChan (newBChan, writeBChan)
import qualified Graphics.Vty as V
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Array (Array, listArray, (!))
import Data.List (find, intercalate, nub)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, isJust)
import Data.Bits (xor, shiftR)
import Control.Monad (forever, void, when)
import Control.Concurrent (forkIO, threadDelay)
import qualified Graphics.Vty.CrossPlatform as V

-- ===========================================================================
-- SPRITE SYSTEM (5w x 3h, same as civ)
-- ===========================================================================

sw, sh :: Int
sw = 5; sh = 3

data SCell = SCell Char (Int,Int,Int)
type Sprite = [[SCell]]

sc :: Char -> (Int,Int,Int) -> SCell
sc = SCell

blank5 :: [SCell]
blank5 = replicate sw (SCell ' ' (0,0,0))

lighten :: Int -> (Int,Int,Int) -> (Int,Int,Int)
lighten n (r,g,b) = (min 255 (r+n), min 255 (g+n), min 255 (b+n))

darken :: Int -> (Int,Int,Int) -> (Int,Int,Int)
darken n (r,g,b) = (max 0 (r-n), max 0 (g-n), max 0 (b-n))

renderSprite :: Sprite -> V.Image
renderSprite rows = V.vertCat $ map renderRow rows
  where
    renderRow cells = V.horizCat $ map renderCell cells
    renderCell (SCell ch (r,g,b)) =
      V.char (V.withForeColor V.defAttr (V.rgbColor r g b)) ch

-- Pad a sprite to exact width w (repeating last cell or spaces)
padSpriteW :: Int -> Sprite -> Sprite
padSpriteW w = map padRow
  where
    padRow row = take w (row ++ repeat (SCell ' ' (0,0,0)))

-- Render a raw string as a Vty image with given attr
vStr :: V.Attr -> String -> V.Image
vStr = V.string

-- ===========================================================================
-- TERRAIN SPRITES  (swamp theme)
-- ===========================================================================

-- Murky water / bog
bogSpr :: Int -> Sprite
bogSpr tick =
  let p = tick `mod` 4
      (c1,c2) = case p of
        0 -> ('~','\''); 1 -> ('\'','~'); 2 -> ('-','~'); _ -> ('~','-')
  in [ [sc c1 d, sc c2 l, sc c1 d, sc c2 l, sc c1 d]
     , [sc c2 l, sc c1 d, sc c2 l, sc c1 d, sc c2 l]
     , [sc c1 d, sc c2 l, sc c1 d, sc c2 l, sc c1 d]
     ]
  where d=(50,90,60); l=(70,120,80)

-- Muddy ground
mudSpr :: Sprite
mudSpr =
  [ [sc '.' mg, sc ',' mg, sc '.' dg, sc '\'' mg, sc '.' mg]
  , [sc ',' dg, sc '.' mg, sc ',' mg, sc '.' dg, sc '\'' mg]
  , [sc '.' mg, sc '\'' mg, sc '.' mg, sc ',' dg, sc '.' mg]
  ]
  where mg=(90,75,50); dg=(70,58,35)

-- Dead tree / gnarled stump
deadTreeSpr :: Sprite
deadTreeSpr =
  [ [sc ' ' k, sc 'Y' b, sc ' ' k, sc '/' b, sc ' ' k]
  , [sc ' ' k, sc '|' b, sc ' ' k, sc ' ' k, sc ' ' k]
  , [sc '.' mg, sc '|' d, sc '.' mg, sc ' ' k, sc ' ' k]
  ]
  where b=(80,65,40); d=(60,48,28); mg=(80,68,45); k=(0,0,0)

-- Thick swamp grass / reeds
reedsSpr :: Sprite
reedsSpr =
  [ [sc '|' r, sc ' ' k, sc '|' r, sc ' ' k, sc '|' r]
  , [sc '|' d, sc '\'' g, sc '|' d, sc '\'' g, sc '|' d]
  , [sc '.' mg, sc '.' mg, sc ',' mg, sc '.' mg, sc '.' mg]
  ]
  where r=(60,120,50); d=(45,95,38); g=(80,150,60); mg=(80,70,45); k=(0,0,0)

-- Murky puddle / stepping stone
puddleSpr :: Sprite
puddleSpr =
  [ [sc '.' mg, sc '.' mg, sc '.' mg, sc '.' mg, sc '.' mg]
  , [sc '.' mg, sc '~' w, sc '~' w, sc '.' mg, sc '.' mg]
  , [sc '.' mg, sc '.' mg, sc '.' mg, sc '.' mg, sc '.' mg]
  ]
  where mg=(85,72,48); w=(60,100,75)

-- Stone rubble / ruin
stoneSpr :: Sprite
stoneSpr =
  [ [sc '#' s, sc ' ' k, sc '#' d, sc ' ' k, sc '#' s]
  , [sc ' ' k, sc '#' d, sc '#' s, sc '#' d, sc ' ' k]
  , [sc '#' d, sc '#' s, sc ' ' k, sc '#' d, sc '#' s]
  ]
  where s=(110,105,100); d=(80,76,72); k=(0,0,0)

terrainSpr :: Int -> Terrain -> Sprite
terrainSpr tick t = case t of
  Bog      -> bogSpr tick
  Mud      -> mudSpr
  DeadTree -> deadTreeSpr
  Reeds    -> reedsSpr
  Puddle   -> puddleSpr
  Stone    -> stoneSpr

-- ===========================================================================
-- ENTITY SPRITES
-- ===========================================================================

-- Player: hooded figure
playerSpr :: Sprite
playerSpr =
  [ [sc ' ' k, sc '(' p, sc 'o' p, sc ')' p, sc ' ' k]
  , [sc ' ' k, sc '[' c, sc '|' c, sc ']' c, sc ' ' k]
  , [sc ' ' k, sc '/' c, sc ' ' c, sc '\\' c, sc ' ' k]
  ]
  where p=(200,185,160); c=(50,80,140); k=(0,0,0)

-- Sword Demon: horned swordsman
swordDemonSpr :: Sprite
swordDemonSpr =
  [ [sc '/' h, sc 'D' r, sc 'D' r, sc '\\' h, sc '|' s]
  , [sc ' ' k, sc '(' r, sc '@' e, sc ')' r, sc '-' s]
  , [sc ' ' k, sc '|' d, sc '|' d, sc ' ' k, sc ' ' k]
  ]
  where h=(200,60,60); r=(170,40,40); e=(255,100,50); d=(130,30,30); s=(190,190,210); k=(0,0,0)

-- Plague Demon: bloated, toxic
plagueDemonSpr :: Sprite
plagueDemonSpr =
  [ [sc ' ' k, sc 'o' g, sc 'O' lg, sc 'o' g, sc ' ' k]
  , [sc '(' g, sc '*' lg, sc '$' y, sc '*' lg, sc ')' g]
  , [sc ' ' k, sc '~' g, sc '~' dg, sc '~' g, sc ' ' k]
  ]
  where g=(60,130,50); lg=(90,170,70); dg=(40,100,35); y=(180,200,50); k=(0,0,0)

-- Shadow Demon: wispy
shadowDemonSpr :: Sprite
shadowDemonSpr =
  [ [sc ' ' k, sc ')' v, sc 'o' p, sc '(' v, sc ' ' k]
  , [sc '~' v, sc ' ' k, sc '|' dv, sc ' ' k, sc '~' v]
  , [sc ' ' k, sc '~' dv, sc ' ' k, sc '~' dv, sc ' ' k]
  ]
  where v=(120,80,180); dv=(80,50,130); p=(200,160,255); k=(0,0,0)

demonSprite :: DemonKind -> Sprite
demonSprite SwordDemon  = swordDemonSpr
demonSprite PlagueDemon = plagueDemonSpr
demonSprite ShadowDemon = shadowDemonSpr

-- Large battle sprites (10w x 6h) -- double-sized for battle screen
largeSpr :: Sprite -> Sprite
largeSpr spr =
  concatMap (\row -> let r2 = concatMap (\c -> [c,c]) row
                     in [r2, r2]) spr

-- ===========================================================================
-- TYPES
-- ===========================================================================

type Pos = (Int,Int)

data Terrain = Bog | Mud | DeadTree | Reeds | Puddle | Stone
  deriving (Eq,Show)

data DemonKind = SwordDemon | PlagueDemon | ShadowDemon
  deriving (Eq,Show,Ord)

data Move = SwordAttack | FireAttack | PoisonCloud | DarkSlash
  deriving (Eq,Show)

data DamageType = Physical | Magical
  deriving (Eq,Show)

moveInfo :: Move -> (String, DamageType, Int)
moveInfo SwordAttack = ("Sword Atk", Physical, 35)
moveInfo FireAttack  = ("Fire Atk",  Magical,  45)
moveInfo PoisonCloud = ("Poison",    Magical,  30)
moveInfo DarkSlash   = ("Dark Slash",Physical, 40)

data DemonTemplate = DemonTemplate
  { dKind    :: DemonKind
  , dName    :: String
  , dMaxHP   :: Int
  , dPhyAtk  :: Int
  , dMagAtk  :: Int
  , dPhyDef  :: Int
  , dMagDef  :: Int
  , dMoves   :: [Move]
  }

demonTemplates :: Map DemonKind DemonTemplate
demonTemplates = Map.fromList
  [ (SwordDemon,  DemonTemplate SwordDemon  "Kelpie"   80  45 25 30 15 [SwordAttack, FireAttack])
  , (PlagueDemon, DemonTemplate PlagueDemon "Boggart"  70  25 40 20 30 [PoisonCloud, FireAttack])
  , (ShadowDemon, DemonTemplate ShadowDemon "Umbra"    65  35 45 20 35 [DarkSlash,   FireAttack])
  ]

-- A combatant in battle (player or demon)
data Combatant = Combatant
  { combName   :: String
  , combHP     :: Int
  , combMaxHP  :: Int
  , combPhyAtk :: Int
  , combMagAtk :: Int
  , combPhyDef :: Int
  , combMagDef :: Int
  , combMoves  :: [Move]
  , combKind   :: Maybe DemonKind   -- Nothing = player
  } deriving (Eq,Show)

playerCombatant :: Combatant
playerCombatant = Combatant
  { combName   = "Hayato"
  , combHP     = 120
  , combMaxHP  = 120
  , combPhyAtk = 40
  , combMagAtk = 20
  , combPhyDef = 25
  , combMagDef = 20
  , combMoves  = [SwordAttack]
  , combKind   = Nothing
  }

fromTemplate :: DemonTemplate -> Combatant
fromTemplate t = Combatant
  { combName   = dName t
  , combHP     = dMaxHP t
  , combMaxHP  = dMaxHP t
  , combPhyAtk = dPhyAtk t
  , combMagAtk = dMagAtk t
  , combPhyDef = dPhyDef t
  , combMagDef = dMagDef t
  , combMoves  = dMoves t
  , combKind   = Just (dKind t)
  }

-- World demons (wandering)
data WorldDemon = WorldDemon
  { wdId       :: Int
  , wdKind     :: DemonKind
  , wdPos      :: Pos
  , wdSpawn    :: Pos
  , wdFacing   :: (Int,Int)
  , wdStepCtr  :: Int
  } deriving (Eq,Show)

-- Battle menu focus
data BattleMenu = MenuFight | MenuSummon | MenuBag | MenuRun | MenuRecruit
  deriving (Eq,Show,Enum,Bounded)

-- Which move is highlighted in fight submenu
data BattlePhase
  = PlayerMenu                    -- main 4-option menu
  | PlayerFight Int               -- move selection, index
  | PlayerSummon Int              -- summon slot selection
  | EnemyTurn String              -- showing enemy action message
  | BattleOver Bool               -- True=won, False=fled
  deriving (Eq,Show)

data BattleState = BattleState
  { bPlayer    :: Combatant
  , bAlly      :: Maybe Combatant   -- summoned demon (if any)
  , bEnemy     :: Combatant
  , bEnemyId   :: Int               -- which world demon
  , bPhase     :: BattlePhase
  , bMenuSel   :: BattleMenu
  , bLog       :: [String]          -- newest first
  }

data Screen = Exploring | Fighting BattleState

-- Player on map
data Player = Player
  { playerPos    :: Pos
  , playerFacing :: (Int,Int)
  , playerHP     :: Int
  , playerMaxHP  :: Int
  , playerParty  :: [DemonKind]  -- recruited demons (max 4)
  } deriving (Eq,Show)

data GameState = GameState
  { worldMap   :: Array Pos Terrain
  , player     :: Player
  , demons     :: Map Int WorldDemon
  , screen     :: Screen
  , animTick   :: Int
  , msgLog     :: [String]
  , nextDmnId  :: Int
  }

data AppEvent = Tick | DemonTick
type Name = ()

-- ===========================================================================
-- MAP GENERATION
-- ===========================================================================

mapW, mapH :: Int
mapW = 40; mapH = 30

hash2 :: Int -> Int -> Int
hash2 x y =
  let h  = x * 374761393 + y * 1103515245
      h2 = h `xor` (h `shiftR` 13)
      h3 = h2 * 1664525
  in h3 `xor` (h3 `shiftR` 16)

valueNoise :: Int -> Int -> Int
valueNoise x y = abs (hash2 x y) `mod` 1001

smoothNoise :: Int -> Int -> Int -> Int
smoothNoise scale x y =
  let gx=x`div`scale; gy=y`div`scale
      tx=(x`mod`scale)*1000`div`scale
      ty=(y`mod`scale)*1000`div`scale
      v00=valueNoise gx gy; v10=valueNoise(gx+1) gy
      v01=valueNoise gx(gy+1); v11=valueNoise(gx+1)(gy+1)
      lerp a b t = a+(b-a)*t`div`1000
  in lerp (lerp v00 v10 tx) (lerp v01 v11 tx) ty

genTerrain :: Int -> Int -> Terrain
genTerrain x y =
  let e = (smoothNoise 8 x y * 600 + smoothNoise 4 x y * 300 + smoothNoise 2 x y * 100) `div` 1000
      d = valueNoise (x+37) (y+91) `mod` 100
  in if e < 300 then Bog
     else if e < 500 then if d < 15 then Puddle else Mud
     else if e < 700 then if d < 10 then Reeds else if d < 20 then DeadTree else Mud
     else if d < 20 then Stone else Reeds

buildWorldMap :: Array Pos Terrain
buildWorldMap = listArray ((0,0),(mapW-1,mapH-1))
  [ genTerrain x y | y <- [0..mapH-1], x <- [0..mapW-1] ]

isPassable :: Terrain -> Bool
isPassable Bog  = False
isPassable Stone = False
isPassable _    = True

initialDemons :: Map Int WorldDemon
initialDemons = Map.fromList $ zip [0..] $
  [ WorldDemon i k p p (1,0) 0
  | (i,k,p) <- [ (0, SwordDemon,  (15,10))
               , (1, PlagueDemon, (25,18))
               , (2, ShadowDemon, (10,22))
               , (3, SwordDemon,  (30,8))
               , (4, PlagueDemon, (20,25))
               ]
  ]

initialState :: GameState
initialState = GameState
  { worldMap  = buildWorldMap
  , player    = Player (5,5) (1,0) 120 120 []
  , demons    = initialDemons
  , screen    = Exploring
  , animTick  = 0
  , msgLog    = ["You enter the fetid swamp...", "SPACE: attack demon in front · TAB: nothing here"]
  , nextDmnId = 5
  }

-- ===========================================================================
-- RENDERING: EXPLORE MODE
-- ===========================================================================

-- Viewport: tiles visible around player
vTilesW, vTilesH :: Int
vTilesW = 13; vTilesH = 11

viewW, viewH :: Int
viewW = vTilesW * sw; viewH = vTilesH * sh

-- Camera: player always centered
camOrigin :: Pos -> (Int,Int)
camOrigin (px,py) =
  ( clamp 0 (mapW-vTilesW) (px - vTilesW`div`2)
  , clamp 0 (mapH-vTilesH) (py - vTilesH`div`2) )

renderExplore :: GameState -> [Widget Name]
renderExplore gs =
  [ raw $ V.horizCat [ mapImg, V.char V.defAttr ' ', sidebarImg ] ]
  where
    pl    = player gs
    (cx,cy) = camOrigin (playerPos pl)
    tick  = animTick gs
    mapImg = V.vertCat
      [ buildMapRow gs tick cx (cy+ty)
      | ty <- [0..vTilesH-1] ]
    sidebarImg = renderExploreSidebar gs

buildMapRow :: GameState -> Int -> Int -> Int -> V.Image
buildMapRow gs tick cx wy =
  let sprites    = [ getTileSprite gs tick (cx+tx) wy | tx <- [0..vTilesW-1] ]
      rowsPerSpr = [ map (!!r) sprites | r <- [0..sh-1] ]
      scImg (SCell ch (r,g,b)) = V.char (V.withForeColor V.defAttr (V.rgbColor r g b)) ch
      rowImg cells = V.horizCat (map scImg cells)
      tileRowImg tileRows = V.horizCat (map rowImg tileRows)
  in V.vertCat (map tileRowImg rowsPerSpr)

getTileSprite :: GameState -> Int -> Int -> Int -> Sprite
getTileSprite gs tick wx wy
  | wx < 0 || wy < 0 || wx >= mapW || wy >= mapH
  = replicate sh (replicate sw (SCell ' ' (5,5,15)))
  | otherwise =
      let pos    = (wx,wy)
          pl     = player gs
          mDemon = listToMaybe [ d | d <- Map.elems (demons gs), wdPos d == pos ]
          isPlayer = playerPos pl == pos
          base   = terrainSpr tick (worldMap gs ! pos)
      in if isPlayer    then overlayEntity playerSpr base
         else case mDemon of
           Just wd -> overlayEntity (demonSprite (wdKind wd)) base
           Nothing -> base

-- Overlay entity sprite over terrain (entity non-black cells win)
overlayEntity :: Sprite -> Sprite -> Sprite
overlayEntity entity terrain =
  zipWith (zipWith mergeCell) entity terrain
  where
    mergeCell e@(SCell _ (0,0,0)) t = t
    mergeCell e _                   = e

renderExploreSidebar :: GameState -> V.Image
renderExploreSidebar gs =
  let pl   = player gs
      bold a = V.withStyle a V.bold
      fg r g b = V.withForeColor V.defAttr (V.rgbColor r g b)
      dim  = fg 130 130 130
      grn  = fg 80 200 100
      red  = fg 200 80  80
      yel  = fg 220 200 70
      sep  = V.string dim (replicate 28 '─')
      blank = V.string V.defAttr ""

      hdr  = V.string (bold (fg 180 160 220)) " ◆ DAEMON HUNT"
      hpBar = let pct = playerHP pl * 20 `div` playerMaxHP pl
                  bar = replicate pct '█' ++ replicate (20-pct) '░'
              in V.string grn $ " HP " ++ bar

      hpNum = V.string V.defAttr $
                "    " ++ show (playerHP pl) ++ "/" ++ show (playerMaxHP pl)

      partyHdr = V.string (bold V.defAttr) " Party"
      partyLines = if null (playerParty pl)
        then [V.string dim "  (no demons)"]
        else zipWith (\i k -> V.string yel $ "  " ++ show i ++ ". " ++ dName (demonTemplates Map.! k))
               [1..] (playerParty pl)

      ctrlHdr = V.string (bold V.defAttr) " Controls"
      ctrls =
        [ V.string V.defAttr "  WASD   move"
        , V.string V.defAttr "  SPACE  attack ahead"
        , V.string V.defAttr "  q      quit"
        ]

      logHdr  = V.string (bold V.defAttr) " Log"
      logLines = map (\l -> V.string dim ("  " ++ take 26 l)) (take 6 (msgLog gs))

  in V.vertCat $
     [hdr, sep, hpBar, hpNum, blank, sep, partyHdr]
     ++ partyLines ++
     [blank, sep, ctrlHdr] ++ ctrls ++
     [blank, sep, logHdr] ++ logLines

-- ===========================================================================
-- RENDERING: BATTLE SCREEN
-- ===========================================================================

battleW, battleH :: Int
battleW = 80; battleH = 30

renderBattle :: GameState -> BattleState -> [Widget Name]
renderBattle gs bs =
  [ raw battleImg ]
  where
    battleImg = V.vertCat
      [ topBanner
      , combatRow
      , divider
      , menuRow
      ]

    fg r g b = V.withForeColor V.defAttr (V.rgbColor r g b)
    bold a   = V.withStyle a V.bold
    dim      = fg 130 130 130
    red      = fg 220 80  60
    grn      = fg 80  200 100
    yel      = fg 220 200 60
    cyn      = fg 80  200 210
    wht      = fg 220 220 220

    topBanner =
      V.string (bold (fg 180 80 80)) "  ══ BATTLE ══" V.<|>
      V.string dim (replicate 60 ' ')

    -- Combat row: enemy top-left, ally/empty bottom-right area
    combatRow = V.horizCat [enemyPanel, V.string V.defAttr "  ", allyPanel]

    enemyPanel =
      let e   = bEnemy bs
          spr = largeSpr (demonSprite (fromMaybe SwordDemon (combKind e)))
          sprImg = V.vertCat $ map renderSprRow spr
          hpPct = combHP e * 16 `div` combMaxHP e
          hpBar = replicate hpPct '█' ++ replicate (16-hpPct) '░'
          nameL = V.string (bold red) $ "  " ++ combName e
          statsL = V.string dim $
            "  HP " ++ hpBar ++ " " ++ show (combHP e) ++ "/" ++ show (combMaxHP e)
          physL = V.string dim $
            "  PHY:" ++ show (combPhyAtk e) ++ " MAG:" ++ show (combMagAtk e)
          defL  = V.string dim $
            "  DEF:" ++ show (combPhyDef e) ++ " MDEF:" ++ show (combMagDef e)
      in V.vertCat [nameL, statsL, physL, defL, V.string V.defAttr "", sprImg]

    allyPanel =
      let p   = bPlayer bs
          hpPct = combHP p * 16 `div` combMaxHP p
          hpBar = replicate hpPct '█' ++ replicate (16-hpPct) '░'
          nameL  = V.string (bold cyn) $ combName p
          statsL = V.string dim $ "HP " ++ hpBar ++ " " ++ show (combHP p) ++ "/" ++ show (combMaxHP p)
          allyInfo = case bAlly bs of
            Nothing -> [V.string dim "(no summon)"]
            Just a  ->
              let apct = combHP a * 16 `div` combMaxHP a
                  abar = replicate apct '█' ++ replicate (16-apct) '░'
              in [ V.string (bold yel) $ combName a
                 , V.string dim $ "HP " ++ abar ++ " " ++ show (combHP a) ++ "/" ++ show (combMaxHP a)
                 ]
          sprImg = V.vertCat $ map renderSprRow (largeSpr playerSpr)
      in V.vertCat $ [nameL, statsL, V.string V.defAttr ""] ++ allyInfo ++ [V.string V.defAttr "", sprImg]

    renderSprRow row = V.horizCat $ map (\(SCell ch (r,g,b)) ->
      V.char (V.withForeColor V.defAttr (V.rgbColor r g b)) ch) row

    divider = V.string dim (replicate battleW '─')

    menuRow = V.horizCat [logPanel, V.string V.defAttr "  ", actionPanel]

    logPanel =
      let ls = take 4 (bLog bs)
          padded = ls ++ replicate (4 - length ls) ""
      in V.vertCat $ map (\l -> V.string dim $ " " ++ take 35 l) padded

    actionPanel = case bPhase bs of
      PlayerMenu      -> mainMenuImg
      PlayerFight idx -> fightMenuImg idx
      PlayerSummon idx -> summonMenuImg idx
      EnemyTurn msg   -> V.vertCat
        [ V.string (fg 200 120 80) $ " " ++ msg
        , V.string dim " (press any key)"
        ]
      BattleOver True  -> V.string grn " ★ Victory! (any key)"
      BattleOver False -> V.string dim " ★ Fled! (any key)"

    mainMenuImg =
      let items = [ (MenuFight,   "FIGHT")
                  , (MenuSummon,  "SUMMON")
                  , (MenuRecruit, "RECRUIT")
                  , (MenuBag,     "BAG")
                  , (MenuRun,     "RUN")
                  ]
          sel = bMenuSel bs
      in V.vertCat $ map (\(m,lbl) ->
           let a = if m == sel
                   then bold (fg 255 230 80)
                   else fg 170 170 170
           in V.string a $ (if m==sel then " ▸ " else "   ") ++ lbl
         ) items

    fightMenuImg idx =
      let moves = combMoves (bPlayer bs)
      in V.vertCat $
         V.string (bold wht) " Choose move:" :
         zipWith (\i mv ->
           let (nm,dt,pw) = moveInfo mv
               a = if i==idx then bold (fg 255 230 80) else fg 170 170 170
               dtStr = case dt of Physical -> "PHY"; Magical -> "MAG"
           in V.string a $ (if i==idx then " ▸ " else "   ") ++ nm ++ " [" ++ dtStr ++ " " ++ show pw ++ "]"
         ) [0..] moves
         ++ [V.string dim "   (ESC back)"]

    summonMenuImg idx =
      let party = playerParty (player gs)
      in V.vertCat $
         V.string (bold wht) " Summon demon:" :
         if null party
           then [V.string dim "   (party empty)"]
           else zipWith (\i k ->
             let t = demonTemplates Map.! k
                 a = if i==idx then bold (fg 255 230 80) else fg 170 170 170
             in V.string a $ (if i==idx then " ▸ " else "   ") ++ dName t
           ) [0..] party
         ++ [V.string dim "   (ESC back)"]

-- ===========================================================================
-- DEMON WANDERING
-- ===========================================================================

clamp :: Int -> Int -> Int -> Int
clamp lo hi x = max lo (min hi x)

addMsg :: String -> GameState -> GameState
addMsg msg gs = gs { msgLog = msg : msgLog gs }

-- Move demons one step toward a random-ish wander target within 4 tiles of spawn
wanderDemon :: Array Pos Terrain -> Int -> WorldDemon -> WorldDemon
wanderDemon wm tick wd =
  let (sx,sy) = wdSpawn wd
      (px,py) = wdPos wd
      -- pick target based on tick + id for variety
      tgt     = ( sx + ((hash2 (wdId wd) tick `mod` 9) - 4)
                , sy + ((hash2 (wdId wd * 7) tick `mod` 9) - 4) )
      (tx,ty) = ( clamp (sx-4) (sx+4) (fst tgt)
                , clamp (sy-4) (sy+4) (snd tgt) )
      -- step one tile toward target
      stepX   = signum (tx - px)
      stepY   = signum (ty - py)
      -- try horizontal then vertical
      (nx,ny)
        | stepX /= 0 && inBounds (px+stepX,py) && isPassable (wm!(px+stepX,py))
            = (px+stepX, py)
        | stepY /= 0 && inBounds (px,py+stepY) && isPassable (wm!(px,py+stepY))
            = (px, py+stepY)
        | otherwise = (px,py)
  in wd { wdPos = (nx,ny) }
  where
    inBounds (x,y) = x>=0 && y>=0 && x<mapW && y<mapH

stepDemons :: GameState -> GameState
stepDemons gs =
  let tick = animTick gs
      wm   = worldMap gs
      -- don't move demon that is in battle
      fightingId = case screen gs of
        Fighting bs -> Just (bEnemyId bs)
        _           -> Nothing
      moved = Map.map (\wd ->
        if Just (wdId wd) == fightingId then wd
        else wanderDemon wm (tick + wdId wd * 13) wd
        ) (demons gs)
  in gs { demons = moved }

-- ===========================================================================
-- BATTLE LOGIC
-- ===========================================================================

calcDamage :: Combatant -> Combatant -> Move -> Int
calcDamage attacker defender mv =
  let (_, dt, basePow) = moveInfo mv
      atk  = case dt of Physical -> combPhyAtk attacker; Magical -> combMagAtk attacker
      def  = case dt of Physical -> combPhyDef defender; Magical -> combMagDef defender
      dmg  = max 1 $ basePow * atk `div` max 1 def
  in dmg

-- Apply damage to enemy
dealToEnemy :: Int -> BattleState -> BattleState
dealToEnemy dmg bs =
  let e  = bEnemy bs
      e' = e { combHP = max 0 (combHP e - dmg) }
  in bs { bEnemy = e' }

dealToPlayer :: Int -> BattleState -> BattleState
dealToPlayer dmg bs =
  let p  = bPlayer bs
      p' = p { combHP = max 0 (combHP p - dmg) }
  in bs { bPlayer = p' }

dealToAlly :: Int -> BattleState -> BattleState
dealToAlly dmg bs = case bAlly bs of
  Nothing -> bs
  Just a  ->
    let a' = a { combHP = max 0 (combHP a - dmg) }
    in bs { bAlly = Just a' }

blogMsg :: String -> BattleState -> BattleState
blogMsg msg bs = bs { bLog = msg : bLog bs }

-- Enemy picks a move and attacks
enemyAction :: BattleState -> BattleState
enemyAction bs =
  let e   = bEnemy bs
      mvs = combMoves e
      mv  = mvs !! (length (bLog bs) `mod` length mvs)
      (nm,_,_) = moveInfo mv
      target = case bAlly bs of
        Just a | combHP a > 0 -> a
        _                      -> bPlayer bs
      dmg = calcDamage e target mv
      msg = combName e ++ " uses " ++ nm ++ "! " ++ show dmg ++ " dmg"
      bs1 = blogMsg msg bs
      bs2 = if combName target == combName (bPlayer bs)
            then dealToPlayer dmg bs1
            else dealToAlly dmg bs1
  in bs2 { bPhase = PlayerMenu }

-- Check win/loss after player action
checkBattleEnd :: BattleState -> BattleState
checkBattleEnd bs
  | combHP (bEnemy bs) <= 0 = bs { bPhase = BattleOver True }
  | combHP (bPlayer bs) <= 0 = bs { bPhase = BattleOver False }
  | otherwise = bs

-- Handle battle input
handleBattleKey :: V.Key -> GameState -> BattleState -> GameState
handleBattleKey key gs bs = case bPhase bs of

  BattleOver won ->
    let gs1 = gs { screen = Exploring }
        gs2 = if won
              then addMsg ("Defeated " ++ combName (bEnemy bs) ++ "!") $
                   gs1 { demons = Map.delete (bEnemyId bs) (demons gs1) }
              else addMsg "You fled the battle." gs1
    in gs2

  EnemyTurn _ ->
    let bs1 = enemyAction bs
        bs2 = checkBattleEnd bs1
    in gs { screen = Fighting bs2 }

  PlayerMenu -> case key of
    V.KChar 'w' -> gs { screen = Fighting bs { bMenuSel = cyclePrev (bMenuSel bs) } }
    V.KChar 's' -> gs { screen = Fighting bs { bMenuSel = cycleNext (bMenuSel bs) } }
    V.KUp       -> gs { screen = Fighting bs { bMenuSel = cyclePrev (bMenuSel bs) } }
    V.KDown     -> gs { screen = Fighting bs { bMenuSel = cycleNext (bMenuSel bs) } }
    V.KEnter    -> handleMenuSelect gs bs
    V.KChar ' ' -> handleMenuSelect gs bs
    _           -> gs

  PlayerFight idx -> case key of
    V.KUp    -> gs { screen = Fighting bs { bPhase = PlayerFight (max 0 (idx-1)) } }
    V.KDown  -> gs { screen = Fighting bs { bPhase = PlayerFight (min (length (combMoves (bPlayer bs))-1) (idx+1)) } }
    V.KChar 'w' -> gs { screen = Fighting bs { bPhase = PlayerFight (max 0 (idx-1)) } }
    V.KChar 's' -> gs { screen = Fighting bs { bPhase = PlayerFight (min (length (combMoves (bPlayer bs))-1) (idx+1)) } }
    V.KEsc   -> gs { screen = Fighting bs { bPhase = PlayerMenu } }
    V.KEnter -> executePlayerMove gs bs idx
    V.KChar ' ' -> executePlayerMove gs bs idx
    _        -> gs

  PlayerSummon idx -> case key of
    V.KUp    -> gs { screen = Fighting bs { bPhase = PlayerSummon (max 0 (idx-1)) } }
    V.KDown  -> gs { screen = Fighting bs { bPhase = PlayerSummon (min (length (playerParty (player gs))-1) (idx+1)) } }
    V.KChar 'w' -> gs { screen = Fighting bs { bPhase = PlayerSummon (max 0 (idx-1)) } }
    V.KChar 's' -> gs { screen = Fighting bs { bPhase = PlayerSummon (min (length (playerParty (player gs))-1) (idx+1)) } }
    V.KEsc   -> gs { screen = Fighting bs { bPhase = PlayerMenu } }
    V.KEnter -> executeSummon gs bs idx
    V.KChar ' ' -> executeSummon gs bs idx
    _        -> gs

cycleNext :: (Eq a, Enum a, Bounded a) => a -> a
cycleNext x = if x == maxBound then minBound else succ x

cyclePrev :: (Eq a, Enum a, Bounded a) => a -> a
cyclePrev x = if x == minBound then maxBound else pred x

handleMenuSelect :: GameState -> BattleState -> GameState
handleMenuSelect gs bs = case bMenuSel bs of
  MenuFight   -> gs { screen = Fighting bs { bPhase = PlayerFight 0 } }
  MenuSummon  ->
    if null (playerParty (player gs))
    then gs { screen = Fighting (blogMsg "No demons in party!" bs) }
    else gs { screen = Fighting bs { bPhase = PlayerSummon 0 } }
  MenuRecruit -> tryRecruit gs bs
  MenuBag     -> gs { screen = Fighting (blogMsg "Bag is empty." bs) }
  MenuRun     -> gs { screen = Fighting bs { bPhase = BattleOver False } }

tryRecruit :: GameState -> BattleState -> GameState
tryRecruit gs bs =
  let e      = bEnemy bs
      hpPct  = combHP e * 100 `div` combMaxHP e
      party  = playerParty (player gs)
  in if hpPct > 30
     then gs { screen = Fighting (blogMsg (combName e ++ " won't join yet! Lower HP first.") bs) }
     else if length party >= 4
     then gs { screen = Fighting (blogMsg "Party full! (max 4 demons)" bs) }
     else
       let kind   = fromMaybe SwordDemon (combKind e)
           party' = party ++ [kind]
           pl'    = (player gs) { playerParty = party' }
           bs'    = bs { bPhase = BattleOver True }
           msg    = combName e ++ " joined your party!"
       in addMsg msg $
          gs { player = pl', screen = Fighting (blogMsg msg bs') }

executePlayerMove :: GameState -> BattleState -> Int -> GameState
executePlayerMove gs bs idx =
  let mv      = combMoves (bPlayer bs) !! idx
      (nm,_,_) = moveInfo mv
      dmg     = calcDamage (bPlayer bs) (bEnemy bs) mv
      msg     = "Hayato uses " ++ nm ++ "! " ++ show dmg ++ " dmg"
      bs1     = dealToEnemy dmg $ blogMsg msg bs
      bs2     = checkBattleEnd bs1
      bs3     = case bPhase bs2 of
                  PlayerMenu -> bs2 { bPhase = EnemyTurn (combName (bEnemy bs2) ++ "'s turn!") }
                  other      -> bs2
  in gs { screen = Fighting bs3 }

executeSummon :: GameState -> BattleState -> Int -> GameState
executeSummon gs bs idx =
  let party  = playerParty (player gs)
      kind   = party !! idx
      tmpl   = demonTemplates Map.! kind
      ally   = fromTemplate tmpl
      msg    = dName tmpl ++ " summoned!"
      bs'    = (blogMsg msg bs) { bAlly = Just ally, bPhase = EnemyTurn (combName (bEnemy bs) ++ " reacts!") }
  in gs { screen = Fighting bs' }

-- ===========================================================================
-- EXPLORE INPUT
-- ===========================================================================

tryMove :: (Int,Int) -> GameState -> GameState
tryMove (dx,dy) gs =
  let pl      = player gs
      (px,py) = playerPos pl
      np      = (px+dx, py+dy)
  in if not (inBounds np) || not (isPassable (worldMap gs ! np))
     then gs { player = pl { playerFacing = (dx,dy) } }
     else
       let pl' = pl { playerPos = np, playerFacing = (dx,dy) }
       in gs { player = pl' }
  where inBounds (x,y) = x>=0 && y>=0 && x<mapW && y<mapH

tryAttack :: GameState -> GameState
tryAttack gs =
  let pl      = player gs
      (px,py) = playerPos pl
      (fx,fy) = playerFacing pl
      frontPos = (px+fx, py+fy)
      mDemon  = find (\d -> wdPos d == frontPos) (Map.elems (demons gs))
  in case mDemon of
       Nothing -> addMsg "Nothing there to attack." gs
       Just wd ->
         let tmpl  = demonTemplates Map.! wdKind wd
             enemy = fromTemplate tmpl
             bs    = BattleState
               { bPlayer   = playerCombatant { combHP = playerHP pl, combMaxHP = playerMaxHP pl }
               , bAlly     = Nothing
               , bEnemy    = enemy
               , bEnemyId  = wdId wd
               , bPhase    = PlayerMenu
               , bMenuSel  = MenuFight
               , bLog      = ["Encountered " ++ dName tmpl ++ "!"]
               }
         in gs { screen = Fighting bs }

-- ===========================================================================
-- EVENT HANDLING
-- ===========================================================================

handleEvent :: BrickEvent Name AppEvent -> EventM Name GameState ()
handleEvent (AppEvent Tick) =
  modify $ \gs -> gs { animTick = animTick gs + 1 }
handleEvent (AppEvent DemonTick) =
  modify stepDemons
handleEvent (VtyEvent (V.EvKey V.KEsc _)) = halt
handleEvent (VtyEvent (V.EvKey key _)) = do
  gs <- get
  case screen gs of
    Exploring  -> handleExploreKey key gs
    Fighting bs -> put $ handleBattleKey key gs bs
handleEvent _ = pure ()

handleExploreKey :: V.Key -> GameState -> EventM Name GameState ()
handleExploreKey key gs = case key of
  V.KChar 'q'  -> halt
  V.KChar 'w'  -> put (tryMove ( 0,-1) gs)
  V.KChar 's'  -> put (tryMove ( 0, 1) gs)
  V.KChar 'a'  -> put (tryMove (-1, 0) gs)
  V.KChar 'd'  -> put (tryMove ( 1, 0) gs)
  V.KChar ' '  -> put (tryAttack gs)
  _            -> pure ()

-- ===========================================================================
-- APP
-- ===========================================================================

drawGame :: GameState -> [Widget Name]
drawGame gs = case screen gs of
  Exploring  -> renderExplore gs
  Fighting bs -> renderBattle gs bs

app :: App GameState AppEvent Name
app = App
  { appDraw         = drawGame
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const (attrMap V.defAttr [])
  }

main :: IO ()
main = do
  chan <- newBChan 20
  -- cosmetic animation tick
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 300000
  -- demon movement tick (slower than animation)
  forkIO $ forever $ do
    writeBChan chan DemonTick
    threadDelay 800000
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app initialState
