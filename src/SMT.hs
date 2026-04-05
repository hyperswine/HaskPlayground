{-# LANGUAGE OverloadedStrings #-}

module SMT where

import Brick hiding (clamp)
import Brick.BChan (newBChan, writeBChan)
import qualified Graphics.Vty as V
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Array (Array, listArray, (!))
import Data.List (find)
import Data.Maybe (fromMaybe, fromJust, listToMaybe)
import Data.Bits (xor, shiftR)
import Control.Monad (forever, void)
import Control.Concurrent (forkIO, threadDelay)
import qualified Graphics.Vty.CrossPlatform as V

-- ===========================================================================
-- ANIMATION HELPERS
-- ===========================================================================

sinApprox :: Int -> Int
sinApprox deg =
  let d = deg `mod` 360
  in if d < 90  then d * 1000 `div` 90
     else if d < 180 then (180 - d) * 1000 `div` 90
     else if d < 270 then negate $ (d - 180) * 1000 `div` 90
     else negate $ (360 - d) * 1000 `div` 90

breathe :: Int -> Int -> Int
breathe speed tick = (sinApprox ((tick * speed) `mod` 360) + 1000) `div` 2

lerpRGB :: (Int,Int,Int) -> (Int,Int,Int) -> Int -> (Int,Int,Int)
lerpRGB (r1,g1,b1) (r2,g2,b2) t =
  ( r1 + (r2-r1)*t`div`1000
  , g1 + (g2-g1)*t`div`1000
  , b1 + (b2-b1)*t`div`1000 )

hueRGB :: Int -> Int -> (Int,Int,Int)
hueRGB sat tick =
  let h   = (tick * 7) `mod` 360
      seg = h `div` 60
      f   = (h `mod` 60) * 255 `div` 60
      s   = sat
      (r,g,b) = case seg of
        0 -> (255,   f,   0)
        1 -> (255-f,255,   0)
        2 -> (  0, 255,   f)
        3 -> (  0, 255-f,255)
        4 -> (  f,   0, 255)
        _ -> (255,   0, 255-f)
      desat c = c + (255-c)*(255-s)`div`255
  in (desat r, desat g, desat b)

hpColour :: Int -> Int -> (Int,Int,Int)
hpColour hp maxHp =
  let pct = hp * 100 `div` max 1 maxHp
  in if pct > 60 then (60, 200, 80)
     else if pct > 30 then lerpRGB (220,200,40) (60,200,80) ((pct-30)*33)
     else lerpRGB (220,50,50) (220,200,40) (pct*33)

pulseRGB :: (Int,Int,Int) -> (Int,Int,Int) -> Int -> Int -> (Int,Int,Int)
pulseRGB c1 c2 speed tick = lerpRGB c1 c2 (breathe speed tick)

flashTint :: Int -> (Int,Int,Int) -> (Int,Int,Int)
flashTint intensity (r,g,b) =
  ( r + (255-r)*intensity`div`255
  , g + (255-g)*intensity`div`255
  , b + (255-b)*intensity`div`255 )

-- ===========================================================================
-- SPRITE SYSTEM
-- ===========================================================================

sw, sh :: Int
sw = 5; sh = 3

data SCell = SCell Char (Int,Int,Int)
type Sprite = [[SCell]]

sc :: Char -> (Int,Int,Int) -> SCell
sc = SCell

largeSpr :: Sprite -> Sprite
largeSpr spr =
  concatMap (\row -> let r2 = concatMap (\c -> [c,c]) row in [r2, r2]) spr

tintSprite :: ((Int,Int,Int) -> (Int,Int,Int)) -> Sprite -> Sprite
tintSprite f = map (map (\(SCell ch col) -> SCell ch (f col)))

-- ===========================================================================
-- TERRAIN SPRITES
-- ===========================================================================

bogSpr :: Int -> Sprite
bogSpr tick =
  let p = tick `mod` 4
      (c1,c2) = case p of
        0 -> ('~','\''); 1 -> ('\'','~'); 2 -> ('-','~'); _ -> ('~','-')
  in [ [sc c1 d, sc c2 l, sc c1 d, sc c2 l, sc c1 d]
     , [sc c2 l, sc c1 d, sc c2 l, sc c1 d, sc c2 l]
     , [sc c1 d, sc c2 l, sc c1 d, sc c2 l, sc c1 d] ]
  where d=(50,90,60); l=(70,120,80)

mudSpr :: Sprite
mudSpr =
  [ [sc '.' mg, sc ',' mg, sc '.' dg, sc '\'' mg, sc '.' mg]
  , [sc ',' dg, sc '.' mg, sc ',' mg, sc '.' dg, sc '\'' mg]
  , [sc '.' mg, sc '\'' mg, sc '.' mg, sc ',' dg, sc '.' mg] ]
  where mg=(90,75,50); dg=(70,58,35)

deadTreeSpr :: Sprite
deadTreeSpr =
  [ [sc ' ' k, sc 'Y' b, sc ' ' k, sc '/' b, sc ' ' k]
  , [sc ' ' k, sc '|' b, sc ' ' k, sc ' ' k, sc ' ' k]
  , [sc '.' mg, sc '|' d, sc '.' mg, sc ' ' k, sc ' ' k] ]
  where b=(80,65,40); d=(60,48,28); mg=(80,68,45); k=(0,0,0)

reedsSpr :: Int -> Sprite
reedsSpr tick =
  let sway = if tick `mod` 6 < 3 then '|' else '/'
  in [ [sc sway r, sc ' ' k, sc '|' r, sc ' ' k, sc sway r]
     , [sc '|' d, sc '\'' g, sc '|' d, sc '\'' g, sc '|' d]
     , [sc '.' mg, sc '.' mg, sc ',' mg, sc '.' mg, sc '.' mg] ]
  where r=(60,120,50); d=(45,95,38); g=(80,150,60); mg=(80,70,45); k=(0,0,0)

puddleSpr :: Sprite
puddleSpr =
  [ [sc '.' mg, sc '.' mg, sc '.' mg, sc '.' mg, sc '.' mg]
  , [sc '.' mg, sc '~' w, sc '~' w, sc '.' mg, sc '.' mg]
  , [sc '.' mg, sc '.' mg, sc '.' mg, sc '.' mg, sc '.' mg] ]
  where mg=(85,72,48); w=(60,100,75)

stoneSpr :: Sprite
stoneSpr =
  [ [sc '#' s, sc ' ' k, sc '#' d, sc ' ' k, sc '#' s]
  , [sc ' ' k, sc '#' d, sc '#' s, sc '#' d, sc ' ' k]
  , [sc '#' d, sc '#' s, sc ' ' k, sc '#' d, sc '#' s] ]
  where s=(110,105,100); d=(80,76,72); k=(0,0,0)

terrainSpr :: Int -> Terrain -> Sprite
terrainSpr tick t = case t of
  Bog -> bogSpr tick; Mud -> mudSpr; DeadTree -> deadTreeSpr
  Reeds -> reedsSpr tick; Puddle -> puddleSpr; Stone -> stoneSpr

-- ===========================================================================
-- ENTITY SPRITES
-- ===========================================================================

playerSpr :: Sprite
playerSpr =
  [ [sc ' ' k, sc '(' p, sc 'o' p, sc ')' p, sc ' ' k]
  , [sc ' ' k, sc '[' c, sc '|' c, sc ']' c, sc ' ' k]
  , [sc ' ' k, sc '/' c, sc ' ' c, sc '\\' c, sc ' ' k] ]
  where p=(210,195,170); c=(60,90,160); k=(0,0,0)

swordDemonSpr :: Int -> Sprite
swordDemonSpr frame =
  let eyeChar = if frame == 0 then '@' else '*'
      hornCol = if frame == 0 then (200,60,60) else (220,80,40)
  in [ [sc '/' hornCol, sc 'D' r, sc 'D' r, sc '\\' hornCol, sc '|' s]
     , [sc ' ' k, sc '(' r, sc eyeChar e, sc ')' r, sc '-' s]
     , [sc ' ' k, sc '|' d, sc '|' d, sc ' ' k, sc ' ' k] ]
  where r=(170,40,40); e=(255,120,50); d=(130,30,30); s=(190,190,210); k=(0,0,0)

plagueDemonSpr :: Int -> Sprite
plagueDemonSpr frame =
  let bubbleCol = if frame == 0 then (90,170,70) else (110,190,90)
  in [ [sc ' ' k, sc 'o' g, sc 'O' bubbleCol, sc 'o' g, sc ' ' k]
     , [sc '(' g, sc '*' bubbleCol, sc '$' y, sc '*' bubbleCol, sc ')' g]
     , [sc ' ' k, sc '~' g, sc '~' dg, sc '~' g, sc ' ' k] ]
  where g=(60,130,50); dg=(40,100,35); y=(180,200,50); k=(0,0,0)

shadowDemonSpr :: Int -> Sprite
shadowDemonSpr frame =
  let wisp = if frame == 0 then '~' else '-'
      vCol = if frame == 0 then (120,80,180) else (140,100,200)
  in [ [sc ' ' k, sc ')' vCol, sc 'o' p, sc '(' vCol, sc ' ' k]
     , [sc wisp vCol, sc ' ' k, sc '|' dv, sc ' ' k, sc wisp vCol]
     , [sc ' ' k, sc wisp dv, sc ' ' k, sc wisp dv, sc ' ' k] ]
  where dv=(80,50,130); p=(210,170,255); k=(0,0,0)

demonSpriteFramed :: DemonKind -> Int -> Sprite
demonSpriteFramed SwordDemon  f = swordDemonSpr f
demonSpriteFramed PlagueDemon f = plagueDemonSpr f
demonSpriteFramed ShadowDemon f = shadowDemonSpr f

demonSprite :: DemonKind -> Sprite
demonSprite k = demonSpriteFramed k 0

overlayEntity :: Sprite -> Sprite -> Sprite
overlayEntity entity terrain =
  zipWith (zipWith mergeCell) entity terrain
  where
    mergeCell (SCell _ (0,0,0)) t = t
    mergeCell e _                 = e

-- ===========================================================================
-- TYPES
-- ===========================================================================

type Pos = (Int,Int)

data Terrain = Bog | Mud | DeadTree | Reeds | Puddle | Stone deriving (Eq,Show)
data DemonKind = SwordDemon | PlagueDemon | ShadowDemon deriving (Eq,Show,Ord)
data Move = SwordAttack | FireAttack | PoisonCloud | DarkSlash deriving (Eq,Show)
data DamageType = Physical | Magical deriving (Eq,Show)

moveInfo :: Move -> (String, DamageType, Int)
moveInfo SwordAttack = ("Sword Atk", Physical, 8)
moveInfo FireAttack  = ("Fire Atk",  Magical,  10)
moveInfo PoisonCloud = ("Poison",    Magical,  10)
moveInfo DarkSlash   = ("Dark Slash",Physical,  8)

data DemonTemplate = DemonTemplate
  { dKind :: DemonKind, dName :: String, dMaxHP :: Int
  , dPhyAtk :: Int, dMagAtk :: Int, dPhyDef :: Int, dMagDef :: Int
  , dMoves :: [Move] }

demonTemplates :: Map DemonKind DemonTemplate
demonTemplates = Map.fromList
  [ (SwordDemon,  DemonTemplate SwordDemon  "Kelpie"  80 35 20 30 15 [SwordAttack, FireAttack])
  , (PlagueDemon, DemonTemplate PlagueDemon "Boggart" 70 25 30 20 30 [PoisonCloud, FireAttack])
  , (ShadowDemon, DemonTemplate ShadowDemon "Umbra"   65 30 35 20 35 [DarkSlash,   FireAttack]) ]

data Combatant = Combatant
  { combName :: String, combHP :: Int, combMaxHP :: Int
  , combPhyAtk :: Int, combMagAtk :: Int, combPhyDef :: Int, combMagDef :: Int
  , combMoves :: [Move], combKind :: Maybe DemonKind } deriving (Eq,Show)

playerCombatant :: Combatant
playerCombatant = Combatant "Hayato" 120 120 40 20 25 20 [SwordAttack] Nothing

fromTemplate :: DemonTemplate -> Combatant
fromTemplate t = Combatant (dName t) (dMaxHP t) (dMaxHP t)
  (dPhyAtk t) (dMagAtk t) (dPhyDef t) (dMagDef t) (dMoves t) (Just (dKind t))

data WorldDemon = WorldDemon
  { wdId :: Int, wdKind :: DemonKind, wdPos :: Pos
  , wdSpawn :: Pos, wdFacing :: (Int,Int), wdStepCtr :: Int } deriving (Eq,Show)

data BattleMenu = MenuFight | MenuSummon | MenuRecruit | MenuBag | MenuRun
  deriving (Eq,Show,Enum,Bounded)

data BattlePhase
  = PlayerMenu Int          -- acting slot: 0=Hayato, 1-3=bAllies index (1-based)
  | PlayerFight Int Int     -- acting slot, move index
  | PlayerSummon Int        -- party demon index (only from Hayato's turn)
  | EnemyTurn Int String    -- enemy slot (0=bEnemy, 1+=bEnemyAllies), message
  | BattleOver Bool
  deriving (Eq,Show)

data BattleState = BattleState
  { bPlayer      :: Combatant
  , bAllies      :: [Combatant]      -- player-side summoned demons (up to 3, slots 1-3)
  , bEnemy       :: Combatant
  , bEnemyAllies :: [Combatant]      -- enemy-side demons (up to 3, slots 1-3)
  , bEnemyId     :: Int
  , bPhase       :: BattlePhase, bMenuSel :: BattleMenu
  , bLog         :: [String]
  , hitFlash     :: Int
  , allyFlash    :: Int
  }

data Screen = Exploring | Fighting BattleState

data Player = Player
  { playerPos :: Pos, playerFacing :: (Int,Int)
  , playerHP :: Int, playerMaxHP :: Int
  , playerParty :: [DemonKind] } deriving (Eq,Show)

data GameState = GameState
  { worldMap :: Array Pos Terrain, player :: Player
  , demons :: Map Int WorldDemon, screen :: Screen
  , animTick :: Int, msgLog :: [String], nextDmnId :: Int }

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
      tx=(x`mod`scale)*1000`div`scale; ty=(y`mod`scale)*1000`div`scale
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
isPassable Bog   = False
isPassable Stone = False
isPassable _     = True

initialDemons :: Map Int WorldDemon
initialDemons = Map.fromList $ zip [0..]
  [ WorldDemon i k p p (1,0) 0
  | (i,k,p) <- [ (0,SwordDemon,(15,10)), (1,PlagueDemon,(25,18))
               , (2,ShadowDemon,(10,22)), (3,SwordDemon,(30,8))
               , (4,PlagueDemon,(20,25)) ] ]

initialState :: GameState
initialState = GameState
  { worldMap = buildWorldMap, player = Player (5,5) (1,0) 120 120 []
  , demons = initialDemons, screen = Exploring, animTick = 0
  , msgLog = ["You enter the fetid swamp...", "SPACE: attack demon ahead"]
  , nextDmnId = 5 }

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

-- Box border around a list of image rows, with a centered label in the top edge
boxedPanel :: (Int,Int,Int) -> Int -> String -> [V.Image] -> V.Image
boxedPanel (br,bg,bb) innerW label rows =
  let borderA = rgb br bg bb
      labelA  = boldA (rgb (min 255 (br+50)) (min 255 (bg+50)) (min 255 (bb+50)))
      padL    = (innerW - length label) `div` 2
      padR    = innerW - padL - length label
      top     = V.string borderA "╔"
             V.<|> V.string borderA (replicate padL '═')
             V.<|> V.string labelA label
             V.<|> V.string borderA (replicate padR '═')
             V.<|> V.string borderA "╗"
      bot     = V.string borderA $ "╚" ++ replicate innerW '═' ++ "╝"
      wrapRow r = V.string borderA "║"
               V.<|> padTo innerW r
               V.<|> V.string borderA "║"
  in V.vertCat $ [top] ++ map wrapRow rows ++ [bot]

padTo :: Int -> V.Image -> V.Image
padTo w img =
  let p = max 0 (w - V.imageWidth img)
  in img V.<|> V.string V.defAttr (replicate p ' ')

-- Render sprite rows with optional hit-flash (intensity 0..255)
spriteImage :: Int -> Sprite -> V.Image
spriteImage flash spr = V.vertCat $ map renderRow spr
  where
    renderRow cells = V.horizCat $ map renderCell cells
    renderCell (SCell ch (r,g,b)) =
      let (r',g',b') = flashTint flash (r,g,b)
      in V.char (rgb r' g' b') ch

-- HP bar with animated colour and low-HP flicker
hpBarImg :: Int -> Int -> Int -> V.Image
hpBarImg tick hp maxHp =
  let pct     = hp * 16 `div` max 1 maxHp
      (r,g,b) = hpColour hp maxHp
      flicker = hp * 100 `div` max 1 maxHp < 20 && tick `mod` 2 == 0
      (r',g',b') = if flicker then flashTint 130 (r,g,b) else (r,g,b)
  in V.string (rgb r' g' b') (replicate pct 'o')
  V.<|> V.string (rgb 50 50 60) (replicate (16 - pct) '.')
  V.<|> V.string dimA (" " ++ show hp ++ "/" ++ show maxHp)

-- ===========================================================================
-- EXPLORE RENDERING
-- ===========================================================================

vTilesW, vTilesH :: Int
vTilesW = 27; vTilesH = 22

camOrigin :: Pos -> (Int,Int)
camOrigin (px,py) =
  ( clamp 0 (mapW-vTilesW) (px - vTilesW`div`2)
  , clamp 0 (mapH-vTilesH) (py - vTilesH`div`2) )

renderExplore :: GameState -> [Widget Name]
renderExplore gs =
  [ raw $ V.horizCat [ mapImg, V.string dimA " ", sidebarImg ] ]
  where
    pl      = player gs
    (cx,cy) = camOrigin (playerPos pl)
    tick    = animTick gs
    mapImg  = V.vertCat [ buildMapRow gs tick cx (cy+ty) | ty <- [0..vTilesH-1] ]
    sidebarImg = renderExploreSidebar gs

buildMapRow :: GameState -> Int -> Int -> Int -> V.Image
buildMapRow gs tick cx wy =
  let sprites    = [ getTileSprite gs tick (cx+tx) wy | tx <- [0..vTilesW-1] ]
      rowsPerSpr = [ map (!!r) sprites | r <- [0..sh-1] ]
      scImg (SCell ch (r,g,b)) = V.char (rgb r g b) ch
      rowImg     = V.horizCat . map scImg
      tileRowImg = V.horizCat . map rowImg
  in V.vertCat (map tileRowImg rowsPerSpr)

getTileSprite :: GameState -> Int -> Int -> Int -> Sprite
getTileSprite gs tick wx wy
  | wx < 0 || wy < 0 || wx >= mapW || wy >= mapH
  = replicate sh (replicate sw (SCell ' ' (5,5,15)))
  | otherwise =
      let pos      = (wx,wy)
          pl       = player gs
          mDemon   = listToMaybe [ d | d <- Map.elems (demons gs), wdPos d == pos ]
          isPlayer = playerPos pl == pos
          base     = terrainSpr tick (worldMap gs ! pos)
          frame wd = (tick + wdId wd * 3) `mod` 2
      in if isPlayer then overlayEntity playerSpr base
         else case mDemon of
           Just wd -> overlayEntity (demonSpriteFramed (wdKind wd) (frame wd)) base
           Nothing -> base

renderExploreSidebar :: GameState -> V.Image
renderExploreSidebar gs =
  let tick = animTick gs
      pl   = player gs
      -- animated title: hue shifts across chars
      titleImg = V.horizCat $ zipWith (\i ch ->
        let (r,g,b) = pulseRGB (60,70,140) (110,90,200) 12 (tick + i*8)
        in V.char (boldA (rgb r g b)) ch) [0..] ("  DAEMON HUNT  " :: String)
      sep  = V.string dimA (replicate 26 '─')
      sp   = V.string V.defAttr " "
      hpImg = V.string (rgb 140 140 155) " HP " V.<|> hpBarImg tick (playerHP pl) (playerMaxHP pl)
      partyHdr = V.string (boldA (rgb 180 160 220)) " Party"
      partyLines = if null (playerParty pl)
        then [V.string dimA "  (no demons)"]
        else zipWith (\i k ->
          let (r,g,b) = pulseRGB (90,75,160) (130,110,210) 10 (tick + i*25)
          in V.string (rgb r g b) $ "  " ++ show i ++ ". " ++ dName (demonTemplates Map.! k)
          ) [1..] (playerParty pl)
      ctrlHdr = V.string (boldA (rgb 180 160 220)) " Controls"
      ctrls =
        [ V.string (rgb 155 155 165) "  WASD   move"
        , V.string (rgb 155 155 165) "  SPACE  attack ahead"
        , V.string (rgb 155 155 165) "  ESC    quit" ]
      logHdr  = V.string (boldA (rgb 180 160 220)) " Log"
      logLines = zipWith (\i l ->
          let br = max 80 (160 - i*20)
          in V.string (rgb br br (min 255 (br+20))) ("  " ++ take 24 l)
        ) [0..] (take 5 (msgLog gs))
  in V.vertCat $
     [titleImg, sep, hpImg, sp, sep, partyHdr]
     ++ partyLines ++ [sp, sep, ctrlHdr] ++ ctrls
     ++ [sp, sep, logHdr] ++ logLines

-- ===========================================================================
-- BATTLE RENDERING
-- ===========================================================================

panelW :: Int
panelW = 36

renderBattle :: GameState -> BattleState -> [Widget Name]
renderBattle gs bs =
  [ raw $ V.vertCat [ titleBar, mainArea, bottomArea ] ]
  where
    tick   = animTick gs
    flash  = hitFlash bs
    aflash = allyFlash bs

    -- ── Title bar ──────────────────────────────────────────────────────
    titleBar =
      let titleImg = V.horizCat $ zipWith (\i ch ->
            let (r,g,b) = pulseRGB (55,65,130) (100,80,190) 10 (tick*2 + i*10)
            in V.char (boldA (rgb r g b)) ch) [0..] (" ◈  DAEMON HUNT  ◈ " :: String)
          (tr,tg,tb) = case bPhase bs of
            EnemyTurn _ _    -> pulseRGB (220,80,60)  (255,130,100) 15 tick
            PlayerMenu i | i > 0 -> pulseRGB (80,180,220) (120,230,255) 15 tick
            BattleOver True  -> pulseRGB (60,160,130) (100,210,170) 8 tick
            BattleOver False -> (150,150,160)
            _              -> pulseRGB (180,170,220) (230,220,255) 8 tick
          phaseStr = case bPhase bs of
            PlayerMenu 0     -> "  YOUR TURN  "
            PlayerMenu i     -> "  SLOT " ++ show i ++ " TURN  "
            PlayerFight _ _  -> "  CHOOSE MOVE  "
            PlayerSummon _   -> "  SUMMON  "
            EnemyTurn _ _    -> "  ENEMY TURN  "
            BattleOver True  -> "  VICTORY!  "
            BattleOver False -> "  RETREATED  "
          phaseImg = V.string (boldA (rgb tr tg tb)) phaseStr
      in titleImg V.<|> phaseImg

    -- ── Main combat panels side by side ────────────────────────────────
    mainArea = V.horizCat [enemyPanel, V.string dimA " ", playerPanel]

    enemyPanel =
      let e     = bEnemy bs
          kind  = fromMaybe SwordDemon (combKind e)
          frame = tick `mod` 2
          spr   = largeSpr (demonSpriteFramed kind frame)
          -- border pulses angry red during flash
          (bcr,bcg,bcb) = if flash > 0
            then flashTint (min 255 (flash*55)) (150,30,30)
            else (100,35,45)
          -- name hue-cycles
          nameImg = V.horizCat $ zipWith (\i ch ->
            let (r,g,b) = hueRGB 240 (tick*2 + i*30)
            in V.char (boldA (rgb r g b)) ch) [0..] (combName e)
          rows =
            [ nameImg
            , hpBarImg tick (combHP e) (combMaxHP e)
            , V.string dimA $ "PHY:" ++ show (combPhyAtk e)
              ++ " MAG:" ++ show (combMagAtk e)
              ++ " DEF:" ++ show (combPhyDef e)
            , V.string V.defAttr " "
            , spriteImage (min 255 (flash * 64)) spr
            ]
      in boxedPanel (bcr,bcg,bcb) panelW " ENEMY " rows

    playerPanel =
      let p        = bPlayer bs
          pFlash   = min 255 (aflash * 64)
          pSprImg  = spriteImage pFlash (largeSpr playerSpr)
          pNameImg = V.string (boldA (rgb 100 190 240)) (combName p)
          actSlot  = case bPhase bs of { PlayerMenu i -> i; PlayerFight i _ -> i; _ -> -1 }
          mkAllyRows i a =
            let aKind    = fromMaybe SwordDemon (combKind a)
                aFrame   = (tick + i) `mod` 2
                aSprImg  = spriteImage (min 255 (aflash * 64))
                             (largeSpr (demonSpriteFramed aKind aFrame))
                aStatus  = if combHP a <= 0 then " [fallen]" else ""
                isActive = actSlot == i
                (ar,ag,ab) = if isActive
                             then pulseRGB (120,220,255) (180,255,255) 15 tick
                             else hueRGB 200 (tick*2 + i*90)
                aNameImg = V.string (boldA (rgb ar ag ab)) (combName a ++ aStatus)
            in [aNameImg, hpBarImg tick (combHP a) (combMaxHP a), aSprImg]
          allyRows = if null (bAllies bs)
            then [V.string dimA " (no demons summoned)"]
            else concatMap (\(i,a) -> mkAllyRows i a) (zip [1..] (bAllies bs))
          (bcr,bcg,bcb) = if aflash > 0
            then flashTint (min 255 (aflash*55)) (30,50,150)
            else (40,60,120)
          rows = [pNameImg, hpBarImg tick (combHP p) (combMaxHP p), pSprImg] ++ allyRows
      in boxedPanel (bcr,bcg,bcb) panelW " PARTY " rows

    -- ── Bottom: log + menu ─────────────────────────────────────────────
    bottomArea = V.horizCat [logPanel, V.string dimA " ", menuPanel]

    logPanel =
      let ls  = take 5 (bLog bs) ++ repeat ""
          rows = zipWith (\i l ->
            let br = max 80 (190 - i*25)
            in V.string (rgb br br (min 255 (br+25))) (" " ++ take (panelW-2) l)
            ) [0..4] ls
      in boxedPanel (70,55,95) panelW " BATTLE LOG " rows

    -- pulsing selection cursor
    cursor isSel =
      if isSel
      then let (r,g,b) = pulseRGB (200,180,50) (255,240,100) 18 tick
           in V.string (boldA (rgb r g b)) " ▸ "
      else V.string dimA "   "

    selLabel isSel lbl =
      let (r,g,b) = if isSel
                    then pulseRGB (220,215,255) (255,252,255) 18 tick
                    else (145,140,160)
      in V.string (boldA (rgb r g b)) lbl

    menuPanel = case bPhase bs of
      PlayerMenu slot ->
        let sel      = bMenuSel bs
            actor    = maybe "???" combName (playerSlotCombatant bs slot)
            isHayato = slot == 0
            items    = if isHayato
                        then [ (MenuFight,   "FIGHT",   "Attack the enemy")
                             , (MenuSummon,  "SUMMON",  "Call a party demon")
                             , (MenuRecruit, "RECRUIT", "Recruit at low HP")
                             , (MenuBag,     "BAG",     "Use an item")
                             , (MenuRun,     "RUN",     "Flee the battle") ]
                        else [ (MenuFight,   "FIGHT",   "Attack the enemy")
                             , (MenuBag,     "BAG",     "Use an item")
                             , (MenuRun,     "PASS",    "Skip this demon's turn") ]
            rows     = V.string dimA (" " ++ actor ++ "'s action:") :
              map (\(m,lbl,hint) ->
                let isSel = m == sel
                in cursor isSel
                   V.<|> selLabel isSel lbl
                   V.<|> V.string dimA ("  " ++ hint)
              ) items
        in boxedPanel (80,65,115) panelW " ACTION " rows

      PlayerFight slot idx ->
        let attacker = fromJust (playerSlotCombatant bs slot)
            moves    = combMoves attacker
            rows     = V.string dimA " Choose attack:" :
              zipWith (\i mv ->
                let (nm,dt,pw) = moveInfo mv
                    isSel = i == idx
                    dtStr = case dt of Physical -> "PHY"; Magical -> "MAG"
                in cursor isSel
                   V.<|> selLabel isSel nm
                   V.<|> V.string dimA (" [" ++ dtStr ++ " " ++ show pw ++ "]")
              ) [0..] moves
              ++ [V.string dimA "   <- back"]
        in boxedPanel (80,65,115) panelW " FIGHT " rows

      PlayerSummon idx ->
        let party = playerParty (player gs)
            rows  = V.string dimA " Choose demon:" :
              if null party
                then [V.string dimA "   (party empty)"]
                else zipWith (\i k ->
                  let t     = demonTemplates Map.! k
                      isSel = i == idx
                  in cursor isSel V.<|> selLabel isSel (dName t)
                  ) [0..] party
              ++ [V.string dimA "   <- back"]
        in boxedPanel (80,65,115) panelW " SUMMON " rows

      EnemyTurn _ msg ->
        let (r,g,b) = pulseRGB (230,80,60) (255,140,100) 12 tick
        in boxedPanel (125,35,35) panelW " ENEMY TURN "
             [V.string (boldA (rgb r g b)) (" " ++ msg)
             ,V.string dimA " (press any key)"]

      BattleOver True ->
        let (r,g,b) = pulseRGB (60,160,130) (100,210,170) 8 tick
        in boxedPanel (50,120,55) panelW " RESULT "
             [V.string (boldA (rgb r g b)) "  VICTORY!"
             ,V.string dimA "  Press any key"]

      BattleOver False ->
        boxedPanel (75,75,90) panelW " RESULT "
          [V.string dimA "  You retreated safely."
          ,V.string dimA "  Press any key"]

-- ===========================================================================
-- DEMON WANDERING
-- ===========================================================================

addMsg :: String -> GameState -> GameState
addMsg msg gs = gs { msgLog = msg : msgLog gs }

wanderDemon :: Array Pos Terrain -> Int -> WorldDemon -> WorldDemon
wanderDemon wm tick wd =
  let (sx,sy) = wdSpawn wd; (px,py) = wdPos wd
      tgt = ( sx + ((hash2 (wdId wd) tick `mod` 9) - 4)
            , sy + ((hash2 (wdId wd * 7) tick `mod` 9) - 4) )
      (tx,ty) = (clamp (sx-4) (sx+4) (fst tgt), clamp (sy-4) (sy+4) (snd tgt))
      stepX = signum (tx - px); stepY = signum (ty - py)
      (nx,ny)
        | stepX /= 0 && inB (px+stepX,py) && isPassable (wm!(px+stepX,py)) = (px+stepX, py)
        | stepY /= 0 && inB (px,py+stepY) && isPassable (wm!(px,py+stepY)) = (px, py+stepY)
        | otherwise = (px,py)
  in wd { wdPos = (nx,ny) }
  where inB (x,y) = x>=0 && y>=0 && x<mapW && y<mapH

stepDemons :: GameState -> GameState
stepDemons gs =
  let tick = animTick gs; wm = worldMap gs
      fid  = case screen gs of { Fighting bs -> Just (bEnemyId bs); _ -> Nothing }
      moved = Map.map (\wd -> if Just (wdId wd) == fid then wd else wanderDemon wm (tick + wdId wd*13) wd) (demons gs)
  in gs { demons = moved }

-- ===========================================================================
-- BATTLE LOGIC
-- ===========================================================================

calcDamage :: Combatant -> Combatant -> Move -> Int
calcDamage atk def mv =
  let (_,dt,pw) = moveInfo mv
      a = case dt of Physical -> combPhyAtk atk; Magical -> combMagAtk atk
      d = case dt of Physical -> combPhyDef def; Magical -> combMagDef def
  in max 1 $ pw * a `div` max 1 d

dealToEnemy :: Int -> BattleState -> BattleState
dealToEnemy dmg bs =
  bs { bEnemy = (bEnemy bs) { combHP = max 0 (combHP (bEnemy bs) - dmg) }, hitFlash = 5 }

-- Deal damage to a player-side slot (0=Hayato, 1-3=bAllies)
dealToPlayerSlot :: Int -> Int -> BattleState -> BattleState
dealToPlayerSlot 0 dmg bs =
  bs { bPlayer = (bPlayer bs) { combHP = max 0 (combHP (bPlayer bs) - dmg) }, allyFlash = 5 }
dealToPlayerSlot i dmg bs =
  let j      = i - 1
      allies = bAllies bs
      a'     = (allies !! j) { combHP = max 0 (combHP (allies !! j) - dmg) }
  in bs { bAllies = take j allies ++ [a'] ++ drop (j+1) allies, allyFlash = 5 }

blogMsg :: String -> BattleState -> BattleState
blogMsg msg bs = bs { bLog = msg : bLog bs }

checkBattleEnd :: BattleState -> BattleState
checkBattleEnd bs
  | combHP (bEnemy bs) <= 0  = bs { bPhase = BattleOver True }
  | combHP (bPlayer bs) <= 0 = bs { bPhase = BattleOver False }
  | otherwise = bs

-- Slot lookup helpers
playerSlotCombatant :: BattleState -> Int -> Maybe Combatant
playerSlotCombatant bs 0 = Just (bPlayer bs)
playerSlotCombatant bs i
  | i >= 1 && i-1 < length (bAllies bs) = Just (bAllies bs !! (i-1))
  | otherwise = Nothing

enemySlotCombatant :: BattleState -> Int -> Maybe Combatant
enemySlotCombatant bs 0 = Just (bEnemy bs)
enemySlotCombatant bs i
  | i >= 1 && i-1 < length (bEnemyAllies bs) = Just (bEnemyAllies bs !! (i-1))
  | otherwise = Nothing

-- Advance to next alive player slot, else start enemy turn
nextPlayerSlot :: BattleState -> Int -> BattlePhase
nextPlayerSlot bs slot =
  let maxSlot = length (bAllies bs)
      nextI   = find (\i -> maybe False (\c -> combHP c > 0)
                              (playerSlotCombatant bs i)) [(slot+1)..maxSlot]
  in case nextI of
       Just i  -> PlayerMenu i
       Nothing -> EnemyTurn 0 (combName (bEnemy bs) ++ "'s turn!")

-- Advance to next alive enemy slot, else back to player turn
nextEnemySlot :: BattleState -> Int -> BattlePhase
nextEnemySlot bs slot =
  let maxSlot = length (bEnemyAllies bs)
      nextI   = find (\i -> maybe False (\c -> combHP c > 0)
                              (enemySlotCombatant bs i)) [(slot+1)..maxSlot]
  in case nextI of
       Just i  -> EnemyTurn i (combName (fromJust (enemySlotCombatant bs i)) ++ "'s turn!")
       Nothing -> PlayerMenu 0

-- Auto-AI action for an enemy slot
enemyAction :: Int -> BattleState -> BattleState
enemyAction slot bs = case enemySlotCombatant bs slot of
  Nothing               -> bs { bPhase = nextEnemySlot bs slot }
  Just e | combHP e <= 0 -> bs { bPhase = nextEnemySlot bs slot }
  Just e ->
    let mv         = combMoves e !! (length (bLog bs) `mod` length (combMoves e))
        (nm,_,_)   = moveInfo mv
        allSlots   = 0 : [1..length (bAllies bs)]
        aliveSlots = filter (\i -> maybe False (\c -> combHP c > 0)
                                     (playerSlotCombatant bs i)) allSlots
        tgtSlot    = head aliveSlots
        tgt        = fromJust (playerSlotCombatant bs tgtSlot)
        dmg        = calcDamage e tgt mv
        bs1        = blogMsg (combName e ++ " uses " ++ nm ++ "! " ++ show dmg ++ " dmg") bs
        bs2        = checkBattleEnd (dealToPlayerSlot tgtSlot dmg bs1)
    in case bPhase bs2 of
         BattleOver _ -> bs2
         _            -> bs2 { bPhase = nextEnemySlot bs2 slot }

cycleNext :: (Eq a, Enum a, Bounded a) => a -> a
cycleNext x = if x == maxBound then minBound else succ x

cyclePrev :: (Eq a, Enum a, Bounded a) => a -> a
cyclePrev x = if x == minBound then maxBound else pred x

handleMenuSelect :: GameState -> BattleState -> Int -> GameState
handleMenuSelect gs bs slot = case bMenuSel bs of
  MenuFight   -> gs { screen = Fighting bs { bPhase = PlayerFight slot 0 } }
  MenuSummon
    | slot /= 0 -> gs
    | null (playerParty (player gs))
                -> gs { screen = Fighting (blogMsg "No demons in party!" bs) }
    | length (bAllies bs) >= 3
                -> gs { screen = Fighting (blogMsg "Demon slots full! (max 3)" bs) }
    | otherwise -> gs { screen = Fighting bs { bPhase = PlayerSummon 0 } }
  MenuRecruit
    | slot /= 0 -> gs
    | otherwise -> tryRecruit gs bs
  MenuBag     -> gs { screen = Fighting (blogMsg "Bag is empty." bs) }
  MenuRun
    | slot == 0 -> gs { screen = Fighting bs { bPhase = BattleOver False } }
    | otherwise -> gs { screen = Fighting bs { bPhase = nextPlayerSlot bs slot } }

tryRecruit :: GameState -> BattleState -> GameState
tryRecruit gs bs =
  let e     = bEnemy bs
      hpPct = combHP e * 100 `div` combMaxHP e
      party = playerParty (player gs)
  in if hpPct > 30
     then gs { screen = Fighting (blogMsg (combName e ++ " won't join yet!") bs) }
     else if length party >= 4
     then gs { screen = Fighting (blogMsg "Party full! (max 4)" bs) }
     else
       let kind   = fromMaybe SwordDemon (combKind e)
           party' = party ++ [kind]
           msg    = combName e ++ " joined your party!"
       in addMsg msg $ gs
            { player = (player gs) { playerParty = party' }
            , screen = Fighting (blogMsg msg bs) { bPhase = BattleOver True } }

executePlayerMove :: GameState -> BattleState -> Int -> Int -> GameState
executePlayerMove gs bs slot idx =
  let attacker = fromJust (playerSlotCombatant bs slot)
      mv       = combMoves attacker !! idx
      (nm,_,_) = moveInfo mv
      dmg      = calcDamage attacker (bEnemy bs) mv
      bs1      = dealToEnemy dmg $ blogMsg (combName attacker ++ " uses " ++ nm ++ "! " ++ show dmg ++ " dmg") bs
      bs2      = checkBattleEnd bs1
      bs3      = case bPhase bs2 of { BattleOver _ -> bs2; _ -> bs2 { bPhase = nextPlayerSlot bs2 slot } }
  in gs { screen = Fighting bs3 }

executeSummon :: GameState -> BattleState -> Int -> GameState
executeSummon gs bs idx =
  let kind     = playerParty (player gs) !! idx
      tmpl     = demonTemplates Map.! kind
      newDemon = fromTemplate tmpl
      msg      = dName tmpl ++ " summoned!"
      allies'  = bAllies bs ++ [newDemon]
      bs'      = (blogMsg msg bs) { bAllies = allies'
                                  , bPhase  = nextPlayerSlot (bs { bAllies = allies' }) 0 }
  in gs { screen = Fighting bs' }

handleBattleKey :: V.Key -> GameState -> BattleState -> GameState
handleBattleKey key gs bs = case bPhase bs of
  BattleOver won ->
    let gs1 = gs { screen = Exploring }
    in if won then addMsg ("Defeated " ++ combName (bEnemy bs) ++ "!")
                    gs1 { demons = Map.delete (bEnemyId bs) (demons gs1) }
              else addMsg "You fled the battle." gs1

  EnemyTurn slot _ -> gs { screen = Fighting (enemyAction slot bs) }

  PlayerMenu slot -> case key of
    V.KChar 'w' -> gs { screen = Fighting bs { bMenuSel = cyclePrev (bMenuSel bs) } }
    V.KChar 's' -> gs { screen = Fighting bs { bMenuSel = cycleNext (bMenuSel bs) } }
    V.KUp       -> gs { screen = Fighting bs { bMenuSel = cyclePrev (bMenuSel bs) } }
    V.KDown     -> gs { screen = Fighting bs { bMenuSel = cycleNext (bMenuSel bs) } }
    V.KEnter    -> handleMenuSelect gs bs slot
    V.KChar ' ' -> handleMenuSelect gs bs slot
    _           -> gs

  PlayerFight slot idx ->
    let attacker = fromJust (playerSlotCombatant bs slot)
        maxIdx   = length (combMoves attacker) - 1
    in case key of
      V.KUp       -> gs { screen = Fighting bs { bPhase = PlayerFight slot (max 0 (idx-1)) } }
      V.KDown     -> gs { screen = Fighting bs { bPhase = PlayerFight slot (min maxIdx (idx+1)) } }
      V.KChar 'w' -> gs { screen = Fighting bs { bPhase = PlayerFight slot (max 0 (idx-1)) } }
      V.KChar 's' -> gs { screen = Fighting bs { bPhase = PlayerFight slot (min maxIdx (idx+1)) } }
      V.KLeft     -> gs { screen = Fighting bs { bPhase = PlayerMenu slot } }
      V.KEnter    -> executePlayerMove gs bs slot idx
      V.KChar ' ' -> executePlayerMove gs bs slot idx
      _           -> gs

  PlayerSummon idx -> case key of
    V.KUp       -> gs { screen = Fighting bs { bPhase = PlayerSummon (max 0 (idx-1)) } }
    V.KDown     -> gs { screen = Fighting bs { bPhase = PlayerSummon (min (length (playerParty (player gs))-1) (idx+1)) } }
    V.KChar 'w' -> gs { screen = Fighting bs { bPhase = PlayerSummon (max 0 (idx-1)) } }
    V.KChar 's' -> gs { screen = Fighting bs { bPhase = PlayerSummon (min (length (playerParty (player gs))-1) (idx+1)) } }
    V.KLeft     -> gs { screen = Fighting bs { bPhase = PlayerMenu 0 } }
    V.KEnter    -> executeSummon gs bs idx
    V.KChar ' ' -> executeSummon gs bs idx
    _           -> gs

-- ===========================================================================
-- EXPLORE INPUT
-- ===========================================================================

tryMove :: (Int,Int) -> GameState -> GameState
tryMove (dx,dy) gs =
  let pl = player gs; (px,py) = playerPos pl
      np = (px+dx, py+dy)
      inB (x,y) = x>=0 && y>=0 && x<mapW && y<mapH
  in if not (inB np) || not (isPassable (worldMap gs ! np))
     then gs { player = pl { playerFacing = (dx,dy) } }
     else gs { player = pl { playerPos = np, playerFacing = (dx,dy) } }

tryAttack :: GameState -> GameState
tryAttack gs =
  let pl = player gs; (px,py) = playerPos pl; (fx,fy) = playerFacing pl
      fp = (px+fx, py+fy)
      mD = find (\d -> wdPos d == fp) (Map.elems (demons gs))
  in case mD of
       Nothing -> addMsg "Nothing there to attack." gs
       Just wd ->
         let tmpl = demonTemplates Map.! wdKind wd
             bs   = BattleState
               { bPlayer = playerCombatant { combHP = playerHP pl, combMaxHP = playerMaxHP pl }
               , bAllies = [], bEnemy = fromTemplate tmpl, bEnemyAllies = [], bEnemyId = wdId wd
               , bPhase = PlayerMenu 0, bMenuSel = MenuFight
               , bLog = ["Encountered " ++ dName tmpl ++ "!"]
               , hitFlash = 0, allyFlash = 0 }
         in gs { screen = Fighting bs }

-- ===========================================================================
-- EVENT HANDLING
-- ===========================================================================

tickGame :: GameState -> GameState
tickGame gs =
  let gs' = gs { animTick = animTick gs + 1 }
  in case screen gs' of
       Fighting bs ->
         gs' { screen = Fighting bs
           { hitFlash  = max 0 (hitFlash bs  - 1)
           , allyFlash = max 0 (allyFlash bs - 1) } }
       _ -> gs'

handleEvent :: BrickEvent Name AppEvent -> EventM Name GameState ()
handleEvent (AppEvent Tick)     = modify tickGame
handleEvent (AppEvent DemonTick) = modify $ \gs ->
  case screen gs of
    Fighting bs -> case bPhase bs of
      EnemyTurn slot _ -> gs { screen = Fighting (enemyAction slot bs) }
      _                -> stepDemons gs
    _ -> stepDemons gs
handleEvent (VtyEvent (V.EvKey V.KEsc _)) = halt
handleEvent (VtyEvent (V.EvKey key _)) = do
  gs <- get
  case screen gs of
    Exploring   -> handleExploreKey key gs
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
  Exploring   -> renderExplore gs
  Fighting bs -> renderBattle gs bs

app :: App GameState AppEvent Name
app = App
  { appDraw         = drawGame
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const (attrMap V.defAttr []) }

main :: IO ()
main = do
  chan <- newBChan 20
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 120000   -- ~8fps for smooth hue/pulse animation
  forkIO $ forever $ do
    writeBChan chan DemonTick
    threadDelay 800000
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app initialState
