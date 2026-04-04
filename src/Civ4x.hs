{-# LANGUAGE OverloadedStrings #-}

module Civ4x where

import Brick hiding (clamp)
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Data.Array (Array, listArray, (!))
import Data.Bits (shiftR, xor)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

type Pos = (Int, Int)

data Terrain = Plains | Hills | Forest | Water | Mountain
  deriving (Eq, Show)

data UnitType = Settler | Warrior | Scout
  deriving (Eq, Show)

data Unit = Unit
  { unitId :: Int,
    unitPos :: Pos,
    unitType :: UnitType,
    unitMoves :: Int,
    unitOwner :: Int
  }
  deriving (Eq, Show)

data BuildItem = BuildWarrior
  deriving (Eq, Show)

buildCost :: BuildItem -> Int
buildCost BuildWarrior = 2

buildName :: BuildItem -> String
buildName BuildWarrior = "Warrior"

data City = City
  { cityId :: Int,
    cityPos :: Pos,
    cityName :: String,
    buildQueue :: Maybe (BuildItem, Int)
  }
  deriving (Eq, Show)

data Selection = SelUnit Int | SelCity Int
  deriving (Eq, Show)

data GameState = GameState
  { worldMap :: Array Pos Terrain,
    units :: Map Int Unit,
    cities :: Map Int City,
    selection :: Maybe Selection,
    camPos :: Pos,
    turn :: Int,
    animTick :: Int,
    msgLog :: [String],
    nextUnitId :: Int,
    nextCityId :: Int
  }

data AppEvent = AnimTick

type Name = ()

-- ---------------------------------------------------------------------------
-- Sprite system
-- sw x sh characters per tile
-- ---------------------------------------------------------------------------

sw, sh :: Int
sw = 5
sh = 3

data SCell = SCell Char (Int, Int, Int)

type Sprite = [[SCell]] -- sh rows of sw cells

sc :: Char -> (Int, Int, Int) -> SCell
sc = SCell

-- Convert sprite to Vty image rows
spriteToImgRows :: Sprite -> [V.Image]
spriteToImgRows = map rowToImg
  where
    rowToImg cells = V.horizCat $ map cellToImg cells
    cellToImg (SCell ch (r, g, b)) =
      V.char (V.withForeColor V.defAttr (V.rgbColor r g b)) ch

-- Highlight: lighten all cells
highlight :: Sprite -> Sprite
highlight = map (map lighten)
  where
    lighten (SCell ch (r, g, b)) =
      SCell ch (min 255 (r + 70), min 255 (g + 70), min 255 (b + 70))

-- ---------------------------------------------------------------------------
-- Terrain sprites
-- ---------------------------------------------------------------------------

plainsSpr :: Sprite
plainsSpr =
  [ [sc '.' g, sc ',' g, sc '.' g, sc '\'' g, sc '.' g],
    [sc ',' g, sc '.' l, sc '\'' g, sc '.' g, sc ',' g],
    [sc '.' g, sc '\'' g, sc '.' l, sc ',' g, sc '.' g]
  ]
  where
    g = (80, 160, 50); l = (110, 190, 70)

hillsSpr :: Sprite
hillsSpr =
  [ [sc ' ' k, sc '/' h, sc '^' s, sc '\\' h, sc ' ' k],
    [sc '/' h, sc '_' d, sc '_' d, sc '_' d, sc '\\' h],
    [sc '\'' g, sc '.' g, sc ',' g, sc '.' g, sc '\'' g]
  ]
  where
    h = (180, 160, 80); s = (220, 200, 120); d = (200, 180, 100); g = (80, 140, 40); k = (0, 0, 0)

forestSpr :: Sprite
forestSpr =
  [ [sc ' ' k, sc '*' t, sc '*' d, sc '*' t, sc ' ' k],
    [sc '*' t, sc '|' b, sc '*' d, sc '|' b, sc '*' t],
    [sc '.' g, sc '|' b, sc '.' g, sc '|' b, sc '.' g]
  ]
  where
    t = (30, 140, 30); d = (50, 170, 50); b = (80, 55, 15); g = (55, 110, 25); k = (0, 0, 0)

mountainSpr :: Sprite
mountainSpr =
  [ [sc ' ' k, sc '/' m, sc '^' s, sc '\\' m, sc ' ' k],
    [sc '/' m, sc '/' r, sc '|' s, sc '\\' r, sc '\\' m],
    [sc 'm' d, sc 'm' d, sc 'm' d, sc 'm' d, sc 'm' d]
  ]
  where
    m = (155, 155, 165); s = (220, 225, 235); r = (125, 125, 135); d = (95, 95, 105); k = (0, 0, 0)

waterSpr :: Int -> Sprite
waterSpr tick =
  let p = tick `mod` 4
      (w1, w2, w3) = case p of
        0 -> ('~', '\'', '-')
        1 -> ('-', '~', '\'')
        2 -> ('\'', '-', '~')
        _ -> ('~', '-', '\'')
   in [ [sc w1 d, sc w2 l, sc w1 d, sc w3 l, sc w2 d],
        [sc w2 l, sc w1 d, sc w3 l, sc w1 d, sc w3 l],
        [sc w3 d, sc w2 l, sc w1 d, sc w2 l, sc w1 d]
      ]
  where
    d = (45, 95, 200); l = (75, 135, 230)

terrainSpr :: Int -> Terrain -> Sprite
terrainSpr tick t = case t of
  Plains -> plainsSpr
  Hills -> hillsSpr
  Forest -> forestSpr
  Mountain -> mountainSpr
  Water -> waterSpr tick

-- ---------------------------------------------------------------------------
-- Entity sprites
-- ---------------------------------------------------------------------------

settlerSpr :: Sprite
settlerSpr =
  [ [sc ' ' fg, sc 'o' fg, sc ' ' fg, sc ' ' fg, sc ' ' fg],
    [sc '[' fg, sc '=' fg, sc ']' fg, sc '~' pk, sc ' ' fg],
    [sc ' ' fg, sc '|' fg, sc '|' fg, sc ' ' fg, sc ' ' fg]
  ]
  where
    fg = (215, 185, 45); pk = (180, 140, 80)

warriorSpr :: Sprite
warriorSpr =
  [ [sc ' ' fg, sc 'o' fg, sc ' ' fg, sc '|' sh2, sc ' ' sh2],
    [sc '(' fg, sc '#' fg, sc ')' fg, sc '|' sh2, sc ' ' sh2],
    [sc ' ' fg, sc '|' fg, sc '|' fg, sc '/' sh2, sc ' ' sh2]
  ]
  where
    fg = (200, 65, 55); sh2 = (150, 150, 195)

scoutSpr :: Sprite
scoutSpr =
  [ [sc ' ' fg, sc 'o' fg, sc ' ' fg, sc ' ' fg, sc ' ' fg],
    [sc '>' fg, sc ':' fg, sc '<' fg, sc '~' fg, sc ' ' fg],
    [sc '/' fg, sc ' ' fg, sc '\\' fg, sc ' ' fg, sc ' ' fg]
  ]
  where
    fg = (60, 195, 170)

citySpr :: Maybe (BuildItem, Int) -> Sprite
citySpr bq =
  let prog = case bq of Just (_, n) -> head (show n); Nothing -> ' '
   in [ [sc '/' rt, sc '^' rt, sc '^' rt, sc '\\' rt, sc ' ' fg],
        [sc '|' wl, sc '#' fg, sc '#' fg, sc '|' wl, sc prog fg],
        [sc '|' wl, sc '_' wl, sc '_' wl, sc '|' wl, sc ' ' fg]
      ]
  where
    rt = (195, 90, 70); wl = (175, 175, 195); fg = (220, 205, 75)

unitSpr :: UnitType -> Sprite
unitSpr Settler = settlerSpr
unitSpr Warrior = warriorSpr
unitSpr Scout = scoutSpr

-- ---------------------------------------------------------------------------
-- Noise-based map generation
-- ---------------------------------------------------------------------------

mapW, mapH :: Int
mapW = 60
mapH = 36

hash2 :: Int -> Int -> Int
hash2 x y =
  let h = x * 374761393 + y * 1103515245
      h2 = h `xor` (h `shiftR` 13)
      h3 = h2 * 1664525
   in h3 `xor` (h3 `shiftR` 16)

valueNoise :: Int -> Int -> Int
valueNoise x y = abs (hash2 x y) `mod` 1001

lerp :: Int -> Int -> Int -> Int
lerp a b t = a + (b - a) * t `div` 1000

bilerp :: Int -> Int -> Int -> Int -> Int -> Int -> Int
bilerp v00 v10 v01 v11 tx ty =
  lerp (lerp v00 v10 tx) (lerp v01 v11 tx) ty

smoothNoise :: Int -> Int -> Int -> Int
smoothNoise scale x y =
  let gx = x `div` scale
      gy = y `div` scale
      tx = (x `mod` scale) * 1000 `div` scale
      ty = (y `mod` scale) * 1000 `div` scale
   in bilerp
        (valueNoise gx gy)
        (valueNoise (gx + 1) gy)
        (valueNoise gx (gy + 1))
        (valueNoise (gx + 1) (gy + 1))
        tx
        ty

fbm :: Int -> Int -> Int
fbm x y =
  clamp 0 1000 $
    ( smoothNoise 10 x y * 512
        + smoothNoise 5 x y * 256
        + smoothNoise 3 x y * 128
        + smoothNoise 2 x y * 64
    )
      `div` 960

detailNoise :: Int -> Int -> Int
detailNoise x y = smoothNoise 4 (x + 73) (y + 151)

genTerrain :: Int -> Int -> Terrain
genTerrain x y =
  let e = fbm x y
      d = detailNoise x y
   in if e < 290
        then Water
        else
          if e < 360
            then Plains
            else
              if e > 820
                then Mountain
                else
                  if e > 640
                    then if d > 420 then Forest else Hills
                    else
                      if d > 580
                        then Forest
                        else
                          if d > 320
                            then Hills
                            else Plains

buildWorldMap :: Array Pos Terrain
buildWorldMap =
  listArray
    ((0, 0), (mapW - 1, mapH - 1))
    [genTerrain x y | y <- [0 .. mapH - 1], x <- [0 .. mapW - 1]]

startPos :: Array Pos Terrain -> Pos
startPos wm =
  head
    [ (x, y) | y <- [mapH `div` 3 .. 2 * mapH `div` 3], x <- [mapW `div` 3 .. 2 * mapW `div` 3], let t = wm ! (x, y), t == Plains || t == Hills
    ]

initialState :: GameState
initialState =
  let wm = buildWorldMap
      (sx, sy) = startPos wm
   in GameState
        { worldMap = wm,
          units =
            Map.fromList
              [ (0, Unit 0 (sx, sy) Settler 2 0),
                (1, Unit 1 (sx + 1, sy) Warrior 2 0)
              ],
          cities = Map.empty,
          selection = Just (SelUnit 0),
          camPos =
            ( clamp 0 (mapW - tilesW) (sx - tilesW `div` 2),
              clamp 0 (mapH - tilesH) (sy - tilesH `div` 2)
            ),
          turn = 1,
          animTick = 0,
          msgLog = ["Arrows:scroll  WASD:move  Tab:cycle  f:found  b:build  Enter:end turn  q:quit"],
          nextUnitId = 2,
          nextCityId = 0
        }

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

tilesW, tilesH :: Int
tilesW = 13
tilesH = 10

viewW, viewH :: Int
viewW = tilesW * sw
viewH = tilesH * sh

renderMap :: GameState -> V.Image
renderMap gs =
  let (cvx, cvy) = camPos gs
      tick = animTick gs
   in V.vertCat
        [ buildSpriteRow gs tick cvx (cvy + ty)
          | ty <- [0 .. tilesH - 1]
        ]

-- Build one full-height strip of sprites for a given world-y
buildSpriteRow :: GameState -> Int -> Int -> Int -> V.Image
buildSpriteRow gs tick cvx wy =
  let sprites = [getTileSprite gs tick (cvx + tx) wy | tx <- [0 .. tilesW - 1]]
      rowsPerSprite = [map (!! r) sprites | r <- [0 .. sh - 1]]
   in V.vertCat $ map (V.horizCat . map rowImg) rowsPerSprite
  where
    rowImg cells = V.horizCat $ map cellImg cells
    cellImg (SCell ch (r, g, b)) =
      V.char (V.withForeColor V.defAttr (V.rgbColor r g b)) ch

getTileSprite :: GameState -> Int -> Int -> Int -> Sprite
getTileSprite gs tick wx wy
  | wx < 0 || wy < 0 || wx >= mapW || wy >= mapH =
      replicate sh (replicate sw (SCell ' ' (8, 8, 25)))
  | otherwise =
      let pos = (wx, wy)
          sel = selection gs
          mCity = listToMaybe [c | c <- Map.elems (cities gs), cityPos c == pos]
          mUnit = listToMaybe [u | u <- Map.elems (units gs), unitPos u == pos]
          isCitySel c = sel == Just (SelCity (cityId c))
          isUnitSel u = sel == Just (SelUnit (unitId u))
          base = terrainSpr tick (worldMap gs ! pos)
       in case mCity of
            Just c -> (if isCitySel c then highlight else id) (citySpr (buildQueue c))
            Nothing -> case mUnit of
              Just u -> (if isUnitSel u then highlight else id) (unitSpr (unitType u))
              Nothing -> base

-- ---------------------------------------------------------------------------
-- Sidebar
-- ---------------------------------------------------------------------------

sideW :: Int
sideW = 32

renderSidebar :: GameState -> V.Image
renderSidebar gs =
  let bold a = V.withStyle a V.bold
      fg r g b = V.withForeColor V.defAttr (V.rgbColor r g b)
      dim = fg 140 140 140
      yel = fg 255 215 70
      cyn = fg 70 215 215
      sep = V.string dim (replicate sideW '─')
      blank = V.string V.defAttr ""

      hdr =
        V.string
          (bold (fg 200 200 220))
          (" ◆ CIVLIKE  Turn " ++ show (turn gs))

      selPanel = case selection gs of
        Nothing -> [V.string dim "  nothing selected"]
        Just (SelUnit uid) -> case Map.lookup uid (units gs) of
          Nothing -> [V.string dim "  (unit gone)"]
          Just u ->
            [ V.string yel ("  ▸ " ++ show (unitType u) ++ " #" ++ show uid),
              V.string V.defAttr ("    pos " ++ showPos (unitPos u)),
              V.string V.defAttr ("    moves " ++ show (unitMoves u)),
              V.string dim "    WASD move · f found"
            ]
        Just (SelCity cid) -> case Map.lookup cid (cities gs) of
          Nothing -> [V.string dim "  (city gone)"]
          Just c ->
            let bstr = case buildQueue c of
                  Nothing -> V.string dim "    queue (empty)"
                  Just (bi, n) ->
                    V.string cyn $
                      "    building: "
                        ++ buildName bi
                        ++ " ("
                        ++ show n
                        ++ "t)"
             in [ V.string yel ("  ▸ " ++ cityName c),
                  V.string V.defAttr ("    pos " ++ showPos (cityPos c)),
                  bstr,
                  V.string dim "    b: queue warrior"
                ]

      cityPanel =
        V.string (bold V.defAttr) " Cities"
          : if Map.null (cities gs)
            then [V.string dim "  (none yet)"]
            else
              [ V.string V.defAttr $
                  "  "
                    ++ cityName c
                    ++ maybe
                      ""
                      (\(bi, n) -> " [" ++ buildName bi ++ " " ++ show n ++ "t]")
                      (buildQueue c)
                | c <- Map.elems (cities gs)
              ]

      keyPanel =
        [ V.string (bold V.defAttr) " Keys",
          V.string V.defAttr "  ↑↓←→  scroll",
          V.string V.defAttr "  wasd  move unit",
          V.string V.defAttr "  Tab   cycle sel",
          V.string V.defAttr "  f     found city",
          V.string V.defAttr "  b     build unit",
          V.string V.defAttr "  Enter end turn",
          V.string V.defAttr "  q     quit"
        ]

      logPanel =
        V.string (bold V.defAttr) " Log"
          : map (\l -> V.string dim ("  " ++ take (sideW - 3) l)) (take 5 (msgLog gs))

      all' =
        [hdr, sep]
          ++ selPanel
          ++ [blank, sep]
          ++ cityPanel
          ++ [blank, sep]
          ++ keyPanel
          ++ [sep]
          ++ logPanel
   in V.vertCat all'

showPos :: Pos -> String
showPos (x, y) = "(" ++ show x ++ "," ++ show y ++ ")"

renderGame :: GameState -> [Widget Name]
renderGame gs =
  [raw $ V.horizCat [renderMap gs, V.char V.defAttr ' ', renderSidebar gs]]

-- ---------------------------------------------------------------------------
-- Game logic
-- ---------------------------------------------------------------------------

clamp :: Int -> Int -> Int -> Int
clamp lo hi x = max lo (min hi x)

addMsg :: String -> GameState -> GameState
addMsg msg gs = gs {msgLog = msg : msgLog gs}

selectedUnit :: GameState -> Maybe Unit
selectedUnit gs = case selection gs of
  Just (SelUnit uid) -> Map.lookup uid (units gs)
  _ -> Nothing

selectedCity :: GameState -> Maybe City
selectedCity gs = case selection gs of
  Just (SelCity cid) -> Map.lookup cid (cities gs)
  _ -> Nothing

moveUnit :: (Int, Int) -> GameState -> GameState
moveUnit (dx, dy) gs = case selectedUnit gs of
  Nothing -> gs
  Just u ->
    let (x, y) = unitPos u
        nx = clamp 0 (mapW - 1) (x + dx)
        ny = clamp 0 (mapH - 1) (y + dy)
        newPos = (nx, ny)
        t = worldMap gs ! newPos
     in if unitMoves u <= 0
          then addMsg "No moves left this turn." gs
          else
            if t == Mountain || t == Water
              then addMsg ("Can't enter " ++ show t ++ ".") gs
              else
                let u' = u {unitPos = newPos, unitMoves = unitMoves u - 1}
                 in centerOn newPos $
                      gs {units = Map.insert (unitId u) u' (units gs)}

settleCity :: GameState -> GameState
settleCity gs = case selectedUnit gs of
  Nothing -> addMsg "Nothing selected." gs
  Just u ->
    if unitType u /= Settler
      then addMsg "Only settlers can found cities." gs
      else
        let pos = unitPos u
         in if any ((== pos) . cityPos) (Map.elems (cities gs))
              then addMsg "A city already exists here." gs
              else
                let cid = nextCityId gs
                    name = "City " ++ show (cid + 1)
                    c = City cid pos name Nothing
                 in addMsg ("Founded " ++ name ++ "!") $
                      gs
                        { cities = Map.insert cid c (cities gs),
                          units = Map.delete (unitId u) (units gs),
                          selection = Just (SelCity cid),
                          nextCityId = cid + 1
                        }

buildInCity :: GameState -> GameState
buildInCity gs = case selectedCity gs of
  Nothing -> addMsg "Select a city first." gs
  Just c ->
    case buildQueue c of
      Just _ -> addMsg (cityName c ++ " is already building something.") gs
      Nothing ->
        let c' = c {buildQueue = Just (BuildWarrior, buildCost BuildWarrior)}
         in addMsg ("Queued " ++ buildName BuildWarrior ++ " in " ++ cityName c ++ ".") $
              gs {cities = Map.insert (cityId c) c' (cities gs)}

cycleSelection :: GameState -> GameState
cycleSelection gs =
  let uSels = map SelUnit (Map.keys (units gs))
      cSels = map SelCity (Map.keys (cities gs))
      all' = uSels ++ cSels
   in case all' of
        [] -> gs
        _ ->
          let cur = selection gs
              next = case cur >>= \s -> case dropWhile (/= s) all' of _ : x : _ -> Just x; _ -> Nothing of
                Just x -> x
                Nothing -> head all'
           in case next of
                SelUnit uid -> centerOn (unitPos $ units gs Map.! uid) gs {selection = Just next}
                SelCity cid -> centerOn (cityPos $ cities gs Map.! cid) gs {selection = Just next}

endTurn :: GameState -> GameState
endTurn gs =
  let gs1 =
        gs
          { turn = turn gs + 1,
            units = Map.map resetMoves (units gs)
          }
      (gs2, msgs) = Map.foldlWithKey' tickCity (gs1, []) (cities gs1)
   in addMsg ("─── Turn " ++ show (turn gs2) ++ " ───") $
        foldr addMsg gs2 msgs
  where
    resetMoves u = u {unitMoves = case unitType u of Scout -> 3; _ -> 2}

tickCity :: (GameState, [String]) -> Int -> City -> (GameState, [String])
tickCity (gs, msgs) cid city =
  case buildQueue city of
    Nothing -> (gs, msgs)
    Just (item, 1) ->
      let uid = nextUnitId gs
          newU = Unit uid (cityPos city) Warrior 2 0
          city' = city {buildQueue = Nothing}
          msg = cityName city ++ " produced a " ++ buildName item ++ "!"
       in ( gs
              { cities = Map.insert cid city' (cities gs),
                units = Map.insert uid newU (units gs),
                nextUnitId = uid + 1
              },
            msg : msgs
          )
    Just (item, n) ->
      let city' = city {buildQueue = Just (item, n - 1)}
       in (gs {cities = Map.insert cid city' (cities gs)}, msgs)

scrollMap :: (Int, Int) -> GameState -> GameState
scrollMap (dx, dy) gs =
  let (vx, vy) = camPos gs
   in gs
        { camPos =
            ( clamp 0 (mapW - tilesW) (vx + dx),
              clamp 0 (mapH - tilesH) (vy + dy)
            )
        }

centerOn :: Pos -> GameState -> GameState
centerOn (x, y) gs =
  gs
    { camPos =
        ( clamp 0 (mapW - tilesW) (x - tilesW `div` 2),
          clamp 0 (mapH - tilesH) (y - tilesH `div` 2)
        )
    }

-- ---------------------------------------------------------------------------
-- Event handling
-- ---------------------------------------------------------------------------

handleEvent :: BrickEvent Name AppEvent -> EventM Name GameState ()
handleEvent (AppEvent AnimTick) = modify $ \gs -> gs {animTick = animTick gs + 1}
handleEvent (VtyEvent (V.EvKey key _)) = case key of
  V.KChar 'q' -> halt
  V.KUp -> modify (scrollMap (0, -1))
  V.KDown -> modify (scrollMap (0, 1))
  V.KLeft -> modify (scrollMap (-1, 0))
  V.KRight -> modify (scrollMap (1, 0))
  V.KChar 'w' -> modify (moveUnit (0, -1))
  V.KChar 's' -> modify (moveUnit (0, 1))
  V.KChar 'a' -> modify (moveUnit (-1, 0))
  V.KChar 'd' -> modify (moveUnit (1, 0))
  V.KChar 'f' -> modify settleCity
  V.KChar 'b' -> modify buildInCity
  V.KChar '\t' -> modify cycleSelection
  V.KEnter -> modify endTurn
  _ -> pure ()
handleEvent _ = pure ()

-- ---------------------------------------------------------------------------
-- App
-- ---------------------------------------------------------------------------

app :: App GameState AppEvent Name
app =
  App
    { appDraw = renderGame,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const (attrMap V.defAttr [])
    }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan AnimTick
    threadDelay 300000
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app initialState