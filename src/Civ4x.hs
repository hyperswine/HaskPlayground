{-# LANGUAGE OverloadedStrings #-}

module Civ4x where

import Brick hiding (clamp)
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Data.Array (Array, listArray, (!))
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
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
    unitMoves :: Int, -- moves remaining this turn
    unitOwner :: Int -- player id (0 = human for now)
  }
  deriving (Eq, Show)

data City = City
  { cityPos :: Pos,
    cityName :: String
  }
  deriving (Eq, Show)

data GameState = GameState
  { worldMap :: Array Pos Terrain,
    units :: Map Int Unit,
    cities :: [City],
    selected :: Maybe Int, -- selected unit id
    camPos :: Pos, -- top-left corner of view
    turn :: Int,
    animTick :: Int, -- cosmetic animation counter
    msgLog :: [String], -- message log (newest first)
    nextUnitId :: Int,
    mapSize :: (Int, Int) -- (width, height)
  }

data AppEvent = AnimTick

type Name = ()

-- ---------------------------------------------------------------------------
-- World generation (simple deterministic)
-- ---------------------------------------------------------------------------

mapW, mapH :: Int
mapW = 80
mapH = 40

genTerrain :: Pos -> Terrain
genTerrain (x, y) =
  let v = (x * 7 + y * 13 + x * y * 3) `mod` 23
   in case v of
        0 -> Water
        1 -> Water
        2 -> Water
        3 -> Mountain
        4 -> Hills
        5 -> Hills
        6 -> Forest
        7 -> Forest
        _ -> Plains

buildMap :: Array Pos Terrain
buildMap =
  listArray
    ((0, 0), (mapW - 1, mapH - 1))
    [genTerrain (x, y) | y <- [0 .. mapH - 1], x <- [0 .. mapW - 1]]

initialUnits :: Map Int Unit
initialUnits =
  Map.fromList
    [ (0, Unit 0 (5, 5) Settler 2 0),
      (1, Unit 1 (6, 5) Warrior 2 0),
      (2, Unit 2 (7, 4) Scout 3 0)
    ]

initialState :: GameState
initialState =
  GameState
    { worldMap = buildMap,
      units = initialUnits,
      cities = [],
      selected = Just 0,
      camPos = (0, 0),
      turn = 1,
      animTick = 0,
      msgLog = ["Welcome! Arrow keys: scroll map. WASD: move unit. Tab: next unit.", "Press ? for help."],
      nextUnitId = 3,
      mapSize = (mapW, mapH)
    }

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

viewW, viewH :: Int
viewW = 60 -- columns for map view
viewH = 28 -- rows for map view

-- Terrain display: (char, attr)
terrainCell :: Int -> Terrain -> (Char, V.Attr)
terrainCell tick t = case t of
  Plains -> ('.', V.withForeColor V.defAttr (V.rgbColor 100 180 60))
  Hills -> ('^', V.withForeColor V.defAttr (V.rgbColor 160 140 80))
  Forest -> ('#', V.withForeColor V.defAttr (V.rgbColor 40 120 40))
  Mountain -> ('M', V.withForeColor V.defAttr (V.rgbColor 140 140 140))
  Water -> (waterChar tick, V.withForeColor V.defAttr (V.rgbColor 60 120 200))
  where
    waterChar t'
      | t' `mod` 6 < 2 = '~'
      | t' `mod` 6 < 4 = '\''
      | otherwise = '-'

unitCell :: Unit -> Bool -> (Char, V.Attr)
unitCell u isSel =
  let (ch, fg) = case unitType u of
        Settler -> ('[', V.rgbColor 255 220 100)
        Warrior -> ('(', V.rgbColor 220 80 80)
        Scout -> ('{', V.rgbColor 100 220 220)
      baseAttr = V.withForeColor V.defAttr fg
      attr =
        if isSel
          then V.withBackColor baseAttr (V.rgbColor 60 60 60)
          else baseAttr
   in (ch, attr)

cityCell :: (Char, V.Attr)
cityCell = ('*', V.withForeColor (V.withStyle V.defAttr V.bold) (V.rgbColor 255 255 100))

-- Build a Vty Image for the map viewport
renderMap :: GameState -> V.Image
renderMap gs =
  let (vx, vy) = camPos gs
      tick = animTick gs
      rows = [renderRow vy vx y | y <- [vy .. vy + viewH - 1]]
   in V.vertCat rows
  where
    renderRow _ vx y =
      let cells = [renderCell vx y x | x <- [vx .. vx + viewW - 1]]
       in V.horizCat cells

    renderCell _ y x
      | x < 0 || y < 0 || x >= mapW || y >= mapH =
          V.char (V.withForeColor V.defAttr (V.rgbColor 20 20 40)) ' '
      | otherwise =
          let pos = (x, y)
              terrain = worldMap gs ! pos
              -- check city
              mCity = find ((== pos) . cityPos) (cities gs)
              -- check unit
              mUnit = find ((== pos) . unitPos) (Map.elems (units gs))
              isSel = case (mUnit, selected gs) of
                (Just u, Just sid) -> unitId u == sid
                _ -> False
           in case mCity of
                Just _ -> let (ch, at) = cityCell in V.char at ch
                Nothing -> case mUnit of
                  Just u -> let (ch, at) = unitCell u isSel in V.char at ch
                  Nothing -> let (ch, at) = terrainCell (animTick gs) terrain in V.char at ch

-- Sidebar: unit info + message log
renderSidebar :: GameState -> V.Image
renderSidebar gs =
  let selUnit = selected gs >>= \sid -> Map.lookup sid (units gs)
      header =
        V.string (V.withStyle V.defAttr V.bold) $
          "=== CIVLIKE === Turn " ++ show (turn gs)
      blank = V.string V.defAttr ""
      unitInfo = case selUnit of
        Nothing -> [V.string V.defAttr "No unit selected"]
        Just u ->
          [ V.string (V.withForeColor V.defAttr (V.rgbColor 255 220 100)) $
              "Unit: " ++ show (unitType u) ++ " #" ++ show (unitId u),
            V.string V.defAttr $ "Pos:  " ++ show (unitPos u),
            V.string V.defAttr $ "Moves:" ++ show (unitMoves u)
          ]
      cityInfo =
        [ V.string (V.withStyle V.defAttr V.bold) "Cities:"
        ]
          ++ if null (cities gs)
            then [V.string V.defAttr "  (none)"]
            else map (\c -> V.string V.defAttr $ "  " ++ cityName c ++ " " ++ show (cityPos c)) (cities gs)
      controls =
        [ V.string (V.withStyle V.defAttr V.bold) "Keys:",
          V.string V.defAttr "  Arrows: scroll",
          V.string V.defAttr "  WASD:   move unit",
          V.string V.defAttr "  Tab:    next unit",
          V.string V.defAttr "  f:      settle city",
          V.string V.defAttr "  r:      recruit warrior",
          V.string V.defAttr "  Enter:  end turn",
          V.string V.defAttr "  q:      quit"
        ]
      logHeader = V.string (V.withStyle V.defAttr V.bold) "Log:"
      logLines = map (V.string V.defAttr . ("  " ++)) (take 6 (msgLog gs))
      allLines = [header, blank] ++ unitInfo ++ [blank] ++ cityInfo ++ [blank] ++ controls ++ [blank, logHeader] ++ logLines
   in V.vertCat allLines

renderGame :: GameState -> [Widget Name]
renderGame gs =
  [ raw $
      V.horizCat
        [ renderMap gs,
          V.char V.defAttr ' ',
          renderSidebar gs
        ]
  ]

-- ---------------------------------------------------------------------------
-- Input handling
-- ---------------------------------------------------------------------------

moveUnit :: Pos -> GameState -> GameState
moveUnit (dx, dy) gs = case selected gs >>= \sid -> Map.lookup sid (units gs) of
  Nothing -> gs
  Just u ->
    let (x, y) = unitPos u
        nx = clamp 0 (mapW - 1) (x + dx)
        ny = clamp 0 (mapH - 1) (y + dy)
        newPos = (nx, ny)
        terrain = worldMap gs ! newPos
        occupied =
          any
            (\u2 -> unitPos u2 == newPos && unitId u2 /= unitId u)
            (Map.elems (units gs))
     in if terrain == Mountain || terrain == Water
          then addMsg ("Can't move there: " ++ show terrain) gs
          else
            if occupied
              then addMsg "Tile occupied!" gs
              else
                if unitMoves u <= 0
                  then addMsg "No moves remaining." gs
                  else
                    let u' = u {unitPos = newPos, unitMoves = unitMoves u - 1}
                        gs' = gs {units = Map.insert (unitId u) u' (units gs)}
                     in centerViewOn newPos gs'

settleCity :: GameState -> GameState
settleCity gs = case selected gs >>= \sid -> Map.lookup sid (units gs) of
  Nothing -> gs
  Just u ->
    if unitType u /= Settler
      then addMsg "Only settlers can found cities." gs
      else
        let pos = unitPos u
            alreadyCity = any ((== pos) . cityPos) (cities gs)
         in if alreadyCity
              then addMsg "City already exists here." gs
              else
                let cityNum = length (cities gs) + 1
                    newCity = City pos ("City " ++ show cityNum)
                    gs' =
                      gs
                        { cities = newCity : cities gs,
                          units = Map.delete (unitId u) (units gs),
                          selected = Nothing,
                          msgLog = ("Founded " ++ cityName newCity ++ "!") : msgLog gs
                        }
                 in gs'

recruitUnit :: GameState -> GameState
recruitUnit gs = case selected gs >>= \sid -> Map.lookup sid (units gs) of
  Nothing -> gs
  Just u ->
    if unitType u /= Settler
      then addMsg "Only settlers can recruit." gs
      else
        let pos = unitPos u
            sid = nextUnitId gs
            newUnit = Unit sid pos Warrior 2 0
            gs' =
              gs
                { units = Map.insert sid newUnit (units gs),
                  nextUnitId = sid + 1,
                  selected = Just sid
                }
         in addMsg ("Recruited Warrior #" ++ show sid) gs'

nextUnit :: GameState -> GameState
nextUnit gs =
  let ids = Map.keys (units gs)
   in case ids of
        [] -> gs
        _ ->
          let cur = fromMaybe (-1) (selected gs)
              next = case dropWhile (<= cur) ids of
                (i : _) -> i
                [] -> head ids
           in centerViewOn (unitPos $ units gs Map.! next) $
                gs {selected = Just next}

endTurn :: GameState -> GameState
endTurn gs =
  let resetMoves u = case unitType u of
        Scout -> u {unitMoves = 3}
        _ -> u {unitMoves = 2}
      gs' =
        gs
          { units = Map.map resetMoves (units gs),
            turn = turn gs + 1
          }
   in addMsg ("--- Turn " ++ show (turn gs') ++ " ---") gs'

scrollMap :: (Int, Int) -> GameState -> GameState
scrollMap (dx, dy) gs =
  let (vx, vy) = camPos gs
      nvx = clamp 0 (mapW - viewW) (vx + dx)
      nvy = clamp 0 (mapH - viewH) (vy + dy)
   in gs {camPos = (nvx, nvy)}

centerViewOn :: Pos -> GameState -> GameState
centerViewOn (x, y) gs =
  let nvx = clamp 0 (mapW - viewW) (x - viewW `div` 2)
      nvy = clamp 0 (mapH - viewH) (y - viewH `div` 2)
   in gs {camPos = (nvx, nvy)}

addMsg :: String -> GameState -> GameState
addMsg msg gs = gs {msgLog = msg : msgLog gs}

clamp :: Int -> Int -> Int -> Int
clamp lo hi x = max lo (min hi x)

handleEvent :: BrickEvent Name AppEvent -> EventM Name GameState ()
handleEvent (AppEvent AnimTick) =
  modify $ \gs -> gs {animTick = animTick gs + 1}
handleEvent (VtyEvent (V.EvKey key _)) = case key of
  -- quit
  V.KChar 'q' -> halt
  -- scroll map
  V.KUp -> modify (scrollMap (0, -1))
  V.KDown -> modify (scrollMap (0, 1))
  V.KLeft -> modify (scrollMap (-1, 0))
  V.KRight -> modify (scrollMap (1, 0))
  -- move selected unit
  V.KChar 'w' -> modify (moveUnit (0, -1))
  V.KChar 's' -> modify (moveUnit (0, 1))
  V.KChar 'a' -> modify (moveUnit (-1, 0))
  V.KChar 'd' -> modify (moveUnit (1, 0))
  -- actions
  V.KChar 'f' -> modify settleCity
  V.KChar 'r' -> modify recruitUnit
  V.KChar '\t' -> modify nextUnit -- Tab
  V.KEnter -> modify endTurn
  _ -> pure ()
handleEvent _ = pure ()

-- ---------------------------------------------------------------------------
-- App definition
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
  -- cosmetic animation tick: ~4fps is enough for water shimmer
  forkIO $ forever $ do
    writeBChan chan AnimTick
    threadDelay 250000
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app initialState
