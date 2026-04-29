{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QuantUI where

-- ---------------------------------------------------------------------------
-- QuantUI – a mobile-style OS shell built with Brick
--
--   Desktop  : arrow keys select app icon, Enter launches, Tab goes next page
--   Inside app: Tab cycles to next open app / back to desktop, q closes app
--
-- Apps
--   Calculator  – expression evaluator, state persisted to ~/.quantui/calc.json
--   Platformer  – simple 1-level side-scroller, persisted to ~/.quantui/plat.json
--   Weather     – simulated weather for current locale, ~/.quantui/weather.json
--   Notes       – line-based notepad, state persisted to ~/.quantui/notes.json
-- ---------------------------------------------------------------------------

import Brick hiding (clamp)
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, decodeFileStrict, encodeFile)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.FilePath ((</>))

-- ===========================================================================
-- 1.  Name  (widget names for Brick)
-- ===========================================================================

data WName
  = WDesktop
  | WCalcInput
  | WNoteInput
  | WWeatherView
  | WPlatView
  deriving (Eq, Ord, Show)

-- ===========================================================================
-- 2.  App-level state types  (each serialisable with Aeson)
-- ===========================================================================

-- ─── Calculator ─────────────────────────────────────────────────────────────

data CalcState = CalcState
  { calcInput :: String,
    calcHistory :: [String]
  }
  deriving (Eq, Show, Generic)

instance ToJSON CalcState
instance FromJSON CalcState

emptyCalc :: CalcState
emptyCalc = CalcState "" []

-- Tiny expression evaluator: handles +−×÷ with left-to-right precedence
evalExpr :: String -> Maybe Double
evalExpr s = parseExpr (filter (/= ' ') s)
  where
    parseExpr [] = Nothing
    parseExpr str = do
      (v, rest) <- parseNum str
      applyOps v rest

    applyOps acc [] = Just acc
    applyOps acc (op : rest) | op `elem` ("+-*/" :: String) = do
      (v, rest2) <- parseNum rest
      let acc' = case op of
            '+' -> acc + v
            '-' -> acc - v
            '*' -> acc * v
            '/' -> if v == 0 then 0 else acc / v
            _ -> acc
      applyOps acc' rest2
    applyOps _ _ = Nothing

    parseNum [] = Nothing
    parseNum str@(c : _)
      | c == '-' =
          let (digits, rest) = span isNumChar (tail str)
           in if null digits
                then Nothing
                else Just (negate (read digits :: Double), rest)
      | isNumChar c =
          let (digits, rest) = span isNumChar str
           in Just (read digits :: Double, rest)
      | otherwise = Nothing

    isNumChar c = c `elem` ("0123456789." :: String)

-- ─── Platformer ─────────────────────────────────────────────────────────────

data PlatState = PlatState
  { platX :: Int,
    platY :: Int,
    platVY :: Int,
    platScore :: Int,
    platOnGround :: Bool,
    platLevelComplete :: Bool,
    platCoins :: [Bool], -- True = already collected
    platTick :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON PlatState
instance FromJSON PlatState

platWidth, platHeight :: Int
platWidth = 60
platHeight = 16

groundY :: Int
groundY = platHeight - 3

-- Coin positions (x, y)
coinPositions :: [(Int, Int)]
coinPositions =
  [(10, groundY - 3), (20, groundY - 5), (30, groundY - 3), (40, groundY - 4), (55, groundY - 2)]

-- Platform tiles (x_start, width, y)
platforms :: [(Int, Int, Int)]
platforms =
  [(8, 6, groundY - 4), (18, 6, groundY - 6), (28, 8, groundY - 4), (38, 6, groundY - 5)]

emptyPlat :: PlatState
emptyPlat =
  PlatState
    { platX = 2,
      platY = groundY,
      platVY = 0,
      platScore = 0,
      platOnGround = True,
      platLevelComplete = False,
      platCoins = replicate (length coinPositions) False,
      platTick = 0
    }

-- ─── Weather ────────────────────────────────────────────────────────────────

data WeatherState = WeatherState
  { weatherCity :: String,
    weatherTemp :: Int, -- °C
    weatherDesc :: String,
    weatherHumidity :: Int,
    weatherWind :: Int, -- km/h
    weatherLastRefreshed :: String -- ISO date string
  }
  deriving (Eq, Show, Generic)

instance ToJSON WeatherState
instance FromJSON WeatherState

-- Simulate a refresh (deterministic-ish from a tick seed)
simulateWeather :: Int -> WeatherState
simulateWeather seed =
  WeatherState
    { weatherCity = "Your Location",
      weatherTemp = 15 + (seed `mod` 15),
      weatherDesc = descs !! (seed `mod` length descs),
      weatherHumidity = 40 + (seed `mod` 50),
      weatherWind = 5 + (seed `mod` 30),
      weatherLastRefreshed = "2026-04-30"
    }
  where
    descs = ["Sunny", "Partly Cloudy", "Cloudy", "Light Rain", "Thunderstorm", "Foggy", "Windy", "Clear"]

emptyWeather :: WeatherState
emptyWeather = simulateWeather 7

-- ─── Notes ──────────────────────────────────────────────────────────────────

data NotesState = NotesState
  { noteLines :: [String],
    noteCurrentLine :: String,
    noteCursor :: Int -- index into noteLines being edited (or = length = new)
  }
  deriving (Eq, Show, Generic)

instance ToJSON NotesState
instance FromJSON NotesState

emptyNotes :: NotesState
emptyNotes = NotesState [] "" 0

-- ===========================================================================
-- 3.  Top-level OS state
-- ===========================================================================

data AppID = AppCalc | AppPlatformer | AppWeather | AppNotes
  deriving (Eq, Ord, Show, Enum, Bounded)

appLabel :: AppID -> String
appLabel AppCalc = "Calculator"
appLabel AppPlatformer = "Platformer"
appLabel AppWeather = "Weather"
appLabel AppNotes = "Notes"

appIcon :: AppID -> String
appIcon AppCalc = "[ CALC ]"
appIcon AppPlatformer = "[ GAME ]"
appIcon AppWeather = "[ WTHR ]"
appIcon AppNotes = "[ NOTE ]"

allApps :: [AppID]
allApps = [minBound .. maxBound]

iconsPerRow :: Int
iconsPerRow = 4

-- Open / focused app stack.  head = currently visible app; empty = desktop.
type AppStack = [AppID]

data OSState = OSState
  { osDesk :: Int, -- selected icon index on desktop
    osStack :: AppStack, -- open apps (top = focused)
    osCalc :: CalcState,
    osPlat :: PlatState,
    osWeather :: WeatherState,
    osNotes :: NotesState,
    osTick :: Int -- global animation tick
  }

-- Custom event

data OSEvent = OSTick

-- ===========================================================================
-- 4.  Rendering helpers
-- ===========================================================================

bld :: V.Attr -> V.Attr
bld a = V.withStyle a V.bold

cFg :: Int -> Int -> Int -> V.Attr
cFg r g b = V.withForeColor V.defAttr (V.rgbColor r g b)

cBg :: Int -> Int -> Int -> V.Attr -> V.Attr
cBg r g b a = V.withBackColor a (V.rgbColor r g b)

bldFg :: Int -> Int -> Int -> V.Attr
bldFg r g b = bld (cFg r g b)

-- ---------------------------------------------------------------------------
-- 4a.  Desktop drawer
-- ---------------------------------------------------------------------------

renderDesktop :: OSState -> Widget WName
renderDesktop os =
  let sel = osDesk os
      iconWidgets =
        [ padLeftRight 2 $
            withAttr (if i == sel then attrName "iconSel" else attrName "iconNorm") $
              str (appIcon aid)
          | (i, aid) <- zip [0 ..] allApps
        ]
      -- lay icons in a single row (4 per page for now)
      iconRow = hBox iconWidgets
      header = withAttr (attrName "header") $ str " QuantOS  ·  Desktop "
      hint =
        withAttr (attrName "hint") $
          str "  ←→ select  Enter: open  Tab: next page  q: quit"
      body = padTop (Pad 3) $ padLeftRight 4 iconRow
   in vBox
        [ header,
          body,
          padTop (Pad 2) hint
        ]

-- ---------------------------------------------------------------------------
-- 4b.  Calculator
-- ---------------------------------------------------------------------------

renderCalc :: CalcState -> Widget WName
renderCalc cs =
  let histLines =
        [ withAttr (attrName "calcHist") $ str ("  " ++ take 38 l)
          | l <- reverse (take 10 (calcHistory cs))
        ]
      inputLine =
        withAttr (attrName "calcInput") $
          str ("  > " ++ calcInput cs ++ "_")
      header = withAttr (attrName "appHeader") $ str "  ── Calculator ──  (Enter: eval, Del: clear, q: close)"
      kbdRows =
        [ " 7  8  9  /",
          " 4  5  6  *",
          " 1  2  3  -",
          " 0  .  =  +"
        ]
      kbd =
        vBox
          [ padLeft (Pad 2) $ withAttr (attrName "calcKbd") $ str r
            | r <- kbdRows
          ]
   in vBox
        [ header,
          padTop (Pad 1) $ vBox histLines,
          padTop (Pad 1) inputLine,
          padTop (Pad 2) kbd
        ]

-- ---------------------------------------------------------------------------
-- 4c.  Platformer
-- ---------------------------------------------------------------------------

renderPlat :: PlatState -> Widget WName
renderPlat ps =
  let w = platWidth
      h = platHeight
      rows =
        [ renderPlatRow ps y w
          | y <- [0 .. h - 1]
        ]
      header =
        withAttr (attrName "appHeader") $
          str "  ── Platformer ──  (←→ move, Space: jump, q: close)"
      scoreBar =
        withAttr (attrName "hint") $
          str $
            "  Score: " ++ show (platScore ps)
              ++ if platLevelComplete ps then "  ★ Level Complete!" else ""
   in vBox
        [ header,
          scoreBar,
          padTop (Pad 1) $
            padLeft (Pad 2) $
              vBox rows
        ]

renderPlatRow :: PlatState -> Int -> Int -> Widget WName
renderPlatRow ps y w =
  let chars = [cellChar ps y c | c <- [0 .. w - 1]]
   in raw $ V.horizCat $ map charToImg chars
  where
    charToImg (ch, attr) = V.char attr ch

cellChar :: PlatState -> Int -> Int -> (Char, V.Attr)
cellChar ps y x
  -- Player
  | x == platX ps && y == platY ps = ('@', cFg 80 200 80)
  -- Ground
  | y == groundY = ('=', cFg 140 100 60)
  -- Platforms
  | any (\(px, pw, py) -> y == py && x >= px && x < px + pw) platforms =
      ('#', cFg 100 140 180)
  -- Goal flag at x = platWidth-2
  | x == platWidth - 2 && y == groundY - 1 = ('P', cFg 255 220 50)
  -- Coins
  | Just idx <- coinIndex x y,
    not (platCoins ps !! idx) =
      ('o', cFg 255 200 0)
  | otherwise = (' ', V.defAttr)
  where
    coinIndex cx cy =
      let candidates = [i | (i, (cx', cy')) <- zip [0 ..] coinPositions, cx' == cx, cy' == cy]
       in case candidates of
            (i : _) -> Just i
            _ -> Nothing

-- ---------------------------------------------------------------------------
-- 4d.  Weather
-- ---------------------------------------------------------------------------

renderWeather :: WeatherState -> Widget WName
renderWeather ws =
  let header = withAttr (attrName "appHeader") $ str "  ── Weather ──  (r: refresh, q: close)"
      tempLine = withAttr (attrName "wTemp") $ str $ "  Temperature : " ++ show (weatherTemp ws) ++ " °C"
      descLine = withAttr (attrName "wDesc") $ str $ "  Conditions  : " ++ weatherDesc ws
      humLine = withAttr (attrName "hint") $ str $ "  Humidity    : " ++ show (weatherHumidity ws) ++ "%"
      windLine = withAttr (attrName "hint") $ str $ "  Wind        : " ++ show (weatherWind ws) ++ " km/h"
      cityLine = withAttr (attrName "boldText") $ str $ "  City        : " ++ weatherCity ws
      refreshLine = withAttr (attrName "hint") $ str $ "  Refreshed   : " ++ weatherLastRefreshed ws
      icon = withAttr (attrName "wIcon") $ str (weatherIcon (weatherDesc ws))
   in vBox
        [ header,
          padTop (Pad 2) $ hBox [padLeft (Pad 4) (vBox [cityLine, tempLine, descLine, humLine, windLine, refreshLine]), padLeft (Pad 6) icon]
        ]

weatherIcon :: String -> String
weatherIcon "Sunny" = unlines ["  \\  |  /  ", " -- (☀) -- ", "  /  |  \\  "]
weatherIcon "Light Rain" = unlines ["  (☁)  ", " /////  ", " rain   "]
weatherIcon "Thunderstorm" = unlines ["  (☁☁) ", "  /⚡\\  ", " storm  "]
weatherIcon "Foggy" = unlines ["  ~~~   ", " ~ ~ ~  ", "  ~~~   "]
weatherIcon _ = unlines ["  (☁)   ", " cloudy  ", "         "]

-- ---------------------------------------------------------------------------
-- 4e.  Notes
-- ---------------------------------------------------------------------------

renderNotes :: NotesState -> Widget WName
renderNotes ns =
  let header = withAttr (attrName "appHeader") $ str "  ── Notes ──  (Enter: new line, Del: delete char, q: close)"
      noteItems =
        [ withAttr (attrName "noteItem") $
            str ("  " ++ show (i + 1) ++ ". " ++ take 58 l)
          | (i, l) <- zip [0 ..] (noteLines ns)
        ]
      inputLine =
        withAttr (attrName "noteInput") $
          str ("  > " ++ noteCurrentLine ns ++ "_")
   in vBox
        [ header,
          padTop (Pad 1) $ vBox noteItems,
          padTop (Pad 1) inputLine
        ]

-- ===========================================================================
-- 5.  Top-level render
-- ===========================================================================

renderOS :: OSState -> [Widget WName]
renderOS os =
  [ case osStack os of
      [] -> renderDesktop os
      (AppCalc : _) -> renderCalc (osCalc os)
      (AppPlatformer : _) -> renderPlat (osPlat os)
      (AppWeather : _) -> renderWeather (osWeather os)
      (AppNotes : _) -> renderNotes (osNotes os)
  ]

-- ===========================================================================
-- 6.  Event handling
-- ===========================================================================

handleOS :: BrickEvent WName OSEvent -> EventM WName OSState ()
handleOS (AppEvent OSTick) = modify $ \os ->
  os {osTick = osTick os + 1, osPlat = tickPlat (osPlat os)}
handleOS (VtyEvent (V.EvKey key _)) = do
  os <- get
  case osStack os of
    -- ── Desktop ──────────────────────────────────────────────────────────
    [] -> case key of
      V.KChar 'q' -> halt
      V.KLeft -> put $ os {osDesk = max 0 (osDesk os - 1)}
      V.KRight -> put $ os {osDesk = min (length allApps - 1) (osDesk os + 1)}
      V.KEnter ->
        let aid = allApps !! osDesk os
         in put $ os {osStack = [aid]}
      V.KChar '\t' ->
        -- next icon "page" wraps around
        put $ os {osDesk = (osDesk os + 1) `mod` length allApps}
      _ -> pure ()
    -- ── Inside an app ────────────────────────────────────────────────────
    (cur : rest) -> case key of
      -- q  closes the current app
      V.KChar 'q' -> do
        saveApp os cur
        put $ os {osStack = rest}
      -- Tab  cycles to next open app or back to desktop
      V.KChar '\t' ->
        put $
          os
            { osStack =
                if null rest
                  then [] -- back to desktop when only one app open
                  else rest ++ [cur]
            }
      -- App-specific keys
      _ -> handleAppKey os cur key
handleOS _ = pure ()

-- ── Per-app key handling ─────────────────────────────────────────────────

handleAppKey :: OSState -> AppID -> V.Key -> EventM WName OSState ()
handleAppKey os AppCalc key = case key of
  V.KChar c | c `elem` ("0123456789+-*/.() " :: String) ->
    put $ os {osCalc = (osCalc os) {calcInput = calcInput (osCalc os) ++ [c]}}
  V.KEnter ->
    let inp = calcInput (osCalc os)
        result = maybe "Error" show (evalExpr inp)
        entry = inp ++ " = " ++ result
        cs = (osCalc os) {calcInput = "", calcHistory = entry : calcHistory (osCalc os)}
     in put $ os {osCalc = cs}
  V.KBS ->
    let inp = calcInput (osCalc os)
     in put $ os {osCalc = (osCalc os) {calcInput = if null inp then "" else init inp}}
  V.KDel ->
    put $ os {osCalc = (osCalc os) {calcInput = ""}}
  _ -> pure ()
handleAppKey os AppPlatformer key = case key of
  V.KLeft -> put $ os {osPlat = movePlat (-1) (osPlat os)}
  V.KRight -> put $ os {osPlat = movePlat 1 (osPlat os)}
  V.KChar ' ' -> put $ os {osPlat = jumpPlat (osPlat os)}
  _ -> pure ()
handleAppKey os AppWeather key = case key of
  V.KChar 'r' ->
    put $ os {osWeather = simulateWeather (osTick os)}
  _ -> pure ()
handleAppKey os AppNotes key = case key of
  V.KChar c ->
    put $
      os
        { osNotes =
            (osNotes os) {noteCurrentLine = noteCurrentLine (osNotes os) ++ [c]}
        }
  V.KBS ->
    let l = noteCurrentLine (osNotes os)
     in put $ os {osNotes = (osNotes os) {noteCurrentLine = if null l then "" else init l}}
  V.KEnter ->
    let ns = osNotes os
        nl = noteLines ns ++ [noteCurrentLine ns]
     in put $ os {osNotes = ns {noteLines = nl, noteCurrentLine = ""}}
  V.KDel ->
    let ns = osNotes os
        ls = noteLines ns
     in put $ os {osNotes = ns {noteLines = if null ls then [] else init ls}}
  _ -> pure ()

-- ===========================================================================
-- 7.  Platformer physics
-- ===========================================================================

movePlat :: Int -> PlatState -> PlatState
movePlat dx ps
  | platLevelComplete ps = ps
  | otherwise =
      let nx = clampPlat (platX ps + dx)
       in collectCoins $ ps {platX = nx}

jumpPlat :: PlatState -> PlatState
jumpPlat ps
  | platOnGround ps && not (platLevelComplete ps) = ps {platVY = -3}
  | otherwise = ps

tickPlat :: PlatState -> PlatState
tickPlat ps
  | platLevelComplete ps = ps
  | otherwise =
      let vy' = platVY ps + 1 -- gravity
          ny' = platY ps + vy'
          -- check floor
          onFloor = ny' >= groundY
          -- check platforms
          onPlat =
            vy' > 0
              && any
                (\(px, pw, py) -> platX ps >= px && platX ps < px + pw && ny' >= py && platY ps <= py)
                platforms
          (finalY, finalVY, onGround) =
            if onFloor
              then (groundY, 0, True)
              else
                if onPlat
                  then
                    let py = head [py' | (px, pw, py') <- platforms, platX ps >= px && platX ps < px + pw, ny' >= py']
                     in (py, 0, True)
                  else (ny', vy', False)
          ps' = ps {platY = finalY, platVY = finalVY, platOnGround = onGround, platTick = platTick ps + 1}
          ps'' = collectCoins ps'
          complete = platX ps'' >= platWidth - 2
       in ps'' {platLevelComplete = complete}

collectCoins :: PlatState -> PlatState
collectCoins ps =
  let coins' =
        [ already || (cx == platX ps && cy == platY ps)
          | ((cx, cy), already) <- zip coinPositions (platCoins ps)
        ]
      newlyCollected = length (filter id coins') - length (filter id (platCoins ps))
   in ps {platCoins = coins', platScore = platScore ps + newlyCollected * 10}

clampPlat :: Int -> Int
clampPlat = max 0 . min (platWidth - 1)

-- ===========================================================================
-- 8.  Persistence
-- ===========================================================================

dataDir :: IO FilePath
dataDir = do
  home <- getHomeDirectory
  let dir = home </> ".quantui"
  createDirectoryIfMissing True dir
  return dir

loadOrDefault :: (FromJSON a) => FilePath -> a -> IO a
loadOrDefault path def = do
  exists <- doesFileExist path
  if exists
    then fromMaybe def <$> decodeFileStrict path
    else return def

saveApp :: OSState -> AppID -> EventM WName OSState ()
saveApp os aid = liftIO $ do
  dir <- dataDir
  case aid of
    AppCalc -> encodeFile (dir </> "calc.json") (osCalc os)
    AppPlatformer -> encodeFile (dir </> "plat.json") (osPlat os)
    AppWeather -> encodeFile (dir </> "weather.json") (osWeather os)
    AppNotes -> encodeFile (dir </> "notes.json") (osNotes os)

loadOSState :: IO OSState
loadOSState = do
  dir <- dataDir
  calc <- loadOrDefault (dir </> "calc.json") emptyCalc
  plat <- loadOrDefault (dir </> "plat.json") emptyPlat
  weather <- loadOrDefault (dir </> "weather.json") emptyWeather
  notes <- loadOrDefault (dir </> "notes.json") emptyNotes
  return
    OSState
      { osDesk = 0,
        osStack = [],
        osCalc = calc,
        osPlat = plat,
        osWeather = weather,
        osNotes = notes,
        osTick = 0
      }

-- ===========================================================================
-- 9.  Attribute map
-- ===========================================================================

theAttrMap :: AttrMap
theAttrMap =
  attrMap
    V.defAttr
    [ (attrName "header", cBg 20 20 80 (bldFg 200 220 255)),
      (attrName "iconSel", cBg 60 120 220 (bldFg 255 255 255)),
      (attrName "iconNorm", cFg 160 180 220),
      (attrName "hint", cFg 130 130 160),
      (attrName "appHeader", cBg 30 30 60 (bldFg 180 210 255)),
      (attrName "calcHist", cFg 180 220 180),
      (attrName "calcInput", bldFg 240 240 100),
      (attrName "calcKbd", cFg 160 160 200),
      (attrName "wTemp", bldFg 255 180 80),
      (attrName "wDesc", cFg 120 200 240),
      (attrName "wIcon", cFg 255 230 100),
      (attrName "noteItem", cFg 200 200 200),
      (attrName "noteInput", bldFg 100 220 180),
      (attrName "boldText", bld V.defAttr)
    ]

-- ===========================================================================
-- 10.  Brick App & main
-- ===========================================================================

osApp :: App OSState OSEvent WName
osApp =
  App
    { appDraw = renderOS,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleOS,
      appStartEvent = pure (),
      appAttrMap = const theAttrMap
    }

main :: IO ()
main = do
  chan <- newBChan 10
  void $ forkIO $ forever $ do
    writeBChan chan OSTick
    threadDelay 100000 -- 10 fps tick for platformer physics
  initState <- loadOSState
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  finalState <- customMain initialVty buildVty (Just chan) osApp initState
  -- Save all open apps on exit
  dir <- dataDir
  encodeFile (dir </> "calc.json") (osCalc finalState)
  encodeFile (dir </> "plat.json") (osPlat finalState)
  encodeFile (dir </> "weather.json") (osWeather finalState)
  encodeFile (dir </> "notes.json") (osNotes finalState)

forever :: IO () -> IO ()
forever action = action >> forever action

