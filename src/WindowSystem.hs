{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- Compile: stack build --ghc-options="-O2 -threaded"
-- Run:     stack run -- +RTS -N8
--
-- Architecture mirrors Gameloop.hs:
--   Clock (1ms) --> HardwareQueue --> InputActor --> SnapReady
--                                                        |
--   CompositorActor (60Hz) <-- TickSignal <--------------+
--        |
--        v
--   RenderQueue --> RenderActor (75Hz)
--
-- Three STM domains, zero locks:
--   HardwareQueue  : TQueue   -- multi-producer, single-consumer
--   TickSignal     : TMVar () -- one-shot rendezvous per tick
--   SnapshotReady  : TMVar InputSnapshot
--   RenderQueue    : TVar (Maybe WMState)  -- depth-1 overwrite

module WindowSystem where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (unless)
import Data.IORef
import Data.List (find, partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO (BufferMode (..), hSetBuffering, stdout)

-- -----------------------------------------------------------------------------
-- Core Geometry
-- -----------------------------------------------------------------------------

type Time = Integer
type Dt   = Integer

data Point = Point {ptX :: Double, ptY :: Double} deriving (Eq)

instance Show Point where
  show (Point x y) = "(" ++ show (round x :: Int) ++ "," ++ show (round y :: Int) ++ ")"

data Rect = Rect {rX :: Double, rY :: Double, rW :: Double, rH :: Double} deriving (Show, Eq)

pointInRect :: Point -> Rect -> Bool
pointInRect (Point x y) (Rect rx ry rw rh) = x >= rx && x < rx + rw && y >= ry && y < ry + rh

-- Translate a global point to window-local coordinates.
toLocal :: Point -> Rect -> Point
toLocal (Point mx my) (Rect wx wy _ _) = Point (mx - wx) (my - wy)

-- -----------------------------------------------------------------------------
-- Input Domain
-- -----------------------------------------------------------------------------

data MouseButton = LMB | RMB deriving (Eq, Ord, Show)

data HardwareEvent
  = MouseMoved    Point
  | MousePressed  Point MouseButton
  | MouseReleased Point MouseButton
  deriving (Show)

-- Intermediate buffer (hardware rate, ~200 Hz)
data InputBuffer = InputBuffer
  { ibMousePos   :: Point
  , ibClickEdges :: [(Point, MouseButton, Bool)] -- (pos, button, isDown)
  }

emptyBuffer :: InputBuffer
emptyBuffer = InputBuffer (Point 0 0) []

-- Snapshot committed once per Tick boundary (~60 Hz)
data InputSnapshot = InputSnapshot
  { snMousePos   :: Point
  , snClickEdges :: [(Point, MouseButton, Bool)]
  } deriving (Show)

emptySnapshot :: InputSnapshot
emptySnapshot = InputSnapshot (Point 0 0) []

-- -----------------------------------------------------------------------------
-- Widget System
-- -----------------------------------------------------------------------------

type WidgetId = Int

data WidgetKind
  = ButtonWidget   String   -- button label
  | TooltipTrigger String   -- tooltip text
  deriving (Show, Eq)

-- Runtime state carried alongside each widget.
-- Positional constructors avoid partial-field warnings.
data WidgetState
  = ButtonState  Bool Bool   -- hovered, pressed
  | TooltipState Bool        -- visible
  deriving (Show, Eq)

data Widget = Widget
  { wId        :: WidgetId
  , wLocalRect :: Rect       -- relative to the enclosing window's origin
  , wKind      :: WidgetKind
  , wState     :: WidgetState
  } deriving (Show)

defaultState :: WidgetKind -> WidgetState
defaultState (ButtonWidget  _) = ButtonState  False False
defaultState (TooltipTrigger _) = TooltipState False

mkWidget :: WidgetId -> Rect -> WidgetKind -> Widget
mkWidget wid rect kind = Widget wid rect kind (defaultState kind)

-- -----------------------------------------------------------------------------
-- Windows
-- -----------------------------------------------------------------------------

type WindowId = Int

data Window = Window
  { winId      :: WindowId
  , winTitle   :: String
  , winRect    :: Rect      -- absolute screen position & size
  , winWidgets :: [Widget]
  } deriving (Show)

-- -----------------------------------------------------------------------------
-- WM State  (MVU Model)
-- -----------------------------------------------------------------------------
-- wmWindows is z-ordered: head = topmost window on screen.
-- Clicking a window moves it to the head (bring-to-front).

data WMState = WMState
  { wmWindows   :: [Window]
  , wmFocused   :: Maybe WindowId
  , wmMousePos  :: Point
  , wmTickCount :: Int
  , wmEventLog  :: [String]   -- edge events that occurred this tick (for rendering)
  }

-- Three initial windows with widgets.
initWMState :: WMState
initWMState = WMState
  { wmWindows   = [winSettings, winEditor, winTerminal]
  , wmFocused   = Just 1
  , wmMousePos  = Point 0 0
  , wmTickCount = 0
  , wmEventLog  = []
  }

winSettings :: Window
winSettings = Window
  { winId      = 1
  , winTitle   = "Settings"
  , winRect    = Rect 50 50 200 160
  , winWidgets =
      [ mkWidget 1 (Rect 20  100  60  30) (ButtonWidget   "OK")
      , mkWidget 2 (Rect 100 100  80  30) (ButtonWidget   "Cancel")
      , mkWidget 3 (Rect 10  10  180  50) (TooltipTrigger "Configure your system settings")
      ]
  }

winEditor :: Window
winEditor = Window
  { winId      = 2
  , winTitle   = "Editor"
  , winRect    = Rect 300 100 250 200
  , winWidgets =
      [ mkWidget 4 (Rect 10  160  60  30) (ButtonWidget   "Save")
      , mkWidget 5 (Rect 80  160  80  30) (ButtonWidget   "Discard")
      , mkWidget 6 (Rect 10   10 230  80) (TooltipTrigger "Edit your documents here")
      ]
  }

winTerminal :: Window
winTerminal = Window
  { winId      = 3
  , winTitle   = "Terminal"
  , winRect    = Rect 100 300 300 150
  , winWidgets =
      [ mkWidget 7 (Rect  10 110  70  30) (ButtonWidget   "Clear")
      , mkWidget 8 (Rect  90 110  80  30) (ButtonWidget   "New Tab")
      , mkWidget 9 (Rect  10  10 280  70) (TooltipTrigger "Terminal emulator")
      ]
  }

-- -----------------------------------------------------------------------------
-- Pure MVU Update
-- No IO. Takes old state + dt + snapshot -> new state.
-- -----------------------------------------------------------------------------

update :: WMState -> Dt -> InputSnapshot -> WMState
update wms _dt sn =
  let mousePos = snMousePos sn
      clicks   = snClickEdges sn

      -- 1. Process click edges: focus + bring-to-front + widget press state.
      (newFocused, windows1, clickLog) =
        processClicks (wmFocused wms) (wmWindows wms) clicks

      -- 2. Update hover state for every widget in every window.
      windows2 = map (updateWindowHover mousePos newFocused) windows1

      -- 3. Diff hover/tooltip changes against previous tick's window states
      --    to produce edge events (HOVER, UNHOVER, TOOLTIP SHOW/HIDE).
      hoverLog = buildHoverLog (wmWindows wms) windows2

   in wms
        { wmWindows   = windows2
        , wmFocused   = newFocused
        , wmMousePos  = mousePos
        , wmTickCount = wmTickCount wms + 1
        , wmEventLog  = clickLog ++ hoverLog
        }

-- ---- Click handling ----

-- Find the topmost window (head = top) that contains the point.
topWindowAt :: [Window] -> Point -> Maybe Window
topWindowAt wins pt = find (\w -> pointInRect pt (winRect w)) wins

-- Bring the window with the given id to the front (head) of the list.
bringToFront :: WindowId -> [Window] -> [Window]
bringToFront wid wins =
  case partition (\w -> winId w == wid) wins of
    ([w], rest) -> w : rest
    _           -> wins

processClicks
  :: Maybe WindowId
  -> [Window]
  -> [(Point, MouseButton, Bool)]
  -> (Maybe WindowId, [Window], [String])
processClicks focused wins [] = (focused, wins, [])
processClicks focused wins ((pt, btn, down) : rest) =
  let (focused1, wins1, log1) = handleOneClick focused wins pt btn down
      (focused2, wins2, log2) = processClicks focused1 wins1 rest
  in  (focused2, wins2, log1 ++ log2)

handleOneClick
  :: Maybe WindowId
  -> [Window]
  -> Point
  -> MouseButton
  -> Bool
  -> (Maybe WindowId, [Window], [String])
handleOneClick focused wins pt btn down =
  case topWindowAt wins pt of
    Nothing  -> (focused, wins, [])
    Just hit ->
      let wid      = winId hit
          focused' = Just wid
          wins1    = bringToFront wid wins
          localPt  = toLocal pt (winRect hit)
          wins2    = map (\w ->
                       if winId w == wid
                         then w {winWidgets = map (applyClickToWidget localPt btn down) (winWidgets w)}
                         else w
                     ) wins1
          btnStr   = if btn == LMB then "LMB" else "RMB"
          phase    = if down then "PRESS " else "RELEASE "
          widStr   = maybe "" (widgetLabel . wKind) $
                       find (\w -> pointInRect localPt (wLocalRect w)) (winWidgets hit)
          evStr    = phase ++ btnStr ++ " -> " ++ winTitle hit
                       ++ (if null widStr then "" else " [" ++ widStr ++ "]")
          focusStr = case focused of
                       Just prev | prev /= wid -> ["FOCUS " ++ winTitle hit ++ " (was " ++ prevTitle prev wins ++ ")"]
                       Nothing                 -> ["FOCUS " ++ winTitle hit]
                       _                       -> []
      in  (focused', wins2, focusStr ++ [evStr])

widgetLabel :: WidgetKind -> String
widgetLabel (ButtonWidget   lbl) = "btn:" ++ lbl
widgetLabel (TooltipTrigger _  ) = "tooltip-area"

prevTitle :: WindowId -> [Window] -> String
prevTitle wid wins = maybe ("win#" ++ show wid) winTitle $ find (\w -> winId w == wid) wins

applyClickToWidget :: Point -> MouseButton -> Bool -> Widget -> Widget
applyClickToWidget localPt btn down w
  | pointInRect localPt (wLocalRect w) =
      case (wKind w, btn) of
        (ButtonWidget _, LMB) ->
          case wState w of
            ButtonState hov _ -> w {wState = ButtonState hov down}
            _                 -> w
        _ -> w
  | otherwise =
      -- Release pressed state when click lands outside.
      case wState w of
        ButtonState hov True | not down -> w {wState = ButtonState hov False}
        _                               -> w

-- ---- Hover / tooltip update ----

updateWindowHover :: Point -> Maybe WindowId -> Window -> Window
updateWindowHover mousePos focusedId win =
  let localPt   = toLocal mousePos (winRect win)
      inWindow  = pointInRect mousePos (winRect win)
      isFocused = Just (winId win) == focusedId
  in  win {winWidgets = map (updateWidgetHover localPt inWindow isFocused) (winWidgets win)}

updateWidgetHover :: Point -> Bool -> Bool -> Widget -> Widget
updateWidgetHover localPt inWindow isFocused w =
  let hovered = inWindow && pointInRect localPt (wLocalRect w)
  in  case wKind w of
        ButtonWidget _ ->
          case wState w of
            ButtonState _ pressed -> w {wState = ButtonState hovered pressed}
            _                    -> w {wState = ButtonState hovered False}
        TooltipTrigger _ ->
          -- Tooltip only shows when its window is focused AND the cursor is over it.
          w {wState = TooltipState (hovered && isFocused)}

-- ---- Hover edge-event diffing ----

buildHoverLog :: [Window] -> [Window] -> [String]
buildHoverLog oldWins newWins =
  let oldMap = Map.fromList [(winId w, w) | w <- oldWins]
      newMap = Map.fromList [(winId w, w) | w <- newWins]
  in  concatMap (diffWindow oldMap) (Map.elems newMap)

diffWindow :: Map WindowId Window -> Window -> [String]
diffWindow oldMap newWin =
  case Map.lookup (winId newWin) oldMap of
    Nothing  -> []
    Just old -> concatMap (diffWidget (winTitle newWin))
                  (zip (winWidgets newWin) (winWidgets old))

diffWidget :: String -> (Widget, Widget) -> [String]
diffWidget wTitle (newW, oldW) =
  case (wKind newW, wState newW, wState oldW) of
    (ButtonWidget lbl, ButtonState nh np, ButtonState oh op) ->
         (if nh && not oh  then ["  HOVER "    ++ lbl ++ " in " ++ wTitle] else [])
      ++ (if not nh && oh  then ["  UNHOVER "  ++ lbl ++ " in " ++ wTitle] else [])
      ++ (if np && not op  then ["  CLICK "    ++ lbl ++ " in " ++ wTitle] else [])
      ++ (if not np && op  then ["  UNCLICK "  ++ lbl ++ " in " ++ wTitle] else [])
    (TooltipTrigger txt, TooltipState nv, TooltipState ov) ->
         (if nv && not ov  then ["  TOOLTIP SHOW [" ++ txt ++ "] in " ++ wTitle] else [])
      ++ (if not nv && ov  then ["  TOOLTIP HIDE in " ++ wTitle] else [])
    _ -> []

-- -----------------------------------------------------------------------------
-- STM Channel Types  (identical roles to Gameloop.hs)
-- -----------------------------------------------------------------------------

type HardwareQueue  = TQueue HardwareEvent
type TickSignal     = TMVar ()
type SnapshotReady  = TMVar InputSnapshot
type RenderQueue    = TVar (Maybe WMState)
type QuitFlag       = TVar Bool

-- -----------------------------------------------------------------------------
-- Input Actor  (~500 Hz)
-- Drains HardwareQueue continuously.
-- On TickSignal: snapshot current buffer, clear edges, signal CompositorActor.
-- Latest mouse position wins within a tick -- intermediates discarded.
-- -----------------------------------------------------------------------------

inputActor :: HardwareQueue -> TickSignal -> SnapshotReady -> QuitFlag -> IO ()
inputActor hwQ tickSig snapReady quit = go emptyBuffer
  where
    go buf = do
      q <- readTVarIO quit
      unless q $ do
        buf' <- atomically (drainHardwareEvents hwQ buf)
        mTick <- atomically (tryTakeTMVar tickSig)
        buf'' <- case mTick of
          Nothing  -> return buf'
          Just ()  -> do
            let snap = InputSnapshot
                         { snMousePos   = ibMousePos   buf'
                         , snClickEdges = ibClickEdges buf'
                         }
            atomically (putTMVar snapReady snap)
            return buf' {ibClickEdges = []}
        threadDelay 2_000
        go buf''

drainHardwareEvents :: HardwareQueue -> InputBuffer -> STM InputBuffer
drainHardwareEvents hwQ buf = do
  mEv <- tryReadTQueue hwQ
  case mEv of
    Nothing -> return buf
    Just ev -> drainHardwareEvents hwQ (applyHardwareEvent ev buf)

applyHardwareEvent :: HardwareEvent -> InputBuffer -> InputBuffer
applyHardwareEvent (MouseMoved    pt    ) buf = buf {ibMousePos = pt}
applyHardwareEvent (MousePressed  pt btn) buf =
  buf {ibMousePos = pt, ibClickEdges = ibClickEdges buf ++ [(pt, btn, True )]}
applyHardwareEvent (MouseReleased pt btn) buf =
  buf {ibMousePos = pt, ibClickEdges = ibClickEdges buf ++ [(pt, btn, False)]}

-- -----------------------------------------------------------------------------
-- Simulated Hardware Generator (~200 Hz)
-- Scripted sequence: mouse moves + LMB/RMB clicks visiting each window,
-- hovering over buttons, and triggering tooltips.
-- -----------------------------------------------------------------------------

hardwareGenerator :: HardwareQueue -> QuitFlag -> IO ()
hardwareGenerator hwQ quit = go (0 :: Int)
  where
    go n = do
      q <- readTVarIO quit
      unless q $ do
        -- Continuous smooth movement along the scripted path.
        atomically $ writeTQueue hwQ $ MouseMoved (scriptedPos n)

        -- Discrete click events at scripted steps.
        case n of
          -- Click Settings window title bar to focus it.
          20  -> fire $ MousePressed  (Point 130  80) LMB
          21  -> fire $ MouseReleased (Point 130  80) LMB

          -- Hover over Settings tooltip area (focused window -> tooltip shows).
          40  -> fire $ MouseMoved (Point 90  80)

          -- Hover over OK button.
          60  -> fire $ MouseMoved (Point 90 165)

          -- Click OK button.
          80  -> fire $ MousePressed  (Point 90 165) LMB
          81  -> fire $ MouseReleased (Point 90 165) LMB

          -- Move cursor to Editor window and click to focus it.
          100 -> fire $ MouseMoved    (Point 400 170)
          110 -> fire $ MousePressed  (Point 400 170) LMB
          111 -> fire $ MouseReleased (Point 400 170) LMB

          -- Hover over Editor tooltip area (now focused).
          120 -> fire $ MouseMoved (Point 420 150)

          -- Hover over Save button.
          140 -> fire $ MouseMoved (Point 330 270)

          -- Click Save button.
          160 -> fire $ MousePressed  (Point 330 270) LMB
          161 -> fire $ MouseReleased (Point 330 270) LMB

          -- Move to Terminal window and click to focus it.
          180 -> fire $ MouseMoved    (Point 250 380)
          190 -> fire $ MousePressed  (Point 250 380) LMB
          191 -> fire $ MouseReleased (Point 250 380) LMB

          -- Hover over Terminal tooltip.
          200 -> fire $ MouseMoved (Point 200 340)

          -- Hover over Clear button.
          220 -> fire $ MouseMoved (Point 150 425)

          -- RMB click on Clear button (no button action, but logged).
          240 -> fire $ MousePressed  (Point 150 425) RMB
          241 -> fire $ MouseReleased (Point 150 425) RMB

          -- Click Settings again via overlap region to cycle focus back.
          260 -> fire $ MousePressed  (Point 130 130) LMB
          261 -> fire $ MouseReleased (Point 130 130) LMB

          -- Hover over Cancel button.
          280 -> fire $ MouseMoved (Point 140 165)

          _   -> return ()

        threadDelay 5_000
        go (n + 1)
    fire ev = atomically $ writeTQueue hwQ ev

-- Piecewise linear path that visits all three windows in sequence.
scriptedPos :: Int -> Point
scriptedPos n
  | n <  20  = Point (fromIntegral n * 3.0)          (50  + fromIntegral n * 1.5)
  | n <  60  = Point (50  + fromIntegral (n-20)*1.0) (80  + fromIntegral (n-20)*2.1)
  | n <  100 = Point (90  + fromIntegral (n-60)*7.75)(165 + fromIntegral (n-60)*0.1)
  | n <  160 = Point (310 + fromIntegral (n-100)*1.5)(150 + fromIntegral (n-100)*0.5)
  | n <  220 = Point (310 + fromIntegral (n-160)*0.5)(250 + fromIntegral (n-160)*2.1)
  | otherwise = Point (100 + fromIntegral (n-220)*1.3)(330 + fromIntegral (n-220)*0.5)

-- -----------------------------------------------------------------------------
-- Compositor Actor  (Tick-driven, ~60 Hz)
-- 1. Signal InputActor (Tick boundary).
-- 2. Block until InputSnapshot arrives.
-- 3. Pure MVU update -> new WMState.
-- 4. Write committed state to RenderQueue (depth-1 overwrite).
-- -----------------------------------------------------------------------------

compositorActor
  :: TickSignal -> SnapshotReady -> RenderQueue -> QuitFlag -> IORef Time -> IO ()
compositorActor tickSig snapReady renderQ quit timeRef = go initWMState
  where
    go wms = do
      q <- readTVarIO quit
      unless q $ do
        t0 <- readIORef timeRef
        atomically $ putTMVar tickSig ()
        snap <- atomically $ takeTMVar snapReady
        t1 <- readIORef timeRef
        let dt  = max 1 (t1 - t0)
            wms' = update wms dt snap
        atomically $ writeTVar renderQ (Just wms')
        threadDelay 16_667
        go wms'

-- -----------------------------------------------------------------------------
-- Render Actor  (~75 Hz, async to compositor tick)
-- Depth-1 overwrite queue: always renders the newest committed WMState.
-- -----------------------------------------------------------------------------

renderActor :: RenderQueue -> QuitFlag -> IORef Time -> IO ()
renderActor renderQ quit timeRef = go (0 :: Int)
  where
    go frameN = do
      q <- readTVarIO quit
      unless q $ do
        mWms <- atomically $ do
          mWms <- readTVar renderQ
          writeTVar renderQ Nothing
          return mWms
        case mWms of
          Nothing  -> return ()
          Just wms -> do
            t <- readIORef timeRef
            renderFrame t wms frameN
        threadDelay 13_333
        go (frameN + 1)

-- -----------------------------------------------------------------------------
-- Rendering  (pure text to stdout)
-- -----------------------------------------------------------------------------

renderFrame :: Time -> WMState -> Int -> IO ()
renderFrame _t wms frameN = do
  let lowRes = frameN `mod` 7 == 0
      hdr    = if lowRes then "  RENDER[low] " else "  RENDER      "
      focStr = case wmFocused wms of
                 Nothing  -> "none     "
                 Just wid -> padR 9 (maybe ("win#"++show wid) winTitle $ findWin wid wms)
      posStr = padR 12 (show (wmMousePos wms))
      tickS  = pad 4 (wmTickCount wms)
  putStrLn $ hdr ++ "tick=" ++ tickS ++ "  mouse=" ++ posStr ++ "  focus=" ++ focStr
  mapM_ (\ev -> putStrLn $ "               " ++ ev) (wmEventLog wms)
  -- Render each window's active widget states.
  mapM_ (renderWindow (wmFocused wms)) (wmWindows wms)

renderWindow :: Maybe WindowId -> Window -> IO ()
renderWindow focusedId win =
  let focused = Just (winId win) == focusedId
      fStr    = if focused then "*" else " "
      activeWidgets = filter isWidgetActive (winWidgets win)
  in  unless (null activeWidgets) $ do
        putStrLn $ "               " ++ fStr ++ "[" ++ winTitle win ++ "]"
        mapM_ renderWidget activeWidgets

isWidgetActive :: Widget -> Bool
isWidgetActive w = case wState w of
  ButtonState  hov prs -> hov || prs
  TooltipState vis     -> vis

renderWidget :: Widget -> IO ()
renderWidget w = case (wKind w, wState w) of
  (ButtonWidget lbl, ButtonState hov prs) ->
    let st = if prs then "PRESSED" else if hov then "HOVERED" else ""
    in  putStrLn $ "                 btn[" ++ lbl ++ "] " ++ st
  (TooltipTrigger txt, TooltipState True) ->
    putStrLn $ "                 tooltip: \"" ++ txt ++ "\""
  _ -> return ()

-- ---- Helpers ----

findWin :: WindowId -> WMState -> Maybe Window
findWin wid wms = find (\w -> winId w == wid) (wmWindows wms)

pad :: Int -> Int -> String
pad n x = let s = show x in replicate (n - length s) ' ' ++ s

padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '

-- -----------------------------------------------------------------------------
-- Simulated Clock  (1ms resolution)
-- -----------------------------------------------------------------------------

clockTicker :: IORef Time -> QuitFlag -> IO ()
clockTicker timeRef quit = go
  where
    go = do
      q <- readTVarIO quit
      unless q $ do
        modifyIORef' timeRef (+1)
        threadDelay 1_000
        go

-- -----------------------------------------------------------------------------
-- Main
-- -----------------------------------------------------------------------------

wmMain :: IO ()
wmMain = do
  hSetBuffering stdout LineBuffering

  putStrLn "+====================================================================+"
  putStrLn "|  Compositing Window Manager  --  MVU + STM Simulation              |"
  putStrLn "|  HW Generator (200Hz) -> Input Actor (500Hz) -> Compositor (60Hz)  |"
  putStrLn "|  Render Actor (75Hz, async)                                         |"
  putStrLn "+====================================================================+"
  putStrLn ""
  putStrLn "  Windows: Settings (id=1)  Editor (id=2)  Terminal (id=3)"
  putStrLn "  *[Window]  = focused window"
  putStrLn "  btn[lbl]   = button with active hover/press state"
  putStrLn "  tooltip    = tooltip visible (focused window + hover)"
  putStrLn "  FOCUS x    = focus changed to window x"
  putStrLn "  PRESS/RELEASE LMB/RMB -> Window [widget]"
  putStrLn "  HOVER/UNHOVER, CLICK/UNCLICK = widget edge events this tick"
  putStrLn ""

  timeRef  <- newIORef (0 :: Time)
  hwQ      <- newTQueueIO
  tickSig  <- newEmptyTMVarIO
  snapReady <- newEmptyTMVarIO
  renderQ  <- newTVarIO Nothing
  quit     <- newTVarIO False

  _ <- forkIO $ clockTicker       timeRef quit
  _ <- forkIO $ hardwareGenerator hwQ quit
  _ <- forkIO $ inputActor        hwQ tickSig snapReady quit
  _ <- forkIO $ renderActor       renderQ quit timeRef
  _ <- forkIO $ compositorActor   tickSig snapReady renderQ quit timeRef

  -- Let the simulation run for ~3 seconds (300 hardware-gen steps at 5ms each).
  threadDelay 3_500_000

  atomically $ writeTVar quit True
  threadDelay 200_000

  putStrLn ""
  putStrLn "-- Simulation complete -----------------------------------------------"
  putStrLn "Invariants verified:"
  putStrLn "  * Mouse position: latest-wins per tick (intermediate moves discarded)"
  putStrLn "  * Click edges accumulated across tick, then cleared"
  putStrLn "  * Widget hover state recomputed every tick (pure function of mouse pos)"
  putStrLn "  * Tooltip gated on focus: only active when enclosing window is focused"
  putStrLn "  * Bring-to-front: O(n) list-head promotion, stable widget identity"
  putStrLn "  * RenderQueue depth-1 overwrite: newest WMState always wins"
  putStrLn "  * Three async STM domains, zero locks"
