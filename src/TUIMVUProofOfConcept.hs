{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- TuiPoc: minimal cell-buffer TUI renderer with frame diffing, in the style of
-- the FPRLive gen_view Static/Dyn/Patch pipeline, but with the terminal cell
-- grid as the "DOM".
--
-- Layers (bottom to top):
--   1. Cell buffer   : Array (Row,Col) Cell, Cell = (Char, Fg, Bg). Pure data.
--   2. Widget layer  : Widget = Size -> CellBuffer. Combinators: hbox, vbox
--                      (fixed/flex layout), text, line, border, padding, fill,
--                      rawW (escape hatch for custom drawing, e.g. the ball).
--   3. Diff renderer : diffFrame old new -> escape-code byte stream. Walks both
--                      grids row by row, emits ONE cursor move per changed run,
--                      merges runs separated by small gaps (overwriting a few
--                      unchanged cells is cheaper than another cursor move),
--                      and only emits SGR color codes when fg/bg actually
--                      change, tracking SGR state ACROSS the whole frame.
--   4. MVU tie-in    : view :: Model -> Widget, update :: Msg -> Model -> Model,
--                      the loop re-renders the widget tree each tick and diffs
--                      against the previous frame's buffer -- same shape as
--                      diffing Dyn trees in FPRLive, different backend.
--
-- Wire protocol used (the entire vocabulary):
--   ESC[<r>;<c>H   absolute cursor position
--   ESC[<f>;<b>m   SGR fg/bg color
--   ESC[2J         clear (once, at startup)
--   ESC[?25l/h     hide/show cursor
--   ESC[?1049h/l   alternate screen enter/exit
--
-- Run interactively (real terminal, 80x24 or larger):
--   ghc -O2 TuiPoc.hs && ./TuiPoc
--   keys: q quit, space pause, +/- counter
--
-- Run headless (no tty needed), N frames, stats on stderr:
--   ./TuiPoc --demo 120

module TUIMVUProofOfConcept where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.Array
import Data.List (foldl')
import System.Environment (getArgs)
import System.IO

--------------------------------------------------------------------------------
-- 1. Cell buffer: pure data, no I/O
--------------------------------------------------------------------------------

data Color = Default | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  deriving (Eq, Enum, Show)

type Cell = (Char, Color, Color) -- (glyph, fg, bg)

type Pos = (Int, Int) -- (row, col), 0-based

type Size = (Int, Int) -- (width, height)

type CellBuffer = Array Pos Cell

blank :: Cell
blank = (' ', Default, Default)

emptyBuf :: Size -> CellBuffer
emptyBuf (w, h) = accumArray (\_ c -> c) blank ((0, 0), (h - 1, w - 1)) []

-- build a buffer from sparse cell assignments; out-of-range cells are clipped
bufFrom :: Size -> [(Pos, Cell)] -> CellBuffer
bufFrom (w, h) cs =
  accumArray
    (\_ c -> c)
    blank
    ((0, 0), (h - 1, w - 1))
    [(p, c) | (p@(r, cl), c) <- cs, r >= 0, r < h, cl >= 0, cl < w]

bufSize :: CellBuffer -> Size
bufSize b = let ((r0, c0), (r1, c1)) = bounds b in (c1 - c0 + 1, r1 - r0 + 1)

-- place a child buffer into a parent at a (row, col) offset, clipping
blit :: Pos -> CellBuffer -> CellBuffer -> CellBuffer
blit (ro, co) child parent =
  parent
    // [ ((r + ro, c + co), cell)
         | ((r, c), cell) <- assocs child,
           inRange (bounds parent) (r + ro, c + co)
       ]

--------------------------------------------------------------------------------
-- 2. Widget layer: Widget = Size -> CellBuffer, composable, still pure
--------------------------------------------------------------------------------

type Widget = Size -> CellBuffer

data LMode = Fix Int | Flex Int -- fixed rows/cols vs weighted share of the rest

splitSizes :: Int -> [LMode] -> [Int]
splitSizes avail ms = fill ms shares
  where
    fixedTotal = sum [n | Fix n <- ms]
    flexTotal = max 1 (sum [w | Flex w <- ms])
    rest = max 0 (avail - fixedTotal)
    shares = [rest * w `div` flexTotal | Flex w <- ms]
    extra = rest - sum shares
    fill (Fix n : r) sh = n : fill r sh
    fill (Flex _ : r) (s : sh)
      | null [() | Flex _ <- r] = (s + extra) : fill r sh -- last flex soaks remainder
      | otherwise = s : fill r sh
    fill _ _ = []

hbox :: [(LMode, Widget)] -> Widget
hbox parts sz@(w, h) =
  let widths = splitSizes w (map fst parts)
      offs = scanl (+) 0 widths
   in foldl'
        (\acc (off, wd, (_, wg)) -> blit (0, off) (wg (wd, h)) acc)
        (emptyBuf sz)
        (zip3 offs widths parts)

vbox :: [(LMode, Widget)] -> Widget
vbox parts sz@(w, h) =
  let heights = splitSizes h (map fst parts)
      offs = scanl (+) 0 heights
   in foldl'
        (\acc (off, ht, (_, wg)) -> blit (off, 0) (wg (w, ht)) acc)
        (emptyBuf sz)
        (zip3 offs heights parts)

-- one line of text, top-left, clipped; rest of the area stays blank
text :: Color -> Color -> String -> Widget
text f b s (w, h) =
  bufFrom (w, h) [((0, i), (ch, f, b)) | (i, ch) <- zip [0 ..] (take w s)]

-- like text, but pads with spaces so the bg color fills the whole row (bars)
line :: Color -> Color -> String -> Widget
line f b s (w, h) =
  bufFrom (w, h) [((0, i), (ch, f, b)) | (i, ch) <- zip [0 .. w - 1] (s ++ repeat ' ')]

fillW :: Cell -> Widget
fillW c (w, h) = accumArray (\_ x -> x) c ((0, 0), (h - 1, w - 1)) []

border :: Color -> Widget -> Widget
border col child (w, h)
  | w < 2 || h < 2 = emptyBuf (w, h)
  | otherwise =
      blit (1, 1) (child (w - 2, h - 2)) (bufFrom (w, h) frame)
  where
    bc ch = (ch, col, Default)
    frame =
      [((0, 0), bc '\9484'), ((0, w - 1), bc '\9488')] -- ┌ ┐
        ++ [((h - 1, 0), bc '\9492'), ((h - 1, w - 1), bc '\9496')] -- └ ┘
        ++ [((r, c), bc '\9472') | r <- [0, h - 1], c <- [1 .. w - 2]] -- ─
        ++ [((r, c), bc '\9474') | c <- [0, w - 1], r <- [1 .. h - 2]] -- │

padding :: Int -> Widget -> Widget
padding n child (w, h)
  | w <= 2 * n || h <= 2 * n = emptyBuf (w, h)
  | otherwise = blit (n, n) (child (w - 2 * n, h - 2 * n)) (emptyBuf (w, h))

-- escape hatch: draw arbitrary sparse cells given the size you were granted
rawW :: (Size -> [(Pos, Cell)]) -> Widget
rawW f sz = bufFrom sz (f sz)

--------------------------------------------------------------------------------
-- 3. Diff renderer: oldBuffer -> newBuffer -> escape-code stream
--------------------------------------------------------------------------------

-- A cursor move costs ~6-8 bytes; overwriting up to `mergeGap` UNCHANGED cells
-- to fuse two nearby dirty runs into one write is cheaper than a second move.
mergeGap :: Int
mergeGap = 4

type SgrState = Maybe (Color, Color) -- Nothing = unknown, forces an SGR emit

fgCode, bgCode :: Color -> String
fgCode Default = "39"
fgCode c = show (29 + fromEnum c) -- Black=30 .. White=37
bgCode Default = "49"
bgCode c = show (39 + fromEnum c) -- Black=40 .. White=47

sgr :: Color -> Color -> String
sgr f b = "\ESC[" ++ fgCode f ++ ";" ++ bgCode b ++ "m"

cup :: Int -> Int -> String -- cursor position, 1-based on the wire
cup r c = "\ESC[" ++ show (r + 1) ++ ";" ++ show (c + 1) ++ "H"

-- group sorted dirty column indices into runs, fusing small gaps
mergeRuns :: [Int] -> [(Int, Int)]
mergeRuns [] = []
mergeRuns (x : xs) = go x x xs
  where
    go s e [] = [(s, e)]
    go s e (c : cs)
      | c - e <= mergeGap + 1 = go s c cs
      | otherwise = (s, e) : go c c cs

diffFrame :: SgrState -> CellBuffer -> CellBuffer -> (String, SgrState)
diffFrame st0 old new = foldl' emitRow ("", st0) [0 .. h - 1]
  where
    (w, h) = bufSize new
    emitRow (acc, st) r =
      let dirty = [c | c <- [0 .. w - 1], old ! (r, c) /= new ! (r, c)]
          step (a, s) run = let (str, s') = emitRun r run s in (a ++ str, s')
       in foldl' step (acc, st) (mergeRuns dirty)
    emitRun r (c0, c1) st = (cup r c0 ++ body, st')
      where
        (body, st') = foldl' cellOut ("", st) [c0 .. c1]
        cellOut (a, s) c =
          let (ch, f, b) = new ! (r, c)
           in if s == Just (f, b)
                then (a ++ [ch], s)
                else (a ++ sgr f b ++ [ch], Just (f, b))

-- a buffer nothing can ever equal, so diffing against it paints everything
invalidate :: CellBuffer -> CellBuffer
invalidate = fmap (const ('\0', Default, Default))

fullEmitLen :: CellBuffer -> Int
fullEmitLen buf = length (fst (diffFrame Nothing (invalidate buf) buf))

--------------------------------------------------------------------------------
-- 4. MVU tie-in: model, update, view (view produces the widget tree per tick)
--------------------------------------------------------------------------------

scrW, scrH, sidebarW, canvasW, canvasH :: Int
scrW = 80
scrH = 24
sidebarW = 30
canvasW = scrW - sidebarW - 2 -- interior of the bordered canvas
canvasH = scrH - 2 - 2 -- minus title, status, border rows

data Model = Model
  { mTick :: Int,
    mCount :: Int,
    mBall :: (Int, Int), -- (x, y) in canvas coords
    mVel :: (Int, Int),
    mBar :: Int,
    mPaused :: Bool,
    mStats :: (Int, Int) -- (diff chars, full-frame chars) of the PREVIOUS frame
  }

data Msg = Tick | Inc | Dec | TogglePause

initModel :: Model
initModel = Model 0 0 (3, 2) (1, 1) 0 False (0, 0)

bounce :: Int -> Int -> Int -> (Int, Int)
bounce v dv hi
  | v < 0 = (negate v, negate dv)
  | v > hi = (2 * hi - v, negate dv)
  | otherwise = (v, dv)

update :: Msg -> Model -> Model
update msg m = case msg of
  Inc -> m {mCount = mCount m + 1}
  Dec -> m {mCount = mCount m - 1}
  TogglePause -> m {mPaused = not (mPaused m)}
  Tick ->
    let (x, y) = mBall m
        (dx, dy) = mVel m
        (x', dx') = bounce (x + dx) dx (canvasW - 1)
        (y', dy') = bounce (y + dy) dy (canvasH - 1)
     in m
          { mBall = (x', y'),
            mVel = (dx', dy'),
            mBar = (mBar m + 1) `mod` canvasW,
            mTick = mTick m + 1
          }

canvas :: Model -> Widget
canvas m = rawW $ \(w, h) ->
  let (bx, by) = mBall m
      label = [((0, i), (ch, Yellow, Default)) | (i, ch) <- zip [2 ..] " bouncing ball / sweeping bar "]
      barY = h - 2
      bar = [((barY, c), (' ', Default, Green)) | c <- [0 .. mBar m `mod` max 1 w]]
      ball = [((by, bx), ('O', Red, Default))]
   in label ++ bar ++ ball

sidebar :: Model -> Widget
sidebar m =
  vbox
    [ (Fix 1, text Yellow Default " MVU state"),
      (Fix 1, text Default Default ("   count : " ++ show (mCount m))),
      (Fix 1, text Default Default ("   tick  : " ++ show (mTick m))),
      (Fix 1, text Default Default ("   paused: " ++ show (mPaused m))),
      (Fix 1, text Default Default ""),
      (Fix 1, text Yellow Default " last frame on the wire"),
      (Fix 1, text Default Default ("   diff : " ++ show d ++ " ch")),
      (Fix 1, text Default Default ("   full : " ++ show f ++ " ch")),
      (Fix 1, text Green Default ("   saved: " ++ pct ++ "%")),
      (Flex 1, fillW blank),
      (Fix 1, text Cyan Default " q quit  space pause  +/-")
    ]
  where
    (d, f) = mStats m
    pct = if f == 0 then "-" else show (100 - (100 * d) `div` f)

view :: Model -> Widget
view m =
  vbox
    [ (Fix 1, line Black Cyan "  FPR TUI PoC \8212 cell-buffer diff renderer"),
      ( Flex 1,
        hbox
          [ (Flex 1, border Cyan (canvas m)),
            (Fix sidebarW, border White (padding 1 (sidebar m)))
          ]
      ),
      (Fix 1, line Black White "  same Static/Dyn/diff shape as FPRLive, cell grid instead of DOM")
    ]

--------------------------------------------------------------------------------
-- 5. Runtime loop: render, diff against previous frame, emit only the delta
--------------------------------------------------------------------------------

-- one render step: widget tree from model, diff vs previous buffer.
-- mStats is fed the NEXT model, so the on-screen numbers lag one frame --
-- the honest alternative to re-rendering after measuring (which would change
-- the measurement).
frame :: Model -> CellBuffer -> SgrState -> (Model, CellBuffer, SgrState, String)
frame m prev st =
  let buf = view m (scrW, scrH)
      (out, st') = diffFrame st prev buf
      m' = m {mStats = (length out, fullEmitLen buf)}
   in (m', buf, st', out)

drainInput :: IO [Char]
drainInput = do
  r <- hReady stdin
  if r then (:) <$> getChar <*> drainInput else pure []

keyMsgs :: Char -> [Msg]
keyMsgs = \case
  '+' -> [Inc]
  '=' -> [Inc]
  '-' -> [Dec]
  ' ' -> [TogglePause]
  _ -> []

enterScreen, leaveScreen :: String
enterScreen = "\ESC[?1049h\ESC[?25l\ESC[2J"
leaveScreen = "\ESC[0m\ESC[?25h\ESC[?1049l"

runInteractive :: IO ()
runInteractive = do
  hSetBuffering stdout (BlockBuffering Nothing)
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  putStr enterScreen
  hFlush stdout
  loop initModel (invalidate (emptyBuf (scrW, scrH))) Nothing
    `finally` (putStr leaveScreen >> hFlush stdout)
  where
    loop m prev st = do
      keys <- drainInput
      if 'q' `elem` keys
        then pure ()
        else do
          let m1 = foldl' (flip update) m (concatMap keyMsgs keys)
              m2 = if mPaused m1 then m1 else update Tick m1
              (m3, buf, st', out) = frame m2 prev st
          putStr out
          hFlush stdout
          threadDelay 33000 -- ~30fps tick
          loop m3 buf st'

-- headless: N frames to stdout (escape codes and all), stats to stderr,
-- an Inc event injected every 15 ticks to exercise the sidebar diff path
runDemo :: Int -> IO ()
runDemo n = do
  hSetBuffering stdout (BlockBuffering Nothing)
  putStr "\ESC[2J"
  (diffTotal, fullTotal) <- loop 1 initModel (invalidate (emptyBuf (scrW, scrH))) Nothing (0, 0)
  hFlush stdout
  let avg t = t `div` max 1 n
  hPutStrLn stderr $ "frames: " ++ show n
  hPutStrLn stderr $ "avg diff emission : " ++ show (avg diffTotal) ++ " chars/frame"
  hPutStrLn stderr $ "avg full emission : " ++ show (avg fullTotal) ++ " chars/frame"
  hPutStrLn stderr $
    "avg saved         : "
      ++ show (100 - (100 * avg diffTotal) `div` max 1 (avg fullTotal))
      ++ "%"
  where
    loop k m prev st acc@(dt, ft)
      | k > n = pure acc
      | otherwise = do
          let m1 = if k `mod` 15 == 0 then update Inc m else m
              m2 = update Tick m1
              (m3, buf, st', out) = frame m2 prev st
              (d, f) = mStats m3
          putStr out
          loop (k + 1) m3 buf st' (dt + d, ft + f)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  getArgs >>= \case
    ["--demo", k] -> runDemo (read k)
    _ -> runInteractive
