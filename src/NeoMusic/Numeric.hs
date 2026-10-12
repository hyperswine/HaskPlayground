-- | The default notation view: integer steps, chords and rests, without a staff.
--
-- Simultaneous lanes are aligned in columns: every event onset in any lane starts
-- a column, so events that sound together are printed one above the other. A
-- column is a moment in time, not a bar; blank cells mean the lane's previous
-- event is still sounding. Wrapping into systems is page layout only.
module NeoMusic.Numeric (NumericView (..), plain, numeric, numericPage, numericHtml, numericLanes, Legend (..), legend) where

import Data.List (intercalate, nub, sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Ratio (denominator, numerator)
import NeoMusic.Midi (midiPitch)
import qualified NeoMusic.Pitch as A
import NeoMusic.Score

-- | What the reading view shows beyond steps and durations. The default,
-- 'plain', is just the numbers: integer steps (with any cents offset, which is
-- part of the pitch), durations, rests and the tempo lane. 'showMarks' adds
-- reading aids for gates and curves; 'flatBars' is the hybrid HTML sketch.
data NumericView = NumericView {showMarks :: Bool, flatBars :: Bool}

plain :: NumericView
plain = NumericView False False

-- | With marks off, events are shown as if they had no gate or curves.
visible :: NumericView -> Event -> Event
visible view e
  | showMarks view = e
  | otherwise = e {gate = 1, bends = [], swells = [], vibrato = 0}

-- | Separate simultaneous lines without merging independently timed events.
-- Every lane starts at beat zero; gaps and trailing silence remain explicit.
numericLanes :: Score -> Either String [(Name, Phrase)]
numericLanes score = do
  (total, es) <- flatten score
  let names = nub (map instrument es)
      lanes name =
        let groups = foldl place [] (sortOn onset [e | e <- es, instrument e == name])
         in [ (name ++ if length groups > 1 then " / " ++ show i else "", fill total group)
              | (i, group) <- zip [1 :: Int ..] groups
            ]
  pure (if null es then [("silence", hush total)] else concatMap lanes names)
  where
    place [] e = [[e]]
    place (lane : lanes) e = case reverse lane of
      previous : _ | onset previous + dur (event previous) <= onset e -> (lane ++ [e]) : lanes
      _ -> lane : place lanes e
    fill total = go 0
      where
        go at [] = hush (total - at)
        go at (e : es) = hush (onset e - at) ++ [event e] ++ go (onset e + dur (event e)) es

-- | A cell is a voice's event or a tempo-lane annotation.
data Cell = EventCell Event | MarkCell String

-- | One column per distinct onset across all lanes, plus every tempo change's
-- start and end; a lane has a cell in a column only if something starts there.
-- The tempo lane comes first and only when the score has tempo changes.
layout :: NumericView -> Score -> Either String [(Name, [Maybe Cell])]
layout view score = do
  lanes <- map (fmap (map (visible view))) <$> numericLanes score
  marks <- tempoMarks score
  let starts phrase = M.fromList (zip (scanl (+) 0 (map dur phrase)) phrase)
      columns = nub (sort (concatMap (M.keys . starts . snd) lanes ++ map fst marks))
      markAt = M.fromList marks
      tempoRow = [("tempo", map (fmap MarkCell . (`M.lookup` markAt)) columns) | not (null marks)]
  pure (tempoRow ++ [(name, map (fmap EventCell . (`M.lookup` starts phrase)) columns) | (name, phrase) <- lanes])

-- | Tempo annotations by written beat, as source-like text: a shape where it
-- starts, and where it ends "a tempo" (or "straight" after swing) unless an
-- earlier change is still going. A constant slow-down covering a single onset
-- reads as "hold k", a fermata, and needs no end mark.
tempoMarks :: Score -> Either String [(Rational, String)]
tempoMarks score = do
  (total, es) <- flatten score
  let nodes = [node | node@(_, len, _) <- tempoNodes score, len > 0]
      onsets = nub (sort [onset e | e <- es, not (null (pitches (event e)))])
      single (t0, len, _) = [t | t <- onsets, t0 <= t, t < t0 + len] == [t0]
      active x = any (\(t0, len, _) -> t0 < x && x < t0 + len) nodes
      startText node@(_, _, shape) = case shape of
        Speed r | r < 1 && single node -> "hold " ++ rationalText (1 / r)
        Speed r -> "speed " ++ rationalText r
        Ramp a b -> "speed " ++ rationalText a ++ " -> " ++ rationalText b
        Swing p sh -> "swing " ++ rationalText sh ++ (if p == 1 then "" else " per " ++ rationalText p)
      isHold node@(_, _, shape) = case shape of Speed r -> r < 1 && single node; _ -> False
      endText (Swing _ _) = "straight"
      endText _ = "a tempo"
      -- Ends sort before starts at the same beat: "a tempo, swing 2/3".
      raw = [(t0 + len, 0 :: Int, endText shape) | node@(t0, len, shape) <- nodes, not (isHold node), t0 + len < total, not (active (t0 + len))]
            ++ [(t0, 1, startText node) | node@(t0, _, _) <- nodes]
  pure [(t, intercalate ", " (nub [x | (u, _, x) <- sortOn (\(u, k, _) -> (u, k)) raw, u == t])) | t <- nub (sort [u | (u, _, _) <- raw])]

-- | Greedy page layout: split column indices so each system's text fits @budget@.
systems :: Int -> [Int] -> [[Int]]
systems budget widths = filter (not . null) (go 0 [] (zip [0 ..] widths))
  where
    go _ current [] = [reverse current]
    go used current ((i, w) : more)
      | null current || used + w + 1 <= budget = go (used + w + 1) (i : current) more
      | otherwise = reverse current : go (w + 1) [i] more

cellText :: Maybe Cell -> String
cellText Nothing = ""
cellText (Just (EventCell e)) = eventText e
cellText (Just (MarkCell m)) = m

columnWidths :: [(Name, [Maybe Cell])] -> [Int]
columnWidths rows = case rows of
  [] -> []
  (_, first) : _ -> [maximum [length (cellText (cells !! i)) | (_, cells) <- rows] | i <- [0 .. length first - 1]]

textSystem :: [(Name, [Maybe Cell])] -> [Int] -> [Int] -> [String]
textSystem rows widths columns =
  [ trimEnd (pad labelWidth (name ++ ":") ++ " " ++ unwords [pad (widths !! i) (cellText (cells !! i)) | i <- columns])
    | (name, cells) <- shownRows rows columns
  ]
  where
    labelWidth = maximum (0 : [length name + 1 | (name, _) <- rows])
    pad k s = s ++ replicate (k - length s) ' '
    trimEnd = reverse . dropWhile (== ' ') . reverse

-- | The tempo lane is only printed in systems where something changes.
shownRows :: [(Name, [Maybe Cell])] -> [Int] -> [(Name, [Maybe Cell])]
shownRows rows columns = [row | row@(name, cells) <- rows, not (isTempo name cells)]
  where isTempo name cells = name == "tempo" && all (\i -> null (cellText (cells !! i))) columns && any isMark cells
        isMark (Just (MarkCell _)) = True
        isMark _ = False

-- | Aligned lanes on unbounded lines, without a legend.
numeric :: NumericView -> Score -> Either String String
numeric view score = do
  rows <- layout view score
  let widths = columnWidths rows
  pure (unlines (textSystem rows widths [0 .. length widths - 1]))

-- | A printable page: the interpretation legend, then aligned lanes wrapped
-- into systems about 72 characters wide.
numericPage :: NumericView -> Performance -> Score -> Either String String
numericPage view perf score = do
  info <- shownLegend view <$> legend perf score
  rows <- layout view score
  let widths = columnWidths rows
      labelWidth = maximum (0 : [length name + 2 | (name, _) <- rows])
      body = map (textSystem rows widths) (systems (max 24 (72 - labelWidth)) widths)
  pure (unlines (legendText info ++ [""]) ++ intercalate "\n" (map unlines body))

-- | The legend explains marks only when they are shown.
shownLegend :: NumericView -> Legend -> Legend
shownLegend view info = if showMarks view then info else info {legendMarks = []}

-- | What a reader needs beside the numbers: tempo, and for each group of
-- instruments sharing a tuning, what step 0 is and which physical keys the
-- piece uses (12-TET only).
data Legend = Legend
  { legendTempo :: Double,
    legendGroups :: [([Name], String, Maybe [(Int, Int)])], -- names, tuning text, (step, MIDI) keys
    legendMarks :: [String] -- explanations of the marks the piece uses
  }

legend :: Performance -> Score -> Either String Legend
legend perf score = do
  validate perf score
  (_, es) <- flatten score
  let used = nub (map instrument es)
      tuningOf name = tuning (instruments perf M.! name)
      describe t =
        let divisions = 1 / logBase 2 (A.ratio t) :: Double
            edo = abs (divisions - fromInteger (round divisions)) < 1e-9
         in (if edo then show (round divisions :: Integer) ++ "-EDO" else "ratio " ++ show (A.ratio t) ++ " per step")
              ++ ", 0 = "
              ++ either (const (hzText (A.f0 t))) pitchName (midiPitch t (A.Step 0))
      -- Instruments are grouped by their tuning, so both hands of a piano share a line.
      groups = nub (map tuningOf used)
      group t = do
        let names = [name | name <- used, tuningOf name == t]
            steps = nub (sort [x | e <- es, instrument e `elem` names, A.Step x <- pitches (event e)])
        keys <- case midiPitch t (A.Step 0) of
          Left _ -> Right Nothing
          Right _ -> Just <$> mapM (\x -> (,) x <$> midiPitch t (A.Step x)) steps
        pure (names, describe t, keys)
      present = concatMap (eventMarks . event) es
      marks = [m | (m, _) <- markMeanings, m `elem` present]
      explain m = m ++ " = " ++ maybe "" id (lookup m markMeanings)
  described <- mapM group groups
  pure (Legend (bpm perf) described (map explain marks))

legendText :: Legend -> [String]
legendText (Legend speed groups marks) =
  ("tempo " ++ tempoText speed ++ ", durations in beats (default 1)")
    : marks
    ++ concat
      [ (intercalate ", " names ++ ": " ++ text)
          : maybe [] (\ks -> map ("  keys: " ++) (wrapWords 70 [show x ++ "=" ++ pitchName p | (x, p) <- ks])) keys
        | (names, text, keys) <- groups
      ]

wrapWords :: Int -> [String] -> [String]
wrapWords budget ws = map unwords (filter (not . null) (go 0 [] ws))
  where
    go _ current [] = [reverse current]
    go used current (w : more)
      | null current || used + length w + 1 <= budget = go (used + length w + 1) (w : current) more
      | otherwise = reverse current : go (length w + 1) [w] more

tempoText :: Double -> String
tempoText x
  | x == fromInteger (round x) = show (round x :: Integer)
  | otherwise = show x

hzText :: Double -> String
hzText x = show (fromInteger (round (x * 100)) / 100 :: Double) ++ " Hz"

-- | Scientific pitch name, sharps only: 60 = C4.
pitchName :: Int -> String
pitchName p = (names !! (p `mod` 12)) ++ show (p `div` 12 - 1)
  where
    names = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

rationalText :: Rational -> String
rationalText value =
  show (numerator value)
    ++ if denominator value == 1 then "" else "/" ++ show (denominator value)

pitchText :: Event -> String
pitchText e = case pitches e of
  [] -> "_"
  [A.Step x] -> show x
  xs -> "(" ++ unwords [show x | A.Step x <- xs] ++ ")"

eventText :: Event -> String
eventText e = pitchText e ++ centsText e ++ (if dur e == 1 then "" else ":" ++ rationalText (dur e)) ++ concat (eventMarks e)

-- | A fine offset is part of the pitch, so it is shown exactly: @7+14c@.
centsText :: Event -> String
centsText e
  | null (pitches e) || cents e == 0 = ""
  | otherwise = (if cents e > 0 then "+" else "") ++ rationalText (cents e) ++ "c"

-- | Reading aids, not exact values, in this order after an event: gate, bend,
-- vibrato, swell.
eventMarks :: Event -> [String]
eventMarks e
  | null (pitches e) = []
  | otherwise = filter (not . null)
      [ gateMark e
      , case bendMotion e of Rising -> "/"; Falling -> "\\"; Returning -> "^"; Flat -> ""
      , if vibrato e > 0 then "w" else ""
      , case swellMotion e of Rising -> "<"; Falling -> ">"; Returning -> "<>"; Flat -> "" ]

-- | The legend line for each mark, in display order.
markMeanings :: [(String, String)]
markMeanings =
  [ ("'", "detached: sounds shorter than written")
  , ("~", "rings on past its written length")
  , ("/", "pitch bends up")
  , ("\\", "pitch bends down")
  , ("^", "pitch bends away and back")
  , ("w", "vibrato")
  , ("<", "swells louder")
  , (">", "fades")
  , ("<>", "swells and fades") ]

-- | A reading aid, not the exact ratio: ' sounds shorter than written, ~ rings on.
gateMark :: Event -> String
gateMark e
  | null (pitches e) || gate e == 1 = ""
  | gate e < 1 = "'"
  | otherwise = "~"

-- | Standalone readable page: legend, a keyboard strip per 12-TET tuning
-- showing which step is which key, and the aligned lanes. Hybrid mode is a
-- visual sketch: equal double strokes sit above events; they do not yet
-- encode rhythm or pitch height.
numericHtml :: NumericView -> Performance -> Score -> Either String String
numericHtml view perf score = do
  info <- shownLegend view <$> legend perf score
  rows <- layout view score
  let widths = columnWidths rows
  pure $
    "<!doctype html><html lang=\"en\"><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">"
      ++ "<title>NeoMusic - "
      ++ title
      ++ "</title><style>"
      ++ ":root{--bg:#faf9f5;--fg:#222820;--muted:#53624e;--rule:#d8d6cc;--accent:#2f6f4e;--black:#222820;--white:#fffefa}"
      ++ "body{margin:0;background:var(--bg);color:var(--fg);font:18px system-ui,sans-serif}main{max-width:1000px;margin:48px auto;padding:0 24px}"
      ++ "h1{font-size:30px;font-weight:500;margin-bottom:8px}p{font-size:14px;color:var(--muted);margin:4px 0}"
      ++ ".legend{margin:16px 0 40px}.keys{overflow-x:auto;padding:8px 0 4px}.kb{position:relative;height:96px}"
      ++ ".w,.b{position:absolute;top:0;box-sizing:border-box;border:1px solid var(--fg);border-radius:0 0 4px 4px;display:flex;flex-direction:column;justify-content:flex-end;align-items:center;font:12px ui-monospace,monospace;padding-bottom:4px}"
      ++ ".w{width:30px;height:96px;background:var(--white);color:var(--fg)}.b{width:20px;height:58px;background:var(--black);color:var(--white);z-index:1}"
      ++ ".u{font-weight:700}.w.u{background:#dcebdd}.b.u{background:var(--accent)}.zero{outline:2px solid var(--accent);outline-offset:-4px}.c{color:var(--muted);font-size:10px}"
      ++ ".system{display:grid;gap:14px 22px;align-items:end;overflow-x:auto;padding:16px 0 24px;border-top:1px solid var(--rule)}"
      ++ ".lane{font-size:14px;color:var(--muted);align-self:center;white-space:nowrap}"
      ++ ".event{justify-self:start;display:inline-flex;align-items:flex-start;flex-direction:column;gap:10px;font:24px ui-monospace,monospace;white-space:nowrap}"
      ++ ".mark{justify-self:start;align-self:end;font-size:14px;font-style:italic;color:var(--muted);white-space:nowrap}"
      ++ ".bar{width:24px;height:4px;border-top:2px solid currentColor;border-bottom:2px solid currentColor}"
      ++ "@media(max-width:480px){main{margin:24px auto;padding:0 16px}.system{gap:12px 16px}.event{font-size:20px}}"
      ++ "@media print{body{background:white}main{margin:0}.system{break-inside:avoid}}</style><main><h1>"
      ++ title
      ++ "</h1>"
      ++ "<p>"
      ++ (if hybrid then "Prototype: double strokes above numeric events; bar meaning is undecided." else "Integer steps from 0. Parentheses group a chord. _ is a rest. :d gives duration in beats.")
      ++ "</p>"
      ++ "<p>Events in the same column start together. A blank cell means that lane's previous event is still sounding.</p>"
      ++ "<section class=\"legend\">"
      ++ concatMap (\l -> "<p>" ++ escape l ++ "</p>") (take (1 + length (legendMarks info)) (legendText info))
      ++ concat ["<p>" ++ escape (intercalate ", " names ++ ": " ++ text) ++ "</p>" ++ maybe "" keyboard keys | (names, text, keys) <- legendGroups info]
      ++ "</section>"
      ++ concatMap (htmlSystem rows) (systems 44 widths)
      ++ "</main></html>"
  where
    hybrid = flatBars view
    title = if hybrid then "Numeric + flat bars" else "Numeric notation"
    htmlSystem rows columns =
      "<section class=\"system\" style=\"grid-template-columns:max-content repeat("
        ++ show (length columns)
        ++ ",max-content)\">"
        ++ concat ["<div class=\"lane\">" ++ escape name ++ "</div>" ++ concat [cell (cells !! i) | i <- columns] | (name, cells) <- shownRows rows columns]
        ++ "</section>"
    cell Nothing = "<span></span>"
    cell (Just (MarkCell m)) = "<span class=\"mark\">" ++ escape m ++ "</span>"
    cell (Just (EventCell e)) =
      "<span class=\"event\">"
        ++ (if hybrid && not (null (pitches e)) then "<span class=\"bar\" aria-hidden=\"true\"></span>" else "")
        ++ "<span>"
        ++ escape (eventText e)
        ++ "</span></span>"
    keyboard [] = ""
    keyboard ks =
      let byMidi = M.fromList [(p, x) | (x, p) <- ks]
          lo = 12 * (minimum (map snd ks) `div` 12)
          hi = 12 * (maximum (map snd ks) `div` 12) + 11
          isBlack p = (p `mod` 12) `elem` [1, 3, 6, 8, 10]
          whitesBefore p = length [q | q <- [lo .. p - 1], not (isBlack q)]
          key p =
            let black = isBlack p
                left = if black then whitesBefore p * 30 - 10 else whitesBefore p * 30
                step = M.lookup p byMidi
                classes =
                  (if black then "b" else "w")
                    ++ maybe "" (const " u") step
                    ++ (if step == Just 0 then " zero" else "")
                label = maybe (if p `mod` 12 == 0 then "<span class=\"c\">" ++ pitchName p ++ "</span>" else "") show step
                name = pitchName p ++ maybe "" (\x -> ", step " ++ show x) step
             in "<div class=\"" ++ classes ++ "\" style=\"left:" ++ show left ++ "px\" title=\"" ++ name ++ "\">" ++ label ++ "</div>"
          whiteCount = length [q | q <- [lo .. hi], not (isBlack q)]
       in "<div class=\"keys\"><div class=\"kb\" role=\"img\" aria-label=\"Keyboard: highlighted keys are the steps this piece uses\" style=\"width:"
            ++ show (whiteCount * 30)
            ++ "px\">"
            ++ concatMap key [lo .. hi]
            ++ "</div></div>"
    escape = concatMap (\c -> case c of '&' -> "&amp;"; '<' -> "&lt;"; '>' -> "&gt;"; '"' -> "&quot;"; _ -> [c])
