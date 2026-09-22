-- | The default notation view: integer steps, chords and rests, without a staff.
module NeoMusic.Numeric (numeric, numericHtml, numericLanes) where

import Data.List (nub, sortOn)
import Data.Ratio (numerator, denominator)
import qualified NeoMusic as A
import NeoMusic.Score

-- | Separate simultaneous lines without merging independently timed events.
-- Every lane starts at beat zero; gaps and trailing silence remain explicit.
numericLanes :: Score -> Either String [(Name, Phrase)]
numericLanes score = do
  (total, es) <- flatten score
  let names = nub (map instrument es)
      lanes name =
        let groups = foldl place [] (sortOn onset [e | e <- es, instrument e == name])
        in [(name ++ if length groups > 1 then " / "++show i else "", fill total group)
           | (i,group) <- zip [1::Int ..] groups]
  pure (if null es then [("silence",hush total)] else concatMap lanes names)
  where
    place [] e = [[e]]
    place (lane:lanes) e = case reverse lane of
      previous:_ | onset previous + dur (event previous) <= onset e -> (lane++[e]):lanes
      _ -> lane : place lanes e
    fill total = go 0
      where
        go at [] = hush (total-at)
        go at (e:es) = hush (onset e-at) ++ [event e] ++ go (onset e+dur (event e)) es

rationalText :: Rational -> String
rationalText value = show (numerator value) ++
  if denominator value == 1 then "" else "/"++show (denominator value)

pitchText :: Event -> String
pitchText e = case pitches e of
  [] -> "_"
  [A.Step x] -> show x
  xs -> "("++unwords [show x | A.Step x <- xs]++")"
eventText :: Event -> String
eventText e = pitchText e ++ if dur e == 1 then "" else ":"++rationalText (dur e)

numeric :: Score -> Either String String
numeric score = do
  lanes <- numericLanes score
  pure (unlines [name++": "++unwords (map eventText phrase) | (name,phrase) <- lanes])

-- | Standalone readable page. Hybrid mode is a visual sketch: equal double
-- strokes sit above events; they do not yet encode rhythm or pitch height.
numericHtml :: Bool -> Score -> Either String String
numericHtml hybrid score = do
  lanes <- numericLanes score
  pure $ "<!doctype html><html lang=\"en\"><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">"
    ++"<title>NeoMusic - "++title++"</title><style>"
    ++"body{margin:0;background:#faf9f5;color:#222820;font:18px system-ui,sans-serif}main{max-width:1000px;margin:48px auto;padding:0 24px}"
    ++"h1{font-size:30px;font-weight:500}h2{font-size:14px;font-weight:500;margin:32px 0 16px;color:#53624e}"
    ++"p{font-size:14px;color:#53624e}.phrase{display:flex;flex-wrap:wrap;gap:20px 24px;margin-bottom:36px}"
    ++".event{display:inline-flex;align-items:center;flex-direction:column;gap:12px;font:24px ui-monospace,monospace;white-space:nowrap}"
    ++".bar{width:24px;height:4px;border-top:2px solid currentColor;border-bottom:2px solid currentColor}"
    ++"@media(max-width:480px){main{margin:24px auto}.phrase{gap:18px}.event{font-size:20px}}"
    ++"@media print{body{background:white}main{margin:0}.event{break-inside:avoid}}</style><main><h1>"++title++"</h1>"
    ++"<p>"++(if hybrid then "Prototype: double strokes above numeric events; bar meaning is undecided." else "Integer steps from 0. Parentheses group a chord. _ is a rest. :d gives duration in beats.")++"</p>"
    ++concat ["<section><h2>"++escape name++"</h2><div class=\"phrase\">"++concatMap cell phrase++"</div></section>" | (name,phrase) <- lanes]
    ++"</main></html>"
  where
    title = if hybrid then "Numeric + flat bars" else "Numeric notation"
    cell e = "<span class=\"event\">"++(if hybrid && not (null (pitches e)) then "<span class=\"bar\" aria-hidden=\"true\"></span>" else "")
             ++"<span>"++escape (eventText e)++"</span></span>"
    escape = concatMap (\c -> case c of '&' -> "&amp;"; '<' -> "&lt;"; '>' -> "&gt;"; '"' -> "&quot;"; _ -> [c])
