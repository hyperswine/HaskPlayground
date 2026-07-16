{-# LANGUAGE LambdaCase #-}
-- MiniBrowser: a deliberately tiny "browser" PoC.
--
--   HTML subset  -> DOM
--   Tailwind-ish class subset -> Style
--   Block/Flex layout -> DisplayList -> cell Canvas (simulated 2D lib)
--   MiniScript (tiny JS subset) -> Alpine-like directives (x-data/x-show/x-text/x-on:click)
--   MVU gen_view: Static template + Dyn slots patched over a simulated
--   websocket-like bidirectional wire (FPRLive-style Static/Dyn split).
--
-- Unsupported things WARN and render placeholders instead of failing.

module MiniBrowser where

import           Data.Char       (isAlphaNum, isDigit, isSpace, toUpper)
import           Data.IORef
import           Data.List       (foldl', intercalate, isPrefixOf, maximumBy)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe, mapMaybe, catMaybes)
import           Data.Ord        (comparing)
import           System.IO       (hSetEncoding, stdout, utf8)

--------------------------------------------------------------------------------
-- 1. DOM
--------------------------------------------------------------------------------

data Node = El String [(String,String)] [Node]
          | Tx String
          deriving (Show, Eq)

attr :: String -> [(String,String)] -> Maybe String
attr = lookup

classesOf :: [(String,String)] -> [String]
classesOf as = maybe [] words (attr "class" as)

--------------------------------------------------------------------------------
-- 2. HTML parser (strict-ish, recursive descent; no error recovery heroics)
--------------------------------------------------------------------------------

voidTags :: [String]
voidTags = ["br","hr","input","img","meta","link"]

parseHTML :: String -> [Node]
parseHTML = fst . pNodes

pNodes :: String -> ([Node], String)
pNodes [] = ([], [])
pNodes s@('<':'/':_) = ([], s)
pNodes ('<':'!':r) =                       -- comment / doctype: skip to '>'
  let r' = drop 1 (dropWhile (/= '>') r) in pNodes r'
pNodes ('<':r) =
  let (n, r1)  = pElem r
      (ns, r2) = pNodes r1
  in (n:ns, r2)
pNodes s =
  let (t, r)   = break (== '<') s
      (ns, r') = pNodes r
      t'       = collapseWs t
  in (if null t' then ns else Tx t' : ns, r')

collapseWs :: String -> String
collapseWs = unwords . words

pElem :: String -> (Node, String)
pElem s =
  let (name, r0)  = span isNameChar s
      (as, r1)    = pAttrs (dropWhile isSpace r0)
  in case r1 of
       ('/':'>':r2) -> (El name as [], r2)
       ('>':r2)
         | name `elem` voidTags -> (El name as [], r2)
         | otherwise ->
             let (kids, r3) = pNodes r2
                 r4 = expectClose name r3
             in (El name as kids, r4)
       _ -> (El name as [], r1)  -- malformed; be lenient but move on
  where isNameChar c = isAlphaNum c || c `elem` "-_"

expectClose :: String -> String -> String
expectClose name ('<':'/':r) =
  let (nm, r0) = span (\c -> isAlphaNum c || c `elem` "-_") r
      r1 = drop 1 (dropWhile (/= '>') r0)
  in if nm == name then r1 else r1   -- mismatched close: warn-worthy, tolerate
expectClose _ s = s

pAttrs :: String -> ([(String,String)], String)
pAttrs s = case dropWhile isSpace s of
  r@('>':_)     -> ([], r)
  r@('/':'>':_) -> ([], r)
  r ->
    let (name, r0) = span (\c -> isAlphaNum c || c `elem` "-_:.@") r
    in if null name then ([], r) else
       case dropWhile isSpace r0 of
         ('=':r1) -> case dropWhile isSpace r1 of
            (q:r2) | q == '"' || q == '\'' ->
              let (v, r3) = break (== q) r2
                  (rest, r4) = pAttrs (drop 1 r3)
              in ((name, v):rest, r4)
            r2 -> let (v, r3) = break (\c -> isSpace c || c == '>') r2
                      (rest, r4) = pAttrs r3
                  in ((name, v):rest, r4)
         r1 -> let (rest, r2) = pAttrs r1 in ((name, ""):rest, r2)

--------------------------------------------------------------------------------
-- 3. Style: a Tailwind-flavoured utility-class subset
--------------------------------------------------------------------------------

data Disp = DBlock | DRow | DCol | DNone deriving (Eq, Show)

data Style = Style
  { sDisp   :: Disp
  , sPadX, sPadY, sMarX, sMarY, sGapX, sGapY :: Int
  , sBorder :: Bool
  , sRound  :: Bool
  , sBg     :: Maybe Char        -- background "colour" as a shade glyph
  , sBold   :: Bool
  , sCenter :: Bool
  , sWFull  :: Bool
  , sFlex1  :: Bool
  , sWidth  :: Maybe Int
  } deriving Show

def :: Style
def = Style DBlock 0 0 0 0 0 0 False False Nothing False False False False Nothing

-- Tailwind spacing scale -> terminal cells. Cells are ~2x taller than wide,
-- so vertical measures get scaled down harder.
sx, sy :: Int -> Int
sx n = max (min n 1) (n `div` 2)
sy n = n `div` 4

-- Returns Nothing when the class is unknown => caller emits a warning.
applyClass :: String -> Style -> Maybe Style
applyClass c st = case c of
  "flex"        -> Just st { sDisp = DRow }
  "flex-row"    -> Just st { sDisp = DRow }
  "flex-col"    -> Just st { sDisp = DCol }
  "block"       -> Just st { sDisp = DBlock }
  "hidden"      -> Just st { sDisp = DNone }
  "border"      -> Just st { sBorder = True }
  "rounded"     -> Just st { sRound = True }
  "rounded-lg"  -> Just st { sRound = True }
  "font-bold"   -> Just st { sBold = True }
  "text-center" -> Just st { sCenter = True }
  "w-full"      -> Just st { sWFull = True }
  "flex-1"      -> Just st { sFlex1 = True }
  _ | Just n <- num "p-"   -> Just st { sPadX = sx n, sPadY = sy n }
    | Just n <- num "px-"  -> Just st { sPadX = sx n }
    | Just n <- num "py-"  -> Just st { sPadY = sy n }
    | Just n <- num "m-"   -> Just st { sMarX = sx n, sMarY = sy n }
    | Just n <- num "mx-"  -> Just st { sMarX = sx n }
    | Just n <- num "my-"  -> Just st { sMarY = sy n }
    | Just n <- num "gap-" -> Just st { sGapX = max 1 (sx n), sGapY = sy n }
    | Just n <- num "w-"   -> Just st { sWidth = Just (max 4 (n `div` 2)) }
    | "bg-" `isPrefixOf` c   -> Just st { sBg = Just (shade c) }
    | "text-" `isPrefixOf` c -> Just st            -- colours: known, no-op in cells
    | otherwise -> Nothing
  where
    num p | p `isPrefixOf` c, let d = drop (length p) c, all isDigit d, not (null d)
          = Just (read d)
          | otherwise = Nothing
    shade s | "bg-white" `isPrefixOf` s = ' '
            | any (`isPrefixOf` s) ["bg-gray-1","bg-gray-2","bg-slate-1"] = '·'
            | otherwise = '░'

styleFor :: String -> [String] -> Style
styleFor tag cs = foldl' (\st c -> fromMaybe st (applyClass c st)) base cs
  where base = case tag of
          "button" -> def { sBorder = True, sPadX = 1 }
          "input"  -> def { sBorder = True, sPadX = 1 }
          _        -> def

isKnownClass :: String -> Bool
isKnownClass c = case applyClass c def of Just _ -> True; Nothing -> False

scanClassWarnings :: Node -> [String]
scanClassWarnings (Tx _) = []
scanClassWarnings (El _ as ks) =
  [ "style: unknown utility class \"" ++ c ++ "\" (ignored)"
  | c <- classesOf as, not (isKnownClass c) ]
  ++ concatMap scanClassWarnings ks

supportedDirectives :: [String]
supportedDirectives = ["x-data","x-show","x-text","x-on:click","ws-click","data-slot","id","class"]

scanDirectiveWarnings :: Node -> [String]
scanDirectiveWarnings (Tx _) = []
scanDirectiveWarnings (El _ as ks) =
  [ "dom: unsupported directive \"" ++ k ++ "\" (ignored)"
  | (k,_) <- as, ("x-" `isPrefixOf` k || "ws-" `isPrefixOf` k)
  , k `notElem` supportedDirectives ]
  ++ concatMap scanDirectiveWarnings ks

unsupportedTags :: [String]
unsupportedTags = ["table","iframe","canvas","img","svg","video","select"]

scanTagWarnings :: Node -> [String]
scanTagWarnings (Tx _) = []
scanTagWarnings (El t _ ks)
  | t `elem` unsupportedTags =
      ["layout: <" ++ t ++ "> not supported — rendering placeholder"]
  | otherwise = concatMap scanTagWarnings ks

--------------------------------------------------------------------------------
-- 4. Layout -> display list
--------------------------------------------------------------------------------

data DrawCmd
  = DRect Int Int Int Int Bool Bool (Maybe Char)  -- x y w h border rounded bg
  | DText Int Int String Bool                     -- x y text bold
  deriving Show

data SizeMode = Fixed Int | Shrink Int   -- Shrink carries a max width

layoutN :: SizeMode -> (Int,Int) -> Node -> ([DrawCmd], (Int,Int))
layoutN mode (x,y) (Tx s) =
  let w  = case mode of Fixed w' -> w'; Shrink mw -> min mw (length s)
      ls = wrapText (max 1 w) s
      cs = [ DText x (y+i) l False | (i,l) <- zip [0..] ls ]
  in (cs, (w, length ls))
layoutN mode (x,y) (El tag as kids0)
  | sDisp st == DNone = ([], (0,0))
  | tag `elem` unsupportedTags =
      let label = "[unsupported: <" ++ tag ++ ">]"
          w = case mode of Fixed w' -> w'; Shrink mw -> min mw (length label + 4)
          tx = x + max 0 ((w - length label) `div` 2)
      in ( [ DRect x y w 3 True False (Just '·'), DText tx (y+1) label False ]
         , (w, 3) )
  | otherwise =
      let (mx,my)   = (sMarX st, sMarY st)
          b         = if sBorder st then 1 else 0
          deco      = 2 * sPadX st + 2*b
          availC    = case (sWidth st, mode) of
                        (Just w', _)       -> w' - deco
                        (_, Fixed w')      -> w' - 2*mx - deco
                        (_, Shrink mw)     -> mw - 2*mx - deco
          (cx, cy)  = (x + mx + b + sPadX st, y + my + b + sPadY st)
          (kcmds, usedW, usedH) = layoutContent st availC (cx, cy) kids
          cw        = case (sWidth st, mode) of
                        (Just _, _)   -> availC
                        (_, Fixed _)  -> availC
                        (_, Shrink _) -> min availC usedW
          bw        = cw + deco
          bh        = usedH + 2 * sPadY st + 2*b
          boxCmds   = if sBorder st || sBg st /= Nothing
                        then [DRect (x+mx) (y+my) bw bh (sBorder st) (sRound st) (sBg st)]
                        else []
          kcmds'    = map (emboldenIf (sBold st)) kcmds
      in (boxCmds ++ kcmds', (bw + 2*mx, bh + 2*my))
  where
    st   = styleFor tag (classesOf as)
    kids = filter (not . isScript) kids0
    isScript (El "script" _ _) = True
    isScript _                 = False

emboldenIf :: Bool -> DrawCmd -> DrawCmd
emboldenIf True (DText a b s _) = DText a b s True
emboldenIf _ c = c

-- Lay out children inside content box at origin; returns (cmds, usedW, usedH)
layoutContent :: Style -> Int -> (Int,Int) -> [Node] -> ([DrawCmd], Int, Int)
layoutContent st availC (cx,cy) kids
  | all isText kids =
      let s  = collapseWs (unwords [t | Tx t <- kids])
          ls = wrapText (max 1 availC) s
          w  = if null ls then 0 else maximum (map length ls)
          place i l = let off = if sCenter st then max 0 ((availC - length l) `div` 2) else 0
                      in DText (cx+off) (cy+i) l (sBold st)
      in ( [ place i l | (i,l) <- zip [0..] ls ], w, length ls )
  | sDisp st == DRow =
      let gap    = sGapX st
          meas k = snd (layoutN (Shrink availC) (0,0) k)
          sizes  = map meas kids
          nflex  = length [ () | k <- kids, isFlex1 k ]
          fixedW = sum [ w | (k,(w,_)) <- zip kids sizes, not (isFlex1 k) ]
          gaps   = gap * max 0 (length kids - 1)
          slack  = max 0 (availC - fixedW - gaps)
          share  = if nflex > 0 then slack `div` nflex else 0
          go _  []     acc = acc
          go x' (k:ks) (cs, mh) =
            let w = if isFlex1 k then share else fst (snd (layoutN (Shrink availC) (0,0) k))
                (c,(w',h)) = layoutN (Fixed w) (x', cy) k
            in go (x' + w' + gap) ks (cs ++ c, max mh h)
          (cmds, mh) = go cx kids ([], 0)
          usedW = sum [ if isFlex1 k then share else w | (k,(w,_)) <- zip kids sizes ] + gaps
      in (cmds, usedW, mh)
  | otherwise =   -- DBlock / DCol: stack
      let gap = sGapY st
          go _  []     acc = acc
          go y' (k:ks) (cs, mw, th) =
            let (c,(w,h)) = layoutN (Fixed availC) (cx, y') k
                extra = if null ks then 0 else gap
            in go (y' + h + extra) ks (cs ++ c, max mw w, th + h + extra)
          (cmds, mw, th) = go cy kids ([], 0, 0)
      in (cmds, mw, th)
  where
    isText (Tx _) = True
    isText _      = False
    isFlex1 (El _ as _) = "flex-1" `elem` classesOf as
    isFlex1 _           = False

wrapText :: Int -> String -> [String]
wrapText w = go . words
  where
    go [] = []
    go ws = let (line, rest) = fill [] ws in line : go rest
    fill acc [] = (unwords (reverse acc), [])
    fill acc (v:vs)
      | null acc && length v >= w = (take w v, if length v > w then drop w v : vs else vs)
      | length (unwords (reverse (v:acc))) <= w = fill (v:acc) vs
      | otherwise = (unwords (reverse acc), v:vs)

--------------------------------------------------------------------------------
-- 5. Simulated 2D graphics lib: cell canvas + painter + diff
--------------------------------------------------------------------------------

type Canvas = M.Map (Int,Int) Char

paint :: [DrawCmd] -> Canvas
paint = foldl' step M.empty
  where
    step cv (DRect x y w h brd rnd bg)
      | w <= 0 || h <= 0 = cv
      | otherwise =
        let fill = case bg of
              Just ch -> M.fromList [ ((xi,yi),ch) | xi <- [x..x+w-1], yi <- [y..y+h-1] ]
              Nothing -> M.empty
            (tl,tr,bl,br) = if rnd then ('╭','╮','╰','╯') else ('┌','┐','└','┘')
            edge = if brd then M.fromList
                     (  [ ((xi,y),'─')      | xi <- [x+1..x+w-2] ]
                     ++ [ ((xi,y+h-1),'─')  | xi <- [x+1..x+w-2] ]
                     ++ [ ((x,yi),'│')      | yi <- [y+1..y+h-2] ]
                     ++ [ ((x+w-1,yi),'│')  | yi <- [y+1..y+h-2] ]
                     ++ [ ((x,y),tl), ((x+w-1,y),tr)
                        , ((x,y+h-1),bl), ((x+w-1,y+h-1),br) ] )
                   else M.empty
        in M.union edge (M.union fill cv)
    step cv (DText x y s bold) =
      let s' = if bold then map toUpper s else s
      in M.union (M.fromList [ ((x+i,y), c) | (i,c) <- zip [0..] s' ]) cv

renderCanvas :: Int -> Canvas -> [String]
renderCanvas w cv =
  let maxY = if M.null cv then 0 else maximum (map snd (M.keys cv))
  in [ [ M.findWithDefault ' ' (x,y) cv | x <- [0..w-1] ] | y <- [0..maxY] ]

diffFrames :: [String] -> [String] -> Maybe ((Int,Int),(Int,Int),Int)
diffFrames a b =
  let h = max (length a) (length b)
      w = maximum (0 : map length (a ++ b))
      pad ls = take h (map (\l -> take w (l ++ repeat ' ')) ls ++ repeat (replicate w ' '))
      (a', b') = (pad a, pad b)
      changed = [ (x,y) | (y,(ra,rb)) <- zip [0..] (zip a' b')
                        , (x,(ca,cb)) <- zip [0..] (zip ra rb), ca /= cb ]
  in case changed of
       [] -> Nothing
       cs -> Just ( (minimum (map fst cs), maximum (map fst cs))
                  , (minimum (map snd cs), maximum (map snd cs))
                  , length cs )

--------------------------------------------------------------------------------
-- 6. MiniScript: a very small JS-flavoured expression/statement language
--------------------------------------------------------------------------------

data Val = VN Double | VS String | VB Bool deriving Eq

showV :: Val -> String
showV (VN d) | d == fromIntegral (round d :: Int) = show (round d :: Int)
             | otherwise = show d
showV (VS s) = s
showV (VB b) = if b then "true" else "false"

truthy :: Val -> Bool
truthy (VB b) = b
truthy (VN n) = n /= 0
truthy (VS s) = not (null s)

data Tok = TN Double | TS String | TI String | TP String deriving (Eq, Show)

lexMS :: String -> [Tok]
lexMS [] = []
lexMS (c:cs)
  | isSpace c = lexMS cs
  | isDigit c = let (d, r) = span (\x -> isDigit x || x == '.') (c:cs)
                in TN (read d) : lexMS r
  | c == '\'' || c == '"' =
      let (s, r) = break (== c) cs in TS s : lexMS (drop 1 r)
  | isAlphaNum c || c == '_' =
      let (i, r) = span (\x -> isAlphaNum x || x == '_') (c:cs)
      in TI i : lexMS r
  | otherwise =
      let twos = ["==","!=","<=",">=","&&","||"]
          two  = [c] ++ take 1 cs
      in if two `elem` twos then TP two : lexMS (drop 1 cs)
         else TP [c] : lexMS cs

data Expr = ENum Double | EStr String | EBool Bool | EVar String
          | EUn String Expr | EBin String Expr Expr
          | ETern Expr Expr Expr | EAssign String Expr
          deriving Show

type P a = [Tok] -> Maybe (a, [Tok])

pStmts :: P [Expr]
pStmts ts = do
  (e, r) <- pExpr ts
  case r of
    (TP ";" : r') | null r' -> Just ([e], [])
                  | otherwise -> do (es, r'') <- pStmts r'; Just (e:es, r'')
    _ -> Just ([e], r)

pExpr :: P Expr
pExpr (TI v : TP "=" : ts) = do (e, r) <- pExpr ts; Just (EAssign v e, r)
pExpr ts = pTern ts

pTern :: P Expr
pTern ts = do
  (c, r) <- pOr ts
  case r of
    (TP "?" : r1) -> do
      (a, r2) <- pExpr r1
      case r2 of
        (TP ":" : r3) -> do (b, r4) <- pExpr r3; Just (ETern c a b, r4)
        _ -> Nothing
    _ -> Just (c, r)

binL :: [String] -> P Expr -> P Expr
binL ops sub ts = do
  (l, r) <- sub ts
  let go acc (TP o : r') | o `elem` ops = do
        (rhs, r'') <- sub r'
        go (EBin o acc rhs) r''
      go acc r' = Just (acc, r')
  go l r

pOr, pAnd, pEq, pRel, pAdd, pMul :: P Expr
pOr  = binL ["||"] pAnd
pAnd = binL ["&&"] pEq
pEq  = binL ["==","!="] pRel
pRel = binL ["<",">","<=",">="] pAdd
pAdd = binL ["+","-"] pMul
pMul = binL ["*","/"] pUnary

pUnary :: P Expr
pUnary (TP "!" : ts) = do (e, r) <- pUnary ts; Just (EUn "!" e, r)
pUnary (TP "-" : ts) = do (e, r) <- pUnary ts; Just (EUn "-" e, r)
pUnary ts = pPrim ts

pPrim :: P Expr
pPrim (TN n : ts) = Just (ENum n, ts)
pPrim (TS s : ts) = Just (EStr s, ts)
pPrim (TI "true"  : ts) = Just (EBool True, ts)
pPrim (TI "false" : ts) = Just (EBool False, ts)
pPrim (TI v : ts) = Just (EVar v, ts)
pPrim (TP "(" : ts) = do
  (e, r) <- pExpr ts
  case r of (TP ")" : r') -> Just (e, r'); _ -> Nothing
pPrim _ = Nothing

-- Object literal for x-data: { k: expr, ... }
pObj :: P [(String, Expr)]
pObj (TP "{" : ts) = go ts
  where
    go (TP "}" : r) = Just ([], r)
    go (TI k : TP ":" : r) = do
      (e, r1) <- pExpr r
      case r1 of
        (TP "," : r2) -> do (kvs, r3) <- go r2; Just ((k,e):kvs, r3)
        (TP "}" : r2) -> Just ([(k,e)], r2)
        _ -> Nothing
    go _ = Nothing
pObj _ = Nothing

type Scope = M.Map String Val

evalE :: IORef Scope -> Expr -> IO Val
evalE _ (ENum n)  = pure (VN n)
evalE _ (EStr s)  = pure (VS s)
evalE _ (EBool b) = pure (VB b)
evalE sc (EVar v) = do
  m <- readIORef sc
  case M.lookup v m of
    Just x  -> pure x
    Nothing -> pure (VS ("<undefined:" ++ v ++ ">"))
evalE sc (EUn "!" e) = (VB . not . truthy) <$> evalE sc e
evalE sc (EUn "-" e) = evalE sc e >>= \case VN n -> pure (VN (negate n)); v -> pure v
evalE sc (EUn _ e)   = evalE sc e
evalE sc (ETern c a b) = do
  cv <- evalE sc c
  evalE sc (if truthy cv then a else b)
evalE sc (EAssign v e) = do
  x <- evalE sc e
  modifyIORef' sc (M.insert v x)
  pure x
evalE sc (EBin op a b) = do
  va <- evalE sc a
  vb <- evalE sc b
  pure (binOp op va vb)

binOp :: String -> Val -> Val -> Val
binOp "+" (VN a) (VN b) = VN (a + b)
binOp "+" a b           = VS (showV a ++ showV b)      -- JS-ish coercion
binOp "-" (VN a) (VN b) = VN (a - b)
binOp "*" (VN a) (VN b) = VN (a * b)
binOp "/" (VN a) (VN b) = VN (a / b)
binOp "==" a b = VB (a == b)
binOp "!=" a b = VB (a /= b)
binOp "<"  (VN a) (VN b) = VB (a < b)
binOp ">"  (VN a) (VN b) = VB (a > b)
binOp "<=" (VN a) (VN b) = VB (a <= b)
binOp ">=" (VN a) (VN b) = VB (a >= b)
binOp "&&" a b = VB (truthy a && truthy b)
binOp "||" a b = VB (truthy a || truthy b)
binOp _ a _ = a

runStmts :: IORef Scope -> String -> IO ()
runStmts sc src = case pStmts (lexMS src) of
  Just (es, []) -> mapM_ (evalE sc) es
  _ -> putStrLn ("  ⚠ miniscript: parse error in \"" ++ src ++ "\" (skipped)")

evalExprStr :: IORef Scope -> String -> IO Val
evalExprStr sc src = case pExpr (lexMS src) of
  Just (e, []) -> evalE sc e
  _ -> do putStrLn ("  ⚠ miniscript: parse error in \"" ++ src ++ "\"")
          pure (VS "<error>")

--------------------------------------------------------------------------------
-- 7. Alpine-flavoured runtime over MiniScript
--------------------------------------------------------------------------------

type Path = [Int]

-- Collect x-data scopes, evaluating initial object literals.
initScopes :: Node -> IO [(Path, IORef Scope)]
initScopes = go []
  where
    go p (El _ as ks) = do
      here <- case attr "x-data" as of
        Just src -> case pObj (lexMS src) of
          Just (kvs, []) -> do
            r <- newIORef M.empty
            mapM_ (\(k,e) -> evalE r e >>= \v -> modifyIORef' r (M.insert k v)) kvs
            pure [(p, r)]
          _ -> do putStrLn ("  ⚠ alpine: bad x-data \"" ++ src ++ "\"")
                  pure []
        Nothing -> pure []
      rest <- concat <$> mapM (\(i,k) -> go (p ++ [i]) k) (zip [0..] ks)
      pure (here ++ rest)
    go _ (Tx _) = pure []

nearestScope :: [(Path, IORef Scope)] -> Path -> Maybe (IORef Scope)
nearestScope scopes p =
  case [ (sp, r) | (sp, r) <- scopes, sp `isPrefixOf` p ] of
    [] -> Nothing
    xs -> Just (snd (maximumBy (comparing (length . fst)) xs))

-- Rebuild the render tree from the static base: apply Dyn slots + Alpine bindings.
bindTree :: [(Path, IORef Scope)] -> M.Map String String -> Node -> IO Node
bindTree scopes dyn = go []
  where
    go _ t@(Tx _) = pure t
    go p (El tag as ks) = do
      let msc = nearestScope scopes p
      -- data-slot: server-owned Dyn region
      ks1 <- case attr "data-slot" as >>= \k -> M.lookup k dyn of
        Just v  -> pure [Tx v]
        Nothing -> mapM (\(i,k) -> go (p ++ [i]) k) (zip [0..] ks)
      -- x-text
      ks2 <- case (attr "x-text" as, msc) of
        (Just src, Just sc) -> do v <- evalExprStr sc src; pure [Tx (showV v)]
        _                   -> pure ks1
      -- x-show
      as' <- case (attr "x-show" as, msc) of
        (Just src, Just sc) -> do
          v <- evalExprStr sc src
          pure (if truthy v then as else addClass "hidden" as)
        _ -> pure as
      pure (El tag as' ks2)
    addClass c as = case lookup "class" as of
      Just cs -> ("class", cs ++ " " ++ c) : filter ((/= "class") . fst) as
      Nothing -> ("class", c) : as

findById :: String -> Node -> Maybe (Path, [(String,String)])
findById i = go []
  where
    go _ (Tx _) = Nothing
    go p (El _ as ks)
      | attr "id" as == Just i = Just (p, as)
      | otherwise = case mapMaybe (\(ix,k) -> go (p ++ [ix]) k) (zip [0..] ks) of
          (x:_) -> Just x
          []    -> Nothing

--------------------------------------------------------------------------------
-- 8. Server side: MVU gen_view with Static/Dyn split over a simulated wire
--------------------------------------------------------------------------------

data Model = Model { mCount :: Int, mLastMsg :: String }

updateModel :: String -> Model -> Model
updateModel "incr"  m = m { mCount = mCount m + 1, mLastMsg = "incr" }
updateModel "reset" m = m { mCount = 0,            mLastMsg = "reset" }
updateModel msg     m = m { mLastMsg = "?" ++ msg }

dynView :: Model -> M.Map String String
dynView m = M.fromList
  [ ("status", "connected")
  , ("count",  show (mCount m))
  , ("parity", if even (mCount m) then "even" else "odd")
  ]

dynPatch :: M.Map String String -> M.Map String String -> [(String,String)]
dynPatch old new = [ (k,v) | (k,v) <- M.toList new, M.lookup k old /= Just v ]

--------------------------------------------------------------------------------
-- 9. The page (Static template): Tailwind-ish classes + Alpine-ish directives
--------------------------------------------------------------------------------

pageSrc :: String
pageSrc = unlines
  [ "<div class=\"p-2 flex flex-col gap-2 w-full\">"
  , "  <div class=\"border rounded p-2 flex flex-row gap-2 bg-blue-500\">"
  , "    <span class=\"font-bold\">fpr live demo</span>"
  , "    <span class=\"flex-1\"></span>"
  , "    <span>wire: </span><span data-slot=\"status\">connecting…</span>"
  , "  </div>"
  , "  <div x-data=\"{ open: false, clicks: 0 }\" class=\"border rounded p-2 flex flex-col gap-2\">"
  , "    <div class=\"flex flex-row gap-2\">"
  , "      <button id=\"toggle\" x-on:click=\"open = !open; clicks = clicks + 1\">details</button>"
  , "      <span x-text=\"'local clicks: ' + clicks\"></span>"
  , "      <span x-text=\"open ? '[open]' : '[closed]'\"></span>"
  , "    </div>"
  , "    <div x-show=\"open\" x-transition class=\"border p-2 bg-gray-100 shadow-md\">"
  , "      Client-side Alpine state. This toggle never touches the wire;"
  , "      only MiniScript scope + rebind + repaint."
  , "    </div>"
  , "  </div>"
  , "  <div class=\"border rounded p-2 flex flex-row gap-2\">"
  , "    <button id=\"incr\" ws-click=\"incr\">+1 server</button>"
  , "    <button id=\"reset\" ws-click=\"reset\">reset</button>"
  , "    <span>count =</span>"
  , "    <span class=\"font-bold\" data-slot=\"count\">?</span>"
  , "    <span>(</span><span data-slot=\"parity\">?</span><span>)</span>"
  , "  </div>"
  , "  <table class=\"w-full\"><tr><td>legacy widget</td></tr></table>"
  , "</div>"
  ]

--------------------------------------------------------------------------------
-- 10. Client shell: frames, wire log, diff reporting
--------------------------------------------------------------------------------

screenW :: Int
screenW = 62

renderFrame :: [(Path, IORef Scope)] -> M.Map String String -> Node -> IO [String]
renderFrame scopes dyn base = do
  bound <- bindTree scopes dyn base
  let (cmds, _) = layoutN (Fixed screenW) (0,0) bound
  pure (renderCanvas screenW (paint cmds))

showFrame :: String -> [String] -> Maybe [String] -> IO ()
showFrame label ls prev = do
  putStrLn ""
  putStrLn ("┏━ " ++ label ++ " " ++ replicate (max 0 (screenW - length label - 1)) '━' ++ "┓")
  mapM_ (\l -> putStrLn ("┃" ++ take screenW (l ++ repeat ' ') ++ "┃")) ls
  putStrLn ("┗" ++ replicate (screenW + 2) '━' ++ "┛")
  case prev of
    Nothing -> putStrLn "  paint: full initial paint"
    Just p  -> case diffFrames p ls of
      Nothing -> putStrLn "  paint: no visual change"
      Just ((x0,x1),(y0,y1),n) ->
        putStrLn ("  paint: dirty region cols " ++ show x0 ++ "–" ++ show x1
                  ++ ", rows " ++ show y0 ++ "–" ++ show y1
                  ++ " (" ++ show n ++ " cells repainted)")

wireUp, wireDown :: String -> IO ()
wireUp   s = putStrLn ("  ⇡ ws → server : " ++ s)
wireDown s = putStrLn ("  ⇣ server → ws : " ++ s)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  let base = case parseHTML pageSrc of
               [n] -> n
               ns  -> El "body" [] ns

  putStrLn "── load: parse + static analysis ──────────────────────────────"
  let warns = scanClassWarnings base ++ scanDirectiveWarnings base ++ scanTagWarnings base
  mapM_ (putStrLn . ("  ⚠ " ++)) warns
  putStrLn "  ✓ static DOM built (template is content-addressable; sent once)"

  scopes  <- initScopes base
  putStrLn ("  ✓ alpine: " ++ show (length scopes) ++ " x-data scope(s) initialised")

  dynRef   <- newIORef (M.empty :: M.Map String String)
  modelRef <- newIORef (Model 0 "-")

  -- Helper: click dispatch
  let click i = do
        putStrLn ""
        putStrLn ("── event: click #" ++ i ++ " ─────────────────────────────────────")
        case findById i base of
          Nothing -> putStrLn "  ⚠ no such element"
          Just (p, as) -> case (attr "x-on:click" as, attr "ws-click" as) of
            (Just src, _) -> case nearestScope scopes p of
              Just sc -> do
                runStmts sc src
                m <- readIORef sc
                putStrLn ("  ⚙ miniscript ran \"" ++ src ++ "\"")
                putStrLn ("  ⚙ scope now: " ++ showScope m)
                putStrLn "  ⚙ purely client-side — no wire traffic"
              Nothing -> putStrLn "  ⚠ handler outside any x-data scope"
            (_, Just msg) -> do
              wireUp ("{event:\"click\", msg:\"" ++ msg ++ "\"}")
              old <- dynView <$> readIORef modelRef
              modifyIORef' modelRef (updateModel msg)
              new <- dynView <$> readIORef modelRef
              let patch = dynPatch old new
              wireDown ("patch " ++ showPatch patch)
              modifyIORef' dynRef (\d -> foldl' (\m (k,v) -> M.insert k v m) d patch)
            _ -> putStrLn "  ⚠ element has no click behaviour"

      frame lbl prev = do
        d <- readIORef dynRef
        ls <- renderFrame scopes d base
        showFrame lbl ls prev
        pure ls

  -- Frame 0: before the connection is up (Dyn slots show template fallbacks)
  f0 <- frame "frame 0 · static template, wire not yet connected" Nothing

  -- Connection: server sends the full initial Dyn set
  putStrLn ""
  putStrLn "── connect: ws handshake ──────────────────────────────────────"
  wireUp "{hello, want:\"dyn-init\"}"
  m0 <- dynView <$> readIORef modelRef
  wireDown ("dyn-init " ++ showPatch (M.toList m0))
  writeIORef dynRef m0
  f1 <- frame "frame 1 · dyn slots hydrated from server" (Just f0)

  click "toggle"
  f2 <- frame "frame 2 · alpine open=true (client-only)" (Just f1)

  click "incr"
  f3 <- frame "frame 3 · server count=1 via dyn patch" (Just f2)

  click "incr"
  f4 <- frame "frame 4 · server count=2" (Just f3)

  click "toggle"
  f5 <- frame "frame 5 · alpine open=false again" (Just f4)

  click "reset"
  _  <- frame "frame 6 · server reset" (Just f5)

  putStrLn ""
  putStrLn "── done ───────────────────────────────────────────────────────"

showScope :: Scope -> String
showScope m = "{" ++ intercalate ", " [ k ++ ": " ++ showV v | (k,v) <- M.toList m ] ++ "}"

showPatch :: [(String,String)] -> String
showPatch kvs = "[" ++ intercalate ", " [ k ++ "←\"" ++ v ++ "\"" | (k,v) <- kvs ] ++ "]"
