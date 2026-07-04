{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

-- FPRLive protocol simulation
--
-- Layers (bottom to top):
--   1. View IR: Static / Dyn / Patch + diff/apply     (from the gen_view sim)
--   2. FPRLive session protocol:
--        Hello        client -> server   { vocab, impl = SelfProvided vocab
--                                                       | WantsDefault name,
--                                          cached module hashes }
--        Rejected     server -> client   vocabulary mismatch, clean refusal
--        Bootstrap    server -> client   content-addressed module ref,
--                                        bytes elided on cache hit
--        Mount        server -> client   seq 0, Static + initial Dyn
--        SPatch       server -> client   seq n, pruned patch
--        CatchUp      server -> client   single patch spanning many seqs
--        Event        client -> server   message name (client view fn emitted)
--        ResyncReq    client -> server   "I'm at seq k, I saw a gap"
--        ResumeReq    client -> server   reconnect: session id + last seq
--   3. Client view functions: SAME Static/Dyn/Patch stream interpreted three
--      ways -- browser (HTML), TUI (box text), embedded (one-line LCD).
--
-- Server keeps, per session: shadow Dyn (last acked view state) plus a ring of
-- recent (seq, Dyn) so ResyncReq/ResumeReq are answered with ONE catch-up
-- patch (diff of retained historic Dyn vs current) instead of a full remount.

module MVUModLiveView where

import Data.IORef
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

type Key = Int

--------------------------------------------------------------------------------
-- 1. View IR (condensed from the gen_view simulation)
--------------------------------------------------------------------------------

data View m
  = VStack [View m]
  | TextS String
  | TextD (m -> String)
  | Button String [View m] -- static event name (msg constructor)
  | Cond (m -> Int) [View m]
  | forall i. ForEach (m -> [(Key, i)]) (View i)

data Static
  = SStack [Static]
  | SText String
  | SHole
  | SButton String [Static]
  | SCond [Static]
  | SForEach Static
  deriving (Eq, Show)

data Dyn
  = DStack [Dyn]
  | DNone
  | DScalar String
  | DCond Int Dyn
  | DRows [(Key, Dyn)]
  deriving (Eq, Show)

data Patch
  = PStack [(Int, Patch)]
  | PScalar String
  | PSwitch Int Dyn
  | PInner Patch
  | PRows RowsPatch
  deriving (Eq, Show)

data RowsPatch = RowsPatch
  { rpOrder :: Maybe [Key],
    rpInserted :: [(Key, Dyn)],
    rpChanged :: [(Key, Patch)]
  }
  deriving (Eq, Show)

staticOf :: View m -> Static
staticOf = \case
  VStack ks -> SStack (map staticOf ks)
  TextS s -> SText s
  TextD _ -> SHole
  Button n ks -> SButton n (map staticOf ks)
  Cond _ bs -> SCond (map staticOf bs)
  ForEach _ t -> SForEach (staticOf t)

extract :: View m -> m -> Dyn
extract v m = case v of
  VStack ks -> DStack [extract k m | k <- ks]
  TextS _ -> DNone
  TextD f -> DScalar (f m)
  Button _ ks -> DStack [extract k m | k <- ks]
  Cond s bs -> let t = s m in DCond t (extract (bs !! t) m)
  ForEach f t -> DRows [(k, extract t i) | (k, i) <- f m]

diff :: Dyn -> Dyn -> Maybe Patch
diff old new = case (old, new) of
  (DNone, DNone) -> Nothing
  (DScalar a, DScalar b) | a == b -> Nothing | otherwise -> Just (PScalar b)
  (DStack as, DStack bs) ->
    let ps = [(i, p) | (i, a, b) <- zip3 [0 ..] as bs, Just p <- [diff a b]]
     in if null ps then Nothing else Just (PStack ps)
  (DCond t1 d1, DCond t2 d2)
    | t1 /= t2 -> Just (PSwitch t2 d2)
    | otherwise -> PInner <$> diff d1 d2
  (DRows as, DRows bs) ->
    let om = M.fromList as
        order = if map fst as == map fst bs then Nothing else Just (map fst bs)
        ins = [(k, d) | (k, d) <- bs, not (M.member k om)]
        chg =
          [ (k, p) | (k, d) <- bs, Just o <- [M.lookup k om], Just p <- [diff o d]
          ]
     in case (order, ins, chg) of
          (Nothing, [], []) -> Nothing
          _ -> Just (PRows (RowsPatch order ins chg))
  _ -> error "shape mismatch"

applyPatch :: Dyn -> Patch -> Dyn
applyPatch d p = case (d, p) of
  (DScalar _, PScalar s) -> DScalar s
  (DStack ks, PStack ps) ->
    let pm = M.fromList ps
     in DStack [maybe k (applyPatch k) (M.lookup i pm) | (i, k) <- zip [0 ..] ks]
  (DCond _ _, PSwitch t nd) -> DCond t nd
  (DCond t b, PInner ip) -> DCond t (applyPatch b ip)
  (DRows rs, PRows rp) ->
    let base =
          foldl
            (\m (k, q) -> M.adjust (`applyPatch` q) k m)
            (M.fromList rs)
            (rpChanged rp)
        wIns = foldl (\m (k, nd) -> M.insert k nd m) base (rpInserted rp)
        ord = fromMaybe (map fst rs) (rpOrder rp)
     in DRows [(k, wIns M.! k) | k <- ord]
  _ -> error "patch mismatch"

dynSize :: Dyn -> Int
dynSize = \case
  DStack ds -> 1 + sum (map dynSize ds)
  DNone -> 0
  DScalar s -> 1 + length s
  DCond _ d -> 1 + dynSize d
  DRows rs -> 1 + sum [1 + dynSize d | (_, d) <- rs]

patchSize :: Patch -> Int
patchSize = \case
  PStack ps -> 1 + sum [1 + patchSize p | (_, p) <- ps]
  PScalar s -> 1 + length s
  PSwitch _ d -> 1 + dynSize d
  PInner p -> 1 + patchSize p
  PRows rp ->
    1
      + maybe 0 length (rpOrder rp)
      + sum [1 + dynSize d | (_, d) <- rpInserted rp]
      + sum [1 + patchSize p | (_, p) <- rpChanged rp]

staticSize :: Static -> Int
staticSize = \case
  SStack ks -> 1 + sum (map staticSize ks)
  SText s -> 1 + length s
  SHole -> 1
  SButton n ks -> 1 + length n + sum (map staticSize ks)
  SCond bs -> 1 + sum (map staticSize bs)
  SForEach t -> 1 + staticSize t

--------------------------------------------------------------------------------
-- 2. The example app (server side)
--------------------------------------------------------------------------------

data Model = Model {count :: Int, dark :: Bool, todos :: [(Key, String)]}
  deriving (Show)

data Msg = Inc | ToggleTheme | AddTodo String | DelTodo Key

update :: Msg -> Model -> Model
update msg m = case msg of
  Inc -> m {count = count m + 1}
  ToggleTheme -> m {dark = not (dark m)}
  AddTodo s -> m {todos = todos m ++ [(1 + maximum (0 : map fst (todos m)), s)]}
  DelTodo k -> m {todos = filter ((/= k) . fst) (todos m)}

parseEvent :: String -> Maybe Msg
parseEvent = \case
  "Inc" -> Just Inc
  "ToggleTheme" -> Just ToggleTheme
  'A' : 'd' : 'd' : ' ' : s -> Just (AddTodo s)
  'D' : 'e' : 'l' : ' ' : k -> Just (DelTodo (read k))
  _ -> Nothing

appView :: View Model
appView =
  VStack
    [ TextD (\m -> "Count: " ++ show (count m)),
      Button "Inc" [TextS "+1"],
      Button
        "ToggleTheme"
        [Cond (fromEnum . dark) [TextS "Switch to Dark", TextS "Switch to Light"]],
      ForEach todos (TextD id)
    ]

--------------------------------------------------------------------------------
-- 3. FPRLive protocol messages
--------------------------------------------------------------------------------

type Hash = String

type SessId = Int

type Seq = Int

data Impl
  = SelfProvided String -- vocab the resident interpreter speaks
  | WantsDefault String -- named default, e.g. "browser"
  deriving (Show)

data Hello = Hello
  { helloVocab :: String, -- protocol/vocabulary version spoken
    helloImpl :: Impl,
    helloCached :: [Hash] -- content-addressed modules on disk
  }
  deriving (Show)

data ServerMsg
  = Rejected String
  | Bootstrap Hash (Maybe Int) -- module hash, Just bytes | Nothing = cache hit
  | NoBootstrap
  | Mount Seq Static Dyn
  | SPatch Seq Patch
  | CatchUp Seq Patch -- one patch spanning a seq gap
  deriving (Show)

data ClientMsg
  = Event SessId String
  | ResyncReq SessId Seq -- "last seq I applied was k"
  | ResumeReq SessId Seq -- reconnect with retained session
  deriving (Show)

serverVocab :: String
serverVocab = "fprlive-core-v1"

-- content-addressed default view-function registry: name -> (hash, byte size)
registry :: M.Map String (Hash, Int)
registry =
  M.fromList
    [("browser", ("fprlive.browser-runtime.v3#a1b2c3", 48211))]

--------------------------------------------------------------------------------
-- 4. Server state
--------------------------------------------------------------------------------

retention :: Int -- ring depth for resync/resume
retention = 8

data Session = Session
  { sShadow :: Dyn, -- last state acked to this client
    sSeq :: Seq,
    sHistory :: [(Seq, Dyn)] -- recent states, newest first
  }

data Server = Server {srvModel :: Model, srvSessions :: M.Map SessId Session}

--------------------------------------------------------------------------------
-- 5. Client view functions: one stream, three interpretations
--------------------------------------------------------------------------------

renderBrowser :: Static -> Dyn -> String
renderBrowser s d = case (s, d) of
  (SText t, _) -> t
  (SHole, DScalar v) -> "<span>" ++ v ++ "</span>"
  (SStack ks, DStack ds) -> "<div class=stack>" ++ zr ks ds ++ "</div>"
  (SButton n ks, DStack ds) -> "<button data-ev=\"" ++ n ++ "\">" ++ zr ks ds ++ "</button>"
  (SButton n ks, DNone) ->
    "<button data-ev=\""
      ++ n
      ++ "\">"
      ++ concatMap (`renderBrowser` DNone) ks
      ++ "</button>"
  (SCond bs, DCond t bd) -> renderBrowser (bs !! t) bd
  (SForEach t, DRows rs) ->
    "<ul>"
      ++ concat
        [ "<li key=" ++ show k ++ ">" ++ renderBrowser t rd ++ "</li>"
          | (k, rd) <- rs
        ]
      ++ "</ul>"
  (_, DNone) -> case s of SText t -> t; _ -> ""
  _ -> error "browser render mismatch"
  where
    zr ks ds = concatMap (uncurry renderBrowser) (zip ks ds)

renderTUI :: Static -> Dyn -> [String]
renderTUI s d = case (s, d) of
  (SText t, _) -> [t]
  (SHole, DScalar v) -> [v]
  (SStack ks, DStack ds) -> concatMap (uncurry renderTUI) (zip ks ds)
  (SButton _ ks, DStack ds) -> ["[ " ++ unwords (concatMap (uncurry renderTUI) (zip ks ds)) ++ " ]"]
  (SButton _ ks, DNone) -> ["[ " ++ unwords (concatMap (`renderTUI` DNone) ks) ++ " ]"]
  (SCond bs, DCond t bd) -> renderTUI (bs !! t) bd
  (SForEach t, DRows rs) -> [" • " ++ unwords (renderTUI t rd) | (k, rd) <- rs, True]
  (_, DNone) -> case s of SText t -> [t]; _ -> []
  _ -> error "tui render mismatch"

renderEmbedded :: Static -> Dyn -> String -- 16x2 LCD-ish: scalars only
renderEmbedded s d = intercalate "|" (go s d)
  where
    go st dy = case (st, dy) of
      (SHole, DScalar v) -> [v]
      (SStack ks, DStack ds) -> concatMap (uncurry go) (zip ks ds)
      (SButton _ ks, DStack ds) -> concatMap (uncurry go) (zip ks ds)
      (SCond bs, DCond t bd) -> go (bs !! t) bd
      (SForEach t, DRows rs) -> [show (length rs) ++ " items"]
      _ -> []

--------------------------------------------------------------------------------
-- 6. Client state machine (shared by all kinds; render fn is the "view function")
--------------------------------------------------------------------------------

data Client = Client
  { cName :: String,
    cRender :: Static -> Dyn -> String,
    cStatic :: Maybe Static,
    cDyn :: Maybe Dyn,
    cExpect :: Seq -- next seq we expect
  }

clientRecv :: Client -> ServerMsg -> (Client, [String], Maybe ClientMsg)
clientRecv c = \case
  Rejected why -> (c, ["  " ++ cName c ++ " REJECTED: " ++ why], Nothing)
  Bootstrap h mb ->
    ( c,
      [ "  "
          ++ cName c
          ++ " bootstrap "
          ++ h
          ++ " "
          ++ maybe
            "(cache HIT, 0 bytes)"
            (\n -> "(cache miss, " ++ show n ++ " bytes)")
            mb
      ],
      Nothing
    )
  NoBootstrap -> (c, ["  " ++ cName c ++ " self-provided, no bootstrap"], Nothing)
  Mount sq st dy ->
    ( c {cStatic = Just st, cDyn = Just dy, cExpect = sq + 1},
      [ "  "
          ++ cName c
          ++ " mounted (seq "
          ++ show sq
          ++ ", static "
          ++ show (staticSize st)
          ++ " + dyn "
          ++ show (dynSize dy)
          ++ ")"
      ],
      Nothing
    )
  SPatch sq p
    | sq /= cExpect c ->
        ( c,
          [ "  "
              ++ cName c
              ++ " GAP: got seq "
              ++ show sq
              ++ ", expected "
              ++ show (cExpect c)
              ++ " -> ResyncReq"
          ],
          Just (ResyncReq 0 (cExpect c - 1)) -- sess id filled by caller
        )
    | otherwise ->
        let d' = applyPatch (fromMaybe (error "no dyn") (cDyn c)) p
         in ( c {cDyn = Just d', cExpect = sq + 1},
              [ "  "
                  ++ cName c
                  ++ " applied seq "
                  ++ show sq
                  ++ " ("
                  ++ show (patchSize p)
                  ++ ")"
              ],
              Nothing
            )
  CatchUp sq p ->
    let d' = applyPatch (fromMaybe (error "no dyn") (cDyn c)) p
     in ( c {cDyn = Just d', cExpect = sq + 1},
          [ "  "
              ++ cName c
              ++ " caught up to seq "
              ++ show sq
              ++ " with ONE patch ("
              ++ show (patchSize p)
              ++ ")"
          ],
          Nothing
        )

showClient :: Client -> [String]
showClient c = case (cStatic c, cDyn c) of
  (Just st, Just dy) -> ["  ┌ " ++ cName c] ++ map ("  │ " ++) body ++ ["  └"]
    where
      body = lines (cRender c st dy)
  _ -> ["  (" ++ cName c ++ " not mounted)"]

--------------------------------------------------------------------------------
-- 7. Server handlers
--------------------------------------------------------------------------------

handleHello :: IORef Server -> SessId -> Hello -> IO [ServerMsg]
handleHello ref sid (Hello vocab impl cached) = do
  srv <- readIORef ref
  case impl of
    SelfProvided v
      | v /= serverVocab ->
          pure
            [ Rejected
                ( "vocabulary mismatch: client "
                    ++ v
                    ++ " vs server "
                    ++ serverVocab
                )
            ]
    _
      | vocab /= serverVocab ->
          pure
            [ Rejected
                ( "vocabulary mismatch: client "
                    ++ vocab
                    ++ " vs server "
                    ++ serverVocab
                )
            ]
    _ -> do
      let boot = case impl of
            SelfProvided _ -> [NoBootstrap]
            WantsDefault nm -> case M.lookup nm registry of
              Nothing -> [Rejected ("no default impl for " ++ show nm)]
              Just (h, size) ->
                [ Bootstrap
                    h
                    ( if h `elem` cached
                        then Nothing
                        else Just size
                    )
                ]
      if any isReject boot
        then pure boot
        else do
          let dy = extract appView (srvModel srv)
              sess = Session dy 0 [(0, dy)]
          writeIORef ref srv {srvSessions = M.insert sid sess (srvSessions srv)}
          pure (boot ++ [Mount 0 (staticOf appView) dy])
  where
    isReject Rejected {} = True; isReject _ = False

-- an update happened: produce per-session patches
broadcast :: IORef Server -> IO (M.Map SessId ServerMsg)
broadcast ref = do
  srv <- readIORef ref
  let newDyn = extract appView (srvModel srv)
      step (out, ss) (sid, sess) =
        case diff (sShadow sess) newDyn of
          Nothing -> (out, (sid, sess) : ss)
          Just p ->
            let sq = sSeq sess + 1
                hist = take retention ((sq, newDyn) : sHistory sess)
                sess' = Session newDyn sq hist
             in (M.insert sid (SPatch sq p) out, (sid, sess') : ss)
      (msgs, ss') = foldl step (M.empty, []) (M.toList (srvSessions srv))
  writeIORef ref srv {srvSessions = M.fromList ss'}
  pure msgs

handleEvent :: IORef Server -> String -> IO (M.Map SessId ServerMsg)
handleEvent ref evName = case parseEvent evName of
  Nothing -> pure M.empty
  Just msg -> do
    modifyIORef ref (\s -> s {srvModel = update msg (srvModel s)})
    broadcast ref

-- resync/resume: answer with ONE catch-up patch from retained history,
-- or a full remount if the requested seq fell out of the retention window
handleCatchUp :: IORef Server -> SessId -> Seq -> IO ServerMsg
handleCatchUp ref sid lastGood = do
  srv <- readIORef ref
  let sess = srvSessions srv M.! sid
      newDyn = extract appView (srvModel srv)
      sq = sSeq sess -- client jumps straight to head
  case lookup lastGood (sHistory sess) of
    Just oldDyn -> pure $ case diff oldDyn newDyn of
      Just p -> CatchUp sq p
      Nothing -> CatchUp sq (PStack []) -- nothing actually changed
    Nothing -> pure (Mount sq (staticOf appView) newDyn) -- out of window

--------------------------------------------------------------------------------
-- 8. Scenario
--------------------------------------------------------------------------------

foldRecvIO :: Client -> [ServerMsg] -> IO Client
foldRecvIO c [] = pure c
foldRecvIO c (m : ms) = do
  let (c', ls, _) = clientRecv c m
  say ls
  foldRecvIO c' ms

say :: [String] -> IO ()
say = mapM_ putStrLn

main :: IO ()
main = do
  ref <-
    newIORef
      ( Server
          (Model 0 False [(1, "verify FPRLive"), (2, "ship it")])
          M.empty
      )

  putStrLn "== PHASE 1+2: NEGOTIATE / BOOTSTRAP =="

  -- a stale TUI build speaking an old vocabulary: clean rejection
  msgs0 <- handleHello ref 99 (Hello "fprlive-core-v0" (SelfProvided "fprlive-core-v0") [])
  let tuiOld = Client "tui-old  " (\_ _ -> "") Nothing Nothing 0
  say . concat $ [ls | m <- msgs0, let (_, ls, _) = clientRecv tuiOld m]

  -- browser A: wants server default, empty cache -> module bytes over the wire
  msgs1 <- handleHello ref 1 (Hello serverVocab (WantsDefault "browser") [])
  browserA1 <- foldRecvIO (Client "browserA " renderBrowser Nothing Nothing 0) msgs1

  -- browser B: same default, has the hash cached -> zero module bytes
  msgs2 <-
    handleHello
      ref
      2
      ( Hello
          serverVocab
          (WantsDefault "browser")
          ["fprlive.browser-runtime.v3#a1b2c3"]
      )
  browserB1 <- foldRecvIO (Client "browserB " renderBrowser Nothing Nothing 0) msgs2

  -- TUI: self-provided interpreter, correct vocabulary -> skip bootstrap
  msgs3 <- handleHello ref 3 (Hello serverVocab (SelfProvided serverVocab) [])
  tui1 <- foldRecvIO (Client "tui      " (\st dy -> intercalate "\n" (renderTUI st dy)) Nothing Nothing 0) msgs3

  -- embedded: self-provided, minimal LCD renderer
  msgs4 <- handleHello ref 4 (Hello serverVocab (SelfProvided serverVocab) [])
  emb1 <- foldRecvIO (Client "embedded " renderEmbedded Nothing Nothing 0) msgs4

  putStrLn "\n== ONE STREAM, THREE INTERPRETATIONS (initial mount) =="
  say (showClient browserA1)
  say (showClient tui1)
  say (showClient emb1)

  putStrLn "\n== PHASE 3: EVENT ROUND-TRIP (browserA clicks Inc twice, adds a todo) =="
  (ba2, bb2, tui2, emb2) <-
    steps
      ref
      [(1, "Inc"), (1, "Inc"), (1, "Add write docs")]
      (browserA1, browserB1, tui1, emb1)
  say (showClient ba2)
  say (showClient tui2)
  say (showClient emb2)

  putStrLn "\n== SEQ GAP -> RESYNC (a patch to tui is dropped by the network) =="
  m1 <- handleEvent ref "ToggleTheme" -- tui's copy of this patch is "lost"
  let (ba3, lg1, _) = clientRecv ba2 (m1 M.! 1)
      (bb3, _, _) = clientRecv bb2 (m1 M.! 2)
      (emb3, _, _) = clientRecv emb2 (m1 M.! 4)
  say lg1
  putStrLn "  tui      ...patch seq lost in transit..."
  m2 <- handleEvent ref "Inc" -- next patch arrives; tui sees a gap
  let (ba4, lg2, _) = clientRecv ba3 (m2 M.! 1)
      (bb4, _, _) = clientRecv bb3 (m2 M.! 2)
      (emb4, _, _) = clientRecv emb3 (m2 M.! 4)
      (tui3, lg3, rq) = clientRecv tui2 (m2 M.! 3)
  say lg2
  say lg3
  tui4 <- case rq of
    Just (ResyncReq _ lastGood) -> do
      cu <- handleCatchUp ref 3 lastGood
      let (t, lg, _) = clientRecv tui3 cu
      say lg
      pure t
    _ -> pure tui3
  say (showClient tui4)

  putStrLn "\n== RECONNECT-RESUME (browserB sleeps through two updates) =="
  putStrLn "  browserB disconnects (server retains session + Dyn ring)"
  m3 <- handleEvent ref "Add test on hardware"
  m4 <- handleEvent ref "Del 1"
  let (ba5, _, _) = clientRecv ba4 (m3 M.! 1)
      (ba6, _, _) = clientRecv ba5 (m4 M.! 1)
      (tui5, _, _) = clientRecv tui4 (m3 M.! 3)
      (tui6, _, _) = clientRecv tui5 (m4 M.! 3)
      _unused = (clientRecv emb4 (m3 M.! 4), tui6)
  putStrLn $
    "  browserB reconnects: ResumeReq(sess 2, last seq "
      ++ show (cExpect bb4 - 1)
      ++ ")"
  cu <- handleCatchUp ref 2 (cExpect bb4 - 1)
  let (bb5, lg4, _) = clientRecv bb4 cu
  say lg4
  full <- do
    srv <- readIORef ref
    pure
      ( staticSize (staticOf appView)
          + dynSize (extract appView (srvModel srv))
      )
  putStrLn $ "  (a full remount would have been " ++ show full ++ ")"
  say (showClient bb5)
  putStrLn "\n  final check: browserA and resumed browserB render identically:"
  putStrLn $
    "  "
      ++ show
        ( fmap (\_ -> ()) (cStatic ba6) == fmap (\_ -> ()) (cStatic bb5)
            && cDyn ba6 == cDyn bb5
        )
  where
    steps ::
      IORef Server ->
      [(SessId, String)] ->
      (Client, Client, Client, Client) ->
      IO (Client, Client, Client, Client)
    steps _ [] cs = pure cs
    steps ref' ((from, ev) : rest) (ba, bb, tu, em) = do
      putStrLn $ "  -> event " ++ show ev ++ " from session " ++ show from
      ms <- handleEvent ref' ev
      let r c sid = case M.lookup sid ms of
            Nothing -> (c, [], Nothing)
            Just m -> clientRecv c m
          (ba', l1, _) = r ba 1
          (bb', l2, _) = r bb 2
          (tu', l3, _) = r tu 3
          (em', l4, _) = r em 4
      say (l1 ++ l2 ++ l3 ++ l4)
      steps ref' rest (ba', bb', tu', em')
