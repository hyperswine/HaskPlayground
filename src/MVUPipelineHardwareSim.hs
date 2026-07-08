{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- DriverSim.hs — end-to-end MVU driver simulation for the Terra II engine.
--
-- Models the actor topology as pure data (no IO, no threads — the "actors"
-- are mailbox + state, the "scheduler" is a pure fold over cycles):
--
--   Input script → [Dispatch] → [Update] → [View] → [Render] → (DMA)
--                      ↑ MIdle ______________|
--
-- Validates:
--   1. Dataflow: an input becomes pixels; input→pixel latency measured.
--   2. Lockout via message passing: View reports MIdle to Dispatch;
--      Dispatch rejects inputs while the board is animating.
--   3. Resumable cascades: an Update job bigger than one tick's budget
--      spans multiple ticks (fold-as-continuation), while View keeps
--      presenting the last *committed* model every tick (double-buffer).
--   4. Scheduling: weighted-random actor selection, with an optional
--      presentation ceiling — the last R cycles of every tick are
--      reserved for the View→Render chain. With the ceiling: zero
--      missed vsync deadlines. Without: measurable misses under load.
--   5. Conservation: every message sent is eventually consumed.
--
-- Time model: 1 tick = one display refresh = `cfgTickBudget` cycles.
-- Actor quanta are non-preemptive: an actor runs only if its quantum
-- fits in the remaining cycles of the tick (this is what makes missed
-- deadlines possible, and what the ceiling protects against).

module MVUPipelineHardwareSim where

import Data.List (find, foldl')
import qualified Data.Map.Strict as M

--------------------------------------------------------------------------------
-- Mini game core (stands in for TerraCore's Model/resolveEvents/Cues)
--------------------------------------------------------------------------------

newtype GModel = GModel {gValue :: Int} deriving (Eq, Show)

-- Poke delta n  ==  an action whose cascade has n effect units, each adding
-- delta to the model and emitting one cue. Unit cost is paid in Update quanta.
data GAction = Poke Int Int deriving (Show)

data UpdJob = UpdJob
  { ujPending :: Int, -- effect units left to fold
    ujDelta :: Int,
    ujScratch :: GModel, -- working copy; committed only when done
    ujCues :: [String], -- accumulated, shipped on commit
    ujStart :: Int -- tick the job started (for span stats)
  }
  deriving (Show)

--------------------------------------------------------------------------------
-- Actors, messages, mailboxes
--------------------------------------------------------------------------------

data ActorId = ADispatch | AUpdate | AView | ARender
  deriving (Eq, Ord, Show)

data Msg
  = MInput GAction -- external → Dispatch
  | MAction GAction -- Dispatch → Update
  | MCommit GModel [String] -- Update → View (completed cascade)
  | MTickView -- vsync → View (build a frame)
  | MFrame Frame -- View → Render
  | MIdle -- View → Dispatch (animation drained)
  deriving (Show)

data Frame = Frame {fValue :: Int, fCue :: String, fTick :: Int}
  deriving (Show)

data DispatchS = DispatchS {dBusy :: Bool, dRejected :: Int}

data UpdateS = UpdateS
  { uCommitted :: GModel,
    uJob :: Maybe UpdJob,
    uSpans :: [(Int, Int)] -- (start, end) ticks
  }

data ViewS = ViewS
  { vModel :: GModel,
    vCues :: [(String, Int)],
    vWasAnimating :: Bool
  }

data RenderS = RenderS {rPresented :: [(Int, Int, Frame)]} -- (tick, cycle, frame)

data Sim = Sim
  { sCfg :: Cfg,
    sTick :: Int,
    sBoxes :: M.Map ActorId [Msg],
    sDisp :: DispatchS,
    sUpd :: UpdateS,
    sView :: ViewS,
    sRend :: RenderS,
    sRng :: Int,
    sSent, sConsumed :: Int,
    sMisses :: Int,
    sPresentedThisTick :: Bool
  }

data Cfg = Cfg
  { cfgTickBudget :: Int, -- cycles per display refresh
    cfgRenderReserve :: Int, -- ceiling: cycles reserved for View+Render
    cfgUseCeiling :: Bool,
    cfgWeights :: [(ActorId, Int)],
    cfgSeed :: Int,
    cfgTicks :: Int,
    cfgScript :: [(Int, GAction)] -- (tick, input)
  }

baseCfg :: Cfg
baseCfg =
  Cfg
    { cfgTickBudget = 200,
      cfgRenderReserve = 90, -- covers View (30) + Render (50) + slack
      cfgUseCeiling = True,
      cfgWeights = [(ADispatch, 1), (AUpdate, 8), (AView, 2), (ARender, 2)],
      cfgSeed = 42,
      cfgTicks = 80,
      cfgScript =
        [ (2, Poke 5 2), -- small action: 2 effect units, +10 total
          (10, Poke 1 15), -- heavy cascade: 15 units, +15 — exceeds one tick
          (12, Poke 99 1), -- probe: arrives mid-animation, must be REJECTED
          (60, Poke 3 2) -- accepted after everything drains, +6
        ]
    }

--------------------------------------------------------------------------------
-- Quantum costs (cycles) — the timing model
--------------------------------------------------------------------------------

unitsPerQuantum :: Int
unitsPerQuantum = 2

quantumCost :: ActorId -> Sim -> Int
quantumCost a s = case a of
  ADispatch -> 5
  AUpdate -> 40 -- folds up to 2 effect units
  AView -> case peek AView s of
    Just MTickView -> 30 -- build + emit a frame
    Just (MCommit {}) -> 10 -- swap committed model, append cues
    _ -> 10
  ARender -> 50 -- DMA push

cueFramesEach :: Int
cueFramesEach = 2 -- ticks a cue stays on screen

--------------------------------------------------------------------------------
-- Mailbox plumbing
--------------------------------------------------------------------------------

send :: ActorId -> Msg -> Sim -> Sim
send a m s =
  s
    { sBoxes = M.adjust (++ [m]) a (sBoxes s),
      sSent = sSent s + 1
    }

peek :: ActorId -> Sim -> Maybe Msg
peek a s = case M.findWithDefault [] a (sBoxes s) of
  (m : _) -> Just m
  [] -> Nothing

pop :: ActorId -> Sim -> (Maybe Msg, Sim)
pop a s = case M.findWithDefault [] a (sBoxes s) of
  (m : ms) ->
    ( Just m,
      s
        { sBoxes = M.insert a ms (sBoxes s),
          sConsumed = sConsumed s + 1
        }
    )
  [] -> (Nothing, s)

isNothing' :: Maybe Msg -> Bool
isNothing' = \case Nothing -> True; _ -> False

hasWork :: ActorId -> Sim -> Bool
hasWork AUpdate s =
  not (isNothing' (peek AUpdate s))
    || maybe False (const True) (uJob (sUpd s))
hasWork a s = not (isNothing' (peek a s))

--------------------------------------------------------------------------------
-- Actor behaviours: one quantum each
--------------------------------------------------------------------------------

runQuantum :: ActorId -> Sim -> Sim
runQuantum ADispatch s = case pop ADispatch s of
  (Just (MInput a), s') ->
    let d = sDisp s'
     in if dBusy d
          then s' {sDisp = d {dRejected = dRejected d + 1}}
          else send AUpdate (MAction a) s' {sDisp = d {dBusy = True}}
  (Just MIdle, s') -> s' {sDisp = (sDisp s') {dBusy = False}}
  (_, s') -> s'
runQuantum AUpdate s = case uJob (sUpd s) of
  Just j -> foldUnits j s
  Nothing -> case pop AUpdate s of
    (Just (MAction (Poke delta n)), s') ->
      let j = UpdJob n delta (uCommitted (sUpd s')) [] (sTick s')
       in foldUnits j s'
    (_, s') -> s'
  where
    foldUnits j sim =
      let k = min unitsPerQuantum (ujPending j)
          scratch' = GModel (gValue (ujScratch j) + ujDelta j * k)
          cues' = ujCues j ++ ["fx" ++ show (ujPending j - i) | i <- [0 .. k - 1]]
          j' =
            j
              { ujPending = ujPending j - k,
                ujScratch = scratch',
                ujCues = cues'
              }
          u = sUpd sim
       in if ujPending j' == 0
            then
              send
                AView
                (MCommit (ujScratch j') (ujCues j'))
                sim
                  { sUpd =
                      u
                        { uCommitted = ujScratch j',
                          uJob = Nothing,
                          uSpans = uSpans u ++ [(ujStart j, sTick sim)]
                        }
                  }
            else sim {sUpd = u {uJob = Just j'}}
runQuantum AView s = case pop AView s of
  (Just MTickView, s') ->
    let v = sView s'
        cues1 = case vCues v of
          ((c, n) : rest)
            | n > 1 -> (c, n - 1) : rest
            | otherwise -> rest
          [] -> []
        nowIdle = null cues1
        drained = vWasAnimating v && nowIdle
        cueLabel = maybe "idle" fst (listToMaybe' cues1)
        frame = Frame (gValue (vModel v)) cueLabel (sTick s')
        s2 = s' {sView = v {vCues = cues1, vWasAnimating = not nowIdle}}
        s3 = send ARender (MFrame frame) s2
     in if drained then send ADispatch MIdle s3 else s3
  (Just (MCommit m cues), s') ->
    let v = sView s'
     in s'
          { sView =
              v
                { vModel = m,
                  vCues = vCues v ++ [(c, cueFramesEach) | c <- cues],
                  vWasAnimating = True
                }
          }
  (_, s') -> s'
  where
    listToMaybe' = \case [] -> Nothing; (x : _) -> Just x
runQuantum ARender s = case pop ARender s of
  (Just (MFrame f), s') ->
    let r = sRend s'
     in s'
          { sRend =
              r
                { rPresented =
                    rPresented r
                      ++ [(sTick s', 0, f)] -- cycle filled by caller
                },
            sPresentedThisTick = True
          }
  (_, s') -> s'

--------------------------------------------------------------------------------
-- Scheduler: weighted-random with optional presentation ceiling
--------------------------------------------------------------------------------

lcg :: Int -> Int
lcg x = (x * 1103515245 + 12345) `mod` 2147483648

weightedPick :: [(ActorId, Int)] -> Int -> (ActorId, Int)
weightedPick ws rng =
  let rng' = lcg rng
      total = sum (map snd ws)
      r = rng' `mod` total
      go acc ((a, w) : rest)
        | r < acc + w = a
        | otherwise = go (acc + w) rest
      go _ [] = fst (head ws)
   in (go 0 ws, rng')

tickLoop :: Int -> Sim -> Sim
tickLoop cyclesLeft s
  | cyclesLeft <= 0 = s
  | otherwise =
      let cfg = sCfg s
          inReserve = cfgUseCeiling cfg && cyclesLeft <= cfgRenderReserve cfg
          allowed =
            if inReserve
              then [AView, ARender]
              else [ADispatch, AUpdate, AView, ARender]
          cands =
            [ (a, w) | (a, w) <- cfgWeights cfg, a `elem` allowed, hasWork a s, quantumCost a s <= cyclesLeft
            ]
       in case cands of
            [] -> s -- nothing fits: idle out
            _ ->
              let (a, rng') = weightedPick cands (sRng s)
                  cost = quantumCost a s
                  s' = runQuantum a s {sRng = rng'}
               in tickLoop (cyclesLeft - cost) s'

runTick :: Int -> Sim -> Sim
runTick t s0 =
  let cfg = sCfg s0
      inputs = [a | (tt, a) <- cfgScript cfg, tt == t]
      s1 = foldl' (\s a -> send ADispatch (MInput a) s) s0 {sTick = t} inputs
      s2 = send AView MTickView s1 -- vsync fires
      s3 = tickLoop (cfgTickBudget cfg) s2 {sPresentedThisTick = False}
      missed = not (sPresentedThisTick s3)
   in s3 {sMisses = sMisses s3 + (if missed then 1 else 0)}

runSim :: Cfg -> Sim
runSim cfg = foldl' (flip runTick) initSim [1 .. cfgTicks cfg]
  where
    initSim =
      Sim
        { sCfg = cfg,
          sTick = 0,
          sBoxes = M.fromList [(a, []) | a <- [ADispatch, AUpdate, AView, ARender]],
          sDisp = DispatchS False 0,
          sUpd = UpdateS (GModel 0) Nothing [],
          sView = ViewS (GModel 0) [] False,
          sRend = RenderS [],
          sRng = cfgSeed cfg,
          sSent = 0,
          sConsumed = 0,
          sMisses = 0,
          sPresentedThisTick = False
        }

--------------------------------------------------------------------------------
-- Test suite
--------------------------------------------------------------------------------

assert :: String -> Bool -> IO ()
assert name ok = putStrLn ((if ok then "  PASS  " else "  FAIL  ") ++ name)

main :: IO ()
main = do
  let s = runSim baseCfg

  putStrLn "== 1. dataflow: input becomes pixels =="
  let frames = [f | (_, _, f) <- rPresented (sRend s)]
      firstAt v = fTick <$> find ((== v) . fValue) frames
  assert
    "small action (tick 2, +10) visible by tick 4"
    (maybe False (<= 4) (firstAt 10))
  assert
    "heavy cascade (+15 -> 25) eventually visible"
    (firstAt 25 /= Nothing)
  assert
    "final action (+6 -> 31) visible"
    (firstAt 31 /= Nothing)

  putStrLn "== 2. lockout via message passing =="
  assert
    "mid-animation input rejected exactly once"
    (dRejected (sDisp s) == 1)
  assert
    "rejected action never reached the model (99 absent)"
    (gValue (uCommitted (sUpd s)) == 31)

  putStrLn "== 3. resumable cascade (fold as continuation) =="
  let spans = uSpans (sUpd s)
      bigSpan = [e - b | (b, e) <- spans, e - b >= 1]
  assert
    "heavy job spanned multiple ticks"
    (any (>= 2) (map (\(b, e) -> e - b) spans))
  assert
    "committed model stable mid-cascade (no frame shows partial 11..24)"
    (null [f | f <- frames, fValue f > 10, fValue f < 25])

  putStrLn "== 4. scheduling: ceiling guarantees presentation =="
  assert "ZERO missed vsync deadlines with ceiling" (sMisses s == 0)
  let noCeil seed =
        sMisses
          ( runSim
              baseCfg
                { cfgUseCeiling = False,
                  cfgSeed = seed
                }
          )
      missTotal = sum (map noCeil [1 .. 10])
  assert "misses occur WITHOUT ceiling (10 seeds)" (missTotal > 0)
  putStrLn
    ( "          (misses without ceiling across 10 seeds: "
        ++ show missTotal
        ++ ")"
    )

  putStrLn "== 5. conservation =="
  let leftover = sum (map length (M.elems (sBoxes s)))
  assert
    "every message sent was consumed (mailboxes drained)"
    (sSent s == sConsumed s && leftover == 0)

  putStrLn "== summary =="
  putStrLn
    ( "  frames presented : "
        ++ show (length frames)
        ++ " / "
        ++ show (cfgTicks baseCfg)
        ++ " ticks"
    )
  putStrLn ("  job spans (start,end): " ++ show spans)
  putStrLn ("  rejected inputs  : " ++ show (dRejected (sDisp s)))
  putStrLn ("  final model      : " ++ show (uCommitted (sUpd s)))
  putStrLn ""
  putStrLn "  frame window around the heavy cascade (ticks 9..18):"
  mapM_
    (putStrLn . ("    " ++) . show)
    [f | f <- frames, fTick f >= 9, fTick f <= 18]
