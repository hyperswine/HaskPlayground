{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TerraII.GameLoop where

import Control.Monad (forM_)
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Ord (comparing)
import qualified Data.Set as Set
import System.Exit (exitSuccess)
import System.IO (BufferMode (..), hSetBuffering, hSetEcho, stdin)
import System.Random (randomR)
import TerraII.AI (addLog', checkVictory, runAI, tickEnv, tickResources)
import TerraII.Render
import TerraII.Types

run :: GameState -> IO ()
run gs = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  putStr "\ESC[?25l" -- hide cursor
  loop gs

loop :: GameState -> IO ()
loop gs = do
  case gsOver gs of
    Just r -> do
      draw gs
      gameOver r
      return ()
    Nothing -> do
      draw gs
      gs1 <- handleMsg gs
      c <- getChar
      gs2 <- input c gs1
      loop gs2

handleMsg :: GameState -> IO GameState
handleMsg gs = case gsMsg gs of
  Nothing -> return gs
  Just msg -> do
    printMsg msg
    _ <- getChar
    return gs {gsMsg = Nothing}

-- Input handling
input :: Char -> GameState -> IO GameState
input 'q' _ = putStr "\ESC[?25h" >> putStrLn "" >> exitSuccess
input 'Q' _ = putStr "\ESC[?25h" >> putStrLn "" >> exitSuccess
input '?' gs = do printHelp; _ <- getChar; return gs
input 't' gs = endTurn gs
input 'T' gs = endTurn gs
input 'n' gs = return $ nextUnit gs
input 'N' gs = return $ nextUnit gs
input 'a' gs = return gs {gsSel = SelNone}
input 'A' gs = return gs {gsSel = SelNone}
input 'r' gs = researchMenu gs
input 'R' gs = researchMenu gs
input 'b' gs = baseMenu gs
input 'B' gs = baseMenu gs
input '\ESC' gs = do
  c1 <- getChar
  if c1 == '[' then do c2 <- getChar; arrowKey c2 gs else return gs {gsSel = SelNone}
input c gs = case gsSel gs of
  SelUnit uid -> unitCmd c uid gs
  _ -> return gs

-- reserved for future cursor navigation
arrowKey :: Char -> GameState -> IO GameState
arrowKey _ = return

-- Unit command dispatch
unitCmd :: Char -> UnitId -> GameState -> IO GameState
unitCmd c uid gs =
  case Map.lookup uid (gsUnits gs) of
    Nothing -> return gs
    Just u ->
      let a = getArea (unitLoc u) gs
          adj = areaAdj a
          enemies = filter (`isEnemy` gs) adj
       in case c of
            'w' -> return $ ordered uid $ addMsg ("  " ++ unitName u ++ " waits.") gs
            'W' -> return $ ordered uid $ addMsg ("  " ++ unitName u ++ " waits.") gs
            'e' -> return $ doExplore uid gs
            'E' -> return $ doExplore uid gs
            's' -> return $ doSpy uid gs
            'S' -> return $ doSpy uid gs
            'b' -> return $ doBuild uid gs
            'B' -> return $ doBuild uid gs
            'x' -> return $ doExploit uid gs
            'X' -> return $ doExploit uid gs
            'c' -> return $ doClear uid gs
            'C' -> return $ doClear uid gs
            'k' -> return $ doSkirmish uid gs
            'K' -> return $ doSkirmish uid gs
            'f' -> return $ ordered uid $ addMsg ("  " ++ unitName u ++ " fortifies position.") gs
            'F' -> return $ ordered uid $ addMsg ("  " ++ unitName u ++ " fortifies position.") gs
            -- MOVE by number
            n | n `elem` ['1' .. '9'] -> let idx = fromEnum n - fromEnum '1' in if idx < length adj then return $ doMove uid (adj !! idx) gs else return gs
            -- Raid by R1, R2... handled via 'r' followed by digit in separate pass. Simplified: 'r' key raids first enemy
            _ -> return gs

-- Player actions
doExplore :: UnitId -> GameState -> GameState
doExplore uid gs =
  case Map.lookup uid (gsUnits gs) of
    Nothing -> gs
    Just u ->
      let loc = unitLoc u
          a = getArea loc gs
          toReveal = loc : areaAdj a
          gs1 = gs {gsAreas = foldr (Map.adjust (\ar -> ar {areaExplored = Set.insert NeoRepublic (areaExplored ar)})) (gsAreas gs) toReveal}
          msg = unitName u ++ " surveys " ++ areaName a ++ ", revealing " ++ show (length (areaAdj a)) ++ " adjacent areas."
       in ordered uid $ addLog' (LogEntry (gsTurn gs) (Just NeoRepublic) msg LDiscover) gs1

doSpy :: UnitId -> GameState -> GameState
doSpy uid gs =
  case Map.lookup uid (gsUnits gs) of
    Nothing -> gs
    Just u ->
      let a = getArea (unitLoc u) gs
          adj = areaAdj a
          gs1 = gs {gsAreas = foldr (Map.adjust (\ar -> ar {areaExplored = Set.insert NeoRepublic (areaExplored ar)})) (gsAreas gs) adj}
          msg = unitName u ++ " gathers intelligence on adjacent areas."
       in ordered uid $ addLog' (LogEntry (gsTurn gs) (Just NeoRepublic) msg LDiscover) gs1

doBuild :: UnitId -> GameState -> GameState
doBuild uid gs =
  case Map.lookup uid (gsUnits gs) of
    Nothing -> gs
    Just u ->
      let loc = unitLoc u
          a = getArea loc gs
          fac = getFaction NeoRepublic gs
          cost = Resources 0 20 0 0
       in if areaOwner a /= Just NeoRepublic
            then addMsg "Cannot build: this area is not under Neo Republic control." gs
            else
              if isJust (areaBase a)
                then addMsg "A base already exists here." gs
                else
                  if not (canAfford (factionResources fac) cost)
                    then addMsg "Need M:20 to build a base." gs
                    else
                      let newBase = Base NeoRepublic 1 30 30 3
                          gs1 = gs {gsAreas = Map.adjust (\ar -> ar {areaBase = Just newBase}) loc (gsAreas gs), gsFacs = Map.adjust (\f -> f {factionResources = subRes (factionResources f) cost}) NeoRepublic (gsFacs gs)}
                          msg = "Outpost established at " ++ areaName a ++ "."
                       in ordered uid $ addLog' (LogEntry (gsTurn gs) (Just NeoRepublic) msg LInfo) gs1

doExploit :: UnitId -> GameState -> GameState
doExploit uid gs =
  case Map.lookup uid (gsUnits gs) of
    Nothing -> gs
    Just u ->
      let loc = unitLoc u
          a = getArea loc gs
          yld = areaYield a
          gs1 = if isNothing (areaOwner a) then gs {gsAreas = Map.adjust (\ar -> ar {areaOwner = Just NeoRepublic}) loc (gsAreas gs)} else gs
          gs2 = gs1 {gsFacs = Map.adjust (\f -> f {factionResources = addRes (factionResources f) yld}) NeoRepublic (gsFacs gs1)}
          msg = "Resources extracted from " ++ areaName a ++ ". +" ++ show (resFood yld) ++ "F +" ++ show (resMat yld) ++ "M +" ++ show (resFuel yld) ++ "Fu"
       in ordered uid $ addLog' (LogEntry (gsTurn gs) (Just NeoRepublic) msg LInfo) gs2

doClear :: UnitId -> GameState -> GameState
doClear uid gs =
  case Map.lookup uid (gsUnits gs) of
    Nothing -> gs
    Just u ->
      let loc = unitLoc u
          a = getArea loc gs
          haz = areaHazard a
       in if haz == 0
            then addMsg "No hostiles to clear here." gs
            else
              let s = unitStats (unitType u)
                  power = usAtk s + vetBonus (unitVet u)
                  (roll, rng') = randomR (1 :: Int, 6) (gsRng gs)
                  success = power + roll > haz * 2
                  gs1 = gs {gsRng = rng'}
               in if success
                    then
                      let gs2 = gs1 {gsAreas = Map.adjust (\ar -> ar {areaHazard = max 0 (areaHazard ar - 1)}) loc (gsAreas gs1)}
                          gs3 = gainXP uid 5 gs2
                          msg = "Hostiles cleared from " ++ areaName a ++ ". Hazard level reduced."
                       in ordered uid $ addLog' (LogEntry (gsTurn gs) (Just NeoRepublic) msg LCombat) gs3
                    else
                      let gs2 = damage uid haz gs1
                          msg = areaName a ++ " hostiles repel the attack! Unit takes " ++ show haz ++ " damage."
                       in ordered uid $ addLog' (LogEntry (gsTurn gs) (Just NeoRepublic) msg LCombat) gs2

doSkirmish :: UnitId -> GameState -> GameState
doSkirmish uid gs =
  case Map.lookup uid (gsUnits gs) of
    Nothing -> gs
    Just u ->
      let a = getArea (unitLoc u) gs
          foeIds = concatMap snd $ filter (\(f, _) -> f /= NeoRepublic) $ Map.toList (areaUnits a)
       in case foeIds of
            [] -> addMsg "No enemy units here to skirmish." gs
            (foe : _) -> combat uid foe gs

doMove :: UnitId -> AreaId -> GameState -> GameState
doMove uid dest gs =
  case Map.lookup uid (gsUnits gs) of
    Nothing -> gs
    Just u ->
      let src = unitLoc u
          da = getArea dest gs
          cap = areaCapacity da
          cur = length $ fromMaybe [] $ Map.lookup NeoRepublic (areaUnits da)
       in if cur >= cap
            then addMsg ("Cannot move to " ++ areaName da ++ ": unit capacity reached.") gs
            else
              let rmSrc ar = ar {areaUnits = Map.adjust (filter (/= uid)) NeoRepublic (areaUnits ar)}
                  addDst ar = ar {areaUnits = Map.insertWith (++) NeoRepublic [uid] (areaUnits ar), areaExplored = Set.insert NeoRepublic (areaExplored ar), areaOwner = if isNothing (areaOwner ar) then Just NeoRepublic else areaOwner ar}
                  gs1 = gs {gsUnits = Map.adjust (\u2 -> u2 {unitLoc = dest}) uid (gsUnits gs), gsAreas = Map.adjust rmSrc src $ Map.adjust addDst dest (gsAreas gs)}
                  msg = unitName u ++ " moves to " ++ areaName da ++ "."
               in ordered uid $ addLog' (LogEntry (gsTurn gs) (Just NeoRepublic) msg LInfo) gs1

-- Combat between two units
combat :: UnitId -> UnitId -> GameState -> GameState
combat attId defId gs =
  case (Map.lookup attId (gsUnits gs), Map.lookup defId (gsUnits gs)) of
    (Just att, Just def) ->
      let as = unitStats (unitType att)
          ds = unitStats (unitType def)
          pow = usAtk as + vetBonus (unitVet att)
          dPow = usDef ds + vetBonus (unitVet def)
          (roll, rng') = randomR (1 :: Int, 6) (gsRng gs)
          dmg = max 1 (pow + roll - dPow)
          gs1 = gs {gsRng = rng'}
          gs2 = damage defId dmg gs1
          gs3 = gainXP attId 5 gs2
          msg = unitName att ++ " skirmishes " ++ unitName def ++ " (dmg:" ++ show dmg ++ ")"
       in ordered attId $ addLog' (LogEntry (gsTurn gs) (Just NeoRepublic) msg LCombat) gs3
    _ -> gs

-- Raid (move to attack adjacent enemy area)
doRaid :: UnitId -> AreaId -> GameState -> GameState
doRaid uid targetId gs =
  case Map.lookup uid (gsUnits gs) of
    Nothing -> gs
    Just u ->
      let target = getArea targetId gs
          defFid = areaOwner target
          as = unitStats (unitType u)
          attPow = usAtk as + vetBonus (unitVet u)
          defUs = maybe 0 (\df -> length $ fromMaybe [] $ Map.lookup df (areaUnits target)) defFid
          bDef = maybe 0 baseDef (areaBase target)
          defPow = bDef + defUs * 2
          (aRoll, rng1) = randomR (1 :: Int, 6) (gsRng gs)
          (dRoll, rng2) = randomR (1 :: Int, 6) rng1
          wins = attPow + aRoll > defPow + dRoll
          gs1 = gs {gsRng = rng2}
          msg = unitName u ++ " raids " ++ areaName target ++ "!"
          gs2 = addLog' (LogEntry (gsTurn gs) (Just NeoRepublic) msg LCombat) gs1
       in if wins
            then
              let gs3 = addLog' (LogEntry (gsTurn gs) (Just NeoRepublic) ("Victory! " ++ areaName target ++ " captured!") LCombat) gs2
                  -- remove unit from src
                  gs4 = gs3 {gsAreas = Map.adjust (\a -> a {areaUnits = Map.adjust (filter (/= uid)) NeoRepublic (areaUnits a)}) (unitLoc u) (gsAreas gs3)}
                  -- take area
                  gs5 = gs4 {gsAreas = Map.adjust (\a -> a {areaOwner = Just NeoRepublic, areaUnits = Map.insertWith (++) NeoRepublic [uid] $ maybe id Map.delete defFid (areaUnits a)}) targetId (gsAreas gs4)}
                  gs6 = gs5 {gsUnits = Map.adjust (\u2 -> u2 {unitLoc = targetId}) uid (gsUnits gs5)}
                  gs7 = gainXP uid 10 gs6
               in ordered uid gs7
            else
              let gs3 = addLog' (LogEntry (gsTurn gs) (Just NeoRepublic) ("Raid repelled at " ++ areaName target ++ ".") LCombat) gs2
                  gs4 = damage uid (defPow `div` 2 + 1) gs3
               in ordered uid gs4

-- End turn
endTurn :: GameState -> IO GameState
endTurn gs = do
  let gs1 = gs {gsUnits = Map.map (\u -> u {unitOrdered = False}) (gsUnits gs)}
  let gs2 = runAI gs1
  let gs3 = tickResources gs2
  let gs4 = tickEnv gs3
  let gs5 = checkVictory gs4
  let gs6 = gs5 {gsTurn = gsTurn gs5 + 1, gsSel = SelNone}
  return gs6

-- Research menu
researchMenu :: GameState -> IO GameState
researchMenu gs = do
  cls
  let fac = getFaction NeoRepublic gs
      known = factionTech fac
      rp = resResearch (factionResources fac)
      avail = filter (\t -> all (`Set.member` known) (techPrereqs t) && not (Set.member (techId t) known)) allTechs
  putStrLn $ bold (col 94 "RESEARCH") ++ "  Points available: " ++ col 36 (show rp)
  putStrLn ""
  forM_ (zip [1 ..] avail) $ \(i, t) -> putStrLn $ col 96 ("  [" ++ show i ++ "] ") ++ bold (techName t) ++ " (" ++ show (techCost t) ++ "rp)" ++ "\n      " ++ dim (techDesc t) ++ "\n      " ++ col 32 (techEffect t)
  putStrLn ""
  let knownNames = [techName t | t <- allTechs, Set.member (techId t) known]
  putStrLn $ "  Known: " ++ (if null knownNames then dim "none" else intercalate ", " knownNames)
  putStrLn ""
  putStrLn $ dim "  [1-9] research  [ESC/other] back"
  c <- getChar
  case c of
    '\ESC' -> return gs
    n | n `elem` ['1' .. '9'] ->
          let idx = fromEnum n - fromEnum '1'
           in if idx < length avail
                then
                  let t = avail !! idx
                   in if rp >= techCost t
                        then
                          let gs' = gs {gsFacs = Map.adjust (\f -> f {factionTech = Set.insert (techId t) (factionTech f), factionResources = (factionResources f) {resResearch = rp - techCost t}}) NeoRepublic (gsFacs gs)}
                              msg = "Research complete: " ++ bold (techName t) ++ ". " ++ techEffect t
                           in return $ addLog' (LogEntry (gsTurn gs) (Just NeoRepublic) msg LDiscover) gs'
                        else return $ addMsg ("Need " ++ show (techCost t) ++ "rp. Have " ++ show rp ++ ".") gs
                else return gs
    _ -> return gs

-- Base production menu
baseMenu :: GameState -> IO GameState
baseMenu gs = do
  cls
  let myBases = [(aid, a) | (aid, a) <- Map.toList (gsAreas gs), areaOwner a == Just NeoRepublic, isJust (areaBase a)]
      fac = getFaction NeoRepublic gs
      res = factionResources fac
  putStrLn $ bold (col 94 "BASE PRODUCTION")
  putStrLn $ "Resources: F:" ++ show (resFood res) ++ " M:" ++ show (resMat res) ++ " Fu:" ++ show (resFuel res)
  putStrLn ""
  putStrLn "  Train unit (costs immediate):"
  let types = [(Recon, 1), (Laborer, 2), (Militia, 3), (LightInfantry, 4), (MechInfantry, 5), (HeavyInfantry, 6)]
  forM_ types $ \(ut, i) ->
    let s = unitStats ut
     in putStrLn $
          "  "
            ++ col 96 ("[" ++ show i ++ "] ")
            ++ padR 16 (unitTypeName ut)
            ++ "  M:"
            ++ show (usBuildMat s)
            ++ " Fu:"
            ++ show (usBuildFuel s)
            ++ " F:"
            ++ show (usBuildFood s)
            ++ "  Atk:"
            ++ show (usAtk s)
            ++ " Def:"
            ++ show (usDef s)
  putStrLn ""
  case myBases of
    [] -> putStrLn $ col 91 "  No bases to produce from!"
    ((bid, _) : _) -> putStrLn $ "  Producing from: " ++ fromMaybe "?" (areaName <$> Map.lookup bid (gsAreas gs))
  putStrLn ""
  putStrLn $ dim "  [1-6] train  [ESC] back"
  c <- getChar
  case c of
    '\ESC' -> return gs
    n
      | n `elem` ['1' .. '6'] ->
          let ut = [Recon, Laborer, Militia, LightInfantry, MechInfantry, HeavyInfantry] !! (fromEnum n - fromEnum '1')
              s = unitStats ut
              cost = Resources (usBuildFood s) (usBuildMat s) (usBuildFuel s) 0
              fac = getFaction NeoRepublic gs
           in case myBases of
                [] -> return $ addMsg "No bases available." gs
                ((bid, _) : _) ->
                  if canAfford (factionResources fac) cost
                    then
                      let uid = gsNextUID gs
                          newU = Unit uid ut NeoRepublic (unitTypeName ut ++ " #" ++ show uid) (unitMaxHP ut) 0 Green bid False
                          cap = areaCapacity (getArea bid gs)
                          cur = length $ fromMaybe [] $ Map.lookup NeoRepublic $ areaUnits (getArea bid gs)
                       in if cur >= cap
                            then return $ addMsg "Base area is at unit capacity!" gs
                            else
                              let gs' = gs {gsUnits = Map.insert uid newU (gsUnits gs), gsNextUID = uid + 1, gsAreas = Map.adjust (\a -> a {areaUnits = Map.insertWith (++) NeoRepublic [uid] (areaUnits a)}) bid (gsAreas gs), gsFacs = Map.adjust (\f -> f {factionResources = subRes (factionResources f) cost}) NeoRepublic (gsFacs gs)}
                                  msg = unitTypeName ut ++ " #" ++ show uid ++ " ready at " ++ areaName (getArea bid gs) ++ "."
                               in return $ addLog' (LogEntry (gsTurn gs) (Just NeoRepublic) msg LInfo) gs'
                    else return $ addMsg ("Not enough resources. Need F:" ++ show (usBuildFood s) ++ " M:" ++ show (usBuildMat s) ++ " Fu:" ++ show (usBuildFuel s)) gs
    _ -> return gs

-- Next unit needing orders
nextUnit :: GameState -> GameState
nextUnit gs =
  let unordered =
        sortBy (comparing unitId) $
          filter (\u -> unitOwner u == NeoRepublic && not (unitOrdered u)) $
            Map.elems (gsUnits gs)
   in case unordered of
        [] ->
          let all' =
                sortBy (comparing unitId) $
                  filter (\u -> unitOwner u == NeoRepublic) $
                    Map.elems (gsUnits gs)
           in case all' of
                (u : _) -> gs {gsSel = SelUnit (unitId u), gsMsg = Just "All units have orders. Press [T] to end turn."}
                [] -> gs {gsSel = SelNone, gsMsg = Just "No units. Build some from [B] Base Production."}
        (u : _) -> gs {gsSel = SelUnit (unitId u)}

-- Game over
gameOver :: GameResult -> IO ()
gameOver r = do
  cls
  case r of
    GWin NeoRepublic ->
      mapM_
        putStrLn
        [ col 94 (bold "+======================================+"),
          col 94 (bold "|    VICTORY FOR THE NEO REPUBLIC      |"),
          col 94 (bold "+======================================+"),
          "",
          "Commander Jake surveys the conquered continent.",
          col 96 "\"The republic lives again. Terra II is ours.\"",
          col 96 "\"From these ashes, we build something worth remembering.\"",
          "",
          dim "Press any key to exit."
        ]
    GWin fid ->
      mapM_
        putStrLn
        [ col 91 (bold "+======================================+"),
          col 91 (bold "|            DEFEAT                    |"),
          col 91 (bold "+======================================+"),
          "",
          factionColored fid (factionShort fid) ++ " has conquered Terra II.",
          dim "The Neo Republic is no more. Another chapter of misery closes.",
          "",
          dim "Press any key to exit."
        ]
    GLose msg ->
      mapM_ putStrLn [col 91 (bold "DEFEAT"), msg, dim "Press any key."]
  _ <- getChar
  putStr "\ESC[?25h"

-- Helpers
getArea :: AreaId -> GameState -> Area
getArea aid gs = fromMaybe (error $ "area " ++ show aid) (Map.lookup aid (gsAreas gs))

getFaction :: FactionId -> GameState -> Faction
getFaction fid gs = fromMaybe (error "fac") (Map.lookup fid (gsFacs gs))

isEnemy :: AreaId -> GameState -> Bool
isEnemy aid gs = case areaOwner =<< Map.lookup aid (gsAreas gs) of
  Just f -> f /= NeoRepublic
  Nothing -> False

ordered :: UnitId -> GameState -> GameState
ordered uid gs = gs {gsUnits = Map.adjust (\u -> u {unitOrdered = True}) uid (gsUnits gs), gsSel = SelNone}

addMsg :: String -> GameState -> GameState
addMsg msg gs = gs {gsMsg = Just msg}

damage :: UnitId -> Int -> GameState -> GameState
damage uid dmg gs = case Map.lookup uid (gsUnits gs) of
  Nothing -> gs
  Just u -> let hp' = unitHP u - dmg in if hp' <= 0 then killUnit uid gs else gs {gsUnits = Map.adjust (\u2 -> u2 {unitHP = hp'}) uid (gsUnits gs)}

killUnit :: UnitId -> GameState -> GameState
killUnit uid gs = case Map.lookup uid (gsUnits gs) of
  Nothing -> gs
  Just u ->
    let msg = unitName u ++ " destroyed!"
        gs' = addLog' (LogEntry (gsTurn gs) (Just (unitOwner u)) msg LCombat) gs
     in gs' {gsUnits = Map.delete uid (gsUnits gs'), gsAreas = Map.adjust (\a -> a {areaUnits = Map.adjust (filter (/= uid)) (unitOwner u) (areaUnits a)}) (unitLoc u) (gsAreas gs')}

gainXP :: UnitId -> Int -> GameState -> GameState
gainXP uid xp gs = case Map.lookup uid (gsUnits gs) of
  Nothing -> gs
  Just u ->
    let xp' = unitXP u + xp
        vet' = newVet xp'
        u' = u {unitXP = xp', unitVet = vet'}
     in gs {gsUnits = Map.insert uid u' (gsUnits gs)}

newVet :: Int -> Veterancy
newVet xp
  | xp >= xpThreshold Elite = Elite
  | xp >= xpThreshold Veteran = Veteran
  | xp >= xpThreshold Regular = Regular
  | otherwise = Green

intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate s (x : xs) = x ++ s ++ intercalate s xs
