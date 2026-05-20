module TerraII.AI where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing, isJust, mapMaybe)
import Data.List (sortBy, find)
import Data.Ord (comparing, Down(..))
import qualified Data.Set as Set
import System.Random (randomR, StdGen)

import TerraII.Types

runAI :: GameState -> GameState
runAI gs = foldl runFaction gs [SouthernEmpire, Insects, Thieves]

runFaction :: GameState -> FactionId -> GameState
runFaction gs fid =
  case Map.lookup fid (gsFacs gs) of
    Nothing -> gs
    Just f  -> if factionDefeated f then gs else doAI fid gs

doAI :: FactionId -> GameState -> GameState
doAI SouthernEmpire gs = runSouthern gs
doAI Insects        gs = runInsects gs
doAI Thieves        gs = runThieves gs
doAI _              gs = gs

-- Southern Empire: aggressive expansion, attacks enemies
runSouthern :: GameState -> GameState
runSouthern gs =
  let fid   = SouthernEmpire
      myUs  = myUnits fid gs
      gs1   = foldl (southernOrder fid) gs myUs
      gs2   = aiProduce fid LightInfantry gs1
  in gs2

southernOrder :: FactionId -> GameState -> Unit -> GameState
southernOrder fid gs u =
  let a       = getArea (unitLoc u) gs
      enemies = filter (isEnemyArea fid gs) (areaAdj a)
      free    = filter (isFreeArea gs)      (areaAdj a)
  in case unitType u of
    Laborer -> buildOutpost fid u gs
    Recon   -> case free of { (t:_) -> aiMove u t gs; [] -> gs }
    _       ->
      case enemies of
        (t:_) -> aiRaid fid u t gs
        []    -> case free of
          (t:_) -> aiClaim fid u t gs
          []    -> gs

-- Hiveform: biological spread, colonise empty land
runInsects :: GameState -> GameState
runInsects gs =
  let fid  = Insects
      myUs = myUnits fid gs
      gs1  = foldl (insectOrder fid) gs myUs
      gs2  = aiProduce fid Militia gs1
  in gs2

insectOrder :: FactionId -> GameState -> Unit -> GameState
insectOrder fid gs u =
  let a      = getArea (unitLoc u) gs
      free   = filter (isFreeArea gs)      (areaAdj a)
      enemies= filter (isEnemyArea fid gs) (areaAdj a)
  in case free of
    (t:_) -> aiClaim fid u t (addLog' (LogEntry (gsTurn gs) (Just fid)
                (factionColored fid "Hiveform" ++ " spreads to " ++ areaName (getArea t gs)) LInfo) gs)
    []    -> case enemies of
      (t:_) -> aiRaid fid u t gs
      []    -> gs

-- Thieves: opportunistic, prefer ruins/industrial, strike weak targets
runThieves :: GameState -> GameState
runThieves gs =
  let fid  = Thieves
      myUs = myUnits fid gs
      gs1  = foldl (thiefOrder fid) gs myUs
      gs2  = aiProduce fid Recon gs1
  in gs2

thiefOrder :: FactionId -> GameState -> Unit -> GameState
thiefOrder fid gs u =
  let a       = getArea (unitLoc u) gs
      adj     = areaAdj a
      free    = filter (isFreeArea gs) adj
      valuable= filter (\i -> areaTerrain (getArea i gs) `elem` [Ruins, Industrial]) free
      enemies = filter (isEnemyArea fid gs) adj
      weak    = filter (\i -> isWeakTarget fid i gs) enemies
  in case unitType u of
    Recon   -> case valuable ++ free of
                 (t:_) -> aiMove u t gs
                 []    -> gs
    Laborer -> buildOutpost fid u gs
    _       -> case weak of
                 (t:_) -> aiRaid fid u t (addLog' (LogEntry (gsTurn gs) (Just fid)
                            (factionColored fid "Thieves" ++ " strike " ++ areaName (getArea t gs) ++ " under cover of dark") LCombat) gs)
                 []    -> case free of
                   (t:_) -> aiClaim fid u t gs
                   []    -> gs

-- Shared AI helpers
myUnits :: FactionId -> GameState -> [Unit]
myUnits fid gs = filter (\u -> unitOwner u == fid) (Map.elems (gsUnits gs))

getArea :: AreaId -> GameState -> Area
getArea aid gs = fromMaybe (error $ "area " ++ show aid) (Map.lookup aid (gsAreas gs))

isEnemyArea :: FactionId -> GameState -> AreaId -> Bool
isEnemyArea fid gs aid = case areaOwner (getArea aid gs) of
  Just o  -> o /= fid
  Nothing -> False

isFreeArea :: GameState -> AreaId -> Bool
isFreeArea gs aid = isNothing (areaOwner (getArea aid gs))

isWeakTarget :: FactionId -> AreaId -> GameState -> Bool
isWeakTarget fid aid gs =
  let a = getArea aid gs
      eu = concatMap snd $ filter (\(f,_) -> f /= fid) $ Map.toList (areaUnits a)
  in length eu <= 1

aiMove :: Unit -> AreaId -> GameState -> GameState
aiMove u dest gs =
  let src = unitLoc u
      uid = unitId u
      rmSrc a = a { areaUnits = Map.adjust (filter (/= uid)) (unitOwner u) (areaUnits a) }
      addDst a = a { areaUnits = Map.insertWith (++) (unitOwner u) [uid] (areaUnits a)
                   , areaExplored = Set.insert (unitOwner u) (areaExplored a) }
  in gs { gsUnits = Map.adjust (\u2 -> u2 { unitLoc = dest }) uid (gsUnits gs)
        , gsAreas = Map.adjust rmSrc src $ Map.adjust addDst dest (gsAreas gs) }

aiClaim :: FactionId -> Unit -> AreaId -> GameState -> GameState
aiClaim fid u dest gs =
  let gs1 = aiMove u dest gs
  in gs1 { gsAreas = Map.adjust (\a -> a { areaOwner = Just fid }) dest (gsAreas gs1) }

aiRaid :: FactionId -> Unit -> AreaId -> GameState -> GameState
aiRaid attFid u targetId gs =
  let target   = getArea targetId gs
      defFid   = areaOwner target
      attPow   = usAtk (unitStats (unitType u)) + vetBonus (unitVet u)
      defUnits = maybe 0 (\df -> length $ fromMaybe [] $ Map.lookup df (areaUnits target)) defFid
      baseDef  = maybe 0 baseDef' (areaBase target)
      defPow   = baseDef + defUnits * 2
      (aRoll, rng1) = randomR (1::Int,6) (gsRng gs)
      (dRoll, rng2) = randomR (1::Int,6) rng1
      wins = attPow + aRoll > defPow + dRoll
      gs1  = gs { gsRng = rng2 }
      msg  = factionColored attFid (factionShort attFid) ++ " raids " ++ areaName target
      gs2  = addLog' (LogEntry (gsTurn gs) (Just attFid) msg LCombat) gs1
  in if wins
     then
       let win  = factionColored attFid "Victory! " ++ areaName target ++ " falls to " ++ factionShort attFid ++ "!"
           gs3  = addLog' (LogEntry (gsTurn gs) (Just attFid) win LCombat) gs2
           gs4  = gs3 { gsAreas = Map.adjust (\a -> a
                    { areaOwner = Just attFid
                    , areaUnits = Map.insertWith (++) attFid [unitId u]
                              $ maybe (areaUnits a) (\df -> Map.delete df (areaUnits a)) defFid
                    }) targetId (gsAreas gs3) }
           gs5  = gs4 { gsUnits = Map.adjust (\u2 -> u2 { unitLoc = targetId }) (unitId u) (gsUnits gs4) }
           gs6  = gs5 { gsAreas = Map.adjust (\a -> a { areaUnits = Map.adjust (filter (/= unitId u)) attFid (areaUnits a) }) (unitLoc u) (gsAreas gs5) }
       in gs6
     else
       let fail = factionShort attFid ++ " assault repelled at " ++ areaName target
       in addLog' (LogEntry (gsTurn gs) (Just attFid) fail LCombat) gs2

baseDef' :: Base -> Int
baseDef' = baseDef

buildOutpost :: FactionId -> Unit -> GameState -> GameState
buildOutpost fid u gs =
  let loc = unitLoc u
      a   = getArea loc gs
  in if areaOwner a == Just fid && isNothing (areaBase a)
     then gs { gsAreas = Map.adjust (\ar -> ar { areaBase = Just (Base fid 1 20 20 2) }) loc (gsAreas gs) }
     else gs

aiProduce :: FactionId -> UnitType -> GameState -> GameState
aiProduce fid ut gs =
  let fac  = fromMaybe (error "fac") $ Map.lookup fid (gsFacs gs)
      cost = Resources 0 (usBuildMat (unitStats ut)) (usBuildFuel (unitStats ut)) 0
      myBases = [areaId a | a <- Map.elems (gsAreas gs), areaOwner a == Just fid, isJust (areaBase a)]
  in if canAfford (factionResources fac) cost && not (null myBases)
     then
       let uid  = gsNextUID gs
           loc  = head myBases
           newU = Unit uid ut fid (unitTypeName ut ++ " #" ++ show uid)
                    (unitMaxHP ut) 0 Green loc False
           gs'  = gs { gsUnits      = Map.insert uid newU (gsUnits gs)
                     , gsNextUID    = uid + 1
                     , gsAreas      = Map.adjust (\a -> a { areaUnits = Map.insertWith (++) fid [uid] (areaUnits a) }) loc (gsAreas gs)
                     , gsFacs       = Map.adjust (\f -> f { factionResources = subRes (factionResources f) cost }) fid (gsFacs gs)
                     }
       in gs'
     else gs

-- Resource tick
tickResources :: GameState -> GameState
tickResources gs = foldl tickFac gs [minBound..maxBound]

tickFac :: GameState -> FactionId -> GameState
tickFac gs fid =
  case Map.lookup fid (gsFacs gs) of
    Nothing -> gs
    Just f  -> if factionDefeated f then gs else
      let myAreas  = [a | a <- Map.elems (gsAreas gs), areaOwner a == Just fid]
          income   = foldl addRes emptyRes (map areaYield myAreas)
          myUs     = filter (\u -> unitOwner u == fid) (Map.elems (gsUnits gs))
          upkeep   = foldl (\acc u -> addRes acc (unitUpkeep (unitType u))) emptyRes myUs
          envMult  = envFoodMult (envPhase (gsEnv gs))
          fIncome  = floor (fromIntegral (resFood income) * envMult :: Double)
          net      = Resources
            { resFood     = max 0 (resFood     (factionResources f) + fIncome  - resFood  upkeep)
            , resMat      = max 0 (resMat      (factionResources f) + resMat   income - resMat  upkeep)
            , resFuel     = max 0 (resFuel     (factionResources f) + resFuel  income - resFuel upkeep)
            , resResearch = resResearch (factionResources f) + resResearch income
            }
      in gs { gsFacs = Map.adjust (\f2 -> f2 { factionResources = net }) fid (gsFacs gs) }

unitUpkeep :: UnitType -> Resources
unitUpkeep ut = let s = unitStats ut in Resources (usFoodUp s) 0 (usFuelUp s) 0

envFoodMult :: EnvPhase -> Double
envFoodMult ENormal   = 1.0
envFoodMult EUVHigh   = 0.8
envFoodMult EFreeze   = 0.5
envFoodMult EAshStorm = 0.9

-- Environment tick
tickEnv :: GameState -> GameState
tickEnv gs =
  let e = gsEnv gs
      tl = envTurnLeft e - 1
  in if tl <= 0
     then
       let (dur, rng') = randomR (4::Int, 10) (gsRng gs)
           next2 = cycleNext (envNext e)
           msg   = envChangeMsg (envNext e)
           gs'   = gs { gsRng = rng'
                       , gsEnv = Env (envNext e) dur next2
                       , gsMsg = Just msg }
       in addLog' (LogEntry (gsTurn gs) Nothing msg LWarn) gs'
     else gs { gsEnv = e { envTurnLeft = tl } }

cycleNext :: EnvPhase -> EnvPhase
cycleNext ENormal   = EUVHigh
cycleNext EUVHigh   = ENormal
cycleNext EFreeze   = ENormal
cycleNext EAshStorm = ENormal

envChangeMsg :: EnvPhase -> String
envChangeMsg ENormal   = "Conditions stabilise. Normal operations resume."
envChangeMsg EUVHigh   = "\ESC[91mBINARY STARS ALIGN -- UV RADIATION SPIKES.\ESC[0m Fuel costs elevated. Food -20%. Stay indoors."
envChangeMsg EFreeze   = "\ESC[96mORBITAL WINTER BEGINS.\ESC[0m Temperature plummets. Food production halved. Restrict movement."
envChangeMsg EAshStorm = "\ESC[90mVOLCANIC ASH STORM.\ESC[0m Visibility near zero. All combat at -1. Ground operations hazardous."

-- Victory check
checkVictory :: GameState -> GameState
checkVictory gs =
  let defeated = [fid | fid <- [minBound..maxBound], not (hasBases fid gs)]
      gs1 = foldl markDefeated gs defeated
      active = [fid | fid <- [minBound..maxBound],
                      let f = Map.lookup fid (gsFacs gs1),
                      maybe False (not . factionDefeated) f]
  in if length active == 1
     then
       let w = head active
           msg = factionColored w (factionShort w) ++ " has conquered Terra II!"
       in gs1 { gsOver = Just (GWin w)
              , gsLog  = LogEntry (gsTurn gs1) (Just w) msg LDefeat : gsLog gs1 }
     else gs1

hasBases :: FactionId -> GameState -> Bool
hasBases fid gs =
  any (\a -> areaOwner a == Just fid && isJust (areaBase a)) (Map.elems (gsAreas gs))

markDefeated :: GameState -> FactionId -> GameState
markDefeated gs fid =
  case Map.lookup fid (gsFacs gs) of
    Nothing -> gs
    Just f  -> if factionDefeated f then gs else
      let msg = factionColored fid (factionShort fid) ++ " has been eliminated."
          gs' = addLog' (LogEntry (gsTurn gs) (Just fid) msg LDefeat) gs
      in gs' { gsFacs = Map.adjust (\f2 -> f2 { factionDefeated = True }) fid (gsFacs gs') }

addLog' :: LogEntry -> GameState -> GameState
addLog' e gs = gs { gsLog = e : gsLog gs }
