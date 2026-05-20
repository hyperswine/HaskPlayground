{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TerraII.Render where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import System.IO (hFlush, stdout)
import TerraII.Types

cls :: IO ()
cls = putStr "\ESC[2J\ESC[H" >> hFlush stdout

-- Map grid: each cell is [row][col], cell width=10, height=3
-- Row x Col layout for 20 areas:
--   Col:  0          1          2          3
-- Row 0: [0]BaseCmp [1]MtPears [2]Ashfld  [3]NWastes
-- Row 1: [4]RvPears [5]PrsnDlt [6]CrgdPss [7]IrnrkRd
-- Row 2: [8]NeoSea  [9]CntrPln [10]Rustfd [11]EmpStg
-- Row 3: [12]KlpRns [13]MdlFst [14]TheRot [15]HivPrm
-- Row 4:             [16]Dstbwl [17]ThfWrn [18]SltSwp
-- Row 5:                                   [19]EFrng

gridLayout :: [[Maybe AreaId]]
gridLayout =
  [ [Just 0, Just 1, Just 2, Just 3],
    [Just 4, Just 5, Just 6, Just 7],
    [Just 8, Just 9, Just 10, Just 11],
    [Just 12, Just 13, Just 14, Just 15],
    [Nothing, Just 16, Just 17, Just 18],
    [Nothing, Nothing, Just 19, Nothing]
  ]

draw :: GameState -> IO ()
draw gs = do
  cls
  let ls = buildScreen gs
  mapM_ putStrLn ls
  hFlush stdout

buildScreen :: GameState -> [String]
buildScreen gs =
  header gs
    ++ [line '=' 100]
    ++ body gs
    ++ [line '-' 100]
    ++ logPanel gs
    ++ [line '-' 100]
    ++ controls gs

line :: Char -> Int -> String
line c n = replicate n c

-- Header: title + resources + env
header :: GameState -> [String]
header gs =
  let fac = getFac NeoRepublic gs
      res = factionResources fac
      e = gsEnv gs
      eStr =
        envPhaseColor (envPhase e)
          ++ bold (envPhaseName (envPhase e))
          ++ reset
          ++ dim (" [" ++ show (envTurnLeft e) ++ "t]")
      rStr =
        col 32 ("F:" ++ show (resFood res))
          ++ "  "
          ++ col 33 ("M:" ++ show (resMat res))
          ++ "  "
          ++ col 91 ("Fu:" ++ show (resFuel res))
          ++ "  "
          ++ col 36 ("R:" ++ show (resResearch res))
   in [ bold (col 94 " TERRA II")
          ++ "  T:"
          ++ bold (show (gsTurn gs))
          ++ "  |  "
          ++ eStr
          ++ "  |  "
          ++ rStr,
        " Playing: "
          ++ factionColored NeoRepublic (factionName fac)
          ++ "  |  Leader: "
          ++ factionLeader fac
          ++ "  |  Morale: "
          ++ moraleStr (factionMorale fac)
      ]

moraleStr :: Int -> String
moraleStr m =
  let c' = if m > 60 then 92 else if m > 30 then 93 else 91
      filled = m `div` 10
   in col c' (replicate filled '=' ++ replicate (10 - filled) '-') ++ " " ++ show m ++ "%"

-- Body: map left, info+actions right
body :: GameState -> [String]
body gs =
  let mapL = renderMap gs
      rightL = renderRight gs
      w = 52
      maxL = max (length mapL) (length rightL)
      padTo n xs = xs ++ replicate (n - length xs) ""
      combined = zipWith (\l r -> padR w l ++ " | " ++ r) (padTo maxL mapL) (padTo maxL rightL)
   in combined

-- Map rendering
renderMap :: GameState -> [String]
renderMap gs = bold " MAP" : concatMap (renderMapRow gs) gridLayout ++ [dim " ~coast  *forest  ^mtn  Hruins  Ovolcanic  ~ river  ~desert  #industrial"]

renderMapRow :: GameState -> [Maybe AreaId] -> [String]
renderMapRow gs row =
  let cells = map (renderCell gs) row
      rows3 = map (\i -> concatMap (!! i) cells) [0, 1, 2]
   in rows3

renderCell :: GameState -> Maybe AreaId -> [String]
renderCell _ Nothing = [replicate 13 ' ', replicate 13 ' ', replicate 13 ' ']
renderCell gs (Just aid) =
  let pf = gsPlayer gs
      ma = Map.lookup aid (gsAreas gs)
      sel = gsSel gs == SelArea aid || gsSel gs == SelUnit aid
      selL = if sel then bold "\ESC[97m[" else " "
      selR = if sel then bold "\ESC[97m]" else " "
   in case ma of
        Nothing -> [replicate 13 ' ', replicate 13 ' ', replicate 13 ' ']
        Just a ->
          let known = Set.member pf (areaExplored a)
              ownCol = maybe (dim "?") (\f -> factionColored f [factionSym' f]) (areaOwner a)
              baseMk = if isJust (areaBase a) then maybe "" (`factionColored` "=") (areaOwner a) else " "
              tChar = terrainChar (areaTerrain a)
              unitMk = renderUnitMark a pf
              nm6 = if known then padR 6 (areaName a) else "  ???  "
              yld = areaYield a
              yStr = dim (col 32 (show (resFood yld)) ++ col 33 (show (resMat yld)) ++ col 91 (show (resFuel yld)))
              hazC = if areaHazard a == 0 then "" else col 91 ("!" ++ show (areaHazard a))
           in if not known
                then [selL ++ dim "   ???   " ++ selR, selL ++ "         " ++ selR, "             "]
                else [selL ++ nm6 ++ ownCol ++ baseMk ++ selR, selL ++ tChar ++ " " ++ yStr ++ " " ++ hazC ++ selR, "  " ++ unitMk ++ "          "]

renderUnitMark :: Area -> FactionId -> String
renderUnitMark a pf =
  let mineIds = fromMaybe [] $ Map.lookup pf (areaUnits a)
      others = concatMap snd $ filter (\(f, _) -> f /= pf) $ Map.toList (areaUnits a)
      mkMine = if null mineIds then "" else factionColored pf "o"
      mkFoe = case others of
        [] -> ""
        _ -> let fid = head $ Map.keys $ Map.filter (not . null) $ Map.filterWithKey (\k _ -> k /= pf) (areaUnits a) in factionColored fid "!"
   in mkMine ++ mkFoe

-- Right panel: info + actions
renderRight :: GameState -> [String]
renderRight gs = case gsSel gs of
  SelNone -> overviewPanel gs ++ [""] ++ actionsPanel gs
  SelArea aid -> areaPanel aid gs ++ [""] ++ actionsPanel gs
  SelUnit uid -> unitPanel uid gs ++ [""] ++ unitActionsPanel uid gs

overviewPanel :: GameState -> [String]
overviewPanel gs =
  let myUs = [u | u <- Map.elems (gsUnits gs), unitOwner u == NeoRepublic]
      myAs = [a | a <- Map.elems (gsAreas gs), areaOwner a == Just NeoRepublic]
      unord = filter (not . unitOrdered) myUs
   in [ bold " OVERVIEW",
        " Areas : " ++ bold (show (length myAs)),
        " Units : "
          ++ bold (show (length myUs))
          ++ "  Unordered: "
          ++ (if null unord then col 32 "all done" else col 93 (show (length unord) ++ " waiting")),
        "",
        " RIVALS:"
      ]
        ++ map (rivalLine gs) [SouthernEmpire, Insects, Thieves]

rivalLine :: GameState -> FactionId -> String
rivalLine gs fid =
  case Map.lookup fid (gsFacs gs) of
    Nothing -> ""
    Just f ->
      if factionDefeated f
        then " " ++ dim (factionShort fid ++ " [ELIMINATED]")
        else
          let as = length [a | a <- Map.elems (gsAreas gs), areaOwner a == Just fid]
              us = length [u | u <- Map.elems (gsUnits gs), unitOwner u == fid]
           in " " ++ factionColored fid (padR 8 (factionShort fid)) ++ " areas:" ++ show as ++ " units:" ++ show us

areaPanel :: AreaId -> GameState -> [String]
areaPanel aid gs =
  case Map.lookup aid (gsAreas gs) of
    Nothing -> [" Unknown area."]
    Just a ->
      let known = Set.member (gsPlayer gs) (areaExplored a)
       in if not known
            then [" Area not explored. Send Recon."]
            else
              let own = maybe (dim "Unclaimed") (\f -> factionColored f (factionShort f)) (areaOwner a)
                  y = areaYield a
               in [ bold (" " ++ areaName a),
                    " Terrain : " ++ show (areaTerrain a),
                    " Owner   : " ++ own,
                    " Capacity: " ++ show (areaCapacity a) ++ " units/faction",
                    " Hazard  : " ++ hazStr (areaHazard a),
                    " Yield   : F:" ++ show (resFood y) ++ " M:" ++ show (resMat y) ++ " Fu:" ++ show (resFuel y),
                    " Features:"
                  ]
                    ++ map (\f -> "   * " ++ f) (areaFeatures a)
                    ++ [" Units:"]
                    ++ areaUnitLines a gs

hazStr :: Int -> String
hazStr 0 = col 32 "Safe"
hazStr 1 = col 32 "Low"
hazStr 2 = col 33 "Moderate"
hazStr 3 = col 33 "High"
hazStr 4 = col 91 "Dangerous"
hazStr _ = col 91 "Extreme"

areaUnitLines :: Area -> GameState -> [String]
areaUnitLines a gs =
  let uids = concatMap snd (Map.toList (areaUnits a))
   in if null uids
        then [dim "   No units."]
        else
          map
            ( \uid -> case Map.lookup uid (gsUnits gs) of
                Nothing -> ""
                Just u -> "   " ++ factionColored (unitOwner u) "[" ++ factionShort (unitOwner u) ++ "] " ++ unitName u ++ " (" ++ unitTypeShort (unitType u) ++ ")" ++ " HP:" ++ show (unitHP u)
            )
            uids

unitPanel :: UnitId -> GameState -> [String]
unitPanel uid gs =
  case Map.lookup uid (gsUnits gs) of
    Nothing -> [" Unit not found."]
    Just u ->
      let s = unitStats (unitType u)
          vb = vetBonus (unitVet u)
          loc = fromMaybe "?" (areaName <$> Map.lookup (unitLoc u) (gsAreas gs))
       in [ bold (" " ++ unitName u),
            " Type    : " ++ factionColored NeoRepublic (unitTypeName (unitType u)),
            " Location: " ++ loc,
            " HP      : " ++ hpBar (unitHP u) (unitMaxHP (unitType u)),
            " XP      : " ++ show (unitXP u) ++ " / " ++ nextXPStr (unitVet u),
            " Rank    : " ++ show (unitVet u),
            " Atk/Def : " ++ show (usAtk s + vb) ++ " / " ++ show (usDef s + vb),
            " Upkeep  : F:" ++ show (usFoodUp s) ++ " Fu:" ++ show (usFuelUp s),
            "",
            dim (" " ++ unitTypeDesc (unitType u)),
            "",
            if unitOrdered u then col 32 " OK Ordered" else col 93 " !! Needs orders"
          ]

hpBar :: Int -> Int -> String
hpBar hp mx =
  let pct = (hp * 10) `div` (max 1 mx)
      c' = if pct > 6 then 92 else if pct > 3 then 93 else 91
   in col c' (replicate pct '=' ++ replicate (10 - pct) '-') ++ " " ++ show hp ++ "/" ++ show mx

nextXPStr :: Veterancy -> String
nextXPStr Elite = "MAX"
nextXPStr v = show (xpThreshold (succ v))

actionsPanel :: GameState -> [String]
actionsPanel _ =
  [ bold " ACTIONS",
    " " ++ col 96 "[N]" ++ " Next unit needing orders",
    " " ++ col 96 "[A]" ++ " Overview / deselect",
    " " ++ col 96 "[R]" ++ " Research menu",
    " " ++ col 96 "[B]" ++ " Base production menu",
    " " ++ col 96 "[T]" ++ " End turn"
  ]

unitActionsPanel :: UnitId -> GameState -> [String]
unitActionsPanel uid gs =
  case Map.lookup uid (gsUnits gs) of
    Nothing -> []
    Just u ->
      let a = fromMaybe (error "area") $ Map.lookup (unitLoc u) (gsAreas gs)
          adj = areaAdj a
          numbered =
            zipWith
              ( \i aid ->
                  let dest = fromMaybe "?" (areaName <$> Map.lookup aid (gsAreas gs))
                      mark = if isEnemyAreaG aid gs then col 91 " [enemy]" else ""
                   in " " ++ col 96 ("[" ++ show i ++ "]") ++ " Move->" ++ dest ++ mark
              )
              [1 ..]
              adj
          typeActs = unitTypeActions u a gs
       in [bold (" ORDERS: " ++ unitName u)] ++ typeActs ++ [""] ++ numbered ++ [" " ++ col 96 "[W]" ++ " Wait"]

unitTypeActions :: Unit -> Area -> GameState -> [String]
unitTypeActions u a gs =
  let enemies = filter (isEnemyAreaG' gs) (areaAdj a)
   in case unitType u of
        Recon ->
          [ " " ++ col 92 "[E]" ++ " Explore & reveal adjacent areas",
            " " ++ col 92 "[S]" ++ " Spy (reveal enemy units nearby)"
          ]
        Laborer ->
          [ " " ++ col 93 "[B]" ++ " Build base here",
            " " ++ col 93 "[X]" ++ " Exploit resources",
            " " ++ col 93 "[C]" ++ " Clear hostiles (haz:" ++ show (areaHazard a) ++ ")"
          ]
        _ ->
          [ " " ++ col 91 "[K]" ++ " Skirmish enemies in this area"
          ]
            ++ [ " " ++ col 91 ("[R" ++ show i ++ "]") ++ " Raid " ++ fromMaybe "?" (areaName <$> Map.lookup aid (gsAreas gs))
                 | (i, aid) <- zip [1 ..] enemies
               ]
            ++ [" " ++ col 93 "[F]" ++ " Fortify position (+def)"]

isEnemyAreaG :: AreaId -> GameState -> Bool
isEnemyAreaG aid gs = case areaOwner =<< Map.lookup aid (gsAreas gs) of
  Just fid -> fid /= NeoRepublic
  Nothing -> False

isEnemyAreaG' :: GameState -> AreaId -> Bool
isEnemyAreaG' gs aid = isEnemyAreaG aid gs

-- Log panel
logPanel :: GameState -> [String]
logPanel gs =
  let es = take 5 (gsLog gs)
   in bold " SITUATION REPORT" : map renderEntry es

renderEntry :: LogEntry -> String
renderEntry e =
  let sev = case leSev e of
        LInfo -> col 36 "[INFO]  "
        LWarn -> col 93 "[WARN]  "
        LCombat -> col 91 "[COMBAT]"
        LDiscover -> col 92 "[FOUND] "
        LDefeat -> col 91 "[DEFEAT]"
      facStr = maybe "" (\f -> factionColored f ("[" ++ factionShort f ++ "] ")) (leFac e)
   in "  " ++ sev ++ " T" ++ show (leTurn e) ++ " " ++ facStr ++ leMsg e

-- Controls
controls :: GameState -> [String]
controls gs =
  let phase = case gsPhase gs of
        PlayerOrders -> col 94 "ORDERS"
        Resolution -> col 91 "RESOLVING"
        SitRep -> col 92 "SITUATION"
   in [" Phase:" ++ phase ++ "  |  " ++ col 96 "[T]" ++ "EndTurn  " ++ col 96 "[Q]" ++ "Quit  " ++ col 96 "[?]" ++ "Help  " ++ col 96 "[N]" ++ "NextUnit  " ++ col 96 "[A]" ++ "Overview"]

-- Message overlay
printMsg :: String -> IO ()
printMsg msg = do
  let lns = lines msg
      w = maximum (map vlen lns) + 4
      box =
        ["+" ++ replicate (w - 2) '-' ++ "+"]
          ++ map (\l -> "| " ++ padR (w - 4) l ++ " |") lns
          ++ ["|" ++ replicate (w - 2) ' ' ++ "|"]
          ++ ["| " ++ padR (w - 4) (dim "Press any key...") ++ " |"]
          ++ ["+" ++ replicate (w - 2) '-' ++ "+"]
  mapM_ putStrLn ("" : box)
  hFlush stdout

-- Help screen
printHelp :: IO ()
printHelp = do
  cls
  mapM_
    putStrLn
    [ bold (col 94 "TERRA II -- HELP"),
      "",
      bold "UNIT TYPES",
      "  Recon          -- Explore, spy. Fast. Reveals fog of war.",
      "  Laborer        -- Build bases, exploit resources, clear hostiles.",
      "  Militia        -- Basic fighters. Good for clearing neutral threats.",
      "  Light Infantry -- Coilguns, RPGs, LMGs. Capable PvP.",
      "  Mech Infantry  -- Vehicles, anti-armour, heavy assault.",
      "  Heavy Infantry -- Ray weapons, railguns. Siege.",
      "",
      bold "TURN STRUCTURE",
      "  Assign orders to all units -> Press [T] to end turn.",
      "  AI factions move simultaneously when you end.",
      "",
      bold "AREAS & MOVEMENT",
      "  Each area holds 1 base per faction, up to N units.",
      "  Moving to an adjacent area costs 1 turn.",
      "  Raid an enemy area from an adjacent position.",
      "",
      bold "ENVIRONMENT PHASES",
      "  UV HIGH    -- Binary stars near. Fuel costs +50%, food -20%.",
      "  FREEZE     -- Far orbit. Food production halved.",
      "  ASH STORM  -- Volcanic event. Combat -1. Poor visibility.",
      "",
      bold "WIN CONDITION",
      "  Destroy all enemy bases. Last faction with a base wins.",
      "",
      dim "Press any key to return."
    ]

-- Helpers
getFac :: FactionId -> GameState -> Faction
getFac fid gs = fromMaybe (error "fac") $ Map.lookup fid (gsFacs gs)

factionSym' :: FactionId -> Char
factionSym' NeoRepublic = 'N'
factionSym' SouthernEmpire = 'S'
factionSym' Insects = 'I'
factionSym' Thieves = 'T'
