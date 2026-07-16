{-# LANGUAGE TupleSections, LambdaCase #-}
-- ============================================================================
-- PortSim: a tiny topological, port-based mechanics simulator (PoC)
-- ============================================================================
-- No geometry, no world frame. A system is a graph:
--
--   * Components expose typed ports. A port carries an ACROSS variable
--     (angular / linear velocity -- a rate) and a THROUGH variable
--     (torque / force), defined as flowing INTO the component.
--   * Connecting ports forms a "net": all ports on a net share the across
--     variable; their through variables sum to zero (Kirchhoff).
--   * Each component contributes one constitutive equation per port,
--     possibly involving internal state (spring extension, stored
--     velocity), discretised semi-implicitly with timestep dt.
--
-- Each timestep is therefore one small LINEAR system: unknowns are net
-- across-variables plus port through-variables; equations are per-net
-- balance laws plus per-component relations. Solved by Gaussian
-- elimination; loops in the graph are handled for free.
--
-- Ideal (lossless) transformers -- gears, levers, winches -- are pure
-- algebraic relations between two ports; power conservation fixes the
-- through-variable relation given the across-variable relation.
-- ============================================================================

module PortSim where

import qualified Data.Map.Strict as M
import           Data.List          (nub, foldl1')
import           Data.Maybe         (fromMaybe)
import           System.Environment (getArgs)
import           Text.Printf        (printf)

-- ----------------------------------------------------------------------------
-- Core types
-- ----------------------------------------------------------------------------

data Domain = Rot | Trans deriving (Eq, Show)

type CompName = String
type PortName = String
type PortRef  = (CompName, PortName)

type State  = M.Map String Double            -- component internal state
type Eqn    = ([(Int, Double)], Double)      -- sum coef*var = rhs
type Solved = M.Map PortName (Double, Double)-- port -> (across, through)

data Component = Component
  { cName   :: CompName
  , cPorts  :: [(PortName, Domain)]
  , cEqns   :: State -> Double -> (PortName -> (Int, Int)) -> [Eqn]
  , cState0 :: State
  , cStep   :: State -> Double -> Solved -> State
  }

data System = System
  { sComps :: [Component]
  , sNets  :: [(String, [PortRef])]
  }

-- ----------------------------------------------------------------------------
-- Netlist elaboration
-- ----------------------------------------------------------------------------

-- Ports not mentioned in any net become open singleton nets;
-- Kirchhoff then forces their through-variable to zero (a free end).
elaborate :: System -> [(String, [PortRef])]
elaborate (System comps nets) = nets ++ singles
  where
    mentioned = concatMap snd nets
    allPorts  = [ (cName c, p) | c <- comps, (p, _) <- cPorts c ]
    singles   = [ (cn ++ "." ++ pn, [(cn, pn)])
                | (cn, pn) <- allPorts, (cn, pn) `notElem` mentioned ]

domainOf :: [Component] -> PortRef -> Domain
domainOf comps (cn, pn) =
  head [ d | c <- comps, cName c == cn, (p, d) <- cPorts c, p == pn ]

checkNets :: [Component] -> [(String, [PortRef])] -> a -> a
checkNets comps nets k
  | any bad nets = error "PortSim: a net mixes Rot and Trans ports"
  | otherwise    = k
  where bad (_, prs) = length (nub (map (domainOf comps) prs)) > 1

-- ----------------------------------------------------------------------------
-- One timestep: assemble + solve the linear system
-- ----------------------------------------------------------------------------

solveStep :: [Component] -> [(String, [PortRef])] -> M.Map CompName State
          -> Double -> (M.Map String Double, M.Map CompName Solved)
solveStep comps nets states dt = (netVals, solvedByComp)
  where
    nNets   = length nets
    ports   = [ (cName c, p) | c <- comps, (p, _) <- cPorts c ]
    portIx  = M.fromList (zip ports [nNets ..])
    netIxOf = M.fromList [ (pr, i) | (i, (_, prs)) <- zip [0..] nets, pr <- prs ]
    nVars   = nNets + length ports

    ixFor :: CompName -> PortName -> (Int, Int)
    ixFor cn pn =
      ( fromMaybe (err "net")  (M.lookup (cn, pn) netIxOf)
      , fromMaybe (err "port") (M.lookup (cn, pn) portIx) )
      where err w = error ("PortSim: unknown " ++ w ++ " for " ++ cn ++ "." ++ pn)

    kcl  = [ ( [ (portIx M.! pr, 1) | pr <- prs ], 0 ) | (_, prs) <- nets ]
    body = concat [ cEqns c (states M.! cName c) dt (ixFor (cName c)) | c <- comps ]

    xs = gauss nVars (kcl ++ body)

    netVals = M.fromList [ (nm, xs !! i) | (i, (nm, _)) <- zip [0..] nets ]
    solvedByComp = M.fromList
      [ ( cName c
        , M.fromList [ (p, (xs !! a, xs !! t))
                     | (p, _) <- cPorts c, let (a, t) = ixFor (cName c) p ] )
      | c <- comps ]

-- Dense Gaussian elimination with partial pivoting on an augmented matrix.
gauss :: Int -> [Eqn] -> [Double]
gauss n eqns
  | length eqns /= n =
      error $ "PortSim: system not square (" ++ show (length eqns)
            ++ " eqns, " ++ show n ++ " vars)"
  | otherwise = backsub (reduce 0 rows0)
  where
    rows0 = [ [ M.findWithDefault 0 j m | j <- [0 .. n-1] ] ++ [rhs]
            | (terms, rhs) <- eqns
            , let m = M.fromListWith (+) terms ]

    reduce k rs
      | k >= n    = rs
      | otherwise =
          let (done, rest) = splitAt k rs
              (bi, _) = maximumOn (\(_, r) -> abs (r !! k)) (zip [0 :: Int ..] rest)
              piv     = rest !! bi
              others  = take bi rest ++ drop (bi + 1) rest
              elim r  = let f = (r !! k) / (piv !! k)
                        in zipWith (\a b -> a - f * b) r piv
          in if abs (piv !! k) < 1e-12
               then error "PortSim: singular system (missing ground/source, or redundant constraint)"
               else reduce (k + 1) (done ++ [piv] ++ map elim others)

    backsub rs = xs
      where
        xs = [ ( (r !! n) - sum [ (r !! j) * (xs !! j) | j <- [i + 1 .. n - 1] ] )
               / (r !! i)
             | (i, r) <- zip [0..] rs ]

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = foldl1' (\a b -> if f b > f a then b else a)

-- ----------------------------------------------------------------------------
-- Component library
-- ----------------------------------------------------------------------------

stateless :: State -> Double -> Solved -> State
stateless s _ _ = s

-- Ideal velocity source: pins the net rate (kinematic drive).
velSource :: CompName -> Domain -> Double -> Component
velSource nm dom w =
  Component nm [("p", dom)] eqs M.empty stateless
  where eqs _ _ ix = let (a, _) = ix "p" in [ ([(a, 1)], w) ]

-- Ideal effort source: injects torque/force f into its net.
effSource :: CompName -> Domain -> Double -> Component
effSource nm dom f =
  Component nm [("p", dom)] eqs M.empty stateless
  where eqs _ _ ix = let (_, t) = ix "p" in [ ([(t, 1)], -f) ]

-- Ground: net rate is zero.
groundC :: CompName -> Domain -> Component
groundC nm dom =
  Component nm [("p", dom)] eqs M.empty stateless
  where eqs _ _ ix = let (a, _) = ix "p" in [ ([(a, 1)], 0) ]

-- Mass / rotary inertia. State: "v" (rate), "x" (integrated position/angle).
--   through = m * dv/dt   (semi-implicit:  (m/dt) v - t = (m/dt) v_prev)
inertiaC :: CompName -> Domain -> Double -> Double -> Component
inertiaC nm dom m v0 =
  Component nm [("p", dom)] eqs (M.fromList [("v", v0), ("x", 0)]) step
  where
    eqs s dt ix =
      let (a, t) = ix "p"
      in [ ([(a, m / dt), (t, -1)], (m / dt) * (s M.! "v")) ]
    step s dt sol =
      let (a, _) = sol M.! "p"
      in M.insert "v" a (M.adjust (+ dt * a) "x" s)

-- Spring to ground. State: "x" (extension).  through = k * x
--   semi-implicit:  t - k*dt*a = k * x_prev
springG :: CompName -> Domain -> Double -> Double -> Component
springG nm dom k x0 =
  Component nm [("p", dom)] eqs (M.fromList [("x", x0)]) step
  where
    eqs s dt ix =
      let (a, t) = ix "p"
      in [ ([(t, 1), (a, -k * dt)], k * (s M.! "x")) ]
    step s dt sol =
      let (a, _) = sol M.! "p" in M.adjust (+ dt * a) "x" s

-- Damper to ground:  through = c * rate.
damperG :: CompName -> Domain -> Double -> Component
damperG nm dom c =
  Component nm [("p", dom)] eqs M.empty stateless
  where
    eqs _ _ ix = let (a, t) = ix "p" in [ ([(t, 1), (a, -c)], 0) ]

-- Ideal gear pair, ratio r = w_in / w_out  (r > 1 => reduction).
--   kinematics:  a_in - r * a_out = 0
--   power:       r * t_in + t_out = 0
gearC :: CompName -> Double -> Component
gearC nm r =
  Component nm [("in", Rot), ("out", Rot)] eqs M.empty stateless
  where
    eqs _ _ ix =
      let (ai, ti) = ix "in"; (ao, to) = ix "out"
      in [ ([(ai, 1), (ao, -r)], 0)
         , ([(ti, r), (to, 1)], 0) ]

-- Ideal lever, arms la and lb on opposite sides of the fulcrum.
--   kinematics:  lb * v_a + la * v_b = 0     (ends move oppositely)
--   power:       la * t_a - lb * t_b = 0
leverC :: CompName -> Double -> Double -> Component
leverC nm la lb =
  Component nm [("a", Trans), ("b", Trans)] eqs M.empty stateless
  where
    eqs _ _ ix =
      let (va, ta) = ix "a"; (vb, tb) = ix "b"
      in [ ([(va, lb), (vb, la)], 0)
         , ([(ta, la), (tb, -lb)], 0) ]

-- Ideal winch drum, radius rad: Rot shaft <-> Trans cable.
--   kinematics:  v_cable - rad * w_shaft = 0
--   power:       t_shaft + rad * t_cable = 0
winchC :: CompName -> Double -> Component
winchC nm rad =
  Component nm [("shaft", Rot), ("cable", Trans)] eqs M.empty stateless
  where
    eqs _ _ ix =
      let (ws, ts) = ix "shaft"; (vc, tc) = ix "cable"
      in [ ([(vc, 1), (ws, -rad)], 0)
         , ([(ts, 1), (tc, rad)], 0) ]

-- Gravity on a Trans net (positive rate = up): a constant downward effort.
gravityC :: CompName -> Double -> Double -> Component
gravityC nm m g = effSource nm Trans (-(m * g))

-- ----------------------------------------------------------------------------
-- Simulation driver
-- ----------------------------------------------------------------------------

simulate :: System -> Double -> Int
         -> (Int -> Double -> M.Map String Double -> M.Map CompName Solved
                 -> M.Map CompName State -> IO ())
         -> IO ()
simulate sys dt steps report =
    checkNets comps nets (go 1 st0)
  where
    nets  = elaborate sys
    comps = sComps sys
    st0   = M.fromList [ (cName c, cState0 c) | c <- comps ]
    go i states
      | i > steps = pure ()
      | otherwise = do
          let (netVals, solved) = solveStep comps nets states dt
              states' = M.fromList
                [ (cName c, cStep c (states M.! cName c) dt (solved M.! cName c))
                | c <- comps ]
          report i (fromIntegral i * dt) netVals solved states'
          go (i + 1) states'

header :: String -> IO ()
header s = putStrLn ("\n== " ++ s ++ " ==")

-- ----------------------------------------------------------------------------
-- Example 1: gear train, kinematics + torque flow (steady state, one solve)
-- ----------------------------------------------------------------------------

exGears :: IO ()
exGears = do
  header "Gear train: 10 rad/s motor -> 3:1 -> 4:1 -> viscous load (c = 2)"
  let sys = System
        { sComps =
            [ velSource "motor" Rot 10
            , gearC "g1" 3
            , gearC "g2" 4
            , damperG "load" Rot 2
            ]
        , sNets =
            [ ("shaftIn",  [("motor","p"), ("g1","in")])
            , ("shaftMid", [("g1","out"), ("g2","in")])
            , ("shaftOut", [("g2","out"), ("load","p")])
            ]
        }
  simulate sys 1 1 $ \_ _ nv sv _ -> do
    let w nm = nv M.! nm
        (_, tMotor) = (sv M.! "motor") M.! "p"
        (_, tLoad)  = (sv M.! "load")  M.! "p"
    printf "  speeds   : in %.3f  mid %.3f  out %.4f rad/s  (expected out = 10/12 = 0.8333)\n"
           (w "shaftIn") (w "shaftMid") (w "shaftOut")
    printf "  torques  : motor supplies %.4f Nm, load absorbs %.4f Nm (12x amplification)\n"
           (negate tMotor) tLoad
    printf "  power    : motor %.4f W  ==  load %.4f W (lossless transmission)\n"
           (negate tMotor * w "shaftIn") (tLoad * w "shaftOut")

-- ----------------------------------------------------------------------------
-- Example 2: seesaw lever with two masses (dynamics)
-- ----------------------------------------------------------------------------

exLever :: IO ()
exLever = do
  header "Seesaw: 10 kg vs 40 kg on equal 1 m arms (analytic accel = 5.886 m/s^2)"
  let sys = System
        { sComps =
            [ leverC "lever" 1 1
            , inertiaC "mA" Trans 10 0
            , inertiaC "mB" Trans 40 0
            , gravityC "gA" 10 9.81
            , gravityC "gB" 40 9.81
            ]
        , sNets =
            [ ("sideA", [("lever","a"), ("mA","p"), ("gA","p")])
            , ("sideB", [("lever","b"), ("mB","p"), ("gB","p")])
            ]
        }
      dt = 0.01
  putStrLn "    t      vA (up)   vB (down)   xA"
  simulate sys dt 200 $ \i t nv _ st ->
    if i `mod` 50 == 0
      then printf "  %5.2f  %8.4f  %9.4f  %7.4f\n"
             t (nv M.! "sideA") (nv M.! "sideB") ((st M.! "mA") M.! "x")
      else pure ()
  putStrLn "  (vA/t should be ~5.886: light side accelerates up, heavy side down)"

-- ----------------------------------------------------------------------------
-- Example 3: spring-mass oscillator (k = 100, m = 1, x0 = 0.1 -> T = 0.628 s)
-- ----------------------------------------------------------------------------

exSpring :: IO ()
exSpring = do
  header "Spring-mass oscillator: k=100 N/m, m=1 kg, x0=0.1 m (period 0.628 s)"
  let sys = System
        { sComps = [ springG "spr" Trans 100 0.1, inertiaC "m" Trans 1 0 ]
        , sNets  = [ ("node", [("spr","p"), ("m","p")]) ]
        }
      dt = 0.002
  putStrLn "    t      x (spring extension)    v"
  simulate sys dt 400 $ \i t nv _ st ->
    if i `mod` 40 == 0
      then printf "  %5.3f  %10.4f  %18.4f\n"
             t ((st M.! "spr") M.! "x") (nv M.! "node")
      else pure ()
  putStrLn "  (x swings -0.1..0.1; sign flips roughly every 0.314 s)"

-- ----------------------------------------------------------------------------
-- Example 4: hoist -- motor -> 20:1 gearbox -> winch -> 80 kg load
-- ----------------------------------------------------------------------------

exHoist :: IO ()
exHoist = do
  header "Hoist: 5 Nm motor -> 20:1 gears -> winch r=0.1 m -> 80 kg + friction c=50"
  let sys = System
        { sComps =
            [ effSource "motor" Rot 5
            , gearC "gbox" 20
            , winchC "drum" 0.1
            , inertiaC "load" Trans 80 0
            , gravityC "grav" 80 9.81
            , damperG "fric" Trans 50
            ]
        , sNets =
            [ ("motorShaft", [("motor","p"), ("gbox","in")])
            , ("drumShaft",  [("gbox","out"), ("drum","shaft")])
            , ("cable",      [("drum","cable"), ("load","p"),
                              ("grav","p"), ("fric","p")])
            ]
        }
      dt = 0.02
  putStrLn "  cable force available: 5*20/0.1 = 1000 N vs weight 784.8 N -> lifts"
  putStrLn "  terminal speed: (1000-784.8)/50 = 4.304 m/s"
  putStrLn "    t      v_load     height     motor rpm"
  simulate sys dt 400 $ \i t nv _ st ->
    if i `mod` 50 == 0
      then printf "  %5.2f  %8.4f  %9.4f  %10.1f\n"
             t (nv M.! "cable") ((st M.! "load") M.! "x")
             (nv M.! "motorShaft" * 60 / (2 * pi))
      else pure ()

-- ----------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  let run = \case
        "gears"  -> exGears
        "lever"  -> exLever
        "spring" -> exSpring
        "hoist"  -> exHoist
        x        -> putStrLn ("unknown example: " ++ x)
  case args of
    [] -> mapM_ run ["gears", "lever", "spring", "hoist"]
    xs -> mapM_ run xs
