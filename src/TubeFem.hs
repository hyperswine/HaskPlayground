{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- TubeFem.hs — minimal FEM pipeline PoC
--   geometry (hollow tube) -> mesh (beam elements) -> loads (gravity + point)
--   -> assemble -> solve -> stress recovery -> ASCII side-view render
--
-- Pure Haskell (base only). Euler–Bernoulli beam bending in the x–y plane.
-- DOFs per node: [v (deflection), theta (rotation)].

module TubeFem where

import Data.List (foldl', maximumBy)
import Data.Ord (comparing)
import Text.Printf (printf)

-- ────────────────────────────── Model types ──────────────────────────────

data Material = Material
  { matName :: String,
    eMod :: Double, -- Young's modulus [Pa]
    density :: Double -- [kg/m^3]
  }

aluminium :: Material
aluminium = Material "Aluminium 6061" 69e9 2700

-- Hollow circular tube cross-section
data Section = Tube
  { outerD :: Double, -- [m]
    wall :: Double -- [m]
  }

area :: Section -> Double
area Tube {..} = pi / 4 * (outerD ^ 2 - innerD ^ 2) where innerD = outerD - 2 * wall

inertia :: Section -> Double -- second moment of area I [m^4]
inertia Tube {..} = pi / 64 * (outerD ^ 4 - innerD ^ 4) where innerD = outerD - 2 * wall

data BC = Fixed | Free deriving (Eq, Show)

data Model = Model
  { material :: Material,
    section :: Section,
    tubeLen :: Double, -- [m]
    nElems :: Int,
    bcLeft :: BC, -- boundary condition at x=0 face
    bcRight :: BC, -- boundary condition at x=L face
    gravity :: Double, -- [m/s^2] acting in -y
    pointLoads :: [(Double, Double)] -- (x position [m], force [N], -y positive down… we use signed: negative = downward)
  }

-- ────────────────────────────── Tiny linear algebra ──────────────────────

type Mat = [[Double]]

type Vec = [Double]

zeros :: Int -> Int -> Mat
zeros r c = replicate r (replicate c 0)

-- Gaussian elimination with partial pivoting. Fine for PoC-scale systems.
solveLin :: Mat -> Vec -> Vec
solveLin a b = backSub (forward (zipWith (\row rhs -> row ++ [rhs]) a b))
  where
    n = length b
    forward m0 = foldl' step m0 [0 .. n - 1]
      where
        step rows k =
          let (above, rest) = splitAt k rows
              pivIdx = snd $ maximumBy (comparing fst) [(abs (r !! k), ix) | (ix, r) <- zip [0 ..] rest]
              pk = rest !! pivIdx
              others = [r | (ix, r) <- zip [0 ..] rest, ix /= pivIdx]
              elim r = let f = (r !! k) / (pk !! k) in zipWith (\ri pi_ -> ri - f * pi_) r pk
           in above ++ pk : map elim others
    backSub m = go (reverse [0 .. n - 1]) (replicate n 0)
      where
        go [] xs = xs
        go (i : is) xs =
          let row = m !! i
              s = sum [row !! j * xs !! j | j <- [i + 1 .. n - 1]]
              xi = (row !! n - s) / (row !! i)
           in go is (setAt i xi xs)

setAt :: Int -> a -> [a] -> [a]
setAt i x xs = take i xs ++ x : drop (i + 1) xs

addAt2 :: Mat -> [(Int, Int, Double)] -> Mat
addAt2 = foldl' (\m (i, j, v) -> setAt i (setAt j ((m !! i !! j) + v) (m !! i)) m)

addAt1 :: Vec -> [(Int, Double)] -> Vec
addAt1 = foldl' (\v (i, x) -> setAt i ((v !! i) + x) v)

-- ────────────────────────────── FEM core ──────────────────────────────

-- Euler–Bernoulli beam element stiffness (bending), 4x4 over [v1,th1,v2,th2]
elemK :: Double -> Double -> Double -> Mat
elemK e i l =
  let c = e * i / l ^ 3
   in map
        (map (* c))
        [ [12, 6 * l, -12, 6 * l],
          [6 * l, 4 * l ^ 2, -6 * l, 2 * l ^ 2],
          [-12, -6 * l, 12, -6 * l],
          [6 * l, 2 * l ^ 2, -6 * l, 4 * l ^ 2]
        ]

-- Equivalent nodal loads for uniform distributed load w [N/m] (signed)
elemDistLoad :: Double -> Double -> Vec
elemDistLoad w l = [w * l / 2, w * l ^ 2 / 12, w * l / 2, -w * l ^ 2 / 12]

data Solution = Solution
  { nodeXs :: [Double],
    deflect :: [Double], -- v at each node [m]
    elemStress :: [Double] -- max bending stress per element [Pa] (signed by moment at start node)
  }

solveModel :: Model -> Solution
solveModel Model {..} =
  let ne = nElems
      nn = ne + 1
      l = tubeLen / fromIntegral ne
      xs = [fromIntegral k * l | k <- [0 .. ne]]
      e = eMod material
      iSec = inertia section
      aSec = area section
      ndof = 2 * nn

      -- distributed load from self-weight under gravity (downward → negative)
      w = negate (density material * aSec * gravity)

      -- assemble global K and F
      kGlobal0 = zeros ndof ndof
      f0 = replicate ndof 0
      ke = elemK e iSec l
      fe = elemDistLoad w l
      dofs el = [2 * el, 2 * el + 1, 2 * el + 2, 2 * el + 3]

      kGlobal = foldl' (\m el -> addAt2 m [(dofs el !! r, dofs el !! c, ke !! r !! c) | r <- [0 .. 3], c <- [0 .. 3]]) kGlobal0 [0 .. ne - 1]

      fGrav = foldl' (\v el -> addAt1 v [(dofs el !! r, fe !! r) | r <- [0 .. 3]]) f0 [0 .. ne - 1]

      -- point loads: snap each to nearest node (fine for PoC)
      nearestNode x = round (x / l) :: Int
      fAll = addAt1 fGrav [(2 * nearestNode px, pf) | (px, pf) <- pointLoads]

      -- boundary conditions: Fixed face clamps v and theta at that node
      fixedDofs = concat [[0, 1] | bcLeft == Fixed] ++ concat [[ndof - 2, ndof - 1] | bcRight == Fixed]

      -- DOF elimination: big-number-free reduction (delete rows/cols)
      freeDofs = [d | d <- [0 .. ndof - 1], d `notElem` fixedDofs]
      kRed = [[kGlobal !! r !! c | c <- freeDofs] | r <- freeDofs]
      fRed = [fAll !! r | r <- freeDofs]

      uRed = solveLin kRed fRed
      uFull = foldl' (\v (d, x) -> setAt d x v) (replicate ndof 0) (zip freeDofs uRed)

      vs = [uFull !! (2 * k) | k <- [0 .. nn - 1]]

      -- stress recovery: element end forces -> bending moment -> sigma = M*c/I
      c = outerD section / 2
      stressOf el =
        let ue = [uFull !! d | d <- dofs el]
            fEnd = [sum (zipWith (*) row ue) | row <- ke]
            m1 = fEnd !! 1 -- moment at start node
            m2 = fEnd !! 3 -- moment at end node
            m = if abs m1 >= abs m2 then m1 else m2
         in m * c / iSec
      stresses = map stressOf [0 .. ne - 1]
   in Solution xs vs stresses

-- ────────────────────────────── ASCII renderer ──────────────────────────────

-- Side view (x–y plane): deflected centerline, char = stress magnitude bucket
render :: Model -> Solution -> String
render Model {..} Solution {..} =
  let cols = 72
      rows = 15
      ramp = ".,:-=+*#%@" -- low → high |stress|
      maxS = maximum (map abs elemStress ++ [1e-12])
      maxV = maximum (map abs deflect ++ [1e-12])

      -- map node index -> column
      nn = length nodeXs
      colOf k = min (cols - 1) $ round (fromIntegral k / fromIntegral (nn - 1) * fromIntegral (cols - 1) :: Double)
      -- deflection row: 0 deflection at row midRow, scaled to fill
      midRow = 2 :: Int
      rowOf v = let scaled = v / maxV * fromIntegral (rows - midRow - 2) in max 0 . min (rows - 1) $ midRow - round scaled

      stressChar el = ramp !! min (length ramp - 1) (floor (abs (elemStress !! el) / maxS * fromIntegral (length ramp - 1)))

      blank = replicate rows (replicate cols ' ')
      put g (r, cIdx, ch) = setAt r (setAt cIdx ch (g !! r)) g

      -- draw each element as a short segment between its two node positions
      cells =
        concat
          [ let (c1, c2) = (colOf el, colOf (el + 1))
                (r1, r2) = (rowOf (deflect !! el), rowOf (deflect !! (el + 1)))
                n = max 1 (c2 - c1)
                ch = stressChar el
             in [(round (fromIntegral r1 + (fromIntegral (r2 - r1) * fromIntegral s / fromIntegral n :: Double)), c1 + s, ch) | s <- [0 .. n]]
            | el <- [0 .. length elemStress - 1]
          ]

      grid = foldl' put blank cells
      -- wall glyphs for fixed faces
      wallify g = [[decorate rIdx cIdx ch | (cIdx, ch) <- zip [0 ..] row] | (rIdx, row) <- zip [0 ..] g]
        where
          decorate _ 0 ch
            | bcLeft == Fixed = '|'
            | otherwise = ch
          decorate _ cI ch | cI == cols - 1 && bcRight == Fixed = '|'
          decorate _ _ ch = ch

      legendBuckets = unwords [printf "'%c'<%.0f" chr (maxS * fromIntegral (ix + 1) / fromIntegral (length ramp) / 1e6) | (ix, chr) <- zip [0 :: Int ..] ramp, ix `mod` 3 == 0]
   in unlines (map (dropTrailing) (wallify grid)) ++ printf "\nstress ramp \"%s\"  (|sigma|, buckets up to %.1f MPa)\n" ramp (maxS / 1e6) ++ printf "legend: %s (MPa)\n" legendBuckets
  where
    dropTrailing = reverse . dropWhile (== ' ') . reverse

-- ────────────────────────────── Report + main ──────────────────────────────

main :: IO ()
main = do
  let model =
        Model
          { material = aluminium,
            section = Tube {outerD = 0.030, wall = 0.002}, -- 30mm OD, 2mm wall
            tubeLen = 1.0,
            nElems = 24,
            bcLeft = Fixed,
            bcRight = Free,
            gravity = 9.81,
            pointLoads = [(0.70, -200)] -- 200 N downward at x = 0.7 m
          }
      sol@Solution {..} = solveModel model

  printf "── TubeFem PoC ─────────────────────────────────────────────\n"
  printf
    "material: %s  (E = %.0f GPa, rho = %.0f kg/m3)\n"
    (matName (material model))
    (eMod (material model) / 1e9)
    (density (material model))
  printf
    "section : hollow tube OD %.1f mm, wall %.1f mm  (A = %.1f mm2, I = %.2e m4)\n"
    (outerD (section model) * 1e3)
    (wall (section model) * 1e3)
    (area (section model) * 1e6)
    (inertia (section model))
  printf
    "mesh    : %d beam elements over %.2f m, BC left=%s right=%s\n"
    (nElems model)
    (tubeLen model)
    (show (bcLeft model))
    (show (bcRight model))
  printf
    "loads   : gravity %.2f m/s2 (self-weight) + point %s\n\n"
    (gravity model)
    (unwords [printf "%.0fN@%.2fm" f x | (x, f) <- pointLoads model])

  let tipV = last deflect
      maxSig = maximumBy (comparing abs) elemStress
  printf "tip deflection : %.3f mm\n" (tipV * 1e3)
  printf "max |stress|   : %.1f MPa  (yield 6061-T6 ~ 276 MPa)\n\n" (abs maxSig / 1e6)

  putStrLn "side view (deflection exaggerated to fit; char = local |stress|):"
  putStr (render model sol)
