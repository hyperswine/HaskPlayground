{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | A minimal histogram-binned decision tree (classification, entropy gain).
--   * Dataset is column-major: a boxed Vector of unboxed Double columns.
--   * Each column is pre-binned once into @pBins@ (default 256) equal-width
--     bins, giving a Word8 code per cell. All split search happens on codes.
--   * At each node, for each column: one O(n) pass builds a (bins x classes)
--     class-count histogram, then one O(bins) prefix sweep scores every
--     threshold. Best (column, bin) wins if its gain clears @pMinGain@.
--   * Stopping: purity, @pMaxDepth@, @pMinLeaf@, or no split clearing the
--     gain threshold.
module DTs where

import Control.Monad.ST (runST)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)

-- ---------------------------------------------------------------------------
-- Types

type Col = U.Vector Double

-- \| ncols entries, each of length nrows
type Dataset = V.Vector Col

-- \| class ids in [0 .. pClasses-1]
type Labels = U.Vector Int

-- | pBins: histogram bins per column (<= 256), pMaxDepth: max tree depth, pMinGain: ditch a split if gain below this, pMinLeaf: min samples on each side of a split, pClasses: number of classes
data Params = Params {pBins :: !Int, pMaxDepth :: !Int, pMinGain :: !Double, pMinLeaf :: !Int, pClasses :: !Int}

defaultParams :: Int -> Params
defaultParams k = Params {pBins = 256, pMaxDepth = 8, pMinGain = 1e-3, pMinLeaf = 5, pClasses = k}

-- | Leaf: predicted class; Split: column, raw threshold; left if x < thr
data Tree = Leaf !Int | Split !Int !Double !Tree !Tree deriving (Show)

-- Pre-binned view of the dataset. bCodes: per column, bin code per row; bEdges: per column, upper edge of bin i
data Binned = Binned {bCodes :: !(V.Vector (U.Vector Word8)), bEdges :: !(V.Vector (U.Vector Double))}

-- ---------------------------------------------------------------------------
-- Binning

binColumn :: Int -> Col -> (U.Vector Word8, U.Vector Double)
binColumn nb col = (codes, edges)
  where
    lo = U.minimum col
    hi = U.maximum col
    w = let w0 = (hi - lo) / fromIntegral nb in if w0 <= 0 then 1 else w0
    codes = U.map (\x -> fromIntegral (min (nb - 1) (floor ((x - lo) / w) :: Int))) col
    -- edges ! i == upper bound of bin i, i.e. code <= i  <=>  x < edges ! i
    edges = U.generate (nb - 1) (\i -> lo + fromIntegral (i + 1) * w)

binAll :: Int -> Dataset -> Binned
binAll nb ds = Binned (V.map fst bs) (V.map snd bs)
  where
    bs = V.map (binColumn nb) ds

-- ---------------------------------------------------------------------------
-- Impurity

entropy :: U.Vector Int -> Double
entropy cnts
  | n <= 0 = 0
  | otherwise = U.foldl' step 0 cnts
  where
    n = fromIntegral (U.sum cnts) :: Double
    step !acc c
      | c == 0 = acc
      | otherwise = let p = fromIntegral c / n in acc - p * logBase 2 p

classCounts :: Int -> Labels -> U.Vector Int -> U.Vector Int
classCounts k ys idxs = runST $ do
  m <- UM.replicate k 0
  U.forM_ idxs $ \i -> UM.modify m (+ 1) (ys U.! i)
  U.unsafeFreeze m

argmax :: U.Vector Int -> Int
argmax = U.maxIndex

-- ---------------------------------------------------------------------------
-- Split search: one column

-- | Best (gain, binThreshold) for one column at this node, or Nothing if no threshold leaves >= pMinLeaf samples on both sides. Semantics of a result @(g, t)@: send row left iff its bin code <= t. @parentH@ is the entropy of the parent node, @codes@ is the bin code per row for this column, @ys@ is the class label per row, and @idxs@ is the subset of rows at this node.
bestSplitCol :: Params -> Double -> U.Vector Word8 -> Labels -> U.Vector Int -> Maybe (Double, Int)
bestSplitCol Params {..} parentH codes ys idxs = sweep
  where
    n = U.length idxs
    nD = fromIntegral n :: Double

    -- (bins x classes) class-count histogram, one pass over the node's rows
    hist :: U.Vector Int
    hist = runST $ do
      m <- UM.replicate (pBins * pClasses) 0
      U.forM_ idxs $ \i -> do
        let b = fromIntegral (codes U.! i)
        UM.modify m (+ 1) (b * pClasses + ys U.! i)
      U.unsafeFreeze m

    binRow b = U.slice (b * pClasses) pClasses hist

    -- prefix sweep over thresholds t = 0 .. pBins-2
    sweep = go 0 (U.replicate pClasses 0) Nothing
      where
        go !t !leftC !best
          | t >= pBins - 1 = best
          | otherwise =
              let leftC' = U.zipWith (+) leftC (binRow t)
                  nl = U.sum leftC'
                  nr = n - nl
                  best'
                    | nl < pMinLeaf || nr < pMinLeaf = best
                    | otherwise =
                        let rightC = U.zipWith subtract leftC' totalC
                            wl = fromIntegral nl / nD
                            wr = fromIntegral nr / nD
                            g = parentH - (wl * entropy leftC' + wr * entropy rightC)
                         in case best of
                              Just (g0, _) | g0 >= g -> best
                              _ -> Just (g, t)
               in go (t + 1) leftC' best'
        totalC = U.foldl' (\acc b -> U.zipWith (+) acc (binRow b)) (U.replicate pClasses 0) (U.enumFromN 0 pBins)

-- ---------------------------------------------------------------------------
-- Tree building

build :: Params -> Binned -> Labels -> Int -> U.Vector Int -> Tree
build ps@Params {..} bn ys depth idxs
  | depth >= pMaxDepth = leaf
  | U.length idxs < 2 * pMinLeaf = leaf
  | parentH == 0 = leaf -- already pure
  | otherwise =
      case bestOverall of
        Just (gain, col, t)
          | gain >= pMinGain ->
              let codes = bCodes bn V.! col
                  (l, r) = U.partition (\i -> codes U.! i <= fromIntegral t) idxs
                  thr = (bEdges bn V.! col) U.! t
               in Split col thr (build ps bn ys (depth + 1) l) (build ps bn ys (depth + 1) r)
        _ -> leaf -- nothing clears pMinGain
  where
    counts = classCounts pClasses ys idxs
    parentH = entropy counts
    leaf = Leaf (argmax counts)

    bestOverall = V.ifoldl' step Nothing (bCodes bn)
      where
        step acc col codes = case bestSplitCol ps parentH codes ys idxs of
          Nothing -> acc
          Just (g, t) -> case acc of
            Just (g0, _, _) | g0 >= g -> acc
            _ -> Just (g, col, t)

fit :: Params -> Dataset -> Labels -> Tree
fit ps ds ys = build ps (binAll (pBins ps) ds) ys 0 (U.enumFromN 0 (U.length ys))

predict :: Tree -> U.Vector Double -> Int
predict (Leaf c) _ = c
predict (Split col thr l r) x
  | x U.! col < thr = predict l x
  | otherwise = predict r x

-- ---------------------------------------------------------------------------
-- Demo: noisy XOR — the canonical "no linear tally can do this" dataset

lcg :: Int -> [Double] -- crude deterministic uniforms in [0,1)
lcg = map (\s -> fromIntegral s / 2147483648) . tail . iterate (\s -> (1103515245 * s + 12345) `mod` 2147483648)

xorData :: Int -> Int -> (Dataset, Labels)
xorData seed n = (V.fromList [xs, zs], ls)
  where
    us = lcg seed
    xs = U.fromList (map (\u -> u * 2 - 1) (take n us))
    zs = U.fromList (map (\u -> u * 2 - 1) (take n (drop n us)))
    noise = take n (drop (2 * n) us)
    -- 5% label noise
    ls = U.fromList [if nz < 0.05 then 1 - c else c | i <- [0 .. n - 1], let c = if (xs U.! i > 0) /= (zs U.! i > 0) then 1 else 0; nz = noise !! i]

accuracy :: Tree -> Dataset -> Labels -> Double
accuracy t ds ys = fromIntegral hits / fromIntegral n
  where
    n = U.length ys
    row i = U.convert (V.map (U.! i) ds)
    hits = length [() | i <- [0 .. n - 1], predict t (row i) == ys U.! i]

depthOf :: Tree -> Int
depthOf (Leaf _) = 0
depthOf (Split _ _ l r) = 1 + max (depthOf l) (depthOf r)

sizeOf :: Tree -> Int
sizeOf (Leaf _) = 1
sizeOf (Split _ _ l r) = 1 + sizeOf l + sizeOf r

main :: IO ()
main = do
  let (trainX, trainY) = xorData 42 4000
      (testX, testY) = xorData 7 1000
      ps = defaultParams 2
      tree = fit ps trainX trainY
  putStrLn $ "nodes=" ++ show (sizeOf tree) ++ " depth=" ++ show (depthOf tree)
  putStrLn $ "train acc: " ++ show (accuracy tree trainX trainY)
  putStrLn $ "test  acc: " ++ show (accuracy tree testX testY)
  -- top of the tree should be x<~0 then y<~0 (or vice versa)
  case tree of
    Split c thr _ _ ->
      putStrLn $ "root split: col " ++ show c ++ " @ " ++ show thr
    _ -> pure ()
