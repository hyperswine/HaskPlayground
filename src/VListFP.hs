{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

--   1. VList  -- segmented list (exponentially growing segments, simulating unboxed contiguous arrays within each segment)
--   2. FPR_Record typeclass -- any record can declare its SoA layout
--   3. cons (::) fans out to each column simultaneously
--   4. List.map   -- can split into independent per-column maps
--   5. List.filter -- keeps all columns in sync
--   6. List.zipWith -- operates column-by-column
--   7. ColUpdate  -- explicit per-field update, enabling SIMD-style splitting
--   8. FprNum     -- numeric type as a record, expanding to 3 float columns

module VListFP where

import Data.Proxy (Proxy (..))

-- ---------------------------------------------------------------------------
-- 1.  VList -- segmented list. Each segment doubles in capacity. Within a segment elements are contiguous (in a real impl: an unboxed array). Here [[a]] simulates the segment chain; the real runtime would use ByteArray# or similar.
-- ---------------------------------------------------------------------------

-- | Segment chain: newest segment first. Within each segment, newest element first (i.e. each segment is itself a reversed sub-list). The targetSize field records the capacity of the current head segment; it doubles each time we spill over into a new segment.
data VList a = VList {segments :: [[a]], targetSize :: Int} deriving (Eq)

instance (Show a) => Show (VList a) where show vl = "VList " ++ show (vlistToList vl)

instance Functor VList where fmap f (VList segs tgt) = VList (map (map f) segs) tgt

-- Oldest segment last in `segments`; within each segment oldest element is at the tail. foldr over the logical sequence = foldr segs-reversed, then within each segment reversed.
instance Foldable VList where foldr f z (VList segs _) = foldr (flip (foldr f)) z segs

-- | O(1) amortised cons. Prepend into the head segment when there is still room; otherwise spill into a new segment of double the target size.
vcons :: a -> VList a -> VList a
vcons x (VList [] _) = VList [[x]] 2
vcons x (VList (h : t) tgt) | length h < tgt = VList ((x : h) : t) tgt
vcons x (VList (h : t) tgt) = VList ([x] : h : t) (tgt * 2)

vnil = VList [] 1

fromListV = foldr vcons vnil

-- | Recover insertion order. `foldr vcons vnil [a,b,c]` = `vcons a (vcons b (vcons c vnil))`, so `a` ends up as the head of the head segment. `concatMap id segments` therefore yields [a, b, c] directly (head segment first, head element first).
vlistToList (VList segs _) = concatMap id segs

vlength :: VList a -> Int
vlength (VList segs _) = sum (map length segs)

-- | Map preserving segment structure (each segment is an independent SIMD lane).
vmapSegs :: (a -> b) -> VList a -> VList b
vmapSegs f (VList segs tgt) = VList (map (map f) segs) tgt

vfilter :: (a -> Bool) -> VList a -> VList a
vfilter p vl = fromListV . filter p . vlistToList $ vl

vzipWith :: (a -> b -> c) -> VList a -> VList b -> VList c
vzipWith f va vb = fromListV $ zipWith f (vlistToList va) (vlistToList vb)

-- ---------------------------------------------------------------------------
-- 2.  FPR_Record typeclass. The associated type `SoA a` is the columnar representation of [a]. The compiler would derive instances automatically from record definitions.
-- ---------------------------------------------------------------------------

class FPR_Record a where
  type SoA a

  -- | Build an SoA from a plain list
  toSoA :: [a] -> SoA a

  -- | Convert SoA back to a list (for inspection / interop)
  fromSoA :: SoA a -> [a]

  -- | The empty SoA (nil). Proxy is required because SoA is a non-injective type family -- the compiler knows the concrete type at every call site.
  emptySoA :: Proxy a -> SoA a

  -- | Cons one record into the SoA, fanning out to each column.
  consSoA :: a -> SoA a -> SoA a

  -- | Map: default goes through list; instances override with column splits.
  mapSoA :: (a -> a) -> SoA a -> SoA a
  mapSoA f = toSoA . map f . fromSoA

  -- | Filter, keeping all columns in sync.
  filterSoA :: (a -> Bool) -> SoA a -> SoA a
  filterSoA p = toSoA . filter p . fromSoA

  -- | ZipWith two equal-length SoAs.
  zipWithSoA :: (a -> a -> a) -> SoA a -> SoA a -> SoA a
  zipWithSoA f sa sb = toSoA $ zipWith f (fromSoA sa) (fromSoA sb)

-- ---------------------------------------------------------------------------
-- 3.  ColUpdate -- an explicit per-field update. The PE analyses a record lambda and emits one ColUpdate per field. Independent updates can be lowered to parallel SIMD column passes.
-- ---------------------------------------------------------------------------

-- | A lens into one column of the SoA.
data Field rec col = Field {getCol :: SoA rec -> VList col, setCol :: VList col -> SoA rec -> SoA rec}

-- | A single-field update (existential over col).
data ColUpdate rec where ColUpdate :: Field rec col -> (col -> col) -> ColUpdate rec

-- | Apply a list of independent ColUpdates. Each touches exactly one column; order is irrelevant when fields are  mutually independent (the PE guarantees this statically).
applyUpdates :: [ColUpdate rec] -> SoA rec -> SoA rec
applyUpdates updates soa = foldr step soa updates where step (ColUpdate field f) s = setCol field (vmapSegs f (getCol field s)) s

-- | Map a single column directly.
mapColumn :: Field rec col -> (col -> col) -> SoA rec -> SoA rec
mapColumn field f soa = setCol field (vmapSegs f (getCol field soa)) soa

-- ---------------------------------------------------------------------------
-- 4.  Example record: Student
-- ---------------------------------------------------------------------------

data Student = Student {studentName :: String, studentAge :: Int} deriving (Eq)

instance Show Student where show s = "Student{name=" ++ studentName s ++ ", age=" ++ show (studentAge s) ++ "}"

-- | SoA layout for List Student. The compiler derives this automatically from the record definition.
data StudentSoA = StudentSoA {nameCol :: VList String, ageCol :: VList Int} deriving (Eq)

instance Show StudentSoA where show s = "StudentSoA\n  names: " ++ show (nameCol s) ++ "\n  ages : " ++ show (ageCol s)

instance FPR_Record Student where
  type SoA Student = StudentSoA

  toSoA ss = StudentSoA {nameCol = fromListV (map studentName ss), ageCol = fromListV (map studentAge ss)}

  fromSoA (StudentSoA ns as) = zipWith Student (vlistToList ns) (vlistToList as)

  emptySoA _ = StudentSoA vnil vnil

  -- | cons fans out to each column simultaneously.
  consSoA s soa = StudentSoA {nameCol = vcons (studentName s) (nameCol soa), ageCol = vcons (studentAge s) (ageCol soa)}

  -- | Column-split map. The PE analyses the lambda AST and finds that the two field updates are independent: new name depends only on old name, new age only on old age. It emits two separate column passes, each a SIMD candidate. The trick: we map each column by reconstructing a minimal dummy record containing only that field's value, apply f, then project the result. The PE does this symbolically (never evaluates f); here it is explicit. Note: this is ONLY correct when fields are independent. The PE verifies  this statically before emitting the split.
  mapSoA f soa = StudentSoA {nameCol = vmapSegs (\n -> studentName (f (Student n 0))) (nameCol soa), ageCol = vmapSegs (\a -> studentAge (f (Student "" a))) (ageCol soa)}

  filterSoA p (StudentSoA ns as) = let pairs = zip (vlistToList ns) (vlistToList as); kept = filter (uncurry (\n a -> p (Student n a))) pairs; (ns', as') = unzip kept in StudentSoA (fromListV ns') (fromListV as')

-- | Student field lenses
nameField :: Field Student String
nameField = Field nameCol (\v s -> s {nameCol = v})

ageField :: Field Student Int
ageField = Field ageCol (\v s -> s {ageCol = v})

-- ---------------------------------------------------------------------------
-- 5.  List operations (the FP-RISC standard library interface on SoA)
-- ---------------------------------------------------------------------------

-- | FP-RISC `::` cons operator for List FPR_Record
(.:) :: (FPR_Record a) => a -> SoA a -> SoA a
(.:) = consSoA

infixr 5 .:

-- | Build an SoA from a list literal. Simulates:  students = [alice, bob] desugaring: alice :: bob :: nil
listLiteral :: (FPR_Record a) => Proxy a -> [a] -> SoA a
listLiteral p = foldr consSoA (emptySoA p)

listMap :: (FPR_Record a) => (a -> a) -> SoA a -> SoA a
listMap = mapSoA

listFilter :: (FPR_Record a) => (a -> Bool) -> SoA a -> SoA a
listFilter = filterSoA

listZipWith :: (FPR_Record a) => (a -> a -> a) -> SoA a -> SoA a -> SoA a
listZipWith = zipWithSoA

listFoldl :: (FPR_Record a) => (b -> a -> b) -> b -> SoA a -> b
listFoldl f z = foldl f z . fromSoA

-- ---------------------------------------------------------------------------
-- 6.  FprNum -- numeric type as a record (analogous to FP-RISC's Num) Demonstrates that VList Num expands to three contiguous float columns. Interval arithmetic then operates as three parallel vector ops.
-- ---------------------------------------------------------------------------

-- error interval low bound and error interval high bound
data FprNum = FprNum {numValue :: Double, numErrLow :: Double, numErrHigh :: Double} deriving (Eq)

instance Show FprNum where show n = show (numValue n) ++ " +/-[" ++ show (numErrLow n) ++ ", " ++ show (numErrHigh n) ++ "]"

data FprNumSoA = FprNumSoA {numValueCol :: VList Double, numErrLowCol :: VList Double, numErrHighCol :: VList Double} deriving (Eq)

instance Show FprNumSoA where show s = "FprNumSoA\n  values : " ++ show (numValueCol s) ++ "\n  errLow : " ++ show (numErrLowCol s) ++ "\n  errHigh: " ++ show (numErrHighCol s)

instance FPR_Record FprNum where
  type SoA FprNum = FprNumSoA

  toSoA ns = FprNumSoA {numValueCol = fromListV (map numValue ns), numErrLowCol = fromListV (map numErrLow ns), numErrHighCol = fromListV (map numErrHigh ns)}

  fromSoA (FprNumSoA vs ls hs) = zipWith3 FprNum (vlistToList vs) (vlistToList ls) (vlistToList hs)

  emptySoA _ = FprNumSoA vnil vnil vnil

  consSoA n soa = FprNumSoA {numValueCol = vcons (numValue n) (numValueCol soa), numErrLowCol = vcons (numErrLow n) (numErrLowCol soa), numErrHighCol = vcons (numErrHigh n) (numErrHighCol soa)}

-- | Interval arithmetic addition. Lowers to three independent SIMD vector adds in the real runtime.
addNumSoA :: FprNumSoA -> FprNumSoA -> FprNumSoA
addNumSoA a b = FprNumSoA {numValueCol = vzipWith (+) (numValueCol a) (numValueCol b), numErrLowCol = vzipWith (+) (numErrLowCol a) (numErrLowCol b), numErrHighCol = vzipWith (+) (numErrHighCol a) (numErrHighCol b)}

-- ---------------------------------------------------------------------------
-- 7.  Demo
-- ---------------------------------------------------------------------------

sep = putStrLn (replicate 60 '-')

main = do
  putStrLn "=== FP-RISC SoA VList Simulation ==="
  putStrLn ""

  -- alice = Student {name="Alice", age=20}.  bob   = Student {name="Bob",   age=20}.
  let alice = Student "Alice" 20; bob = Student "Bob" 20; charlie = Student "Charlie" 22

  -- students : List Student. students = [alice, bob]. desugars to: alice :: bob :: nil
  let students = alice .: bob .: emptySoA (Proxy :: Proxy Student)

  sep
  putStrLn "Initial students SoA  (alice :: bob :: nil)"
  print students

  sep
  putStrLn "cons: charlie .: students"
  print (charlie .: students)

  sep
  -- f = List.map (s => {name = "Dr "++name, age = age+1}) students. PE splits into two independent column maps: mapColumn nameField ("Dr " ++) and mapColumn ageField (+1)
  putStrLn "List.map (name => \"Dr \"++name,  age => age+1)"
  putStrLn "  [PE splits this into two independent column passes]"
  let promoted = listMap (\s -> s {studentName = "Dr " ++ studentName s, studentAge = studentAge s + 1}) students
  print promoted

  -- Correctness check: column-split result == naive record-level map
  let naivePromoted = toSoA . map (\s -> s {studentName = "Dr " ++ studentName s, studentAge = studentAge s + 1}) . fromSoA $ students
  sep
  putStrLn "Correctness: column-split == naive record map?"
  putStrLn $ if (fromSoA promoted :: [Student]) == fromSoA naivePromoted then "  PASS" else "  FAIL"

  sep
  putStrLn "List.filter (age > 20) on promoted list"
  let filtered = listFilter (\s -> studentAge s > 20) promoted
  print filtered

  sep
  putStrLn "List.foldl summing total age"
  let totalAge = listFoldl (\acc s -> acc + studentAge s) 0 students
  putStrLn $ "  total age = " ++ show totalAge

  sep
  putStrLn "List.zipWith (add ages) students students"
  let doubled = listZipWith (\a b -> a {studentAge = studentAge a + studentAge b}) students students
  print doubled

  sep
  putStrLn "FprNum SoA  (Num-as-record => 3 contiguous float columns)"
  let nums = listLiteral (Proxy :: Proxy FprNum) [FprNum 1.0 0.01 0.01, FprNum 2.0 0.02 0.02, FprNum 3.0 0.03 0.03]
  print nums

  sep
  putStrLn "addNumSoA nums nums  (3 parallel vector adds, SIMD-ready)"
  let added = addNumSoA nums nums
  print added
  putStrLn "  as records:"
  mapM_ (putStrLn . ("  " ++) . show) (fromSoA added :: [FprNum])

  sep
  putStrLn "Done."
