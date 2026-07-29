{-# LANGUAGE GADTs, ExistentialQuantification, ScopedTypeVariables, LambdaCase, TypeOperators #-}
-- FPRLive client model.
--
-- Server sends:  a STATIC BATCH (tree + schema + untyped compute exprs)
--                then a stream of DELTAS (cell writes + subtree patches).
-- Client does:   typecheck once -> lower to its own ISA once -> cache by hash,
--                then on each delta re-execute ONLY the dirty binds.
--
-- Reading order:
--   Part 1  wire vocabulary (untyped, what crosses the network)
--   Part 2  typed AST + checker      (the "typed AST" the question is about)
--   Part 3  static tree + deltas
--   Part 4  lowering to a RISC-like ISA + assembler + encoder
--   Part 5  the client machine (executes the lowered code directly)
--   Part 6  client obligations: deps, subscriptions, dirty sets, effects, lifetimes
--   Part 7  demo

module FPRLive (main) where

import           Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import           Data.Int  (Int32)
import           Data.List (intercalate, foldl', nub, sort)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import           Data.Maybe (fromMaybe)
import           Data.Type.Equality ((:~:)(Refl))
import           Data.Word (Word32)
import           Text.Printf (printf)

--------------------------------------------------------------------------------
-- Part 1. Wire vocabulary. Untyped, tagged, tiny. This is what is serialized.
--------------------------------------------------------------------------------

data Ty = TInt | TBool | TStr deriving (Eq, Show)

data BinOp = OAdd | OSub | OMul | OLt | OEq | OAnd | OOr | OCat
  deriving (Eq, Show)
data UnOp  = ONot | OI2S
  deriving (Eq, Show)

-- Compute expression as it arrives. Cells referenced by *name*; the client
-- links names to slots at compile time (like a relocation pass).
data RExpr
  = RI Int32
  | RB Bool
  | RS String
  | RCell String
  | RBin BinOp RExpr RExpr
  | RUn  UnOp  RExpr
  | RIf  RExpr RExpr RExpr
  deriving (Eq, Show)

-- Content address of a compute. Same expression from any node in any batch
-- hits the same compiled code object. (FNV-1a over the wire form.)
newtype Hash = Hash Word32 deriving (Eq, Ord)
instance Show Hash where show (Hash w) = printf "%08x" w

hashOf :: RExpr -> Hash
hashOf = Hash . foldl' step 2166136261 . show
  where step h c = (h `xor` fromIntegral (fromEnum c)) * 16777619

--------------------------------------------------------------------------------
-- Part 2. Typed AST. The client checks the wire form ONCE against the schema.
--         Everything downstream (deps, lowering, result decoding) consumes this.
--------------------------------------------------------------------------------

type Slot = Int                      -- reactive cell address, post-link
type Schema = M.Map String (Slot, Ty)

data STy a where
  SInt  :: STy Int32
  SBool :: STy Bool
  SStr  :: STy String

instance Show (STy a) where
  show SInt = "Int"; show SBool = "Bool"; show SStr = "Str"

eqTy :: STy a -> STy b -> Maybe (a :~: b)
eqTy SInt  SInt  = Just Refl
eqTy SBool SBool = Just Refl
eqTy SStr  SStr  = Just Refl
eqTy _     _     = Nothing

data Expr a where
  LitI  :: Int32                                  -> Expr Int32
  LitB  :: Bool                                   -> Expr Bool
  LitS  :: String                                 -> Expr String
  Cell  :: Slot -> STy a                           -> Expr a
  Add   :: Expr Int32  -> Expr Int32  -> Expr Int32
  Sub   :: Expr Int32  -> Expr Int32  -> Expr Int32
  Mul   :: Expr Int32  -> Expr Int32  -> Expr Int32
  Lt    :: Expr Int32  -> Expr Int32  -> Expr Bool
  Eq    :: Expr Int32  -> Expr Int32  -> Expr Bool
  And   :: Expr Bool   -> Expr Bool   -> Expr Bool
  Or    :: Expr Bool   -> Expr Bool   -> Expr Bool
  Not   :: Expr Bool                  -> Expr Bool
  Cat   :: Expr String -> Expr String -> Expr String
  I2S   :: Expr Int32                 -> Expr String
  If    :: Expr Bool   -> Expr a      -> Expr a -> Expr a

-- Type erased once checked; the STy witness is what lets the driver decode the
-- single machine word the program returns.
data Typed = forall a. Typed (STy a) (Expr a)

check :: Schema -> RExpr -> Either String Typed
check env = go
 where
  go = \case
    RI n  -> Right (Typed SInt  (LitI n))
    RB b  -> Right (Typed SBool (LitB b))
    RS s  -> Right (Typed SStr  (LitS s))
    RCell n -> case M.lookup n env of
      Nothing        -> Left ("unbound cell: " ++ n)
      Just (s, TInt) -> Right (Typed SInt  (Cell s SInt))
      Just (s, TBool)-> Right (Typed SBool (Cell s SBool))
      Just (s, TStr) -> Right (Typed SStr  (Cell s SStr))
    RUn ONot e -> do
      Typed t x <- go e
      case t of SBool -> Right (Typed SBool (Not x))
                _     -> Left ("not: expected Bool, got " ++ show t)
    RUn OI2S e -> do
      Typed t x <- go e
      case t of SInt -> Right (Typed SStr (I2S x))
                _    -> Left ("i2s: expected Int, got " ++ show t)
    RBin op a b -> do
      Typed ta xa <- go a
      Typed tb xb <- go b
      let arith f = case (ta, tb) of
            (SInt, SInt) -> Right (Typed SInt (f xa xb))
            _ -> Left (show op ++ ": expected Int/Int, got "
                       ++ show ta ++ "/" ++ show tb)
          cmp f = case (ta, tb) of
            (SInt, SInt) -> Right (Typed SBool (f xa xb))
            _ -> Left (show op ++ ": expected Int/Int")
          logi f = case (ta, tb) of
            (SBool, SBool) -> Right (Typed SBool (f xa xb))
            _ -> Left (show op ++ ": expected Bool/Bool")
      case op of
        OAdd -> arith Add; OSub -> arith Sub; OMul -> arith Mul
        OLt  -> cmp Lt;    OEq  -> cmp Eq
        OAnd -> logi And;  OOr  -> logi Or
        OCat -> case (ta, tb) of
          (SStr, SStr) -> Right (Typed SStr (Cat xa xb))
          _ -> Left "cat: expected Str/Str"
    RIf c t e -> do
      Typed tc xc <- go c
      Typed tt xt <- go t
      Typed te xe <- go e
      case tc of
        SBool -> case eqTy tt te of
          Just Refl -> Right (Typed tt (If xc xt xe))
          Nothing   -> Left ("if: branch mismatch "
                             ++ show tt ++ " vs " ++ show te)
        _ -> Left "if: condition must be Bool"

-- Dependencies, straight off the typed tree. Computed once, at compile time.
deps :: Expr a -> [Slot]
deps = sort . nub . go
 where
  go :: Expr b -> [Slot]
  go = \case
    Cell s _ -> [s]
    Add a b -> go a ++ go b; Sub a b -> go a ++ go b; Mul a b -> go a ++ go b
    Lt  a b -> go a ++ go b; Eq  a b -> go a ++ go b
    And a b -> go a ++ go b; Or  a b -> go a ++ go b
    Cat a b -> go a ++ go b
    Not a -> go a; I2S a -> go a
    If c t e -> go c ++ go t ++ go e
    _ -> []

--------------------------------------------------------------------------------
-- Part 3. Static tree and deltas.
--------------------------------------------------------------------------------

type Path   = [Int]
type BindId = Int

-- The Alpine-shaped obligations: what a computed value is *for*.
data Target = TgText | TgAttr String | TgClass | TgShow deriving (Eq, Show)

data Bind = Bind { bTarget :: Target, bExpr :: RExpr } deriving Show

data Node = Node
  { nTag   :: String
  , nAttrs :: M.Map String String     -- fully static; never re-evaluated
  , nBinds :: [Bind]                  -- dynamic obligations on this node
  , nKids  :: [Node]
  } deriving Show

data StaticBatch = StaticBatch
  { sbSchema :: Schema
  , sbInit   :: M.Map String Val      -- initial cell values, by name
  , sbRoot   :: Node
  }

data Delta
  = DSet   String Val                 -- reactive write (event handler or push)
  | DInsert Path Int Node             -- subtree arrives: compile + subscribe
  | DRemove Path Int                  -- subtree leaves: unsubscribe or leak
  deriving Show

data Val = VI Int32 | VB Bool | VS String deriving Eq
instance Show Val where
  show (VI n) = show n; show (VB b) = show b; show (VS s) = show s

--------------------------------------------------------------------------------
-- Part 4. Lowering. Types are erased here; a compute becomes a straight-line
--         register program in the client's own ISA.
--------------------------------------------------------------------------------

newtype Reg = R Int deriving Eq
instance Show Reg where show (R n) = "r" ++ show n

type Label = Int

data Instr
  = LI   Reg Int32            -- load immediate (may need 2 words, see encode)
  | LCON Reg Int              -- load const-pool handle (strings)
  | LCEL Reg Slot             -- load reactive cell
  | MOV  Reg Reg
  | ADD  Reg Reg Reg
  | SUB  Reg Reg Reg
  | MUL  Reg Reg Reg
  | SLT  Reg Reg Reg          -- set-less-than
  | SEQ  Reg Reg Reg
  | NOT  Reg Reg
  | CAT  Reg Reg Reg          -- builtin: arena string concat
  | ITOA Reg Reg              -- builtin: int -> arena string
  | BEQZ Reg Label
  | BNEZ Reg Label
  | JMP  Label
  | LBL  Label                -- pseudo; removed by the assembler
  | RET  Reg
  deriving Show

-- Tiny hand-rolled codegen monad (no mtl dependency).
data GS = GS { gsMax :: !Int, gsLbl :: !Int, gsOut :: [Instr], gsPool :: [String] }
newtype Gen a = Gen { runGen :: GS -> (a, GS) }
instance Functor Gen where fmap f (Gen g) = Gen (\s -> let (a,s') = g s in (f a, s'))
instance Applicative Gen where
  pure a = Gen (\s -> (a, s))
  Gen f <*> Gen x = Gen (\s -> let (g,s1) = f s; (a,s2) = x s1 in (g a, s2))
instance Monad Gen where
  Gen m >>= k = Gen (\s -> let (a,s') = m s in runGen (k a) s')

emit :: Instr -> Gen ()
emit i = Gen (\s -> ((), s { gsOut = i : gsOut s }))

note :: Int -> Gen ()
note d = Gen (\s -> ((), s { gsMax = max (gsMax s) d }))

newLbl :: Gen Label
newLbl = Gen (\s -> (gsLbl s, s { gsLbl = gsLbl s + 1 }))

intern :: String -> Gen Int
intern str = Gen (\s ->
  let pool = reverse (gsPool s)
  in case lookup str (zip pool [0..]) of
       Just i  -> (i, s)
       Nothing -> (length pool, s { gsPool = str : gsPool s }))

-- Result lands in register d; d, d+1, ... are scratch. Naive but bounded:
-- max register = tree depth, which the compiler reports so the client can
-- refuse a compute that would exceed its register file rather than spill.
lowerE :: Int -> Expr a -> Gen ()
lowerE d = \case
  LitI n   -> note d >> emit (LI (R d) n)
  LitB b   -> note d >> emit (LI (R d) (if b then 1 else 0))
  LitS s   -> do { i <- intern s; note d; emit (LCON (R d) i) }
  Cell s _ -> note d >> emit (LCEL (R d) s)
  Add a b  -> bin ADD a b
  Sub a b  -> bin SUB a b
  Mul a b  -> bin MUL a b
  Lt  a b  -> bin SLT a b
  Eq  a b  -> bin SEQ a b
  Cat a b  -> bin CAT a b
  Not a    -> lowerE d a >> emit (NOT (R d) (R d))
  I2S a    -> lowerE d a >> emit (ITOA (R d) (R d))
  -- short-circuit, like a real backend: cheap paths stay cheap in fuel terms
  And a b  -> do l <- newLbl
                 lowerE d a; emit (BEQZ (R d) l); lowerE d b; emit (LBL l)
  Or  a b  -> do l <- newLbl
                 lowerE d a; emit (BNEZ (R d) l); lowerE d b; emit (LBL l)
  If c t e -> do lt' <- newLbl; le <- newLbl
                 lowerE d c
                 emit (BEQZ (R d) lt')
                 lowerE d t; emit (JMP le)
                 emit (LBL lt')
                 lowerE d e
                 emit (LBL le)
 where
  bin :: (Reg -> Reg -> Reg -> Instr) -> Expr x -> Expr y -> Gen ()
  bin op a b = do lowerE d a; lowerE (d+1) b; note (d+1)
                  emit (op (R d) (R d) (R (d+1)))

-- A compiled code object: what actually lives in the client's code cache.
data Code = Code
  { cInstrs :: IM.IntMap Instr   -- label-free, pc-indexed
  , cPool   :: [String]          -- string constants, interned
  , cRegs   :: Int               -- frame size (register high-water mark)
  , cWords  :: [Word32]          -- encoded image, what you'd memcpy to iRAM
  , cTy     :: Ty                -- how to decode the returned word
  , cDeps   :: [Slot]
  }

compile :: Typed -> Code
compile (Typed sty e) =
  let (_, st) = runGen (lowerE 0 e >> emit (RET (R 0)))
                       (GS 0 0 [] [])
      raw     = reverse (gsOut st)
      resolved = resolveLabels raw
  in Code { cInstrs = IM.fromList (zip [0..] resolved)
          , cPool   = reverse (gsPool st)
          , cRegs   = gsMax st + 1
          , cWords  = concatMap encode resolved
          , cTy     = case sty of SInt -> TInt; SBool -> TBool; SStr -> TStr
          , cDeps   = deps e
          }

-- Drop LBL pseudo-instructions, rewrite branch targets to pc.
resolveLabels :: [Instr] -> [Instr]
resolveLabels is = map fix (filter notLbl is)
 where
  notLbl (LBL _) = False; notLbl _ = True
  table = go 0 is where
    go _ []          = M.empty
    go pc (LBL l:xs) = M.insert l pc (go pc xs)
    go pc (_:xs)     = go (pc+1) xs
  at l = fromMaybe (error "bad label") (M.lookup l table)
  fix = \case
    BEQZ r l -> BEQZ r (at l); BNEZ r l -> BNEZ r (at l); JMP l -> JMP (at l)
    i -> i

-- Fixed-width encoding, op:6 | rd:5 | rs1:5 | rs2:5 | imm:11.
-- LI with a wide immediate honestly costs two words (LUI-style pair).
encode :: Instr -> [Word32]
encode = \case
  LI (R d) n
    | n >= -1024 && n < 1024 -> [w 1 d 0 0 (fromIntegral n)]
    | otherwise -> [ w 2 d 0 0 (fromIntegral (n `shiftR` 11) .&. 0x7ff)
                   , w 3 d d 0 (fromIntegral n .&. 0x7ff) ]
  LCON (R d) i     -> [w 4 d 0 0 (fromIntegral i)]
  LCEL (R d) s     -> [w 5 d 0 0 (fromIntegral s)]
  MOV (R d) (R a)  -> [w 6 d a 0 0]
  ADD (R d)(R a)(R b) -> [w 7 d a b 0]
  SUB (R d)(R a)(R b) -> [w 8 d a b 0]
  MUL (R d)(R a)(R b) -> [w 9 d a b 0]
  SLT (R d)(R a)(R b) -> [w 10 d a b 0]
  SEQ (R d)(R a)(R b) -> [w 11 d a b 0]
  NOT (R d)(R a)      -> [w 12 d a 0 0]
  CAT (R d)(R a)(R b) -> [w 13 d a b 0]
  ITOA (R d)(R a)     -> [w 14 d a 0 0]
  BEQZ (R a) t        -> [w 15 0 a 0 (fromIntegral t)]
  BNEZ (R a) t        -> [w 16 0 a 0 (fromIntegral t)]
  JMP t               -> [w 17 0 0 0 (fromIntegral t)]
  RET (R a)           -> [w 18 0 a 0 0]
  LBL _               -> []
 where
  w :: Word32 -> Int -> Int -> Int -> Word32 -> Word32
  w op d a b imm = (op `shiftL` 26)
               .|. (fromIntegral d `shiftL` 21)
               .|. (fromIntegral a `shiftL` 16)
               .|. (fromIntegral b `shiftL` 11)
               .|. (imm .&. 0x7ff)

--------------------------------------------------------------------------------
-- Part 5. The machine. Registers hold raw words. A Str value is a handle into
--         the string arena; the arena is a bump region reset every frame.
--------------------------------------------------------------------------------

data Mach = Mach
  { mRegs  :: IM.IntMap Int32
  , mCells :: IM.IntMap Int32       -- slot -> word (Str cells hold handles)
  , mArena :: IM.IntMap String      -- handle -> string
  , mNext  :: Int                   -- bump pointer
  , mFuel  :: Int                   -- instructions retired
  }

exec :: Mach -> Code -> (Int32, Mach)
exec m0 code = go 0 (seedPool m0)
 where
  base = mNext m0
  seedPool m = m { mArena = IM.union (mArena m)
                     (IM.fromList (zip [base ..] (cPool code)))
                 , mNext  = base + length (cPool code) }
  rd r (R i) = IM.findWithDefault 0 i r
  step m = m { mFuel = mFuel m + 1 }
  go pc m = case IM.lookup pc (cInstrs code) of
    Nothing -> error "pc out of range"
    Just i  -> let m' = step m in case i of
      RET a          -> (rd (mRegs m') a, m')
      LI (R d) n     -> nxt pc (set d n m')
      LCON (R d) k   -> nxt pc (set d (fromIntegral (base + k)) m')
      LCEL (R d) s   -> nxt pc (set d (IM.findWithDefault 0 s (mCells m')) m')
      MOV (R d) a    -> nxt pc (set d (rd (mRegs m') a) m')
      ADD (R d) a b  -> arith pc m' d (+) a b
      SUB (R d) a b  -> arith pc m' d (-) a b
      MUL (R d) a b  -> arith pc m' d (*) a b
      SLT (R d) a b  -> arith pc m' d (\x y -> b2w (x < y)) a b
      SEQ (R d) a b  -> arith pc m' d (\x y -> b2w (x == y)) a b
      NOT (R d) a    -> nxt pc (set d (b2w (rd (mRegs m') a == 0)) m')
      ITOA (R d) a   -> let s = show (rd (mRegs m') a)
                            (h, m'') = alloc s m'
                        in nxt pc (set d h m'')
      CAT (R d) a b  -> let s = str m' (rd (mRegs m') a)
                             ++ str m' (rd (mRegs m') b)
                            (h, m'') = alloc s m'
                        in nxt pc (set d h m'')
      BEQZ a t       -> if rd (mRegs m') a == 0 then go t m' else nxt pc m'
      BNEZ a t       -> if rd (mRegs m') a /= 0 then go t m' else nxt pc m'
      JMP t          -> go t m'
      LBL _          -> nxt pc m'
   where
    nxt p m' = go (p + 1) m'
    set d v m' = m' { mRegs = IM.insert d v (mRegs m') }
    arith p m' d f a b = nxt p (set d (f (rd (mRegs m') a) (rd (mRegs m') b)) m')

  b2w c = if c then 1 else 0
  alloc s m = ( fromIntegral (mNext m)
              , m { mArena = IM.insert (mNext m) s (mArena m)
                  , mNext  = mNext m + 1 } )

str :: Mach -> Int32 -> String
str m h = IM.findWithDefault "" (fromIntegral h) (mArena m)

-- The typed AST's payoff: the ISA is untyped, but cTy says how to read the word.
decode :: Mach -> Ty -> Int32 -> Val
decode _ TInt  w = VI w
decode _ TBool w = VB (w /= 0)
decode m TStr  w = VS (str m w)

--------------------------------------------------------------------------------
-- Part 6. Client obligations. This is the part Alpine hides behind proxies and
--         the part an embedded client has to do explicitly.
--------------------------------------------------------------------------------

data Client = Client
  { clSchema :: Schema
  , clMach   :: Mach
  , clCache  :: M.Map Hash Code        -- obligation: content-addressed code cache
  , clBinds  :: IM.IntMap (Path, Target, Hash)
  , clSubs   :: IM.IntMap [BindId]     -- obligation: slot -> dependents index
  , clOut    :: IM.IntMap Val          -- last computed value per bind
  , clTree   :: Node
  , clNextB  :: BindId
  , clLog    :: [String]
  , clRuns   :: Int          -- binds actually re-executed
  , clWould  :: Int          -- binds a naive full re-eval would have run
  }

say :: String -> Client -> Client
say s c = c { clLog = s : clLog c }

-- Obligation 1: compile-or-reuse. Identical computes across nodes/batches
-- share one code object.
install :: Path -> Bind -> Client -> Client
install path (Bind tgt rex) c =
  let h = hashOf rex
  in case M.lookup h (clCache c) of
       Just code -> attach h code (say ("    cache hit  " ++ show h) c)
       Nothing   -> case check (clSchema c) rex of
         Left err   -> say ("    TYPE ERROR " ++ err ++ " -- bind rejected") c
         Right typed ->
           let code = compile typed
               msg  = printf "    compiled %s  %d instr / %d words / %d regs / deps %s"
                        (show h) (IM.size (cInstrs code))
                        (length (cWords code)) (cRegs code) (show (cDeps code))
           in attach h code (say msg c { clCache = M.insert h code (clCache c) })
 where
  attach h code cl =
    let bid = clNextB cl
        subs = foldl' (\s sl -> IM.insertWith (++) sl [bid] s) (clSubs cl) (cDeps code)
    in cl { clBinds  = IM.insert bid (path, bTarget (Bind tgt rex), h) (clBinds cl)
          , clSubs   = subs
          , clNextB  = bid + 1 }

-- Obligation 2: walk the static tree once, installing every bind.
installTree :: Path -> Node -> Client -> Client
installTree path n c0 =
  let c1 = foldl' (\c b -> install path b c) c0 (nBinds n)
  in foldl' (\c (i,k) -> installTree (path ++ [i]) k c) c1 (zip [0..] (nKids n))

-- Obligation 3: run a set of binds, apply their effects, account the fuel.
runBinds :: [BindId] -> Client -> Client
runBinds bids c0 = foldl' one c0 bids
 where
  one c bid = case IM.lookup bid (clBinds c) of
    Nothing -> c
    Just (path, tgt, h) -> case M.lookup h (clCache c) of
      Nothing -> c
      Just code ->
        let f0 = mFuel (clMach c)
            (w, m') = exec (clMach c) code
            v = decode m' (cTy code) w
            fuel = mFuel m' - f0
        in say (printf "    b%-2d %-22s %-18s = %-34s [%d fuel]"
                  bid (showPath path) (show tgt) (show v) fuel)
             c { clMach = m', clOut = IM.insert bid v (clOut c) }

showPath :: Path -> String
showPath [] = "/"
showPath p  = concatMap (('/':) . show) p

-- Obligation 4: reactive write -> dirty set -> re-run only what depends on it.
-- (Single pass is sound here because computes read cells only. If derived cells
-- were allowed, this is where a topological order would be required to avoid
-- glitches -- that is the cost of that feature, paid exactly once, here.)
applyDelta :: Delta -> Client -> Client
applyDelta d c = case d of
  DSet name v -> case M.lookup name (clSchema c) of
    Nothing -> say ("  ! unknown cell " ++ name) c
    Just (slot, ty)
      | not (tyOK ty v) -> say ("  ! type mismatch writing " ++ name) c
      | otherwise ->
          let (w, m') = store v (clMach c)
              dirty   = IM.findWithDefault [] slot (clSubs c)
              c'      = c { clMach = m' { mCells = IM.insert slot w (mCells m') } }
              hdr     = printf "  SET %s = %s -> dirty %s (of %d binds)"
                          name (show v) (show dirty) (IM.size (clBinds c))
              c'' = c' { clRuns  = clRuns c' + length dirty
                       , clWould = clWould c' + IM.size (clBinds c') }
          in runBinds dirty (say hdr c'')
  DInsert path ix n ->
    let c1 = say (printf "  INSERT at %s[%d] <%s>" (showPath path) ix (nTag n)) c
        c2 = c1 { clTree = insertAt path ix n (clTree c1) }
        before = clNextB c2
        c3 = installTree (path ++ [ix]) n c2
        new = [before .. clNextB c3 - 1]
    in runBinds new c3
  DRemove path ix ->
    let target = path ++ [ix]
        gone = [ b | (b,(p,_,_)) <- IM.toList (clBinds c), target `isPrefix` p ]
        c1 = say (printf "  REMOVE %s -> unsubscribing binds %s" (showPath target) (show gone)) c
    in c1 { clTree  = removeAt path ix (clTree c1)
          , clBinds = foldl' (flip IM.delete) (clBinds c1) gone
          , clOut   = foldl' (flip IM.delete) (clOut c1) gone
          , clSubs  = IM.map (filter (`notElem` gone)) (clSubs c1) }
 where
  tyOK TInt (VI _) = True; tyOK TBool (VB _) = True; tyOK TStr (VS _) = True
  tyOK _ _ = False

isPrefix :: Path -> Path -> Bool
isPrefix a b = take (length a) b == a

store :: Val -> Mach -> (Int32, Mach)
store (VI n) m = (n, m)
store (VB b) m = (if b then 1 else 0, m)
store (VS s) m = ( fromIntegral (mNext m)
                 , m { mArena = IM.insert (mNext m) s (mArena m)
                     , mNext = mNext m + 1 } )

insertAt :: Path -> Int -> Node -> Node -> Node
insertAt [] ix n root = root { nKids = let (a,b) = splitAt ix (nKids root) in a ++ [n] ++ b }
insertAt (i:is) ix n root =
  root { nKids = [ if j == i then insertAt is ix n k else k
                 | (j,k) <- zip [0..] (nKids root) ] }

removeAt :: Path -> Int -> Node -> Node
removeAt [] ix root = root { nKids = [ k | (j,k) <- zip [0..] (nKids root), j /= ix ] }
removeAt (i:is) ix root =
  root { nKids = [ if j == i then removeAt is ix k else k
                 | (j,k) <- zip [0..] (nKids root) ] }

boot :: StaticBatch -> Client
boot sb =
  let slots = [ (sl, v) | (nm,(sl,_)) <- M.toList (sbSchema sb)
                        , Just v <- [M.lookup nm (sbInit sb)] ]
      m0 = Mach IM.empty IM.empty IM.empty 1 0
      m1 = foldl' (\m (sl,v) -> let (w,m') = store v m
                                in m' { mCells = IM.insert sl w (mCells m') }) m0 slots
      c0 = Client (sbSchema sb) m1 M.empty IM.empty IM.empty IM.empty
                  (sbRoot sb) 0 [] 0 0
      c1 = installTree [] (sbRoot sb) (say "  installing static tree:" c0)
  in runBinds (IM.keys (clBinds c1)) (say "  initial full evaluation:" c1)

-- Render the tree with each bind's current value folded in.
render :: Client -> String
render c = go [] (clTree c)
 where
  outs p = [ (t, IM.findWithDefault (VS "?") b (clOut c))
           | (b,(p',t,_)) <- IM.toList (clBinds c), p' == p ]
  go p n =
    let ind = concat (replicate (length p) "  ")
        dyn = [ case t of
                  TgText   -> "text=" ++ show v
                  TgAttr a -> a ++ "=" ++ show v
                  TgClass  -> "class=" ++ show v
                  TgShow   -> "show=" ++ show v
              | (t,v) <- outs p ]
        sta = [ k ++ "=" ++ show v | (k,v) <- M.toList (nAttrs n) ]
        vis = not (any (\(t,v) -> t == TgShow && v == VB False) (outs p))
        hd  = ind ++ "<" ++ nTag n ++ ">"
              ++ (if null (sta ++ dyn) then "" else " " ++ intercalate " " (sta ++ dyn))
              ++ (if vis then "" else "   [hidden]")
    in intercalate "\n" (hd : [ go (p ++ [i]) k | (i,k) <- zip [0..] (nKids n) ])

--------------------------------------------------------------------------------
-- Part 7. Demo: a cart row. Tailwind-ish class computation, Alpine-ish binds.
--------------------------------------------------------------------------------

cell :: String -> RExpr
cell = RCell

lit :: String -> RExpr
lit = RS

themeClass :: RExpr
themeClass = RIf (cell "dark")
  (lit "bg-slate-900 text-slate-100")
  (lit "bg-white text-slate-900")

totalText :: RExpr
totalText = RBin OCat (lit "$") (RUn OI2S (RBin OMul (cell "qty") (cell "price")))

badgeClass :: RExpr
badgeClass = RIf (RBin OAnd (cell "selected") (RBin OLt (RI 0) (cell "qty")))
  (lit "ring-2 ring-blue-500")
  (lit "ring-0")

lowStock :: RExpr
lowStock = RBin OLt (RI 9) (cell "qty")

label :: RExpr
label = RBin OCat (lit "item: ") (cell "name")

batch :: StaticBatch
batch = StaticBatch
  { sbSchema = M.fromList
      [ ("qty",      (0, TInt))
      , ("price",    (1, TInt))
      , ("dark",     (2, TBool))
      , ("name",     (3, TStr))
      , ("selected", (4, TBool)) ]
  , sbInit = M.fromList
      [ ("qty", VI 1), ("price", VI 250), ("dark", VB False)
      , ("name", VS "widget"), ("selected", VB False) ]
  , sbRoot = Node "div" (M.fromList [("id","cart")])
      [ Bind TgClass themeClass ]
      [ Node "span" M.empty [ Bind (TgAttr "aria-label") label ] []
      , Node "b"    M.empty [ Bind TgText  totalText ] []
      , Node "em"   (M.fromList [("class","text-amber-600")])
                    [ Bind TgShow lowStock ] []
      , Node "i"    M.empty [ Bind TgClass badgeClass ] []
      ]
  }

newRow :: Node
newRow = Node "span" M.empty
  [ Bind TgClass badgeClass          -- identical compute -> cache hit
  , Bind TgText (RBin OCat (lit "qty ") (RUn OI2S (cell "qty")))
  ] []

deltaStream :: [Delta]
deltaStream =
  [ DSet "qty" (VI 3)
  , DSet "dark" (VB True)
  , DSet "selected" (VB True)
  , DSet "qty" (VI 12)
  , DInsert [] 4 newRow
  , DSet "qty" (VI 2)
  , DRemove [] 4
  , DSet "qty" (VI 5)
  ]

section :: String -> IO ()
section s = putStrLn ("\n=== " ++ s ++ " " ++ replicate (60 - length s) '=')

main :: IO ()
main = do
  section "BOOT: static batch"
  let c0 = boot batch
  mapM_ putStrLn (reverse (clLog c0))
  putStrLn (render c0)

  section "CODE OBJECT: the theme class compute, as the client holds it"
  let Right t = check (sbSchema batch) themeClass
      code = compile t
  putStrLn ("wire:  " ++ show themeClass)
  putStrLn ("type:  " ++ show (cTy code) ++ "   deps: " ++ show (cDeps code)
            ++ "   frame: " ++ show (cRegs code) ++ " regs")
  putStrLn "asm:"
  mapM_ (\(pc,i) -> printf "  %2d  %s\n" pc (show i)) (IM.toList (cInstrs code))
  putStrLn "image:"
  putStrLn ("  " ++ unwords (map (printf "%08x") (cWords code)))
  putStrLn ("  " ++ show (length (cWords code) * 4) ++ " bytes of code, "
            ++ show (length (cPool code)) ++ " pooled strings")

  section "TYPE ERROR: an ill-typed compute is rejected at install, not at frame time"
  let report r = putStrLn (case r of
        Left e            -> "rejected: " ++ e
        Right (Typed t _) -> "ok: " ++ show t)
  report (check (sbSchema batch) (RBin OAdd (cell "qty") (cell "name")))
  report (check (sbSchema batch) (RIf (cell "qty") (RI 1) (RI 2)))
  report (check (sbSchema batch) (RIf (cell "dark") (RI 1) (lit "x")))
  report (check (sbSchema batch) (RBin OMul (cell "qty") (cell "price")))

  section "DELTA STREAM"
  let step c d = let c' = applyDelta d c
                 in do mapM_ putStrLn (reverse (take (length (clLog c') - length (clLog c)) (clLog c')))
                       return c'
  cN <- foldM' step c0 deltaStream

  section "FINAL TREE"
  putStrLn (render cN)

  section "ACCOUNTING"
  let allBinds = IM.size (clBinds cN)
  printf "binds live:            %d\n" allBinds
  printf "code objects cached:   %d  (for %d installed binds over the session)\n"
         (M.size (clCache cN)) (clNextB cN)
  printf "total fuel retired:    %d instructions\n" (mFuel (clMach cN))
  printf "arena high-water:      %d string slots (reset per frame in a real client)\n"
         (mNext (clMach cN))
  printf "binds re-run:          %d  (naive full re-eval would be %d)\n"
         (clRuns cN) (clWould cN)
 where
  foldM' f z []     = return z
  foldM' f z (x:xs) = f z x >>= \z' -> foldM' f z' xs
