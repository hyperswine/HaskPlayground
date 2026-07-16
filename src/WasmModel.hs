{-# LANGUAGE LambdaCase #-}
-- ============================================================================
-- A small executable model of:
--   * a wasm-like structured-control-flow stack VM
--   * fuel-counted cooperative yielding (interpreter-level fuel)
--   * a WASI-like host-call boundary ("thin translation layer")
--   * capability-gated resources with ON-DEMAND user permission prompts,
--     where a blocked actor suspends and OTHER actors keep running
--   * a round-robin actor scheduler (stand-in for the FPR OS scheduler)
--
-- The VM is deliberately shaped like a real minimal wasm interpreter:
-- explicit value stack, structured Block/Loop/If with de-Bruijn-style Br
-- depths, and suspension implemented by simply returning the VM state
-- (no host stack capture needed -- the wasm state IS the continuation).
-- ============================================================================

module WasmModel where

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import           Data.Char (chr, ord)
import           Data.List (intercalate)

-- ----------------------------------------------------------------------------
-- Instruction set (tiny wasm-flavored subset, structured control flow)
-- ----------------------------------------------------------------------------

data HostFn = FdWrite | FdRead | PathOpen
  deriving (Show, Eq)

data Instr
  = Const Int
  | LGet Int | LSet Int | LTee Int
  | Add | Sub | Mul | Eqz
  | Blk [Instr]            -- block: Br n targets exit
  | Lp  [Instr]            -- loop:  Br n targets re-entry (back-edge)
  | Ift [Instr] [Instr]    -- if/else
  | Br Int | BrIf Int      -- de Bruijn label depth, like real wasm
  | Host HostFn            -- call into the WASI-ish translation layer
  | Drp
  deriving (Show)

-- ----------------------------------------------------------------------------
-- VM state.  A suspended VM is just this record -- that is the whole trick
-- that makes fuel-yield cheap: wasm's operand stack + control frames are
-- explicit, so "save the continuation" is "return the record".
-- ----------------------------------------------------------------------------

data Frame = Frame
  { frCont :: [Instr]   -- code after this construct (fall-through / block-br)
  , frBr   :: [Instr]   -- code to run when a Br targets this frame
  }                     --   Block/If: same as frCont.  Loop: re-enter loop.

data VM = VM
  { vmCode :: [Instr]
  , vmStk  :: [Int]
  , vmFrs  :: [Frame]
  , vmLoc  :: IM.IntMap Int
  , vmMem  :: IM.IntMap Int   -- linear memory, byte-granular (Int as byte)
  , vmFuel :: Int
  }

data Step
  = Running VM
  | Yielded VM                 -- fuel exhausted: cooperative yield point
  | NeedHost HostFn [Int] VM   -- args already popped; host pushes results
  | Finished VM
  | Trapped String

-- one small-step transition -----------------------------------------------

step :: VM -> Step
step vm
  | vmFuel vm <= 0 = Yielded vm
  | otherwise = case vmCode vm of
      [] -> case vmFrs vm of
              (f:fs) -> Running vm { vmCode = frCont f, vmFrs = fs }
              []     -> Finished vm
      (i:rest) ->
        let vm' = vm { vmCode = rest, vmFuel = vmFuel vm - 1 }
        in exec i vm'

exec :: Instr -> VM -> Step
exec ins vm = case ins of
  Const n -> push n vm
  LGet k  -> push (IM.findWithDefault 0 k (vmLoc vm)) vm
  LSet k  -> pop1 vm $ \v m -> Running m { vmLoc = IM.insert k v (vmLoc m) }
  LTee k  -> pop1 vm $ \v m -> Running m { vmLoc = IM.insert k v (vmLoc m)
                                         , vmStk = v : vmStk m }
  Add -> bin (+) vm
  Sub -> bin (-) vm
  Mul -> bin (*) vm
  Eqz -> pop1 vm $ \v m -> push (if v == 0 then 1 else 0) m
  Drp -> pop1 vm $ \_ m -> Running m

  Blk body -> Running vm { vmCode = body
                         , vmFrs  = Frame (vmCode vm) (vmCode vm) : vmFrs vm }
  Lp body  -> Running vm { vmCode = body
                         , vmFrs  = Frame (vmCode vm)
                                          (Lp body : vmCode vm) : vmFrs vm }
  Ift t e  -> pop1 vm $ \c m ->
                Running m { vmCode = if c /= 0 then t else e
                          , vmFrs  = Frame (vmCode m) (vmCode m) : vmFrs m }

  Br n   -> branch n vm
  BrIf n -> pop1 vm $ \c m -> if c /= 0 then branch n m else Running m

  Host fn -> let arity = hostArity fn
             in if length (vmStk vm) < arity
                  then Trapped "host-call stack underflow"
                  else let (args, rest) = splitAt arity (vmStk vm)
                       in NeedHost fn (reverse args) vm { vmStk = rest }

branch :: Int -> VM -> Step
branch n vm = case drop n (vmFrs vm) of
  (f:fs) -> Running vm { vmCode = frBr f, vmFrs = fs }
  []     -> Trapped ("br depth " ++ show n ++ " exceeds frame stack")

push :: Int -> VM -> Step
push v vm = Running vm { vmStk = v : vmStk vm }

pop1 :: VM -> (Int -> VM -> Step) -> Step
pop1 vm k = case vmStk vm of
  (v:vs) -> k v vm { vmStk = vs }
  []     -> Trapped "stack underflow"

bin :: (Int -> Int -> Int) -> VM -> Step
bin f vm = case vmStk vm of
  (b:a:vs) -> Running vm { vmStk = f a b : vs }
  _        -> Trapped "stack underflow"

hostArity :: HostFn -> Int
hostArity FdWrite  = 3   -- fd, ptr, len          -> bytes-written | -1
hostArity FdRead   = 3   -- fd, ptr, len          -> bytes-read    | -1
hostArity PathOpen = 3   -- ptr, len, rights(0=R) -> fd            | -1

-- linear-memory helpers ------------------------------------------------------

memReadStr :: VM -> Int -> Int -> String
memReadStr vm p n = [ chr (IM.findWithDefault 0 (p+i) (vmMem vm)) | i <- [0..n-1] ]

memWriteStr :: Int -> String -> VM -> VM
memWriteStr p s vm =
  vm { vmMem = foldr (\(i,c) m -> IM.insert (p+i) (ord c) m) (vmMem vm)
                     (zip [0..] s) }

-- ----------------------------------------------------------------------------
-- Capability layer: resources, grants, and on-demand prompting
-- ----------------------------------------------------------------------------

data Rights = R | W deriving (Show, Eq, Ord)

data Resource = FileCap Rights FilePath
  deriving (Show, Eq, Ord)

-- the "System actor": consults the grant cache; a miss means we must ask the
-- user, which in this model suspends the requesting actor until resolved.
data World = World
  { wGrants :: M.Map Resource Bool          -- cached user decisions
  , wFiles  :: M.Map FilePath String        -- the simulated FS
  , wPolicy :: Resource -> Bool             -- stand-in for the human
  }

-- per-actor fd table (fd 1 preopened as stdout, like a WASI preopen)
data FdEntry = FdStdout | FdFile FilePath Int   -- path + read offset

data Actor = Actor
  { aName   :: String
  , aStatus :: Status
  , aFds    :: IM.IntMap FdEntry
  , aNextFd :: Int
  }

data Status
  = Runnable VM
  | WaitPerm Resource HostFn [Int] VM   -- suspended awaiting the user
  | Dead String                         -- finished or trapped

-- Try to perform a host call.  Left res  => capability miss, must prompt.
--                              Right ... => done, values to push.
doHost :: World -> Actor -> HostFn -> [Int]
       -> Either Resource (World, Actor, [Int], [String])
doHost w a fn args = case (fn, args) of

  (FdWrite, [fd, p, n]) ->
    case IM.lookup fd (aFds a) of
      Just FdStdout ->
        let s = memReadStr (curVM a) p n
        in Right (w, a, [n], ["[" ++ aName a ++ " stdout] " ++ show s])
      Just (FdFile path _) -> needs (FileCap W path)   -- writes gated too
      Nothing -> Right (w, a, [-1], [])

  (FdRead, [fd, p, n]) ->
    case IM.lookup fd (aFds a) of
      Just (FdFile path off) ->
        case M.lookup path (wFiles w) of
          Nothing   -> Right (w, a, [-1], [])
          Just body ->
            let chunk = take n (drop off body)
                a'    = a { aFds = IM.insert fd (FdFile path (off + length chunk))
                                             (aFds a) }
                a''   = withVM a' (memWriteStr p chunk)
            in Right (w, a'', [length chunk], [])
      _ -> Right (w, a, [-1], [])

  (PathOpen, [p, n, rw]) ->
    let path   = memReadStr (curVM a) p n
        rights = if rw == 0 then R else W
        res    = FileCap rights path
    in case M.lookup res (wGrants w) of
         Just True  ->
           let fd = aNextFd a
               a' = a { aFds = IM.insert fd (FdFile path 0) (aFds a)
                      , aNextFd = fd + 1 }
           in Right (w, a', [fd], ["[cap] " ++ aName a ++ " uses cached grant for "
                                     ++ show res ++ " -> fd " ++ show fd])
         Just False -> Right (w, a, [-1], ["[cap] " ++ aName a
                                     ++ " denied (cached) for " ++ show res])
         Nothing    -> needs res

  _ -> Right (w, a, [-1], [])
  where
    needs = Left

curVM :: Actor -> VM
curVM a = case aStatus a of
  Runnable vm         -> vm
  WaitPerm _ _ _ vm   -> vm
  Dead _              -> error "no VM"

withVM :: Actor -> (VM -> VM) -> Actor
withVM a f = case aStatus a of
  Runnable vm       -> a { aStatus = Runnable (f vm) }
  WaitPerm r h g vm -> a { aStatus = WaitPerm r h g (f vm) }
  Dead _            -> a

-- ----------------------------------------------------------------------------
-- Scheduler: round-robin, fixed fuel slice per turn.
-- A permission miss parks the actor; prompts resolve at end of round,
-- so you can SEE the other actor keep running while one is blocked.
-- ----------------------------------------------------------------------------

fuelSlice :: Int
fuelSlice = 60

hostFuelCost :: Int
hostFuelCost = 10

runRounds :: World -> [Actor] -> IO ()
runRounds = go (1 :: Int)
  where
    go _ _ actors | all dead actors = putStrLn "[sched] all actors finished."
    go r w actors = do
      putStrLn $ "----- round " ++ show r ++ " -----"
      (w', actors') <- runRound w actors
      (w'', actors'') <- resolveOnePrompt w' actors'
      go (r+1) w'' actors''

    dead a = case aStatus a of Dead _ -> True; _ -> False

runRound :: World -> [Actor] -> IO (World, [Actor])
runRound w [] = pure (w, [])
runRound w (a:as) = do
  (w', a') <- runActorSlice w a
  (w'', as') <- runRound w' as
  pure (w'', a' : as')

runActorSlice :: World -> Actor -> IO (World, Actor)
runActorSlice w a = case aStatus a of
  Runnable vm -> loop w a { aStatus = Runnable vm { vmFuel = fuelSlice } }
  _           -> pure (w, a)   -- waiting on user, or dead: skip
  where
    loop wd act = case aStatus act of
      Runnable vm -> case step vm of
        Running vm'  -> loop wd act { aStatus = Runnable vm' }
        Yielded vm'  -> do
          putStrLn $ "[sched] " ++ aName act ++ " fuel exhausted -> yields"
          pure (wd, act { aStatus = Runnable vm' })
        Finished _   -> do
          putStrLn $ "[sched] " ++ aName act ++ " finished."
          pure (wd, act { aStatus = Dead "ok" })
        Trapped msg  -> do
          putStrLn $ "[sched] " ++ aName act ++ " TRAPPED: " ++ msg
          pure (wd, act { aStatus = Dead msg })
        NeedHost fn args vm' ->
          let vmCharged = vm' { vmFuel = vmFuel vm' - hostFuelCost } in
          case doHost wd act { aStatus = Runnable vmCharged } fn args of
            Right (wd', act', results, logs) -> do
              mapM_ putStrLn logs
              let act'' = withVM act' (\m -> m { vmStk = reverse results ++ vmStk m })
              loop wd' act''
            Left res -> do
              putStrLn $ "[cap] " ++ aName act ++ " needs " ++ show res
                       ++ " -> no grant cached; SUSPENDING actor, asking user"
              pure (wd, act { aStatus = WaitPerm res fn args vmCharged })
      _ -> pure (wd, act)

-- end-of-round: the "System actor" resolves one pending user prompt
resolveOnePrompt :: World -> [Actor] -> IO (World, [Actor])
resolveOnePrompt w actors = go actors []
  where
    go [] acc = pure (w, reverse acc)
    go (a:as) acc = case aStatus a of
      WaitPerm res fn args vm -> do
        let verdict = wPolicy w res
        putStrLn $ "[user] prompt: \"" ++ aName a ++ "\" requests " ++ show res
                 ++ " -> user says " ++ (if verdict then "ALLOW" else "DENY")
        let w' = w { wGrants = M.insert res verdict (wGrants w) }
        -- retry the host call now that a decision is cached
        a' <- case doHost w' a { aStatus = Runnable vm } fn args of
          Right (_, actR, results, logs) -> do
            mapM_ putStrLn logs
            pure $ withVM actR (\m -> m { vmStk = reverse results ++ vmStk m })
          Left _ -> pure a   -- unreachable: grant is now cached
        pure (w', reverse acc ++ (a' : as))
      _ -> go as (a:acc)

-- ----------------------------------------------------------------------------
-- Two demo "apps" (hand-assembled wasm-ish modules)
-- ----------------------------------------------------------------------------

-- data-segment layout helpers
seg :: [(Int, String)] -> IM.IntMap Int
seg xs = IM.fromList [ (p+i, ord c) | (p, s) <- xs, (i, c) <- zip [0..] s ]

mkActor :: String -> [Instr] -> [(Int,String)] -> Actor
mkActor name body dataSegs = Actor
  { aName   = name
  , aStatus = Runnable VM { vmCode = body, vmStk = [], vmFrs = []
                          , vmLoc = IM.empty, vmMem = seg dataSegs
                          , vmFuel = 0 }
  , aFds    = IM.fromList [(1, FdStdout)]
  , aNextFd = 3
  }

-- App 1: "ticker" -- 4 iterations of (print "tick" then burn fuel in an
-- inner loop).  Forces multiple fuel-yields per tick: pure compute,
-- zero capabilities needed.
ticker :: Actor
ticker = mkActor "ticker" body [(0, "tick\n")]
  where
    body =
      [ Const 0, LSet 0                                   -- i = 0
      , Blk [ Lp
          [ LGet 0, Const 4, Sub, Eqz, BrIf 1             -- if i==4 exit
          , Const 1, Const 0, Const 5, Host FdWrite, Drp  -- write "tick\n"
          , Const 25, LSet 1                              -- j = 25
          , Lp [ LGet 1, Const 1, Sub, LTee 1, BrIf 0 ]   -- busy loop
          , LGet 0, Const 1, Add, LSet 0                  -- i++
          , Br 0 ] ]                                      -- back-edge
      ]

-- App 2: "filer" -- opens a file it has no capability for (prompts the user,
-- ALLOW), reads + prints it, then tries a second path (prompts, DENY) and
-- handles the -1 gracefully.
filer :: Actor
filer = mkActor "filer" body
          [ (100, "/home/jasen/notes.txt")
          , (140, "/etc/shadow")
          , (300, "denied!\n") ]
  where
    body =
      [ Const 100, Const 21, Const 0, Host PathOpen       -- open notes (R)
      , LSet 0                                            -- fd
      , LGet 0, Const 200, Const 64, Host FdRead          -- read into 200
      , LSet 1                                            -- n
      , Const 1, Const 200, LGet 1, Host FdWrite, Drp     -- echo to stdout
      , Const 140, Const 11, Const 0, Host PathOpen       -- open shadow (R)
      , Const (-1), Sub, Eqz                              -- == -1 ?
      , Ift [ Const 1, Const 300, Const 8, Host FdWrite, Drp ]
            [ ]
      ]

-- ----------------------------------------------------------------------------

main :: IO ()
main = do
  let world = World
        { wGrants = M.empty
        , wFiles  = M.fromList
            [ ("/home/jasen/notes.txt", "remember: fuel checks at loop headers\n")
            , ("/etc/shadow", "root:$6$nope") ]
        , wPolicy = \case
            FileCap R "/home/jasen/notes.txt" -> True
            _                                 -> False
        }
  putStrLn "=== FPR OS wasm-VM model: fuel yields + on-demand capabilities ==="
  -- filer first in the run queue so its permission-block visibly lets
  -- ticker keep running in the same round
  runRounds world [filer, ticker]
