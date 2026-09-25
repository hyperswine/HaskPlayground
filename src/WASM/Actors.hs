{-# LANGUAGE LambdaCase #-}

-- ============================================================================
-- Actors: the capability layer + scheduler from the original toy, ported to
-- WasmVM.  Nothing conceptual changed: an actor is a VM record plus an fd
-- table; a host call that misses the grant cache parks the actor in WaitPerm
-- while the round continues; prompts resolve at end of round.
--
-- Host ABI is still the toy "thin translation layer" (3-arg fd_write etc.),
-- now dispatched by (import module, name) and typed.
-- ============================================================================

module WASM.Actors where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import WASM.WasmVM

data Rights = R | W deriving (Show, Eq, Ord)

data Resource = FileCap Rights FilePath deriving (Show, Eq, Ord)

data World = World
  { wGrants :: M.Map Resource Bool,
    wFiles :: M.Map FilePath String,
    wPolicy :: Resource -> Bool
  }

data FdEntry = FdStdout | FdFile FilePath Int

data Actor = Actor
  { aName :: String,
    aStatus :: Status,
    aFds :: IM.IntMap FdEntry,
    aNextFd :: Int
  }

data Status
  = Runnable VM
  | WaitPerm Resource String [Val] VM -- suspended awaiting the user
  | Dead String

mkActor :: String -> Inst -> Int -> Actor
mkActor name inst entry =
  Actor
    { aName = name,
      aStatus = either (Dead . ("bad entry: " ++)) Runnable (newVM inst entry []),
      aFds = IM.fromList [(1, FdStdout)],
      aNextFd = 3
    }

curVM :: Actor -> VM
curVM a = case aStatus a of
  Runnable vm -> vm
  WaitPerm _ _ _ vm -> vm
  Dead _ -> error "no VM"

withVM :: Actor -> (VM -> VM) -> Actor
withVM a f = case aStatus a of
  Runnable vm -> a {aStatus = Runnable (f vm)}
  WaitPerm r h g vm -> a {aStatus = WaitPerm r h g (f vm)}
  Dead _ -> a

mem :: Actor -> Memory
mem = inMem . vmInst . curVM

i32v :: Int -> Val
i32v = VI32 . fromIntegral

-- Left res => capability miss (must prompt).  Right => results + log lines.
doHost :: World -> Actor -> String -> [Val] -> IO (Either Resource (World, Actor, [Val], [String]))
doHost w a fn args = case (fn, map i32 args) of
  ("fd_write", [fd, p, n]) -> case IM.lookup fd (aFds a) of
    Just FdStdout -> do
      s <- memReadString (mem a) p n
      pure $ case s of
        Left _ -> Right (w, a, [i32v (-1)], [])
        Right str -> Right (w, a, [i32v n], ["[" ++ aName a ++ " stdout] " ++ show str])
    Just (FdFile path _) -> pure (Left (FileCap W path))
    Nothing -> pure (Right (w, a, [i32v (-1)], []))
  ("fd_read", [fd, p, n]) -> case IM.lookup fd (aFds a) of
    Just (FdFile path off) -> case M.lookup path (wFiles w) of
      Nothing -> pure (Right (w, a, [i32v (-1)], []))
      Just body -> do
        let chunk = take n (drop off body)
        r <- memWriteString (mem a) p chunk
        pure $ case r of
          Left _ -> Right (w, a, [i32v (-1)], [])
          Right () ->
            Right
              ( w,
                a {aFds = IM.insert fd (FdFile path (off + length chunk)) (aFds a)},
                [i32v (length chunk)],
                []
              )
    _ -> pure (Right (w, a, [i32v (-1)], []))
  ("path_open", [p, n, rw]) -> do
    s <- memReadString (mem a) p n
    case s of
      Left _ -> pure (Right (w, a, [i32v (-1)], []))
      Right path ->
        let res = FileCap (if rw == 0 then R else W) path
         in case M.lookup res (wGrants w) of
              Just True ->
                let fd = aNextFd a
                 in pure $
                      Right
                        ( w,
                          a {aFds = IM.insert fd (FdFile path 0) (aFds a), aNextFd = fd + 1},
                          [i32v fd],
                          ["[cap] " ++ aName a ++ " uses cached grant for " ++ show res ++ " -> fd " ++ show fd]
                        )
              Just False -> pure (Right (w, a, [i32v (-1)], ["[cap] " ++ aName a ++ " denied (cached) for " ++ show res]))
              Nothing -> pure (Left res)
  _ -> pure (Right (w, a, [i32v (-1)], ["[host] unknown call " ++ fn]))

fuelSlice, hostFuelCost :: Int
fuelSlice = 60
hostFuelCost = 10

runRounds :: World -> [Actor] -> IO ()
runRounds = go (1 :: Int)
  where
    go _ _ actors | all dead actors = putStrLn "[sched] all actors finished."
    go r w actors = do
      putStrLn $ "----- round " ++ show r ++ " -----"
      (w', actors') <- runRound w actors
      (w'', actors'') <- resolveOnePrompt w' actors'
      go (r + 1) w'' actors''
    dead a = case aStatus a of Dead _ -> True; _ -> False

runRound :: World -> [Actor] -> IO (World, [Actor])
runRound w [] = pure (w, [])
runRound w (a : as) = do
  (w', a') <- runActorSlice w a
  (w'', as') <- runRound w' as
  pure (w'', a' : as')

runActorSlice :: World -> Actor -> IO (World, Actor)
runActorSlice w a = case aStatus a of
  Runnable vm -> loop w a {aStatus = Runnable vm {vmFuel = fuelSlice}}
  _ -> pure (w, a)
  where
    loop wd act = case aStatus act of
      Runnable vm ->
        step vm >>= \case
          Running vm' -> loop wd act {aStatus = Runnable vm'}
          Yielded vm' -> do
            putStrLn $ "[sched] " ++ aName act ++ " fuel exhausted -> yields"
            pure (wd, act {aStatus = Runnable vm'})
          Finished _ -> do
            putStrLn $ "[sched] " ++ aName act ++ " finished."
            pure (wd, act {aStatus = Dead "ok"})
          Trapped msg -> do
            putStrLn $ "[sched] " ++ aName act ++ " TRAPPED: " ++ msg
            pure (wd, act {aStatus = Dead msg})
          NeedHost _ fn args vm' -> do
            let vmCharged = vm' {vmFuel = vmFuel vm' - hostFuelCost}
            r <- doHost wd act {aStatus = Runnable vmCharged} fn args
            case r of
              Right (wd', act', results, logs) -> do
                mapM_ putStrLn logs
                loop wd' (withVM act' (resumeHost results))
              Left res -> do
                putStrLn $ "[cap] " ++ aName act ++ " needs " ++ show res ++ " -> no grant cached; SUSPENDING actor, asking user"
                pure (wd, act {aStatus = WaitPerm res fn args vmCharged})
      _ -> pure (wd, act)

resolveOnePrompt :: World -> [Actor] -> IO (World, [Actor])
resolveOnePrompt w actors = go actors []
  where
    go [] acc = pure (w, reverse acc)
    go (a : as) acc = case aStatus a of
      WaitPerm res fn args vm -> do
        let verdict = wPolicy w res
        putStrLn $ "[user] prompt: \"" ++ aName a ++ "\" requests " ++ show res ++ " -> user says " ++ (if verdict then "ALLOW" else "DENY")
        let w' = w {wGrants = M.insert res verdict (wGrants w)}
        r <- doHost w' a {aStatus = Runnable vm} fn args
        a' <- case r of
          Right (_, actR, results, logs) -> mapM_ putStrLn logs >> pure (withVM actR (resumeHost results))
          Left _ -> pure a
        pure (w', reverse acc ++ (a' : as))
      _ -> go as (a : acc)
