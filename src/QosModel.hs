{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | A small executable model of the QOS .qa app lifecycle:
--
--   .qa format (manifest + code, content-address pinned)
--     -> `run App.qa` via System.app
--     -> grant dialogue (two checkboxes: compulsory / optional)
--     -> VFS discovery of /services/* (discovery == grant == channel mint)
--     -> per-launch SPSC channel pairs as the capability handles
--     -> root actor spawned with handles in its environment,
--        child actors inherit by closure (process-scoped capabilities)
--     -> graceful teardown: channels closed, service endpoints drain & exit
--
-- Design notes encoded here:
--   * Compulsory capabilities are unconditional fields of the app Env
--     (no Maybe): the Env cannot be constructed without them.
--   * Optional capabilities are `Grant h = Granted h | Denied`; the app is
--     forced by exhaustive matching to carry a fallback.
--   * The handle IS the channel pair; Denied is the absence of one.
--   * The manifest is folded into the app's content hash, so editing the
--     permission list changes the app identity (grant/hash binding).

module QosModel (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM, forM_, unless, when)
import Data.Bits (xor)
import Data.Char (ord)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Word (Word64)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- 1. Content addressing (stand-in for the real store)
--------------------------------------------------------------------------------

newtype Hash = Hash Word64 deriving (Eq)

instance Show Hash where
  show (Hash w) = printf "#%016x" w

-- FNV-1a over a rendering; stands in for the real content hash.
hashOf :: String -> Hash
hashOf = Hash . foldl step 0xcbf29ce484222325
  where
    step h c = (h `xor` fromIntegral (ord c)) * 0x100000001b3

--------------------------------------------------------------------------------
-- 2. Manifest and the .qa format
--------------------------------------------------------------------------------

type ServicePath = String            -- e.g. "/services/net"

data Tier = Compulsory | Optional
  deriving (Eq, Show)

data Requirement = Requirement
  { reqService :: ServicePath
  , reqTier    :: Tier
  , reqApi     :: String             -- versioned std surface, e.g. "std.net#2.1"
  } deriving (Show)

data Manifest = Manifest
  { mName     :: String
  , mVersion  :: String
  , mRequires :: [Requirement]
  } deriving (Show)

renderManifest :: Manifest -> String
renderManifest m =
  mName m ++ "@" ++ mVersion m ++ "|" ++
  concatMap (\r -> reqService r ++ ":" ++ show (reqTier r) ++ ":" ++ reqApi r ++ ";")
            (mRequires m)

-- | A .qa app: manifest + code, identified by the hash of BOTH.
--   (Code is modelled as its source text for hashing purposes, plus the
--   actual behaviour as a Haskell function.)
data QaApp = QaApp
  { qaManifest   :: Manifest
  , qaCodeText   :: String                 -- stands in for compiled Sol bytecode
  , qaEntryPoint :: Env -> IO AppExit      -- the root actor's behaviour
  }

-- Content address of the whole app: manifest folded in, so a manifest edit
-- (e.g. quietly adding a permission) is a different app identity.
qaHash :: QaApp -> Hash
qaHash app = hashOf (renderManifest (qaManifest app) ++ "\0" ++ qaCodeText app)

data AppExit = ExitOk | ExitErr String
  deriving (Show)

--------------------------------------------------------------------------------
-- 3. SPSC channels: the capability handle IS the channel pair
--------------------------------------------------------------------------------

data SvcReq  = SvcReq String  | SvcClose   deriving (Show)
data SvcResp = SvcResp String               deriving (Show)

-- One direction of an SPSC link. Bounded queue of 1 slot ~= your SPSC ring.
newtype Pipe a = Pipe (TBQueue a)

newPipe :: IO (Pipe a)
newPipe = Pipe <$> newTBQueueIO 8

pushP :: Pipe a -> a -> IO ()
pushP (Pipe q) = atomically . writeTBQueue q

popP :: Pipe a -> IO a
popP (Pipe q) = atomically (readTBQueue q)

-- | The app-side handle to a service: created at launch, closed at teardown.
data Handle = Handle
  { hService :: ServicePath
  , hReqs    :: Pipe SvcReq         -- app -> service  (app is the S producer)
  , hResps   :: Pipe SvcResp        -- service -> app  (app is the S consumer)
  , hOpen    :: TVar Bool
  }

call :: Handle -> String -> IO (Either String String)
call h msg = do
  open <- readTVarIO (hOpen h)
  if not open
    then pure (Left ("channel to " ++ hService h ++ " is closed"))
    else do
      pushP (hReqs h) (SvcReq msg)
      SvcResp r <- popP (hResps h)
      pure (Right r)

closeHandle :: Handle -> IO ()
closeHandle h = do
  open <- atomically $ do
    o <- readTVar (hOpen h)
    when o (writeTVar (hOpen h) False)
    pure o
  when open $ pushP (hReqs h) SvcClose   -- let the endpoint drain and exit

--------------------------------------------------------------------------------
-- 4. VFS discovery layer (Files.app's /services namespace)
--------------------------------------------------------------------------------

-- A service implementation: given the service ends of a freshly minted
-- channel pair, run an endpoint loop until SvcClose.
type ServiceImpl = Pipe SvcReq -> Pipe SvcResp -> TVar Int -> IO ()

newtype VFS = VFS (M.Map ServicePath ServiceImpl)

lookupService :: VFS -> ServicePath -> Maybe ServiceImpl
lookupService (VFS m) = flip M.lookup m

-- | Discovery == grant == mint: resolving the path constructs the channel
--   pair, spawns the per-launch service endpoint, and hands back the handle.
mint :: VFS -> TVar Int -> ServicePath -> IO (Either String Handle)
mint vfs liveCount path =
  case lookupService vfs path of
    Nothing   -> pure (Left ("VFS: no such service " ++ path))
    Just impl -> do
      reqs  <- newPipe
      resps <- newPipe
      open  <- newTVarIO True
      atomically $ modifyTVar' liveCount (+ 1)
      _ <- forkIO (impl reqs resps liveCount)
      pure (Right (Handle path reqs resps open))

-- Two toy services --------------------------------------------------------

netService :: ServiceImpl
netService reqs resps live = loop
  where
    loop = popP reqs >>= \case
      SvcClose   -> done
      SvcReq msg -> do
        pushP resps (SvcResp ("net: fetched <" ++ msg ++ "> (200 OK)"))
        loop
    done = atomically (modifyTVar' live (subtract 1))

cameraService :: ServiceImpl
cameraService reqs resps live = loop
  where
    loop = popP reqs >>= \case
      SvcClose  -> done
      SvcReq _  -> do
        pushP resps (SvcResp "camera: frame[640x480]")
        loop
    done = atomically (modifyTVar' live (subtract 1))

--------------------------------------------------------------------------------
-- 5. The app environment: compulsory unconditional, optional as Grant
--------------------------------------------------------------------------------

data Grant = Granted Handle | Denied

-- The typed projection for this demo app. The point: `envNet` is NOT a
-- Maybe. If you hold an Env, net was granted; the constructor below is the
-- only way to make one.
data Env = Env
  { envNet    :: Handle          -- compulsory
  , envCamera :: Grant           -- optional
  }

--------------------------------------------------------------------------------
-- 6. System.app: the run flow
--------------------------------------------------------------------------------

data Dialogue = Dialogue
  { tickCompulsory :: Bool       -- checkbox 1
  , tickOptional   :: Bool       -- checkbox 2
  } deriving (Show)

data RunResult
  = NotLaunched String           -- compulsory denied / discovery failed / bad pin
  | Ran AppExit
  deriving (Show)

-- | `run App.qa` — System.app checks the pin, shows the dialogue, mints
--   handles, spawns the root actor, then tears everything down.
runQa :: VFS -> Hash -> QaApp -> Dialogue -> IO RunResult
runQa vfs pinnedHash app Dialogue{tickCompulsory, tickOptional} = do
  let mf   = qaManifest app
      comp = [r | r <- mRequires mf, reqTier r == Compulsory]
      opts = [r | r <- mRequires mf, reqTier r == Optional]

  say $ "run " ++ mName mf ++ ".qa " ++ show (qaHash app)
  say $ "  System.app: dialogue -> compulsory " ++ box tickCompulsory
                          ++ " " ++ show (map reqService comp)
                          ++ " | optional " ++ box tickOptional
                          ++ " " ++ show (map reqService opts)

  -- (0) content-address pin: the manifest is inside the hash
  if qaHash app /= pinnedHash
    then pure (NotLaunched "hash mismatch: app/manifest differs from pinned identity")
    else if not tickCompulsory
      then pure (NotLaunched ("compulsory capabilities not granted: "
                              ++ show (map reqService comp)))
      else do
        live <- newTVarIO (0 :: Int)   -- live service-endpoint counter

        -- (1) discovery + mint for compulsory (all must resolve)
        compHs <- forM comp (mint vfs live . reqService)
        case sequence compHs of
          Left err -> pure (NotLaunched err)
          Right [netH] -> do
            say "  System.app: compulsory handles minted (discovery==grant==mint)"

            -- (2) optional: mint if ticked, otherwise Denied (no channel exists)
            camG <- if tickOptional
              then forM opts (mint vfs live . reqService) >>= \case
                     [Right h] -> say "  System.app: optional handle minted"
                                    >> pure (Granted h)
                     _         -> pure Denied
              else say "  System.app: optional denied — no channel minted"
                     >> pure Denied

            -- (3) the ONLY construction site of Env: compulsory is
            --     unconditional by the time the app sees it
            let env = Env { envNet = netH, envCamera = camG }

            -- (4) run the root actor
            exit <- qaEntryPoint app env

            -- (5) teardown: close every channel this launch minted
            closeHandle netH
            case camG of Granted h -> closeHandle h
                         Denied    -> pure ()

            -- (6) wait for every service endpoint to drain and exit
            atomically $ readTVar live >>= \n -> unless (n == 0) retry
            say "  System.app: teardown complete, all service endpoints exited"
            pure (Ran exit)
          Right _ -> pure (NotLaunched "demo wiring: expected exactly one compulsory service")
  where
    box b = if b then "[x]" else "[ ]"

say :: String -> IO ()
say = putStrLn

--------------------------------------------------------------------------------
-- 7. A demo app: root actor + a child actor inheriting the net handle
--------------------------------------------------------------------------------

photoApp :: QaApp
photoApp = QaApp
  { qaManifest = Manifest
      { mName     = "PhotoShare"
      , mVersion  = "1.0"
      , mRequires =
          [ Requirement "/services/net"    Compulsory "std.net#2.1"
          , Requirement "/services/camera" Optional   "std.camera#1.0"
          ]
      }
  , qaCodeText = "root = spawn uploader; case app.optional.camera of ..."
  , qaEntryPoint = \env -> do
      -- Optional capability: exhaustive match forced, fallback required.
      shot <- case envCamera env of
        Granted cam -> call cam "capture" >>= \case
          Right frame -> pure frame
          Left err    -> pure ("camera error: " ++ err)
        Denied      -> pure "placeholder.png (camera not granted — fallback)"
      say $ "    [root]  image source: " ++ shot

      -- Child actor inherits envNet by ordinary closure over the spawn env:
      -- process-scoped capability, no delegation machinery.
      doneChild <- newIORef False
      _ <- forkIO $ do
        r <- call (envNet env) ("upload " ++ shot)
        say $ "    [child] " ++ either id id r
        writeIORef doneChild True
      waitRef doneChild

      -- Root uses the same compulsory handle unconditionally (no Maybe).
      r <- call (envNet env) "GET /feed"
      say $ "    [root]  " ++ either id id r
      pure ExitOk
  }
  where
    waitRef ref = readIORef ref >>= \d ->
      unless d (threadDelay 1000 >> waitRef ref)

--------------------------------------------------------------------------------
-- 8. Scenarios
--------------------------------------------------------------------------------

main :: IO ()
main = do
  let vfs = VFS $ M.fromList
        [ ("/services/net",    netService)
        , ("/services/camera", cameraService)
        ]
      pin = qaHash photoApp

  banner "Scenario 1: both boxes ticked"
  runQa vfs pin photoApp (Dialogue True True)  >>= report

  banner "Scenario 2: compulsory ticked, optional NOT ticked (fallback path)"
  runQa vfs pin photoApp (Dialogue True False) >>= report

  banner "Scenario 3: compulsory NOT ticked (app must not launch)"
  runQa vfs pin photoApp (Dialogue False True) >>= report

  banner "Scenario 4: manifest tampered after pinning (permission added)"
  let sneaky = photoApp
        { qaManifest = (qaManifest photoApp)
            { mRequires = mRequires (qaManifest photoApp)
                       ++ [Requirement "/services/mic" Compulsory "std.audio#1.0"] } }
  runQa vfs pin sneaky (Dialogue True True) >>= report

  banner "Scenario 5: manifest asks for a service the VFS doesn't expose"
  let lost = photoApp
        { qaManifest = (qaManifest photoApp)
            { mRequires = [Requirement "/services/warp" Compulsory "std.warp#0.1"] } }
  runQa vfs (qaHash lost) lost (Dialogue True True) >>= report
  where
    banner s = putStrLn "" >> putStrLn ("== " ++ s ++ " ==")
    report r = putStrLn ("  => " ++ show r)
