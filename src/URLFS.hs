{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use tuple-section" #-}

-- UrlFs: a filesystem modelled as a trie over string "URLs".
--
-- Two views over one structure:
--   * URL semantics:       resolve caller path root  -- opaque string -> handle,
--                          no requirement that intermediate nodes "exist"
--   * Directory semantics: ls caller path root       -- prefix query, listing
--                          synthesized on demand from the trie spine
--
-- Design points demonstrated:
--   1. bind creates arbitrarily deep paths with no ceremony; intermediate
--      nodes are just Map spine (routing structure), not owned entities.
--      There is no "empty boilerplate subdirectory": a node with
--      nRes = Nothing has no metadata, no permissions, no identity.
--   2. A path can simultaneously hold a resource AND have children
--      (no file/dir dichotomy).
--   3. Dynamic namespaces: an Actor bound at a prefix resolves the
--      *remainder* of the path itself. Paths under it need no trie nodes
--      at all. Listing under it is whatever the actor chooses to support.
--   4. Capability consistency: resolve and ls share ONE visibility
--      predicate (canSee). ls cannot leak the existence of a path that
--      resolve would deny -- a subtree is listed iff some resource in it
--      is visible to the caller. Single code path, not two conventions.

module URLFS where

import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

--------------------------------------------------------------------------------
-- Paths: just strings, split on '/'
--------------------------------------------------------------------------------

type Seg = String

type Path = [Seg]

parse :: String -> Path
parse = filter (not . null) . splitOn '/'
  where
    splitOn c s = case break (== c) s of
      (a, []) -> [a]
      (a, _ : b) -> a : splitOn c b

render :: Path -> String
render p = '/' : intercalate "/" p

--------------------------------------------------------------------------------
-- Capabilities
--------------------------------------------------------------------------------

newtype Cap = Cap String deriving (Eq, Ord, Show)

type CapSet = S.Set Cap

-- The single visibility predicate used by BOTH resolve and ls.
canSee :: CapSet -> CapSet -> Bool
canSee caller required = required `S.isSubsetOf` caller

--------------------------------------------------------------------------------
-- Resources
--------------------------------------------------------------------------------

data Resource
  = Blob String -- a stored value
  | Actor Service -- a dynamic namespace: resolves its own suffixes

data Service = Service
  { svcName :: String,
    svcResolve :: Path -> Maybe Resource, -- remainder of path -> resource
    svcList :: Path -> Maybe [Seg] -- Nothing = not enumerable;
  } -- enumeration is the actor's choice

describe :: Resource -> String
describe (Blob s) = "Blob " ++ show s
describe (Actor s) = "Actor <" ++ svcName s ++ ">"

--------------------------------------------------------------------------------
-- The trie
--------------------------------------------------------------------------------

data Bound = Bound {bRes :: Resource, bReq :: CapSet}

data Node = Node
  { nRes :: Maybe Bound, -- resource AT this path (may be absent)
    nKids :: M.Map Seg Node -- children (may coexist with a resource)
  }

emptyN :: Node
emptyN = Node Nothing M.empty

-- Bind a resource at a path. Intermediates are synthesized as bare spine:
-- no metadata, no permission bits, no separate creation step.
bind :: String -> [Cap] -> Resource -> Node -> Node
bind url caps r = go (parse url)
  where
    b = Bound r (S.fromList caps)
    go [] n = n {nRes = Just b}
    go (s : ss) n = n {nKids = M.alter (Just . go ss . fromMaybe emptyN) s (nKids n)}

--------------------------------------------------------------------------------
-- URL semantics: resolve
--------------------------------------------------------------------------------

-- Walk the trie; if the trie runs out but the current node holds a
-- visible Actor, hand the remainder of the path to it (dynamic namespace).
resolve :: CapSet -> String -> Node -> Maybe Resource
resolve caller url root = go (parse url) root
  where
    go [] n = do
      Bound r req <- nRes n
      if canSee caller req then Just r else Nothing
    go p@(s : ss) n =
      case M.lookup s (nKids n) of
        Just child -> go ss child
        Nothing -> case nRes n of
          Just (Bound (Actor svc) req) | canSee caller req -> svcResolve svc p
          _ -> Nothing

--------------------------------------------------------------------------------
-- Directory semantics: ls (prefix query, listing synthesized on demand)
--------------------------------------------------------------------------------

data Kind = HasRes | HasKids | Both deriving (Eq, Show)

-- A subtree is worth listing iff it contains at least one resource the
-- caller can see. Same predicate as resolve -> ls can never advertise a
-- path that resolve would refuse.
anyVisible :: CapSet -> Node -> Bool
anyVisible c n = maybe False (canSee c . bReq) (nRes n) || any (anyVisible c) (M.elems (nKids n))

ls :: CapSet -> String -> Node -> Maybe [(Seg, Kind)]
ls caller url root = go (parse url) root
  where
    go [] n =
      let static = [(s, kindOf child) | (s, child) <- M.toList (nKids n), anyVisible caller child]
          dynamic = case nRes n of
            Just (Bound (Actor svc) req) | canSee caller req -> maybe [] (map (\s -> (s, HasRes))) (svcList svc [])
            _ -> []
       in Just (static ++ dynamic)
    go p@(s : ss) n =
      case M.lookup s (nKids n) of
        Just child -> go ss child
        Nothing -> case nRes n of
          Just (Bound (Actor svc) req) | canSee caller req -> map (\seg -> (seg, HasRes)) <$> svcList svc p
          _ -> Nothing

    kindOf child =
      case (nRes child, M.null (nKids child)) of
        (Just _, True) -> HasRes
        (Just _, False) -> Both
        (Nothing, _) -> HasKids

--------------------------------------------------------------------------------
-- Demo
--------------------------------------------------------------------------------

-- A synthetic USB service: paths like /0/config, /0/status, /1/config
-- exist only as far as this actor is willing to answer for them.
usbService :: Service
usbService =
  Service
    { svcName = "usb",
      svcResolve = \case
        [dev, "config"] | dev `elem` ["0", "1"] -> Just (Blob ("usb" ++ dev ++ " configuration"))
        [dev, "status"] | dev `elem` ["0", "1"] -> Just (Blob ("usb" ++ dev ++ ": ok"))
        _ -> Nothing,
      svcList = \case
        [] -> Just ["0", "1"]
        [dev] | dev `elem` ["0", "1"] -> Just ["config", "status"]
        _ -> Nothing
    }

world :: Node
world =
  bind "/readme" [] (Blob "welcome") $
    bind "/svc/usb" [] (Actor usbService) $
      bind "/project/x/notes" [Cap "x"] (Blob "x planning notes") $
        bind "/project/x/build/out" [Cap "x"] (Blob "artifact") $
          bind "/a/very/deep/leaf" [] (Blob "no ceremony required") $
            emptyN

-- /svc/usb is both a resource (the Actor) and, via delegation, a "directory".
-- /project/x, /project, /a, /a/very, ... are bare spine: they were never
-- created, they have no owner, they are only visible through prefix queries.

showRes :: Maybe Resource -> String
showRes = maybe "DENIED / not found" describe

showLs :: Maybe [(Seg, Kind)] -> String
showLs Nothing = "not enumerable / not found"
showLs (Just xs) = intercalate "  " [s ++ tag k | (s, k) <- xs]
  where
    tag HasRes = ""; tag HasKids = "/"; tag Both = "/*"

main :: IO ()
main = do
  let anon = S.empty
      dev = S.fromList [Cap "x"]

  putStrLn "== URL semantics: direct resolution, no intermediate existence =="
  putStrLn $ "  /a/very/deep/leaf        -> " ++ showRes (resolve anon "/a/very/deep/leaf" world)
  putStrLn $ "  /svc/usb/0/config        -> " ++ showRes (resolve anon "/svc/usb/0/config" world)
  putStrLn $ "  /svc/usb/9/config        -> " ++ showRes (resolve anon "/svc/usb/9/config" world)
  putStrLn $ "  /svc/usb  (the actor)    -> " ++ showRes (resolve anon "/svc/usb" world)

  putStrLn "\n== Directory semantics: listings synthesized from prefixes =="
  putStrLn $ "  ls /                     -> " ++ showLs (ls anon "/" world)
  putStrLn $ "  ls /a/very               -> " ++ showLs (ls anon "/a/very" world)
  putStrLn $ "  ls /svc/usb  (delegated) -> " ++ showLs (ls anon "/svc/usb" world)
  putStrLn $ "  ls /svc/usb/0            -> " ++ showLs (ls anon "/svc/usb/0" world)

  putStrLn "\n== Capability consistency: ls filters with the SAME predicate as resolve =="
  putStrLn $ "  [anon] resolve /project/x/notes -> " ++ showRes (resolve anon "/project/x/notes" world)
  putStrLn $ "  [anon] ls /                     -> " ++ showLs (ls anon "/" world)
  putStrLn $ "  [anon] ls /project              -> " ++ showLs (ls anon "/project" world)
  putStrLn $ "  [dev ] resolve /project/x/notes -> " ++ showRes (resolve dev "/project/x/notes" world)
  putStrLn $ "  [dev ] ls /                     -> " ++ showLs (ls dev "/" world)
  putStrLn $ "  [dev ] ls /project/x            -> " ++ showLs (ls dev "/project/x" world)
