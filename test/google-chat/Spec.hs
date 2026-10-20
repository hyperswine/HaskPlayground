{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (unless)
import Data.Aeson (object, (.=))
import GoogleChat
import System.Exit (die)

main :: IO ()
main = do
  let check label ok = unless ok (die label)
  check "valid space" (validateSpace "spaces/AAA_ab-12")
  mapM_ (\s -> check ("unsafe space " <> s) (not (validateSpace s)))
    ["spaces/", "spaces/a/messages", "spaces/..", "spaces/a?x=1", "https://example.com", "spaces/é"]
  check "valid callback" (validateCallback "random" "/oauth/callback?state=random&code=abc%2B123" == Right "abc+123")
  mapM_ (\q -> check "rejected unsafe callback" (either (const True) (const False) (validateCallback "random" q)))
    ["/oauth/callback?state=wrong&code=abc", "/oauth/callback?code=abc", "/oauth/callback?state=random&state=random&code=abc"
    ,"/oauth/callback?state=random&code=a&code=b", "/oauth/callback?state=random&error=access_denied", "/other?state=random&code=a"]
  check "spaces minimum permissions" (maybe False ((==3) . length) (scopesFor "spaces"))
  check "invalid scope mode" (scopesFor "admin" == Nothing)
  let rejected value = either (const True) (const False) (validateReadQuery value)
  check "MCP reader rejects writes" (rejected (object ["action" .= ("send" :: String)]))
  check "MCP reader rejects extra arguments" (rejected (object ["action" .= ("spaces" :: String), "url" .= ("https://evil.example" :: String)]))
  check "MCP reader bounds pages" (rejected (object ["action" .= ("spaces" :: String), "limit" .= (101 :: Int)]))
  check "MCP reader rejects path traversal" (rejected (object ["action" .= ("message" :: String), "message" .= ("spaces/AAA/messages/.." :: String)]))
  check "MCP reader rejects thread filter injection" (rejected (object ["action" .= ("thread" :: String), "thread" .= ("spaces/AAA/threads/x OR true" :: String)]))
  let expectPath value expected = case validateReadQuery value of
        Right (ReadQuery path _ _) -> path == expected
        Left _ -> False
  check "MCP reader space list" (expectPath (object ["action" .= ("spaces" :: String)]) "spaces")
  check "MCP reader message lookup" (expectPath (object ["action" .= ("message" :: String), "message" .= ("spaces/AAA/messages/a.b" :: String)]) "spaces/AAA/messages/a.b")
  check "MCP reader thread uses bounded message endpoint" (expectPath (object ["action" .= ("thread" :: String), "thread" .= ("spaces/AAA/threads/thread" :: String)]) "spaces/AAA/messages")
  let sendArgs space text rid = object ["space" .= (space :: String), "text" .= (text :: String), "request_id" .= (rid :: String)]
      rid = "a1234567-1234-4234-8234-123456789abc"
      badSend q = either (const True) (const False) (validateSendQuery q)
  check "sender valid exact text" (validateSendQuery (sendArgs "spaces/AAA" "hello" rid) == Right ("spaces/AAA", "hello", "a1234567-1234-4234-8234-123456789abc"))
  check "sender blocks traversal" (badSend (sendArgs "spaces/../evil" "hello" rid))
  check "sender blocks empty text" (badSend (sendArgs "spaces/AAA" "  " rid))
  check "sender bounds UTF8 bytes" (badSend (sendArgs "spaces/AAA" (replicate 7001 '😀') rid))
  check "sender requires retry UUID" (badSend (sendArgs "spaces/AAA" "hello" "invalid"))
  check "sender rejects arbitrary arguments" (badSend (object ["space" .= ("spaces/AAA" :: String), "text" .= ("hello" :: String), "request_id" .= rid, "url" .= ("evil" :: String)]))
  putStrLn "Google Chat validation checks passed."
