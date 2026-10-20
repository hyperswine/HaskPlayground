{-# LANGUAGE OverloadedStrings #-}
module GoogleChat
  ( run, validateSpace, validateCallback, scopesFor, redirectUri
  , ReadQuery(..), validateReadQuery, readOnlyRequest, validateSendQuery, sendOnlyRequest ) where

import Control.Exception (bracket, catch, IOException)
import Control.Monad (unless, when)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client hiding (path)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusCode, parseQuery, renderQuery, urlEncode)
import Network.Socket
import qualified Network.Socket.ByteString as S
import Numeric (showHex)
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath ((</>), takeDirectory)
import System.IO hiding (utf8)
import System.Posix.Files (setFileMode)
import System.Process (callProcess)
import System.Timeout (timeout)

data Client = Client { clientId :: Text, clientSecret :: Text }
instance FromJSON Client where
  parseJSON = withObject "OAuth client JSON" $ \o -> do
    c <- o .: "web" -- This gateway uses the existing Web application client.
    Client <$> c .: "client_id" <*> c .: "client_secret"

data Session = Session { refreshToken :: Text, sessionScopes :: Text, sessionEmail :: Text }
instance FromJSON Session where
  parseJSON = withObject "session" $ \o -> Session <$> o .: "refresh_token" <*> o .: "scope" <*> o .: "email"
instance ToJSON Session where
  toJSON s = object ["refresh_token" .= refreshToken s, "scope" .= sessionScopes s, "email" .= sessionEmail s]

redirectUri :: B.ByteString
redirectUri = "http://localhost:8765/oauth/callback"

scopesFor :: String -> Maybe [Text]
scopesFor mode = case mode of
  "spaces" -> Just ["openid", "email", prefix <> "chat.spaces.readonly"]
  "read" -> (<> [prefix <> "chat.messages.readonly"]) <$> scopesFor "spaces"
  "send" -> (<> [prefix <> "chat.messages.create"]) <$> scopesFor "read"
  _ -> Nothing
  where prefix = "https://www.googleapis.com/auth/"

validateSpace :: String -> Bool
validateSpace s = case stripPrefix "spaces/" s of
  Just ident -> not (null ident) && all (\c -> isAlphaNum c && c < '\128' || c `elem` ("_-" :: String)) ident
  Nothing -> False
  where stripPrefix p x | take (length p) x == p = Just (drop (length p) x)
                        | otherwise = Nothing

validateCallback :: B.ByteString -> B.ByteString -> Either String B.ByteString
validateCallback expected target = do
  let (path, query) = C.break (== '?') target
      params = parseQuery query
      values k = [v | (k', Just v) <- params, k' == k]
  unless (path == "/oauth/callback") (Left "Wrong callback path")
  unless (values "state" == [expected]) (Left "OAuth state mismatch")
  when (not (null (values "error"))) (Left "Google authorization was declined")
  case values "code" of
    [code] | not (B.null code) -> Right code
    _ -> Left "Missing or duplicate authorization code"

privateWrite :: FilePath -> L.ByteString -> IO ()
privateWrite path bytes = do
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  setFileMode dir 0o700
  bracket (openBinaryTempFile dir ".session")
    (\(tmp,h) -> do hClose h `catch` ignore; exists <- doesFileExist tmp; when exists (removeFile tmp))
    (\(tmp,h) -> do setFileMode tmp 0o600; L.hPut h bytes; hClose h; renameFile tmp path)
  where ignore :: IOException -> IO (); ignore _ = pure ()

loadJson :: FromJSON a => FilePath -> IO a
loadJson path = do
  exists <- doesFileExist path
  unless exists (die ("Missing private file: " <> path))
  setFileMode path 0o600
  bytes <- L.readFile path
  case eitherDecode bytes of
    Left _ -> die ("Invalid JSON or unexpected format in " <> path)
    Right x -> pure x

-- Never print HTTP exceptions: they can contain authorization headers or form bodies.
requestJson :: Manager -> Request -> IO Value
requestJson manager req = do
  response <- httpLbs req { checkResponse = \_ _ -> pure (), redirectCount = 0 } manager
  let code = statusCode (responseStatus response)
  unless (code >= 200 && code < 300) $ do
    let reason = case eitherDecode (responseBody response) :: Either String Value of
          Right value -> fromMaybe "request rejected" $ parseMaybe (withObject "error" $ \o -> do
            err <- o .: "error"
            case err of
              String s -> pure s
              Object e -> e .:? "status" .!= "request rejected"
              _ -> pure "request rejected") value
          Left _ -> "non-JSON response"
    die ("Google request failed (HTTP " <> show code <> ", " <> T.unpack reason <> "). Check API enablement, consent/scopes, Workspace policy, or sign in again.")
  either (const (die "Google returned invalid JSON")) pure (eitherDecode (responseBody response))

field :: FromJSON a => Text -> Value -> IO a
field name value = either (const (die ("Google response missing " <> T.unpack name))) pure
  (parseEither (withObject "response" (\o -> o .: fromStringKey name)) value)
  where fromStringKey = Key.fromText

form :: Manager -> [(B.ByteString, B.ByteString)] -> IO Value
form manager params = do
  req <- parseRequest "https://oauth2.googleapis.com/token"
  requestJson manager (urlEncodedBody params req)

utf8 :: Text -> B.ByteString
utf8 = T.encodeUtf8

credentials :: Client -> [(B.ByteString, B.ByteString)]
credentials c = [("client_id", utf8 (clientId c)), ("client_secret", utf8 (clientSecret c))]

bearer :: Text -> Request -> Request
bearer token req = req { requestHeaders = [("Authorization", "Bearer " <> utf8 token), ("Accept", "application/json")] }

verifyIdentity :: Manager -> Text -> Text -> IO ()
verifyIdentity manager access expected = do
  req <- parseRequest "https://openidconnect.googleapis.com/v1/userinfo"
  info <- requestJson manager (bearer access req)
  email <- field "email" info
  verified <- field "email_verified" info
  unless (verified && T.toCaseFold email == T.toCaseFold expected) (die "Signed-in account does not match the expected verified email; session was not saved.")

randomState :: IO B.ByteString
randomState = withBinaryFile "/dev/urandom" ReadMode $ \h -> do
  bytes <- B.hGet h 32
  unless (B.length bytes == 32) (die "Could not generate OAuth state")
  pure (C.pack (concatMap (\x -> let s = showHex x "" in replicate (2-length s) '0' <> s) (B.unpack bytes)))

login :: Manager -> Client -> FilePath -> Text -> String -> IO ()
login manager client sessionPath email mode = do
  scopes <- maybe (die "Use login spaces, login read, or login send") pure (scopesFor mode)
  state <- randomState
  let url = "https://accounts.google.com/o/oauth2/v2/auth" <> C.unpack (renderQuery True
        [("client_id", Just (utf8 (clientId client))), ("redirect_uri", Just redirectUri)
        ,("response_type", Just "code"), ("scope", Just (utf8 (T.unwords scopes)))
        ,("state", Just state), ("access_type", Just "offline"), ("prompt", Just "consent")
        ,("login_hint", Just (utf8 email))])
  withSocketsDo $ bracket (socket AF_INET Stream defaultProtocol) close $ \listener -> do
    setSocketOption listener ReuseAddr 1
    bind listener (SockAddrInet 8765 (tupleToHostAddress (127,0,0,1)))
    listen listener 4
    putStrLn "Opening Safari for Google consent. Waiting up to five minutes on localhost:8765."
    callProcess "open" ["-a", "Safari", url]
    result <- timeout (300 * 1000000) (receiveCode listener state)
    code <- maybe (die "Sign-in timed out; run login again") pure result
    tokens <- form manager (credentials client <> [("code",code),("grant_type","authorization_code"),("redirect_uri",redirectUri)])
    access <- field "access_token" tokens
    verifyIdentity manager access email
    refresh <- field "refresh_token" tokens
    granted <- field "scope" tokens
    unless (all (`elem` T.words granted) (filter (/= "email") scopes) &&
      ("email" `elem` T.words granted || "https://www.googleapis.com/auth/userinfo.email" `elem` T.words granted))
      (die "Required scopes were not granted; session was not saved")
    privateWrite sessionPath (encode (Session refresh granted email))
    putStrLn ("Signed in as " <> T.unpack email <> ". Private refresh token saved.")
  where
    receiveCode listener state = do
      (conn, _) <- accept listener
      result <- bracket (pure conn) close $ \sock -> do
        incoming <- timeout (5 * 1000000) (readHeaders sock B.empty)
        let checked = case incoming >>= requestTarget of
              Just target -> validateCallback state target
              Nothing -> Left "Invalid HTTP callback"
            body = case checked of
              Right _ -> "Authorization received. Return to the terminal for the final result."
              Left _ -> "Callback rejected. Return to the sign-in tab or restart login."
        S.sendAll sock ("HTTP/1.1 " <> either (const "400 Bad Request") (const "200 OK") checked <>
          "\r\nContent-Type: text/plain\r\nCache-Control: no-store\r\nReferrer-Policy: no-referrer\r\nConnection: close\r\nContent-Length: " <>
          C.pack (show (B.length body)) <> "\r\n\r\n" <> body)
        pure checked
      either (const (receiveCode listener state)) pure result
    readHeaders sock accumulated
      | B.length accumulated > 16384 = pure B.empty
      | "\r\n\r\n" `B.isInfixOf` accumulated = pure accumulated
      | otherwise = do
          chunk <- S.recv sock 4096
          if B.null chunk then pure B.empty else readHeaders sock (accumulated <> chunk)
    requestTarget bytes = case map C.words (take 1 (C.lines bytes)) of
      [["GET", target, "HTTP/1.1"]] -> Just target
      _ -> Nothing

accessToken :: Manager -> Client -> Session -> IO Text
accessToken manager client session = do
  tokens <- form manager (credentials client <> [("grant_type","refresh_token"),("refresh_token",utf8 (refreshToken session))])
  field "access_token" tokens

-- Separate entry point for the MCP reader: no write action or arbitrary URL.
data ReadQuery = ReadQuery String [(B.ByteString, B.ByteString)] Text
  deriving (Eq, Show)

validateReadQuery :: Value -> Either String ReadQuery
validateReadQuery = parseEither $ withObject "read-only request" $ \o -> do
  action <- o .: "action" :: Parser Text
  let allowed = case action of
        "spaces" -> ["action", "limit", "page_token"]
        "messages" -> ["action", "space", "limit", "page_token"]
        "thread" -> ["action", "thread", "limit", "page_token"]
        "message" -> ["action", "message"]
        _ -> []
  unless (not (null allowed) && all (`elem` allowed) (KM.keys o))
    (fail "Unknown action or unexpected arguments; only read operations are allowed")
  limit <- o .:? "limit" .!= (25 :: Int)
  unless (limit >= 1 && limit <= 100) (fail "limit must be between 1 and 100")
  page <- o .:? "page_token" .!= ""
  unless (T.length page <= 4096) (fail "page_token is too long")
  let pagination = [("pageSize", C.pack (show limit))] <>
        [("pageToken", utf8 page) | not (T.null page)]
      scope name = "https://www.googleapis.com/auth/" <> name
      resource kind name = case T.splitOn "/" name of
        ["spaces", sid, k, ident] -> k == kind && validateSpace (T.unpack ("spaces/" <> sid)) &&
          not (T.null ident) && T.all (\c -> (isAlphaNum c && c < '\128') || c `elem` ("_.-" :: String)) ident &&
          ident /= "." && ident /= ".."
        _ -> False
  case action of
    "spaces" -> pure (ReadQuery "spaces" pagination (scope "chat.spaces.readonly"))
    "messages" -> do
      space <- o .: "space"
      unless (validateSpace space) (fail "Expected spaces/SPACE_ID")
      pure (ReadQuery (space <> "/messages") (pagination <> [("orderBy", "createTime DESC")]) (scope "chat.messages.readonly"))
    "thread" -> do
      thread <- o .: "thread"
      unless (resource "threads" thread) (fail "Expected spaces/SPACE_ID/threads/THREAD_ID")
      let space = T.intercalate "/" (take 2 (T.splitOn "/" thread))
      pure (ReadQuery (T.unpack space <> "/messages")
        (pagination <> [("filter", utf8 ("thread.name = " <> thread)), ("orderBy", "createTime ASC")])
        (scope "chat.messages.readonly"))
    "message" -> do
      name <- o .: "message"
      unless (resource "messages" name) (fail "Expected spaces/SPACE_ID/messages/MESSAGE_ID")
      pure (ReadQuery (T.unpack name) [] (scope "chat.messages.readonly"))
    _ -> fail "Unsupported read operation"

readOnlyRequest :: Value -> IO Value
readOnlyRequest value = do
  ReadQuery path params scope <- either (const (die "Invalid read-only query")) pure (validateReadQuery value)
  dir <- fromMaybe ".private/google-chat" <$> lookupEnv "GOOGLE_CHAT_CONFIG_DIR"
  email <- T.pack . fromMaybe "support@vpply.com" <$> lookupEnv "GOOGLE_CHAT_EMAIL"
  client <- loadJson (dir </> "credentials.json")
  session <- loadJson (dir </> "session.json")
  unless (T.toCaseFold (sessionEmail session) == T.toCaseFold email)
    (die "Saved Google session belongs to another account; sign in again")
  unless (scope `elem` T.words (sessionScopes session))
    (die "Missing read scope. Run google-chat login read outside the MCP server")
  manager <- newManager tlsManagerSettings
  token <- accessToken manager client session
  verifyIdentity manager token email
  req <- parseRequest ("https://chat.googleapis.com/v1/" <> path)
  result <- requestJson manager (bearer token (setQueryString [(k,Just v) | (k,v) <- params] req))
  pure (object ["account" .= email, "data" .= result])

-- A separate, narrowly scoped writer; the event worker keeps using the reader.
validateSendQuery :: Value -> Either String (String, Text, Text)
validateSendQuery = parseEither $ withObject "send request" $ \o -> do
  unless (all (`elem` ["space", "text", "request_id"]) (KM.keys o)) (fail "Unexpected send arguments")
  space <- o .: "space"
  text <- o .: "text"
  requestId <- o .: "request_id"
  unless (validateSpace space) (fail "Invalid space")
  unless (not (T.null (T.strip text)) && B.length (utf8 text) <= 28000) (fail "Text must be nonblank and at most 28000 UTF-8 bytes")
  unless (map T.length (T.splitOn "-" requestId) == [8,4,4,4,12] &&
    T.all (\c -> c `elem` ("0123456789abcdefABCDEF-" :: String)) requestId) (fail "request_id must be a UUID")
  pure (space, text, requestId)

sendOnlyRequest :: Value -> IO Value
sendOnlyRequest value = do
  (space, text, requestId) <- either (const (die "Invalid send query")) pure (validateSendQuery value)
  dir <- fromMaybe ".private/google-chat" <$> lookupEnv "GOOGLE_CHAT_CONFIG_DIR"
  email <- T.pack . fromMaybe "support@vpply.com" <$> lookupEnv "GOOGLE_CHAT_EMAIL"
  client <- loadJson (dir </> "credentials.json")
  session <- loadJson (dir </> "session.json")
  unless (T.toCaseFold (sessionEmail session) == T.toCaseFold email) (die "Saved session belongs to another account")
  unless ("https://www.googleapis.com/auth/chat.messages.create" `elem` T.words (sessionScopes session))
    (die "Missing send scope. Run google-chat login send outside MCP")
  manager <- newManager tlsManagerSettings
  token <- accessToken manager client session
  verifyIdentity manager token email
  req <- parseRequest ("https://chat.googleapis.com/v1/" <> space <> "/messages")
  let authenticated = bearer token (setQueryString [("requestId", Just (utf8 requestId))] req)
  result <- requestJson manager authenticated { method = "POST",
    requestBody = RequestBodyLBS (encode (object ["text" .= text])),
    requestHeaders = ("Content-Type", "application/json") : requestHeaders authenticated }
  pure (object ["account" .= email, "data" .= result])

run :: [String] -> IO ()
run args = do
  let defaultDir = ".private/google-chat"
  dir <- fromMaybe defaultDir <$> lookupEnv "GOOGLE_CHAT_CONFIG_DIR"
  email <- T.pack . fromMaybe "support@vpply.com" <$> lookupEnv "GOOGLE_CHAT_EMAIL"
  manager <- newManager tlsManagerSettings
  case args of
    ["help"] -> usage
    [] -> usage
    ["login", mode] -> do
      client <- loadJson (dir </> "credentials.json")
      login manager client (dir </> "session.json") email mode
    ["status"] -> do
      session <- loadJson (dir </> "session.json")
      putStrLn ("Account: " <> T.unpack (sessionEmail session))
      putStrLn ("Scopes: " <> T.unpack (sessionScopes session))
    ["spaces"] -> authenticated manager dir email $ \token session -> do
      requireScope session "chat.spaces.readonly"
      listAll manager token "spaces" "https://chat.googleapis.com/v1/spaces?pageSize=100" Nothing
    ["messages", space] | validateSpace space -> authenticated manager dir email $ \token session -> do
      requireScope session "chat.messages.readonly"
      listAll manager token "messages" ("https://chat.googleapis.com/v1/" <> space <> "/messages?pageSize=100") Nothing
    ["send", space, message] | validateSpace space && not (null message) -> authenticated manager dir email $ \token session -> do
      requireScope session "chat.messages.create"
      req <- parseRequest ("https://chat.googleapis.com/v1/" <> space <> "/messages")
      value <- requestJson manager ((bearer token req) {method="POST", requestBody=RequestBodyLBS (encode (object ["text" .= message])), requestHeaders=("Content-Type","application/json") : requestHeaders (bearer token req)})
      L.putStrLn (encode value)
    _ -> usage
  where
    usage = putStrLn "google-chat login spaces|read|send\ngoogle-chat status\ngoogle-chat spaces\ngoogle-chat messages spaces/SPACE_ID\ngoogle-chat send spaces/SPACE_ID 'message'\nConfig: GOOGLE_CHAT_CONFIG_DIR (default .private/google-chat); GOOGLE_CHAT_EMAIL (default support@vpply.com)."
    requireScope session name = unless (("https://www.googleapis.com/auth/" <> name) `elem` T.words (sessionScopes session))
      (die "Missing scope. Run login read or login send to request the required permission.")
    authenticated manager dir email action = do
      client <- loadJson (dir </> "credentials.json")
      session <- loadJson (dir </> "session.json")
      unless (T.toCaseFold (sessionEmail session) == T.toCaseFold email) (die "Saved session belongs to another account; run login again")
      token <- accessToken manager client session
      verifyIdentity manager token email
      action token session

listAll :: Manager -> Text -> Text -> String -> Maybe Text -> IO ()
listAll manager token key url page = do
  req <- parseRequest (url <> maybe "" (\p -> "&pageToken=" <> C.unpack (urlEncode True (utf8 p)) ) page)
  value <- requestJson manager (bearer token req)
  let items = fromMaybe [] (parseMaybe (withObject "list" (\o -> o .:? Key.fromText key .!= [])) value) :: [Value]
  mapM_ (L.putStrLn . encode) items
  case parseMaybe (withObject "list" (\o -> o .:? "nextPageToken")) value of
    Just (Just next) | not (T.null next) -> listAll manager token key url (Just next)
    _ -> when (null items && page == Nothing) (putStrLn "No results.")
