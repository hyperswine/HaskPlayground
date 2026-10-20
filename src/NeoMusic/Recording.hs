-- | Recordings: WAV files and SFZ instrument mappings (see docs/2026-10-14-NEOMUSIC-SAMPLES.md).
--
-- A 'Recording' is read in two steps: its header at once (so a missing or
-- unreadable file is an error before anything renders), and its audio only
-- when a note first plays it. A 'SampleSet' is an instrument: regions, each a
-- recording with the pitch it was recorded at and the keys, velocities and
-- take it answers to.
module NeoMusic.Recording
  ( Recording(..), Region(..), SampleSet(..)
  , readRecording, readSampleSet, parseSfz, findFile, chooseRegions, recordedPitch
  ) where

import Control.Exception (IOException, try)
import Control.Monad (foldM, forM, mfilter, unless, when)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BU
import Data.Char (isAlphaNum, isDigit, isSpace, toLower)
import Data.Int (Int32)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32, Word64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import System.Directory (doesFileExist)
import System.FilePath ((</>), isAbsolute, takeDirectory)
import System.IO (Handle, IOMode (ReadMode), SeekMode (AbsoluteSeek, RelativeSeek), hFileSize, hSeek, hTell, withBinaryFile)
import System.IO.Unsafe (unsafeInterleaveIO)

-- | A recorded sound: its rate, length in frames, loop (from the file's
-- @smpl@ chunk), and one vector per channel, read on first use.
data Recording = Recording
  { recordingPath :: FilePath, recordingRate :: Double, recordingFrames :: Int
  , recordingLoop :: Maybe (Int, Int), recordingAudio :: [VU.Vector Float] }

-- | Two recordings are the same file with the same shape (never comparing
-- audio, which would read it).
instance Eq Recording where
  a == b = shape a == shape b
    where shape r = (recordingPath r, recordingRate r, recordingFrames r, recordingLoop r, length (recordingAudio r))
instance Show Recording where
  show r = "Recording " ++ show (recordingPath r)

-- | One region of an instrument. 'regionPitch' is the pitch the recording
-- sounds at, as a MIDI number (its key centre less its tune).
data Region = Region
  { regionRecording :: Recording, regionPitch :: Double
  , regionKeys :: (Int, Int), regionVelocities :: (Int, Int)
  , regionGain :: Double                  -- ^ linear, from @volume@ in dB
  , regionTake :: (Int, Int)              -- ^ @(seq_length, seq_position)@
  , regionLoop :: Maybe (Int, Int)        -- ^ loop start and end frames (inclusive)
  , regionSustainLoop :: Bool }           -- ^ loop only until note-off
  deriving (Eq, Show)

data SampleSet = SampleSet { setPath :: FilePath, setRegions :: [Region] } deriving (Eq, Show)

-- | The pitch a region's recording plays at, in Hz.
recordedPitch :: Region -> Double
recordedPitch r = 440 * 2 ** ((regionPitch r - 69) / 12)

-- | The regions that answer a note: its key (the pitch reaching the
-- recordings at note-on, rounded), its velocity (0..1 as 1..127), and its take
-- (how many times this pitch has played before). Velocity layers switch at
-- their boundaries, as in SFZ; regions with the same ranges all play.
chooseRegions :: SampleSet -> Int -> Double -> Double -> [Region]
chooseRegions set takeNumber vel hz =
  [ r | r <- setRegions set
      , let (lo, hi) = regionKeys r
            (vlo, vhi) = regionVelocities r
            (len, pos) = regionTake r
      , key >= lo, key <= hi, v >= vlo, v <= vhi
      , len <= 1 || takeNumber `mod` len + 1 == pos ]
  where
    key = round (69 + 12 * logBase 2 (hz / 440)) :: Int
    v = max 1 (round (vel * 127)) :: Int

-- WAV -------------------------------------------------------------------------

data Header = Header
  { headerFormat :: Int, headerChannels :: Int, headerRate :: Int, headerBits :: Int
  , headerOffset :: Integer, headerSize :: Int, headerLoop :: Maybe (Int, Int) }

-- | Read a WAV file's header now and its audio when first used: PCM at 16, 24
-- or 32 bits, or float at 32 or 64 bits, any channel count and rate.
readRecording :: FilePath -> IO (Either String Recording)
readRecording path = do
  result <- try (withBinaryFile path ReadMode scan) :: IO (Either IOException (Either String Header))
  case result of
    Left e -> pure (Left (path ++ ": " ++ show e))
    Right (Left e) -> pure (Left (path ++ ": " ++ e))
    Right (Right h) -> do
      -- Read once, on first use, and shared by the channels.
      bytes <- unsafeInterleaveIO $ withBinaryFile path ReadMode $ \handle -> do
        hSeek handle AbsoluteSeek (headerOffset h)
        BS.hGet handle (headerSize h)
      let width = headerBits h `div` 8
          frames = headerSize h `div` (width * headerChannels h)
      pure (Right (Recording path (fromIntegral (headerRate h)) frames (headerLoop h)
        [decodeChannel h frames bytes c | c <- [0 .. headerChannels h - 1]]))

scan :: Handle -> IO (Either String Header)
scan handle = do
  riff <- BS.hGet handle 12
  size <- hFileSize handle
  if BS.length riff < 12 || BS.take 4 riff /= str "RIFF" || BS.drop 8 riff /= str "WAVE"
    then pure (Left "not a RIFF WAVE file")
    else chunks size Nothing Nothing Nothing
  where
    chunks size fmt dat loop = do
      here <- hTell handle
      if here + 8 > size then finish fmt dat loop else do
        h <- BS.hGet handle 8
        let cid = BS.take 4 h
            len = fromIntegral (word32 h 4) :: Integer
            next = here + 8 + len + (len `mod` 2)
        if cid == str "fmt " then do
          body <- BS.hGet handle (fromIntegral (min len 40))
          hSeek handle AbsoluteSeek next
          chunks size (Just body) dat loop
        else if cid == str "data" then do
          hSeek handle AbsoluteSeek (min size next)
          chunks size fmt (Just (here + 8, fromIntegral (min len (size - here - 8)))) loop
        else if cid == str "smpl" then do
          body <- BS.hGet handle (fromIntegral (min len 60))
          hSeek handle AbsoluteSeek next
          let loops = if BS.length body >= 36 then word32 body 28 else 0
              found = if loops > 0 && BS.length body >= 60
                        then Just (fromIntegral (word32 body 44), fromIntegral (word32 body 48)) else Nothing
          chunks size fmt dat (maybe loop Just found)
        else do
          hSeek handle RelativeSeek (len + (len `mod` 2))
          chunks size fmt dat loop
    finish fmt dat loop = pure $ case (fmt, dat) of
      (Nothing, _) -> Left "no fmt chunk"
      (_, Nothing) -> Left "no data chunk"
      (Just body, Just (offset, len)) -> do
        unless (BS.length body >= 16) (Left "short fmt chunk")
        let tag = word16 body 0
            -- WAVE_FORMAT_EXTENSIBLE names its real format in the subformat.
            format = if tag == 0xFFFE && BS.length body >= 26 then word16 body 24 else tag
            channels = word16 body 2
            rate = fromIntegral (word32 body 4)
            bits = word16 body 14
        unless ((format == 1 && bits `elem` [16, 24, 32]) || (format == 3 && bits `elem` [32, 64]))
          (Left ("unsupported sample format " ++ show format ++ " at " ++ show bits ++ " bits"))
        unless (channels >= 1 && rate > 0) (Left "invalid channel count or rate")
        Right (Header format channels rate bits offset len loop)
    str = BS.pack . map (fromIntegral . fromEnum)

word16 :: BS.ByteString -> Int -> Int
word16 b i = fromIntegral (BU.unsafeIndex b i) .|. (fromIntegral (BU.unsafeIndex b (i + 1)) `shiftL` 8)

word32 :: BS.ByteString -> Int -> Word32
word32 b i = foldr (\k acc -> (acc `shiftL` 8) .|. fromIntegral (BU.unsafeIndex b (i + k))) 0 [0 .. 3]

-- | One channel of interleaved samples, as floats in -1..1.
decodeChannel :: Header -> Int -> BS.ByteString -> Int -> VU.Vector Float
decodeChannel h frames bytes c = VU.generate frames sample
  where
    width = headerBits h `div` 8
    stride = width * headerChannels h
    byte i = fromIntegral (BU.unsafeIndex bytes i) :: Int
    sample k =
      let i = k * stride + c * width
      in case (headerFormat h, headerBits h) of
           (1, 16) -> let v = byte i .|. (byte (i + 1) `shiftL` 8)
                      in fromIntegral (if v >= 32768 then v - 65536 else v) / 32768
           (1, 24) -> let v = byte i .|. (byte (i + 1) `shiftL` 8) .|. (byte (i + 2) `shiftL` 16)
                      in fromIntegral (if v >= 8388608 then v - 16777216 else v) / 8388608
           (1, _) -> fromIntegral (fromIntegral (word32 bytes i) :: Int32) / 2147483648
           (_, 32) -> castWord32ToFloat (word32 bytes i)
           _ -> realToFrac (castWord64ToDouble (fromIntegral (word32 bytes i) .|. (fromIntegral (word32 bytes (i + 4)) `shiftL` 32 :: Word64)))

-- SFZ -------------------------------------------------------------------------

-- | The first of: the path itself if absolute, relative to @here@, then under
-- each search directory.
findFile :: FilePath -> [FilePath] -> FilePath -> IO (Maybe FilePath)
findFile here dirs path
  | isAbsolute path = present [path]
  | otherwise = present ((here </> path) : [d </> path | d <- dirs])
  where
    present [] = pure Nothing
    present (p : ps) = doesFileExist p >>= \ok -> if ok then pure (Just p) else present ps

-- | An SFZ file's regions, each as its opcodes (group and control opcodes
-- included). The supported subset is in docs/2026-10-14-NEOMUSIC-SAMPLES.md; any other opcode
-- or header is an error naming it.
parseSfz :: String -> Either String [M.Map String String]
parseSfz text = do
  items <- concat <$> mapM tokens (zip [1 :: Int ..] (lines text))
  let step (control, group, current, done) item = case item of
        Left "control" -> Right (control, M.empty, Nothing, flush current done)
        Left "group" -> Right (control, M.empty, Nothing, flush current done)
        Left "region" -> Right (control, group, Just M.empty, flush current done)
        Left other -> Left ("unsupported SFZ header <" ++ other ++ ">")
        Right (k, v) -> do
          unless (k `elem` supported) (Left ("unsupported SFZ opcode " ++ k))
          Right $ case current of
            Just r -> (control, group, Just (M.insert k v r), done)
            Nothing | k == "default_path" -> (M.insert k v control, group, current, done)
                    | otherwise -> (control, M.insert k v group, current, done)
        where flush r acc = maybe acc (\x -> (x, control, group) : acc) r
  (control, group, current, done) <- foldM step (M.empty, M.empty, Nothing, []) items
  let regions = reverse (maybe done (\x -> (x, control, group) : done) current)
  pure [M.unions [r, g, c] | (r, c, g) <- regions]
  where
    supported = ["sample", "default_path", "pitch_keycenter", "key", "lokey", "hikey", "lovel", "hivel"
                , "tune", "volume", "loop_mode", "loop_start", "loop_end", "seq_length", "seq_position"]
    -- Headers, and opcodes whose values run to the next opcode or header.
    tokens (_, raw) = Right (go (dropWhile isSpace (stripComment raw)))
    stripComment s = case breakOn "//" s of (a, _) -> a
    go [] = []
    go ('<' : rest) = let (name, after) = break (== '>') rest in Left name : go (dropWhile isSpace (drop 1 after))
    go s = let (k, afterKey) = break (== '=') s
               (v, after) = valueUntilNext (drop 1 afterKey)
           in if null afterKey then [] else Right (trim k, trim v) : go (dropWhile isSpace after)
    valueUntilNext s = scanValue "" s
    scanValue acc [] = (reverse acc, [])
    scanValue acc rest@(c : cs)
      | c == '<' = (reverse acc, rest)
      | isSpace c, isOpcodeAhead (dropWhile isSpace cs) = (reverse acc, dropWhile isSpace cs)
      | otherwise = scanValue (c : acc) cs
    isOpcodeAhead s = let (w, after) = span (\x -> isAlphaNum x || x == '_') s in not (null w) && take 1 after == "="
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    breakOn pat s = case [i | i <- [0 .. length s - length pat], pat `isPrefixOf` drop i s] of
      (i : _) -> splitAt i s
      [] -> (s, "")

-- | Read an instrument mapping and every recording it names (headers only).
-- A sample is found relative to the SFZ file (after @default_path@), then
-- under each search directory.
readSampleSet :: [FilePath] -> FilePath -> IO (Either String SampleSet)
readSampleSet dirs path = do
  text <- try (readFile path >>= \t -> length t `seq` pure t) :: IO (Either IOException String)
  case either (Left . show) parseSfz text of
    Left e -> pure (Left (path ++ ": " ++ e))
    Right regions -> do
      built <- forM regions $ \ops -> case regionShape ops of
        Left e -> pure (Left e)
        Right shape -> do
          let sample = map slash (M.findWithDefault "" "default_path" ops ++ M.findWithDefault "" "sample" ops)
          found <- findFile (takeDirectory path) dirs sample
          case found of
            Nothing -> pure (Left ("recording not found: " ++ sample))
            Just file -> fmap shape <$> readRecording file
      pure $ case sequence built of
        Left e -> Left (path ++ ": " ++ e)
        Right [] -> Left (path ++ ": no regions")
        Right rs -> Right (SampleSet path rs)
  where slash ch = if ch == '\\' then '/' else ch

-- | A region from its opcodes, given its recording.
regionShape :: M.Map String String -> Either String (Recording -> Region)
regionShape ops = do
  let get k = M.lookup k ops
      number k = maybe (Right Nothing) (fmap Just . readNumber k) (get k)
      note k = maybe (Right Nothing) (fmap Just . readNote k) (get k)
  when (M.notMember "sample" ops) (Left "a region needs a sample")
  key <- note "key"
  centre <- maybe (maybe (Right 60) Right key) (readNote "pitch_keycenter") (get "pitch_keycenter")
  lo <- fromMaybe (fromMaybe 0 key) <$> note "lokey"
  hi <- fromMaybe (fromMaybe 127 key) <$> note "hikey"
  vlo <- maybe 1 round <$> number "lovel"
  vhi <- maybe 127 round <$> number "hivel"
  tune <- fromMaybe 0 <$> number "tune"
  volume <- fromMaybe 0 <$> number "volume"
  len <- maybe 1 round <$> number "seq_length"
  pos <- maybe 1 round <$> number "seq_position"
  start <- fmap round <$> number "loop_start"
  end <- fmap round <$> number "loop_end"
  mode <- case get "loop_mode" of
    Nothing -> Right Nothing
    Just m | m `elem` ["no_loop", "one_shot", "loop_continuous", "loop_sustain"] -> Right (Just m)
           | otherwise -> Left ("unsupported loop_mode " ++ m)
  unless (all (\k -> k >= 0 && k <= 127) [centre, lo, hi] && lo <= hi) (Left "keys must be within 0..127, lokey <= hikey")
  unless (vlo >= 1 && vhi <= 127 && vlo <= vhi) (Left "velocities must be within 1..127, lovel <= hivel")
  unless (len >= 1 && pos >= 1 && pos <= len) (Left "seq_position must be within 1..seq_length")
  pure $ \recording ->
    let fromFile = recordingLoop recording
        points = case (start, end) of
          (Just s, Just e) -> Just (s, e)
          _ -> fromFile
        looping = case mode of
          Nothing -> fromFile /= Nothing
          Just m -> m `elem` ["loop_continuous", "loop_sustain"]
        valid (s, e) = s >= 0 && e > s && e < recordingFrames recording
    in Region recording (fromIntegral centre - tune / 100) (lo, hi) (vlo, vhi) (10 ** (volume / 20)) (len, pos)
         (if looping then mfilter valid points else Nothing) (mode == Just "loop_sustain")

readNumber :: String -> String -> Either String Double
readNumber k s = case reads s :: [(Double, String)] of
  [(x, rest)] | all isSpace rest -> Right x
  _ -> case reads s :: [(Integer, String)] of
    [(x, rest)] | all isSpace rest -> Right (fromIntegral x)
    _ -> Left (k ++ ": not a number: " ++ s)

-- | A MIDI key: a number, or a name such as @c4@ or @f#3@ (C4 = 60).
readNote :: String -> String -> Either String Int
readNote k s = case s of
  (c : rest) | toLower c `elem` "cdefgab", not (null rest) ->
    let base = M.fromList (zip "cdefgab" [0, 2, 4, 5, 7, 9, 11]) M.! toLower c
        (accidental, octave) = case rest of
          ('#' : o) -> (1, o)
          ('b' : o) | not (null o) -> (-1, o)
          o -> (0, o)
    in case reads octave :: [(Int, String)] of
         [(o, "")] -> Right (12 * (o + 1) + base + accidental)
         _ -> Left (k ++ ": not a note: " ++ s)
  _ | all (\ch -> isDigit ch || ch == '-') s, not (null s) -> Right (read s)
  _ -> Left (k ++ ": not a note: " ++ s)
