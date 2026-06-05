{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies #-}

module LinFile
  ( LinearFile
  , Mode(..)
  , withFile
  , hRead
  , hReadLine
  , hWrite
  , hWriteLine
  , hClose
  , hSeek
  , hTell
  , hIsEOF
  -- Re-export for threading
  , Ur(..)
  ) where

import Prelude.Linear hiding (Eq(..))
import Prelude (Eq(..), Show(..))
import Control.Functor.Linear   ((<$>))
import qualified System.IO      as IO
import qualified Control.Exception as E
import Data.Text (Text)
import qualified Data.Text.IO   as TIO
import GHC.IO (IO(..))

-- | A file handle that must be consumed linearly — it cannot be
-- duplicated or silently dropped. Every code path must end in hClose.
newtype LinearFile = LinearFile IO.Handle

data Mode = ReadMode | WriteMode | AppendMode | ReadWriteMode
  deriving stock (Eq, Show)

toIOMode :: Mode -> IO.IOMode
toIOMode ReadMode      = IO.ReadMode
toIOMode WriteMode     = IO.WriteMode
toIOMode AppendMode    = IO.AppendMode
toIOMode ReadWriteMode = IO.ReadWriteMode

-- ---------------------------------------------------------------------------
-- Core bracket: the ONLY way to open a file
-- ---------------------------------------------------------------------------

-- | Open a file and run a linear continuation over it.
-- The continuation must consume the LinearFile exactly once (via hClose).
-- The underlying handle is guaranteed to be closed even on exceptions.
--
-- The type:
--   (LinearFile %1 -> IO (Ur a))
--
-- means the function must use the file handle linearly — GHC will reject
-- programs that drop or duplicate it. Ur ("unrestricted") wraps the result
-- so it can escape the linear scope.
withFile :: FilePath -> Mode -> (LinearFile %1 -> IO (Ur a)) -> IO a
withFile path mode k =
  E.bracket
    (IO.openFile path (toIOMode mode))
    (\h -> IO.hClose h)
    (\h -> do
        Ur result <- k (LinearFile h)
        pure result)

-- ---------------------------------------------------------------------------
-- Consuming operations — each takes the file linearly and returns it linearly
-- so the handle is threaded through every operation in sequence.
-- ---------------------------------------------------------------------------

-- | Read the entire remaining contents of the file.
hRead :: LinearFile %1 -> IO (Ur Text, LinearFile)
hRead (LinearFile h) = do
  contents <- TIO.hGetContents h
  pure (Ur contents, LinearFile h)

-- | Read a single line.
hReadLine :: LinearFile %1 -> IO (Ur Text, LinearFile)
hReadLine (LinearFile h) = do
  line <- TIO.hGetLine h
  pure (Ur line, LinearFile h)

-- | Write text to the file.
hWrite :: LinearFile %1 -> Text -> IO LinearFile
hWrite (LinearFile h) txt = do
  TIO.hPutStr h txt
  pure (LinearFile h)

-- | Write a line (appends newline).
hWriteLine :: LinearFile %1 -> Text -> IO LinearFile
hWriteLine (LinearFile h) txt = do
  TIO.hPutStrLn h txt
  pure (LinearFile h)

-- | Seek to a byte offset. SeekMode mirrors System.IO.
hSeek :: LinearFile %1 -> IO.SeekMode -> Integer -> IO LinearFile
hSeek (LinearFile h) mode offset = do
  IO.hSeek h mode offset
  pure (LinearFile h)

-- | Return the current byte offset without consuming the file.
hTell :: LinearFile %1 -> IO (Ur Integer, LinearFile)
hTell (LinearFile h) = do
  pos <- IO.hTell h
  pure (Ur pos, LinearFile h)

-- | Check whether we're at end-of-file.
hIsEOF :: LinearFile %1 -> IO (Ur Bool, LinearFile)
hIsEOF (LinearFile h) = do
  eof <- IO.hIsEOF h
  pure (Ur eof, LinearFile h)

-- | Close the file. This is the ONLY way to consume a LinearFile —
-- the linear type system ensures every branch of your code reaches here.
hClose :: LinearFile %1 -> IO ()
hClose (LinearFile h) = IO.hClose h

-- ---------------------------------------------------------------------------
-- Example: copy a file, demonstrating linear threading
-- ---------------------------------------------------------------------------

copyFile :: FilePath -> FilePath -> IO ()
copyFile src dst =
  withFile src ReadMode  \srcF ->
  withFile dst WriteMode \dstF -> do
    (Ur contents, srcF') <- hRead srcF
    dstF' <- hWrite dstF contents
    hClose srcF'
    hClose dstF'
    pure (Ur ())

-- ---------------------------------------------------------------------------
-- Example: safe line-by-line accumulation
-- ---------------------------------------------------------------------------

readLines :: FilePath -> IO [Text]
readLines path =
  withFile path ReadMode \f -> do
    (lines', f') <- go f []
    hClose f'
    pure (Ur lines')
  where
    go :: LinearFile %1 -> [Text] -> IO ([Text], LinearFile)
    go f acc = do
      (Ur eof, f') <- hIsEOF f
      if eof
        then pure (reverse acc, f')
        else do
          (Ur line, f'') <- hReadLine f'
          go f'' (line : acc)
