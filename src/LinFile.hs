{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LinFile
  ( LinearFile,
    Mode (..),
    withFile,
    hRead,
    hReadLine,
    hWrite,
    hWriteLine,
    hClose,
    hSeek,
    hTell,
    hIsEOF,
    -- Examples
    copyFile,
    readLines,
    appendLine,
    countBytes,
    writeLines,
    -- Re-export for threading
    Ur (..),
    -- REPL examples
    res,
    res1,
  )
where

import qualified Control.Exception as E
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Prelude.Linear hiding (Eq (..))
import qualified System.IO as IO
import Unsafe.Linear (toLinear)
import Prelude (Eq (..), pure)

-- | A file handle that must be consumed linearly — it cannot be
-- duplicated or silently dropped. Every code path must end in hClose.
newtype LinearFile = LinearFile IO.Handle

data Mode = ReadMode | WriteMode | AppendMode | ReadWriteMode
  deriving stock (Eq, Show)

toIOMode :: Mode -> IO.IOMode
toIOMode ReadMode = IO.ReadMode
toIOMode WriteMode = IO.WriteMode
toIOMode AppendMode = IO.AppendMode
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
    IO.hClose
    (\h -> do Ur result <- k (LinearFile h); pure result)

-- ---------------------------------------------------------------------------
-- Consuming operations — each takes the file linearly and returns it linearly
-- so the handle is threaded through every operation in sequence.
-- ---------------------------------------------------------------------------

-- | Read the entire remaining contents of the file.
hRead :: LinearFile %1 -> IO (Ur Text, LinearFile)
hRead = toLinear $ \(LinearFile h) -> do
  contents <- TIO.hGetContents h
  pure (Ur contents, LinearFile h)

-- | Read a single line.
hReadLine :: LinearFile %1 -> IO (Ur Text, LinearFile)
hReadLine = toLinear $ \(LinearFile h) -> do
  line <- TIO.hGetLine h
  pure (Ur line, LinearFile h)

-- | Write text to the file.
hWrite :: LinearFile %1 -> Text -> IO LinearFile
hWrite = toLinear $ \(LinearFile h) txt -> do
  TIO.hPutStr h txt
  pure (LinearFile h)

-- | Write a line (appends newline).
hWriteLine :: LinearFile %1 -> Text -> IO LinearFile
hWriteLine = toLinear $ \(LinearFile h) txt -> do
  TIO.hPutStrLn h txt
  pure (LinearFile h)

-- | Seek to a byte offset. SeekMode mirrors System.IO.
hSeek :: LinearFile %1 -> IO.SeekMode -> Integer -> IO LinearFile
hSeek = toLinear $ \(LinearFile h) mode offset -> do
  IO.hSeek h mode offset
  pure (LinearFile h)

-- | Return the current byte offset without consuming the file.
hTell :: LinearFile %1 -> IO (Ur Integer, LinearFile)
hTell = toLinear $ \(LinearFile h) -> do
  pos <- IO.hTell h
  pure (Ur pos, LinearFile h)

-- | Check whether we're at end-of-file.
hIsEOF :: LinearFile %1 -> IO (Ur Bool, LinearFile)
hIsEOF = toLinear $ \(LinearFile h) -> do
  eof <- IO.hIsEOF h
  pure (Ur eof, LinearFile h)

-- | Close the file. This is the ONLY way to consume a LinearFile —
-- the linear type system ensures every branch of your code reaches here.
hClose :: LinearFile %1 -> IO ()
hClose = toLinear $ \(LinearFile h) -> IO.hClose h

-- ---------------------------------------------------------------------------
-- Example: copy a file, demonstrating linear threading
-- ---------------------------------------------------------------------------

copyFile :: FilePath -> FilePath -> IO ()
copyFile src dst =
  withFile src ReadMode $ toLinear $ \srcF -> do
    (Ur contents, srcF') <- hRead srcF
    hClose srcF'
    withFile dst WriteMode $ toLinear $ \dstF -> do
      dstF' <- hWrite dstF contents
      hClose dstF'
      pure (Ur ())
    pure (Ur ())

-- ---------------------------------------------------------------------------
-- Example: safe line-by-line accumulation
-- ---------------------------------------------------------------------------

readLines :: FilePath -> IO [Text]
readLines path =
  withFile path ReadMode $ toLinear $ \f -> do
    (lines', f') <- go f []
    hClose f'
    pure (Ur lines')
  where
    go :: LinearFile %1 -> [Text] -> IO ([Text], LinearFile)
    go = toLinear $ \f acc -> do
      (Ur eof, f') <- hIsEOF f
      if eof
        then pure (reverse acc, f')
        else do
          (Ur line, f'') <- hReadLine f'
          go f'' (line : acc)

-- ---------------------------------------------------------------------------
-- Example: append a single line to a file
-- ---------------------------------------------------------------------------

-- | Open a file in append mode, write one line, then close it.
appendLine :: FilePath -> Text -> IO ()
appendLine path line =
  withFile path AppendMode $ toLinear $ \f -> do
    f' <- hWriteLine f line
    hClose f'
    pure (Ur ())

-- ---------------------------------------------------------------------------
-- Example: report the byte size of a file using hSeek / hTell
-- ---------------------------------------------------------------------------

-- | Return the byte length of a file without reading its contents.
countBytes :: FilePath -> IO Int
countBytes path =
  withFile path ReadMode $ toLinear $ \f -> do
    -- Seek to end, read position, seek back to start, close.
    f' <- hSeek f IO.SeekFromEnd 0
    (Ur size, f'') <- hTell f'
    f''' <- hSeek f'' IO.AbsoluteSeek 0
    hClose f'''
    pure (Ur (fromIntegral size))

-- ---------------------------------------------------------------------------
-- Example: write a list of lines to a file
-- ---------------------------------------------------------------------------

-- | Write each element of a list as its own line, creating or overwriting
-- the file.
writeLines :: FilePath -> [Text] -> IO ()
writeLines path ls =
  withFile path WriteMode $ toLinear $ \f -> do
    f' <- go f ls
    hClose f'
    pure (Ur ())
  where
    go :: LinearFile %1 -> [Text] -> IO LinearFile
    go = toLinear $ \f xs -> case xs of
      [] -> pure f
      (l : ls') -> do
        f' <- hWriteLine f l
        go f' ls'

res :: IO ()
res = appendLine "file.txt" "Hi"

--- >>> res

res1 :: IO ()
res1 = withFile "file.txt" ReadWriteMode $ toLinear $ \f -> do
  (Ur x, f') <- hRead f
  TIO.putStrLn x
  hClose f'
  pure (Ur ())
