{-# LANGUAGE TemplateHaskell #-}
-- | The prelude's source text, read from @prelude.neomusic@ at compile time
-- (and rebuilt when that file changes).
module NeoMusic.Prelude (preludeSource) where

import Language.Haskell.TH.Syntax (addDependentFile, lift, runIO)

preludeSource :: String
preludeSource = $(do
  let path = "src/NeoMusic/prelude.neomusic"
  addDependentFile path
  runIO (readFile path) >>= lift)
