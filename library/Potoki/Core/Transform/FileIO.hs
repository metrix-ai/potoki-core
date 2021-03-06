module Potoki.Core.Transform.FileIO
where

import Potoki.Core.Prelude hiding (take, takeWhile, filter, drop)
import Potoki.Core.Transform.Basic
import Potoki.Core.Types
import qualified Data.ByteString as J
import qualified Data.Text.IO as Q
import qualified System.Directory as I


{-# INLINE deleteFile #-}
deleteFile :: Transform FilePath (Either IOException ())
deleteFile =
  mapInIO (try . I.removeFile)

{-# INLINE appendBytesToFile #-}
appendBytesToFile :: Transform (FilePath, ByteString) (Either IOException ())
appendBytesToFile =
  mapInIO $ \ (path, bytes) ->
  try $ 
  withFile path AppendMode $ \ handleVal -> 
  J.hPut handleVal bytes

{-# INLINABLE writeTextToFile #-}
writeTextToFile :: Transform (FilePath, Text) (Either IOException ())
writeTextToFile =
  mapInIO $ \ (path, text) ->
  try $ 
  Q.writeFile path text
