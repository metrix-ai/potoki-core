module Potoki.Core.Transform.ByteString
where

import Potoki.Core.Prelude hiding (filter)
import Potoki.Core.Transform.Basic
import Potoki.Core.Transform.Instances ()
import Potoki.Core.Types
import qualified Potoki.Core.Fetch as A
import qualified Potoki.Core.Produce as H
import qualified Ptr.Poking as C
import qualified Ptr.ByteString as D
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as E
import qualified Data.ByteString.Lazy as F
import qualified Acquire.Acquire as M


{-# INLINE builderChunks #-}
builderChunks :: Transform E.Builder ByteString
builderChunks =
  produce (H.list . F.toChunks . E.toLazyByteString)

{-|
Convert freeform bytestring chunks into chunks,
which are strictly separated by newline no matter how long they may be.
-}
extractLines :: Transform ByteString ByteString
extractLines =
  lineList >>> filter (not . null) >>> list
  where
    lineList =
      Transform $ \ (A.Fetch fetchIO) -> M.Acquire $ do
        stateRef <- newIORef Nothing
        return $ (, return ()) $  A.Fetch $ fetchIO >>= \case
          Nothing -> (do
            state <- readIORef stateRef
            case state of
              Just poking -> do
                writeIORef stateRef Nothing
                return (Just [D.poking poking])
              Nothing -> return Nothing)
          Just chunk -> (
            case B.split 10 chunk of
              firstInput : tailVal -> do
                state <- readIORef stateRef
                let
                  newPoking =
                    fold state <> C.bytes firstInput
                  in case unsnoc tailVal of
                    Just (initVal, lastVal) ->
                      do
                        writeIORef stateRef (Just (C.bytes lastVal))
                        return (Just (D.poking newPoking : initVal))
                    Nothing ->
                      do
                        writeIORef stateRef (Just newPoking)
                        return (Just [])
              _ -> return (Just []))
