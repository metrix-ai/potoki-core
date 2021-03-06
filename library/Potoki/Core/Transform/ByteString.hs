module Potoki.Core.Transform.ByteString
where

import Potoki.Core.Prelude hiding (filter)
import Potoki.Core.Transform.Basic
import Potoki.Core.Transform.Instances ()
import Potoki.Core.Transform.Concurrency
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
  lineList >>> list
  where
    lineList =
      Transform $ \ (Fetch fetchIO) -> liftIO $ do
        stateRef <- newIORef Nothing
        return $ Fetch $ fetchIO >>= \case
          Just chunk ->
            case B.split 10 chunk of
              firstInput : tailVal -> do
                state <- readIORef stateRef
                let
                  newPoking =
                    fold state <> C.bytes firstInput
                  in case unsnoc tailVal of
                    Just (!initVal, !lastVal) ->
                      do
                        writeIORef stateRef $! Just $! C.bytes lastVal
                        let !bytes = D.poking newPoking
                        return (Just (bytes : initVal))
                    Nothing ->
                      do
                        writeIORef stateRef (Just newPoking)
                        return (Just [])
              _ -> return (Just [])
          Nothing -> do
            state <- readIORef stateRef
            case state of
              Just poking -> do
                writeIORef stateRef Nothing
                return (Just [D.poking poking])
              Nothing -> return Nothing

extractLinesWithoutTrail :: Transform ByteString ByteString
extractLinesWithoutTrail =
  lineList >>> list
  where
    lineList =
      Transform $ \ (Fetch fetchIO) -> liftIO $ do
        pokingRef <- newIORef mempty
        return $ Fetch $ do
          fetchResult <- fetchIO
          case fetchResult of
            Just chunk -> case B.split 10 chunk of
              head : tail -> do
                poking <- readIORef pokingRef
                let newPoking = poking <> C.bytes head
                case unsnoc tail of
                  Just (!init, last) -> do
                    writeIORef pokingRef (C.bytes last)
                    let
                      !bytes = D.poking newPoking
                      in return (Just (bytes : init))
                  Nothing -> do
                    writeIORef pokingRef newPoking
                    return (Just [])
              _ -> return (Just [])
            Nothing -> do
              poking <- readIORef pokingRef
              if C.null poking
                then return Nothing
                else let
                  !bytes = D.poking poking
                  in do
                    writeIORef pokingRef mempty
                    return (Just [bytes])

extractLinesConcurrently :: Int -> Transform ByteString ByteString
extractLinesConcurrently concurrency =
  concurrentlyInOrderWithBatching 100 concurrency (arr (B.split 10)) >>> mergeLineChunks

mergeLineChunks :: Transform [ByteString] ByteString
mergeLineChunks = Transform $ \ (Fetch fetchChunkList) -> liftIO $ do
  cachedChunkListRef <- newIORef (Just [])
  incompleteChunkRef <- newIORef Nothing
  return $ Fetch $ let
    loop = do
      cachedChunkListIfAny <- readIORef cachedChunkListRef
      case cachedChunkListIfAny of
        Just cachedChunkList -> case cachedChunkList of
          (!a) : b : tail -> do
            writeIORef cachedChunkListRef (Just (b : tail))
            return (Just a)
          (!a) : _ -> do
            writeIORef cachedChunkListRef (Just [])
            writeIORef incompleteChunkRef (Just a)
            loop
          _ -> do
            newChunkListIfAny <- fetchChunkList
            incompleteChunkIfAny <- readIORef incompleteChunkRef
            case incompleteChunkIfAny of
              Just incompleteChunk -> case newChunkListIfAny of
                Just newChunkList -> case newChunkList of
                  a : b : tail -> do
                    completeChunk <- evaluate (incompleteChunk <> a)
                    writeIORef cachedChunkListRef (Just (b : tail))
                    writeIORef incompleteChunkRef Nothing
                    return (Just completeChunk)
                  a : _ -> do
                    newIncompleteChunk <- evaluate (incompleteChunk <> a)
                    writeIORef incompleteChunkRef (Just newIncompleteChunk)
                    loop
                  _ -> loop
                Nothing -> do
                  writeIORef cachedChunkListRef Nothing
                  return (Just incompleteChunk)
              Nothing -> do
                writeIORef cachedChunkListRef newChunkListIfAny
                loop
        Nothing -> return Nothing
    in loop
