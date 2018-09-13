module Potoki.Core.Transform.Concurrency
where

import Potoki.Core.Prelude hiding (take, takeWhile, filter)
import Potoki.Core.Transform.Instances ()
import Potoki.Core.Transform.Basic
import Potoki.Core.Types
import qualified Potoki.Core.Fetch as A
import qualified Acquire.Acquire as M


bufferizeFlushing :: Int -> Transform input [input]
bufferizeFlushing maxSize =
  Transform $ \ (A.Fetch fetchIO) -> liftIO $ do
    buffer <- newTBQueueIO maxSize
    activeVar <- newTVarIO True

    forkIO $ let
      loop = do
        fetchingResult <- fetchIO
        case fetchingResult of
          Just !element -> do
            atomically $ writeTBQueue buffer element
            loop
          Nothing -> atomically $ writeTVar activeVar False
      in loop

    return $ Fetch $ atomically $ do
      batch <- flushTBQueue buffer
      if null batch
        then do
          active <- readTVar activeVar
          if active
            then retry
            else return Nothing
        else return (Just batch)

{-# INLINE bufferize #-}
bufferize :: Int -> Transform element element
bufferize size =
  Transform $ \ (A.Fetch fetchIO) -> liftIO $ do
    buffer <- newTBQueueIO size
    activeVar <- newTVarIO True

    forkIO $ let
      loop = do
        fetchingResult <- fetchIO
        case fetchingResult of
          Just !element -> do
            atomically $ writeTBQueue buffer element
            loop
          Nothing -> atomically $ writeTVar activeVar False
      in loop

    return $ Fetch $ let
      readBuffer = Just <$> readTBQueue buffer
      terminate = do
        active <- readTVar activeVar
        if active
          then empty
          else return Nothing
      in atomically (readBuffer <|> terminate)

{-|
Identity Transform, which ensures that the inputs are fetched synchronously.

Useful for concurrent transforms.
-}
{-# INLINABLE sync #-}
sync :: Transform a a
sync =
  Transform $ \ (A.Fetch fetchIO) -> liftIO $ do
    activeVar <- newMVar True
    return $ A.Fetch $ do
      active <- takeMVar activeVar
      if active
        then fetchIO >>= \ case
          Just !element -> do
            putMVar activeVar True
            return (Just element)
          Nothing -> do
            putMVar activeVar False
            return Nothing
        else do
          putMVar activeVar False
          return Nothing

{-|
Execute the transform on the specified amount of threads.
The order of the outputs produced is indiscriminate.
-}
{-# INLINABLE concurrently #-}
concurrently :: Int -> Transform input output -> Transform input output
concurrently workersAmount transform =
  if workersAmount == 1
    then transform
    else
      sync >>>
      unsafeConcurrently workersAmount transform

{-# INLINE unsafeConcurrently #-}
unsafeConcurrently :: Int -> Transform input output -> Transform input output
unsafeConcurrently workersAmount (Transform syncTransformIO) = 
  Transform $ \ fetchIO -> liftIO $ do
    chan <- newTBQueueIO (workersAmount * 2)
    workersCounter <- newTVarIO workersAmount

    replicateM_ workersAmount $ forkIO $ do
      (A.Fetch fetchIO, finalize) <- case syncTransformIO fetchIO of M.Acquire io -> io
      let
        loop = do
          fetchResult <- fetchIO
          case fetchResult of
            Just !result -> do
              atomically (writeTBQueue chan result)
              loop
            Nothing -> atomically (modifyTVar' workersCounter pred)
        in loop *> finalize

    return $ A.Fetch $ let
      readChan = Just <$> readTBQueue chan
      terminate = do
        workersActive <- readTVar workersCounter
        if workersActive > 0
          then empty
          else return Nothing
      in atomically (readChan <|> terminate)

{-|
A transform, which fetches the inputs asynchronously on the specified number of threads.
-}
async :: Int -> Transform input input
async workersAmount = 
  Transform $ \ (A.Fetch fetchIO) -> liftIO $ do
    chan <- atomically newEmptyTMVar
    workersCounter <- atomically (newTVar workersAmount)

    replicateM_ workersAmount $ forkIO $ let
      loop = do
        fetchResult <- fetchIO
        case fetchResult of
          Just input -> atomically (putTMVar chan input) *> loop
          Nothing -> atomically (modifyTVar' workersCounter pred)
      in loop

    return $ A.Fetch $ let
      readChan = Just <$> takeTMVar chan
      terminate = do
        workersActive <- readTVar workersCounter
        if workersActive > 0
          then empty
          else return Nothing
      in atomically (readChan <|> terminate)

concurrentlyWithBatching :: Int -> Int -> Transform a b -> Transform a b
concurrentlyWithBatching batching concurrency transform =
  batch @Vector batching >>> bufferize concurrency >>>
  unsafeConcurrently concurrency (vector >>> transform >>> batch @Vector batching) >>>
  vector
