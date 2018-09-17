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
bufferize :: NFData element => Int -> Transform element element
bufferize size =
  Transform $ \ (A.Fetch fetchIO) -> liftIO $ do
    buffer <- newTBQueueIO size
    activeVar <- newTVarIO True

    forkIO $ let
      loop = do
        fetchingResult <- fetchIO
        case fetchingResult of
          Just element -> do
            forcedElement <- evaluate (force element)
            atomically $ writeTBQueue buffer forcedElement
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
concurrently :: NFData output => Int -> Transform input output -> Transform input output
concurrently workersAmount transform =
  if workersAmount == 1
    then transform
    else
      sync >>>
      unsafeConcurrently workersAmount transform

{-# INLINE unsafeConcurrently #-}
unsafeConcurrently :: NFData output => Int -> Transform input output -> Transform input output
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
            Just result -> do
              forcedResult <- evaluate (force result)
              atomically (writeTBQueue chan forcedResult)
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

concurrentlyInOrder :: NFData b => Int -> Transform a b -> Transform a b
concurrentlyInOrder concurrency (Transform transform) = Transform $ \ (Fetch fetchA) -> liftIO $ do
  inputQueue <- newTBQueueIO concurrency
  outputSlotQueue <- newTQueueIO
  liveWorkersVar <- newTVarIO concurrency

  forkIO $ let
    loop = do
      fetchAResult <- fetchA
      case fetchAResult of
        Just a -> do
          atomically $ writeTBQueue inputQueue (Just a)
          loop
        Nothing -> atomically $ replicateM_ concurrency $ writeTBQueue inputQueue Nothing
    in loop

  replicateM_ concurrency $ forkIO $ do
    outputQueue <- newTQueueIO
    needsSwitchVar <- newTVarIO False

    let
      localizedFetchA = Fetch $ atomically $ do
        needsSwitch <- readTVar needsSwitchVar
        if needsSwitch
          then writeTQueue outputQueue Nothing
          else writeTVar needsSwitchVar True
        writeTQueue outputSlotQueue outputQueue
        readTBQueue inputQueue

      in do
        (Fetch fetchB, finalize) <- case transform localizedFetchA of M.Acquire io -> io
        let
          loop = do
            fetchBResult <- fetchB
            case fetchBResult of
              Just b -> do
                forcedB <- evaluate (force b)
                atomically $ writeTQueue outputQueue (Just forcedB)
                loop
              Nothing -> do
                atomically $ do
                  writeTQueue outputQueue Nothing
                  modifyTVar' liveWorkersVar pred
                finalize
            in loop

  return $ Fetch $ atomically $ fix $ \ loop -> mplus
    (do
      outputQueue <- peekTQueue outputSlotQueue
      bIfAny <- readTQueue outputQueue
      case bIfAny of
        Just b -> return (Just b)
        Nothing -> do
          readTQueue outputSlotQueue
          loop)
    (do
      liveWorkers <- readTVar liveWorkersVar
      guard (liveWorkers <= 0)
      return Nothing)

{-|
A transform, which fetches the inputs asynchronously on the specified number of threads.
-}
async :: NFData input => Int -> Transform input input
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

concurrentlyWithBatching :: (NFData a, NFData b) => Int -> Int -> Transform a b -> Transform a b
concurrentlyWithBatching batching concurrency transform =
  batch @Vector batching >>> bufferize concurrency >>>
  unsafeConcurrently concurrency (vector >>> transform >>> batch @Vector batching) >>>
  vector

concurrentlyInOrderWithBatching :: (NFData b) => Int -> Int -> Transform a b -> Transform a b
concurrentlyInOrderWithBatching batching concurrency transform =
  batch @Vector batching >>> 
  concurrentlyInOrder concurrency (vector >>> transform >>> batch @Vector batching) >>>
  vector
