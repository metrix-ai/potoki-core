module Potoki.Core.Transform.Concurrency
where

import Potoki.Core.Prelude hiding (take, takeWhile, filter)
import Potoki.Core.Transform.Instances ()
import Potoki.Core.Types
import qualified Potoki.Core.Fetch as A
import qualified Acquire.Acquire as M
import qualified Control.Concurrent.Chan.Unagi.Bounded as B


{-# INLINE bufferize #-}
bufferize :: Int -> Transform element element
bufferize size =
  Transform $ \ (A.Fetch fetch) -> M.Acquire $ do
    (inChan, outChan) <- B.newChan size
    forkIO $ fix $ \ doLoop ->
      fetch >>= \case
        Nothing -> B.writeChan inChan Nothing
        Just !element -> B.writeChan inChan (Just element) >> doLoop
    return $ (A.Fetch $ B.readChan outChan, return ())

{-|
Identity Transform, which ensures that the inputs are fetched synchronously.

Useful for concurrent transforms.
-}
{-# INLINABLE sync #-}
sync :: Transform a a
sync =
  Transform $ \ (A.Fetch fetch) -> M.Acquire $ do
    activeVar <- newMVar True
    return $ (, return ()) $ A.Fetch $ do
      active <- takeMVar activeVar
      if active
        then fetch >>= \case
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
      concurrentlyUnsafe workersAmount transform

{-# INLINE concurrentlyUnsafe #-}
concurrentlyUnsafe :: Int -> Transform input output -> Transform input output
concurrentlyUnsafe workersAmount (Transform syncTransformIO) = 
  Transform $ \ fetch -> M.Acquire $ do
    outChan <- newEmptyMVar
    replicateM_ workersAmount $ forkIO $ do
      let runAcquire (M.Acquire io) = io
      (A.Fetch fetchIO, _) <- runAcquire $ syncTransformIO fetch
      fix $ \ doLoop -> fetchIO >>= \case
        Nothing -> putMVar outChan Nothing
        Just !result -> putMVar outChan (Just result) >> doLoop
    activeWorkersAmountVar <- newMVar workersAmount
    return $ (, return ()) $ A.Fetch $ fix $ \ doLoop' -> do
      activeWorkersAmount <- takeMVar activeWorkersAmountVar
      if activeWorkersAmount <= 0
        then return Nothing
        else do
          fetchResult <- takeMVar outChan
          case fetchResult of
            Just result -> do
              putMVar activeWorkersAmountVar activeWorkersAmount
              return (Just result)
            Nothing -> do
              putMVar activeWorkersAmountVar (pred activeWorkersAmount)
              doLoop'

{-|
A transform, which fetches the inputs asynchronously on the specified number of threads.
-}
async :: Int -> Transform input input
async workersAmount = 
  Transform $ \ (A.Fetch fetchIO) -> M.Acquire $ do
    
    chan <- atomically newEmptyTMVar
    workersCounter <- atomically (newTVar workersAmount)

    replicateM_ workersAmount $ forkIO $ fix $ \ loop -> do
      fetchResult <- fetchIO
      case fetchResult of
        Just input -> atomically (putTMVar chan input) *> loop
        Nothing -> atomically (modifyTVar' workersCounter pred)

    let
      newFetchIO = let
        readChan = Just <$> readTMVar chan
        checkCounter = do
          workersActive <- readTVar workersCounter
          if workersActive > 0
            then empty
            else return Nothing
        in atomically (readChan <|> checkCounter)
    
    return (A.Fetch newFetchIO, return ())
