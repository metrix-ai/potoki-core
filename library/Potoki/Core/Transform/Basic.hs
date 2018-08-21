module Potoki.Core.Transform.Basic
where

import Potoki.Core.Prelude hiding (take, takeWhile, filter, drop)
import Potoki.Core.Types
import qualified Potoki.Core.Fetch as A
import qualified Data.HashSet as C
import qualified Data.Vector as P
import qualified Acquire.Acquire as M


{-# INLINE mapFilter #-}
mapFilter :: (input -> Maybe output) -> Transform input output
mapFilter mapping =
  Transform (return . A.mapFilter mapping)

{-# INLINE filter #-}
filter :: (input -> Bool) -> Transform input input
filter predicate =
  Transform (pure . A.filter predicate)

{-# INLINE just #-}
just :: Transform (Maybe input) input
just =
  Transform (pure . A.just)

{-# INLINE takeWhile #-}
takeWhile :: (input -> Bool) -> Transform input input
takeWhile predicate =
  Transform (pure . A.takeWhile predicate)

{-# INLINE drop #-}
drop :: Int -> Transform input input
drop amount =
  Transform $ \ (A.Fetch fetchIO) -> M.Acquire $ do
    countRef <- newIORef amount
    return $ (, return ()) $ 
      A.Fetch $ fix $ \ doLoop -> do
        count <- readIORef countRef
        if count > 0
          then do
            writeIORef countRef $! pred count
            doLoop
          else fetchIO

{-# INLINE list #-}
list :: Transform [a] a
list =
  Transform $ \ (A.Fetch fetchListIO) -> M.Acquire $ do
    bufferRef <- newIORef []
    return $ (, return ()) $ 
      A.Fetch $ do
        buffer <- readIORef bufferRef
        case buffer of
          headVal : tailVal -> do
            writeIORef bufferRef tailVal
            return (Just headVal)
          _ ->
            let
              fetchElementIO = do
                fetchListIO >>= \case
                  Nothing -> return Nothing
                  Just (headVal : tailVal) -> do
                    writeIORef bufferRef tailVal
                    return (Just headVal)
                  _ -> do
                    writeIORef bufferRef []
                    return Nothing
              in fetchElementIO

{-# INLINABLE vector #-}
vector :: Transform (Vector a) a
vector =
  Transform $ \ (A.Fetch fetchVectorIO) -> M.Acquire $ do
    indexRef <- newIORef 0
    vectorRef <- newIORef mempty
    return $ (, return ()) $ 
      A.Fetch $ fix $ \ doLoop -> do
        vectorVal <- readIORef vectorRef
        indexVal <- readIORef indexRef
        if indexVal < P.length vectorVal
          then do
            writeIORef indexRef (succ indexVal)
            return (Just (P.unsafeIndex vectorVal indexVal))
          else fetchVectorIO >>= \case 
            Just vectorVal' -> do
              writeIORef vectorRef vectorVal'
              writeIORef indexRef 0
              doLoop
            Nothing -> return Nothing

{-# INLINE distinctBy #-}
distinctBy :: (Eq comparable, Hashable comparable) => (element -> comparable) -> Transform element element
distinctBy f =
  Transform $ \ (A.Fetch fetch) -> M.Acquire $ do
    stateRef <- newIORef mempty
    return $ (, return ()) $ 
      A.Fetch $ fix $ \ doLoop -> 
        fetch >>= \case 
          Nothing -> return Nothing
          Just input -> do
            let comparable = f input
            !set <- readIORef stateRef
            if C.member comparable set
              then doLoop
              else do
                writeIORef stateRef $! C.insert comparable set
                return (Just input)

{-# INLINE distinct #-}
distinct :: (Eq element, Hashable element) => Transform element element
distinct = distinctBy id

{-# INLINE mapInIO #-}
mapInIO :: (a -> IO b) -> Transform a b
mapInIO io =
  Transform $ \ (A.Fetch fetch) -> M.Acquire $ 
  return $ (, return ()) $ A.Fetch $ 
  join $ (sequence . fmap io) <$> fetch

{-# INLINE ioTransform #-}
ioTransform :: IO (Transform a b) -> Transform a b
ioTransform io =
  Transform $ \ fetch -> do
    Transform acquire <- liftIO io
    acquire fetch

count :: Transform a Int
count = Transform $ \ (Fetch fetchIO) -> do
  counter <- liftIO (newIORef 0)
  return $ Fetch $ do
    result <- fetchIO
    case result of
      Just _ -> Just <$> atomicModifyIORef' counter (\ n -> (succ n, n))
      Nothing -> return Nothing

mapInIOWithCounter :: (Int -> a -> IO b) -> Transform a b
mapInIOWithCounter handler =
  ioTransform $ do
    counter <- newIORef 0
    return $ mapInIO $ \ a -> do
      count <- atomicModifyIORef' counter (\ n -> (succ n, n))
      handler count a

handleCount :: (Int -> IO ()) -> Transform a a
handleCount handler = mapInIOWithCounter $ \ count a -> do
  handler count
  return a

{-|
Useful for debugging
-}
traceWithCounter :: (Int -> String) -> Transform a a
traceWithCounter shower = handleCount (putStrLn . shower)

{-# INLINE consume #-}
consume :: Consume input output -> Transform input output
consume (Consume runFetch) =
  Transform $ \ (Fetch inputIO) -> do
    stoppedRef <- liftIO $ newIORef False
    return $ Fetch $ do
      stopped <- readIORef stoppedRef
      if stopped
        then do
          writeIORef stoppedRef False
          return Nothing
        else do
          emittedRef <- newIORef False
          output <- runFetch $ Fetch $ do
            input <- inputIO
            case input of
              Nothing     -> do
                writeIORef stoppedRef True
                return Nothing
              Just element -> do
                writeIORef emittedRef True
                return $ Just element
          checkStopped <- readIORef stoppedRef
          if checkStopped
            then do
              emitted <- readIORef emittedRef
              if emitted
                then return $ Just output
                else do
                  writeIORef stoppedRef False
                  return Nothing
            else return $ Just output

{-# INLINABLE produce #-}
produce :: (input -> Produce output) -> Transform input output
produce inputToProduce =
  Transform $ \ (Fetch inputFetchIO) -> do
    stateRef <- liftIO $ newIORef Nothing
    return $ Fetch $ fix $ \ doLoop -> do
      state <- readIORef stateRef
      case state of
        Just (Fetch outputFetchIO, kill) ->
          do
            outputFetchResult <- outputFetchIO
            case outputFetchResult of
              Just x -> return (Just x)
              Nothing -> do
                kill
                writeIORef stateRef Nothing
                doLoop
        Nothing ->
          do
            inputFetchResult <- inputFetchIO
            case inputFetchResult of
              Just input -> do
                case inputToProduce input of
                  Produce (Acquire produceIO) -> do
                    fetchAndKill <- produceIO
                    writeIORef stateRef (Just fetchAndKill)
                    doLoop
              Nothing -> return Nothing

{-# INLINE mapFetch #-}
mapFetch :: (Fetch a -> Fetch b) -> Transform a b
mapFetch mapping =
  Transform $ return . mapping

{-|
Execute the IO action.
-}
{-# INLINE executeIO #-}
executeIO :: Transform (IO a) a
executeIO =
  mapFetch $ \ (Fetch fetchIO) -> Fetch (fetchIO >>= sequence)

{-# INLINE take #-}
take :: Int -> Transform input input
take amount
  | amount <= 0 =
    Transform $ \ _ -> return $ Fetch $ return Nothing
  | otherwise   =
    Transform $ \ (Fetch fetchIO) -> do
      countRef <- liftIO $ newIORef amount
      return $ Fetch $ do
        count <- readIORef countRef
        if count > 0
          then do
            modifyIORef countRef pred
            fetchIO
          else
            return Nothing
      
