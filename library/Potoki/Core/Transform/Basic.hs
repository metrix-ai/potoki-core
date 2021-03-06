module Potoki.Core.Transform.Basic
where

import Potoki.Core.Prelude hiding (take, takeWhile, filter, drop)
import Potoki.Core.Types
import qualified Potoki.Core.Fetch as A
import qualified Data.HashSet as C
import qualified Data.Vector as P
import qualified Acquire.Acquire as M
import qualified Data.Vector.Generic.Mutable as MutableGenericVector
import qualified Data.Vector.Generic as GenericVector
import qualified Text.Builder as TextBuilder
import qualified Potoki.Core.TextBuilder as TextBuilder


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
  Transform $ \ (A.Fetch fetchIO) -> do
    countRef <- liftIO $ newIORef amount
    return $ Fetch $ do
      let
        loop = do
          count <- readIORef countRef
          fetch <- fetchIO
          case fetch of
            Just _ ->
              if count > 0
                then do
                  writeIORef countRef $! pred count
                  loop
                else (return fetch)
            Nothing -> return Nothing
      loop

{-# INLINE list #-}
list :: Transform [a] a
list =
  Transform $ \ (A.Fetch fetchListIO) -> liftIO $ do
    bufferRef <- newIORef []
    let
      fetchFromBufferOr whenEmpty = do
        buffer <- readIORef bufferRef
        case buffer of
          (!head) : tail -> do
            writeIORef bufferRef tail
            return (Just head)
          _ -> whenEmpty
      fetchFromSource = do
        fetchedList <- fetchListIO
        case fetchedList of
          Just list -> case list of
            (!head) : tail -> do
              writeIORef bufferRef tail
              return (Just head)
            _ -> do
              fetchFromSource
          Nothing -> return Nothing
      in return $ A.Fetch $ fetchFromBufferOr fetchFromSource

{-# INLINABLE vector #-}
vector :: GenericVector.Vector vector a => Transform (vector a) a
vector =
  Transform $ \ (Fetch fetchVectorIO) -> liftIO $ do
    indexRef <- newIORef 0
    vectorRef <- newIORef GenericVector.empty
    return $ Fetch $ let
      loop = do
        vectorVal <- readIORef vectorRef
        indexVal <- readIORef indexRef
        if indexVal < GenericVector.length vectorVal
          then do
            writeIORef indexRef (succ indexVal)
            return (Just (GenericVector.unsafeIndex vectorVal indexVal))
          else fetchVectorIO >>= \case 
            Just vectorVal' -> do
              writeIORef vectorRef vectorVal'
              writeIORef indexRef 0
              loop
            Nothing -> return Nothing
      in loop

{-|
Chunk the stream to vector batches of the given size.

It's useful in combination with 'concurrently' in cases where the lifted transform's iteration is too light.
Actually, there is a composed variation of 'concurrently', which utilizes it: 'concurrentlyWithBatching'.
-}
{-# INLINABLE batch #-}
batch :: GenericVector.Vector vector a => Int -> Transform a (vector a)
batch size = if size < 1
  then Transform $ const $ liftIO $ return $ empty
  else Transform $ \ (Fetch fetch) -> liftIO $ do
    mvec <- MutableGenericVector.new size
    cursor <- newIORef 0
    activeVar <- newIORef True
    return $ Fetch $ let
      loop = do
        active <- readIORef activeVar
        if active
          then do
            fetchingResult <- fetch
            case fetchingResult of
              Just !a -> do
                index <- readIORef cursor
                MutableGenericVector.unsafeWrite mvec index a
                let !nextIndex = succ index
                if nextIndex == size
                  then do
                    writeIORef cursor 0
                    !vec <- GenericVector.freeze mvec
                    return (Just vec)
                  else do
                    writeIORef cursor nextIndex
                    loop
              Nothing -> do
                writeIORef activeVar False
                index <- readIORef cursor
                if index > 0
                  then do
                    !vec <- GenericVector.freeze (MutableGenericVector.unsafeSlice 0 index mvec)
                    return (Just vec)
                  else return Nothing
          else return Nothing
      in loop

{-# INLINE distinctBy #-}
distinctBy :: (Eq comparable, Hashable comparable) => (element -> comparable) -> Transform element element
distinctBy f =
  Transform $ \ (A.Fetch fetch) -> M.Acquire $ do
    stateRef <- newIORef mempty
    return $ (, return ()) $ A.Fetch $ let
      loop = fetch >>= \case
        Nothing -> return Nothing
        Just input -> do
          let comparable = f input
          !set <- readIORef stateRef
          if C.member comparable set
            then loop
            else do
              writeIORef stateRef $! C.insert comparable set
              return (Just input)
      in loop

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
    return $ mapInIO $ \ !a -> do
      count <- atomicModifyIORef' counter (\ n -> (succ n, n))
      handler count a

handleCount :: (Int -> IO ()) -> Transform a a
handleCount handler = mapInIOWithCounter $ \ count a -> do
  handler count
  return a

{-|
Provides for progress monitoring by means of periodic measurement.
-}
handleCountOnInterval :: NominalDiffTime -> (Int -> IO ()) -> Transform a a
handleCountOnInterval interval handler = ioTransform $ do
  nextTime <- addUTCTime interval <$> getCurrentTime
  nextTimeRef <- newIORef nextTime
  return $ handleCount $ \ count -> do
    nextTime <- readIORef nextTimeRef
    time <- getCurrentTime
    when (time >= nextTime) $ do
      writeIORef nextTimeRef (addUTCTime interval nextTime)
      handler count

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
    return $ Fetch $ let
      loop = do
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
                  loop
          Nothing ->
            do
              inputFetchResult <- inputFetchIO
              case inputFetchResult of
                Just input -> do
                  case inputToProduce input of
                    Produce (Acquire produceIO) -> do
                      fetchAndKill <- produceIO
                      writeIORef stateRef (Just fetchAndKill)
                      loop
                Nothing -> return Nothing
      in loop

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

reportProgress :: (Text -> IO ()) -> Transform a a
reportProgress log = handleProgressAndCountOnInterval 1 $ \ progress count ->
  log $ TextBuilder.run $ TextBuilder.streamProgressMessage progress count

handleProgressAndCountOnInterval :: NominalDiffTime -> (Int -> Int -> IO ()) -> Transform a a
handleProgressAndCountOnInterval interval handle = ioTransform $ do
  lastCountRef <- newIORef 0
  return $ handleCountOnInterval interval $ \ count -> do
    lastCount <- readIORef lastCountRef
    writeIORef lastCountRef count
    handle (count - lastCount) count

uniquify :: (Eq a) => Transform a a
uniquify = 
  Transform $ \ (Fetch fetchIO) -> M.Acquire $ do
    duplicateRef <- newIORef Nothing
    return $ (, return ()) $ A.Fetch $ let
      loop = do
        dupliacte <- readIORef duplicateRef
        fetch <- fetchIO
        case fetch of
          Nothing -> return Nothing
          Just elem -> do
            case dupliacte of
              Nothing -> do
                writeIORef duplicateRef fetch
                return fetch
              Just dupl -> do
                if elem == dupl 
                  then
                    loop
                  else do
                    writeIORef duplicateRef (Just elem)
                    return fetch
      in loop
