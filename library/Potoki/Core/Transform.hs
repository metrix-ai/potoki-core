module Potoki.Core.Transform
(
  Transform(..),
  consume,
  produce,
  mapFetch,
  executeIO,
  take,
)
where

import Potoki.Core.Prelude hiding (take)
import Potoki.Core.Types
import qualified Potoki.Core.Fetch as A


instance Category Transform where
  id =
    Transform (return id)
  (.) (Transform leftVal) (Transform rightVal) =
    Transform ((.) <$> leftVal <*> rightVal)

instance Profunctor Transform where
  dimap inputMapping outputMapping (Transform acquire) =
    Transform $ do
      newFetch <- acquire
      return $ \ oldFetch -> fmap outputMapping (newFetch $ fmap inputMapping oldFetch)

instance Choice Transform where
  right' :: Transform a b -> Transform (Either c a) (Either c b)
  right' (Transform rightTransformAcquire) =
    Transform $ do
      rightInFetchToOutFetch <- rightTransformAcquire
      fetchedLeftMaybeRef <- liftIO $ newIORef Nothing
      return $ \ inFetch ->
        let
          Fetch rightFetchIO = rightInFetchToOutFetch $ A.rightHandlingLeft (writeIORef fetchedLeftMaybeRef . Just) inFetch
         in Fetch $ do
           rightFetch <- rightFetchIO
           case rightFetch of
             Nothing    -> do
               fetchedLeftMaybe <- readIORef fetchedLeftMaybeRef
               case fetchedLeftMaybe of
                 Nothing          -> return Nothing
                 Just fetchedLeft -> do
                   writeIORef fetchedLeftMaybeRef Nothing
                   return $ Just (Left fetchedLeft)
             Just element -> return $ Just (Right element)

instance Strong Transform where
  first' (Transform firstTransformAcquire) =
    Transform $ do
      cacheRef <- liftIO $ newIORef undefined
      firstInFetchToOutFetch <- firstTransformAcquire
      return $ A.bothFetchingFirst cacheRef . firstInFetchToOutFetch . A.firstCachingSecond cacheRef

instance Arrow Transform where
  arr fn =
    Transform (return (fmap fn))
  first =
    first'

instance ArrowChoice Transform where
  left =
    left'

{-# INLINE consume #-}
consume :: Consume input output -> Transform input output
consume (Consume runFetch) =
  Transform $ do
    stoppedRef <- liftIO $ newIORef False
    return $ \ (Fetch inputIO) -> Fetch $ do
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
  Transform $ do
    stateRef <- liftIO $ newIORef Nothing
    return $ \ (Fetch inputFetchIO) -> Fetch $ fix $ \ doLoop -> do
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
  Transform $ return mapping

{-|
Execute the IO action.
-}
{-# INLINE executeIO #-}
executeIO :: Transform (IO a) a
executeIO =
  mapFetch $ \ (Fetch fetchIO) ->
    Fetch $ do
      fetch <- fetchIO
      case fetch of
        Nothing        -> return Nothing
        Just ioElement -> fmap Just ioElement

{-# INLINE take #-}
take :: Int -> Transform input input
take amount
  | amount <= 0 =
    Transform $ return $ \ _ -> Fetch $ return Nothing
  | otherwise   =
    Transform $ do
      countRef <- liftIO $ newIORef amount
      return $ \ (Fetch fetchIO) -> Fetch $ do
        count <- readIORef countRef
        if count > 0
          then do
            modifyIORef countRef pred
            fetchIO
          else
            return Nothing
