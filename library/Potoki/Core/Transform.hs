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
import qualified Potoki.Core.Consume as C
import qualified Potoki.Core.Produce as D
import qualified Potoki.Core.IO as E
import qualified Acquire.IO as B


instance Category Transform where
  id =
    Transform (return id)
  (.) (Transform left) (Transform right) =
    Transform ((.) <$> left <*> right)

instance Profunctor Transform where
  dimap inputMapping outputMapping (Transform acquire) =
    Transform $ do
      newFetch <- acquire
      return $ \oldFetch -> fmap outputMapping (newFetch (fmap inputMapping oldFetch))

instance Choice Transform where
  right' :: Transform a b -> Transform (Either c a) (Either c b)
  right' (Transform rightTransformAcquire) =
    Transform $ do
      rightInFetchToOutFetch <- rightTransformAcquire
      fetchedLeftMaybeRef <- liftIO (newIORef Nothing)
      return $ \inFetch ->
        let
          Fetch rightFetchIO = rightInFetchToOutFetch $ A.rightHandlingLeft (writeIORef fetchedLeftMaybeRef . Just) inFetch
         in Fetch $ join $ do
           rightFetch <- rightFetchIO
           return $ case rightFetch of
             Nothing    -> do
               fetchedLeftMaybe <- readIORef fetchedLeftMaybeRef
               case fetchedLeftMaybe of
                 Nothing          -> return Nothing
                 Just fetchedLeft -> do
                   writeIORef fetchedLeftMaybeRef Nothing
                   return $ Just (Left fetchedLeft)
             Just right -> return $ Just (Right right)

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
    return $ \(Fetch inputIO) -> Fetch $ do
      input <- inputIO
      join $ do
        stopped <- readIORef stoppedRef
        return $ if stopped
        then do
          writeIORef stoppedRef False
          return Nothing
        else do
          emittedRef <- newIORef False
          output <- runFetch $ Fetch $
            case input of
              Nothing     -> do
                writeIORef stoppedRef True
                return Nothing
              Just !input -> do
                writeIORef emittedRef True
                return $ Just input
          stopped <- readIORef stoppedRef
          if stopped
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
    stateRef <- liftIO (newIORef Nothing)
    return $ \ (Fetch inputFetchIO) -> Fetch $ fix $ \ loop -> do
      state <- readIORef stateRef
      case state of
        Just (Fetch outputFetchIO, kill) ->
          do
            outputFetchResult <- outputFetchIO
            case outputFetchResult of
              Just x -> return (Just x)
              Nothing -> kill >> writeIORef stateRef Nothing >> loop
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
  mapFetch $ \(Fetch fetchIO) ->
    Fetch $ join $ do
      fetch <- fetchIO
      return $ case fetch of
        Nothing        -> return Nothing
        Just ioElement -> fmap Just ioElement

{-# INLINE take #-}
take :: Int -> Transform input input
take amount
  | amount <= 0 =
    Transform $ return $ \_ -> Fetch $ return Nothing
  | otherwise   =
    Transform $ return $ 
      \(Fetch fetch) ->
        Fetch $ join $ do
          countIO <- newIORef amount
          return $ do
            count <- readIORef countIO
            if count == 0
            then return Nothing
            else do
              modifyIORef countIO pred
              fetch
