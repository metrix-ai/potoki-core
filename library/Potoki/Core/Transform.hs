module Potoki.Core.Transform
(
  Transform(..),
  consume,
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


instance Category Transform where
  id =
    Transform (return id)
  (.) (Transform left) (Transform right) =
    Transform ((.) <$> left <*> right)

instance Profunctor Transform where
  dimap inputMapping outputMapping (Transform managed) =
    Transform $ do
      newFetch <- managed
      return $ \ oldFetch -> fmap outputMapping (newFetch (fmap inputMapping oldFetch))

instance Choice Transform where
  right' (Transform rightTransformManaged) =
    Transform $ do
      rightInFetchToOutFetch <- rightTransformManaged
      fetchedLeftMaybeRef <- liftIO (newIORef Nothing)
      return $ \ inFetch ->
        let
          Fetch rightFetchIO = rightInFetchToOutFetch (A.rightHandlingLeft (writeIORef fetchedLeftMaybeRef . Just) inFetch)
          in Fetch $ \ stop yield -> do
            join $ rightFetchIO
              (do
                fetchedLeftMaybe <- readIORef fetchedLeftMaybeRef
                case fetchedLeftMaybe of
                  Just fetchedLeft -> do
                    writeIORef fetchedLeftMaybeRef Nothing
                    return (yield (Left fetchedLeft))
                  Nothing -> return stop)
              (\ right -> return (yield (Right right)))

instance Strong Transform where
  first' (Transform firstTransformManaged) =
    Transform $ do
      cacheRef <- liftIO (newIORef undefined)
      firstInFetchToOutFetch <- firstTransformManaged
      return (A.bothFetchingFirst cacheRef . firstInFetchToOutFetch . A.firstCachingSecond cacheRef)

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
    stoppedRef <- liftIO (newIORef False)
    return $ \ (Fetch fetch) -> Fetch $ \ stop yield -> do
      stopped <- readIORef stoppedRef
      if stopped
        then do
          writeIORef stoppedRef False
          return stop
        else do
          emittedRef <- newIORef False
          output <-
            runFetch $ Fetch $ \ inputNil inputJust ->
            join
              (fetch
                (do
                  writeIORef stoppedRef True
                  return inputNil)
                (\ !input -> do
                  writeIORef emittedRef True
                  return (inputJust input)))
          stopped <- readIORef stoppedRef
          if stopped
            then do
              emitted <- readIORef emittedRef
              if emitted
                then return (yield output)
                else do
                  writeIORef stoppedRef False
                  return stop
            else return (yield output)

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
  mapFetch $ \ (Fetch fetchIO) -> Fetch $ \ stop yield ->
  join (fetchIO (return stop) (fmap yield))

{-# INLINE take #-}
take :: Int -> Transform input input
take amount =
  Transform $ do
    countRef <- liftIO (newIORef amount)
    return $ \ (Fetch fetchIO) -> Fetch $ \ stop yield -> do
      count <- readIORef countRef
      if count > 0
        then do
          writeIORef countRef $! pred count
          fetchIO stop yield
        else return stop
