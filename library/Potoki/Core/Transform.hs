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
import Potoki.Core.Transform.Types
import qualified Potoki.Core.Fetch as A
import qualified Potoki.Core.Consume as C
import qualified Potoki.Core.Produce as D


instance Category Transform where
  id =
    Transform return
  (.) (Transform leftFetchIO) (Transform rightFetchIO) =
    Transform (leftFetchIO <=< rightFetchIO)

instance Profunctor Transform where
  dimap inputMapping outputMapping (Transform fetchIO) =
    Transform (\ inputFetch -> (fmap . fmap) outputMapping (fetchIO (fmap inputMapping inputFetch)))

instance Choice Transform where
  right' (Transform rightTransformIO) =
    Transform $ \ eitherFetch -> do
      fetchedLeftMaybeRef <- newIORef Nothing
      rightFetchMaybeRef <- newIORef Nothing
      return $ A.Fetch $ \ nil just -> do
        A.Fetch rightFetchIO <- do
          rightFetchMaybe <- readIORef rightFetchMaybeRef
          case rightFetchMaybe of
            Just rightFetch -> return rightFetch
            Nothing -> do
              rightFetch <- rightTransformIO (A.rightCachingLeft fetchedLeftMaybeRef eitherFetch)
              writeIORef rightFetchMaybeRef (Just rightFetch)
              return rightFetch
        join $ rightFetchIO
          (do
            fetchedLeftMaybe <- readIORef fetchedLeftMaybeRef
            case fetchedLeftMaybe of
              Just fetchedLeft -> do
                writeIORef fetchedLeftMaybeRef Nothing
                writeIORef rightFetchMaybeRef Nothing
                return (just (Left fetchedLeft))
              Nothing -> return nil)
          (\ right -> return (just (Right right)))

instance Strong Transform where
  first' (Transform firstTransformIO) =
    Transform $ \ bothFetch -> do
      stateRef <- newIORef undefined
      firstFetch <- firstTransformIO (A.firstCachingSecond stateRef bothFetch)
      return $ A.bothFetchingFirst stateRef firstFetch

instance Arrow Transform where
  arr fn =
    Transform (pure . fmap fn)
  first =
    first'

instance ArrowChoice Transform where
  left =
    left'

{-# INLINE consume #-}
consume :: C.Consume input output -> Transform input output
consume (C.Consume runFetch) =
  Transform $ \ (A.Fetch fetch) -> do
    stoppedRef <- newIORef False
    return $ A.Fetch $ \ nil just -> do
      stopped <- readIORef stoppedRef
      if stopped
        then return nil
        else do
          emittedRef <- newIORef False
          output <-
            runFetch $ A.Fetch $ \ inputNil inputJust ->
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
                then return (just output)
                else return nil
            else return (just output)

{-# INLINABLE produce #-}
produce :: (input -> D.Produce output) -> Transform input output
produce inputToProduce =
  Transform $ \ (A.Fetch inputFetchIO) -> do
    stateRef <- newIORef Nothing
    return $ A.Fetch $ \ nil just -> fix $ \ loop -> do
      state <- readIORef stateRef
      case state of
        Just (A.Fetch outputFetchIO, kill) ->
          join $ outputFetchIO
            (kill >> writeIORef stateRef Nothing >> loop)
            (return . just)
        Nothing ->
          join $ inputFetchIO (return nil) $ \ !input -> do
            case inputToProduce input of
              D.Produce produceIO -> do
                fetchAndKill <- produceIO
                writeIORef stateRef (Just fetchAndKill)
                loop

{-# INLINE mapFetch #-}
mapFetch :: (A.Fetch a -> A.Fetch b) -> Transform a b
mapFetch mapping =
  Transform $ return . mapping

{-|
Execute the IO action.
-}
{-# INLINE executeIO #-}
executeIO :: Transform (IO a) a
executeIO =
  mapFetch $ \ (A.Fetch fetchIO) -> A.Fetch $ \ nil just ->
  join (fetchIO (return nil) (fmap just))

{-# INLINE take #-}
take :: Int -> Transform input input
take amount =
  Transform $ \ (A.Fetch fetchIO) -> do
    countRef <- newIORef amount
    return $ A.Fetch $ \ nil just -> do
      count <- readIORef countRef
      if count > 0
        then do
          writeIORef countRef $! pred count
          fetchIO nil just
        else return nil
