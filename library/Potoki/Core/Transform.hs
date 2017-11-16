module Potoki.Core.Transform where

import Potoki.Core.Prelude
import qualified Potoki.Core.Fetch as A
import qualified Potoki.Core.Consume as C
import qualified Deque as B


newtype Transform input output =
  Transform (A.Fetch input -> IO (A.Fetch output))

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
    Transform $ \ (A.Fetch eitherFetchIO) -> do
      fetchedLeftMaybeRef <- newIORef Nothing
      let
        createRightFetchIO =
          rightTransformIO $ A.Fetch $ \ nil just -> join $ eitherFetchIO (return nil) $ \ case
            Right !rightInput -> return (just rightInput)
            Left !leftInput -> writeIORef fetchedLeftMaybeRef (Just leftInput) $> nil
      rightFetchIORef <- newIORef =<< createRightFetchIO
      return $ A.Fetch $ \ nil just -> do
        A.Fetch rightFetchIO <- readIORef rightFetchIORef
        join $ rightFetchIO
          (do
            fetchedLeftMaybe <- readIORef fetchedLeftMaybeRef
            case fetchedLeftMaybe of
              Just fetchedLeft -> do
                writeIORef fetchedLeftMaybeRef Nothing
                writeIORef rightFetchIORef =<< createRightFetchIO
                return (just (Left fetchedLeft))
              Nothing -> return nil)
          (\ right -> return (just (Right right)))

instance Strong Transform where
  first' (Transform firstTransformIO) =
    Transform $ \ (A.Fetch bothFetchIO) -> do
      secondFetchedDequeRef <- newIORef mempty
      A.Fetch firstFetchIO <-
        firstTransformIO $ A.Fetch $ \ nil just ->
        join $ bothFetchIO (return nil) $ \ (!firstFetched, !secondFetched) -> do
          modifyIORef' secondFetchedDequeRef (B.snoc secondFetched)
          return (just firstFetched)
      return $ A.Fetch $ \ nil just -> join $ firstFetchIO (return nil) $ \ !firstFetched -> do
        secondFetchedDeque <- readIORef secondFetchedDequeRef
        case B.uncons secondFetchedDeque of
          Just (!secondFetched, !secondFetchedDequeTail) -> do
            writeIORef secondFetchedDequeRef secondFetchedDequeTail
            return (just (firstFetched, secondFetched))
          Nothing -> return nil

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
  implode runFetch

{-# INLINABLE implode #-}
implode :: (A.Fetch input -> IO output) -> Transform input output
implode runFetch =
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

{-# INLINABLE explode #-}
explode :: (input -> IO (A.Fetch output)) -> Transform input output
explode produce =
  Transform $ \ (A.Fetch fetch) -> do
    stateRef <- newIORef Nothing
    return $ A.Fetch $ \ nil just -> fix $ \ loop -> do
      state <- readIORef stateRef
      case state of
        Just (A.Fetch fetch) ->
          join (fetch (writeIORef stateRef Nothing >> loop) (return . just))
        Nothing ->
          join $ fetch (return nil) $ \ !input -> do
            currentFetch <- produce input
            writeIORef stateRef (Just currentFetch)
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
