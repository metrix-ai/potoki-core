module Potoki.Core.Fetch where

import Potoki.Core.Prelude
import Potoki.Core.Types


deriving instance Functor Fetch

instance Applicative Fetch where
  pure x =
    Fetch (\ nil just -> pure (just x))
  (<*>) (Fetch leftFn) (Fetch rightFn) =
    Fetch (\ nil just ->
      join (leftFn (pure nil) (\ leftElement ->
        rightFn nil (\ rightElement -> just (leftElement rightElement)))))

instance Monad Fetch where
  return =
    pure
  (>>=) (Fetch leftFn) rightK =
    Fetch (\ nil just ->
      join (leftFn (pure nil) (\ leftElement ->
        case rightK leftElement of
          Fetch rightFn -> rightFn nil just)))

instance Alternative Fetch where
  empty =
    Fetch (\ nil just -> pure nil)
  (<|>) (Fetch leftSignal) (Fetch rightSignal) =
    Fetch (\ nil just -> join (leftSignal (rightSignal nil just) (pure . just)))

instance MonadPlus Fetch where
  mzero =
    empty
  mplus =
    (<|>)

{-# INLINABLE duplicate #-}
duplicate :: Fetch element -> IO (Fetch element, Fetch element)
duplicate (Fetch fetchIO) =
  do
    leftBuffer <- newTQueueIO
    rightBuffer <- newTQueueIO
    notFetchingVar <- newTVarIO True
    notEndVar <- newTVarIO True
    let
      newFetch ownBuffer mirrorBuffer =
        Fetch
          (\ nil just -> do
            join
              (atomically
                (mplus
                  (do
                    element <- readTQueue ownBuffer
                    return (return (just element)))
                  (do
                    notEnd <- readTVar notEndVar
                    if notEnd
                      then do
                        notFetching <- readTVar notFetchingVar
                        guard notFetching
                        writeTVar notFetchingVar False
                        return
                          (join
                            (fetchIO
                              (do
                                atomically
                                  (do
                                    writeTVar notEndVar False
                                    writeTVar notFetchingVar True)
                                return nil)
                              (\ !element -> do
                                atomically
                                  (do
                                    writeTQueue mirrorBuffer element
                                    writeTVar notFetchingVar True)
                                return (just element))))
                      else return (return nil)))))
      leftFetch =
        newFetch leftBuffer rightBuffer
      rightFetch =
        newFetch rightBuffer leftBuffer
      in return (leftFetch, rightFetch)

{-# INLINABLE maybeRef #-}
maybeRef :: IORef (Maybe a) -> Fetch a
maybeRef refElem =
  Fetch $ \nil just -> do
    elem <- readIORef refElem
    case elem of
      Nothing -> return nil
      Just e  -> do
        writeIORef refElem Nothing
        return $ just e

{-# INLINABLE list #-}
list :: IORef [element] -> Fetch element
list unsentListRef =
  Fetch $ \nil just -> do
    refList <- readIORef unsentListRef
    case refList of
      (!head) : tail -> do
        writeIORef unsentListRef tail
        return $ just head
      _              -> do
        writeIORef unsentListRef []
        return nil

{-# INLINABLE firstCachingSecond #-}
firstCachingSecond :: IORef b -> Fetch (a, b) -> Fetch a
firstCachingSecond cacheRef (Fetch bothFetchIO) =
  Fetch $ \ nil just ->
  join $
  bothFetchIO
    (return nil)
    (\ (!first, !second) -> do
      writeIORef cacheRef second
      return (just first))

{-# INLINABLE bothFetchingFirst #-}
bothFetchingFirst :: IORef b -> Fetch a -> Fetch (a, b)
bothFetchingFirst cacheRef (Fetch firstFetchIO) =
  Fetch $ \ nil just ->
  join $
  firstFetchIO
    (return nil)
    (\ !firstFetched -> do
      secondCached <- readIORef cacheRef
      return (just (firstFetched, secondCached)))

{-# INLINABLE rightHandlingLeft #-}
rightHandlingLeft :: (left -> IO ()) -> Fetch (Either left right) -> Fetch right
rightHandlingLeft handle (Fetch eitherFetchIO) =
  Fetch $ \ nil just ->
  join $ eitherFetchIO (return nil) $ \ case
    Right !rightInput -> return (just rightInput)
    Left !leftInput -> handle leftInput $> nil

{-# INLINABLE rightCachingLeft #-}
rightCachingLeft :: IORef (Maybe left) -> Fetch (Either left right) -> Fetch right
rightCachingLeft cacheRef =
  rightHandlingLeft (writeIORef cacheRef . Just)

{-# INLINABLE eitherFetchingRight #-}
eitherFetchingRight :: IORef (Maybe left) -> Fetch right -> Fetch (Either left right)
eitherFetchingRight cacheRef (Fetch rightFetchIO) =
  Fetch $ \ nil just ->
  join $ rightFetchIO (return nil) $ \ right ->
  atomicModifyIORef' cacheRef $ \ case
    Nothing -> (Nothing, just (Right right))
    Just left -> (Nothing, just (Left left))

{-# INLINABLE signaling #-}
signaling :: IO () -> IO () -> Fetch a -> Fetch a
signaling signalEnd signalElement (Fetch io) =
  Fetch $ \ nil just ->
  join (io (signalEnd $> nil) (\ element -> signalElement >> return (just element)))

{-# INLINE ioMaybe #-}
ioMaybe :: IO (Maybe a) -> Fetch a
ioMaybe io =
  Fetch $ \nil just -> maybe nil just <$> io
