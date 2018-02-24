module Potoki.Core.Fetch where

import Potoki.Core.Prelude


{-|
Passive producer of elements.
-}
newtype Fetch element =
  {-|
  Something close to a Church encoding of @IO (Maybe element)@.
  -}
  Fetch (forall x. x -> (element -> x) -> IO x)

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

{-# INLINABLE list #-}
list :: IORef [element] -> Fetch element
list unsentListRef =
  Fetch $ \ nil just -> atomicModifyIORef' unsentListRef $ \ case
    (!head) : tail -> (tail, just head)
    _ -> ([], nil)

{-# INLINABLE firstCachingSecond #-}
firstCachingSecond :: IORef (Maybe b) -> Fetch (a, b) -> Fetch a
firstCachingSecond cacheRef (Fetch bothFetchIO) =
  Fetch $ \ nil just ->
  join $
  bothFetchIO
    (atomicModifyIORef' cacheRef (\ cache -> (cache, nil)))
    (\ (!first, !second) -> atomicModifyIORef' cacheRef (const (Just second, just first)))

{-# INLINABLE bothFetchingFirst #-}
bothFetchingFirst :: IORef (Maybe b) -> Fetch a -> Fetch (a, b)
bothFetchingFirst cacheRef (Fetch firstFetchIO) =
  Fetch $ \ nil just ->
  join $
  firstFetchIO
    (atomicModifyIORef' cacheRef (\ cache -> (cache, nil)))
    (\ !firstFetched -> 
      atomicModifyIORef' cacheRef
        (\ case
          Just secondCached -> (Nothing, just (firstFetched, secondCached))
          Nothing -> (Nothing, nil)))
