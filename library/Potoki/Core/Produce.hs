module Potoki.Core.Produce
(
  Produce(..),
  list,
  transform,
  concurrently,
)
where

import Potoki.Core.Prelude
import Potoki.Core.Types
import qualified Potoki.Core.Fetch as A


deriving instance Functor Produce

instance Applicative Produce where
  pure x = Produce $ do
    refX <- liftIO (newIORef (Just x))
    return (A.maybeRef refX)
  (<*>) (Produce leftAcquire) (Produce rightAcquire) =
    Produce ((<*>) <$> leftAcquire <*> rightAcquire)

instance Alternative Produce where
  empty =
    Produce (pure empty)
  (<|>) (Produce leftAcquire) (Produce rightAcquire) =
    Produce ((<|>) <$> leftAcquire <*> rightAcquire)

instance Monad Produce where
  return = pure
  (>>=) (Produce (Acquire io1)) k2 =
    Produce $ Acquire $ do
      (fetch1, release1) <- io1
      release2Ref <- newIORef (return ())
      let
        fetch2 input1 =
          case k2 input1 of
            Produce (Acquire io2) ->
              A.ioFetch $ do
                join (readIORef release2Ref)
                (fetch2', release2') <- io2
                writeIORef release2Ref release2'
                return fetch2'
        release3 =
          join (readIORef release2Ref) >> release1
        in return (fetch1 >>= fetch2, release3)

instance MonadIO Produce where
  liftIO io =
    Produce (return (liftIO io))

{-# INLINABLE list #-}
list :: [input] -> Produce input
list inputList =
  Produce $ liftIO (A.list <$> newIORef inputList)

{-# INLINE transform #-}
transform :: Transform input output -> Produce input -> Produce output
transform (Transform transformAcquire) (Produce produceAcquire) =
  Produce $ do
    fetch <- produceAcquire
    newFetch <- transformAcquire
    return $ newFetch fetch

{-|
Squash multiple produces into a single one, which prefetches elements from all
on concurrent threads.
-}
{-# INLINABLE concurrently #-}
concurrently :: [Produce a] -> Produce a
concurrently produces =
  Produce $ Acquire $ do
    let Acquire fetchesAcquireIO = traverse (\ (Produce acquire) -> acquire) produces
    (fetches, releaseFetches) <- fetchesAcquireIO
    activeVar <- newTVarIO True
    outputVar <- newEmptyTMVarIO
    unfinishedFetchesVar <- newTVarIO (length fetches)
    let
      decrementUnfinishedFetches =
        do
          unfinishedFetches <- readTVar unfinishedFetchesVar
          writeTVar unfinishedFetchesVar $! pred unfinishedFetches
      ensureNoUnfinishedFetchesIsLeft =
        do
          unfinishedFetches <- readTVar unfinishedFetchesVar
          guard (unfinishedFetches == 0)
    forM_ fetches $ \ (Fetch fetchIO) -> forkIO $ fix $ \ loop -> do
      fetchingResult <- fetchIO
      case fetchingResult of
        Nothing -> atomically decrementUnfinishedFetches
        Just element -> join $ atomically $ do
          active <- readTVar activeVar
          if active
            then do
              putTMVar outputVar element
              return loop
            else do
              decrementUnfinishedFetches
              return (return ())
    let
      release = do
        atomically $ writeTVar activeVar False
        atomically $ ensureNoUnfinishedFetchesIsLeft
        releaseFetches
      newFetch =
        let
          takeOutput =
            Just <$> takeTMVar outputVar
          signalEnd =
            Nothing <$ ensureNoUnfinishedFetchesIsLeft
          in Fetch $ atomically $ takeOutput <|> signalEnd
      in return (newFetch, release)
