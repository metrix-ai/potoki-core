module Potoki.Core.Consume
(
  Consume(..),
  apConcurrently,
  list,
  sum,
  transform,
)
where

import Potoki.Core.Prelude hiding (sum)
import Potoki.Core.Types
import qualified Potoki.Core.Fetch as A
import qualified Acquire.IO as B


instance Profunctor Consume where
  {-# INLINE dimap #-}
  dimap inputMapping outputMapping (Consume consume) =
    Consume (\ fetch -> fmap outputMapping (consume $ fmap inputMapping fetch))

instance Choice Consume where
  right' :: Consume a b -> Consume (Either c a) (Either c b)
  right' (Consume rightConsumeIO) =
     Consume $ \ (Fetch eitherFetchIO) -> do
       fetchedLeftMaybeRef <- newIORef Nothing
       consumedRight <-
         rightConsumeIO $ Fetch $ do
           eitherFetch <- eitherFetchIO
           case eitherFetch of
             Nothing      -> return Nothing
             Just element -> case element of
               Right fetchedRight -> return $ Just fetchedRight
               Left  fetchedLeft  -> do
                 writeIORef fetchedLeftMaybeRef $ Just fetchedLeft
                 return Nothing
       fetchedLeftMaybe <- readIORef fetchedLeftMaybeRef
       case fetchedLeftMaybe of
         Nothing          -> return $ Right consumedRight
         Just fetchedLeft -> return $ Left fetchedLeft 

instance Functor (Consume input) where
  fmap = rmap

instance Applicative (Consume a) where
  pure x = Consume $ \ _ -> pure x

  Consume leftConsumeIO <*> Consume rightConsumeIO =
    Consume $ \ fetch -> leftConsumeIO fetch <*> rightConsumeIO fetch

instance Monad (Consume a) where
  Consume leftConsumeIO >>= toRightConsumeIO = Consume $ \ fetch -> do
    Consume rightConsumeIO <- toRightConsumeIO <$> leftConsumeIO fetch
    rightConsumeIO fetch

instance MonadIO (Consume a) where
  liftIO a = Consume $ \ _ -> a

apConcurrently :: Consume a (b -> c) -> Consume a b -> Consume a c
apConcurrently (Consume leftConsumeIO) (Consume rightConsumeIO) =
  Consume $ \ fetch -> do
    (leftFetch, rightFetch) <- A.duplicate fetch
    rightOutputVar <- newEmptyMVar
    _ <- forkIO $ do
      !rightOutput <- rightConsumeIO rightFetch
      putMVar rightOutputVar rightOutput
    !leftOutput <- leftConsumeIO leftFetch
    rightOutput <- takeMVar rightOutputVar
    return (leftOutput rightOutput)

{-# INLINABLE list #-}
list :: Consume input [input]
list =
  Consume $ \ (Fetch fetchIO) ->
    let 
      build !acc = do
        fetch <- fetchIO
        case fetch of
          Nothing       -> pure $ acc []
          Just !element -> build $ acc . (:) element
     in build id

{-# INLINE sum #-}
sum :: Num num => Consume num num
sum =
  Consume $ \ (Fetch fetchIO) ->
    let
      build !acc = do
        fetch <- fetchIO
        case fetch of
          Nothing       -> pure acc
          Just !element -> build $ element + acc
     in build 0

{-# INLINABLE transform #-}
transform :: Transform input1 input2 -> Consume input2 output -> Consume input1 output
transform (Transform transformAcquire) (Consume sink) =
  Consume $ \ fetch -> B.acquire (fmap ($ fetch) transformAcquire) sink
