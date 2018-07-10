module Potoki.Core.Consume where

import Potoki.Core.Prelude
import qualified Potoki.Core.Fetch as A
import qualified Potoki.Core.Transform.Types as B
import qualified Control.Foldl as D


{-|
Active consumer of input into output.
Sort of like a reducer in Map/Reduce.

Automates the management of resources.
-}
newtype Consume input output =
  {-|
  An action, which executes the provided fetch in IO,
  while managing the resources behind the scenes.
  -}
  Consume (A.Fetch input -> IO output)

instance Profunctor Consume where
  {-# INLINE dimap #-}
  dimap inputMapping outputMapping (Consume consume) =
    Consume (\ fetch -> fmap outputMapping (consume (fmap inputMapping fetch)))

instance Choice Consume where
  right' (Consume rightConsumeIO) =
    Consume $ \ (A.Fetch eitherFetchIO) -> do
      fetchedLeftMaybeRef <- newIORef Nothing
      consumedRight <-
        rightConsumeIO $ A.Fetch $ \ nil just -> join $ eitherFetchIO (return nil) $ \ case
          Right !fetchedRight -> return (just fetchedRight)
          Left !fetchedLeft -> writeIORef fetchedLeftMaybeRef (Just fetchedLeft) >> return nil
      fetchedLeftMaybe <- readIORef fetchedLeftMaybeRef
      case fetchedLeftMaybe of
        Nothing -> return (Right consumedRight)
        Just fetchedLeft -> return (Left fetchedLeft)

instance Functor (Consume input) where
  fmap = rmap

instance Applicative (Consume a) where
  pure x = Consume $ \_ -> pure x

  Consume leftConsumeIO <*> Consume rightConsumeIO =
    Consume $ \fetch -> leftConsumeIO fetch <*> rightConsumeIO fetch

instance Monad (Consume a) where
  Consume leftConsumeIO >>= toRightConsumeIO = Consume $ \fetch -> do
    Consume rightConsumeIO <- toRightConsumeIO <$> leftConsumeIO fetch
    rightConsumeIO fetch

instance MonadIO (Consume a) where
  liftIO a = Consume $ \_ -> a

apConcurrently :: Consume a (b -> c) -> Consume a b -> Consume a c
apConcurrently (Consume leftConsumeIO) (Consume rightConsumeIO) =
  Consume $ \ fetch -> do
    (leftFetch, rightFetch) <- A.duplicate fetch
    rightOutputVar <- newEmptyMVar
    forkIO $ do
      !rightOutput <- rightConsumeIO rightFetch
      putMVar rightOutputVar rightOutput
    !leftOutput <- leftConsumeIO leftFetch
    rightOutput <- takeMVar rightOutputVar
    return (leftOutput rightOutput)

{-# INLINABLE list #-}
list :: Consume input [input]
list =
  Consume $ \ (A.Fetch fetchIO) ->
  let
    build !acc =
      join
        (fetchIO
          (pure (acc []))
          (\ !element -> build (acc . (:) element)))
    in build id

{-# INLINE sum #-}
sum :: Num num => Consume num num
sum =
  Consume $ \ (A.Fetch fetchIO) ->
  let
    build !acc =
      join
        (fetchIO
          (pure acc)
          (\ !element -> build (element + acc)))
    in build 0

{-# INLINABLE transform #-}
transform :: B.Transform input output -> Consume output sinkOutput -> Consume input sinkOutput
transform (B.Transform transform) (Consume sink) =
  Consume (transform >=> sink)

{-# INLINABLE fold #-}
fold :: D.Fold input output -> Consume input output
fold (D.Fold step init finish) =
  Consume $ \ (A.Fetch fetch) -> build fetch init
  where
    build fetch !accumulator =
      join (fetch (pure (finish accumulator)) (\ !input -> build fetch (step accumulator input)))

{-# INLINABLE count #-}
count :: Consume input Int
count =
  Consume $ \ (A.Fetch fetchIO) -> build fetchIO 0
  where
    build fetchIO !acc =
      join (fetchIO (pure acc) (const (build fetchIO (succ acc))))
