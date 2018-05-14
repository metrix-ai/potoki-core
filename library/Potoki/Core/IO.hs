module Potoki.Core.IO where

import Potoki.Core.Prelude
import Potoki.Core.Types
import qualified Potoki.Core.Produce as A
import qualified Potoki.Core.Consume as B
import qualified Acquire.IO as C


produceAndConsume :: Produce input -> Consume input output -> IO output
produceAndConsume (Produce produceAcquire) (Consume consume) =
  C.acquire produceAcquire consume

produceAndTransformAndConsume :: Produce input -> Transform input anotherInput -> Consume anotherInput output -> IO output
produceAndTransformAndConsume (Produce produceAcquire) (Transform transformAcquire) (Consume consume) =
  C.acquire (($) <$> transformAcquire <*> produceAcquire) consume

produce :: Produce input -> forall x. IO x -> (input -> IO x) -> IO x
produce (Produce produceAcquire) stop emit =
  C.acquire produceAcquire $ \ (Fetch fetchIO) ->
    join $ do
      fetch <- fetchIO
      return $ fix $ \ loop ->
        case fetch of
          Nothing      -> stop
          Just element -> emit element >> loop

consume :: IO (Maybe input) -> Consume input output -> IO output
consume fetchIO (Consume consume) =
  consume (Fetch fetchIO)

{-| Fetch all the elements running the provided handler on them -}
fetchAndHandleAll :: Fetch element -> IO () -> (element -> IO ()) -> IO ()
fetchAndHandleAll (Fetch fetchIO) onEnd onElement =
  join $ do
    fetch <- fetchIO
    return $ fix $ \ loop ->
      case fetch of
        Nothing      -> onEnd
        Just element -> onElement element >> loop

{-| Fetch and handle just one element -}
fetchAndHandle :: Fetch element -> IO a -> (element -> IO a) -> IO a
fetchAndHandle (Fetch fetchIO) onEnd onElement =
  join $ do
    fetch <- fetchIO
    return $ case fetch of
      Nothing      -> onEnd
      Just element -> onElement element

{-| Fetch just one element -}
fetch :: Fetch element -> IO (Maybe element)
fetch (Fetch fetchIO) =
  fetchIO

transformList :: Transform a b -> [a] -> IO [b]
transformList transform inputList =
  produceAndTransformAndConsume
    (A.list inputList)
    transform
    (B.list)
