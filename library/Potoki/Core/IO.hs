module Potoki.Core.IO (
  produceAndConsume,
  produceAndTransformAndConsume,
  produce,
  consume,
  transformList,
) where

import Potoki.Core.Prelude
import Potoki.Core.Types
import qualified Potoki.Core.Produce as A
import qualified Potoki.Core.Consume as B
import qualified Acquire.IO as C


produceAndConsume :: Produce input -> Consume input output -> IO output
produceAndConsume (Produce produceAcquire) (Consume consumeIO) =
  C.acquire produceAcquire consumeIO

produceAndTransformAndConsume :: Produce input -> Transform input anotherInput -> Consume anotherInput output -> IO output
produceAndTransformAndConsume (Produce produceAcquire) (Transform transformAcquire) (Consume consumeIO) =
  C.acquire (produceAcquire >>= transformAcquire) consumeIO

produce :: Produce input -> forall x. IO x -> (input -> IO x) -> IO x
produce (Produce produceAcquire) stop emit =
  C.acquire produceAcquire $ \ (Fetch fetchIO) ->
    fix $ \ doLoop -> do
      fetch <- fetchIO
      case fetch of
        Nothing      -> stop
        Just element -> emit element >> doLoop

consume :: IO (Maybe input) -> Consume input output -> IO output
consume fetchIO (Consume consumeIO) =
  consumeIO $ Fetch fetchIO

transformList :: Transform a b -> [a] -> IO [b]
transformList transform inputList =
  produceAndTransformAndConsume
    (A.list inputList)
    transform
    (B.list)
