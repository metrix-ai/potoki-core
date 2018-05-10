module Potoki.Core.IO where

import Potoki.Core.Prelude
import qualified Potoki.Core.Produce as A
import qualified Potoki.Core.Consume as B
import qualified Potoki.Core.Transform as C
import qualified Potoki.Core.Fetch as D


produceAndConsume :: A.Produce input -> B.Consume input output -> IO output
produceAndConsume (A.Produce produce) (B.Consume consume) =
  do
    (fetch, kill) <- produce
    consume fetch <* kill

produceAndTransformAndConsume :: A.Produce input -> C.Transform input anotherInput -> B.Consume anotherInput output -> IO output
produceAndTransformAndConsume (A.Produce produce) (C.Transform transform) (B.Consume consume) =
  do
    (fetch, kill) <- produce
    (transform fetch >>= consume) <* kill

produce :: A.Produce input -> forall x. IO x -> (input -> IO x) -> IO x
produce (A.Produce produce) stop emit =
  do
    (D.Fetch fetchIO, kill) <- produce
    fix (\ loop -> join (fetchIO stop (\ element -> emit element >> loop))) <* kill

consume :: (forall x. x -> (input -> x) -> IO x) -> B.Consume input output -> IO output
consume fetch (B.Consume consume) =
  consume (D.Fetch fetch)

{-| Fetch all the elements running the provided handler on them -}
fetchAndHandleAll :: D.Fetch element -> IO () -> (element -> IO ()) -> IO ()
fetchAndHandleAll (D.Fetch fetchIO) onEnd onElement =
  fix (\ loop -> join (fetchIO onEnd (\ element -> onElement element >> loop)))

{-| Fetch just one element -}
fetch :: D.Fetch element -> IO (Maybe element)
fetch (D.Fetch fetchIO) =
  fetchIO Nothing Just

transformList :: C.Transform a b -> [a] -> IO [b]
transformList transform inputList =
  produceAndTransformAndConsume
    (A.list inputList)
    transform
    (B.list)
