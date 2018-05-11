module Potoki.Core.IO where

import Potoki.Core.Prelude
import Potoki.Core.Types
import qualified Potoki.Core.Produce as A
import qualified Potoki.Core.Consume as B
import qualified Acquire.IO as C


produceAndConsume :: Produce input -> Consume input output -> IO output
produceAndConsume (Produce produceManaged) (Consume consume) =
  C.acquire produceManaged consume

produceAndTransformAndConsume :: Produce input -> Transform input anotherInput -> Consume anotherInput output -> IO output
produceAndTransformAndConsume (Produce produceManaged) (Transform transformManaged) (Consume consume) =
  C.acquire (($) <$> transformManaged <*> produceManaged) consume

produce :: Produce input -> forall x. IO x -> (input -> IO x) -> IO x
produce (Produce produceManaged) stop emit =
  C.acquire produceManaged $ \ (Fetch fetchIO) -> 
  fix (\ loop -> join (fetchIO stop (\ element -> emit element >> loop)))

consume :: (forall x. x -> (input -> x) -> IO x) -> Consume input output -> IO output
consume fetch (Consume consume) =
  consume (Fetch fetch)

{-| Fetch all the elements running the provided handler on them -}
fetchAndHandleAll :: Fetch element -> IO () -> (element -> IO ()) -> IO ()
fetchAndHandleAll (Fetch fetchIO) onEnd onElement =
  fix (\ loop -> join (fetchIO onEnd (\ element -> onElement element >> loop)))

{-| Fetch and handle just one element -}
fetchAndHandle :: Fetch element -> IO a -> (element -> IO a) -> IO a
fetchAndHandle (Fetch fetchIO) onEnd onElement =
  join (fetchIO onEnd onElement)

{-| Fetch just one element -}
fetch :: Fetch element -> IO (Maybe element)
fetch (Fetch fetchIO) =
  fetchIO Nothing Just

transformList :: Transform a b -> [a] -> IO [b]
transformList transform inputList =
  produceAndTransformAndConsume
    (A.list inputList)
    transform
    (B.list)
