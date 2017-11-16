module Potoki.Core.IO where

import Potoki.Core.Prelude
import qualified Potoki.Core.Produce as A
import qualified Potoki.Core.Consume as B
import qualified Potoki.Core.Transform as C
import qualified Potoki.Core.Fetch as D


produceAndConsume :: A.Produce input -> B.Consume input output -> IO output
produceAndConsume (A.Produce produce) (B.Consume consume) =
  produce consume

produceAndTransformAndConsume :: A.Produce input -> C.Transform input anotherInput -> B.Consume anotherInput output -> IO output
produceAndTransformAndConsume (A.Produce produce) (C.Transform transform) (B.Consume consume) =
  produce (transform >=> consume)

produce :: A.Produce input -> forall x. x -> (input -> x) -> IO x
produce (A.Produce produce) end element =
  produce (\ (D.Fetch fetch) -> fetch end element)

consume :: (forall x. x -> (input -> x) -> IO x) -> B.Consume input output -> IO output
consume fetch (B.Consume consume) =
  consume (D.Fetch fetch)

fetchAndHandle :: D.Fetch element -> (element -> IO ()) -> IO ()
fetchAndHandle (D.Fetch fetch) onElement =
  fix (\ loop -> join (fetch (pure ()) (\ element -> onElement element >> loop)))
