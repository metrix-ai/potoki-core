module Potoki.Core.Transduce
where

import Potoki.Core.Prelude
import Potoki.Core.Types
import qualified Potoki.Core.Consume as A


instance Profunctor Transduce where
  dimap inputMapping outputMapping (Transduce transduceIO) =
    Transduce $ \ oldConsume -> do
      (newConsume, finalize) <- transduceIO (contramap outputMapping oldConsume)
      return (contramap inputMapping newConsume, finalize)

reduce :: Reduce a b -> Transduce a b
reduce (Reduce reduceIO) =
  Transduce $ \ consume -> do
    (reduceConsume, reduceFinish) <- reduceIO
    undefined

produce :: (a -> Produce b) -> Transduce a b
produce aToProduceB =
  Transduce $ \ consumeB ->
  let consumeA = Consume $ \ a -> case aToProduceB a of Produce produceIO -> produceIO consumeB
      in return (consumeA, return ())
