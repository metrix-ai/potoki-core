module Potoki.Core.Reduce
where

import Potoki.Core.Prelude
import Potoki.Core.Types
import qualified Potoki.Core.Consume as A


instance Functor (Reduce input) where
  fmap fn (Reduce io) =
    Reduce $ do
      (consume, finish) <- io
      return (consume, fmap fn finish)

instance Pointed (Reduce input) where
  point x = Reduce (return (conquer, return x))

instance Applicative (Reduce input) where
  pure = point
  (<*>) (Reduce io1) (Reduce io2) =
    Reduce $ do
      (consume1, finish1) <- io1
      (consume2, finish2) <- io2
      let newConsume = consume1 <> consume2
          newFinish = finish1 <*> finish2
          in return (newConsume, newFinish)

transduce :: Transduce a b -> Reduce b c -> Reduce a c
transduce (Transduce transduceIO) (Reduce reduceIO) =
  Reduce $ do
    (consume, finishReduce) <- reduceIO
    (transducedConsume, finishTransduce) <- transduceIO consume
    return (transducedConsume, finishTransduce *> finishReduce)
