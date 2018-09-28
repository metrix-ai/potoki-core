module Potoki.Core.ReduceZipping
(
  ReduceZipping(..),
  reduce,
)
where

import Potoki.Core.Prelude
import Potoki.Core.Types
import qualified Potoki.Core.Send as A
import qualified Potoki.Core.Reduce as B


instance Functor (ReduceZipping input) where
  fmap fn (ReduceZipping reduce) = ReduceZipping (fmap fn reduce)

instance Pointed (ReduceZipping input) where
  point x = ReduceZipping (point x)

instance Applicative (ReduceZipping input) where
  pure = point
  (<*>) (ReduceZipping (Reduce io1)) (ReduceZipping (Reduce io2)) =
    ReduceZipping $ Reduce $ do
      (consume1, finish1) <- io1
      (consume2, finish2) <- io2
      let newSend = consume1 <> consume2
          newFinish = finish1 <*> finish2
          in return (newSend, newFinish)

reduce :: Reduce a b -> ReduceZipping a b
reduce = ReduceZipping
