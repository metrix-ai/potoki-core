module Potoki.Core.Produce
where

import Potoki.Core.Prelude
import Potoki.Core.Types
import qualified Potoki.Core.Consume as A


instance Functor Produce where
  fmap fn (Produce io) = Produce (io . contramap fn)

instance Pointed Produce where
  point = singleton

{-|
Unlift the concurrent producer.
-}
concurrently :: ProduceConcurrently element -> Produce element
concurrently (ProduceConcurrently produce) = produce

empty :: Produce element
empty = Produce (\ _ -> return ())

singleton :: element -> Produce element
singleton x = Produce (\ (Consume io) -> io x $> ())
