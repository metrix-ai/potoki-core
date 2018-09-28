module Potoki.Core.ProduceConcurrently
(
  ProduceConcurrently(..),
  produce,
)
where

import Potoki.Core.Prelude
import Potoki.Core.Types
import qualified Potoki.Core.Send as A
import qualified Potoki.Core.Produce as E


deriving instance Functor ProduceConcurrently

instance Applicative ProduceConcurrently where
  pure x = ProduceConcurrently (point x)
  (<*>) = unsafeCoerce E.apConcurrently

{-|
Composes the producers to compete whoever produces the value first.
-}
instance Alternative ProduceConcurrently where
  empty = ProduceConcurrently E.empty
  (<|>) = unsafeCoerce E.alternate

instance Semigroup (ProduceConcurrently a) where
  (<>) = (<|>)

{-|
Provides the same interface as "Alternative" with a specialized implementation of "mconcat",
which is more efficient than "asum".
-}
instance Monoid (ProduceConcurrently a) where
  mempty = empty
  mappend = (<|>)
  mconcat list = ProduceConcurrently (E.concatConcurrently ((coerce :: [ProduceConcurrently a] -> [Produce a]) list))

produce :: Produce element -> ProduceConcurrently element
produce = ProduceConcurrently
