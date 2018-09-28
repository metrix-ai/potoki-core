module Potoki.Core.ProduceSequentially
(
  ProduceSequentially(..),
  produce,
)
where

import Potoki.Core.Prelude
import Potoki.Core.Types
import qualified Potoki.Core.Push as A
import qualified Potoki.Core.Produce as E


deriving instance Functor ProduceSequentially

instance Applicative ProduceSequentially where
  pure x = ProduceSequentially (point x)
  (<*>) (ProduceSequentially produce1) (ProduceSequentially produce2) =
    ProduceSequentially (E.apSequentially produce1 produce2)

instance Alternative ProduceSequentially where
  empty = produce E.empty
  (<|>) (ProduceSequentially produce1) (ProduceSequentially produce2) =
    ProduceSequentially (E.prepend produce1 produce2)

instance Monad ProduceSequentially where
  return = pure
  (>>=) = unsafeCoerce E.bind

instance MonadPlus ProduceSequentially where
  mzero = empty
  mplus = (<|>)

instance Semigroup (ProduceSequentially a) where
  (<>) = (<|>)

instance Monoid (ProduceSequentially a) where
  mempty = empty
  mappend = (<|>)

produce :: Produce element -> ProduceSequentially element
produce = ProduceSequentially
