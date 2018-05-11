module Potoki.Core.Produce
(
  Produce(..),
  list,
  transform,
)
where

import Potoki.Core.Prelude
import Potoki.Core.Types
import qualified Potoki.Core.Fetch as A
import qualified Potoki.Core.Acquire as B


deriving instance Functor Produce

instance Applicative Produce where
  pure x = Produce $ do
    refX <- liftIO (newIORef (Just x))
    return (A.maybeRef refX)
  (<*>) (Produce leftAcquire) (Produce rightAcquire) =
    Produce ((<*>) <$> leftAcquire <*> rightAcquire)

instance Alternative Produce where
  empty =
    Produce (pure empty)
  (<|>) (Produce leftAcquire) (Produce rightAcquire) =
    Produce ((<|>) <$> leftAcquire <*> rightAcquire)

{-# INLINABLE list #-}
list :: [input] -> Produce input
list list =
  Produce (liftIO (A.list <$> newIORef list))

{-# INLINE transform #-}
transform :: Transform input output -> Produce input -> Produce output
transform (Transform transformAcquire) (Produce produceAcquire) =
  Produce $ do
    fetch <- produceAcquire
    newFetch <- transformAcquire
    return (newFetch fetch)
