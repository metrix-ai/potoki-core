module Potoki.Core.Produce where

import Potoki.Core.Prelude
import Potoki.Core.Types
import qualified Potoki.Core.Fetch as A


deriving instance Functor Produce

instance Applicative Produce where
  pure x = Produce $ do
    refX <- newIORef (Just x)
    return (A.maybeRef refX, pure ())
  (<*>) (Produce leftManaged) (Produce rightManaged) =
    Produce ((<*>) <$> leftManaged <*> rightManaged)

instance Alternative Produce where
  empty =
    Produce (pure empty)
  (<|>) (Produce leftManaged) (Produce rightManaged) =
    Produce ((<|>) <$> leftManaged <*> rightManaged)

{-# INLINABLE list #-}
list :: [input] -> Produce input
list list =
  Produce (liftIO (A.list <$> newIORef list))

{-# INLINE transform #-}
transform :: Transform input output -> Produce input -> Produce output
transform (Transform transformManaged) (Produce produceManaged) =
  Produce $ do
    fetch <- produceManaged
    newFetch <- transformManaged
    return (newFetch fetch)
