module Potoki.Core.Produce where

import Potoki.Core.Prelude
import qualified Potoki.Core.Fetch as A
import qualified Potoki.Core.Transform.Types as B


{-|
Passive producer of elements with support for early termination.

Automates the management of resources.
-}
newtype Produce element =
  Produce (IO (A.Fetch element, IO ()))

deriving instance Functor Produce

instance Applicative Produce where
  pure x =
    Produce $ pure (pure x, pure ())
  (<*>) (Produce leftIO) (Produce rightIO) =
    Produce $ do
      (leftFetch, leftKill) <- leftIO
      (rightFetch, rightKill) <- rightIO
      return (leftFetch <*> rightFetch, leftKill >> rightKill)

instance Alternative Produce where
  empty =
    Produce (pure (empty, pure ()))
  (<|>) (Produce leftIO) (Produce rightIO) =
    Produce $ do
      (leftFetch, leftKill) <- leftIO
      (rightFetch, rightKill) <- rightIO
      return (leftFetch <|> rightFetch, leftKill >> rightKill)

{-# INLINABLE list #-}
list :: [input] -> Produce input
list list =
  Produce $ do
    unsentListRef <- newIORef list
    return (A.list unsentListRef, return ())

{-# INLINE transform #-}
transform :: B.Transform input output -> Produce input -> Produce output
transform (B.Transform transformIO) (Produce produceIO) =
  Produce $ do
    (fetch, kill) <- produceIO
    newFetch <- transformIO fetch
    return (newFetch, kill)
