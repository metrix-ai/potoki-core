module Potoki.Core.Produce where

import Potoki.Core.Prelude
import qualified Potoki.Core.Fetch as A


{-|
Passive producer of elements with support for early termination.

Automates the management of resources.
-}
newtype Produce element =
  Produce (IO (A.Fetch element, IO ()))

deriving instance Functor Produce

instance Applicative Produce where
  pure x =
    list [x]
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
    let
      fetch =
        A.Fetch $ \ nil just -> do
          list <- readIORef unsentListRef
          case list of
            (!head) : (!tail) -> do
              writeIORef unsentListRef tail
              return (just head)
            _ -> return nil
      in return (fetch, return ())
