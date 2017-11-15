module Potoki.Core.Produce where

import Potoki.Core.Prelude
import qualified Potoki.Core.Fetch as A


{-|
Passive producer of elements with support for early termination.

Automates the management of resources.
-}
newtype Produce element =
  Produce (forall x. (A.Fetch element -> IO x) -> IO x)

deriving instance Functor Produce

instance Applicative Produce where
  pure x =
    Produce (\ fetch -> fetch (pure x))
  (<*>) (Produce leftIO) (Produce rightIO) =
    Produce (\ fetch -> leftIO (\ leftFetch -> rightIO (\ rightFetch -> fetch (leftFetch <*> rightFetch))))

instance Monad Produce where
  return = pure
  (>>=) (Produce leftIO) rightK =
    Produce $ \ fetch ->
    leftIO $ \ (A.Fetch sendLeft) ->
    fetch $ A.Fetch $ \ nil just ->
    join $ sendLeft (return nil) $ \ leftElement ->
    case rightK leftElement of
      Produce rightIO ->
        rightIO $ \ (A.Fetch sendRight) ->
        sendRight nil just

instance Alternative Produce where
  empty =
    Produce (\ fetch -> fetch empty)
  (<|>) (Produce leftIO) (Produce rightIO) =
    Produce (\ fetch -> leftIO (\ leftFetch -> rightIO (\ rightFetch -> fetch (leftFetch <|> rightFetch))))

{-# INLINABLE list #-}
list :: [input] -> Produce input
list list =
  Produce $ \ fetch -> do
    unsentListRef <- newIORef list
    fetch $ A.Fetch $ \ nil just -> do
      list <- readIORef unsentListRef
      case list of
        (!head) : (!tail) -> do
          writeIORef unsentListRef tail
          return (just head)
        _ -> return nil
