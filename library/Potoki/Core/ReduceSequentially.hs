module Potoki.Core.ReduceSequentially
where

import Potoki.Core.Prelude
import Potoki.Core.Types
import qualified Potoki.Core.Consume as A
import qualified Potoki.Core.Reduce as B


instance Functor (ReduceSequentially input) where
  fmap fn (ReduceSequentially reduce) = ReduceSequentially (fmap (fmap fn) reduce)

instance Pointed (ReduceSequentially input) where
  point x = ReduceSequentially (point (Just x))

instance Applicative (ReduceSequentially input) where
  pure = point
  (<*>) = ap

instance Monad (ReduceSequentially input) where
  return = point
  (>>=) :: ReduceSequentially x a -> (a -> ReduceSequentially x b) -> ReduceSequentially x b
  (>>=) (ReduceSequentially (Reduce reduceA)) aToReduceB =
    ReduceSequentially $ Reduce $ do
      stateRef <- newIORef Nothing
      (Consume consumeOfA, extractA) <- reduceA
      let
        consumeOfB x = do
          state <- readIORef stateRef
          case state of
            Just (consumeOfB, _) -> do
              consumeOfB x
            Nothing -> do
              readyForMore <- consumeOfA x
              if readyForMore
                then return True
                else do
                  maybeA <- extractA
                  case maybeA of
                    Just a -> case aToReduceB a of
                      ReduceSequentially (Reduce reduceB) -> do
                        (Consume consumeOfB, extractB) <- reduceB
                        writeIORef stateRef (Just (consumeOfB, extractB))
                        return True
                    Nothing -> return False
        extractB = do
          state <- readIORef stateRef
          case state of
            Just (_, extractB) -> extractB
            Nothing -> return Nothing
        in return (Consume consumeOfB, extractB)

reduce :: Reduce a b -> ReduceSequentially a b
reduce reduce = ReduceSequentially (fmap Just reduce)
