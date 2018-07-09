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

instance Category ReduceSequentially where
  id = ReduceSequentially $ Reduce $ do
    headRef <- newIORef Nothing
    let consume x = do
          writeIORef headRef (Just x)
          return False
        extract = readIORef headRef
        in return (Consume consume, extract)
  (.) (ReduceSequentially (Reduce reduceBToC)) (ReduceSequentially (Reduce reduceAToB)) =
    ReduceSequentially $ Reduce $ do
      stateRef <- newIORef Nothing
      (Consume consumeB, extractC) <- reduceBToC
      let consume a = do
            state <- readIORef stateRef
            case state of
              Nothing -> do
                (Consume consumeA, extractB) <- reduceAToB
                readyForMore <- consumeA a
                if readyForMore
                  then do
                    writeIORef stateRef (Just (consumeA, extractB))
                    return True
                  else do
                    maybeB <- extractB
                    case maybeB of
                      Just b -> consumeB b
                      Nothing -> return False
              Just (consumeA, extractB) -> do
                readyForMore <- consumeA a
                if readyForMore
                  then return True
                  else do
                    writeIORef stateRef Nothing
                    maybeB <- extractB
                    case maybeB of
                      Just b -> consumeB b
                      Nothing -> return False
          extract = do
            state <- readIORef stateRef
            case state of
              Nothing -> extractC
              Just (_, extractB) -> do
                writeIORef stateRef Nothing
                maybeB <- extractB
                case maybeB of
                  Just b -> do
                    consumeB b
                    extractC
                  Nothing -> return Nothing
      return (Consume consume, extract)

reduce :: Reduce a b -> ReduceSequentially a b
reduce reduce = ReduceSequentially (fmap Just reduce)
