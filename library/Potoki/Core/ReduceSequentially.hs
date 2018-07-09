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

instance Arrow ReduceSequentially where
  arr fn = ReduceSequentially $ Reduce $ do
    outputRef <- newIORef Nothing
    let consume x = do
          writeIORef outputRef (Just (fn x))
          return False
        extract = readIORef outputRef
        in return (Consume consume, extract)
  first = first'
  second = second'

instance ArrowChoice ReduceSequentially where
  left = left'
  right = right'

instance Profunctor ReduceSequentially where
  dimap fn1 fn2 (ReduceSequentially reduce) =
    ReduceSequentially (dimap fn1 (fmap fn2) reduce)

instance Strong ReduceSequentially where
  first' :: ReduceSequentially a b -> ReduceSequentially (a, c) (b, c)
  first' (ReduceSequentially (Reduce reduceAToB)) = ReduceSequentially $ Reduce $ do
    maybeCRef <- newIORef Nothing
    (Consume consumeA, extractB) <- reduceAToB 
    let consumeAAndC (a, c) = do
          writeIORef maybeCRef (Just c)
          consumeA a
        extractBAndC = do
          maybeB <- extractB
          case maybeB of
            Just b -> do
              maybeC <- readIORef maybeCRef
              case maybeC of
                Just c -> return (Just (b, c))
                Nothing -> return Nothing
            Nothing -> return Nothing
        in return (Consume consumeAAndC, extractBAndC)

instance Choice ReduceSequentially where
  right' :: ReduceSequentially a b -> ReduceSequentially (Either c a) (Either c b)
  right' (ReduceSequentially reduce) =
    ReduceSequentially (fmap sequence (right' reduce))

reduce :: Reduce a b -> ReduceSequentially a b
reduce reduce = ReduceSequentially (fmap Just reduce)
