module Potoki.Core.Reduce
(
  Reduce(..),
  zipping,
  sequentially,
  transduce,
)
where

import Potoki.Core.Prelude
import Potoki.Core.Types
import qualified Potoki.Core.Consume as A


instance Functor (Reduce input) where
  fmap fn (Reduce io) =
    Reduce $ do
      (consume, finish) <- io
      return (consume, fmap fn finish)

instance Pointed (Reduce input) where
  point x = Reduce (return (conquer, return x))

instance Semigroupoid Reduce where
  o (Reduce reduceBToC) (Reduce reduceAToB) =
    Reduce $ do
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
                    b <- extractB
                    consumeB b
              Just (consumeA, extractB) -> do
                readyForMore <- consumeA a
                if readyForMore
                  then return True
                  else do
                    writeIORef stateRef Nothing
                    b <- extractB
                    consumeB b
          extract = do
            state <- readIORef stateRef
            case state of
              Nothing -> extractC
              Just (_, extractB) -> do
                writeIORef stateRef Nothing
                b <- extractB
                consumeB b
                extractC
      return (Consume consume, extract)

instance Profunctor Reduce where
  dimap :: (a -> b) -> (c -> d) -> Reduce b c -> Reduce a d
  dimap aToB cToD (Reduce reduceBToC) =
    Reduce $ do
      (consumeB, extractC) <- reduceBToC
      return (contramap aToB consumeB, fmap cToD extractC)

instance Choice Reduce where
  right' :: Reduce a b -> Reduce (Either c a) (Either c b)
  right' (Reduce reduceAToB) =
    Reduce $ do
      stateRef <- newIORef Nothing
      (Consume consumeA, extractB) <- reduceAToB
      let consumeCOrA = \case
            Right a -> consumeA a
            Left c -> do
              writeIORef stateRef (Just c)
              return False
          extractCOrB = do
            state <- readIORef stateRef
            case state of
              Nothing -> fmap Right extractB
              Just c -> return (Left c)
          in return (Consume consumeCOrA, extractCOrB)

transduce :: Transduce a b -> Reduce b c -> Reduce a c
transduce (Transduce transduceIO) (Reduce reduceIO) =
  Reduce $ do
    (consume, finishReduce) <- reduceIO
    (transducedConsume, finishTransduce) <- transduceIO consume
    return (transducedConsume, finishTransduce *> finishReduce)

zipping :: ReduceZipping a b -> Reduce a b
zipping (ReduceZipping reduce) = reduce

sequentially :: ReduceSequentially a b -> Reduce a (Maybe b)
sequentially (ReduceSequentially reduce) = reduce
