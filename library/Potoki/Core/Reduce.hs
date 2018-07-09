module Potoki.Core.Reduce
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

transduce :: Transduce a b -> Reduce b c -> Reduce a c
transduce (Transduce transduceIO) (Reduce reduceIO) =
  Reduce $ do
    (consume, finishReduce) <- reduceIO
    (transducedConsume, finishTransduce) <- transduceIO consume
    return (transducedConsume, finishTransduce *> finishReduce)
