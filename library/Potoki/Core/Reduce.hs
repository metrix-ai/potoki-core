module Potoki.Core.Reduce
(
  Reduce(..),
  list,
  count,
  unit,
  sum,
  zipping,
  sequentially,
  transduce,
  foldM,
  fold,
  unit,
)
where

import Potoki.Core.Prelude hiding (foldM, fold, sum)
import Potoki.Core.Types
import qualified Potoki.Core.EatOne as A


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
      (EatOne consumeB, extractC) <- reduceBToC
      let consume a = do
            state <- readIORef stateRef
            case state of
              Nothing -> do
                (EatOne consumeA, extractB) <- reduceAToB
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
      return (EatOne consume, extract)

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
      (EatOne consumeA, extractB) <- reduceAToB
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
          in return (EatOne consumeCOrA, extractCOrB)

unit :: Reduce a ()
unit =
  Reduce $ do
    return (EatOne (\_ -> return True), return ())

{-# INLINABLE list #-}
list :: Reduce a [a]
list =
  Reduce $ do
    stateRef <- newIORef id
    let step !acc element = acc . (element:)
        consume input = do
          state <- readIORef stateRef
          let newState = step state input
          writeIORef stateRef newState
          return True
        finish = do
          state <- readIORef stateRef
          return (state $ [])
        in return (EatOne consume, finish)

count :: Reduce a Int
count =
  Reduce $ do
    varRef <- newIORef 0
    let consume _ = do
          modifyIORef' varRef succ
          return True
        finish = do
          state <- readIORef varRef
          return state
        in return (EatOne consume, finish)

sum :: (Num num) => Reduce num num
sum =
  Reduce $ do
    varRef <- newIORef 0
    let consume input = do
          modifyIORef' varRef (+input)
          return True
        finish = do
          state <- readIORef varRef
          return state
        in return (EatOne consume, finish)

transduce :: Transduce a b -> Reduce b c -> Reduce a c
transduce (Transduce transduceIO) (Reduce reduceIO) =
  Reduce $ do
    (consume, finishReduce) <- reduceIO
    (transducedEatOne, finishTransduce) <- transduceIO consume
    return (transducedEatOne, finishTransduce *> finishReduce)

zipping :: ReduceZipping a b -> Reduce a b
zipping (ReduceZipping reduce) = reduce

sequentially :: ReduceSequentially a b -> Reduce a (Maybe b)
sequentially (ReduceSequentially reduce) = reduce

foldM :: FoldM IO a b -> Reduce a b
foldM (FoldM step init extract) =
  Reduce $ do
    stateRef <- newIORef =<< init
    let consume input = do
          state <- readIORef stateRef
          newState <- step state input
          writeIORef stateRef newState
          return True
        finish = do
          state <- readIORef stateRef
          extract state
        in return (EatOne consume, finish)

fold :: Fold a b -> Reduce a b
fold (Fold step init extract) =
  Reduce $ do
    stateRef <- newIORef init
    let consume input = do
          state <- readIORef stateRef
          let newState = step state input
          writeIORef stateRef newState
          return True
        finish = do
          state <- readIORef stateRef
          return (extract state)
        in return (EatOne consume, finish)
