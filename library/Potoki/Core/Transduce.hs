module Potoki.Core.Transduce
(
  Transduce(..),
  produce,
  reduce,
  concurrently,
)
where

import Potoki.Core.Prelude
import Potoki.Core.Types
import qualified Potoki.Core.Consume as A


instance Profunctor Transduce where
  dimap inputMapping outputMapping (Transduce transduceIO) =
    Transduce $ \ oldConsume -> do
      (newConsume, finalize) <- transduceIO (contramap outputMapping oldConsume)
      return (contramap inputMapping newConsume, finalize)

instance Choice Transduce where
  right' (Transduce transduceRight) =
    Transduce $ \ (Consume consumeEitherOutput) -> do
      let
        consumeRightOutput rightOutput = do
          consumeEitherOutput (Right rightOutput)
        in do
          (Consume consumeRightInput, releaseRightTransduce) <- transduceRight (Consume consumeRightOutput)
          let
            consumeEitherInput eitherInput = do
              case eitherInput of
                Left leftInput -> consumeEitherOutput (Left leftInput)
                Right rightInput -> consumeRightInput rightInput
            in return (Consume consumeEitherInput, releaseRightTransduce)

instance Strong Transduce where
  second' (Transduce transduceSecond) =
    Transduce $ \ (Consume consumeBothOutput) -> do
      firstInputRef <- newIORef undefined
      let
        consumeSecondOutput secondOutput = do
          firstInput <- readIORef firstInputRef
          consumeBothOutput (firstInput, secondOutput)
        in do
          (Consume consumeSecondInput, releaseSecondTransduce) <- transduceSecond (Consume consumeSecondOutput)
          let
            consumeBothInput (firstInput, secondInput) = do
              writeIORef firstInputRef firstInput
              consumeSecondInput secondInput
            in return (Consume consumeBothInput, releaseSecondTransduce)

instance Semigroupoid Transduce where
  o = (.)

instance Category Transduce where
  id = Transduce $ \ consume -> return (consume, return ())
  (.) (Transduce transduceBToC) (Transduce transduceAToB) =
    Transduce $ \ consumeC -> do
      (consumeB, finishTransduceBToC) <- transduceBToC consumeC
      (consumeA, finishTransduceAToB) <- transduceAToB consumeB
      return (consumeA, finishTransduceAToB *> finishTransduceBToC)

instance Arrow Transduce where
  arr fn = Transduce $ \ consume -> return (contramap fn consume, return ())
  first = first'
  second = second'

instance ArrowChoice Transduce where
  left = left'
  right = right'

reduce :: Reduce a b -> Transduce a b
reduce (Reduce initReduceActions) =
  Transduce $ \ (Consume consumeB) -> do
    activeReduceActionsIfAnyRef <- newIORef Nothing
    let transduceConsumeA = Consume $ \ a -> do
          activeReduceActionsIfAny <- readIORef activeReduceActionsIfAnyRef
          case activeReduceActionsIfAny of
            Just (reduceConsumeA, reduceFinishB) -> do
              readyToConsumeMore <- reduceConsumeA a
              if readyToConsumeMore
                then return True
                else do
                  writeIORef activeReduceActionsIfAnyRef Nothing
                  b <- reduceFinishB
                  consumeB b
            Nothing -> do
              (Consume reduceConsumeA, reduceFinishB) <- initReduceActions
              readyToConsumeMore <- reduceConsumeA a
              if readyToConsumeMore
                then do
                  writeIORef activeReduceActionsIfAnyRef (Just (reduceConsumeA, reduceFinishB))
                  return True
                else do
                  b <- reduceFinishB
                  consumeB b
        transduceFinish = do
          activeReduceActionsIfAny <- readIORef activeReduceActionsIfAnyRef
          case activeReduceActionsIfAny of
            Just (_, reduceFinishB) -> do
              writeIORef activeReduceActionsIfAnyRef Nothing
              b <- reduceFinishB
              consumeB b
              return ()
            Nothing -> return ()
        in return (transduceConsumeA, transduceFinish)

produce :: (a -> Produce b) -> Transduce a b
produce aToProduceB =
  Transduce $ \ consumeB ->
  let consumeA = Consume $ \ a -> case aToProduceB a of Produce produceIO -> produceIO consumeB
      in return (consumeA, return ())

concurrently :: Int -> TransduceConcurrently a b -> Transduce a b
concurrently = undefined
