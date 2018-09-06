module Potoki.Core.Transduce
(
  Transduce(..),
  produce,
  reduce,
  concurrently,
  take,
)
where

import Potoki.Core.Prelude hiding (take)
import Potoki.Core.Types
import qualified Potoki.Core.EatOne as A


instance Profunctor Transduce where
  dimap inputMapping outputMapping (Transduce transduceIO) =
    Transduce $ \ oldEatOne -> do
      (newEatOne, finalize) <- transduceIO (contramap outputMapping oldEatOne)
      return (contramap inputMapping newEatOne, finalize)

instance Choice Transduce where
  right' (Transduce transduceRight) =
    Transduce $ \ (EatOne consumeEitherOutput) -> do
      let
        consumeRightOutput rightOutput = do
          consumeEitherOutput (Right rightOutput)
        in do
          (EatOne consumeRightInput, releaseRightTransduce) <- transduceRight (EatOne consumeRightOutput)
          let
            consumeEitherInput eitherInput = do
              case eitherInput of
                Left leftInput -> consumeEitherOutput (Left leftInput)
                Right rightInput -> consumeRightInput rightInput
            in return (EatOne consumeEitherInput, releaseRightTransduce)

instance Strong Transduce where
  second' (Transduce transduceSecond) =
    Transduce $ \ (EatOne consumeBothOutput) -> do
      firstInputRef <- newIORef undefined
      let
        consumeSecondOutput secondOutput = do
          firstInput <- readIORef firstInputRef
          consumeBothOutput (firstInput, secondOutput)
        in do
          (EatOne consumeSecondInput, releaseSecondTransduce) <- transduceSecond (EatOne consumeSecondOutput)
          let
            consumeBothInput (firstInput, secondInput) = do
              writeIORef firstInputRef firstInput
              consumeSecondInput secondInput
            in return (EatOne consumeBothInput, releaseSecondTransduce)

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

-- reduce :: Reduce (IO (EatOne a, IO b)) -> Transduce (EatOne b -> IO (EatOne a, IO ()))
reduce :: Reduce a b -> Transduce a b
reduce (Reduce initReduceActions) =
  Transduce $ \ (EatOne consumeB) -> do
    activeReduceActionsRef <- newIORef =<< initReduceActions
    let transduceEatOneA = EatOne $ \ a -> do
          (EatOne reduceEatOneA, reduceFinishB) <- readIORef activeReduceActionsRef
          status <- reduceEatOneA a
          if status
            then return True
            else do
              (EatOne reduceEatOneA, reduceFinishB1) <- initReduceActions
              readyToEatOneMore <- reduceEatOneA a
              if readyToEatOneMore
                then do
                  writeIORef activeReduceActionsRef (EatOne reduceEatOneA, reduceFinishB1)
                  -- b <- reduceFinishB
                  -- consumeB b
                  return True
                else do
                  b <- reduceFinishB
                  consumeB b
              -- writeIORef activeReduceActionsRef =<< initReduceActions
              -- b <- reduceFinishB
              -- consumeB b
          -- case status of
          --   ConsumedAndReadyForMoreConsumptionStatus -> return ConsumedAndReadyForMoreConsumptionStatus
          --   ConsumedAndNotReadyForMoreConsumptionStatus -> do
          --     b <- reduceFinishB
          --     writeIORef activeReduceActionsRef =<< initReduceActions
          --     consumeB b
          --   DidntConsumeAndNotReadyForMoreConsumptionStatus -> do
          --     b <- reduceFinishB
          --     (reduceEatOneA, reduceFinishB) <- initReduceActions
          --     -- # TODO loop
          --     reduceEatOneA a
          --     writeIORef activeReduceActionsRef (reduceEatOneA, reduceFinishB)
          --     consumeB b
        transduceFinish = do
          (_, reduceFinishB) <- readIORef activeReduceActionsRef
          writeIORef activeReduceActionsRef (mempty, reduceFinishB)
          b <- reduceFinishB
          consumeB b
          return ()
        in return (transduceEatOneA, transduceFinish)

produce :: (a -> Produce b) -> Transduce a b
produce aToProduceB =
  Transduce $ \ consumeB ->
  let consumeA = EatOne $ \ a -> case aToProduceB a of Produce produceIO -> produceIO consumeB
      in return (consumeA, return ())

concurrently :: Int -> TransduceConcurrently a b -> Transduce a b
concurrently = undefined

take :: Int -> Transduce a a
take amount
  | amount <= 0 =
    Transduce $ \ _ -> return $ (, return ()) $ EatOne (\ _ -> return False)
  | otherwise =
    Transduce $ \ (EatOne consume) -> do
      countRef <- newIORef amount
      return $ (,return ()) $ EatOne $ \ input -> do
        count <- readIORef countRef
        if count > 0
          then do
            modifyIORef' countRef pred
            consume input
          else
            return False
