module Potoki.Core.Transduce
(
  Transduce(..),
  produce,
  reduce,
  concurrently,
  take,
  filter,
  list,
  vector,
  foldable,
)
where

import Potoki.Core.Prelude hiding (take, filter, list)
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

reduce :: Reduce a b -> Transduce a b
reduce (Reduce initReduceActions) =
  Transduce $ \ (EatOne consumeB) -> do
    activeReduceActionsIfAnyRef <- newIORef Nothing
    let transduceEatOneA = EatOne $ \ a -> do
          activeReduceActionsIfAny <- readIORef activeReduceActionsIfAnyRef
          case activeReduceActionsIfAny of
            Just (reduceEatOneA, reduceFinishB) -> do
              readyToEatOneMore <- reduceEatOneA a
              if readyToEatOneMore
                then return True
                else do
                  writeIORef activeReduceActionsIfAnyRef Nothing
                  b <- reduceFinishB
                  consumeB b
            Nothing -> do
              (EatOne reduceEatOneA, reduceFinishB) <- initReduceActions
              readyToEatOneMore <- reduceEatOneA a
              if readyToEatOneMore
                then do
                  writeIORef activeReduceActionsIfAnyRef (Just (reduceEatOneA, reduceFinishB))
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
        let nextCount = pred count
        writeIORef countRef nextCount
        status <- consume input
        return $ status && (nextCount > 0)

filter :: (a -> Bool) -> Transduce a a
filter predicate =
  Transduce $ \ (EatOne consume) -> do
    return $ (,return ()) $ EatOne $ \ input -> do
      if (predicate input)
        then (consume input)
        else (return True)            
      
list :: Transduce [a] a
list = foldable

vector :: Transduce (Vector a) a
vector = foldable

foldable :: (Foldable t) => Transduce (t a) a 
foldable =
  Transduce $ \ (EatOne consume) ->
    return $ (, return ()) $ EatOne $ \ input ->
      let step element nextIO = do
            hungry <- consume element
            if hungry
              then nextIO
              else return False
      in foldr step (return True) input