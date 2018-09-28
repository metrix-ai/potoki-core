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
import qualified Potoki.Core.Push as A


instance Profunctor Transduce where
  dimap inputMapping outputMapping (Transduce transduceIO) =
    Transduce $ \ oldPush -> do
      (newPush, finalize) <- transduceIO (contramap outputMapping oldPush)
      return (contramap inputMapping newPush, finalize)

instance Choice Transduce where
  right' (Transduce transduceRight) =
    Transduce $ \ (Push consumeEitherOutput) -> do
      let
        consumeRightOutput rightOutput = do
          consumeEitherOutput (Right rightOutput)
        in do
          (Push consumeRightInput, releaseRightTransduce) <- transduceRight (Push consumeRightOutput)
          let
            consumeEitherInput eitherInput = do
              case eitherInput of
                Left leftInput -> consumeEitherOutput (Left leftInput)
                Right rightInput -> consumeRightInput rightInput
            in return (Push consumeEitherInput, releaseRightTransduce)

instance Strong Transduce where
  second' (Transduce transduceSecond) =
    Transduce $ \ (Push consumeBothOutput) -> do
      firstInputRef <- newIORef undefined
      let
        consumeSecondOutput secondOutput = do
          firstInput <- readIORef firstInputRef
          consumeBothOutput (firstInput, secondOutput)
        in do
          (Push consumeSecondInput, releaseSecondTransduce) <- transduceSecond (Push consumeSecondOutput)
          let
            consumeBothInput (firstInput, secondInput) = do
              writeIORef firstInputRef firstInput
              consumeSecondInput secondInput
            in return (Push consumeBothInput, releaseSecondTransduce)

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
  Transduce $ \ (Push consumeB) -> do
    activeReduceActionsIfAnyRef <- newIORef Nothing
    let transducePushA = Push $ \ a -> do
          activeReduceActionsIfAny <- readIORef activeReduceActionsIfAnyRef
          case activeReduceActionsIfAny of
            Just (reducePushA, reduceFinishB) -> do
              readyToPushMore <- reducePushA a
              if readyToPushMore
                then return True
                else do
                  writeIORef activeReduceActionsIfAnyRef Nothing
                  b <- reduceFinishB
                  consumeB b
            Nothing -> do
              (Push reducePushA, reduceFinishB) <- initReduceActions
              readyToPushMore <- reducePushA a
              if readyToPushMore
                then do
                  writeIORef activeReduceActionsIfAnyRef (Just (reducePushA, reduceFinishB))
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
        in return (transducePushA, transduceFinish)

produce :: (a -> Produce b) -> Transduce a b
produce aToProduceB =
  Transduce $ \ consumeB ->
  let consumeA = Push $ \ a -> case aToProduceB a of Produce produceIO -> produceIO consumeB
      in return (consumeA, return ())

concurrently :: Int -> TransduceConcurrently a b -> Transduce a b
concurrently = undefined

take :: Int -> Transduce a a
take amount
  | amount <= 0 =
    Transduce $ \ _ -> return $ (, return ()) $ Push (\ _ -> return False)
  | otherwise =
    Transduce $ \ (Push consume) -> do
      countRef <- newIORef amount
      return $ (,return ()) $ Push $ \ input -> do
        count <- readIORef countRef
        let nextCount = pred count
        writeIORef countRef nextCount
        status <- consume input
        return $ status && (nextCount > 0)

filter :: (a -> Bool) -> Transduce a a
filter predicate =
  Transduce $ \ (Push consume) -> do
    return $ (,return ()) $ Push $ \ input -> do
      if (predicate input)
        then (consume input)
        else (return True)            
      
list :: Transduce [a] a
list = foldable

vector :: Transduce (Vector a) a
vector = foldable

foldable :: (Foldable t) => Transduce (t a) a 
foldable =
  Transduce $ \ (Push consume) ->
    return $ (, return ()) $ Push $ \ input ->
      let step element nextIO = do
            hungry <- consume element
            if hungry
              then nextIO
              else return False
      in foldr step (return True) input