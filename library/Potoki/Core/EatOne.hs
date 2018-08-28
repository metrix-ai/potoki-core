module Potoki.Core.EatOne
(
  EatOne(..),
  putToVarWhileActive,
  apWhileActive,
)
where

import Potoki.Core.Prelude hiding (sum, head, fold, concat, last)
import Potoki.Core.Types

instance Contravariant EatOne where
  contramap fn (EatOne io) = EatOne (io . fn)

instance Divisible EatOne where
  conquer = EatOne (\ _ -> return True)
  divide fn (EatOne io1) (EatOne io2) =
    EatOne $ \ input -> case fn input of
      (input1, input2) -> do
        continue1 <- io1 input1
        continue2 <- io2 input2
        let continue = continue1 && continue2
        return continue

instance Semigroup (EatOne a) where
  (<>) (EatOne io1) (EatOne io2) =
    EatOne $ \ input -> do
      continue1 <- io1 input
      continue2 <- io2 input
      return (continue1 && continue2)

instance Monoid (EatOne a) where
  mempty = conquer
  mappend = (<>)

putToVarWhileActive :: STM Bool -> TMVar element -> EatOne element
putToVarWhileActive checkIfActive elementVar =
  EatOne $ \ element -> atomically $
  mplus
    (putTMVar elementVar element $> True)
    (do
      active <- checkIfActive
      if active
        then retry
        else return False)

apWhileActive :: STM Bool -> STM () -> TMVar (a -> b) -> EatOne b -> EatOne a
apWhileActive checkIfActive signalEnd aToBVar (EatOne consumeB) =
  EatOne $ \ a -> join $ atomically $
  mplus
    (do
      aToB <- takeTMVar aToBVar
      return $ do
        active <- consumeB (aToB a)
        atomically $ if active
          then checkIfActive
          else signalEnd $> False)
    (do
      active <- checkIfActive
      if active
        then retry
        else return (return False))
