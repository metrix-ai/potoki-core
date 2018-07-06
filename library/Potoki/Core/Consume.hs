module Potoki.Core.Consume
where

import Potoki.Core.Prelude hiding (sum, head, fold, concat, last)
import Potoki.Core.Types

instance Contravariant Consume where
  contramap fn (Consume io) = Consume (io . fn)

instance Divisible Consume where
  conquer = Consume (\ _ -> return True)
  divide fn (Consume io1) (Consume io2) =
    Consume $ \ input -> case fn input of
      (input1, input2) -> do
        continue1 <- io1 input1
        continue2 <- io2 input2
        let continue = continue1 && continue2
        return continue

instance Semigroup (Consume a) where
  (<>) (Consume io1) (Consume io2) =
    Consume $ \ input -> do
      continue1 <- io1 input
      if continue1
        then io2 input
        else return False

instance Monoid (Consume a) where
  mempty = conquer
  mappend = (<>)

putToVarWhileActive :: STM Bool -> TMVar element -> Consume element
putToVarWhileActive checkIfActive elementVar =
  Consume $ \ element -> atomically $
  mplus
    (putTMVar elementVar element $> True)
    (do
      active <- checkIfActive
      if active
        then retry
        else return False)

apWhileActive :: STM Bool -> STM () -> TMVar (a -> b) -> Consume b -> Consume a
apWhileActive checkIfActive signalEnd aToBVar (Consume consumeB) =
  Consume $ \ a -> join $ atomically $
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
