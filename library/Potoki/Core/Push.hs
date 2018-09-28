module Potoki.Core.Push
(
  Push(..),
  putToVarWhileActive,
  apWhileActive,
)
where

import Potoki.Core.Prelude hiding (sum, head, fold, concat, last)
import Potoki.Core.Types

instance Contravariant Push where
  contramap fn (Push io) = Push (io . fn)

instance Divisible Push where
  conquer = Push (\ _ -> return True)
  divide fn (Push io1) (Push io2) =
    Push $ \ input -> case fn input of
      (input1, input2) -> do
        continue1 <- io1 input1
        continue2 <- io2 input2
        let continue = continue1 && continue2
        return continue

instance Semigroup (Push a) where
  (<>) (Push io1) (Push io2) =
    Push $ \ input -> do
      continue1 <- io1 input
      continue2 <- io2 input
      return (continue1 && continue2)

instance Monoid (Push a) where
  mempty = conquer
  mappend = (<>)

putToVarWhileActive :: STM Bool -> TMVar element -> Push element
putToVarWhileActive checkIfActive elementVar =
  Push $ \ element -> atomically $
  mplus
    (putTMVar elementVar element $> True)
    (do
      active <- checkIfActive
      if active
        then retry
        else return False)

apWhileActive :: STM Bool -> STM () -> TMVar (a -> b) -> Push b -> Push a
apWhileActive checkIfActive signalEnd aToBVar (Push consumeB) =
  Push $ \ a -> join $ atomically $
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
