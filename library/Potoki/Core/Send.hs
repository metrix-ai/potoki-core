module Potoki.Core.Send
(
  Send(..),
  putToVarWhileActive,
  apWhileActive,
)
where

import Potoki.Core.Prelude hiding (sum, head, fold, concat, last)
import Potoki.Core.Types

instance Contravariant Send where
  contramap fn (Send io) = Send (io . fn)

instance Divisible Send where
  conquer = Send (\ _ -> return True)
  divide fn (Send io1) (Send io2) =
    Send $ \ input -> case fn input of
      (input1, input2) -> do
        continue1 <- io1 input1
        continue2 <- io2 input2
        let continue = continue1 && continue2
        return continue

instance Semigroup (Send a) where
  (<>) (Send io1) (Send io2) =
    Send $ \ input -> do
      continue1 <- io1 input
      continue2 <- io2 input
      return (continue1 && continue2)

instance Monoid (Send a) where
  mempty = conquer
  mappend = (<>)

putToVarWhileActive :: STM Bool -> TMVar element -> Send element
putToVarWhileActive checkIfActive elementVar =
  Send $ \ element -> atomically $
  mplus
    (putTMVar elementVar element $> True)
    (do
      active <- checkIfActive
      if active
        then retry
        else return False)

apWhileActive :: STM Bool -> STM () -> TMVar (a -> b) -> Send b -> Send a
apWhileActive checkIfActive signalEnd aToBVar (Send consumeB) =
  Send $ \ a -> join $ atomically $
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
