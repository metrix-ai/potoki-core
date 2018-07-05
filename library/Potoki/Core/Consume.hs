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

putToVarWhileActive :: STM Bool -> TMVar element -> Consume element
putToVarWhileActive checkIfActive elementVar =
  Consume $ \ element -> atomically $
  mplus
    (putTMVar elementVar element $> True)
    (do
      active <- checkIfActive
      if active
        then empty
        else return False)

apWhileActive :: TVar Bool -> TMVar (a -> b) -> Consume b -> Consume a
apWhileActive activeVar element1Var (Consume consumeElement3) =
  Consume $ \ element2 -> join $ atomically $ do
    active <- readTVar activeVar
    if active
      then do
        element1 <- takeTMVar element1Var
        return $ do
          activeAfterConsuming <- consumeElement3 (element1 element2)
          if activeAfterConsuming
            then atomically (readTVar activeVar)
            else atomically (writeTVar activeVar False) $> False
      else return (return False)
