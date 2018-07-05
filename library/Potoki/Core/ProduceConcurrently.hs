module Potoki.Core.ProduceConcurrently
where

import Potoki.Core.Prelude
import Potoki.Core.Types
import qualified Potoki.Core.Consume as A
import qualified Potoki.Core.Produce as E


deriving instance Functor ProduceConcurrently

instance Applicative ProduceConcurrently where
  pure x = ProduceConcurrently (E.singleton x)
  (<*>) (ProduceConcurrently (Produce runConsume1)) (ProduceConcurrently (Produce runConsume2)) =
    ProduceConcurrently (Produce runConsume3)
    where
      runConsume3 consume3 = do
        elementVar1 <- newEmptyTMVarIO
        activeVar <- newTVarIO True
        forkIO $ do
          runConsume1 (A.putToVarWhileActive activeVar elementVar1)
          atomically (writeTVar activeVar False)
        runConsume2 (A.apWhileActive activeVar elementVar1 consume3)
        atomically (writeTVar activeVar False)

{-|
Composes the producers to compete whoever produces the value first.
-}
instance Alternative ProduceConcurrently where
  empty = ProduceConcurrently E.empty
  (<|>) (ProduceConcurrently (Produce runConsume1)) (ProduceConcurrently (Produce runConsume2)) =
    ProduceConcurrently (Produce runConsume3)
    where
      runConsume3 (Consume consumeElement3) = do
        elementVar <- newEmptyTMVarIO
        activeVar1 <- newTVarIO True
        activeVar2 <- newTVarIO True
        forkIO $ do
          runConsume1 (A.putToVarWhileActive activeVar1 elementVar)
          atomically (writeTVar activeVar1 False)
        forkIO $ do
          runConsume2 (A.putToVarWhileActive activeVar2 elementVar)
          atomically (writeTVar activeVar2 False)
        let
          processNextElement =
            let
              processNextElementIfExists = do
                element <- takeTMVar elementVar
                return $ do
                  active <- consumeElement3 element
                  if active
                    then processNextElement
                    else atomically $ do
                      writeTVar activeVar1 False
                      writeTVar activeVar2 False
              handleShutdownOfProducers = do
                active1 <- readTVar activeVar1 
                active2 <- readTVar activeVar2 
                if active1 || active2
                  then empty
                  else return (return ())
              in join (atomically (processNextElementIfExists <|> handleShutdownOfProducers))
          in processNextElement

produce :: Produce element -> ProduceConcurrently element
produce = ProduceConcurrently
