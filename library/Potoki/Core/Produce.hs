module Potoki.Core.Produce
where

import Potoki.Core.Prelude
import Potoki.Core.Types
import qualified Potoki.Core.Consume as A


instance Functor Produce where
  fmap fn (Produce io) = Produce (io . contramap fn)

instance Pointed Produce where
  point = singleton

{-|
Unlift a concurrently composed producer.
-}
concurrently :: ProduceConcurrently element -> Produce element
concurrently (ProduceConcurrently produce) = produce

empty :: Produce element
empty = Produce (\ _ -> return True)

singleton :: element -> Produce element
singleton x = Produce (\ (Consume io) -> io x)

apConcurrently :: Produce (a -> b) -> Produce a -> Produce b
apConcurrently (Produce runConsume1) (Produce runConsume2) =
  Produce $ \ consume3 -> do
    elementVar1 <- newEmptyTMVarIO
    readyToProduceVar <- newTVarIO True
    readyToConsumeVar <- newTVarIO True
    forkIO $ do
      runConsume1 (A.putToVarWhileActive (readTVar readyToProduceVar) elementVar1)
      atomically (writeTVar readyToProduceVar False)
    runConsume2 (A.apWhileActive (readTVar readyToProduceVar) (writeTVar readyToConsumeVar False) elementVar1 consume3)
    atomically (writeTVar readyToProduceVar False)
    atomically (readTVar readyToConsumeVar)

alternate :: Produce a -> Produce a -> Produce a
alternate (Produce runConsume1) (Produce runConsume2) =
  Produce runConsume3
  where
    runConsume3 (Consume consumeElement3) = do
      elementVar <- newEmptyTMVarIO
      activeVar1 <- newTVarIO True
      activeVar2 <- newTVarIO True
      readyToConsumeVar <- newTVarIO True
      forkIO $ do
        runConsume1 (A.putToVarWhileActive (readTVar activeVar1) elementVar)
        atomically (writeTVar activeVar1 False)
      forkIO $ do
        runConsume2 (A.putToVarWhileActive (readTVar activeVar2) elementVar)
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
                    writeTVar readyToConsumeVar False
                    writeTVar activeVar1 False
                    writeTVar activeVar2 False
            handleShutdownOfProducers = do
              active1 <- readTVar activeVar1 
              active2 <- readTVar activeVar2 
              if active1 || active2
                then retry
                else return (return ())
            in join (atomically (processNextElementIfExists <|> handleShutdownOfProducers))
        in do
          processNextElement
          atomically (readTVar readyToConsumeVar)

concatConcurrently :: Foldable t => t (Produce element) -> Produce element
concatConcurrently list = Produce runConsume where
  runConsume (Consume consumeElement) = do
    elementVar <- newEmptyTMVarIO
    consumeIsActiveVar <- newTVarIO True
    activeProducersCountVar <- newTVarIO (length list)
    let
      checkWhetherToProduce = readTVar consumeIsActiveVar
    forM_ list $ \ (Produce subRunConsume) -> forkIO $ do
      subRunConsume $ A.putToVarWhileActive checkWhetherToProduce elementVar
      atomically $ modifyTVar' activeProducersCountVar pred
    let
      processNextElement =
        let
          processNextElementIfExists = do
            element <- takeTMVar elementVar
            return $ do
              active <- consumeElement element
              if active
                then processNextElement
                else atomically (writeTVar consumeIsActiveVar False)
          handleShutdownOfProducers = do
            activeProducersCount <- readTVar activeProducersCountVar
            if activeProducersCount == 0
              then return (return ())
              else retry
          in join (atomically (processNextElementIfExists <|> handleShutdownOfProducers))
      in do
        processNextElement
        atomically (readTVar consumeIsActiveVar)
