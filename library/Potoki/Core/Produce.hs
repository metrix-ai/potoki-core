module Potoki.Core.Produce
(
  Produce(..),
  concurrently,
  sequentially,
  empty,
  singleton,
  apSequentially,
  apConcurrently,
  alternate,
  prepend,
  concatConcurrently,
  bind,
  transduce,
  unfoldM,
)
where

import Potoki.Core.Prelude hiding (empty)
import Potoki.Core.Types
import qualified Potoki.Core.EatOne as A
import qualified DeferredFolds.UnfoldM as B


instance Functor Produce where
  fmap fn (Produce io) = Produce (io . contramap fn)

instance Pointed Produce where
  point = singleton

{-|
Unlift a concurrently composed producer.
-}
concurrently :: ProduceConcurrently element -> Produce element
concurrently (ProduceConcurrently produce) = produce

{-|
Unlift a sequentially composed producer.
-}
sequentially :: ProduceSequentially element -> Produce element
sequentially (ProduceSequentially produce) = produce

empty :: Produce element
empty = Produce (\ _ -> return True)

singleton :: element -> Produce element
singleton x = Produce (\ (EatOne io) -> io x)

apSequentially :: Produce (a -> b) -> Produce a -> Produce b
apSequentially (Produce runEatOne1) (Produce runEatOne2) =
  Produce $ \ (EatOne consumeIO2) ->
  runEatOne1 $ EatOne $ \ element1 ->
  runEatOne2 $ EatOne $ \ element2 ->
  consumeIO2 $ element1 element2

apConcurrently :: Produce (a -> b) -> Produce a -> Produce b
apConcurrently (Produce runEatOne1) (Produce runEatOne2) =
  Produce $ \ consume3 -> do
    elementVar1 <- newEmptyTMVarIO
    readyToProduceVar <- newTVarIO True
    readyToEatOneVar <- newTVarIO True
    forkIO $ do
      runEatOne1 (A.putToVarWhileActive (readTVar readyToProduceVar) elementVar1)
      atomically (writeTVar readyToProduceVar False)
    runEatOne2 (A.apWhileActive (readTVar readyToProduceVar) (writeTVar readyToEatOneVar False) elementVar1 consume3)
    atomically (writeTVar readyToProduceVar False)
    atomically (readTVar readyToEatOneVar)

alternate :: Produce a -> Produce a -> Produce a
alternate (Produce runEatOne1) (Produce runEatOne2) =
  Produce runEatOne3
  where
    runEatOne3 (EatOne consumeElement3) = do
      elementVar <- newEmptyTMVarIO
      activeVar1 <- newTVarIO True
      activeVar2 <- newTVarIO True
      readyToEatOneVar <- newTVarIO True
      forkIO $ do
        runEatOne1 (A.putToVarWhileActive (readTVar activeVar1) elementVar)
        atomically (writeTVar activeVar1 False)
      forkIO $ do
        runEatOne2 (A.putToVarWhileActive (readTVar activeVar2) elementVar)
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
                    writeTVar readyToEatOneVar False
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
          atomically (readTVar readyToEatOneVar)

prepend :: Produce a -> Produce a -> Produce a
prepend (Produce runEatOne1) (Produce runEatOne2) =
  Produce $ \ consume -> do
    readyToEatOne <- runEatOne1 consume
    if readyToEatOne
      then runEatOne2 consume
      else return False

concatConcurrently :: Foldable t => t (Produce element) -> Produce element
concatConcurrently list = Produce runEatOne where
  runEatOne (EatOne consumeElement) = do
    elementVar <- newEmptyTMVarIO
    consumeIsActiveVar <- newTVarIO True
    activeProducersCountVar <- newTVarIO (length list)
    let
      checkWhetherToProduce = readTVar consumeIsActiveVar
    forM_ list $ \ (Produce subRunEatOne) -> forkIO $ do
      subRunEatOne $ A.putToVarWhileActive checkWhetherToProduce elementVar
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

bind :: Produce a -> (a -> Produce b) -> Produce b
bind (Produce runEatOne1) k2 =
  Produce $ \ consume2 ->
  runEatOne1 $ EatOne $ \ element1 ->
  case k2 element1 of
    Produce runEatOne2 ->
      runEatOne2 consume2 $> True

transduce :: Transduce a b -> Produce a -> Produce b
transduce (Transduce transduceIO) (Produce produceIO) =
  Produce $ \ consume -> do
    (transducedEatOne, finishTransducer) <- transduceIO consume
    produceIO transducedEatOne <* finishTransducer

unfoldM :: UnfoldM IO a -> Produce a
unfoldM (UnfoldM fold) =
  Produce $ \ (EatOne consume) ->
  let step state input = if state then consume input else return False
      in fold step True
