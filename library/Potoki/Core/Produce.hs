module Potoki.Core.Produce
(
  Produce(..),
  list,
  vector,
  foldable,
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
  unfoldlM,
)
where

import Potoki.Core.Prelude hiding (empty)
import Potoki.Core.Types
import qualified Potoki.Core.Push as A
import qualified DeferredFolds.UnfoldlM as B


instance Functor Produce where
  fmap fn (Produce io) = Produce (io . contramap fn)

instance Pointed Produce where
  point = singleton

{-# INLINABLE list #-}
list :: [element] -> Produce element
list = foldable

{-# INLINE vector #-}
vector :: Vector element -> Produce element
vector = foldable

{-# INLINE foldable #-}
foldable :: (Foldable t) => t element -> Produce element
foldable input = Produce $ \ (Push io) ->
  let step element nextIO = do
        hungry <- io element
        if hungry
          then nextIO
          else return False
  in foldr step (return True) input

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
singleton x = Produce (\ (Push io) -> io x)

apSequentially :: Produce (a -> b) -> Produce a -> Produce b
apSequentially (Produce runPush1) (Produce runPush2) =
  Produce $ \ (Push consumeIO2) ->
  runPush1 $ Push $ \ element1 ->
  runPush2 $ Push $ \ element2 ->
  consumeIO2 $ element1 element2

apConcurrently :: Produce (a -> b) -> Produce a -> Produce b
apConcurrently (Produce runPush1) (Produce runPush2) =
  Produce $ \ consume3 -> do
    elementVar1 <- newEmptyTMVarIO
    readyToProduceVar <- newTVarIO True
    readyToPushVar <- newTVarIO True
    forkIO $ do
      runPush1 (A.putToVarWhileActive (readTVar readyToProduceVar) elementVar1)
      atomically (writeTVar readyToProduceVar False)
    runPush2 (A.apWhileActive (readTVar readyToProduceVar) (writeTVar readyToPushVar False) elementVar1 consume3)
    atomically (writeTVar readyToProduceVar False)
    atomically (readTVar readyToPushVar)

alternate :: Produce a -> Produce a -> Produce a
alternate (Produce runPush1) (Produce runPush2) =
  Produce runPush3
  where
    runPush3 (Push consumeElement3) = do
      elementVar <- newEmptyTMVarIO
      activeVar1 <- newTVarIO True
      activeVar2 <- newTVarIO True
      readyToPushVar <- newTVarIO True
      forkIO $ do
        runPush1 (A.putToVarWhileActive (readTVar activeVar1) elementVar)
        atomically (writeTVar activeVar1 False)
      forkIO $ do
        runPush2 (A.putToVarWhileActive (readTVar activeVar2) elementVar)
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
                    writeTVar readyToPushVar False
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
          atomically (readTVar readyToPushVar)

prepend :: Produce a -> Produce a -> Produce a
prepend (Produce runPush1) (Produce runPush2) =
  Produce $ \ consume -> do
    readyToPush <- runPush1 consume
    if readyToPush
      then runPush2 consume
      else return False

concatConcurrently :: Foldable t => t (Produce element) -> Produce element
concatConcurrently list = Produce runPush where
  runPush (Push consumeElement) = do
    elementVar <- newEmptyTMVarIO
    consumeIsActiveVar <- newTVarIO True
    activeProducersCountVar <- newTVarIO (length list)
    let
      checkWhetherToProduce = readTVar consumeIsActiveVar
    forM_ list $ \ (Produce subRunPush) -> forkIO $ do
      subRunPush $ A.putToVarWhileActive checkWhetherToProduce elementVar
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
bind (Produce runPush1) k2 =
  Produce $ \ consume2 ->
  runPush1 $ Push $ \ element1 ->
  case k2 element1 of
    Produce runPush2 ->
      runPush2 consume2 $> True

transduce :: Transduce a b -> Produce a -> Produce b
transduce (Transduce transduceIO) (Produce produceIO) =
  Produce $ \ consume -> do
    (transducedPush, finishTransducer) <- transduceIO consume
    produceIO transducedPush <* finishTransducer

unfoldlM :: UnfoldlM IO a -> Produce a
unfoldlM (UnfoldlM fold) =
  Produce $ \ (Push consume) ->
  let step state input = if state then consume input else return False
      in fold step True
