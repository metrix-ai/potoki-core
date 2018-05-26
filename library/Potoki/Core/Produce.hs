module Potoki.Core.Produce
(
  Produce(..),
  list,
  transform,
)
where

import Potoki.Core.Prelude
import Potoki.Core.Types
import qualified Potoki.Core.Fetch as A


deriving instance Functor Produce

instance Applicative Produce where
  pure x = Produce $ do
    refX <- liftIO (newIORef (Just x))
    return (A.maybeRef refX)
  (<*>) (Produce leftAcquire) (Produce rightAcquire) =
    Produce ((<*>) <$> leftAcquire <*> rightAcquire)

instance Alternative Produce where
  empty =
    Produce (pure empty)
  (<|>) (Produce leftAcquire) (Produce rightAcquire) =
    Produce ((<|>) <$> leftAcquire <*> rightAcquire)

instance Monad Produce where
  return = pure
  (>>=) (Produce (Acquire io1)) k2 =
    Produce $ Acquire $ do
      (fetch1, release1) <- io1
      release2Ref <- newIORef (return ())
      let
        fetch2 input1 =
          case k2 input1 of
            Produce (Acquire io2) ->
              A.ioFetch $ do
                join (readIORef release2Ref)
                (fetch2', release2') <- io2
                writeIORef release2Ref release2'
                return fetch2'
        release3 =
          join (readIORef release2Ref) >> release1
        in return (fetch1 >>= fetch2, release3)

instance MonadIO Produce where
  liftIO io =
    Produce (return (liftIO io))

{-# INLINABLE list #-}
list :: [input] -> Produce input
list inputList =
  Produce $ liftIO (A.list <$> newIORef inputList)

{-# INLINE transform #-}
transform :: Transform input output -> Produce input -> Produce output
transform (Transform transformAcquire) (Produce produceAcquire) =
  Produce $ do
    fetch <- produceAcquire
    newFetch <- transformAcquire
    return $ newFetch fetch
