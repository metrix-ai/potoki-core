module Potoki.Core.Transform.State
where

import Potoki.Core.Prelude
import Potoki.Core.Types
import Potoki.Core.Transform.Instances
import qualified Potoki.Core.Fetch as A
import qualified Data.ByteString as B
import qualified Control.Monad.Trans.State.Strict as O
import qualified Acquire.Acquire as M


{-|
Notice that you can control the emission of output of each step
by producing a list of outputs and then composing the transform with
the "list" transform.
-}
{-# INLINE runState #-}
runState :: (a -> O.State s b) -> s -> Transform a (s, b)
runState stateFn initialState =
  Transform $ \ (A.Fetch fetchIO) -> M.Acquire $ do
    stateRef <- newIORef initialState
    return $ (, return ()) $ A.Fetch $  
      fetchIO >>= \case
        Just input -> do
          currentState <- readIORef stateRef
          case O.runState (stateFn input) currentState of
            (output, newState) -> do
              writeIORef stateRef newState
              return (Just (newState, output))
        Nothing -> return Nothing

{-# INLINE evalState #-}
evalState :: (a -> O.State s b) -> s -> Transform a b
evalState stateFn initialState =
  runState stateFn initialState >>> arr snd

{-# INLINE execState #-}
execState :: (a -> O.State s b) -> s -> Transform a s
execState stateFn initialState =
  runState stateFn initialState >>> arr fst