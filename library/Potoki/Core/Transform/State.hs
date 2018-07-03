module Potoki.Core.Transform.State
where

import Potoki.Core.Prelude
import Potoki.Core.Types
import Potoki.Core.Transform.Instances ()
import qualified Potoki.Core.Fetch as A
import qualified Control.Monad.Trans.State.Strict as O
import qualified Acquire.Acquire as M


{-|
Notice that you can control the emission of output of each step
by producing a list of outputs and then composing the transform with
the "list" transform.
-}
{-# INLINE runState #-}
runState :: (input -> O.State state output) -> state -> Transform input (state, output)
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
evalState :: (input -> O.State state output) -> state -> Transform input output
evalState stateFn initialState =
  runState stateFn initialState >>> arr snd

{-# INLINE execState #-}
execState :: (input -> O.State state output) -> state -> Transform input state
execState stateFn initialState =
  runState stateFn initialState >>> arr fst
