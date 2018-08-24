module Potoki.Core.IO.Fetch where

import Potoki.Core.Prelude
import Potoki.Core.Types

{-| Fetch all the elements running the provided handler on them -}
fetchAndHandleAll :: Fetch element -> IO () -> (element -> IO ()) -> IO ()
fetchAndHandleAll (Fetch fetchIO) onEnd onElement =
  let
    loop = do
      fetch <- fetchIO
      case fetch of
        Nothing      -> onEnd
        Just element -> onElement element >> loop
    in loop

{-| Fetch and handle just one element -}
fetchAndHandle :: Fetch element -> IO a -> (element -> IO a) -> IO a
fetchAndHandle (Fetch fetchIO) onEnd onElement =
  do
    fetch <- fetchIO
    case fetch of
      Nothing      -> onEnd
      Just element -> onElement element