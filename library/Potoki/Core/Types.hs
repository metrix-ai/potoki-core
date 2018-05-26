module Potoki.Core.Types
where

import Potoki.Core.Prelude


{-|
Passive producer of elements.
-}
newtype Fetch element =
  Fetch (IO (Maybe element))

{-|
Passive producer of elements with support for early termination
and resource management.
-}
newtype Produce element =
  Produce (Acquire (Fetch element))

{-|
Active consumer of input into output.
Sort of like a reducer in Map/Reduce.

Automates the management of resources.
-}
newtype Consume input output =
  {-|
  An action, which executes the provided fetch in IO,
  while managing the resources behind the scenes.
  -}
  Consume (Fetch input -> IO output)

newtype Transform input output =
  Transform (Fetch input -> Acquire (Fetch output))
