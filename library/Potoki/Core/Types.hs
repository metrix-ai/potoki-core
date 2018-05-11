module Potoki.Core.Types
where

import Potoki.Core.Prelude


{-|
Implementation of http://www.haskellforall.com/2013/06/the-resource-applicative.html
-}
newtype Acquire resource =
  Acquire (IO (resource, IO ()))

{-|
Passive producer of elements with support for early termination.
-}
newtype Fetch element =
  {-|
  Something close to a Church encoding of @IO (Maybe element)@.
  -}
  Fetch (forall x. x -> (element -> x) -> IO x)

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
  Transform (Acquire (Fetch input -> Fetch output))
