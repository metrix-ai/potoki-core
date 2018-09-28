module Potoki.Core.Types
where

import Potoki.Core.Prelude


{-|
A specification of how to send one input value.
-}
newtype Push input =
  Push (input -> IO Bool)

newtype Produce element =
  {-| An action, which executes the consuming action, and indicates,
  whether the consumer is still ready to process more input. -}
  Produce (Push element -> IO Bool)

{-|
A producer which composes concurrently.
-}
newtype ProduceConcurrently element =
  ProduceConcurrently (Produce element)

{-|
A producer which composes sequentially.
-}
newtype ProduceSequentially element =
  ProduceSequentially (Produce element)

newtype Reduce element reduction =
  Reduce (IO (Push element, IO reduction))

newtype ReduceSequentially element reduction =
  ReduceSequentially (Reduce element (Maybe reduction))

newtype ReduceZipping element reduction =
  ReduceZipping (Reduce element reduction)

newtype Transduce input output =
  Transduce (Push output -> IO (Push input, IO ()))

{-|
Same as 'Transduce',
only comes with guarantees that it's safe
to use it concurrently.
-}
newtype TransduceConcurrently input output =
  TransduceConcurrently (Transduce input output)
