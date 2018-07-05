module Potoki.Core.Types
where

import Potoki.Core.Prelude


{-|
A specification of how to consume one input.
-}
newtype Consume input =
  Consume (input -> IO Bool)

newtype Produce element =
  Produce (Consume element -> IO ())

newtype Reduce element reduction =
  Reduce (IO (Consume element, IO reduction))

newtype Transduce input output =
  Transduce (Consume input -> Acquire (Consume output))

{-|
A producer which composes concurrently.
-}
newtype ProduceConcurrently element =
  ProduceConcurrently (Produce element)
