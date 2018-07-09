module Potoki.Core.Types
where

import Potoki.Core.Prelude


{-|
A specification of how to consume one input.
-}
newtype Consume input =
  Consume (input -> IO Bool)

newtype Produce element =
  {-| An action, which executes the consuming action, and indicates,
  whether the consumer is still ready to process more input. -}
  Produce (Consume element -> IO Bool)

newtype Reduce element reduction =
  Reduce (IO (Consume element, IO reduction))

newtype ReduceSequentially element reduction =
  ReduceSequentially (IO (Consume element, IO (Maybe reduction)))

newtype ReduceZipping element reduction =
  ReduceZipping (Reduce element reduction)

newtype Transduce input output =
  Transduce (Consume output -> IO (Consume input, IO ()))

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
