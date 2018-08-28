module Potoki.Core.IO
where

import Potoki.Core.Prelude
import Potoki.Core.Types


produceAndReduce :: Produce input -> Reduce input output -> IO output
produceAndReduce (Produce produceIO) (Reduce reduceIO) =
  do
    (consume, finish) <- reduceIO
    produceIO consume
    finish

produceAndTransduceAndReduce :: Produce input1 -> Transduce input1 input2 -> Reduce input2 output -> IO output
produceAndTransduceAndReduce (Produce produceIO) (Transduce transduceIO) (Reduce reduceIO) =
  do
    (consume, finishReducer) <- reduceIO
    (transducedEatOne, finishTransducer) <- transduceIO consume
    produceIO transducedEatOne
    finishTransducer
    finishReducer
