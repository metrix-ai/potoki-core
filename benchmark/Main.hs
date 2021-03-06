module Main where

import Prelude
import Criterion.Main
import Potoki.Core.IO
import qualified Potoki.Core.Produce as Produce
import qualified Potoki.Core.Consume as Consume
import qualified Potoki.Core.Transform as Transform


main =
  defaultMain $
  [
    bench "extractLinesConcurrently" $ whnfIO $ produceAndConsume
      (Produce.fileBytes "data/2.tsv")
      (right' (Consume.transform
        (Transform.extractLinesConcurrently numCapabilities)
        (Consume.count)))
    ,
    bench "extractLines" $ whnfIO $ produceAndConsume
      (Produce.fileBytes "data/2.tsv")
      (right' (Consume.transform
        (Transform.extractLines)
        (Consume.count)))
  ]
