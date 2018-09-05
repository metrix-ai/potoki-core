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
    bgroup "Transforms"
    [
      bench "extractLinesWithoutTrailAsPoking" $ whnfIO $ produceAndConsume
        (Produce.fileBytes "data/2.tsv")
        (right' (Consume.transform
          (Transform.extractLinesWithoutTrailAsPoking)
          (Consume.list)))
      ,
      bench "extractLinesWithoutTrail" $ whnfIO $ produceAndConsume
        (Produce.fileBytes "data/2.tsv")
        (right' (Consume.transform
          (Transform.extractLinesWithoutTrail)
          (Consume.list)))
      ,
      bench "extractLines" $ whnfIO $ produceAndConsume
        (Produce.fileBytes "data/2.tsv")
        (right' (Consume.transform
          (Transform.extractLines)
          (Consume.list)))
    ]
  ]
