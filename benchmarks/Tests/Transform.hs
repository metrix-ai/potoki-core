module Tests.Transform
where

import Prelude hiding (take)
import Potoki.Core.Transform
import qualified Potoki.Core.IO as IO
import qualified Potoki.Core.Produce as P
import qualified Potoki.Core.Consume as C


transformSucc :: Transform Int Int
transformSucc =
  arr succ

transformNot :: Transform Bool Bool
transformNot =
  arr not

transformTake :: Int -> IO Int
transformTake n =
  let list = [0..n]
  in
    IO.produceAndTransformAndConsume
      (P.list list)
      (take n)
      C.sum
