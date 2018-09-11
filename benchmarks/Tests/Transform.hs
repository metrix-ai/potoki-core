module Tests.Transform
where

import Prelude hiding (take, filter)
import Potoki.Core.Transform
import qualified Data.Vector as Vec
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

transformFilter :: Int -> IO Int
transformFilter n =
  let list = [0..n]
  in
    IO.produceAndTransformAndConsume
      (P.list list)
      (filter even)
      C.sum  

transformVector :: Int -> IO Int
transformVector n =
  let list = fmap (\x -> Vec.replicate x 1) [0..n]
  in
    IO.produceAndTransformAndConsume
      (P.list list)
      (vector)
      C.sum        