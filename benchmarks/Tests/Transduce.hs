module Tests.Transduce
where

import Prelude hiding (take)
import Potoki.Core.Transduce
import qualified Potoki.Core.IO as IO
import qualified Potoki.Core.Produce as P
import qualified Potoki.Core.Reduce as R

transduceSucc :: Transduce Int Int
transduceSucc =
  arr succ

transduceNot :: Transduce Bool Bool
transduceNot =
  arr not

transduceTake :: Int -> IO Int
transduceTake n =
  let list = [0..n]
  in
    IO.produceAndTransduceAndReduce
      (P.list list)
      (take n)
      R.sum
