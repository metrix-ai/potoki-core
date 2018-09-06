module Tests.Produce
where

import Prelude
import qualified Potoki.Core.IO as IO
import qualified Potoki.Core.Produce as P
import qualified Potoki.Core.ProduceSequentially as PS
import qualified Potoki.Core.Reduce as C
import qualified Data.Vector as V

monad :: Int -> IO ()
monad n =
  let list = [0..n]
      prod1 = PS.produce $ P.list list
      prod2 = \x -> PS.produce $ P.list $ enumFromTo 0 x
  in
    IO.produceAndReduce
      (unsafeCoerce $ prod1 >>= prod2)
      C.unit

produceList :: Int -> IO ()
produceList n =
  let list = [0..n]
  in
    IO.produceAndReduce
      (P.list list)
      C.unit

produceVector :: Int -> IO ()
produceVector n =
  let vector = V.fromList [0..n]
  in
    IO.produceAndReduce
      (P.vector vector)
      C.unit

produceAlternative :: Int -> IO ()
produceAlternative n =
  let list = [0..n]
      vector = V.fromList list
      prod1 = PS.produce $ P.list list
      prod2 = PS.produce $ P.vector vector
  in
    IO.produceAndReduce
      (unsafeCoerce $ prod1 <|> prod2)
      C.unit
