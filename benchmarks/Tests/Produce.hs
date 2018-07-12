module Tests.Produce
where

import Prelude
import qualified Potoki.Core.IO as IO
import qualified Potoki.Core.Produce as P
import qualified Potoki.Core.Consume as C
import qualified Data.Vector as V

monad :: Int -> IO Int
monad n =
  let list = [0..n]
      prod1 = P.list list
      prod2 = \x -> P.list $ enumFromTo 0 x
  in
    IO.produceAndConsume
      (prod1 >>= prod2)
      C.count

produceList :: Int -> IO Int
produceList n =
  let list = [0..n]
  in
    IO.produceAndConsume
      (P.list list)
      C.count

produceVector :: Int -> IO Int
produceVector n =
  let vector = V.fromList [0..n]
  in
    IO.produceAndConsume
      (P.vector vector)
      C.count

produceAlternative :: Int -> IO Int
produceAlternative n =
  let list = [0..n]
      vector = V.fromList list
      prod1 = P.list list
      prod2 = P.vector vector
  in
    IO.produceAndConsume
      (prod1 <|> prod2)
      C.count
