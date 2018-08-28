module Tests.Choice
where

import Prelude
import qualified Potoki.Core.IO as IO
import qualified Potoki.Core.Produce as P
import qualified Potoki.Core.Reduce as C
import qualified Tests.Transform as BT

val2Either :: (a -> Bool) -> a -> Either a a
val2Either predicate val =
  if predicate val
    then Right val
    else Left val

testChoice1 :: Int -> IO Int
testChoice1 n =
  let list = fmap (val2Either even) [0..n]
  in
    IO.produceAndReduce
      (P.list list)
      C.count

testChoice2 :: Int -> IO Int
testChoice2 n =
  let list = fmap (val2Either even) [0..n]
  in
    IO.produceAndTransduceAndReduce
      (P.list list)
      (left' id)
      C.count

testChoice3 :: Int -> IO Int
testChoice3 n =
  let list = fmap (val2Either even) [0..n]
  in
    IO.produceAndTransduceAndReduce
      (P.list list)
      (right' id)
      C.count

testChoice4 :: Int -> IO Int
testChoice4 n =
  let list = fmap (val2Either even) [0..n]
  in
    IO.produceAndTransduceAndReduce
      (P.list list)
      (left' BT.transformSucc)
      C.count

testChoice5 :: Int -> IO Int
testChoice5 n =
  let list = fmap (val2Either even) [0..n]
  in
    IO.produceAndTransduceAndReduce
      (P.list list)
      (right' BT.transformSucc)
      C.count
