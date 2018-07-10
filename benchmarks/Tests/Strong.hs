module Tests.Strong
where

import Prelude
import Control.Foldl hiding (list)
import qualified Potoki.Core.IO as IO
import qualified Potoki.Core.Produce as P
import qualified Potoki.Core.Consume as C
import qualified Tests.Transform as BT

val2Tuple :: (Int -> Bool) -> Int -> (Int, Bool)
val2Tuple predicate val =
  if predicate val
    then (val, True)
    else (val, False)

foldTuple :: Fold (Int, Bool) (Int, Bool)
foldTuple = Fold step ini extr
  where
    ini = (0, True)
    extr = id
    step (!num, !flag) (accInt, accBool) = (num + accInt, flag && accBool)

testStrong1 :: Int -> IO (Int, Bool)
testStrong1 n =
  let list = fmap (val2Tuple even) [0..n]
  in
    IO.produceAndConsume
      (P.list list)
      (C.fold foldTuple)

testStrong2 :: Int -> IO (Int, Bool)
testStrong2 n =
  let list = fmap (val2Tuple  even) [0..n]
  in
    IO.produceAndTransformAndConsume
      (P.list list)
      (first' id)
      (C.fold foldTuple)

testStrong3 :: Int -> IO (Int, Bool)
testStrong3 n =
  let list = fmap (val2Tuple  even) [0..n]
  in
    IO.produceAndTransformAndConsume
      (P.list list)
      (second' id)
      (C.fold foldTuple)

testStrong4 :: Int -> IO (Int, Bool)
testStrong4 n =
  let list = fmap (val2Tuple  even) [0..n]
  in
    IO.produceAndTransformAndConsume
      (P.list list)
      (first' BT.transformSucc)
      (C.fold foldTuple)

testStrong5 :: Int -> IO (Int, Bool)
testStrong5 n =
  let list = fmap (val2Tuple  even) [0..n]
  in
    IO.produceAndTransformAndConsume
      (P.list list)
      (second' BT.transformNot)
      (C.fold foldTuple)

-- testStrong1' :: Int -> IO (Int, Bool)
-- testStrong1' n =
--   IO.produceAndConsume
--     (BP.getIntAndBool n)
--     BC.sumOfIntAndBool
--
-- testStrong2' :: Int -> IO (Int, Bool)
-- testStrong2' n =
--   IO.produceAndTransformAndConsume
--     (BP.getIntAndBool n)
--     (first' id)
--     BC.sumOfIntAndBool
--
-- testStrong3' :: Int -> IO (Int, Bool)
-- testStrong3' n =
--   IO.produceAndTransformAndConsume
--     (BP.getIntAndBool n)
--     (second' id)
--     BC.sumOfIntAndBool
--
-- testStrong4' :: Int -> IO (Int, Bool)
-- testStrong4' n =
--   IO.produceAndTransformAndConsume
--     (BP.getIntAndBool n)
--     (first' BT.transformSucc)
--     BC.sumOfIntAndBool
--
-- testStrong5' :: Int -> IO (Int, Bool)
-- testStrong5' n =
--   IO.produceAndTransformAndConsume
--     (BP.getIntAndBool n)
--     (second' BT.transformNot)
--     BC.sumOfIntAndBool
