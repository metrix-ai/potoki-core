module Tests.Reduce
where

import Prelude hiding (sum)
import Control.Foldl hiding (list)
import qualified Potoki.Core.IO as IO
import qualified Potoki.Core.Produce as P
import qualified Potoki.Core.Reduce as R
import qualified Tests.Transduce as BT
import qualified Data.Vector as V
import Tests.Choice (val2Either)

reduceUnit :: Int -> IO ()
reduceUnit n =
  let list = [0..n]
  in
    IO.produceAndReduce
      (P.list list)
      R.unit

reduceCount :: Int -> IO Int
reduceCount n =
  let list = [0..n]
  in
    IO.produceAndReduce
      (P.list list)
      R.count

reduceList :: Int -> IO [Int]
reduceList n =
  let list = [0..n]
  in
    IO.produceAndReduce
      (P.list list)
      R.list

reduceFold :: Int -> IO Int
reduceFold n =
  let list = [0..n]
  in
    IO.produceAndReduce
      (P.list list)
      (R.fold sum)

reduceVector :: Int -> IO (V.Vector Int)
reduceVector n =
  let list = [0..n]
  in
    IO.produceAndReduce
      (P.list list)
      R.vector

-- reduceApConcurrently :: Int -> IO Int
-- reduceApConcurrently n =
--   let list = [0..n]
--   in
--     IO.produceAndReduce
--       (P.list list)
--       (R.apConcurrently (R.fold foldFunc) R.count)

reduceRight' :: Int -> IO (Either Int Int)
reduceRight' n =
  let list = fmap (val2Either even) [0..n]
  in
    IO.produceAndReduce
      (P.list list)
      (right' R.count)

foldFunc :: Fold Int (Int -> Int)
foldFunc = Fold step ini extr
  where
    ini = 0
    extr x = \b -> x + b
    step !acc !num = succ acc
