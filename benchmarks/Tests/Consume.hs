module Tests.Consume
where

import Prelude hiding (sum)
import Control.Foldl hiding (list)
import qualified Potoki.Core.IO as IO
import qualified Potoki.Core.Produce as P
import qualified Potoki.Core.Consume as C
import qualified Tests.Transform as BT
import qualified Data.Vector as V
import Tests.Choice (val2Either)

consumeUnit :: Int -> IO ()
consumeUnit n =
  let list = [0..n]
  in
    IO.produceAndConsume
      (P.list list)
      C.unit

consumeCount :: Int -> IO Int
consumeCount n =
  let list = [0..n]
  in
    IO.produceAndConsume
      (P.list list)
      C.count

consumeList :: Int -> IO [Int]
consumeList n =
  let list = [0..n]
  in
    IO.produceAndConsume
      (P.list list)
      C.list

consumeFold :: Int -> IO Int
consumeFold n =
  let list = [0..n]
  in
    IO.produceAndConsume
      (P.list list)
      (C.fold sum)

consumeVector :: Int -> IO (V.Vector Int)
consumeVector n =
  let list = [0..n]
  in
    IO.produceAndConsume
      (P.list list)
      C.vector

consumeApConcurrently :: Int -> IO Int
consumeApConcurrently n =
  let list = [0..n]
  in
    IO.produceAndConsume
      (P.list list)
      (C.apConcurrently (C.fold foldFunc) C.count)

consumeRight' :: Int -> IO (Either Int Int)
consumeRight' n =
  let list = fmap (val2Either even) [0..n]
  in
    IO.produceAndConsume
      (P.list list)
      (right' C.count)

foldFunc :: Fold Int (Int -> Int)
foldFunc = Fold step ini extr
  where
    ini = 0
    extr x = \b -> x + b
    step !acc !num = succ acc
