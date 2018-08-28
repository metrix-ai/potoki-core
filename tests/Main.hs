module Main where

import Prelude hiding (first, second)
import Control.Arrow
import qualified Control.Foldl as Foldl
import Test.QuickCheck.Instances
import Test.QuickCheck.Monadic as M
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Potoki.Core.IO as IO
import qualified Potoki.Core.Reduce as Reduce
import qualified Potoki.Core.Produce as Produce


main =
  defaultMain $
  testGroup "All tests" $
  [
    testProperty "list to list" $ \ (list :: [Int]) ->
    list === unsafePerformIO (IO.produceAndReduce (Produce.list list) Reduce.list)
    ,
    testProperty "list with FoldM" $ \ (list :: [Int]) ->
    let fold = Foldl.generalize $ Foldl.list
    in list === unsafePerformIO (IO.produceAndReduce (Produce.list list) (Reduce.foldM fold))
    ,
    testProperty "FoldM list to Reduce list" $ \ (list :: [Int]) ->
    let fold = Foldl.generalize $ Foldl.list
    in unsafePerformIO (IO.produceAndReduce (Produce.list list) Reduce.list)
      === unsafePerformIO (IO.produceAndReduce (Produce.list list) (Reduce.foldM fold))
    ,
    testProperty "Count" $ \ (list :: [Int]) ->
    let len = length list
    in len === unsafePerformIO (IO.produceAndReduce (Produce.list list) Reduce.count)
  ]
