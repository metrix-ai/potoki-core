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
import qualified Potoki.Core.Transduce as Transduce


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
    ,
    testProperty "Sum" $ \ (list :: [Int]) ->
    let s = sum list
    in s === unsafePerformIO (IO.produceAndReduce (Produce.list list) Reduce.sum)
    ,
    transduce
  ]

transduce :: TestTree
transduce =
  testGroup "Transduce" $
  [
    testProperty "take" $ \ (list :: [Int], amount :: Int) ->
      let l = (take amount list)
      in l === unsafePerformIO (IO.produceAndTransduceAndReduce (Produce.list list) (Transduce.take amount) Reduce.list)
    ,
    testProperty "filter" $ \ (list :: [Int]) ->
      let l = (filter even list)
      in l === unsafePerformIO (IO.produceAndTransduceAndReduce (Produce.list list) (Transduce.filter even) Reduce.list)
    ,
    testProperty "list" $ \ (list :: [[Int]]) ->
    let l = concat list
    in l === unsafePerformIO (IO.produceAndTransduceAndReduce (Produce.list list) (Transduce.list) Reduce.list)
    ,
    transduceProfunctor
    ,
    transduceProduce
    ,
    transduceChoice
    ,
    transduceArrowLaws
  ]

transduceProfunctor =
  testGroup "Profunctor"
  [
    testProperty "dimap transduceId id show" $ \ (list :: [Int], amount :: Int) ->
      let l = map (show) list
      in l === unsafePerformIO (IO.produceAndTransduceAndReduce (Produce.list list) (dimap (show) (id) id) Reduce.list)
    ,
    testProperty "dimap transduceId (+1) id" $ \ (list :: [Int], amount :: Int) ->
    let l = map (+1) list
    in l === unsafePerformIO (IO.produceAndTransduceAndReduce (Produce.list list) (dimap (+1) (id) id) Reduce.list)
    ,
    testProperty "dimap transduceId (+1) show" $ \ (list :: [Int], amount :: Int) ->
    let l = map (show . (+1)) list
    in l === unsafePerformIO (IO.produceAndTransduceAndReduce (Produce.list list) (dimap (+1) (show) id) Reduce.list)
    ,
    testProperty "lmap" $ \ (list :: [Int], amount :: Int) ->
    let l = map (show) list
    in l === unsafePerformIO (IO.produceAndTransduceAndReduce (Produce.list list) (lmap show id) Reduce.list)
    ,
    testProperty "rmap" $ \ (list :: [Int], amount :: Int) ->
    let l = map (show) list
    in l === unsafePerformIO (IO.produceAndTransduceAndReduce (Produce.list list) (rmap show id) Reduce.list)
  ]

transduceProduce =
  testGroup "Produce" $
  [
    testCase "1" $ do
      let
        list = [1, 2, 3] :: [Int]
      result <- IO.produceAndTransduceAndReduce
        (Produce.list list)
        (Transduce.produce (Produce.list . \ n -> flip replicate n n))
        (Reduce.list)
      assertEqual "" [1, 2, 2, 3, 3, 3] result
  ,
    testCase "2" $ do
      let
        list = [1, 2, 3] :: [Int]
      result <- IO.produceAndTransduceAndReduce
        (Produce.list list)
        (Transduce.produce (Produce.list . \ n -> [(n, n)]))
        (Reduce.list)
      assertEqual "" [(1, 1), (2, 2), (3, 3)] result
  ,
    testCase "3" $ do
      let
        list = [1, 2, 3] :: [Int]
      result <- IO.produceAndTransduceAndReduce
        (Produce.list list)
        (Transduce.produce (Produce.list . \ n -> [n, n]))
        (Reduce.list)
      assertEqual "" [1, 1, 2, 2, 3, 3] result
  ]

transduceChoice =
  testGroup "Choice" $
  [
    testCase "1" $ do
      let
        list = [Left 1, Left 2, Right 'z', Left 2, Right 'a', Left 1, Right 'b', Left 0, Right 'x', Left 4, Left 3]
        transduce = left' id
      result <- IO.produceAndTransduceAndReduce (Produce.list list) transduce Reduce.list
      assertEqual "" [Left 1, Left 2, Right 'z', Left 2, Right 'a', Left 1, Right 'b', Left 0, Right 'x', Left 4, Left 3] result
    ,
    testCase "2" $ do
      let
        list = [Left 1, Left 2, Right 'z', Right 'a', Right 'b', Left 0, Right 'x', Left 4, Left 3]
        transduce = right' (Transduce.reduce Reduce.list) 
      result <- IO.produceAndTransduceAndReduce (Produce.list list) transduce Reduce.list
      assertEqual "" [Left 1, Left 2, Left 0, Left 4, Left 3, Right "zabx"] result
    ,
    testCase "3" $ do
      let
        list = [Left 4, Right 'z', Right 'a', Left 3, Right 'b', Left 0, Left 1, Right 'x', Left 4, Left 3]
        transduce = left' (Transduce.reduce Reduce.list)
      result <- IO.produceAndTransduceAndReduce (Produce.list list) transduce Reduce.list
      assertEqual "" [Right 'z', Right 'a', Right 'b', Right 'x', Left [4, 3, 0, 1, 4, 3]] result
  ]

transduceArrowLaws :: TestTree
transduceArrowLaws =
  testGroup "Arrow laws"
   [
       testGroup "Strong"
       [
       testCase "second" $ do
         let
           input = [('a',1),('b',2),('c',3),('d',4),('e', 5),('f',6)]
           transduce = second' $ Transduce.reduce (Reduce.transduce (Transduce.take 3) Reduce.sum)
           reduce = Reduce.list
         result <- IO.produceAndTransduceAndReduce (Produce.list input) transduce reduce
         assertEqual "" [('c', 6),('f',15)] result
       ,
       testCase "Transduce.reduce with take" $ do
         let
           input = [1,2,3,4,5,6]
           transduce = Transduce.reduce (Reduce.transduce (Transduce.take 3) Reduce.sum)
           reduce = Reduce.list
         result <- IO.produceAndTransduceAndReduce (Produce.list input) transduce reduce
         assertEqual "" [6, 15] result
       ,
       testCase "reduce" $ do
         let
           input = [1,2,3,4,5,6]
           transduce = Transduce.reduce Reduce.sum
           reduce = Reduce.list
         result <- IO.produceAndTransduceAndReduce (Produce.list input) transduce reduce
         assertEqual "" [21] result
       ,
       testCase "0" $ do
         let
           input = [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f')]
           transduce = first id
           reduce = (Reduce.transduce (Transduce.take 3) Reduce.list)
         result <- IO.produceAndTransduceAndReduce (Produce.list input) transduce reduce
         assertEqual "" [(1,'a'),(2,'b'),(3,'c')] result
       ,
       testCase "1" $ do
         let
           input = [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e')]
           transduce = first transduce1
         result <- IO.produceAndTransduceAndReduce (Produce.list input) transduce Reduce.list
         assertEqual "" [(6,'c'),(9,'e')] result
       ,
       testCase "Lack of elements" $ do
         let
           input = [(1,'a'),(2,'b')]
           transduce = first transduce1
         result <- IO.produceAndTransduceAndReduce (Produce.list input) transduce Reduce.list
         assertEqual "" [(3,'b')] result
     ]
     ,
     transduceProperty "arr id = id"
       (arr id :: Transduce.Transduce Int Int)
       id
     ,
     transduceProperty "arr (f >>> g) = arr f >>> arr g"
       (arr (f >>> g))
       (arr f >>> arr g)
     ,
     transduceProperty "first (arr f) = arr (first f)"
       (first (arr f) :: Transduce.Transduce (Int, Char) (Int, Char))
       (arr (first f))
     ,
     transduceProperty "first (f >>> g) = first f >>> first g"
       (first (transduce1 >>> transduce2) :: Transduce.Transduce (Int, Char) (Int, Char))
       (first (transduce1) >>> first (transduce2))
     ,
     transduceProperty "first f >>> arr fst = arr fst >>> f"
       (first transduce1 >>> arr fst :: Transduce.Transduce (Int, Char) Int)
       (arr fst >>> transduce1)
     ,
     transduceProperty "first f >>> arr (id *** g) = arr (id *** g) >>> first f"
       (first transduce1 >>> arr (id *** g))
       (arr (id *** g) >>> first transduce1)
     ,
     transduceProperty "first (first f) >>> arr assoc = arr assoc >>> first f"
       (first (first transduce1) >>> arr assoc :: Transduce.Transduce ((Int, Char), Double) (Int, (Char, Double)))
       (arr assoc >>> first transduce1)
     ,
     transduceProperty "left (arr f) = arr (left f)"
       (left (arr f) :: Transduce.Transduce (Either Int Char) (Either Int Char))
       (arr (left f))
     ,
     transduceProperty "left (f >>> g) = left f >>> left g"
       (left (transduce1 >>> transduce2) :: Transduce.Transduce (Either Int Char) (Either Int Char))
       (left (transduce1) >>> left (transduce2))
     ,
     transduceProperty "f >>> arr Left = arr Left >>> left f"
       (transduce1 >>> arr Left :: Transduce.Transduce Int (Either Int Char))
       (arr Left >>> left transduce1)
     ,
     transduceProperty "left f >>> arr (id +++ g) = arr (id +++ g) >>> left f"
       (left transduce1 >>> arr (id +++ g))
       (arr (id +++ g) >>> left transduce1)
     ,
     transduceProperty "left (left f) >>> arr assocsum = arr assocsum >>> left f"
       (left (left transduce1) >>> arr assocsum :: Transduce.Transduce (Either (Either Int Char) Double) (Either Int (Either Char Double)))
       (arr assocsum >>> left transduce1)
     ,
     transduceProperty "left (left (arr f)) >>> arr assocsum = arr assocsum >>> left (arr f)"
       (left (left (arr f)) >>> arr assocsum :: Transduce.Transduce (Either (Either Int Char) Double) (Either Int (Either Char Double)))
       (arr assocsum >>> left (arr f))
   ]
   where
     f = (+24) :: Int -> Int
     g = (*3) :: Int -> Int
     transduce1 = Transduce.reduce (Reduce.transduce (Transduce.take 3) Reduce.sum) :: Transduce.Transduce Int Int
     transduce2 = Transduce.reduce (Reduce.transduce (Transduce.take 4) Reduce.sum) :: Transduce.Transduce Int Int
     assoc ((a,b),c) = (a,(b,c))
     assocsum (Left (Left x)) = Left x
     assocsum (Left (Right y)) = Right (Left y)
     assocsum (Right z) = Right (Right z)

transduceProperty ::
 (Arbitrary input, Show input, Eq output, Show output) =>
 String -> Transduce.Transduce input output -> Transduce.Transduce input output -> TestTree
transduceProperty name leftTransform rightTransform =
 testProperty name property
 where
   property list =
     transduce leftTransform === transduce rightTransform
     where
       transduce transduce =
         unsafePerformIO (IO.produceAndTransduceAndReduce (Produce.list list) transduce Reduce.list)
