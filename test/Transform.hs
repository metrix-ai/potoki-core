module Transform where

import Prelude hiding (first, second, choose)
import Control.Arrow
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Potoki.Core.IO as C
import qualified Potoki.Core.Consume as D
import qualified Potoki.Core.Transform as A
import qualified Potoki.Core.Produce as E
import qualified Data.Attoparsec.ByteString.Char8 as B
import qualified Data.ByteString as F
import qualified Data.Vector as G
import qualified Data.List.Split as SplitList
import qualified System.Random as H

transform :: TestTree
transform =
  testGroup "Transform" $
  [
    testGroup "concurrentlyInOrder unit tests" $ let
      testList (list :: [[Int]]) =
        testCase (show list) $ do
          actual <- C.produceAndTransformAndConsume (E.list list) (A.concurrentlyInOrder 4 A.list) D.list
          assertEqual "" (concat list) actual
      in [
          testList [[]] ,
          testList [[],[]],
          testList [[0, 1, 2], []],
          testList [[0, 1, 2], [3]],
          testList [[0,3,7,-2],[],[],[-8,7,-8,7,3,-4,1,4],[-4,1],[5,-2,3,-2,-5,6,-7],[-8,6,7,1,8,-8,5]]
        ]
    ,
    testProperty "concurrentlyInOrder" $ \ (list :: [[Int]]) ->
      concat list ===
      unsafePerformIO (C.produceAndTransformAndConsume (E.list list) (A.concurrentlyInOrder 4 A.list) D.list)
    ,
    testProperty "list" $ \ (list :: [[Int]]) -> 
      concat list ===
      unsafePerformIO (C.produceAndTransformAndConsume (E.list list) A.list D.list)
    ,
    testProperty "Applying chunksOf to list has the same effect as the \"batch\" transform" $ let
      gen = do
        list <- listOf (choose (0, 1000 :: Int))
        batchSize <- frequency [(1000, choose (1, 3)), (100, choose (4, 100)), (1, pure 0)]
        traceShowM (list, batchSize)
        return (list, batchSize)
      in forAll gen $ \ (list, batchSize) -> let
        listChunks = if batchSize < 1 then [] else SplitList.chunksOf batchSize list
        potokiChunks = unsafePerformIO $ C.produceAndTransformAndConsume (E.list list) (rmap toList (A.batch @Vector batchSize)) D.list
        in listChunks === potokiChunks
    ,
    transformProduce
    ,
    transformChoice
    ,
    transformArrowLaws
  ]

transformProduce =
  testGroup "Produce" $
  [
    testCase "1" $ do
      let
        list = [1, 2, 3] :: [Int]
      result <- C.produceAndTransformAndConsume
        (E.list list)
        (A.produce (E.list . \ n -> flip replicate n n))
        (D.list)
      assertEqual "" [1, 2, 2, 3, 3, 3] result
  ,
    testCase "2" $ do
      let
        list = [1, 2, 3] :: [Int]
      result <- C.produceAndTransformAndConsume
        (E.list list)
        (A.produce (E.list . \ n -> [(n, n)]))
        (D.list)
      assertEqual "" [(1, 1), (2, 2), (3, 3)] result
  ,
    testCase "3" $ do
      let
        list = [1, 2, 3] :: [Int]
      result <- C.produceAndTransformAndConsume
        (E.list list)
        (A.produce (E.list . \ n -> [n, n]))
        (D.list)
      assertEqual "" [1, 1, 2, 2, 3, 3] result
  ]

transformChoice =
  testGroup "Choice" $
  [
    testCase "1" $ do
      let
        list = [Left 1, Left 2, Right 'z', Left 2, Right 'a', Left 1, Right 'b', Left 0, Right 'x', Left 4, Left 3]
        transform = left' id
      result <- C.produceAndTransformAndConsume (E.list list) transform D.list
      assertEqual "" [Left 1, Left 2, Right 'z', Left 2, Right 'a', Left 1, Right 'b', Left 0, Right 'x', Left 4, Left 3] result
    ,
    testCase "2" $ do
      let
        list = [Left 1, Left 2, Right 'z', Right 'a', Right 'b', Left 0, Right 'x', Left 4, Left 3]
        transform = right' (A.consume D.list)
      result <- C.produceAndTransformAndConsume (E.list list) transform D.list
      assertEqual "" [Left 1, Left 2, Right "zab", Left 0, Right "x", Left 4, Left 3] result
    ,
    testCase "3" $ do
      let
        list = [Left 4, Right 'z', Right 'a', Left 3, Right 'b', Left 0, Left 1, Right 'x', Left 4, Left 3]
        transform = left' (A.consume D.list)
      result <- C.produceAndTransformAndConsume (E.list list) transform D.list
      assertEqual "" [Left [4], Right 'z', Right 'a', Left [3], Right 'b', Left [0, 1], Right 'x', Left [4, 3]] result
  ]

transformArrowLaws =
  testGroup "Arrow laws"
  [
    testGroup "Strong"
    [
      testCase "1" $ do
        let
          input = [(1,'a'),(2,'b'),(3,'c'),(4,'d')]
          transform = first transform1
        result <- C.produceAndTransformAndConsume (E.list input) transform D.list
        assertEqual "" [(6,'c'),(4,'d')] result
      ,
      testCase "Lack of elements" $ do
        let
          input = [(1,'a'),(2,'b')]
          transform = first transform1
        result <- C.produceAndTransformAndConsume (E.list input) transform D.list
        assertEqual "" [(3,'b')] result
    ]
    ,
    transformProperty "arr id = id"
      (arr id :: A.Transform Int Int)
      id
    ,
    transformProperty "arr (f >>> g) = arr f >>> arr g"
      (arr (f >>> g))
      (arr f >>> arr g)
    ,
    transformProperty "first (arr f) = arr (first f)"
      (first (arr f) :: A.Transform (Int, Char) (Int, Char))
      (arr (first f))
    ,
    transformProperty "first (f >>> g) = first f >>> first g"
      (first (transform1 >>> transform2) :: A.Transform (Int, Char) (Int, Char))
      (first (transform1) >>> first (transform2))
    ,
    transformProperty "first f >>> arr fst = arr fst >>> f"
      (first transform1 >>> arr fst :: A.Transform (Int, Char) Int)
      (arr fst >>> transform1)
    ,
    transformProperty "first f >>> arr (id *** g) = arr (id *** g) >>> first f"
      (first transform1 >>> arr (id *** g))
      (arr (id *** g) >>> first transform1)
    ,
    transformProperty "first (first f) >>> arr assoc = arr assoc >>> first f"
      (first (first transform1) >>> arr assoc :: A.Transform ((Int, Char), Double) (Int, (Char, Double)))
      (arr assoc >>> first transform1)
    ,
    transformProperty "left (arr f) = arr (left f)"
      (left (arr f) :: A.Transform (Either Int Char) (Either Int Char))
      (arr (left f))
    ,
    transformProperty "left (f >>> g) = left f >>> left g"
      (left (transform1 >>> transform2) :: A.Transform (Either Int Char) (Either Int Char))
      (left (transform1) >>> left (transform2))
    ,
    transformProperty "f >>> arr Left = arr Left >>> left f"
      (transform1 >>> arr Left :: A.Transform Int (Either Int Char))
      (arr Left >>> left transform1)
    ,
    transformProperty "left f >>> arr (id +++ g) = arr (id +++ g) >>> left f"
      (left transform1 >>> arr (id +++ g))
      (arr (id +++ g) >>> left transform1)
    ,
    transformProperty "left (left f) >>> arr assocsum = arr assocsum >>> left f"
      (left (left transform1) >>> arr assocsum :: A.Transform (Either (Either Int Char) Double) (Either Int (Either Char Double)))
      (arr assocsum >>> left transform1)
    ,
    transformProperty "left (left (arr f)) >>> arr assocsum = arr assocsum >>> left (arr f)"
      (left (left (arr f)) >>> arr assocsum :: A.Transform (Either (Either Int Char) Double) (Either Int (Either Char Double)))
      (arr assocsum >>> left (arr f))
  ]
  where
    f = (+24) :: Int -> Int
    g = (*3) :: Int -> Int
    transform1 = A.consume (D.transform (A.take 3) D.sum) :: A.Transform Int Int
    transform2 = A.consume (D.transform (A.take 4) D.sum) :: A.Transform Int Int
    assoc ((a,b),c) = (a,(b,c))
    assocsum (Left (Left x)) = Left x
    assocsum (Left (Right y)) = Right (Left y)
    assocsum (Right z) = Right (Right z)

transformProperty ::
  (Arbitrary input, Show input, Eq output, Show output) =>
  String -> A.Transform input output -> A.Transform input output -> TestTree
transformProperty name leftTransform rightTransform =
  testProperty name property
  where
    property list =
      transform leftTransform === transform rightTransform
      where
        transform transform =
          unsafePerformIO (C.produceAndTransformAndConsume (E.list list) transform D.list)
