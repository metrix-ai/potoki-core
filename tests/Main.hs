module Main where

import Prelude hiding (first, second)
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
import qualified System.Random as H


main =
  defaultMain $
  testGroup "All tests" $
  [
    testProperty "list to list" $ \ (list :: [Int]) ->
    list === unsafePerformIO (C.produceAndConsume (E.list list) D.list)
    ,
    testProperty "consecutive consumers" $ \ (list :: [Int], amount) ->
    list === unsafePerformIO (C.produceAndConsume (E.list list) ((++) <$> D.transform (A.take amount) D.list <*> D.list))
    ,
    potoki
    ,
    transform
  ]

transform =
  testGroup "Transform" $
  [
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

potoki :: TestTree
potoki =
  testGroup "All tests for potoki's end-users functions" $
  [
    testCase "vector to list" $ do
      result <- C.produceAndConsume (E.vector (G.fromList [1,2,3])) (D.list)
      assertEqual "" [1,2,3] result
    ,
    testCase "just" $ do
      result <- C.produceAndConsume (E.list [Just 1, Nothing, Just 2]) (D.transform A.just D.list)
      assertEqual "" [1,2] result
    ,
    testCase "transform,consume,take" $ do
      let
        transform = A.consume (D.transform (A.take 3) D.list)
        consume = D.transform transform D.list
        produceAndConsume list = C.produceAndConsume (E.list list) (consume)
      assertEqual "" [[1,2,3], [4,5,6], [7,8]] =<< produceAndConsume [1,2,3,4,5,6,7,8]
      assertEqual "" [[1,2,3], [4,5,6], [7,8,9]] =<< produceAndConsume [1,2,3,4,5,6,7,8,9]
      assertEqual "" [] =<< produceAndConsume ([] :: [Int])
    ,
    testCase "File reading" $ do
      let produce =
            E.transform (arr (either (const Nothing) Just) >>> A.just) $
            E.fileBytes "samples/1"
      result <- C.produceAndConsume produce (fmap F.length D.concat)
      assertEqual "" 17400 result
    ,
    transformPotoki
    ,
    parsingPotoki
  ]


transformPotoki :: TestTree
transformPotoki =
  testGroup "Transform" $
  [
    testCase "Order" $ do
      let
        list = [Left 1, Left 2, Right 'z', Left 2, Right 'a', Left 1, Right 'b', Left 0, Right 'x', Left 4, Left 3]
        transform = left (A.consume (D.transform (A.take 2) D.sum))
      result <- C.produceAndConsume (E.list list) (D.transform transform D.list)
      assertEqual "" [Left 3, Right 'z', Left 2, Right 'a', Left 1, Right 'b', Left 0, Right 'x', Left 7] result
    ,
    testCase "Interrupted order" $ do
      let
        list = [Left 1, Left 2, Right 'a']
        transform = left (A.consume (D.transform (A.take 3) D.sum))
      result <- C.produceAndConsume (E.list list) (D.transform transform D.list)
      assertEqual "" [Left 3, Right 'a'] result
    ,
    testCase "Distinct" $ do
      let
        list = [1,2,3,2,3,2,1,4,1] :: [Int]
      result <- C.produceAndConsume (E.list list) (D.transform A.distinct D.list)
      assertEqual "" [1,2,3,4] result
    ,
    testCase "Distinct By" $ do
      let
        list = [(1, ""),(2, ""),(3, ""),(2, ""),(3, ""),(2, ""),(1, ""),(4, ""),(1, "")] :: [(Int, String)]
      result <- C.produceAndConsume (E.list list) (D.transform (A.distinctBy fst) D.list)
      assertEqual "" [(1, ""),(2, ""),(3, ""),(4, "")] result
    ,
    testCase "Concurrently" $ do
      let
        list = [1..20000]
        produce = E.list list
        transform =
          A.concurrently 12 $
          arr (\ x -> H.randomRIO (0, 100) >>= threadDelay >> return x) >>>
          A.executeIO
        consume = D.transform transform D.list
      result <- C.produceAndConsume produce consume
      assertBool "Is dispersed" (list /= result)
      assertEqual "Contains no duplicates" 0 (length result - length (nub result))
      assertEqual "Equals the original once sorted" list (sort result)
    ,
    testProperty "Line" $ \ chunks ->
    let
      expected =
        mconcat chunks
      actual =
        unsafePerformIO (C.produceAndConsume produce consume)
        where
          produce =
            E.list chunks
          consume =
            rmap (mconcat . intersperse "\n") $
            D.transform A.extractLines D.list
      in expected === actual
  ]
  
parsingPotoki :: TestTree
parsingPotoki =
  testGroup "Parsing" $
  [
    testCase "Sample 1" $ do
      let parser = B.double <* B.char ','
          transform = arr (either (const Nothing) Just) >>> A.just >>> A.parseBytes parser
          produce = E.transform transform (E.fileBytes "samples/1")
      result <- C.produceAndConsume produce D.count
      assertEqual "" 4350 result
    ,
    testCase "Sample 1 greedy" $ do
      let parser = B.sepBy B.double (B.char ',')
          transform = arr (either (const Nothing) Just) >>> A.just >>> A.parseBytes parser
          produce = E.transform transform (E.fileBytes "samples/1")
      result <- C.produceAndConsume produce D.list
      assertEqual "" [Right 4350] (fmap (fmap length) result)
    ,
    testCase "Split chunk" $
    let
      produce = E.list ["1", "2", "3"]
      parser = B.anyChar
      transform = A.parseBytes parser >>> arr (either (const Nothing) Just) >>> A.just
      consume = D.transform transform D.count
      in do
        assertEqual "" 3 =<< C.produceAndConsume produce consume
  ]
