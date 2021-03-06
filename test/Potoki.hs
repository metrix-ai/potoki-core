module Potoki where

import Prelude hiding (first, second)
import Control.Arrow
import Test.QuickCheck.Instances
import Test.QuickCheck.Monadic as M
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Control.Foldl as Fl
import qualified Potoki.Core.IO as C
import qualified Potoki.Core.Consume as D
import qualified Potoki.Core.Transform as A
import qualified Potoki.Core.Produce as E
import qualified Data.Attoparsec.ByteString.Char8 as B
import qualified Data.ByteString as F
import qualified Data.Vector as G
import qualified System.Random as H
import Data.List.Index (indexed)

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
    testCase "mergeOrdering produce testCase" $ do
      let
        consume = D.list
        produceAndConsume list1 list2 = C.produceAndConsume (E.mergeOrdering (\a b -> a <= b) (E.list list1) (E.list list2)) (consume)
      assertEqual "" [1,2,3,4,5,6,7,8,9,10] =<< produceAndConsume [1,3,5,7,9] [2,4,6,8,10]
      assertEqual "" [1,2,3,4,5,6,7,8,9,10] =<< produceAndConsume [2,4,6,8,10] [1,3,5,7,9]
      assertEqual "" [1,2,2,3,3,4,5,6,7,8,8,9,9,10] =<< produceAndConsume [1,2,3,8,9,10] [2,3,4,5,6,7,8,9]
      assertEqual "" [1,2,3,4,5,6,7,8,9,10] =<< produceAndConsume [1,2,3] [4,5,6,7,8,9,10]
      assertEqual "" [1,2,3,4,5,6,7,8,9,10] =<< produceAndConsume [4,5,6] [1,2,3,7,8,9,10]
      assertEqual "" [1,2,3,4,5,6,7,8,9,10] =<< produceAndConsume [1,2,3,7,8,9,10] [4,5,6]
      assertEqual "" [1,2,3,4,5,6,7,8,9,10] =<< produceAndConsume [1,2,3,4,5,6,7,8,9,10] []
      assertEqual "" [] =<< produceAndConsume ([] :: [Int]) ([] :: [Int]) 
    ,
    testProperty "mergeOrdering produce" $ \ (l1 :: [Int], l2 :: [Int]) -> 
    let 
      list1 = sort l1
      list2 = sort l2
      list = sort $ l1 ++ l2
    in list === unsafePerformIO (C.produceAndConsume (E.mergeOrdering (\a b -> a <= b) (E.list list1) (E.list list2)) D.list)
    ,
    transformPotoki
    ,
    parsingPotoki
    ,
    consumePotoki
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
        list = [1..20000] :: [Int]
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
    ,
    testProperty "takeWhile" $ \ (list :: [Int]) ->
    let listPart = takeWhile odd list
    in monadicIO $ do
      let prod = E.list list
      res <- run (C.produceAndTransformAndConsume prod (A.takeWhile odd) D.list)
      M.assert (res == listPart)
    ,
    testProperty "mapFilter" $ \ (list :: [Int]) ->
    let in2MaybeOut input =
          if input `mod` 4 == 0
            then Just $ input `mod` 4
            else Nothing
        filteredList = map fromJust . filter (/= Nothing) . map in2MaybeOut $ list
    in monadicIO $ do
      let prod = E.list list
      res <- run (C.produceAndTransformAndConsume prod (A.mapFilter in2MaybeOut) D.list)
      M.assert (res == filteredList)
    ,
    testProperty "filter" $ \ (list :: [Int]) ->
    let filteredList = filter even list
    in monadicIO $ do
      let prod = E.list list
      res <- run (C.produceAndTransformAndConsume prod (A.filter even) D.list)
      M.assert (res == filteredList)
    ,
    testCase "uniquify" $ do
      let list = [1,1,2,3,4,5,5,5,6,7,8,9,10]
      res <- C.produceAndTransformAndConsume (E.list list) (A.uniquify) D.list
      assertEqual "" [1,2,3,4,5,6,7,8,9,10] res
    ,
    testProperty "uniquify unit test" $ \ (list :: [Int]) ->
    let sortList = sort list
        uniquifyList = nub sortList
    in uniquifyList === unsafePerformIO (C.produceAndTransformAndConsume (E.list sortList) (A.uniquify) D.list)
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

consumePotoki =
  testGroup "Consume" $
  [
    testProperty "count" $ \ (list :: [Int]) ->
    let n = length list
    in monadicIO $ do
      let prod = E.list list
      len <- run (C.produceAndConsume prod D.count)
      M.assert (len == n)
    ,
    testProperty "sum" $ \ (list :: [Int]) ->
    let n = sum list
    in monadicIO $ do
      let prod = E.list list
      len <- run (C.produceAndConsume prod D.sum)
      M.assert (len == n)
    ,
    testProperty "head" $ \ (list :: [Int]) ->
    let el = if null list then Nothing else (Just (head list))
    in monadicIO $ do
      let prod = E.list list
      he <- run (C.produceAndConsume prod D.head)
      M.assert (he == el)
    ,
    testProperty "last" $ \ (list :: [Int]) ->
    let el = if null list then Nothing else (Just (last list))
    in monadicIO $ do
      let prod = E.list list
      he <- run (C.produceAndConsume prod D.last)
      M.assert (he == el)
    ,
    testProperty "list" $ \ (list :: [Int]) ->
    monadicIO $ do
      let prod = E.list list
      res <- run (C.produceAndConsume prod D.list)
      M.assert (res == list)
    ,
    testProperty "reverseList" $ \ (list :: [Int]) ->
    let revList = reverse list
    in monadicIO $ do
      let prod = E.list list
      res <- run (C.produceAndConsume prod D.reverseList)
      M.assert (res == revList)
    ,
    testProperty "vector" $ \ (list :: [Int]) ->
    let vec = G.fromList list
    in monadicIO $ do
      let prod = E.list list
      res <- run (C.produceAndConsume prod D.vector)
      M.assert (res == vec)
    ,
    testProperty "concat" $ \ (list :: [[Int]]) ->
    let con = concat list
    in monadicIO $ do
      let prod = E.list list
      res <- run (C.produceAndConsume prod D.concat)
      M.assert (res == con)
    ,
    testProperty "fold" $ \ (list :: [Int], fun :: (Fun (Int, Int) Int), first :: Int) ->
    let f = applyFun2 fun
        fol = foldl' f first list
    in monadicIO $ do
      let prod = E.list list
      res <- run (C.produceAndConsume prod (D.fold (Fl.Fold f first id)))
      M.assert (res == fol)
    ,
    testProperty "foldInIO" $ \ (list :: [Int]) ->
    let fin = sum list
    in monadicIO $ do
      let prod = E.list list
      res <- run $ do
        sumVar <- newIORef 0
        (C.produceAndConsume prod (D.foldInIO (Fl.FoldM (\_ a -> modifyIORef' sumVar (a+)) (pure ()) (\_ -> readIORef sumVar)) ) )
      M.assert (res == fin)
    ,
    testProperty "folding" $ \ (list :: [Int], fun :: (Fun (Int, Int) Int), first :: Int) ->
    let f = applyFun2 fun
        fol = foldl' f first list
    in monadicIO $ do
      let prod = E.list list
      res <- run (C.produceAndConsume prod (D.folding (Fl.Fold f first id) (D.sum) ))
      a <- run (C.produceAndConsume prod D.sum)
      M.assert (res == (fol, a))
    ,
    testProperty "foldingInIO" $ \ (list :: [Int]) ->
    let fol = sum list
    in monadicIO $ do
      let prod = E.list list
      res <- run $ do
        sumVar <- newIORef 0
        (C.produceAndConsume prod (D.foldingInIO (Fl.FoldM (\_ a -> modifyIORef' sumVar (a+)) (pure ()) (\_ -> readIORef sumVar)) (D.sum) ))
      a <- run (C.produceAndConsume prod D.sum)
      M.assert (res == (fol, a))
    ,
    testProperty "execState" $ \ (list :: [Int]) ->
    let f = modify' . (+)
        resL = sum list
    in monadicIO $ do
      let prod = E.list list
      res <- run (C.produceAndConsume prod (D.execState f 0))
      M.assert (res == resL)
    ,
    testProperty "Choice consume right'" $ \ (list :: [Either Bool Int]) ->
    let n = sum <$> sequence list
    in monadicIO $ do
      let prod = E.list list
      len <- run (C.produceAndConsume prod $ right' D.sum)
      M.assert (len == n)
  ]

