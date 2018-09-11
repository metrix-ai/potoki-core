module Main
(
  main,
)
where

import Prelude
import Gauge.Main
import Tests.Choice as C
import Tests.Strong as S
import Tests.Produce as P
import Tests.Consume as Co
import Tests.Transform as T

main :: IO ()
main = defaultMain
  [
  --CHOICE
    bgroup "Transform Choice --> 1000000"
      [ bench "natural"      $ nfIO (C.testChoice1 1000000)
      , bench "left'(succ)"  $ nfIO (C.testChoice4 1000000)
      , bench "right'(succ)" $ nfIO (C.testChoice5 1000000)
      ]
--STRONG
  , bgroup "Transform Strong --> 1000000"
      [ bench "natural"       $ nfIO (S.testStrong1 1000000)
      , bench "first'(succ)"  $ nfIO (S.testStrong4 1000000)
      , bench "second'(succ)" $ nfIO (S.testStrong5 1000000)
      ]
  , bgroup "produceAndConsume"
      [ bench "list unit" $ nfIO (Co.consumeUnit 1000000)
      , bench "list list" $ nfIO (Co.consumeList 1000000)
      ]
  , bgroup "Produce"
      [ bgroup "list by length"
          [ bench "100" $ nfIO (P.produceList 100)
          , bench "10000" $ nfIO (P.produceList 10000)
          , bench "1000000" $ nfIO (P.produceList 1000000)
          ]
      , bgroup "vector by length"
          [ bench "100" $ nfIO (P.produceVector 100)
          , bench "10000" $ nfIO (P.produceVector 10000)
          , bench "1000000" $ nfIO (P.produceVector 1000000)
          ]
      , bench "Produce monad --> 1000" $ nfIO (P.monad 1000)
      , bench "produceAlternative --> 10000" $ nfIO (P.produceAlternative 10000)
      ]
  , bgroup "Consume"
      [ bgroup "unit by length"
          [ bench "100" $ nfIO (Co.consumeUnit 100)
          , bench "10000" $ nfIO (Co.consumeUnit 10000)
          , bench "1000000" $ nfIO (Co.consumeUnit 1000000)
          ]
      , bgroup "count by length"
          [ bench "100" $ nfIO (Co.consumeCount 100)
          , bench "10000" $ nfIO (Co.consumeCount 10000)
          , bench "1000000" $ nfIO (Co.consumeCount 1000000)
          ]
      , bgroup "list by length"
          [ bench "100" $ nfIO (Co.consumeList 100)
          , bench "10000" $ nfIO (Co.consumeList 10000)
          , bench "1000000" $ nfIO (Co.consumeList 1000000)
          ]
      , bgroup "fold (Foldl.sum) by length"
          [ bench "100" $ nfIO (Co.consumeFold 100)
          , bench "10000" $ nfIO (Co.consumeFold 10000)
          , bench "1000000" $ nfIO (Co.consumeFold 1000000)
          ]
      , bgroup "vector by length"
          [ bench "100" $ nfIO (Co.consumeVector 100)
          , bench "10000" $ nfIO (Co.consumeVector 10000)
          ]
      -- , bench "Reduce apConcurrently --> 10000" $ nfIO (Co.reduceApConcurrently 10000)
      , bench "Reduce Choice right' --> 10000" $ nfIO (Co.consumeRight' 10000)
      ]
  , bgroup "Transform"
      [ bgroup "take by length"
          [ bench "100" $ nfIO (T.transformTake 100)
          , bench "10000" $ nfIO (T.transformTake 10000)
          , bench "1000000" $ nfIO (T.transformTake 1000000)
          ]
      , bgroup "filter even by length"
          [ bench "100" $ nfIO (T.transformFilter 100)
          , bench "10000" $ nfIO (T.transformFilter 10000)
          ]
      , bgroup "vector by length"
          [ bench "100" $ nfIO (T.transformVector 100)
          , bench "10000" $ nfIO (T.transformVector 10000)
          ]
      ]
  ]
