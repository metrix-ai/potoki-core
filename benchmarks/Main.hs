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
import Tests.Reduce as R
import Tests.Transduce as T

main :: IO ()
main = defaultMain
  [
  --CHOICE Didnt work
    bgroup "Transduce Choice --> 1000000 its didnt work"
      [ bench "natural"      $ nfIO (C.testChoice1 1000000)
      , bench "left'(succ)"  $ nfIO (C.testChoice4 1000000)
      , bench "right'(succ)" $ nfIO (C.testChoice5 1000000)
      ]
--STRONG
  , bgroup "Transduce Strong --> 1000000"
      [ bench "natural"       $ nfIO (S.testStrong1 1000000)
      , bench "first'(succ)"  $ nfIO (S.testStrong4 1000000)
      , bench "second'(succ)" $ nfIO (S.testStrong5 1000000)
      ]
  , bgroup "produceAndReduce"
      [ bench "list unit" $ nfIO (R.reduceUnit 1000000)
      , bench "list list" $ nfIO (R.reduceList 1000000)
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
  , bgroup "Reduce"
      [ bgroup "unit by length"
          [ bench "100" $ nfIO (R.reduceUnit 100)
          , bench "10000" $ nfIO (R.reduceUnit 10000)
          , bench "1000000" $ nfIO (R.reduceUnit 1000000)
          ]
      , bgroup "count by length"
          [ bench "100" $ nfIO (R.reduceCount 100)
          , bench "10000" $ nfIO (R.reduceCount 10000)
          , bench "1000000" $ nfIO (R.reduceCount 1000000)
          ]
      , bgroup "list by length"
          [ bench "100" $ nfIO (R.reduceList 100)
          , bench "10000" $ nfIO (R.reduceList 10000)
          , bench "1000000" $ nfIO (R.reduceList 1000000)
          ]
      , bgroup "fold (Foldl.sum) by length"
          [ bench "100" $ nfIO (R.reduceFold 100)
          , bench "10000" $ nfIO (R.reduceFold 10000)
          , bench "1000000" $ nfIO (R.reduceFold 1000000)
          ]
      , bgroup "vector by length"
          [ bench "100" $ nfIO (R.reduceVector 100)
          , bench "10000" $ nfIO (R.reduceVector 10000)
          , bench "1000000" $ nfIO (R.reduceVector 1000000)
          ]
      -- , bench "Reduce apConcurrently --> 10000" $ nfIO (Co.reduceApConcurrently 10000)
      , bench "Reduce Choice right' --> 10000" $ nfIO (R.reduceRight' 10000)
      ]
    , bgroup "Transduce"
        [ bgroup "take by length"
            [ bench "100" $ nfIO (T.transduceTake 100)
            , bench "10000" $ nfIO (T.transduceTake 10000)
            , bench "1000000" $ nfIO (T.transduceTake 1000000)
            ]
        ]
  ]
