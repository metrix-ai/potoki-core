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
  , bgroup "produceAndReduce"
      [ bench "list unit" $ nfIO (R.reduceUnit 1000000)
      , bench "list list" $ nfIO (R.reduceList 1000000)
      ]
  , bgroup "Produce"
      [
        bench "Produce monad --> 1000" $ nfIO (P.monad 1000)
      , bench "Produce List --> 10000" $ nfIO (P.produceList 10000)
      , bench "Produce Vector --> 10000" $ nfIO (P.produceVector 10000)
      , bench "produceAlternative --> 10000" $ nfIO (P.produceAlternative 10000)
      ]
  , bgroup "Reduce"
      [ bench "Reduce List --> 10000" $ nfIO (R.reduceList 10000)
      -- , bench "Reduce Vector --> 10000" $ nfIO (Co.reduceVector 10000)
      -- , bench "Reduce apConcurrently --> 10000" $ nfIO (Co.reduceApConcurrently 10000)
      , bench "Reduce Choice right' --> 10000" $ nfIO (R.reduceRight' 10000)
      ]
  ]
