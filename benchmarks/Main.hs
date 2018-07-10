module Main
(
  main,
)
where

import Prelude
import System.IO
import Gauge.Main
import Weigh
import Tests.Choice as C
import Tests.Strong as S
import Tests.Transform

main :: IO ()
main = defaultMain
  [
  --CHOICE
    bgroup "Transform Choice --> 1"
      [ bench "natural"      $ nfIO (C.testChoice1 1)
      , bench "left'(id)"    $ nfIO (C.testChoice2 1)
      , bench "right'(id)"   $ nfIO (C.testChoice3 1)
      , bench "left'(succ)"  $ nfIO (C.testChoice4 1)
      , bench "right'(succ)" $ nfIO (C.testChoice5 1)
      ]
  , bgroup "Transform Choice --> 10"
      [ bench "natural"      $ nfIO (C.testChoice1 10)
      , bench "left'(id)"    $ nfIO (C.testChoice2 10)
      , bench "right'(id)"   $ nfIO (C.testChoice3 10)
      , bench "left'(succ)"  $ nfIO (C.testChoice4 10)
      , bench "right'(succ)" $ nfIO (C.testChoice5 10)
    ]
  , bgroup "Transform Choice --> 1000"
      [ bench "natural"      $ nfIO (C.testChoice1 1000)
      , bench "left'(id)"    $ nfIO (C.testChoice2 1000)
      , bench "right'(id)"   $ nfIO (C.testChoice3 1000)
      , bench "left'(succ)"  $ nfIO (C.testChoice4 1000)
      , bench "right'(succ)" $ nfIO (C.testChoice5 1000)
    ]
  , bgroup "Transform Choice --> 1000000"
      [ bench "natural"      $ nfIO (C.testChoice1 1000000)
      , bench "left'(id)"    $ nfIO (C.testChoice2 1000000)
      , bench "right'(id)"   $ nfIO (C.testChoice3 1000000)
      , bench "left'(succ)"  $ nfIO (C.testChoice4 1000000)
      , bench "right'(succ)" $ nfIO (C.testChoice5 1000000)
      ]

--STRONG
  , bgroup "Transform Strong --> 1"
      [ bench "natural"       $ nfIO (S.testStrong1 1)
      , bench "first'(id)"    $ nfIO (S.testStrong2 1)
      , bench "second'(id)"   $ nfIO (S.testStrong3 1)
      , bench "first'(succ)"  $ nfIO (S.testStrong4 1)
      , bench "second'(succ)" $ nfIO (S.testStrong5 1)
      ]
  , bgroup "Transform Strong --> 10"
      [ bench "natural"       $ nfIO (S.testStrong1 10)
      , bench "first'(id)"    $ nfIO (S.testStrong2 10)
      , bench "second'(id)"   $ nfIO (S.testStrong3 10)
      , bench "first'(succ)"  $ nfIO (S.testStrong4 10)
      , bench "second'(succ)" $ nfIO (S.testStrong5 10)
    ]
  , bgroup "Transform Strong --> 1000"
      [ bench "natural"       $ nfIO (S.testStrong1 1000)
      , bench "first'(id)"    $ nfIO (S.testStrong2 1000)
      , bench "second'(id)"   $ nfIO (S.testStrong3 1000)
      , bench "first'(succ)"  $ nfIO (S.testStrong4 1000)
      , bench "second'(succ)" $ nfIO (S.testStrong5 1000)
    ]
  , bgroup "Transform Strong --> 1000000"
      [ bench "natural"       $ nfIO (S.testStrong1 1000000)
      , bench "first'(id)"    $ nfIO (S.testStrong2 1000000)
      , bench "second'(id)"   $ nfIO (S.testStrong3 1000000)
      , bench "first'(succ)"  $ nfIO (S.testStrong4 1000000)
      , bench "second'(succ)" $ nfIO (S.testStrong5 1000000)
    ]
  ]
