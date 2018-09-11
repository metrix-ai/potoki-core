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
import Tests.Base
import Tests.Transduce
import qualified Potoki.Core.IO as IO
import qualified Potoki.Core.Reduce as Reduce
import qualified Potoki.Core.Produce as Produce
import qualified Potoki.Core.Transduce as Transduce


main =
  defaultMain $
  testGroup "All tests" $
  [
    base
    ,
    transduce
  ]


