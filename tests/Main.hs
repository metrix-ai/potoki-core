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
import Potoki
import Transform


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
