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
import qualified Potoki.Core.Fetch as Fe
import qualified Data.Attoparsec.ByteString.Char8 as B
import qualified Data.ByteString as F
import qualified Data.Vector as G
import qualified System.Random as H
import qualified Acquire.Acquire as Ac
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
    ,
    testCase "sync resource checker" $ do
      resourceVar <- newIORef Initial
      let produce = syncChechResource resourceVar
      C.produceAndConsume produce D.sum
      fin <- readIORef resourceVar
      assertEqual "" Released fin
    ,
    testCase "async resource checker" $ do
      resourceVar <- newTVarIO Initial
      let produce = asyncChechResource resourceVar
      potokiThreadId <- forkIO $ do
          C.produceAndConsume produce D.sum
          return ()
      atomically $ do
        resource <- readTVar resourceVar
        guard $ resource == Acquired
      killThread potokiThreadId
      atomically $ do
        resource <- readTVar resourceVar
        guard $ resource == Released
  ]

data Resource
  = Initial
  | Acquired
  | Released
  deriving (Show, Eq)

asyncChechResource :: TVar Resource -> E.Produce Int
asyncChechResource resourceVar = E.Produce . Ac.Acquire $ do
  atomically $ writeTVar resourceVar Acquired
  stVar <- newIORef 0
  let fetch = do
        n <- readIORef stVar
        writeIORef stVar $! n + 1
        return (Just n)
  return (Fe.Fetch fetch, atomically $ writeTVar resourceVar Released)

syncChechResource :: IORef Resource -> E.Produce Int
syncChechResource resourceVar = E.Produce . Ac.Acquire $ do
  writeIORef resourceVar Acquired
  stVar <- newIORef 0
  let fetch = do
        n <- readIORef stVar
        if n >= 1000 then return (Nothing)
        else do
          writeIORef stVar $! n + 1
          return (Just n)
  return (Fe.Fetch fetch, writeIORef resourceVar Released)
