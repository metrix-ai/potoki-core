module Main where

import Prelude hiding (first, second)
import Control.Arrow
import Test.QuickCheck.Instances
import Test.QuickCheck.Monadic as M
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
    resourceChecker
    ,
    testCase "sync resource checker" $ do
      resourceVar <- newIORef Initial
      let produce = syncCheckResource resourceVar
      C.produceAndConsume produce D.sum
      fin <- readIORef resourceVar
      assertEqual "" Released fin
    ,
    testCase "async resource checker" $ do
      resourceVar <- newTVarIO Initial
      let produce = asyncCheckResource resourceVar
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

resourceChecker :: TestTree
resourceChecker =
  testGroup "produce1 >>= produce2" $
  [
    testCase "Resource produce1 is released 1 time" $ do
      resourceVar1 <- newIORef Initial
      resourceVar2 <- newIORef Initial
      let prod1 = checkProduce resourceVar1 100
          prod2 = checkProduce resourceVar2
      res <- C.produceAndConsume (prod1 >>= prod2) D.sum
      print res
      fin <- readIORef resourceVar1
      assertEqual "" Released fin
    ,
    testCase "Fetch1 >>= Fetch2" $ do
      stVar1 <- newIORef [1]
      stVar2 <- newIORef [2,3,4]
      let fetch = Fe.list stVar1 >> Fe.list stVar2
      let prod = E.Produce $ pure fetch
      res <- C.produceAndConsume prod D.list
      assertEqual "" [2,3,4] res
    ,
    testProperty "Bind for produce" $ \ (list :: [Int]) ->
    let check = list >>= (enumFromTo 0)
        prod1 = E.list list
        prod2 = \x -> E.list $ enumFromTo 0 x
    in monadicIO $ do
      res <- run $ C.produceAndConsume (prod1 >>= prod2) D.list
      M.assert (check == res)
  ]

data Resource
  = Initial
  | Acquired
  | Released
  | ReleasedNonAcquired
  deriving (Show, Eq)

checkProduce :: IORef Resource -> Int -> E.Produce Int
checkProduce resourceVar k = E.Produce . Ac.Acquire $ do
  writeIORef resourceVar Acquired
  stVar <- newIORef 0
  let fetch = do
        n <- readIORef stVar
        if n >= k then return (Nothing)
        else do
          writeIORef stVar $! n + 1
          print n
          return (Just n)
  return $ (,) (Fe.Fetch fetch) $ do
      res <- readIORef resourceVar
      case res of
        Acquired -> writeIORef resourceVar Released
        _ -> writeIORef resourceVar ReleasedNonAcquired


asyncCheckResource :: TVar Resource -> E.Produce Int
asyncCheckResource resourceVar = E.Produce . Ac.Acquire $ do
  atomically $ writeTVar resourceVar Acquired
  stVar <- newIORef 0
  let fetch = do
        n <- readIORef stVar
        writeIORef stVar $! n + 1
        return (Just n)
  return (Fe.Fetch fetch, atomically $ writeTVar resourceVar Released)

syncCheckResource :: IORef Resource -> E.Produce Int
syncCheckResource resourceVar = E.Produce . Ac.Acquire $ do
  writeIORef resourceVar Acquired
  stVar <- newIORef 0
  let fetch = do
        n <- readIORef stVar
        if n >= 1000 then return (Nothing)
        else do
          writeIORef stVar $! n + 1
          return (Just n)
  return $ (,) (Fe.Fetch fetch) $ do
      res <- readIORef resourceVar
      case res of
        Acquired -> writeIORef resourceVar Released
        _ -> writeIORef resourceVar ReleasedNonAcquired
