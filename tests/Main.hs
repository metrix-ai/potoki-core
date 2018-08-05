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
    testCase "extractLines" $ do
      assertEqual "" ["ab", "", "cd"] =<<
        C.produceAndConsume (E.transform A.extractLinesWithoutTrail (E.list ["a", "b\n", "\nc", "d\n"])) D.list
      assertEqual "" ["ab", "", "cd", ""] =<<
        C.produceAndConsume (E.transform A.extractLines (E.list ["a", "b\n", "\nc", "d\n"])) D.list
    ,
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
    ,
    testProperty "Produce.transform resource checker" $ \ (list :: [Int]) ->
    let prod = E.list list
    in monadicIO $ do
      check <- run $ do
        resourceVar1 <-  newIORef Initial
        res <- C.produceAndConsume (E.transform (checkTransform resourceVar1) prod) D.sum
        readIORef resourceVar1
      M.assert $ check == Released
    ,
    testProperty "Consume.transform resource checker" $ \ (list :: [Int]) ->
    let prod = E.list list
    in monadicIO $ do
      check <- run $ do
        resourceVar1 <-  newIORef Initial
        res <- C.produceAndConsume prod (D.transform (checkTransform resourceVar1) D.sum)
        readIORef resourceVar1
      M.assert $ check == Released
    ,
    testCase "Transform.produce resource checker #1" $ do
      resourceVar <- newIORef Initial
      let prod = checkProduce resourceVar (/= Released) 100
      res1 <- C.produceAndConsume (E.transform (A.produce intToProduce) prod) D.sum
      res2 <- C.produceAndConsume prod (D.transform (A.produce intToProduce) D.sum)
      fin <- readIORef resourceVar
      assertEqual "" Released fin
      assertEqual "" res1 res2
    ,
    testCase "Transform.produce resource checker #2" $ do
      resourceVar <- newIORef Initial
      let prod = checkProduce resourceVar (/= Released) 100
      res1 <- C.produceAndConsume (E.transform (A.take 5 >>> A.produce intToProduce) prod) D.sum
      res2 <- C.produceAndConsume prod (D.transform (A.take 5 >>> A.produce intToProduce) D.sum)
      fin <- readIORef resourceVar
      assertEqual "" Released fin
      assertEqual "" res1 res2
  ]

resourceChecker :: TestTree
resourceChecker =
  testGroup "produce1 >>= produce2" $
  [
    testCase "Check produce binding" $ do
      resourceVar1 <- newIORef Initial
      resourceVar2 <- newIORef Initial
      let prod1 = checkProduce resourceVar1 (const True) 100
          prod2 = \x -> checkProduce resourceVar2 (/= Released) x
      res <- C.produceAndConsume (prod1 >>= prod2) D.sum
      fin <- readIORef resourceVar1
      assertEqual "" Released fin
    ,
    testProperty "Bind for produce" $ \ (list :: [Int]) ->
    let check = list >>= (enumFromTo 0)
        prod1 = E.list list
        prod2 = \x -> E.list $ enumFromTo 0 x
    in monadicIO $ do
      res <- run $ C.produceAndConsume (prod1 >>= prod2) D.list
      M.assert (check == res)
    ,
    testProperty "liftIO for Produce. Consume0" $ \ (_ :: Int) ->
    monadicIO $ do
      check <- run $ do
        checkVar <- newIORef False
        let prod = liftIO $ writeIORef checkVar True
        C.produceAndConsume prod someThing
        readIORef checkVar
      M.assert $ check == False
    ,
    testProperty "liftIO for Produce. ConsumeN" $ \ (n :: Int) ->
    monadicIO $ do
      let prod = liftIO (return n)
      len <- run (C.produceAndConsume prod D.count)
      M.assert (len == 1)
    ]

intToProduce :: Int -> E.Produce Int
intToProduce a = E.Produce . Ac.Acquire $ do
  stVar <- newIORef 0
  return $ flip (,) (return ()) $ Fe.Fetch $ do
      n <- readIORef stVar
      if n >= a + 1 then (return Nothing)
      else do
        writeIORef stVar $! n + 1
        return (Just n)

someThing :: D.Consume input Int
someThing = D.Consume $ \ (Fe.Fetch _) -> return 0

single :: Foldable f => f a -> Maybe a
single = join . (foldr' f Nothing)
  where
    f x Nothing = Just $ Just x
    f _ _       = Just Nothing

data Resource
  = Initial
  | Acquired
  | Released
  | AcquiredImproperly
  | ReleasedImproperly
  deriving (Show, Eq)

checkTransform :: IORef Resource -> A.Transform Int Int
checkTransform resourceVar = A.Transform $ \ fetchIO -> Ac.Acquire $ do
  writeIORef resourceVar Acquired
  return $ (,) (plusFetch fetchIO) $ do
      res <- readIORef resourceVar
      case res of
        Acquired -> writeIORef resourceVar Released
        _ -> writeIORef resourceVar ReleasedImproperly

plusFetch :: Fe.Fetch Int -> Fe.Fetch Int
plusFetch (Fe.Fetch fetchIO) = Fe.Fetch $ do
  fetch <- fetchIO
  return $ fmap succ fetch

checkProduce :: IORef Resource -> (Resource -> Bool) -> Int -> E.Produce Int
checkProduce resourceVar f k = E.Produce . Ac.Acquire $ do
  res <- readIORef resourceVar
  if f res && res /= Initial
    then do
      return $ (,) (Fe.Fetch $ return Nothing) $ writeIORef resourceVar AcquiredImproperly
    else do
      writeIORef resourceVar Acquired
      stVar <- newIORef 0
      let fetch = do
            n <- readIORef stVar
            if n >= k then return (Nothing)
            else do
              writeIORef stVar $! n + 1
              return (Just n)
      return $ (,) (Fe.Fetch fetch) $ do
          res <- readIORef resourceVar
          case res of
            Acquired -> writeIORef resourceVar Released
            _ -> writeIORef resourceVar ReleasedImproperly

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
        _ -> writeIORef resourceVar ReleasedImproperly
