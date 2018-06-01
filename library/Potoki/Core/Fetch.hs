module Potoki.Core.Fetch
(
  Fetch(..),
  duplicate,
  maybeRef,
  list,
  firstCachingSecond,
  bothFetchingFirst,
  rightHandlingLeft,
  rightCachingLeft,
  eitherFetchingRight,
  signaling,
  ioFetch,
  handleBytes,
  handleBytesWithChunkSize,
  handleText,
  mapFilter,
  filter,
  just,
  takeWhile,
  infiniteMVar,
  finiteMVar,
  vector,
  handlingElements,
)
where

import Potoki.Core.Prelude hiding (filter, takeWhile)
import Potoki.Core.Types
import qualified Data.Vector as C
import qualified Data.ByteString as D
import qualified Data.Text.IO as A


deriving instance Functor Fetch

instance Applicative Fetch where
  pure x =
    Fetch (pure (Just x))
  (<*>) (Fetch leftIO) (Fetch rightIO) =
    Fetch ((<*>) <$> leftIO <*> rightIO)

instance Monad Fetch where
  return =
    pure
  (>>=) (Fetch leftIO) rightFetch =
    Fetch $ do
      leftFetching <- leftIO
      case leftFetching of
        Nothing -> return Nothing
        Just leftElement -> case rightFetch leftElement of
          Fetch rightIO -> rightIO

instance Alternative Fetch where
  empty =
    Fetch (pure Nothing)
  (<|>) (Fetch leftIO) (Fetch rightIO) =
    Fetch ((<|>) <$> leftIO <*> rightIO)

instance MonadPlus Fetch where
  mzero =
    empty
  mplus =
    (<|>)

instance MonadIO Fetch where
  liftIO io =
    Fetch (fmap Just io)

{-# INLINABLE duplicate #-}
duplicate :: Fetch element -> IO (Fetch element, Fetch element)
duplicate (Fetch fetchIO) =
  do
    leftBuffer <- newTQueueIO
    rightBuffer <- newTQueueIO
    notFetchingVar <- newTVarIO True
    notEndVar <- newTVarIO True
    let
      newFetch ownBuffer mirrorBuffer =
        Fetch $ do
          fetch <- fetchIO
          join $
            atomically
             (mplus
                (do
                   element <- readTQueue ownBuffer
                   return $ return (Just element)
                )
                (do
                   notEnd <- readTVar notEndVar
                   if notEnd
                     then do
                       notFetching <- readTVar notFetchingVar
                       guard notFetching
                       writeTVar notFetchingVar False
                       return $ case fetch of
                         Nothing      -> do
                           atomically
                             (do
                                writeTVar notEndVar False
                                writeTVar notFetchingVar True
                             )
                           return Nothing
                         Just !element -> do
                           atomically
                             (do
                                writeTQueue mirrorBuffer element
                                writeTVar notFetchingVar True
                             )
                           return $ Just element
                     else return $ return Nothing
                )
             )
      leftFetch =
        newFetch leftBuffer rightBuffer
      rightFetch =
        newFetch rightBuffer leftBuffer
     in return (leftFetch, rightFetch)

{-# INLINABLE maybeRef #-}
maybeRef :: IORef (Maybe a) -> Fetch a
maybeRef refElem =
  Fetch $ do
    elemVal <- readIORef refElem
    case elemVal of
      Nothing      -> return Nothing
      Just element -> do
        writeIORef refElem Nothing
        return $ Just element

{-# INLINABLE list #-}
list :: IORef [element] -> Fetch element
list unsentListRef =
  Fetch $ do
    refList <- readIORef unsentListRef
    case refList of
      (!headVal) : (!tailVal) -> do
        writeIORef unsentListRef tailVal
        return $ Just headVal
      _              -> do
        writeIORef unsentListRef []
        return Nothing

{-# INLINABLE firstCachingSecond #-}
firstCachingSecond :: IORef b -> Fetch (a, b) -> Fetch a
firstCachingSecond cacheRef (Fetch bothFetchIO) =
  Fetch $ do
    bothFetch <- bothFetchIO
    case bothFetch of
      Nothing                -> return Nothing
      Just (!firstVal, !secondVal) -> do
        writeIORef cacheRef secondVal
        return $ Just firstVal

{-# INLINABLE bothFetchingFirst #-}
bothFetchingFirst :: IORef b -> Fetch a -> Fetch (a, b)
bothFetchingFirst cacheRef (Fetch firstFetchIO) =
  Fetch $ do
    firstFetch <- firstFetchIO
    case firstFetch of
      Nothing            -> return Nothing
      Just !firstFetched -> do
        secondCached <- readIORef cacheRef
        return $ Just (firstFetched, secondCached)

{-# INLINABLE rightHandlingLeft #-}
rightHandlingLeft :: (left -> IO ()) -> Fetch (Either left right) -> Fetch right
rightHandlingLeft left2IO (Fetch eitherFetchIO) =
  Fetch $ do
    eitherFetch <- eitherFetchIO
    case eitherFetch of
      Nothing    -> return Nothing
      Just input -> case input of
        Right rightInput -> return $ Just rightInput
        Left  leftInput  -> left2IO leftInput $> Nothing

{-# INLINABLE rightCachingLeft #-}
rightCachingLeft :: IORef (Maybe left) -> Fetch (Either left right) -> Fetch right
rightCachingLeft cacheRef =
  rightHandlingLeft (writeIORef cacheRef . Just)

{-# INLINABLE eitherFetchingRight #-}
eitherFetchingRight :: IORef (Maybe left) -> Fetch right -> Fetch (Either left right)
eitherFetchingRight cacheRef (Fetch rightFetchIO) =
  Fetch $ do
    rightFetch <- rightFetchIO
    case rightFetch of
      Nothing -> return Nothing
      Just r  -> atomicModifyIORef' cacheRef $ \ case
        Nothing -> (Nothing, Just $ Right r)
        Just l  -> (Nothing, Just $ Left  l)

{-# INLINABLE signaling #-}
signaling :: IO () -> IO () -> Fetch a -> Fetch a
signaling signalEnd signalElement (Fetch fetchIO) =
  Fetch $ do
    fetch <- fetchIO
    case fetch of
      Nothing      -> signalEnd $> Nothing
      Just element -> signalElement >> return (Just element)

{-# INLINABLE ioFetch #-}
ioFetch :: IO (Fetch a) -> Fetch a
ioFetch fetchIO =
  Fetch $ do
    Fetch fetch <- fetchIO
    fetch

{-# INLINABLE handleBytes #-}
handleBytes :: Handle -> Fetch (Either IOException ByteString)
handleBytes =
  handleBytesWithChunkSize ioChunkSize

{-# INLINABLE handleBytesWithChunkSize #-}
handleBytesWithChunkSize :: Int -> Handle -> Fetch (Either IOException ByteString)
handleBytesWithChunkSize chunkSize handleVal =
  Fetch $ do
    chunk <- try (D.hGetSome handleVal chunkSize)
    case chunk of
      Right "" -> return Nothing
      _ -> return (Just chunk)

{-# INLINABLE handleText #-}
handleText :: Handle -> Fetch (Either IOException Text)
handleText handleVal =
  Fetch $ do
    chunk <- try (A.hGetChunk handleVal)
    case chunk of
      Right "" -> return Nothing
      _ -> return (Just chunk)

{-# INLINABLE mapFilter #-}
mapFilter :: (input -> Maybe output) -> Fetch input -> Fetch output
mapFilter mapping (Fetch fetchIO) =
  Fetch $ 
  fix $ \ doLoop -> do 
    fetch <- fetchIO
    case mapping <$> fetch of
      Just (Just output) -> return (Just output)
      Just Nothing -> doLoop
      Nothing -> return Nothing

{-# INLINABLE filter #-}
filter :: (input -> Bool) -> Fetch input -> Fetch input
filter predicate (Fetch fetchIO) =
  Fetch $ 
  fix $ \ doLoop -> do 
    fetch <- fetchIO
    case predicate <$> fetch of
      Just True -> return fetch
      Just False -> doLoop
      Nothing -> return Nothing


{-# INLINABLE just #-}
just :: Fetch (Maybe element) -> Fetch element
just (Fetch fetchIO) =
  Fetch $ 
  fix $ \ doLoop -> do 
    fetch <- fetchIO
    case fetch of
      Just (Just element) -> return (Just element)
      Just (Nothing) -> doLoop
      Nothing -> return Nothing

{-# INLINABLE takeWhile #-}
takeWhile :: (element -> Bool) -> Fetch element -> Fetch element
takeWhile predicate (Fetch fetchIO) =
  Fetch $ do
    fetch <- fetchIO
    case predicate <$> fetch of
      Just True -> return fetch
      _ -> return Nothing

{-# INLINABLE infiniteMVar #-}
infiniteMVar :: MVar element -> Fetch element
infiniteMVar var =
  Fetch $ fmap Just $ takeMVar var

{-# INLINABLE finiteMVar #-}
finiteMVar :: MVar (Maybe element) -> Fetch element
finiteMVar var =
  Fetch $ takeMVar var

{-# INLINABLE vector #-}
vector :: IORef Int -> Vector element -> Fetch element
vector indexRef vectorVal =
  Fetch $ do
    indexVal <- readIORef indexRef
    if indexVal < C.length vectorVal
      then do
        writeIORef indexRef (succ indexVal)
        return (Just (C.unsafeIndex vectorVal indexVal))
      else return Nothing

{-# INLINABLE handlingElements #-}
handlingElements :: (element -> IO ()) -> Fetch element -> Fetch element
handlingElements xRay (Fetch fetchIO) =
  Fetch $ do
    mbElement <- fetchIO
    case mbElement of
      Just element -> xRay element $> mbElement
      Nothing -> return Nothing
    
