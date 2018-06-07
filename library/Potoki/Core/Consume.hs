module Potoki.Core.Consume
(
  Consume(..),
  apConcurrently,
  list,
  sum,
  transform,
  count,
  head,
  last,
  reverseList,
  vector,
  concat,
  fold,
  foldInIO,
  folding,
  foldingInIO,
  execState,
  writeBytesToFile,
  appendBytesToFile,
  deleteFiles,
  printBytes,
  printText,
  printString,
  parseBytes,
  parseText,
  concurrently,
)
where

import Potoki.Core.Prelude hiding (sum, head, fold, concat, last)
import Potoki.Core.Types
import qualified Potoki.Core.Fetch as A
import qualified Acquire.IO as B
import qualified Potoki.Core.Transform as J
import qualified Potoki.Core.IO.Fetch as L
import qualified Data.ByteString as C
import qualified Data.Attoparsec.ByteString as E
import qualified Data.Attoparsec.Text as F
import qualified Data.Attoparsec.Types as I
import qualified Data.Text.IO as K
import qualified Control.Foldl as D
import qualified System.Directory as G
import qualified Potoki.Core.Transform.Concurrency as B
import qualified Control.Monad.Trans.State.Strict as O


instance Profunctor Consume where
  {-# INLINE dimap #-}
  dimap inputMapping outputMapping (Consume consume) =
    Consume (\ fetch -> fmap outputMapping (consume $ fmap inputMapping fetch))

instance Choice Consume where
  right' :: Consume a b -> Consume (Either c a) (Either c b)
  right' (Consume rightConsumeIO) =
     Consume $ \ (Fetch eitherFetchIO) -> do
       fetchedLeftMaybeRef <- newIORef Nothing
       consumedRight <-
         rightConsumeIO $ Fetch $ do
           eitherFetch <- eitherFetchIO
           case eitherFetch of
             Nothing      -> return Nothing
             Just element -> case element of
               Right fetchedRight -> return $ Just fetchedRight
               Left  fetchedLeft  -> do
                 writeIORef fetchedLeftMaybeRef $ Just fetchedLeft
                 return Nothing
       fetchedLeftMaybe <- readIORef fetchedLeftMaybeRef
       case fetchedLeftMaybe of
         Nothing          -> return $ Right consumedRight
         Just fetchedLeft -> return $ Left fetchedLeft 

instance Functor (Consume input) where
  fmap = rmap

instance Applicative (Consume a) where
  pure x = Consume $ \ _ -> pure x

  Consume leftConsumeIO <*> Consume rightConsumeIO =
    Consume $ \ fetch -> leftConsumeIO fetch <*> rightConsumeIO fetch

instance Monad (Consume a) where
  Consume leftConsumeIO >>= toRightConsumeIO = Consume $ \ fetch -> do
    Consume rightConsumeIO <- toRightConsumeIO <$> leftConsumeIO fetch
    rightConsumeIO fetch

instance MonadIO (Consume a) where
  liftIO a = Consume $ \ _ -> a

apConcurrently :: Consume a (b -> c) -> Consume a b -> Consume a c
apConcurrently (Consume leftConsumeIO) (Consume rightConsumeIO) =
  Consume $ \ fetch -> do
    (leftFetch, rightFetch) <- A.duplicate fetch
    rightOutputVar <- newEmptyMVar
    _ <- forkIO $ do
      !rightOutput <- rightConsumeIO rightFetch
      putMVar rightOutputVar rightOutput
    !leftOutput <- leftConsumeIO leftFetch
    rightOutput <- takeMVar rightOutputVar
    return (leftOutput rightOutput)

{-# INLINABLE list #-}
list :: Consume input [input]
list =
  Consume $ \ (Fetch fetchIO) ->
    let 
      build !acc = do
        fetch <- fetchIO
        case fetch of
          Nothing       -> pure $ acc []
          Just !element -> build $ acc . (:) element
     in build id

{-# INLINE sum #-}
sum :: Num num => Consume num num
sum =
  Consume $ \ (Fetch fetchIO) ->
    let
      build !acc = do
        fetch <- fetchIO
        case fetch of
          Nothing       -> pure acc
          Just !element -> build $ element + acc
     in build 0

{-# INLINABLE transform #-}
transform :: Transform input1 input2 -> Consume input2 output -> Consume input1 output
transform (Transform transformAcquire) (Consume consumeIO) =
  Consume $ \ fetch -> B.acquire (transformAcquire fetch) consumeIO

{-# INLINABLE head #-}
head :: Consume input (Maybe input)
head =
  Consume (\ (A.Fetch fetchIO) -> fetchIO)

{-# INLINABLE last #-}
last :: Consume input (Maybe input)
last = 
  fold D.last 

{-|
A faster alternative to "list",
which however constructs the list in the reverse order.
-}
{-# INLINABLE reverseList #-}
reverseList :: Consume input [input]
reverseList =
  Consume $ \ (A.Fetch fetchIO) -> build fetchIO []
  where
    build fetchIO !acc =
      fetchIO >>= \case
        Nothing -> pure acc
        Just element -> build fetchIO (element : acc)

{-# INLINABLE vector #-}
vector :: Consume input (Vector input)
vector =
  foldInIO D.vectorM

{-# INLINABLE count #-}
count :: Consume input Int
count =
  Consume $ \ (A.Fetch fetchIO) -> build fetchIO 0
  where
    build fetchIO !acc =
      fetchIO >>= \case
        Nothing -> pure acc
        Just _ -> build fetchIO (succ acc)

{-# INLINABLE concat #-}
concat :: Monoid monoid => Consume monoid monoid
concat =
  Consume $ \ (A.Fetch fetchIO) -> build fetchIO mempty
  where
    build fetchIO !acc =
      fetchIO >>= \case
        Nothing -> pure acc
        Just element -> build fetchIO (acc <> element)

{-# INLINABLE processInIO #-}
processInIO :: IO () -> (element -> IO ()) -> Consume element ()
processInIO stop process =
  Consume (\ fetch -> L.fetchAndHandleAll fetch stop process)

{-# INLINABLE printBytes #-}
printBytes :: Consume ByteString ()
printBytes =
  processInIO (putChar '\n') C.putStr

{-# INLINABLE printText #-}
printText :: Consume Text ()
printText =
  processInIO (putChar '\n') K.putStr

{-# INLINABLE printString #-}
printString :: Consume String ()
printString =
  processInIO (putChar '\n') putStr

{-|
Overwrite a file.

* Exception-free
* Automatic resource management
-}
{-# INLINABLE writeBytesToFile #-}
writeBytesToFile :: FilePath -> Consume ByteString (Either IOException ())
writeBytesToFile path =
  Consume $ \ fetch ->
  try $ withFile path WriteMode $ \ handleVal ->
  do
    L.fetchAndHandleAll fetch (return ()) (C.hPut handleVal)

{-|
Append to a file.

* Exception-free
* Automatic resource management
-}
{-# INLINABLE appendBytesToFile #-}
appendBytesToFile :: FilePath -> Consume ByteString (Either IOException ())
appendBytesToFile path =
  Consume $ \ fetch ->
  try $ withFile path AppendMode $ \ handleVal ->
  do
    L.fetchAndHandleAll fetch (return ()) (C.hPut handleVal)

{-# INLINABLE deleteFiles #-}
deleteFiles :: Consume FilePath (Either IOException ())
deleteFiles =
  Consume $ \ fetch ->
  try $ L.fetchAndHandleAll fetch (return ()) G.removeFile

{-# INLINABLE fold #-}
fold :: D.Fold input output -> Consume input output
fold (D.Fold step initVal finish) =
  Consume $ \ (A.Fetch fetch) -> build fetch initVal
  where
    build fetch !acc =
      fetch >>= \case
        Nothing -> pure $ finish acc
        Just input -> build fetch (step acc input)

{-# INLINABLE foldInIO #-}
foldInIO :: D.FoldM IO input output -> Consume input output
foldInIO (D.FoldM step initVal finish) =
  Consume $ \ (A.Fetch fetch) -> build fetch =<< initVal
  where
    build fetch !acc =
      fetch >>= \case
        Nothing -> finish acc
        Just input -> step acc input >>= build fetch

{-# INLINABLE folding #-}
folding :: D.Fold a b -> Consume a c -> Consume a (b, c)
folding (D.Fold step initVal extract) (Consume consumeIO) =
  Consume $ \ fetch -> do
    foldStateRef <- newIORef initVal
    consumptionResult <-
      consumeIO (A.handlingElements (\ element -> do
        !newState <- flip step element <$> readIORef foldStateRef
        writeIORef foldStateRef newState) fetch)
    foldResult <- extract <$> readIORef foldStateRef
    return (foldResult, consumptionResult)

{-# INLINABLE foldingInIO #-}
foldingInIO :: D.FoldM IO a b -> Consume a c -> Consume a (b, c)
foldingInIO (D.FoldM step initVal extract) (Consume consumeIO) =
  Consume $ \ fetch -> do
    foldStateRef <- newIORef =<< initVal
    consumptionResult <-
      consumeIO (A.handlingElements (\ element -> do
        !newState <- flip step element =<< readIORef foldStateRef
        writeIORef foldStateRef newState) fetch)
    foldResult <- extract =<< readIORef foldStateRef
    return (foldResult, consumptionResult)

{-# INLINE execState #-}
execState :: (a -> O.State s b) -> s -> Consume a s
execState stateFn initialState = 
  fold $ D.Fold (\currentState input -> snd $ O.runState (stateFn input) currentState) initialState id

{-# INLINABLE runParseResult #-}
runParseResult :: (Monoid input, Eq input) => (input -> I.IResult input output) -> Consume input (Either Text output)
runParseResult inputToResult =
  Consume $ \ (A.Fetch fetchInput) ->
  let
    just !input =
      case inputToResult input of
        I.Partial newInputToResult -> consume newInputToResult
        I.Done _ parsed -> return (Right parsed)
        I.Fail _ contexts message -> return (Left resultMessage)
          where
            resultMessage =
              if null contexts
                then fromString message
                else fromString (showString (intercalate " > " contexts) (showString ": " message))
    consume _ =
      fetchInput >>= \case
        Nothing -> just mempty
        Just !input -> just input
    in consume inputToResult

{-# INLINABLE parseBytes #-}
parseBytes :: E.Parser output -> Consume ByteString (Either Text output)
parseBytes =
  runParseResult . E.parse

{-# INLINABLE parseText #-}
parseText :: F.Parser output -> Consume Text (Either Text output)
parseText =
  runParseResult . F.parse

{-|
Execute a Consume concurrently and consume its results.
-}
{-# INLINABLE concurrently #-}
concurrently :: Int -> Consume a b -> Consume b c -> Consume a c
concurrently amount consume1 consume2 =
  transform (B.concurrently amount (J.consume consume1)) consume2
  
