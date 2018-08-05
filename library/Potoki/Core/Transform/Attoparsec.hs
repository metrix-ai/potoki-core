module Potoki.Core.Transform.Attoparsec
where

import Potoki.Core.Prelude hiding (take, takeWhile, filter, drop)
import Potoki.Core.Types
import Potoki.Core.Transform.Concurrency
import Potoki.Core.Transform.ByteString
import qualified Potoki.Core.Fetch as A
import qualified Data.Attoparsec.ByteString as K
import qualified Data.Attoparsec.Text as L
import qualified Data.Attoparsec.Types as M
import qualified Acquire.Acquire as N


{-# INLINE mapWithParseResult #-}
mapWithParseResult :: forall input parsed. (Monoid input, Eq input) => (input -> M.IResult input parsed) -> Transform input (Either Text parsed)
mapWithParseResult inputToResult =
  Transform $ \inputFetch -> N.Acquire $ do
    unconsumedRef <- newIORef mempty
    finishedRef <- newIORef False
    return (A.Fetch (fetchParsed inputFetch finishedRef unconsumedRef), return ())
  where
    fetchParsed :: A.Fetch input -> IORef Bool -> IORef input -> IO (Maybe (Either Text parsed))
    fetchParsed (A.Fetch inputFetchIO) finishedRef unconsumedRef =
      do
        finished <- readIORef finishedRef
        if finished
          then return Nothing
          else do
            unconsumed <- readIORef unconsumedRef
            if unconsumed == mempty
              then
                inputFetchIO >>= \case
                  Nothing -> return Nothing
                  Just input -> do
                    if input == mempty
                      then return Nothing
                      else matchResult (inputToResult input)
              else do
                writeIORef unconsumedRef mempty
                matchResult (inputToResult unconsumed)
      where
        matchResult :: M.IResult input parsed -> IO (Maybe (Either Text parsed))
        matchResult =
          \case
            M.Partial inputToResultVal ->
              consumeVal inputToResultVal
            M.Done unconsumed parsed ->
              do
                writeIORef unconsumedRef unconsumed
                return (Just (Right parsed))
            M.Fail unconsumed contexts message ->
              do
                writeIORef unconsumedRef unconsumed
                writeIORef finishedRef True
                return (Just (Left resultMessage))
              where
                resultMessage =
                  if null contexts
                    then fromString message
                    else fromString (showString (intercalate " > " contexts) (showString ": " message))
        consumeVal inputToResultVal' =
          inputFetchIO >>= \case
            Nothing -> do
              writeIORef finishedRef True
              matchResult (inputToResultVal' mempty)
            Just input -> do
              when (input == mempty) (writeIORef finishedRef True)
              matchResult (inputToResultVal' input)

{-|
Lift an Attoparsec ByteString parser.
-}
{-# INLINE parseBytes #-}
parseBytes :: K.Parser parsed -> Transform ByteString (Either Text parsed)
parseBytes parser =
  mapWithParseResult (K.parse parser)

{-|
Lift an Attoparsec Text parser.
-}
{-# INLINE parseText #-}
parseText :: L.Parser parsed -> Transform Text (Either Text parsed)
parseText parser =
  mapWithParseResult (L.parse parser)

{-|
Lift an Attoparsec ByteString parser to a transform,
which parses the lines concurrently.
-}
{-# INLINE parseLineBytesConcurrently #-}
parseLineBytesConcurrently :: Int -> K.Parser a -> Transform ByteString (Either Text a)
parseLineBytesConcurrently concurrency parser =
  extractLines >>> bufferize concurrency >>>
  concurrently concurrency (arr (mapLeft fromString . K.parseOnly parser))
