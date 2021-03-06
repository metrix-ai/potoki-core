module Potoki.Core.Transform.Attoparsec
where

import Potoki.Core.Prelude hiding (take, takeWhile, filter, drop)
import Potoki.Core.Types
import Potoki.Core.Transform.Basic
import Potoki.Core.Transform.Concurrency
import Potoki.Core.Transform.ByteString
import qualified Potoki.Core.Fetch as A
import qualified Data.Attoparsec.ByteString as K
import qualified Data.Attoparsec.Text as L
import qualified Data.Attoparsec.Types as M
import qualified Acquire.Acquire as N
import qualified Data.ByteString as ByteString


{-# INLINE mapWithParseResult #-}
mapWithParseResult :: forall input parsed. (Monoid input, Eq input) => (input -> M.IResult input parsed) -> Transform input (Either Text parsed)
mapWithParseResult inputToResult =
  Transform $ \inputFetch -> N.Acquire $ do
    unconsumedRef <- newMutVar mempty
    finishedRef <- newMutVar False
    return (A.Fetch (fetchParsed inputFetch finishedRef unconsumedRef), return ())
  where
    fetchParsed :: A.Fetch input -> MutVar RealWorld Bool -> MutVar RealWorld input -> IO (Maybe (Either Text parsed))
    fetchParsed (A.Fetch inputFetchIO) finishedRef unconsumedRef =
      do
        finished <- readMutVar finishedRef
        if finished
          then return Nothing
          else do
            unconsumed <- readMutVar unconsumedRef
            if unconsumed == mempty
              then
                inputFetchIO >>= \case
                  Nothing -> return Nothing
                  Just input -> do
                    if input == mempty
                      then return Nothing
                      else matchResult (inputToResult input)
              else do
                writeMutVar unconsumedRef mempty
                matchResult (inputToResult unconsumed)
      where
        matchResult :: M.IResult input parsed -> IO (Maybe (Either Text parsed))
        matchResult =
          \case
            M.Partial inputToResultVal ->
              consumeVal inputToResultVal
            M.Done unconsumed parsed ->
              do
                writeMutVar unconsumedRef unconsumed
                return (Just (Right parsed))
            M.Fail unconsumed contexts message ->
              do
                writeMutVar unconsumedRef unconsumed
                writeMutVar finishedRef True
                return (Just (Left resultMessage))
              where
                resultMessage =
                  if null contexts
                    then fromString message
                    else fromString (showString (intercalate " > " contexts) (showString ": " message))
        consumeVal inputToResultVal' =
          inputFetchIO >>= \case
            Nothing -> do
              writeMutVar finishedRef True
              matchResult (inputToResultVal' mempty)
            Just input -> do
              when (input == mempty) (writeMutVar finishedRef True)
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
parseLineBytesConcurrently :: NFData a => Int -> K.Parser a -> Transform ByteString (Either Text a)
parseLineBytesConcurrently concurrency parser =
  extractLines >>> bufferize concurrency >>>
  unsafeConcurrently concurrency (arr (mapLeft fromString . K.parseOnly parser))

{-|
Lift an Attoparsec ByteString parser to a transform,
which parses the lines concurrently.
-}
{-# INLINE parseNonEmptyLineBytesConcurrently #-}
parseNonEmptyLineBytesConcurrently :: NFData a => Int -> K.Parser a -> Transform ByteString (Either Text a)
parseNonEmptyLineBytesConcurrently concurrency parser =
  extractLinesWithoutTrail >>> filter (not . ByteString.null) >>> bufferize (concurrency * 2) >>>
  unsafeConcurrently concurrency (arr (mapLeft fromString . K.parseOnly parser))
