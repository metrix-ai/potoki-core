module Potoki.Core.Transform.Scanner
where

import Potoki.Core.Prelude hiding (take, takeWhile, filter, drop)
import Potoki.Core.Types
import Potoki.Core.Transform.Basic
import Scanner


{-# INLINE scan #-}
scan :: Scanner a -> Transform ByteString (Either Text a)
scan scanner =
  Transform $ \ (Fetch fetchIo) -> liftIO $ do
    unconsumedRef <- newMutVar mempty
    finishedRef <- newMutVar False
    return $ Fetch $ let
      matchResult =
        \ case
          More inputToResultVal ->
            consumeVal inputToResultVal
          Done unconsumed parsed ->
            do
              writeMutVar unconsumedRef unconsumed
              return (Just (Right parsed))
          Fail unconsumed message ->
            do
              writeMutVar unconsumedRef unconsumed
              writeMutVar finishedRef True
              return (Just (Left (fromString message)))
      consumeVal inputToResultVal' =
        fetchIo >>= \ case
          Nothing -> do
            writeMutVar finishedRef True
            matchResult (inputToResultVal' mempty)
          Just input -> do
            when (input == mempty) (writeMutVar finishedRef True)
            matchResult (inputToResultVal' input)
      in do
          finished <- readMutVar finishedRef
          if finished
            then return Nothing
            else do
              unconsumed <- readMutVar unconsumedRef
              if unconsumed == mempty
                then
                  fetchIo >>= \ case
                    Nothing -> return Nothing
                    Just input -> do
                      if input == mempty
                        then return Nothing
                        else matchResult (Scanner.scan scanner input)
                else do
                  writeMutVar unconsumedRef mempty
                  matchResult (Scanner.scan scanner unconsumed)
