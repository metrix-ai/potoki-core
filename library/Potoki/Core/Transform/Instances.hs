module Potoki.Core.Transform.Instances where

import Potoki.Core.Prelude
import Potoki.Core.Types
import qualified Potoki.Core.Fetch as A

instance Category Transform where
  id =
    Transform (return)
  (.) (Transform leftVal) (Transform rightVal) =
    Transform (leftVal <=< rightVal)

instance Profunctor Transform where
  dimap inputMapping outputMapping (Transform acquire) =
    Transform $ \ oldFetch -> do
      newFetch <- acquire (fmap inputMapping oldFetch)
      return $ fmap outputMapping newFetch

instance Choice Transform where
  right' :: Transform a b -> Transform (Either c a) (Either c b)
  right' (Transform rightTransformAcquire) =
    Transform $ \ inFetch -> do
      fetchedLeftMaybeRef <- liftIO $ newIORef Nothing
      Fetch rightFetchIO <- rightTransformAcquire (A.rightHandlingLeft (writeIORef fetchedLeftMaybeRef . Just) inFetch)
      return $ Fetch $ do
          rightFetch <- rightFetchIO
          case rightFetch of
            Nothing    -> do
              fetchedLeftMaybe <- readIORef fetchedLeftMaybeRef
              case fetchedLeftMaybe of
                Nothing          -> return Nothing
                Just fetchedLeft -> do
                  writeIORef fetchedLeftMaybeRef Nothing
                  return $ Just (Left fetchedLeft)
            Just element -> return $ Just (Right element)

instance Strong Transform where
  first' (Transform firstTransformAcquire) =
    Transform $ \ inFetch -> do
      cacheRef <- liftIO $ newIORef undefined
      outFetch <- firstTransformAcquire (A.firstCachingSecond cacheRef inFetch)
      return $ A.bothFetchingFirst cacheRef outFetch

instance Arrow Transform where
  arr fn =
    Transform (return . fmap fn)
  first =
    first'

instance ArrowChoice Transform where
  left =
    left'

