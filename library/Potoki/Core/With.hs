module Potoki.Core.With
(
  With(..),
  with,
)
where

import Potoki.Core.Prelude
import Potoki.Core.Types


instance Functor With where
  fmap f (With io) =
    With $ do
      (resource, release) <- io
      return (f resource, release)

instance Applicative With where
  pure resource =
    With (pure (resource, pure ()))
  With io1 <*> With io2 =
    With $ do
      (f, release1) <- io1
      (x, release2) <- onException io2 release1
      return (f x, release2 >> release1)

instance Monad With where
  return = pure
  (>>=) (With io1) k2 =
    With $ do
      (resource1, release1) <- io1
      (resource2, release2) <- case k2 resource1 of With io2 -> onException io2 release1
      return (resource2, release2 >> release1)

instance MonadIO With where
  liftIO io =
    With (fmap (, return ()) io)

with :: With resource -> (resource -> IO a) -> IO a
with (With io) handle =
  do
    (resource, release) <- io
    finally (handle resource) release

