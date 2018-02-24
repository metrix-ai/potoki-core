module Potoki.Core.Transform.Types
where

import Potoki.Core.Prelude
import qualified Potoki.Core.Fetch as A


newtype Transform input output =
  Transform (A.Fetch input -> IO (A.Fetch output))
