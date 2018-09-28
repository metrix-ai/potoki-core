module Potoki.Core.TransduceConcurrently
where

import Potoki.Core.Prelude
import Potoki.Core.Types
import Potoki.Core.Transduce ()
import qualified Potoki.Core.Push as Push


deriving instance Profunctor TransduceConcurrently
deriving instance Choice TransduceConcurrently
deriving instance Strong TransduceConcurrently
deriving instance Semigroupoid TransduceConcurrently
deriving instance Category TransduceConcurrently
deriving instance Arrow TransduceConcurrently
deriving instance ArrowChoice TransduceConcurrently
