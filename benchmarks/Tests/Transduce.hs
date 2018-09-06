module Tests.Transduce
(
  transduceSucc,
  transduceNot,
)
where

import Prelude
import Potoki.Core.Transduce


transduceSucc :: Transduce Int Int
transduceSucc =
  arr succ

transduceNot :: Transduce Bool Bool
transduceNot =
  arr not
