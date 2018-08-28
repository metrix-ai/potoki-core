module Tests.Transform
(
  transformSucc,
  transformNot,
)
where

import Prelude
import Potoki.Core.Transduce


transformSucc :: Transduce Int Int
transformSucc =
  arr succ

transformNot :: Transduce Bool Bool
transformNot =
  arr not
