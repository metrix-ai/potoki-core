module Tests.Transform
(
  transformSucc,
  transformNot,
)
where

import Prelude
import Potoki.Core.Transform


transformSucc :: Transform Int Int
transformSucc =
  arr succ

transformNot :: Transform Bool Bool
transformNot =
  arr not
