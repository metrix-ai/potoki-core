module Potoki.Core.TextBuilder
where

import Potoki.Core.Prelude
import Text.Builder


count :: Int -> Builder
count = thousandSeparatedUnsignedDecimal ','

streamProgressMessage :: Int -> Int -> Builder
streamProgressMessage progress total =
  "Processed " <> count progress <> " elements (" <>
  count total <> " in total)"

indexationSummary :: Int -> Int -> Builder
indexationSummary userAmount uriAmount =
  "Got the following amounts of nodes:\n" <>
  "\tUsers: " <> count userAmount <> "\n" <>
  "\tUris: " <> count uriAmount
