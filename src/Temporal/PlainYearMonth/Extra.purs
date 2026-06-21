-- | Convenience helpers for `Temporal.PlainYearMonth` that are **not** part of the TC39
-- | Temporal spec. Currently the no-options variants of `since`/`until`
-- | (the spec methods themselves live in `Temporal.PlainYearMonth`).
module Temporal.PlainYearMonth.Extra
  ( since'
  , until'
  ) where

import Temporal.Internal.Types (Duration, PlainYearMonth)
import Temporal.PlainYearMonth (defaultPlainYearMonthDiffOptions, since, until)

-- | `since' a b` - duration from `b` to `a` with default options.
since' :: PlainYearMonth -> PlainYearMonth -> Duration
since' = since defaultPlainYearMonthDiffOptions

-- | `until' a b` - duration from `a` to `b` with default options.
until' :: PlainYearMonth -> PlainYearMonth -> Duration
until' = until defaultPlainYearMonthDiffOptions
