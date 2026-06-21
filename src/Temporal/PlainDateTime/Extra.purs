-- | Convenience helpers for `Temporal.PlainDateTime` that are **not** part of the TC39
-- | Temporal spec. Currently the no-options variants of `since`/`until`
-- | (the spec methods themselves live in `Temporal.PlainDateTime`).
module Temporal.PlainDateTime.Extra
  ( since'
  , until'
  ) where

import Temporal.Internal.Types (Duration, PlainDateTime)
import Temporal.PlainDateTime (defaultPlainDateTimeDiffOptions, since, until)

-- | `since' a b` - duration from `b` to `a` with default options.
since' :: PlainDateTime -> PlainDateTime -> Duration
since' = since defaultPlainDateTimeDiffOptions

-- | `until' a b` - duration from `a` to `b` with default options.
until' :: PlainDateTime -> PlainDateTime -> Duration
until' = until defaultPlainDateTimeDiffOptions
