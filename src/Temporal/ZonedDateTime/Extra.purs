-- | Convenience helpers for `Temporal.ZonedDateTime` that are **not** part of the TC39
-- | Temporal spec. Currently the no-options variants of `since`/`until`
-- | (the spec methods themselves live in `Temporal.ZonedDateTime`).
module Temporal.ZonedDateTime.Extra
  ( since'
  , until'
  ) where

import Temporal.Internal.Types (Duration, ZonedDateTime)
import Temporal.ZonedDateTime (defaultZonedDateTimeDiffOptions, since, until)

-- | `since' a b` - duration from `b` to `a` with default options.
since' :: ZonedDateTime -> ZonedDateTime -> Duration
since' = since defaultZonedDateTimeDiffOptions

-- | `until' a b` - duration from `a` to `b` with default options.
until' :: ZonedDateTime -> ZonedDateTime -> Duration
until' = until defaultZonedDateTimeDiffOptions
