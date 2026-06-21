-- | Convenience helpers for `Temporal.PlainTime` that are **not** part of the
-- | TC39 Temporal spec: the no-options variants of `since`/`until`, and the
-- | `midnight` / `noon` constants. (The spec methods live in
-- | `Temporal.PlainTime`.)
module Temporal.PlainTime.Extra
  ( since'
  , until'
  , midnight
  , noon
  ) where

import Prelude

import Temporal.Internal.Types (Duration, PlainTime)
import Temporal.PlainTime (defaultPlainTimeDiffOptions, since, until)

-- | `since' a b` - duration from `b` to `a` with default options.
since' :: PlainTime -> PlainTime -> Duration
since' = since defaultPlainTimeDiffOptions

-- | `until' a b` - duration from `a` to `b` with default options.
until' :: PlainTime -> PlainTime -> Duration
until' = until defaultPlainTimeDiffOptions

-- | Midnight, `00:00:00` - the minimum `PlainTime` (`bottom`).
midnight :: PlainTime
midnight = bottom

-- | Noon, `12:00:00`.
foreign import noon :: PlainTime
