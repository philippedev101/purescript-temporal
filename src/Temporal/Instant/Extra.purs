-- | Convenience helpers for `Temporal.Instant` that are **not** part of the TC39
-- | Temporal spec. Currently the no-options variants of `since`/`until`
-- | (the spec methods themselves live in `Temporal.Instant`).
module Temporal.Instant.Extra
  ( since'
  , until'
  ) where

import Temporal.Internal.Types (Duration, Instant)
import Temporal.Instant (defaultInstantDiffOptions, since, until)

-- | `since' a b` - duration from `b` to `a` with default options.
since' :: Instant -> Instant -> Duration
since' = since defaultInstantDiffOptions

-- | `until' a b` - duration from `a` to `b` with default options.
until' :: Instant -> Instant -> Duration
until' = until defaultInstantDiffOptions
