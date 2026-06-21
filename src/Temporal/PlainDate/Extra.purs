-- | Convenience helpers for `Temporal.PlainDate` that are **not** part of the TC39
-- | Temporal spec. Currently the no-options variants of `since`/`until`
-- | (the spec methods themselves live in `Temporal.PlainDate`).
module Temporal.PlainDate.Extra
  ( since'
  , until'
  ) where

import Temporal.Internal.Types (Duration, PlainDate)
import Temporal.PlainDate (defaultPlainDateDiffOptions, since, until)

-- | `since' a b` - duration from `b` to `a` with default options.
since' :: PlainDate -> PlainDate -> Duration
since' = since defaultPlainDateDiffOptions

-- | `until' a b` - duration from `a` to `b` with default options.
until' :: PlainDate -> PlainDate -> Duration
until' = until defaultPlainDateDiffOptions
