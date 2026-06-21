-- | Convenience helpers for `Temporal.Duration` that are **not** part of the
-- | TC39 Temporal spec. (The spec methods live in `Temporal.Duration`.)
module Temporal.Duration.Extra
  ( totalUnit
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Temporal.Duration (total)
import Temporal.Internal.Options (DateTimeUnit)
import Temporal.Internal.Types (Duration)

-- | Convenience wrapper for `total` without a `relativeTo` reference.
totalUnit :: DateTimeUnit -> Duration -> Maybe Number
totalUnit unit = total { unit, relativeTo: Nothing }
