-- | Typeclass for computing the duration of an interval.
-- |
-- | Provides `HasDuration` instances for all Temporal types that support
-- | `until'`, and a `duration` function that computes the duration of an
-- | `Interval` using that typeclass.
module Temporal.Interval.Duration
  ( class HasDuration
  , durationBetween
  , duration
  ) where

import Temporal.Interval (Interval, start, end)
import Temporal.Internal.Types (Duration, Instant, PlainDate, PlainDateTime, PlainTime, ZonedDateTime)
import Temporal.Instant as I
import Temporal.PlainDate as PD
import Temporal.PlainDateTime as PDT
import Temporal.PlainTime as PT
import Temporal.ZonedDateTime as ZDT
import Temporal.Instant.Extra as IX
import Temporal.PlainDate.Extra as PDX
import Temporal.PlainDateTime.Extra as PDTX
import Temporal.PlainTime.Extra as PTX
import Temporal.ZonedDateTime.Extra as ZDTX

-- | Types that support computing a `Duration` between two values.
class HasDuration a where
  durationBetween :: a -> a -> Duration

instance HasDuration PlainDate where
  durationBetween = PDX.until'

instance HasDuration PlainTime where
  durationBetween = PTX.until'

instance HasDuration PlainDateTime where
  durationBetween = PDTX.until'

instance HasDuration ZonedDateTime where
  durationBetween = ZDTX.until'

instance HasDuration Instant where
  durationBetween = IX.until'

-- | Compute the duration of an interval using the `until'` function of the
-- | contained type.
-- |
-- | ```purescript
-- | duration (unsafeInterval date1 date2)  -- uses PlainDate.until'
-- | duration (unsafeInterval zdt1 zdt2)    -- uses ZonedDateTime.until' (DST-aware)
-- | ```
duration :: forall a. HasDuration a => Interval a -> Duration
duration i = durationBetween (start i) (end i)
