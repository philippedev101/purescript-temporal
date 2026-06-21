-- | Convenience diff functions that return a single number.
-- |
-- | These are thin wrappers over the `until` functions, configured to return
-- | a single-unit duration in the requested unit. The sign of the result
-- | indicates direction (negative if the first argument is after the second).
module Temporal.Diff
  ( diffDays
  , diffWeeks
  , diffMonths
  , diffYears
  , diffHours
  , diffMinutes
  , diffSeconds
  , diffMilliseconds
  ) where

import Prelude

import Temporal.Duration as D
import Temporal.Instant as I
import Temporal.Internal.Options (DateTimeUnit(..), DateUnit(..), RoundingMode(..), TimeUnit(..))
import Temporal.Internal.Types (Instant, PlainDate, PlainDateTime)
import Temporal.PlainDate as PD
import Temporal.PlainDateTime as PDT

-- | Days between two dates. Positive when `b` is after `a`.
-- |
-- | ```purescript
-- | diffDays (plainDate 2024 1 1) (plainDate 2024 1 15)  -- 14
-- | ```
diffDays :: PlainDate -> PlainDate -> Int
diffDays a b = truncate $ D.getDays $ PD.until
  { largestUnit: DateU Days, smallestUnit: DateU Days, roundingIncrement: 1, roundingMode: Trunc }
  a b

-- | ISO weeks between two dates. Positive when `b` is after `a`.
diffWeeks :: PlainDate -> PlainDate -> Int
diffWeeks a b = truncate $ D.getWeeks $ PD.until
  { largestUnit: DateU Weeks, smallestUnit: DateU Weeks, roundingIncrement: 1, roundingMode: Trunc }
  a b

-- | Months between two dates. Positive when `b` is after `a`.
diffMonths :: PlainDate -> PlainDate -> Int
diffMonths a b = truncate $ D.getMonths $ PD.until
  { largestUnit: DateU Months, smallestUnit: DateU Months, roundingIncrement: 1, roundingMode: Trunc }
  a b

-- | Years between two dates. Positive when `b` is after `a`.
diffYears :: PlainDate -> PlainDate -> Int
diffYears a b = truncate $ D.getYears $ PD.until
  { largestUnit: DateU Years, smallestUnit: DateU Years, roundingIncrement: 1, roundingMode: Trunc }
  a b

-- | Hours between two datetimes. Positive when `b` is after `a`.
diffHours :: PlainDateTime -> PlainDateTime -> Number
diffHours a b = D.getHours $ PDT.until
  { largestUnit: TimeU Hours, smallestUnit: TimeU Hours, roundingIncrement: 1, roundingMode: Trunc }
  a b

-- | Minutes between two datetimes. Positive when `b` is after `a`.
diffMinutes :: PlainDateTime -> PlainDateTime -> Number
diffMinutes a b = D.getMinutes $ PDT.until
  { largestUnit: TimeU Minutes, smallestUnit: TimeU Minutes, roundingIncrement: 1, roundingMode: Trunc }
  a b

-- | Seconds between two datetimes. Positive when `b` is after `a`.
diffSeconds :: PlainDateTime -> PlainDateTime -> Number
diffSeconds a b = D.getSeconds $ PDT.until
  { largestUnit: TimeU Seconds, smallestUnit: TimeU Seconds, roundingIncrement: 1, roundingMode: Trunc }
  a b

-- | Milliseconds between two instants. Positive when `b` is after `a`.
diffMilliseconds :: Instant -> Instant -> Number
diffMilliseconds a b = D.getMilliseconds $ I.until
  { largestUnit: Milliseconds, smallestUnit: Milliseconds, roundingIncrement: 1, roundingMode: Trunc }
  a b

-- Internal
truncate :: Number -> Int
truncate n = if n >= 0.0 then floor n else ceil n
  where
  floor x = unsafeFloor x
  ceil x = negate (unsafeFloor (negate x))

foreign import unsafeFloor :: Number -> Int
