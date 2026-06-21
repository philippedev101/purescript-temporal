-- | Boundary / truncation functions for `PlainDate`.
-- |
-- | All functions are pure PureScript — no FFI required.
module Temporal.PlainDate.Boundary
  ( startOfWeek
  , endOfWeek
  , startOfMonth
  , endOfMonth
  , startOfYear
  , endOfYear
  , startOfDay
  , endOfDay
  ) where

import Prelude

import Temporal.Internal.Types (PlainDate, PlainDateTime, PlainTime)
import Temporal.Internal.Unsafe (unsafePlainDate, unsafeAddDays, unsafeSubtractDays)
import Temporal.PlainDate as PD

-- | The Monday of the ISO week containing this date.
-- |
-- | ```purescript
-- | startOfWeek (plainDate 2024 3 20)  -- Wednesday -> Monday 2024-03-18
-- | ```
startOfWeek :: PlainDate -> PlainDate
startOfWeek d =
  let dow = PD.getDayOfWeek d -- 1=Monday, 7=Sunday
  in unsafeSubtractDays (dow - 1) d

-- | The Sunday of the ISO week containing this date.
-- |
-- | ```purescript
-- | endOfWeek (plainDate 2024 3 20)  -- Wednesday -> Sunday 2024-03-24
-- | ```
endOfWeek :: PlainDate -> PlainDate
endOfWeek d =
  let dow = PD.getDayOfWeek d
  in unsafeAddDays (7 - dow) d

-- | The first day of the month containing this date.
startOfMonth :: PlainDate -> PlainDate
startOfMonth d = unsafePlainDate (PD.getYear d) (PD.getMonth d) 1

-- | The last day of the month containing this date.
endOfMonth :: PlainDate -> PlainDate
endOfMonth d = unsafePlainDate (PD.getYear d) (PD.getMonth d) (PD.getDaysInMonth d)

-- | January 1 of the year containing this date.
startOfYear :: PlainDate -> PlainDate
startOfYear d = unsafePlainDate (PD.getYear d) 1 1

-- | December 31 of the year containing this date.
endOfYear :: PlainDate -> PlainDate
endOfYear d = unsafePlainDate (PD.getYear d) 12 31

-- | The given date at midnight (00:00:00.000000000).
startOfDay :: PlainDate -> PlainDateTime
startOfDay = PD.toPlainDateTime (bottom :: PlainTime)

-- | The given date at the last representable nanosecond (23:59:59.999999999).
endOfDay :: PlainDate -> PlainDateTime
endOfDay = PD.toPlainDateTime (top :: PlainTime)
