-- | DST-aware boundary / truncation functions for `ZonedDateTime`.
-- |
-- | All functions preserve the timezone and correctly handle DST transitions.
-- | For example, `startOfDay` may return a non-midnight time in zones where
-- | DST transitions occur at midnight.
module Temporal.ZonedDateTime.Boundary
  ( startOfWeek
  , endOfWeek
  , startOfMonth
  , endOfMonth
  , startOfYear
  , endOfYear
  ) where

import Prelude

import Temporal.Internal.Types (PlainTime, ZonedDateTime)
import Temporal.PlainDate.Boundary as PDB
import Temporal.ZonedDateTime as ZDT
import Temporal.PlainDate as PD

-- | The start of the Monday of the ISO week containing this ZonedDateTime.
-- | DST-aware: may not be midnight.
startOfWeek :: ZonedDateTime -> ZonedDateTime
startOfWeek = toStartOfDate PDB.startOfWeek

-- | The end of the Sunday of the ISO week containing this ZonedDateTime
-- | (last nanosecond of that day).
endOfWeek :: ZonedDateTime -> ZonedDateTime
endOfWeek = toEndOfDate PDB.endOfWeek

-- | The start of the first day of the month containing this ZonedDateTime.
startOfMonth :: ZonedDateTime -> ZonedDateTime
startOfMonth = toStartOfDate PDB.startOfMonth

-- | The last nanosecond of the last day of the month containing this
-- | ZonedDateTime.
endOfMonth :: ZonedDateTime -> ZonedDateTime
endOfMonth = toEndOfDate PDB.endOfMonth

-- | The start of January 1 of the year containing this ZonedDateTime.
startOfYear :: ZonedDateTime -> ZonedDateTime
startOfYear = toStartOfDate PDB.startOfYear

-- | The last nanosecond of December 31 of the year containing this
-- | ZonedDateTime.
endOfYear :: ZonedDateTime -> ZonedDateTime
endOfYear = toEndOfDate PDB.endOfYear

-- Internal helpers

-- | Navigate to a PlainDate derived from the ZonedDateTime, then get the
-- | start of that day in the original timezone. DST-aware because
-- | `PD.toZonedDateTime` and `ZDT.startOfDay` respect timezone rules.
toStartOfDate :: (PD.PlainDate -> PD.PlainDate) -> ZonedDateTime -> ZonedDateTime
toStartOfDate f zdt =
  let tz = ZDT.getTimeZoneId zdt
      targetDate = f (ZDT.toPlainDate zdt)
      -- Create a ZonedDateTime at midnight of the target date in the same tz,
      -- then use startOfDay to get the correct DST-aware start
      targetZdt = PD.toZonedDateTime tz (bottom :: PlainTime) targetDate
  in ZDT.startOfDay targetZdt

-- | Navigate to a PlainDate derived from the ZonedDateTime, then set the
-- | time to the last nanosecond of that day.
toEndOfDate :: (PD.PlainDate -> PD.PlainDate) -> ZonedDateTime -> ZonedDateTime
toEndOfDate f zdt =
  let tz = ZDT.getTimeZoneId zdt
      targetDate = f (ZDT.toPlainDate zdt)
      targetZdt = PD.toZonedDateTime tz (bottom :: PlainTime) targetDate
  in ZDT.withPlainTime (top :: PlainTime) (ZDT.startOfDay targetZdt)
