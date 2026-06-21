-- | Construction, arithmetic, and inspection of `PlainDateTime` values.
-- |
-- | A `PlainDateTime` represents a calendar date and wall-clock time without a
-- | time zone (e.g. 2024-03-15T10:30:00). It combines the semantics of
-- | `PlainDate` and `PlainTime`.
-- |
-- | Use `PlainDateTime` when you need date+time but the time zone is tracked
-- | separately or is irrelevant. For unambiguous moments in time, use `Instant`
-- | or `ZonedDateTime`.
module Temporal.PlainDateTime
  ( module Temporal.Internal.Types
  , PlainDateTimeFields
  , defaultPlainDateTimeFields
  , plainDateTime
  , fromString
  , getYear
  , getMonth
  , getDay
  , getHour
  , getMinute
  , getSecond
  , getMillisecond
  , getMicrosecond
  , getNanosecond
  , getDayOfWeek
  , getDayOfYear
  , getWeekOfYear
  , getYearOfWeek
  , getDaysInMonth
  , getDaysInWeek
  , getDaysInYear
  , getMonthsInYear
  , getInLeapYear
  , with
  , withPlainTime
  , add
  , subtract
  , PlainDateTimeDiffOptions
  , defaultPlainDateTimeDiffOptions
  , since
  , until
  , PlainDateTimeRoundOptions
  , defaultPlainDateTimeRoundOptions
  , round
  , toPlainDate
  , toPlainTime
  , toPlainYearMonth
  , toPlainMonthDay
  , toZonedDateTime
  , toString
  ) where

import Prelude hiding (add)

import Data.Maybe (Maybe(..))
import Temporal.Internal.Options (DateTimeUnit(..), DateUnit(..), Disambiguation, RoundingMode(..), TimeUnit(..), dateTimeUnitToString, disambiguationToString, roundingModeToString)
import Temporal.Internal.Types (Duration, PlainDate, PlainDateTime, PlainMonthDay, PlainTime, PlainYearMonth, ZonedDateTime)

-- | All fields of a `PlainDateTime`. Date fields default to year 0, month 1,
-- | day 1. Time fields default to 0.
type PlainDateTimeFields =
  { year :: Int
  , month :: Int
  , day :: Int
  , hour :: Int
  , minute :: Int
  , second :: Int
  , millisecond :: Int
  , microsecond :: Int
  , nanosecond :: Int
  }

-- | Default fields: 0000-01-01T00:00:00.000000000. Use record update syntax:
-- |
-- | ```purescript
-- | plainDateTime (defaultPlainDateTimeFields { year = 2024, month = 3, day = 15, hour = 10 })
-- | ```
defaultPlainDateTimeFields :: PlainDateTimeFields
defaultPlainDateTimeFields =
  { year: 0
  , month: 1
  , day: 1
  , hour: 0
  , minute: 0
  , second: 0
  , millisecond: 0
  , microsecond: 0
  , nanosecond: 0
  }

-- | Construct a `PlainDateTime` from fields. Returns `Nothing` for invalid values.
plainDateTime :: PlainDateTimeFields -> Maybe PlainDateTime
plainDateTime fields = plainDateTimeImpl Just Nothing fields

-- | Parse an ISO 8601 date-time string (e.g. `"2024-03-15T10:30:00"`).
-- | Returns `Nothing` for invalid strings. Time zone information, if present,
-- | is ignored.
fromString :: String -> Maybe PlainDateTime
fromString s = fromStringImpl Just Nothing s

-- Properties

-- | Calendar year.
foreign import getYear :: PlainDateTime -> Int

-- | Month of the year (1–12).
foreign import getMonth :: PlainDateTime -> Int

-- | Day of the month (1–31).
foreign import getDay :: PlainDateTime -> Int

-- | Hour of the day (0–23).
foreign import getHour :: PlainDateTime -> Int

-- | Minute of the hour (0–59).
foreign import getMinute :: PlainDateTime -> Int

-- | Second of the minute (0–59).
foreign import getSecond :: PlainDateTime -> Int

-- | Millisecond (0–999).
foreign import getMillisecond :: PlainDateTime -> Int

-- | Microsecond (0–999).
foreign import getMicrosecond :: PlainDateTime -> Int

-- | Nanosecond (0–999).
foreign import getNanosecond :: PlainDateTime -> Int

-- | Day of the week (1 = Monday, 7 = Sunday), per ISO 8601.
foreign import getDayOfWeek :: PlainDateTime -> Int

-- | Day of the year (1–366).
foreign import getDayOfYear :: PlainDateTime -> Int

-- | ISO 8601 week number (1–53).
foreign import getWeekOfYear :: PlainDateTime -> Int

-- | The year that corresponds to `getWeekOfYear`.
foreign import getYearOfWeek :: PlainDateTime -> Int

-- | Number of days in this date-time's month (28–31).
foreign import getDaysInMonth :: PlainDateTime -> Int

-- | Always 7.
foreign import getDaysInWeek :: PlainDateTime -> Int

-- | Number of days in this date-time's year (365 or 366).
foreign import getDaysInYear :: PlainDateTime -> Int

-- | Always 12 for the ISO calendar.
foreign import getMonthsInYear :: PlainDateTime -> Int

-- | Whether this date-time's year is a leap year.
foreign import getInLeapYear :: PlainDateTime -> Boolean

-- | Create a modified copy with the given fields. Returns `Nothing` if the
-- | resulting date-time would be invalid.
with :: PlainDateTimeFields -> PlainDateTime -> Maybe PlainDateTime
with fields pdt = withImpl Just Nothing fields pdt

-- | Replace the time component, keeping the date.
foreign import withPlainTime :: PlainTime -> PlainDateTime -> PlainDateTime

-- | Add a duration. Returns `Nothing` on overflow.
add :: Duration -> PlainDateTime -> Maybe PlainDateTime
add dur pdt = addImpl Just Nothing dur pdt

-- | Subtract a duration. Returns `Nothing` on overflow.
subtract :: Duration -> PlainDateTime -> Maybe PlainDateTime
subtract dur pdt = subtractImpl Just Nothing dur pdt

-- Diff

-- | Options for `since` and `until`. Default: largest unit is `Years`,
-- | smallest unit is `Nanoseconds`.
type PlainDateTimeDiffOptions =
  { largestUnit :: DateTimeUnit
  , smallestUnit :: DateTimeUnit
  , roundingIncrement :: Int
  , roundingMode :: RoundingMode
  }

defaultPlainDateTimeDiffOptions :: PlainDateTimeDiffOptions
defaultPlainDateTimeDiffOptions =
  { largestUnit: DateU Years
  , smallestUnit: TimeU Nanoseconds
  , roundingIncrement: 1
  , roundingMode: HalfExpand
  }

-- | `since opts a b` returns the duration from `b` to `a`
-- | (i.e., `a.since(b)` in JS). The result is positive when `a` is after `b`.
since :: PlainDateTimeDiffOptions -> PlainDateTime -> PlainDateTime -> Duration
since opts other self = sinceImpl
  { largestUnit: dateTimeUnitToString opts.largestUnit
  , smallestUnit: dateTimeUnitToString opts.smallestUnit
  , roundingIncrement: opts.roundingIncrement
  , roundingMode: roundingModeToString opts.roundingMode
  }
  other self

-- | `until opts a b` returns the duration from `a` to `b`
-- | (i.e., `a.until(b)` in JS). The result is positive when `b` is after `a`.
until :: PlainDateTimeDiffOptions -> PlainDateTime -> PlainDateTime -> Duration
until opts other self = untilImpl
  { largestUnit: dateTimeUnitToString opts.largestUnit
  , smallestUnit: dateTimeUnitToString opts.smallestUnit
  , roundingIncrement: opts.roundingIncrement
  , roundingMode: roundingModeToString opts.roundingMode
  }
  other self


-- Round

-- | Options for `round`.
type PlainDateTimeRoundOptions =
  { smallestUnit :: DateTimeUnit
  , roundingIncrement :: Int
  , roundingMode :: RoundingMode
  }

defaultPlainDateTimeRoundOptions :: PlainDateTimeRoundOptions
defaultPlainDateTimeRoundOptions =
  { smallestUnit: TimeU Nanoseconds
  , roundingIncrement: 1
  , roundingMode: HalfExpand
  }

-- | Round the date-time to the given precision.
round :: PlainDateTimeRoundOptions -> PlainDateTime -> PlainDateTime
round opts pdt = roundImpl
  { smallestUnit: dateTimeUnitToString opts.smallestUnit
  , roundingIncrement: opts.roundingIncrement
  , roundingMode: roundingModeToString opts.roundingMode
  }
  pdt

-- Conversions

-- | Extract the date component.
foreign import toPlainDate :: PlainDateTime -> PlainDate

-- | Extract the time component.
foreign import toPlainTime :: PlainDateTime -> PlainTime

-- | Extract the year and month.
foreign import toPlainYearMonth :: PlainDateTime -> PlainYearMonth

-- | Extract the month and day.
foreign import toPlainMonthDay :: PlainDateTime -> PlainMonthDay

-- | Convert to a `ZonedDateTime` in the given IANA time zone.
-- | The `Disambiguation` option controls how ambiguous or non-existent
-- | wall-clock times (during DST transitions) are resolved:
-- |
-- | - `Compatible` (recommended default): acts like `Later` for gaps
-- |   (spring forward) and `Earlier` for ambiguities (fall back)
-- | - `Earlier`: pick the earlier of two possible instants
-- | - `Later`: pick the later of two possible instants
-- | - `RejectDisambiguation`: return the result but may be unpredictable
toZonedDateTime :: String -> Disambiguation -> PlainDateTime -> ZonedDateTime
toZonedDateTime tz disambiguation pdt = toZonedDateTimeImpl tz (disambiguationToString disambiguation) pdt

-- | Serialize to an ISO 8601 string (e.g. `"2024-03-15T10:30:00"`).
foreign import toString :: PlainDateTime -> String

-- FFI imports
foreign import plainDateTimeImpl :: (PlainDateTime -> Maybe PlainDateTime) -> Maybe PlainDateTime -> PlainDateTimeFields -> Maybe PlainDateTime
foreign import fromStringImpl :: (PlainDateTime -> Maybe PlainDateTime) -> Maybe PlainDateTime -> String -> Maybe PlainDateTime
foreign import withImpl :: (PlainDateTime -> Maybe PlainDateTime) -> Maybe PlainDateTime -> PlainDateTimeFields -> PlainDateTime -> Maybe PlainDateTime
foreign import addImpl :: (PlainDateTime -> Maybe PlainDateTime) -> Maybe PlainDateTime -> Duration -> PlainDateTime -> Maybe PlainDateTime
foreign import subtractImpl :: (PlainDateTime -> Maybe PlainDateTime) -> Maybe PlainDateTime -> Duration -> PlainDateTime -> Maybe PlainDateTime
foreign import sinceImpl :: { largestUnit :: String, smallestUnit :: String, roundingIncrement :: Int, roundingMode :: String } -> PlainDateTime -> PlainDateTime -> Duration
foreign import untilImpl :: { largestUnit :: String, smallestUnit :: String, roundingIncrement :: Int, roundingMode :: String } -> PlainDateTime -> PlainDateTime -> Duration
foreign import roundImpl :: { smallestUnit :: String, roundingIncrement :: Int, roundingMode :: String } -> PlainDateTime -> PlainDateTime
foreign import toZonedDateTimeImpl :: String -> String -> PlainDateTime -> ZonedDateTime
