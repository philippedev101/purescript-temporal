-- | Construction, arithmetic, and inspection of `ZonedDateTime` values.
-- |
-- | A `ZonedDateTime` represents a date and time in a specific IANA time zone,
-- | with full awareness of UTC offset and DST transitions. It is the richest
-- | Temporal type.
-- |
-- | Key things to know:
-- |
-- | - **DST disambiguation**: when constructing from wall-clock time, the
-- |   default `"compatible"` mode picks the later time for gaps (spring forward)
-- |   and the earlier time for ambiguities (fall back).
-- | - **Arithmetic splits date and time**: date components (years, months, days)
-- |   are added as calendar arithmetic (wall-clock time preserved), while time
-- |   components (hours, minutes, ...) are added as real elapsed time. Date
-- |   components are processed first.
-- | - **`startOfDay` may not be midnight**: in time zones where DST transitions
-- |   occur at midnight (e.g. historically in Brazil), the start of day can be
-- |   01:00 or another time.
-- | - **Eq compares instant + timezone**: two `ZonedDateTime`s with the same
-- |   instant but different time zones are not equal.
module Temporal.ZonedDateTime
  ( module Temporal.Internal.Types
  , ZonedDateTimeFromOptions
  , defaultZonedDateTimeFromOptions
  , fromEpochNanoseconds
  , fromString
  , fromStringWith
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
  , getTimeZoneId
  , getOffset
  , getOffsetNanoseconds
  , getEpochMilliseconds
  , getEpochNanoseconds
  , getHoursInDay
  , with
  , withPlainTime
  , withTimeZone
  , add
  , subtract
  , ZonedDateTimeDiffOptions
  , defaultZonedDateTimeDiffOptions
  , since
  , until
  , ZonedDateTimeRoundOptions
  , defaultZonedDateTimeRoundOptions
  , round
  , startOfDay
  , getTimeZoneTransition
  , toInstant
  , toPlainDateTime
  , toPlainDate
  , toPlainTime
  , toPlainYearMonth
  , toPlainMonthDay
  , toString
  ) where

import Prelude hiding (add)

import Data.Maybe (Maybe(..))
import JS.BigInt (BigInt)
import Temporal.Internal.Options (DateTimeUnit(..), DateUnit(..), Disambiguation(..), OffsetDisambiguation(..), Overflow(..), RoundingMode(..), TimeUnit(..), TransitionDirection, dateTimeUnitToString, disambiguationToString, offsetDisambiguationToString, overflowToString, roundingModeToString, transitionDirectionToString)
import Temporal.Internal.Types (Duration, Instant, PlainDate, PlainDateTime, PlainMonthDay, PlainTime, PlainYearMonth, ZonedDateTime)

-- | Options controlling how ambiguous inputs are resolved during construction.
-- |
-- | - `disambiguation`: how to handle wall-clock times that don't exist
-- |   (spring forward) or are ambiguous (fall back)
-- | - `offset`: how to handle conflicts between an explicit UTC offset and
-- |   the IANA timezone rules
-- | - `overflow`: how to handle out-of-range field values
type ZonedDateTimeFromOptions =
  { disambiguation :: Disambiguation
  , offset :: OffsetDisambiguation
  , overflow :: Overflow
  }

defaultZonedDateTimeFromOptions :: ZonedDateTimeFromOptions
defaultZonedDateTimeFromOptions =
  { disambiguation: Compatible
  , offset: Use
  , overflow: Constrain
  }

-- Construction

-- | Construct from nanoseconds since the Unix epoch and an IANA time zone name.
-- | Returns `Nothing` for invalid time zones or out-of-range epoch values.
fromEpochNanoseconds :: BigInt -> String -> Maybe ZonedDateTime
fromEpochNanoseconds ns tz = fromEpochNanosecondsImpl Just Nothing ns tz

-- | Parse a string with timezone annotation
-- | (e.g. `"2024-03-15T10:30:00+00:00[UTC]"`).
-- | Returns `Nothing` for invalid strings.
-- |
-- | The UTC offset in the string is validated against the timezone rules.
-- | If the offset is inconsistent with the IANA timezone at that instant
-- | (e.g. due to changed DST rules), parsing fails. Use `fromStringWith` with
-- | `offset: Ignore` to handle strings where timezone rules may have changed.
fromString :: String -> Maybe ZonedDateTime
fromString s = fromStringImpl Just Nothing s

-- | Parse with explicit disambiguation options. See `ZonedDateTimeFromOptions`.
fromStringWith :: ZonedDateTimeFromOptions -> String -> Maybe ZonedDateTime
fromStringWith opts s = fromStringWithImpl Just Nothing
  { disambiguation: disambiguationToString opts.disambiguation
  , offset: offsetDisambiguationToString opts.offset
  , overflow: overflowToString opts.overflow
  }
  s

-- Properties

-- | Calendar year.
foreign import getYear :: ZonedDateTime -> Int

-- | Month of the year (1–12).
foreign import getMonth :: ZonedDateTime -> Int

-- | Day of the month (1–31).
foreign import getDay :: ZonedDateTime -> Int

-- | Hour of the day (0–23).
foreign import getHour :: ZonedDateTime -> Int

-- | Minute of the hour (0–59).
foreign import getMinute :: ZonedDateTime -> Int

-- | Second of the minute (0–59).
foreign import getSecond :: ZonedDateTime -> Int

-- | Millisecond (0–999).
foreign import getMillisecond :: ZonedDateTime -> Int

-- | Microsecond (0–999).
foreign import getMicrosecond :: ZonedDateTime -> Int

-- | Nanosecond (0–999).
foreign import getNanosecond :: ZonedDateTime -> Int

-- | Day of the week (1 = Monday, 7 = Sunday), per ISO 8601.
foreign import getDayOfWeek :: ZonedDateTime -> Int

-- | Day of the year (1–366).
foreign import getDayOfYear :: ZonedDateTime -> Int

-- | ISO 8601 week number (1–53).
foreign import getWeekOfYear :: ZonedDateTime -> Int

-- | The year that corresponds to `getWeekOfYear`.
foreign import getYearOfWeek :: ZonedDateTime -> Int

-- | Number of days in this month (28–31).
foreign import getDaysInMonth :: ZonedDateTime -> Int

-- | Always 7.
foreign import getDaysInWeek :: ZonedDateTime -> Int

-- | Number of days in this year (365 or 366).
foreign import getDaysInYear :: ZonedDateTime -> Int

-- | Always 12 for the ISO calendar.
foreign import getMonthsInYear :: ZonedDateTime -> Int

-- | Whether this year is a leap year.
foreign import getInLeapYear :: ZonedDateTime -> Boolean

-- | The IANA time zone identifier (e.g. `"America/New_York"`, `"UTC"`).
foreign import getTimeZoneId :: ZonedDateTime -> String

-- | The UTC offset as a string (e.g. `"+00:00"`, `"-05:00"`).
foreign import getOffset :: ZonedDateTime -> String

-- | The UTC offset in nanoseconds.
foreign import getOffsetNanoseconds :: ZonedDateTime -> Int

-- | Milliseconds since the Unix epoch. May lose sub-millisecond precision.
foreign import getEpochMilliseconds :: ZonedDateTime -> Number

-- | Nanoseconds since the Unix epoch (full precision).
foreign import getEpochNanoseconds :: ZonedDateTime -> BigInt

-- | The number of real-world hours in this calendar day, accounting for DST.
-- | Usually 24, but can be 23 or 25 during DST transitions.
foreign import getHoursInDay :: ZonedDateTime -> Number

-- Update
type ZonedDateTimeWithFields =
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

-- | Create a modified copy with the given fields and disambiguation options.
-- | Returns `Nothing` if the result would be invalid.
with :: ZonedDateTimeFromOptions -> ZonedDateTimeWithFields -> ZonedDateTime -> Maybe ZonedDateTime
with opts fields zdt = withImpl Just Nothing
  { disambiguation: disambiguationToString opts.disambiguation
  , offset: offsetDisambiguationToString opts.offset
  , overflow: overflowToString opts.overflow
  }
  fields zdt

-- | Replace the time component, keeping date and timezone.
foreign import withPlainTime :: PlainTime -> ZonedDateTime -> ZonedDateTime

-- | Change the time zone, preserving the exact instant. The wall-clock time
-- | will change to reflect the new timezone.
foreign import withTimeZone :: String -> ZonedDateTime -> ZonedDateTime

-- Arithmetic

-- | Add a duration. Date components are added as calendar arithmetic
-- | (wall-clock time preserved), then time components as real elapsed time.
-- | Returns `Nothing` on overflow.
add :: Duration -> ZonedDateTime -> Maybe ZonedDateTime
add dur zdt = addImpl Just Nothing dur zdt

-- | Subtract a duration. Same semantics as `add`.
subtract :: Duration -> ZonedDateTime -> Maybe ZonedDateTime
subtract dur zdt = subtractImpl Just Nothing dur zdt

-- Diff

-- | Options for `since` and `until`. Default: largest unit is `Years`,
-- | smallest unit is `Nanoseconds`.
type ZonedDateTimeDiffOptions =
  { largestUnit :: DateTimeUnit
  , smallestUnit :: DateTimeUnit
  , roundingIncrement :: Int
  , roundingMode :: RoundingMode
  }

defaultZonedDateTimeDiffOptions :: ZonedDateTimeDiffOptions
defaultZonedDateTimeDiffOptions =
  { largestUnit: DateU Years
  , smallestUnit: TimeU Nanoseconds
  , roundingIncrement: 1
  , roundingMode: HalfExpand
  }

-- | `since opts a b` returns the duration from `b` to `a`
-- | (i.e., `a.since(b)` in JS). The result is positive when `a` is after `b`.
since :: ZonedDateTimeDiffOptions -> ZonedDateTime -> ZonedDateTime -> Duration
since opts other self = sinceImpl
  { largestUnit: dateTimeUnitToString opts.largestUnit
  , smallestUnit: dateTimeUnitToString opts.smallestUnit
  , roundingIncrement: opts.roundingIncrement
  , roundingMode: roundingModeToString opts.roundingMode
  }
  other self

-- | `until opts a b` returns the duration from `a` to `b`
-- | (i.e., `a.until(b)` in JS). The result is positive when `b` is after `a`.
until :: ZonedDateTimeDiffOptions -> ZonedDateTime -> ZonedDateTime -> Duration
until opts other self = untilImpl
  { largestUnit: dateTimeUnitToString opts.largestUnit
  , smallestUnit: dateTimeUnitToString opts.smallestUnit
  , roundingIncrement: opts.roundingIncrement
  , roundingMode: roundingModeToString opts.roundingMode
  }
  other self


-- Round

-- | Options for `round`.
type ZonedDateTimeRoundOptions =
  { smallestUnit :: DateTimeUnit
  , roundingIncrement :: Int
  , roundingMode :: RoundingMode
  }

defaultZonedDateTimeRoundOptions :: ZonedDateTimeRoundOptions
defaultZonedDateTimeRoundOptions =
  { smallestUnit: TimeU Nanoseconds
  , roundingIncrement: 1
  , roundingMode: HalfExpand
  }

-- | Round the date-time to the given precision.
round :: ZonedDateTimeRoundOptions -> ZonedDateTime -> ZonedDateTime
round opts zdt = roundImpl
  { smallestUnit: dateTimeUnitToString opts.smallestUnit
  , roundingIncrement: opts.roundingIncrement
  , roundingMode: roundingModeToString opts.roundingMode
  }
  zdt

-- Timezone operations

-- | The first instant of the calendar day in this time zone.
-- | Usually midnight, but may be a later time in zones where DST transitions
-- | occur at midnight.
foreign import startOfDay :: ZonedDateTime -> ZonedDateTime

-- | Find the next or previous UTC offset transition (DST change) from this
-- | instant. Returns `Nothing` if there is no transition in the given
-- | direction (e.g. for fixed-offset zones like UTC).
getTimeZoneTransition :: TransitionDirection -> ZonedDateTime -> Maybe Instant
getTimeZoneTransition dir zdt = getTimeZoneTransitionImpl Just Nothing (transitionDirectionToString dir) zdt

-- Conversions

-- | Extract the exact instant, discarding timezone information.
foreign import toInstant :: ZonedDateTime -> Instant

-- | Extract the wall-clock date and time, discarding timezone information.
foreign import toPlainDateTime :: ZonedDateTime -> PlainDateTime

-- | Extract the date component.
foreign import toPlainDate :: ZonedDateTime -> PlainDate

-- | Extract the time component.
foreign import toPlainTime :: ZonedDateTime -> PlainTime

-- | Extract the year and month.
foreign import toPlainYearMonth :: ZonedDateTime -> PlainYearMonth

-- | Extract the month and day.
foreign import toPlainMonthDay :: ZonedDateTime -> PlainMonthDay

-- | Serialize to an ISO 8601 string with offset and timezone annotation
-- | (e.g. `"2024-03-15T10:30:00-04:00[America/New_York]"`).
foreign import toString :: ZonedDateTime -> String

-- FFI imports
foreign import fromEpochNanosecondsImpl :: (ZonedDateTime -> Maybe ZonedDateTime) -> Maybe ZonedDateTime -> BigInt -> String -> Maybe ZonedDateTime
foreign import fromStringImpl :: (ZonedDateTime -> Maybe ZonedDateTime) -> Maybe ZonedDateTime -> String -> Maybe ZonedDateTime
foreign import fromStringWithImpl :: (ZonedDateTime -> Maybe ZonedDateTime) -> Maybe ZonedDateTime -> { disambiguation :: String, offset :: String, overflow :: String } -> String -> Maybe ZonedDateTime
foreign import withImpl :: (ZonedDateTime -> Maybe ZonedDateTime) -> Maybe ZonedDateTime -> { disambiguation :: String, offset :: String, overflow :: String } -> ZonedDateTimeWithFields -> ZonedDateTime -> Maybe ZonedDateTime
foreign import addImpl :: (ZonedDateTime -> Maybe ZonedDateTime) -> Maybe ZonedDateTime -> Duration -> ZonedDateTime -> Maybe ZonedDateTime
foreign import subtractImpl :: (ZonedDateTime -> Maybe ZonedDateTime) -> Maybe ZonedDateTime -> Duration -> ZonedDateTime -> Maybe ZonedDateTime
foreign import sinceImpl :: { largestUnit :: String, smallestUnit :: String, roundingIncrement :: Int, roundingMode :: String } -> ZonedDateTime -> ZonedDateTime -> Duration
foreign import untilImpl :: { largestUnit :: String, smallestUnit :: String, roundingIncrement :: Int, roundingMode :: String } -> ZonedDateTime -> ZonedDateTime -> Duration
foreign import roundImpl :: { smallestUnit :: String, roundingIncrement :: Int, roundingMode :: String } -> ZonedDateTime -> ZonedDateTime
foreign import getTimeZoneTransitionImpl :: (Instant -> Maybe Instant) -> Maybe Instant -> String -> ZonedDateTime -> Maybe Instant
