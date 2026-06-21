-- | Construction, arithmetic, and inspection of `PlainDate` values.
-- |
-- | A `PlainDate` represents a calendar date without a time component or time
-- | zone (e.g. 2024-03-15).
-- |
-- | Key things to know:
-- |
-- | - **Default overflow is constrain**: `plainDate 2024 2 31` silently
-- |   produces February 29 (clamped), not `Nothing`. Use `plainDateWith Reject`
-- |   for strict validation.
-- | - **Arithmetic order matters**: when adding a multi-component duration,
-- |   fields are applied in order (years, then months, then weeks+days) with
-- |   clamping after each step. `add { months: 1, days: 31 }` on Jan 31
-- |   gives Mar 31 (Jan 31 + 1 month = Feb 28, + 31 days = Mar 31), which
-- |   differs from adding months and days in separate steps.
module Temporal.PlainDate
  ( module Temporal.Internal.Types
  , PlainDateFields
  , plainDate
  , plainDateWith
  , fromString
  , getYear
  , getMonth
  , getDay
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
  , add
  , subtract
  , PlainDateDiffOptions
  , defaultPlainDateDiffOptions
  , since
  , until
  , toPlainDateTime
  , toPlainYearMonth
  , toPlainMonthDay
  , toZonedDateTime
  , toString
  ) where

import Prelude hiding (add)

import Data.Maybe (Maybe(..))
import Temporal.Internal.Options (DateTimeUnit(..), DateUnit(..), Overflow, RoundingMode(..), dateTimeUnitToString, overflowToString, roundingModeToString)
import Temporal.Internal.Types (Duration, PlainDate, PlainDateTime, PlainMonthDay, PlainTime, PlainYearMonth, ZonedDateTime)

type PlainDateFields =
  { year :: Int
  , month :: Int
  , day :: Int
  }

-- | Construct a `PlainDate` from year, month, and day. Out-of-range days are
-- | **constrained** (clamped) to the nearest valid value. For example,
-- | `plainDate 2024 2 31` yields February 29 (2024 is a leap year).
-- |
-- | Returns `Nothing` only for truly invalid values (e.g. month 13).
-- | Use `plainDateWith Reject` if you want strict validation.
plainDate :: Int -> Int -> Int -> Maybe PlainDate
plainDate y m d = plainDateImpl Just Nothing y m d

-- | Construct a `PlainDate` with explicit overflow handling.
-- |
-- | - `Constrain`: clamp out-of-range values (default Temporal behavior)
-- | - `Reject`: return `Nothing` for any out-of-range value
plainDateWith :: Overflow -> Int -> Int -> Int -> Maybe PlainDate
plainDateWith overflow y m d = plainDateWithImpl Just Nothing (overflowToString overflow) y m d

-- | Parse an ISO 8601 date string (e.g. `"2024-03-15"`).
-- | Returns `Nothing` for invalid strings.
fromString :: String -> Maybe PlainDate
fromString s = fromStringImpl Just Nothing s

-- Properties

-- | Calendar year (e.g. 2024). Can be negative for BCE dates.
foreign import getYear :: PlainDate -> Int

-- | Month of the year (1–12).
foreign import getMonth :: PlainDate -> Int

-- | Day of the month (1–31).
foreign import getDay :: PlainDate -> Int

-- | Day of the week (1 = Monday, 7 = Sunday), per ISO 8601.
foreign import getDayOfWeek :: PlainDate -> Int

-- | Day of the year (1–366).
foreign import getDayOfYear :: PlainDate -> Int

-- | ISO 8601 week number (1–53).
foreign import getWeekOfYear :: PlainDate -> Int

-- | The year that corresponds to `getWeekOfYear`. May differ from `getYear`
-- | at year boundaries (e.g. Dec 31 may be in week 1 of the next year).
foreign import getYearOfWeek :: PlainDate -> Int

-- | Number of days in this date's month (28–31).
foreign import getDaysInMonth :: PlainDate -> Int

-- | Always 7.
foreign import getDaysInWeek :: PlainDate -> Int

-- | Number of days in this date's year (365 or 366).
foreign import getDaysInYear :: PlainDate -> Int

-- | Always 12 for the ISO calendar.
foreign import getMonthsInYear :: PlainDate -> Int

-- | Whether this date's year is a leap year.
foreign import getInLeapYear :: PlainDate -> Boolean

-- | Create a modified copy with the given fields. The `Overflow` option
-- | controls how out-of-range values are handled.
with :: Overflow -> PlainDateFields -> PlainDate -> Maybe PlainDate
with overflow fields pd = withImpl Just Nothing (overflowToString overflow) fields pd

-- | Add a duration to this date. Returns `Nothing` on overflow.
-- | Time components of the duration are ignored.
add :: Duration -> PlainDate -> Maybe PlainDate
add dur pd = addImpl Just Nothing dur pd

-- | Subtract a duration from this date. Returns `Nothing` on overflow.
-- | Time components of the duration are ignored.
subtract :: Duration -> PlainDate -> Maybe PlainDate
subtract dur pd = subtractImpl Just Nothing dur pd

-- Diff

-- | Options for `since` and `until`. Default: largest unit is `Years`,
-- | smallest unit is `Days`.
type PlainDateDiffOptions =
  { largestUnit :: DateTimeUnit
  , smallestUnit :: DateTimeUnit
  , roundingIncrement :: Int
  , roundingMode :: RoundingMode
  }

defaultPlainDateDiffOptions :: PlainDateDiffOptions
defaultPlainDateDiffOptions =
  { largestUnit: DateU Years
  , smallestUnit: DateU Days
  , roundingIncrement: 1
  , roundingMode: HalfExpand
  }

-- | `since opts a b` returns the duration from `b` to `a`
-- | (i.e., `a.since(b)` in JS). The result is positive when `a` is after `b`.
since :: PlainDateDiffOptions -> PlainDate -> PlainDate -> Duration
since opts other self = sinceImpl
  { largestUnit: dateTimeUnitToString opts.largestUnit
  , smallestUnit: dateTimeUnitToString opts.smallestUnit
  , roundingIncrement: opts.roundingIncrement
  , roundingMode: roundingModeToString opts.roundingMode
  }
  other self

-- | `until opts a b` returns the duration from `a` to `b`
-- | (i.e., `a.until(b)` in JS). The result is positive when `b` is after `a`.
until :: PlainDateDiffOptions -> PlainDate -> PlainDate -> Duration
until opts other self = untilImpl
  { largestUnit: dateTimeUnitToString opts.largestUnit
  , smallestUnit: dateTimeUnitToString opts.smallestUnit
  , roundingIncrement: opts.roundingIncrement
  , roundingMode: roundingModeToString opts.roundingMode
  }
  other self


-- Conversions

-- | Combine this date with a time to produce a `PlainDateTime`.
foreign import toPlainDateTime :: PlainTime -> PlainDate -> PlainDateTime

-- | Drop the day component, keeping year and month.
foreign import toPlainYearMonth :: PlainDate -> PlainYearMonth

-- | Drop the year component, keeping month and day.
foreign import toPlainMonthDay :: PlainDate -> PlainMonthDay

-- | Combine this date with a time and IANA time zone to produce a
-- | `ZonedDateTime`. DST disambiguation uses the default `"compatible"` mode.
foreign import toZonedDateTime :: String -> PlainTime -> PlainDate -> ZonedDateTime

-- | Serialize to an ISO 8601 date string (e.g. `"2024-03-15"`).
foreign import toString :: PlainDate -> String

-- FFI imports
foreign import plainDateImpl :: (PlainDate -> Maybe PlainDate) -> Maybe PlainDate -> Int -> Int -> Int -> Maybe PlainDate
foreign import plainDateWithImpl :: (PlainDate -> Maybe PlainDate) -> Maybe PlainDate -> String -> Int -> Int -> Int -> Maybe PlainDate
foreign import fromStringImpl :: (PlainDate -> Maybe PlainDate) -> Maybe PlainDate -> String -> Maybe PlainDate
foreign import withImpl :: (PlainDate -> Maybe PlainDate) -> Maybe PlainDate -> String -> PlainDateFields -> PlainDate -> Maybe PlainDate
foreign import addImpl :: (PlainDate -> Maybe PlainDate) -> Maybe PlainDate -> Duration -> PlainDate -> Maybe PlainDate
foreign import subtractImpl :: (PlainDate -> Maybe PlainDate) -> Maybe PlainDate -> Duration -> PlainDate -> Maybe PlainDate
foreign import sinceImpl :: { largestUnit :: String, smallestUnit :: String, roundingIncrement :: Int, roundingMode :: String } -> PlainDate -> PlainDate -> Duration
foreign import untilImpl :: { largestUnit :: String, smallestUnit :: String, roundingIncrement :: Int, roundingMode :: String } -> PlainDate -> PlainDate -> Duration
