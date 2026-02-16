-- | Construction, arithmetic, and inspection of `PlainYearMonth` values.
-- |
-- | A `PlainYearMonth` represents a year and month without a day component
-- | (e.g. 2024-03). Useful for calendar month views, credit card expiry dates,
-- | or any context where the specific day is irrelevant.
module Temporal.PlainYearMonth
  ( module Temporal.Internal.Types
  , plainYearMonth
  , fromString
  , getYear
  , getMonth
  , getMonthCode
  , getDaysInMonth
  , getDaysInYear
  , getMonthsInYear
  , getInLeapYear
  , with
  , add
  , subtract
  , PlainYearMonthDiffOptions
  , defaultPlainYearMonthDiffOptions
  , since
  , until
  , since'
  , until'
  , toPlainDate
  , toString
  ) where

import Prelude hiding (add)

import Data.Maybe (Maybe(..))
import Temporal.Internal.Options (DateUnit(..), RoundingMode(..), dateUnitToString, roundingModeToString)
import Temporal.Internal.Types (Duration, PlainDate, PlainYearMonth)

-- Construction

-- | Construct from year and month (1–12). Returns `Nothing` for invalid months.
plainYearMonth :: Int -> Int -> Maybe PlainYearMonth
plainYearMonth y m = plainYearMonthImpl Just Nothing y m

-- | Parse an ISO 8601 year-month string (e.g. `"2024-03"`).
-- | Returns `Nothing` for invalid strings.
fromString :: String -> Maybe PlainYearMonth
fromString s = fromStringImpl Just Nothing s

-- Properties

-- | Calendar year.
foreign import getYear :: PlainYearMonth -> Int

-- | Month of the year (1–12).
foreign import getMonth :: PlainYearMonth -> Int

-- | Month code string (e.g. `"M03"` for March). Useful for non-ISO calendars.
foreign import getMonthCode :: PlainYearMonth -> String

-- | Number of days in this month (28–31).
foreign import getDaysInMonth :: PlainYearMonth -> Int

-- | Number of days in this year (365 or 366).
foreign import getDaysInYear :: PlainYearMonth -> Int

-- | Always 12 for the ISO calendar.
foreign import getMonthsInYear :: PlainYearMonth -> Int

-- | Whether this year is a leap year.
foreign import getInLeapYear :: PlainYearMonth -> Boolean

-- | Create a modified copy with the given year and month. Returns `Nothing`
-- | if the result would be invalid.
with :: { year :: Int, month :: Int } -> PlainYearMonth -> Maybe PlainYearMonth
with fields pym = withImpl Just Nothing fields pym

-- | Add a duration (only year and month components are meaningful).
-- | Returns `Nothing` on overflow.
add :: Duration -> PlainYearMonth -> Maybe PlainYearMonth
add dur pym = addImpl Just Nothing dur pym

-- | Subtract a duration. Returns `Nothing` on overflow.
subtract :: Duration -> PlainYearMonth -> Maybe PlainYearMonth
subtract dur pym = subtractImpl Just Nothing dur pym

-- Diff

-- | Options for `since` and `until`. Default: largest unit is `Years`,
-- | smallest unit is `Months`.
type PlainYearMonthDiffOptions =
  { largestUnit :: DateUnit
  , smallestUnit :: DateUnit
  , roundingIncrement :: Int
  , roundingMode :: RoundingMode
  }

defaultPlainYearMonthDiffOptions :: PlainYearMonthDiffOptions
defaultPlainYearMonthDiffOptions =
  { largestUnit: Years
  , smallestUnit: Months
  , roundingIncrement: 1
  , roundingMode: HalfExpand
  }

-- | `since opts a b` returns the duration from `b` to `a`.
since :: PlainYearMonthDiffOptions -> PlainYearMonth -> PlainYearMonth -> Duration
since opts other self = sinceImpl
  { largestUnit: dateUnitToString opts.largestUnit
  , smallestUnit: dateUnitToString opts.smallestUnit
  , roundingIncrement: opts.roundingIncrement
  , roundingMode: roundingModeToString opts.roundingMode
  }
  other self

-- | `until opts a b` returns the duration from `a` to `b`.
until :: PlainYearMonthDiffOptions -> PlainYearMonth -> PlainYearMonth -> Duration
until opts other self = untilImpl
  { largestUnit: dateUnitToString opts.largestUnit
  , smallestUnit: dateUnitToString opts.smallestUnit
  , roundingIncrement: opts.roundingIncrement
  , roundingMode: roundingModeToString opts.roundingMode
  }
  other self

-- | `since' a b` — duration from `b` to `a` with default options.
since' :: PlainYearMonth -> PlainYearMonth -> Duration
since' = since defaultPlainYearMonthDiffOptions

-- | `until' a b` — duration from `a` to `b` with default options.
until' :: PlainYearMonth -> PlainYearMonth -> Duration
until' = until defaultPlainYearMonthDiffOptions

-- Conversion

-- | Convert to a `PlainDate` by specifying a day. Out-of-range days are
-- | **constrained** — e.g. `toPlainDate 31` on February yields Feb 28 or 29.
-- | Returns `Nothing` for truly invalid values.
toPlainDate :: Int -> PlainYearMonth -> Maybe PlainDate
toPlainDate day pym = toPlainDateImpl Just Nothing day pym

-- | Serialize to an ISO 8601 year-month string (e.g. `"2024-03"`).
foreign import toString :: PlainYearMonth -> String

-- FFI imports
foreign import plainYearMonthImpl :: (PlainYearMonth -> Maybe PlainYearMonth) -> Maybe PlainYearMonth -> Int -> Int -> Maybe PlainYearMonth
foreign import fromStringImpl :: (PlainYearMonth -> Maybe PlainYearMonth) -> Maybe PlainYearMonth -> String -> Maybe PlainYearMonth
foreign import withImpl :: (PlainYearMonth -> Maybe PlainYearMonth) -> Maybe PlainYearMonth -> { year :: Int, month :: Int } -> PlainYearMonth -> Maybe PlainYearMonth
foreign import addImpl :: (PlainYearMonth -> Maybe PlainYearMonth) -> Maybe PlainYearMonth -> Duration -> PlainYearMonth -> Maybe PlainYearMonth
foreign import subtractImpl :: (PlainYearMonth -> Maybe PlainYearMonth) -> Maybe PlainYearMonth -> Duration -> PlainYearMonth -> Maybe PlainYearMonth
foreign import sinceImpl :: { largestUnit :: String, smallestUnit :: String, roundingIncrement :: Int, roundingMode :: String } -> PlainYearMonth -> PlainYearMonth -> Duration
foreign import untilImpl :: { largestUnit :: String, smallestUnit :: String, roundingIncrement :: Int, roundingMode :: String } -> PlainYearMonth -> PlainYearMonth -> Duration
foreign import toPlainDateImpl :: (PlainDate -> Maybe PlainDate) -> Maybe PlainDate -> Int -> PlainYearMonth -> Maybe PlainDate
