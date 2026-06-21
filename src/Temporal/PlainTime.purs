-- | Construction, arithmetic, and inspection of `PlainTime` values.
-- |
-- | A `PlainTime` represents a wall-clock time without a date or time zone
-- | (e.g. 10:30:00). Range: 00:00:00.000000000 to 23:59:59.999999999.
-- |
-- | Key things to know:
-- |
-- | - **Wraps at midnight**: `add` and `subtract` silently wrap past midnight.
-- |   Adding 2 hours to 23:30 gives 01:30 with no overflow indication.
-- | - **Date units ignored**: if the duration passed to `add`/`subtract`
-- |   contains years, months, weeks, or days, those fields are silently
-- |   ignored — no error is raised.
module Temporal.PlainTime
  ( module Temporal.Internal.Types
  , PlainTimeFields
  , defaultPlainTimeFields
  , plainTime
  , fromString
  , getHour
  , getMinute
  , getSecond
  , getMillisecond
  , getMicrosecond
  , getNanosecond
  , with
  , add
  , subtract
  , PlainTimeDiffOptions
  , defaultPlainTimeDiffOptions
  , since
  , until
  , PlainTimeRoundOptions
  , defaultPlainTimeRoundOptions
  , round
  , toPlainDateTime
  , toString
  ) where

import Prelude hiding (add)

import Data.Maybe (Maybe(..))
import Temporal.Internal.Options (RoundingMode(..), TimeUnit(..), roundingModeToString, timeUnitToString)
import Temporal.Internal.Types (Duration, PlainDate, PlainDateTime, PlainTime)

-- | Fields for constructing a `PlainTime`. All fields default to `0`.
type PlainTimeFields =
  { hour :: Int
  , minute :: Int
  , second :: Int
  , millisecond :: Int
  , microsecond :: Int
  , nanosecond :: Int
  }

-- | All fields set to `0`. Use record update syntax:
-- |
-- | ```purescript
-- | plainTime (defaultPlainTimeFields { hour = 14, minute = 30 })
-- | ```
defaultPlainTimeFields :: PlainTimeFields
defaultPlainTimeFields =
  { hour: 0
  , minute: 0
  , second: 0
  , millisecond: 0
  , microsecond: 0
  , nanosecond: 0
  }

-- | Construct a `PlainTime` from fields. Returns `Nothing` if any field is
-- | out of range (hour: 0-23, minute/second: 0-59, sub-second: 0-999).
plainTime :: PlainTimeFields -> Maybe PlainTime
plainTime fields = plainTimeImpl Just Nothing fields

-- | Parse an ISO 8601 time string (e.g. `"10:30:00"`, `"14:00"`).
-- | Returns `Nothing` for invalid strings.
fromString :: String -> Maybe PlainTime
fromString s = fromStringImpl Just Nothing s

-- Properties

-- | Hour of the day (0–23).
foreign import getHour :: PlainTime -> Int

-- | Minute of the hour (0–59).
foreign import getMinute :: PlainTime -> Int

-- | Second of the minute (0–59).
foreign import getSecond :: PlainTime -> Int

-- | Millisecond (0–999).
foreign import getMillisecond :: PlainTime -> Int

-- | Microsecond (0–999).
foreign import getMicrosecond :: PlainTime -> Int

-- | Nanosecond (0–999).
foreign import getNanosecond :: PlainTime -> Int

-- | Create a modified copy with the given fields. Returns `Nothing` if the
-- | resulting time would be invalid.
with :: PlainTimeFields -> PlainTime -> Maybe PlainTime
with fields pt = withImpl Just Nothing fields pt

-- | Add a duration, wrapping at midnight. Date components (years, months,
-- | weeks, days) in the duration are silently ignored.
foreign import add :: Duration -> PlainTime -> PlainTime

-- | Subtract a duration, wrapping at midnight. Date components (years, months,
-- | weeks, days) in the duration are silently ignored.
foreign import subtract :: Duration -> PlainTime -> PlainTime

-- Diff

-- | Options for `since` and `until`. Default: largest unit is `Hours`,
-- | smallest unit is `Nanoseconds`.
type PlainTimeDiffOptions =
  { largestUnit :: TimeUnit
  , smallestUnit :: TimeUnit
  , roundingIncrement :: Int
  , roundingMode :: RoundingMode
  }

defaultPlainTimeDiffOptions :: PlainTimeDiffOptions
defaultPlainTimeDiffOptions =
  { largestUnit: Hours
  , smallestUnit: Nanoseconds
  , roundingIncrement: 1
  , roundingMode: HalfExpand
  }

-- | `since opts a b` returns the duration from `b` to `a`
-- | (i.e., `a.since(b)` in JS). The result is positive when `a` is after `b`.
since :: PlainTimeDiffOptions -> PlainTime -> PlainTime -> Duration
since opts a b = sinceImpl
  { largestUnit: timeUnitToString opts.largestUnit
  , smallestUnit: timeUnitToString opts.smallestUnit
  , roundingIncrement: opts.roundingIncrement
  , roundingMode: roundingModeToString opts.roundingMode
  }
  a b

-- | `until opts a b` returns the duration from `a` to `b`
-- | (i.e., `a.until(b)` in JS). The result is positive when `b` is after `a`.
-- |
-- | `until opts a b == negated (since opts a b)` for all `a`, `b`.
until :: PlainTimeDiffOptions -> PlainTime -> PlainTime -> Duration
until opts a b = untilImpl
  { largestUnit: timeUnitToString opts.largestUnit
  , smallestUnit: timeUnitToString opts.smallestUnit
  , roundingIncrement: opts.roundingIncrement
  , roundingMode: roundingModeToString opts.roundingMode
  }
  a b


-- Round

-- | Options for `round`.
type PlainTimeRoundOptions =
  { smallestUnit :: TimeUnit
  , roundingIncrement :: Int
  , roundingMode :: RoundingMode
  }

defaultPlainTimeRoundOptions :: PlainTimeRoundOptions
defaultPlainTimeRoundOptions =
  { smallestUnit: Nanoseconds
  , roundingIncrement: 1
  , roundingMode: HalfExpand
  }

-- | Round the time to the given precision.
round :: PlainTimeRoundOptions -> PlainTime -> PlainTime
round opts pt = roundImpl
  { smallestUnit: timeUnitToString opts.smallestUnit
  , roundingIncrement: opts.roundingIncrement
  , roundingMode: roundingModeToString opts.roundingMode
  }
  pt

-- | Combine this time with a date to produce a `PlainDateTime`.
foreign import toPlainDateTime :: PlainDate -> PlainTime -> PlainDateTime

-- | Serialize to an ISO 8601 time string (e.g. `"10:30:00"`).
-- | Trailing zero components are omitted.
foreign import toString :: PlainTime -> String


-- FFI imports
foreign import plainTimeImpl :: (PlainTime -> Maybe PlainTime) -> Maybe PlainTime -> PlainTimeFields -> Maybe PlainTime
foreign import fromStringImpl :: (PlainTime -> Maybe PlainTime) -> Maybe PlainTime -> String -> Maybe PlainTime
foreign import withImpl :: (PlainTime -> Maybe PlainTime) -> Maybe PlainTime -> PlainTimeFields -> PlainTime -> Maybe PlainTime
foreign import sinceImpl :: { largestUnit :: String, smallestUnit :: String, roundingIncrement :: Int, roundingMode :: String } -> PlainTime -> PlainTime -> Duration
foreign import untilImpl :: { largestUnit :: String, smallestUnit :: String, roundingIncrement :: Int, roundingMode :: String } -> PlainTime -> PlainTime -> Duration
foreign import roundImpl :: { smallestUnit :: String, roundingIncrement :: Int, roundingMode :: String } -> PlainTime -> PlainTime
