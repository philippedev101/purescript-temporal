-- | Construction, arithmetic, and inspection of `Instant` values.
-- |
-- | An `Instant` represents an exact moment on the UTC timeline with nanosecond
-- | precision, independent of any time zone or calendar.
-- |
-- | Key things to know:
-- |
-- | - **Time-only arithmetic**: `add` and `subtract` only accept durations with
-- |   time components (hours and below). Passing years, months, weeks, or days
-- |   returns `Nothing`, because their real-world length varies with time zone
-- |   and calendar. Convert to `ZonedDateTime` first for calendar arithmetic.
-- | - **Parsing requires UTC offset**: `fromString "2024-03-15T10:30:00"`
-- |   returns `Nothing` — you must include `Z` or an offset like `+00:00`.
-- | - **Diff returns time-only**: `since`/`until` return durations with `Hours`
-- |   as the default largest unit. The result may have hours exceeding 24.
module Temporal.Instant
  ( module Temporal.Internal.Types
  , fromEpochNanoseconds
  , fromEpochMilliseconds
  , fromString
  , getEpochMilliseconds
  , getEpochNanoseconds
  , add
  , subtract
  , InstantDiffOptions
  , defaultInstantDiffOptions
  , since
  , until
  , since'
  , until'
  , InstantRoundOptions
  , defaultInstantRoundOptions
  , round
  , toZonedDateTimeISO
  , toString
  ) where

import Prelude hiding (add)

import Data.Maybe (Maybe(..))
import JS.BigInt (BigInt)
import Temporal.Internal.Options (RoundingMode(..), TimeUnit(..), roundingModeToString, timeUnitToString)
import Temporal.Internal.Types (Duration, Instant, ZonedDateTime)

-- Construction

-- | Construct an `Instant` from nanoseconds since the Unix epoch.
-- | Returns `Nothing` if the value is outside the representable range
-- | (approximately ±10^8 days from epoch).
fromEpochNanoseconds :: BigInt -> Maybe Instant
fromEpochNanoseconds ns = fromEpochNanosecondsImpl Just Nothing ns

-- | Construct an `Instant` from milliseconds since the Unix epoch.
-- | Returns `Nothing` if the value is outside the representable range.
-- |
-- | The fractional part is truncated (not rounded).
fromEpochMilliseconds :: Number -> Maybe Instant
fromEpochMilliseconds ms = fromEpochMillisecondsImpl Just Nothing ms

-- | Parse an ISO 8601 string with a UTC offset (e.g. `"2024-03-15T10:30:00Z"`,
-- | `"2024-03-15T10:30:00+05:30"`). Returns `Nothing` for invalid strings or
-- | strings without a UTC offset.
fromString :: String -> Maybe Instant
fromString s = fromStringImpl Just Nothing s

-- Properties

-- | Milliseconds since the Unix epoch. This may lose sub-millisecond precision.
foreign import getEpochMilliseconds :: Instant -> Number

-- | Nanoseconds since the Unix epoch (full precision).
foreign import getEpochNanoseconds :: Instant -> BigInt

-- Arithmetic

-- | Add a time-only duration. Returns `Nothing` if the duration contains
-- | date components (years, months, weeks, or days) or if the result would
-- | be outside the representable range.
add :: Duration -> Instant -> Maybe Instant
add dur inst = addImpl Just Nothing dur inst

-- | Subtract a time-only duration. Same restrictions as `add`.
subtract :: Duration -> Instant -> Maybe Instant
subtract dur inst = subtractImpl Just Nothing dur inst

-- Diff

-- | Options for `since` and `until`. Only time units (hours and below) are
-- | allowed. Default: largest unit is `Hours`, smallest unit is `Nanoseconds`.
type InstantDiffOptions =
  { largestUnit :: TimeUnit
  , smallestUnit :: TimeUnit
  , roundingIncrement :: Int
  , roundingMode :: RoundingMode
  }

defaultInstantDiffOptions :: InstantDiffOptions
defaultInstantDiffOptions =
  { largestUnit: Hours
  , smallestUnit: Nanoseconds
  , roundingIncrement: 1
  , roundingMode: HalfExpand
  }

-- | `since opts a b` returns the duration from `b` to `a`
-- | (i.e., `a.since(b)` in JS). The result is positive when `a` is after `b`.
since :: InstantDiffOptions -> Instant -> Instant -> Duration
since opts other self = sinceImpl
  { largestUnit: timeUnitToString opts.largestUnit
  , smallestUnit: timeUnitToString opts.smallestUnit
  , roundingIncrement: opts.roundingIncrement
  , roundingMode: roundingModeToString opts.roundingMode
  }
  other self

-- | `until opts a b` returns the duration from `a` to `b`
-- | (i.e., `a.until(b)` in JS). The result is positive when `b` is after `a`.
until :: InstantDiffOptions -> Instant -> Instant -> Duration
until opts other self = untilImpl
  { largestUnit: timeUnitToString opts.largestUnit
  , smallestUnit: timeUnitToString opts.smallestUnit
  , roundingIncrement: opts.roundingIncrement
  , roundingMode: roundingModeToString opts.roundingMode
  }
  other self

-- | `since' a b` — duration from `b` to `a` with default options.
since' :: Instant -> Instant -> Duration
since' = since defaultInstantDiffOptions

-- | `until' a b` — duration from `a` to `b` with default options.
until' :: Instant -> Instant -> Duration
until' = until defaultInstantDiffOptions

-- Round

-- | Options for `round`. Only time units are allowed.
type InstantRoundOptions =
  { smallestUnit :: TimeUnit
  , roundingIncrement :: Int
  , roundingMode :: RoundingMode
  }

defaultInstantRoundOptions :: InstantRoundOptions
defaultInstantRoundOptions =
  { smallestUnit: Nanoseconds
  , roundingIncrement: 1
  , roundingMode: HalfExpand
  }

-- | Round the instant to the given precision.
round :: InstantRoundOptions -> Instant -> Instant
round opts inst = roundImpl
  { smallestUnit: timeUnitToString opts.smallestUnit
  , roundingIncrement: opts.roundingIncrement
  , roundingMode: roundingModeToString opts.roundingMode
  }
  inst

-- Conversion

-- | Convert to a `ZonedDateTime` in the given IANA time zone using the
-- | ISO 8601 calendar.
-- |
-- | ```purescript
-- | toZonedDateTimeISO "America/New_York" instant
-- | ```
foreign import toZonedDateTimeISO :: String -> Instant -> ZonedDateTime

-- | Serialize to an ISO 8601 string with `Z` suffix (e.g. `"2024-03-15T10:30:00Z"`).
foreign import toString :: Instant -> String

-- FFI imports
foreign import fromEpochNanosecondsImpl :: (Instant -> Maybe Instant) -> Maybe Instant -> BigInt -> Maybe Instant
foreign import fromEpochMillisecondsImpl :: (Instant -> Maybe Instant) -> Maybe Instant -> Number -> Maybe Instant
foreign import fromStringImpl :: (Instant -> Maybe Instant) -> Maybe Instant -> String -> Maybe Instant
foreign import addImpl :: (Instant -> Maybe Instant) -> Maybe Instant -> Duration -> Instant -> Maybe Instant
foreign import subtractImpl :: (Instant -> Maybe Instant) -> Maybe Instant -> Duration -> Instant -> Maybe Instant
foreign import sinceImpl :: { largestUnit :: String, smallestUnit :: String, roundingIncrement :: Int, roundingMode :: String } -> Instant -> Instant -> Duration
foreign import untilImpl :: { largestUnit :: String, smallestUnit :: String, roundingIncrement :: Int, roundingMode :: String } -> Instant -> Instant -> Duration
foreign import roundImpl :: { smallestUnit :: String, roundingIncrement :: Int, roundingMode :: String } -> Instant -> Instant
