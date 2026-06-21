-- | Construction, arithmetic, and inspection of `Duration` values.
-- |
-- | A `Duration` represents a length of time as a combination of date and time
-- | fields (years through nanoseconds). Key things to know:
-- |
-- | - **Same-sign rule**: all components must be zero or share the same sign.
-- |   Constructing `{ years: 1, months: -1 }` will fail.
-- | - **Unbalanced by default**: `{ hours: 36 }` stays as-is and does not
-- |   auto-convert to `{ days: 1, hours: 12 }`. Use `round` to rebalance.
-- | - **Eq is structural**: `hours 1.0 /= minutes 60.0` because `Eq` compares
-- |   component-by-component, not total elapsed time.
-- | - **Calendar operations need context**: `round`, `total`, `add`, `subtract`,
-- |   and `compare` may return `Nothing` (or throw) when calendar units (years,
-- |   months, weeks) are involved and no `relativeTo` is provided, because their
-- |   real-world length depends on a reference date.
module Temporal.Duration
  ( module Temporal.Internal.Types
  , module ReExportedOptions
  , DurationFields
  , defaultDurationFields
  , duration
  , fromString
  , years
  , months
  , weeks
  , days
  , hours
  , minutes
  , seconds
  , milliseconds
  , microseconds
  , nanoseconds
  , getYears
  , getMonths
  , getWeeks
  , getDays
  , getHours
  , getMinutes
  , getSeconds
  , getMilliseconds
  , getMicroseconds
  , getNanoseconds
  , sign
  , blank
  , add
  , subtract
  , negated
  , abs
  , DurationRoundOptions
  , defaultDurationRoundOptions
  , round
  , DurationTotalOptions
  , defaultDurationTotalOptions
  , total
  , compare
  , toString
  , zero
  ) where

import Prelude hiding (add)

import Data.Maybe (Maybe(..))
import Temporal.Internal.Options (DateTimeUnit(..), DateUnit(..), RelativeTo(..), RoundingMode(..), TimeUnit(..)) as ReExportedOptions
import Temporal.Internal.Options (DateTimeUnit(..), RelativeTo(..), RoundingMode(..), TimeUnit(..), dateTimeUnitToString, roundingModeToString)
import Temporal.Internal.Types (Duration, PlainDate, PlainDateTime, ZonedDateTime)

-- | All fields of a `Duration`. Every field defaults to `0.0` — set only the
-- | fields you need.
-- |
-- | All non-zero fields must share the same sign, or construction will fail.
type DurationFields =
  { years :: Number
  , months :: Number
  , weeks :: Number
  , days :: Number
  , hours :: Number
  , minutes :: Number
  , seconds :: Number
  , milliseconds :: Number
  , microseconds :: Number
  , nanoseconds :: Number
  }

-- | All fields set to `0.0`. Use record update syntax:
-- |
-- | ```purescript
-- | duration (defaultDurationFields { hours = 2.0, minutes = 30.0 })
-- | ```
defaultDurationFields :: DurationFields
defaultDurationFields =
  { years: 0.0
  , months: 0.0
  , weeks: 0.0
  , days: 0.0
  , hours: 0.0
  , minutes: 0.0
  , seconds: 0.0
  , milliseconds: 0.0
  , microseconds: 0.0
  , nanoseconds: 0.0
  }

-- | Construct a `Duration` from explicit fields. Returns `Nothing` if the
-- | fields have mixed signs (e.g. positive years with negative months).
duration :: DurationFields -> Maybe Duration
duration fields = durationImpl Just Nothing fields

-- | Parse an ISO 8601 duration string (e.g. `"PT1H30M"`, `"P1Y2M3D"`).
-- | Returns `Nothing` for invalid strings.
-- |
-- | Note: subsecond components are collapsed during serialization, so
-- | `fromString (toString d)` may produce a structurally different (but equal)
-- | duration. For example, `{ milliseconds: 1000 }` serializes to `"PT1S"` and
-- | parses back as `{ seconds: 1 }`.
fromString :: String -> Maybe Duration
fromString s = fromStringImpl Just Nothing s

-- | Create a duration of the given number of years. Always succeeds for
-- | finite values.
years :: Number -> Duration
years n = unsafeDuration (defaultDurationFields { years = n })

-- | Create a duration of the given number of months.
months :: Number -> Duration
months n = unsafeDuration (defaultDurationFields { months = n })

-- | Create a duration of the given number of weeks.
weeks :: Number -> Duration
weeks n = unsafeDuration (defaultDurationFields { weeks = n })

-- | Create a duration of the given number of days.
days :: Number -> Duration
days n = unsafeDuration (defaultDurationFields { days = n })

-- | Create a duration of the given number of hours.
hours :: Number -> Duration
hours n = unsafeDuration (defaultDurationFields { hours = n })

-- | Create a duration of the given number of minutes.
minutes :: Number -> Duration
minutes n = unsafeDuration (defaultDurationFields { minutes = n })

-- | Create a duration of the given number of seconds.
seconds :: Number -> Duration
seconds n = unsafeDuration (defaultDurationFields { seconds = n })

-- | Create a duration of the given number of milliseconds.
milliseconds :: Number -> Duration
milliseconds n = unsafeDuration (defaultDurationFields { milliseconds = n })

-- | Create a duration of the given number of microseconds.
microseconds :: Number -> Duration
microseconds n = unsafeDuration (defaultDurationFields { microseconds = n })

-- | Create a duration of the given number of nanoseconds.
nanoseconds :: Number -> Duration
nanoseconds n = unsafeDuration (defaultDurationFields { nanoseconds = n })

-- | The zero duration (all fields 0). `blank zero == true`.
zero :: Duration
zero = unsafeDuration defaultDurationFields

-- Properties
foreign import getYears :: Duration -> Number
foreign import getMonths :: Duration -> Number
foreign import getWeeks :: Duration -> Number
foreign import getDays :: Duration -> Number
foreign import getHours :: Duration -> Number
foreign import getMinutes :: Duration -> Number
foreign import getSeconds :: Duration -> Number
foreign import getMilliseconds :: Duration -> Number
foreign import getMicroseconds :: Duration -> Number
foreign import getNanoseconds :: Duration -> Number

-- | The sign of the duration: `1` for positive, `-1` for negative, `0` for blank.
foreign import sign :: Duration -> Int

-- | Whether all fields are zero.
foreign import blank :: Duration -> Boolean

-- Arithmetic

-- | Add two durations. Returns `Nothing` if the result would have mixed signs.
-- |
-- | When adding durations that contain calendar units (years, months, weeks),
-- | a `relativeTo` reference point is required to resolve the variable length
-- | of those units.
-- |
-- | ```purescript
-- | -- Time-only: no relativeTo needed
-- | add Nothing (hours 1.0) (hours 2.0)  -- Just PT3H
-- |
-- | -- Calendar units: provide a reference date
-- | add (Just (RelDate someDate)) (months 1.0) (months 2.0)  -- Just P3M
-- | ```
add :: Maybe RelativeTo -> Duration -> Duration -> Maybe Duration
add rel a b = addImpl Just Nothing (maybeRelativeToJS rel) a b

-- | Subtract one duration from another. Same caveats as `add` regarding
-- | calendar units and `relativeTo`.
subtract :: Maybe RelativeTo -> Duration -> Duration -> Maybe Duration
subtract rel a b = subtractImpl Just Nothing (maybeRelativeToJS rel) a b

-- | Return the duration with all component signs flipped.
-- | `negated (negated d) == d` for all durations.
foreign import negated :: Duration -> Duration

-- | Return the duration with all components made non-negative.
-- | `abs d == abs (negated d)` for all durations.
foreign import abs :: Duration -> Duration

-- Round/Total

-- | Options for `round`. The `roundingIncrement` must evenly divide the
-- | next-larger unit's maximum (e.g. for minutes: 1, 2, 3, 4, 5, 6, 10,
-- | 12, 15, 20, 30).
-- |
-- | When `relativeTo` is `Nothing`, rounding fails for durations with calendar
-- | units. Provide a `RelDate` or `RelDateTime` for calendar-aware rounding,
-- | or a `RelZoned` for DST-aware rounding.
type DurationRoundOptions =
  { largestUnit :: DateTimeUnit
  , smallestUnit :: DateTimeUnit
  , roundingIncrement :: Int
  , roundingMode :: RoundingMode
  , relativeTo :: Maybe RelativeTo
  }

defaultDurationRoundOptions :: DurationRoundOptions
defaultDurationRoundOptions =
  { largestUnit: TimeU Nanoseconds
  , smallestUnit: TimeU Nanoseconds
  , roundingIncrement: 1
  , roundingMode: HalfExpand
  , relativeTo: Nothing
  }

-- | Round and/or rebalance a duration. Returns `Nothing` if the operation
-- | fails (e.g. calendar units without `relativeTo`).
-- |
-- | To rebalance without rounding, set `largestUnit` to the desired largest
-- | unit:
-- |
-- | ```purescript
-- | -- Time-only rebalancing
-- | round (defaultDurationRoundOptions { largestUnit = TimeU Hours }) (minutes 90.0)
-- | -- Just PT1H30M
-- |
-- | -- Calendar-aware rebalancing
-- | round (defaultDurationRoundOptions
-- |   { largestUnit = DateU Years
-- |   , relativeTo = Just (RelDate someDate)
-- |   }) (months 18.0)
-- | -- Just P1Y6M
-- | ```
round :: DurationRoundOptions -> Duration -> Maybe Duration
round opts d = roundImpl Just Nothing
  { largestUnit: dateTimeUnitToString opts.largestUnit
  , smallestUnit: dateTimeUnitToString opts.smallestUnit
  , roundingIncrement: opts.roundingIncrement
  , roundingMode: roundingModeToString opts.roundingMode
  }
  (maybeRelativeToJS opts.relativeTo)
  d

-- | Options for `total`.
-- |
-- | When `relativeTo` is `Nothing`, computing the total fails for durations
-- | with calendar units. Provide a reference point to resolve calendar
-- | ambiguity.
type DurationTotalOptions =
  { unit :: DateTimeUnit
  , relativeTo :: Maybe RelativeTo
  }

defaultDurationTotalOptions :: DurationTotalOptions
defaultDurationTotalOptions =
  { unit: TimeU Nanoseconds
  , relativeTo: Nothing
  }

-- | Compute the total duration in the given unit as a fractional number.
-- | Returns `Nothing` if the operation fails (e.g. calendar units without
-- | `relativeTo`).
-- |
-- | ```purescript
-- | -- Time-only
-- | total { unit: TimeU Minutes, relativeTo: Nothing } (hours 1.0)
-- | -- Just 60.0
-- |
-- | -- Calendar-aware: "how many months is 45 days from Jan 1?"
-- | total { unit: DateU Months, relativeTo: Just (RelDate jan1) } (days 45.0)
-- | -- Just 1.5 (approximately)
-- | ```
total :: DurationTotalOptions -> Duration -> Maybe Number
total opts d = totalImpl Just Nothing
  (dateTimeUnitToString opts.unit)
  (maybeRelativeToJS opts.relativeTo)
  d


-- | Compare two durations relative to a reference point, returning their
-- | ordering.
-- |
-- | Unlike `Eq` (which compares component-by-component), `compare` resolves
-- | durations to actual elapsed time from the reference point. This means
-- | `compare ref (days 30.0) (months 1.0)` can determine which is longer
-- | for a specific starting date.
-- |
-- | A `relativeTo` is always required because even time-only durations are
-- | compared via the Temporal spec's `Duration.compare` static method.
-- |
-- | ```purescript
-- | compare (RelDate jan1) (days 31.0) (months 1.0)  -- EQ (January has 31 days)
-- | compare (RelDate feb1) (days 30.0) (months 1.0)  -- GT (February has 28 days)
-- | ```
compare :: RelativeTo -> Duration -> Duration -> Ordering
compare rel a b = case compareImpl (relativeToJS rel) a b of
  x | x < 0 -> LT
  x | x > 0 -> GT
  _ -> EQ

-- | Serialize to an ISO 8601 duration string (e.g. `"PT1H30M"`).
foreign import toString :: Duration -> String

-- Internal: RelativeTo JS conversion

-- | Opaque JS value representing either a Temporal date/datetime/zoneddatetime
-- | or null.
foreign import data RelativeToJS :: Type

foreign import noRelativeToJS :: RelativeToJS
foreign import relDateToJS :: PlainDate -> RelativeToJS
foreign import relDateTimeToJS :: PlainDateTime -> RelativeToJS
foreign import relZonedToJS :: ZonedDateTime -> RelativeToJS

relativeToJS :: RelativeTo -> RelativeToJS
relativeToJS (RelDate d) = relDateToJS d
relativeToJS (RelDateTime dt) = relDateTimeToJS dt
relativeToJS (RelZoned z) = relZonedToJS z

maybeRelativeToJS :: Maybe RelativeTo -> RelativeToJS
maybeRelativeToJS Nothing = noRelativeToJS
maybeRelativeToJS (Just rel) = relativeToJS rel

-- FFI imports
foreign import durationImpl :: (Duration -> Maybe Duration) -> Maybe Duration -> DurationFields -> Maybe Duration
foreign import fromStringImpl :: (Duration -> Maybe Duration) -> Maybe Duration -> String -> Maybe Duration
foreign import unsafeDuration :: DurationFields -> Duration
foreign import addImpl :: (Duration -> Maybe Duration) -> Maybe Duration -> RelativeToJS -> Duration -> Duration -> Maybe Duration
foreign import subtractImpl :: (Duration -> Maybe Duration) -> Maybe Duration -> RelativeToJS -> Duration -> Duration -> Maybe Duration
foreign import roundImpl :: (Duration -> Maybe Duration) -> Maybe Duration -> { largestUnit :: String, smallestUnit :: String, roundingIncrement :: Int, roundingMode :: String } -> RelativeToJS -> Duration -> Maybe Duration
foreign import totalImpl :: (Number -> Maybe Number) -> Maybe Number -> String -> RelativeToJS -> Duration -> Maybe Number
foreign import compareImpl :: RelativeToJS -> Duration -> Duration -> Int
