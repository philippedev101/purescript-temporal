-- | Internal module defining foreign Temporal types with their core typeclass
-- | instances. These types are re-exported from their respective modules
-- | (e.g. `Temporal.Duration`, `Temporal.Instant`, etc.).
module Temporal.Internal.Types where

import Prelude

-- | A length of time expressed as a combination of date and time components
-- | (years, months, weeks, days, hours, minutes, seconds, milliseconds,
-- | microseconds, nanoseconds). All components must share the same sign.
-- |
-- | Durations are **unbalanced** — `{ hours: 36 }` is not automatically
-- | converted to `{ days: 1, hours: 12 }`. Use `round` to balance.
-- |
-- | `Eq` compares component-by-component (not total elapsed time), so
-- | `{ hours: 1 }` is not equal to `{ minutes: 60 }`.
foreign import data Duration :: Type

-- | An exact moment on the UTC timeline with nanosecond precision,
-- | independent of any time zone or calendar.
-- |
-- | Use `Instant` for timestamps, event logs, and any case where you need
-- | an unambiguous point in time. Convert to `ZonedDateTime` when you need
-- | wall-clock representation in a specific time zone.
foreign import data Instant :: Type

-- | A calendar date without a time component or time zone (e.g. 2024-03-15).
-- |
-- | Use `PlainDate` for birthdays, holidays, and other dates where time of day
-- | is irrelevant. Do not use for scheduling events — use `ZonedDateTime` instead.
foreign import data PlainDate :: Type

-- | A wall-clock time without a date or time zone (e.g. 10:30:00).
-- | Range: 00:00:00.000000000 to 23:59:59.999999999.
-- |
-- | Arithmetic wraps at midnight with no overflow indication.
foreign import data PlainTime :: Type

-- | A calendar date and wall-clock time without a time zone
-- | (e.g. 2024-03-15T10:30:00).
-- |
-- | Use `PlainDateTime` for events whose time zone is tracked separately,
-- | or for wall-clock displays. For unambiguous moments in time, use `Instant`
-- | or `ZonedDateTime` instead.
foreign import data PlainDateTime :: Type

-- | A year and month without a day component (e.g. 2024-03).
-- |
-- | Useful for representing months in a calendar UI or credit card expiry dates.
foreign import data PlainYearMonth :: Type

-- | A month and day without a year component (e.g. 12-25).
-- |
-- | Useful for recurring annual dates like birthdays or holidays.
foreign import data PlainMonthDay :: Type

-- | A date and time in a specific time zone, with full awareness of UTC offset
-- | and DST transitions (e.g. 2024-03-15T10:30:00-04:00[America/New_York]).
-- |
-- | This is the richest Temporal type. Use it for scheduling future events
-- | where wall-clock time should be preserved even if DST rules change.
-- | For past events or timestamps, `Instant` is usually sufficient.
foreign import data ZonedDateTime :: Type

-- Duration instances
foreign import durationEquals :: Duration -> Duration -> Boolean
foreign import durationToString :: Duration -> String

instance eqDuration :: Eq Duration where
  eq = durationEquals

instance showDuration :: Show Duration where
  show d = "(Duration " <> durationToString d <> ")"

-- Instant instances
foreign import instantCompare :: Instant -> Instant -> Int
foreign import instantToString :: Instant -> String

instance eqInstant :: Eq Instant where
  eq a b = instantCompare a b == 0

instance ordInstant :: Ord Instant where
  compare a b = case instantCompare a b of
    x | x < 0 -> LT
    x | x > 0 -> GT
    _ -> EQ

instance showInstant :: Show Instant where
  show i = "(Instant " <> instantToString i <> ")"

-- PlainDate instances
foreign import plainDateCompare :: PlainDate -> PlainDate -> Int
foreign import plainDateToString :: PlainDate -> String

instance eqPlainDate :: Eq PlainDate where
  eq a b = plainDateCompare a b == 0

instance ordPlainDate :: Ord PlainDate where
  compare a b = case plainDateCompare a b of
    x | x < 0 -> LT
    x | x > 0 -> GT
    _ -> EQ

instance showPlainDate :: Show PlainDate where
  show d = "(PlainDate " <> plainDateToString d <> ")"

-- PlainTime instances
foreign import plainTimeCompare :: PlainTime -> PlainTime -> Int
foreign import plainTimeToString :: PlainTime -> String

instance eqPlainTime :: Eq PlainTime where
  eq a b = plainTimeCompare a b == 0

instance ordPlainTime :: Ord PlainTime where
  compare a b = case plainTimeCompare a b of
    x | x < 0 -> LT
    x | x > 0 -> GT
    _ -> EQ

instance showPlainTime :: Show PlainTime where
  show t = "(PlainTime " <> plainTimeToString t <> ")"

instance boundedPlainTime :: Bounded PlainTime where
  bottom = unsafePlainTimeBottom
  top = unsafePlainTimeTop

foreign import unsafePlainTimeBottom :: PlainTime
foreign import unsafePlainTimeTop :: PlainTime

-- PlainDateTime instances
foreign import plainDateTimeCompare :: PlainDateTime -> PlainDateTime -> Int
foreign import plainDateTimeToString :: PlainDateTime -> String

instance eqPlainDateTime :: Eq PlainDateTime where
  eq a b = plainDateTimeCompare a b == 0

instance ordPlainDateTime :: Ord PlainDateTime where
  compare a b = case plainDateTimeCompare a b of
    x | x < 0 -> LT
    x | x > 0 -> GT
    _ -> EQ

instance showPlainDateTime :: Show PlainDateTime where
  show dt = "(PlainDateTime " <> plainDateTimeToString dt <> ")"

-- PlainYearMonth instances
foreign import plainYearMonthCompare :: PlainYearMonth -> PlainYearMonth -> Int
foreign import plainYearMonthToString :: PlainYearMonth -> String

instance eqPlainYearMonth :: Eq PlainYearMonth where
  eq a b = plainYearMonthCompare a b == 0

instance ordPlainYearMonth :: Ord PlainYearMonth where
  compare a b = case plainYearMonthCompare a b of
    x | x < 0 -> LT
    x | x > 0 -> GT
    _ -> EQ

instance showPlainYearMonth :: Show PlainYearMonth where
  show ym = "(PlainYearMonth " <> plainYearMonthToString ym <> ")"

-- PlainMonthDay instances
foreign import plainMonthDayEquals :: PlainMonthDay -> PlainMonthDay -> Boolean
foreign import plainMonthDayToString :: PlainMonthDay -> String

instance eqPlainMonthDay :: Eq PlainMonthDay where
  eq = plainMonthDayEquals

instance showPlainMonthDay :: Show PlainMonthDay where
  show md = "(PlainMonthDay " <> plainMonthDayToString md <> ")"

-- ZonedDateTime instances
foreign import zonedDateTimeCompare :: ZonedDateTime -> ZonedDateTime -> Int
foreign import zonedDateTimeToString :: ZonedDateTime -> String

instance eqZonedDateTime :: Eq ZonedDateTime where
  eq a b = zonedDateTimeCompare a b == 0

instance ordZonedDateTime :: Ord ZonedDateTime where
  compare a b = case zonedDateTimeCompare a b of
    x | x < 0 -> LT
    x | x > 0 -> GT
    _ -> EQ

instance showZonedDateTime :: Show ZonedDateTime where
  show zdt = "(ZonedDateTime " <> zonedDateTimeToString zdt <> ")"
