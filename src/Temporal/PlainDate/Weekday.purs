-- | Weekday utilities for `PlainDate`.
-- |
-- | Provides a `Weekday` ADT with full typeclass instances, weekday predicates,
-- | business day arithmetic, and ordinal weekday functions (e.g. "3rd Thursday
-- | of the month").
-- |
-- | All functions are pure PureScript — no FFI required.
module Temporal.PlainDate.Weekday
  ( Weekday(..)
  , getWeekday
  , isWeekday
  , isWeekend
  , addBusinessDays
  , nextWeekday
  , nextOrSameWeekday
  , previousWeekday
  , previousOrSameWeekday
  , dayOfWeekInSameWeek
  , nthWeekdayOfMonth
  , lastWeekdayOfMonth
  ) where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Temporal.Internal.Types (PlainDate)
import Temporal.Internal.Unsafe (unsafePlainDate, unsafeAddDays, unsafeSubtractDays)
import Temporal.PlainDate as PD

-- | Days of the week, ordered per ISO 8601 (Monday = 1, Sunday = 7).
data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

derive instance eqWeekday :: Eq Weekday
derive instance ordWeekday :: Ord Weekday
derive instance genericWeekday :: Generic Weekday _

instance encodeJsonWeekday :: EncodeJson Weekday where encodeJson = genericEncodeJson
instance decodeJsonWeekday :: DecodeJson Weekday where decodeJson = genericDecodeJson

instance showWeekday :: Show Weekday where
  show Monday = "Monday"
  show Tuesday = "Tuesday"
  show Wednesday = "Wednesday"
  show Thursday = "Thursday"
  show Friday = "Friday"
  show Saturday = "Saturday"
  show Sunday = "Sunday"

instance boundedWeekday :: Bounded Weekday where
  bottom = Monday
  top = Sunday

instance enumWeekday :: Enum Weekday where
  succ Monday = Just Tuesday
  succ Tuesday = Just Wednesday
  succ Wednesday = Just Thursday
  succ Thursday = Just Friday
  succ Friday = Just Saturday
  succ Saturday = Just Sunday
  succ Sunday = Nothing
  pred Monday = Nothing
  pred Tuesday = Just Monday
  pred Wednesday = Just Tuesday
  pred Thursday = Just Wednesday
  pred Friday = Just Thursday
  pred Saturday = Just Friday
  pred Sunday = Just Saturday

instance boundedEnumWeekday :: BoundedEnum Weekday where
  cardinality = Cardinality 7
  toEnum 1 = Just Monday
  toEnum 2 = Just Tuesday
  toEnum 3 = Just Wednesday
  toEnum 4 = Just Thursday
  toEnum 5 = Just Friday
  toEnum 6 = Just Saturday
  toEnum 7 = Just Sunday
  toEnum _ = Nothing
  fromEnum Monday = 1
  fromEnum Tuesday = 2
  fromEnum Wednesday = 3
  fromEnum Thursday = 4
  fromEnum Friday = 5
  fromEnum Saturday = 6
  fromEnum Sunday = 7

-- | Get the weekday of a date.
getWeekday :: PlainDate -> Weekday
getWeekday d = intToWeekday (PD.getDayOfWeek d)

-- | Whether the date falls on a weekday (Monday through Friday).
isWeekday :: PlainDate -> Boolean
isWeekday d = PD.getDayOfWeek d <= 5

-- | Whether the date falls on a weekend (Saturday or Sunday).
isWeekend :: PlainDate -> Boolean
isWeekend d = PD.getDayOfWeek d >= 6

-- | Add business days (Monday–Friday), skipping weekends. Supports negative
-- | values for going backward.
-- |
-- | ```purescript
-- | addBusinessDays 1 friday    == monday     -- skips weekend
-- | addBusinessDays (-1) monday == friday     -- skips weekend backward
-- | addBusinessDays 0 saturday  == saturday   -- identity
-- | addBusinessDays 6 monday    == tuesday    -- 5 days = 1 week, +1
-- | ```
addBusinessDays :: Int -> PlainDate -> PlainDate
addBusinessDays 0 d = d
addBusinessDays n d
  | n > 0 = addBusinessDaysForward n d
  | otherwise = addBusinessDaysBackward (negate n) d

-- | The next occurrence of the given weekday, always advancing at least 1 day.
-- |
-- | ```purescript
-- | nextWeekday Monday someMonday  == next Monday (7 days later)
-- | nextWeekday Wednesday someMonday == next Wednesday (2 days later)
-- | ```
nextWeekday :: Weekday -> PlainDate -> PlainDate
nextWeekday target d =
  let diff = weekdayDiff target d
      days = if diff == 0 then 7 else diff
  in unsafeAddDays days d

-- | The next occurrence of the given weekday, or the same date if it already
-- | matches.
nextOrSameWeekday :: Weekday -> PlainDate -> PlainDate
nextOrSameWeekday target d =
  let diff = weekdayDiff target d
  in if diff == 0 then d else unsafeAddDays diff d

-- | The previous occurrence of the given weekday, always retreating at least
-- | 1 day.
previousWeekday :: Weekday -> PlainDate -> PlainDate
previousWeekday target d =
  let diff = weekdayDiffBack target d
      days = if diff == 0 then 7 else diff
  in unsafeSubtractDays days d

-- | The previous occurrence of the given weekday, or the same date if it
-- | already matches.
previousOrSameWeekday :: Weekday -> PlainDate -> PlainDate
previousOrSameWeekday target d =
  let diff = weekdayDiffBack target d
  in if diff == 0 then d else unsafeSubtractDays diff d

-- | Find the given weekday within the same ISO week as the input date.
-- | The ISO week runs Monday through Sunday.
-- |
-- | ```purescript
-- | dayOfWeekInSameWeek Wednesday someMonday  == the Wednesday of that week
-- | dayOfWeekInSameWeek Monday someSunday     == the Monday of that week
-- | ```
dayOfWeekInSameWeek :: Weekday -> PlainDate -> PlainDate
dayOfWeekInSameWeek target d =
  let current = PD.getDayOfWeek d
      targetInt = weekdayToInt target
      diff = targetInt - current
  in if diff == 0 then d
     else if diff > 0 then unsafeAddDays diff d
     else unsafeSubtractDays (negate diff) d

-- | The nth occurrence of a weekday within the month containing the given
-- | date. Returns `Nothing` if the nth occurrence doesn't exist (e.g. there
-- | is no 5th Monday in most months).
-- |
-- | ```purescript
-- | -- Thanksgiving: 4th Thursday of November
-- | nthWeekdayOfMonth 4 Thursday (plainDate 2024 11 1)
-- |
-- | -- 1st Monday of the month
-- | nthWeekdayOfMonth 1 Monday (plainDate 2024 3 15)
-- | ```
nthWeekdayOfMonth :: Int -> Weekday -> PlainDate -> Maybe PlainDate
nthWeekdayOfMonth n target d
  | n < 1 = Nothing
  | otherwise =
      let firstOfMonth = unsafePlainDate (PD.getYear d) (PD.getMonth d) 1
          firstOccurrence = nextOrSameWeekday target firstOfMonth
          result = unsafeAddDays ((n - 1) * 7) firstOccurrence
      in if PD.getMonth result == PD.getMonth d
         then Just result
         else Nothing

-- | The last occurrence of a weekday within the month containing the given
-- | date.
-- |
-- | ```purescript
-- | lastWeekdayOfMonth Friday (plainDate 2024 3 15)  -- last Friday of March
-- | ```
lastWeekdayOfMonth :: Weekday -> PlainDate -> PlainDate
lastWeekdayOfMonth target d =
  let lastDay = unsafePlainDate (PD.getYear d) (PD.getMonth d) (PD.getDaysInMonth d)
  in previousOrSameWeekday target lastDay

-- Internal helpers

weekdayToInt :: Weekday -> Int
weekdayToInt Monday = 1
weekdayToInt Tuesday = 2
weekdayToInt Wednesday = 3
weekdayToInt Thursday = 4
weekdayToInt Friday = 5
weekdayToInt Saturday = 6
weekdayToInt Sunday = 7

intToWeekday :: Int -> Weekday
intToWeekday 1 = Monday
intToWeekday 2 = Tuesday
intToWeekday 3 = Wednesday
intToWeekday 4 = Thursday
intToWeekday 5 = Friday
intToWeekday 6 = Saturday
intToWeekday _ = Sunday

-- Days forward from current to target (0-6)
weekdayDiff :: Weekday -> PlainDate -> Int
weekdayDiff target d =
  let current = PD.getDayOfWeek d
      targetInt = weekdayToInt target
  in (targetInt - current + 7) `mod` 7

-- Days backward from current to target (0-6)
weekdayDiffBack :: Weekday -> PlainDate -> Int
weekdayDiffBack target d =
  let current = PD.getDayOfWeek d
      targetInt = weekdayToInt target
  in (current - targetInt + 7) `mod` 7

-- O(1) business day addition (forward)
addBusinessDaysForward :: Int -> PlainDate -> PlainDate
addBusinessDaysForward n d =
  let dow = PD.getDayOfWeek d -- 1=Mon..7=Sun
      -- If starting on weekend, advance to Monday first, then n-1 more
      -- If starting on weekday, advance n business days
  in if dow == 6 then -- Saturday: Monday is 1 business day away
       addBusinessDaysFromWeekday (n - 1) 1 (unsafeAddDays 2 d)
     else if dow == 7 then -- Sunday: Monday is 1 business day away
       addBusinessDaysFromWeekday (n - 1) 1 (unsafeAddDays 1 d)
     else
       addBusinessDaysFromWeekday n dow d

-- Add n business days starting from a known weekday (dow 1-5).
-- n >= 1. The starting date is NOT counted as one of the n days.
addBusinessDaysFromWeekday :: Int -> Int -> PlainDate -> PlainDate
addBusinessDaysFromWeekday n dow d =
  let fullWeeks = n / 5
      remainder = n `mod` 5
      -- After adding remainder business days from dow
      newDow = dow + remainder
      -- If we cross over the weekend (past Friday=5), add 2 calendar days
      weekendSkip = if newDow > 5 then 2 else 0
      totalDays = fullWeeks * 7 + remainder + weekendSkip
  in unsafeAddDays totalDays d

-- O(1) business day subtraction (backward, n is positive count)
addBusinessDaysBackward :: Int -> PlainDate -> PlainDate
addBusinessDaysBackward n d =
  let dow = PD.getDayOfWeek d
  in if dow == 6 then -- Saturday: Friday is 1 business day away
       subtractBusinessDaysFromWeekday (n - 1) 5 (unsafeSubtractDays 1 d)
     else if dow == 7 then -- Sunday: Friday is 1 business day away
       subtractBusinessDaysFromWeekday (n - 1) 5 (unsafeSubtractDays 2 d)
     else
       subtractBusinessDaysFromWeekday n dow d

-- Subtract n business days starting from a known weekday (dow 1-5).
-- n >= 1. The starting date is NOT counted as one of the n days.
subtractBusinessDaysFromWeekday :: Int -> Int -> PlainDate -> PlainDate
subtractBusinessDaysFromWeekday n dow d =
  let fullWeeks = n / 5
      remainder = n `mod` 5
      -- After subtracting remainder business days from dow
      newDow = dow - remainder
      -- If we cross over the weekend backward (past Monday=1), add 2 days
      weekendSkip = if newDow < 1 then 2 else 0
      totalDays = fullWeeks * 7 + remainder + weekendSkip
  in unsafeSubtractDays totalDays d
