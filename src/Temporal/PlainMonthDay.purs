-- | Construction and inspection of `PlainMonthDay` values.
-- |
-- | A `PlainMonthDay` represents a month and day without a year component
-- | (e.g. 12-25). Useful for recurring annual dates like birthdays or holidays.
-- |
-- | Note: `PlainMonthDay` has `Eq` but not `Ord`, because ordering month-day
-- | pairs without a year is ambiguous (leap day considerations).
module Temporal.PlainMonthDay
  ( module Temporal.Internal.Types
  , plainMonthDay
  , fromString
  , getMonthCode
  , getDay
  , with
  , toPlainDate
  , toString
  ) where

import Data.Maybe (Maybe(..))
import Temporal.Internal.Types (PlainDate, PlainMonthDay)

-- | Construct from month (1–12) and day (1–31). Returns `Nothing` for
-- | invalid combinations (e.g. month 2, day 30).
plainMonthDay :: Int -> Int -> Maybe PlainMonthDay
plainMonthDay m d = plainMonthDayImpl Just Nothing m d

-- | Parse an ISO 8601 month-day string (e.g. `"--12-25"`).
-- | Returns `Nothing` for invalid strings.
fromString :: String -> Maybe PlainMonthDay
fromString s = fromStringImpl Just Nothing s

-- Properties

-- | Month code string (e.g. `"M12"` for December).
foreign import getMonthCode :: PlainMonthDay -> String

-- | Day of the month (1–31).
foreign import getDay :: PlainMonthDay -> Int

-- | Create a modified copy with the given month and day. Returns `Nothing`
-- | if the result would be invalid.
with :: { month :: Int, day :: Int } -> PlainMonthDay -> Maybe PlainMonthDay
with fields pmd = withImpl Just Nothing fields pmd

-- | Convert to a `PlainDate` by specifying a year. Out-of-range days are
-- | **constrained** — e.g. Feb 29 in a non-leap year yields Feb 28.
-- | Returns `Nothing` for truly invalid values.
toPlainDate :: Int -> PlainMonthDay -> Maybe PlainDate
toPlainDate year pmd = toPlainDateImpl Just Nothing year pmd

-- | Serialize to an ISO 8601 month-day string (e.g. `"--12-25"`).
foreign import toString :: PlainMonthDay -> String

-- FFI imports
foreign import plainMonthDayImpl :: (PlainMonthDay -> Maybe PlainMonthDay) -> Maybe PlainMonthDay -> Int -> Int -> Maybe PlainMonthDay
foreign import fromStringImpl :: (PlainMonthDay -> Maybe PlainMonthDay) -> Maybe PlainMonthDay -> String -> Maybe PlainMonthDay
foreign import withImpl :: (PlainMonthDay -> Maybe PlainMonthDay) -> Maybe PlainMonthDay -> { month :: Int, day :: Int } -> PlainMonthDay -> Maybe PlainMonthDay
foreign import toPlainDateImpl :: (PlainDate -> Maybe PlainDate) -> Maybe PlainDate -> Int -> PlainMonthDay -> Maybe PlainDate
