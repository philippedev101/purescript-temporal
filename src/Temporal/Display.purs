-- | Timezone-aware display helpers.
-- |
-- | Convenience functions for converting instants to local representations
-- | and getting the current date/time in a specific timezone.
module Temporal.Display
  ( toLocal
  , toLocalDate
  , toLocalTime
  , todayIn
  , nowIn
  ) where

import Prelude

import Effect (Effect)
import Temporal.Instant as I
import Temporal.Internal.Types (Instant, PlainDate, PlainDateTime, PlainTime)
import Temporal.Now as Now
import Temporal.ZonedDateTime as ZDT

-- | Convert an `Instant` to a `PlainDateTime` in the given IANA time zone.
-- |
-- | Shorthand for `toPlainDateTime (toZonedDateTimeISO tz inst)`.
-- |
-- | ```purescript
-- | toLocal "America/New_York" instant  -- wall-clock time in New York
-- | toLocal "Asia/Tokyo" instant        -- wall-clock time in Tokyo
-- | ```
toLocal :: String -> Instant -> PlainDateTime
toLocal tz inst = ZDT.toPlainDateTime (I.toZonedDateTimeISO tz inst)

-- | Convert an `Instant` to a `PlainDate` in the given IANA time zone.
toLocalDate :: String -> Instant -> PlainDate
toLocalDate tz inst = ZDT.toPlainDate (I.toZonedDateTimeISO tz inst)

-- | Convert an `Instant` to a `PlainTime` in the given IANA time zone.
toLocalTime :: String -> Instant -> PlainTime
toLocalTime tz inst = ZDT.toPlainTime (I.toZonedDateTimeISO tz inst)

-- | The current date in the given IANA time zone.
-- |
-- | Alias for `Now.plainDateISOIn`.
todayIn :: String -> Effect PlainDate
todayIn = Now.plainDateISOIn

-- | The current date and time in the given IANA time zone.
-- |
-- | Alias for `Now.plainDateTimeISOIn`.
nowIn :: String -> Effect PlainDateTime
nowIn = Now.plainDateTimeISOIn
