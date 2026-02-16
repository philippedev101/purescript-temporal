-- | Access the current date, time, and time zone from the system clock.
-- |
-- | All functions in this module perform effects (reading the system clock).
-- | Functions without an `In` suffix use the system's default time zone.
-- | Functions with an `In` suffix take an IANA time zone name.
module Temporal.Now
  ( instant
  , zonedDateTimeISO
  , zonedDateTimeISOIn
  , plainDateTimeISO
  , plainDateTimeISOIn
  , plainDateISO
  , plainDateISOIn
  , plainTimeISO
  , plainTimeISOIn
  , timeZoneId
  ) where

import Effect (Effect)
import Temporal.Internal.Types (Instant, PlainDate, PlainDateTime, PlainTime, ZonedDateTime)

-- | The current instant (exact moment on the UTC timeline).
foreign import instant :: Effect Instant

-- | The current date and time in the system's default time zone.
foreign import zonedDateTimeISO :: Effect ZonedDateTime

-- | The current date and time in the given IANA time zone.
foreign import zonedDateTimeISOIn :: String -> Effect ZonedDateTime

-- | The current date and time (no timezone) in the system's default time zone.
foreign import plainDateTimeISO :: Effect PlainDateTime

-- | The current date and time (no timezone) in the given IANA time zone.
foreign import plainDateTimeISOIn :: String -> Effect PlainDateTime

-- | The current date in the system's default time zone.
foreign import plainDateISO :: Effect PlainDate

-- | The current date in the given IANA time zone.
foreign import plainDateISOIn :: String -> Effect PlainDate

-- | The current time in the system's default time zone.
foreign import plainTimeISO :: Effect PlainTime

-- | The current time in the given IANA time zone.
foreign import plainTimeISOIn :: String -> Effect PlainTime

-- | The system's default IANA time zone identifier (e.g. `"America/New_York"`).
foreign import timeZoneId :: Effect String
