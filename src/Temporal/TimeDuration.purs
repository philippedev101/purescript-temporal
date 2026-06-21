-- | A time-only duration (hours through nanoseconds) with total `Semigroup`
-- | and `Monoid` instances.
-- |
-- | Unlike `Duration`, `TimeDuration` has no calendar components (years,
-- | months, weeks, days), so addition is always total — it never fails.
-- | This makes it suitable for use with `fold`, `foldMap`, and other
-- | PureScript combinators.
-- |
-- | Use `TimeDuration` for elapsed time, timers, time tracking, and
-- | aggregation.
module Temporal.TimeDuration
  ( TimeDuration(..)
  , fromDuration
  , toDuration
  , timeDuration
  , fromHours
  , fromMinutes
  , fromSeconds
  , fromMilliseconds
  , fromMicroseconds
  , fromNanoseconds
  , getHours
  , getMinutes
  , getSeconds
  , getMilliseconds
  , getMicroseconds
  , getNanoseconds
  , negate
  , abs
  , zero
  ) where

import Prelude hiding (negate, zero)
import Prelude as P

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Temporal.Duration as D
import Temporal.Duration.Extra as DX
import Temporal.Internal.Options (RelativeTo(..))
import Temporal.Internal.Types (Duration)
import Temporal.PlainDate as PD

-- | A duration restricted to time components only (hours, minutes, seconds,
-- | milliseconds, microseconds, nanoseconds). No calendar components.
-- |
-- | Addition is total: `TimeDuration` values can always be added without
-- | requiring a `relativeTo` reference point.
newtype TimeDuration = TimeDuration Duration

-- JSON: round-trips as the underlying Duration (ISO-8601 string).
instance encodeJsonTimeDuration :: EncodeJson TimeDuration where
  encodeJson (TimeDuration d) = encodeJson d
instance decodeJsonTimeDuration :: DecodeJson TimeDuration where
  decodeJson j = TimeDuration <$> decodeJson j

derive newtype instance eqTimeDuration :: Eq TimeDuration

instance showTimeDuration :: Show TimeDuration where
  show (TimeDuration d) = "(TimeDuration " <> D.toString d <> ")"

instance semigroupTimeDuration :: Semigroup TimeDuration where
  append (TimeDuration a) (TimeDuration b) =
    -- Use a fixed relativeTo so mixed-sign additions resolve correctly.
    -- Without relativeTo, Duration.add fails when the result has mixed
    -- signs (e.g. hours:1 + minutes:-30 → hours:1, minutes:-30 → error).
    -- With relativeTo, Temporal balances the result (→ PT30M).
    case D.add (Just (RelDate epoch)) a b of
      Just d -> TimeDuration d
      Nothing -> TimeDuration D.zero -- should never happen with relativeTo

instance monoidTimeDuration :: Monoid TimeDuration where
  mempty = TimeDuration D.zero

instance ordTimeDuration :: Ord TimeDuration where
  compare (TimeDuration a) (TimeDuration b) =
    case DX.totalUnit timeNano a, DX.totalUnit timeNano b of
      Just na, Just nb -> P.compare na nb
      _, _ -> EQ -- should not happen for time-only durations
    where
    timeNano = D.TimeU D.Nanoseconds

-- | Try to convert a `Duration` to a `TimeDuration`. Returns `Nothing` if
-- | the duration has non-zero calendar components (years, months, weeks, days).
fromDuration :: Duration -> Maybe TimeDuration
fromDuration d
  | D.getYears d /= 0.0 = Nothing
  | D.getMonths d /= 0.0 = Nothing
  | D.getWeeks d /= 0.0 = Nothing
  | D.getDays d /= 0.0 = Nothing
  | otherwise = Just (TimeDuration d)

-- | Convert back to a `Duration`. Always succeeds.
toDuration :: TimeDuration -> Duration
toDuration (TimeDuration d) = d

-- | Construct from individual time fields. Returns `Nothing` if signs are
-- | mixed.
timeDuration
  :: { hours :: Number
     , minutes :: Number
     , seconds :: Number
     , milliseconds :: Number
     , microseconds :: Number
     , nanoseconds :: Number
     }
  -> Maybe TimeDuration
timeDuration fields = case D.duration D.defaultDurationFields
  { hours = fields.hours
  , minutes = fields.minutes
  , seconds = fields.seconds
  , milliseconds = fields.milliseconds
  , microseconds = fields.microseconds
  , nanoseconds = fields.nanoseconds
  } of
  Just d -> Just (TimeDuration d)
  Nothing -> Nothing

-- | Create from hours.
fromHours :: Number -> TimeDuration
fromHours n = TimeDuration (D.hours n)

-- | Create from minutes.
fromMinutes :: Number -> TimeDuration
fromMinutes n = TimeDuration (D.minutes n)

-- | Create from seconds.
fromSeconds :: Number -> TimeDuration
fromSeconds n = TimeDuration (D.seconds n)

-- | Create from milliseconds.
fromMilliseconds :: Number -> TimeDuration
fromMilliseconds n = TimeDuration (D.milliseconds n)

-- | Create from microseconds.
fromMicroseconds :: Number -> TimeDuration
fromMicroseconds n = TimeDuration (D.microseconds n)

-- | Create from nanoseconds.
fromNanoseconds :: Number -> TimeDuration
fromNanoseconds n = TimeDuration (D.nanoseconds n)

-- Getters

getHours :: TimeDuration -> Number
getHours (TimeDuration d) = D.getHours d

getMinutes :: TimeDuration -> Number
getMinutes (TimeDuration d) = D.getMinutes d

getSeconds :: TimeDuration -> Number
getSeconds (TimeDuration d) = D.getSeconds d

getMilliseconds :: TimeDuration -> Number
getMilliseconds (TimeDuration d) = D.getMilliseconds d

getMicroseconds :: TimeDuration -> Number
getMicroseconds (TimeDuration d) = D.getMicroseconds d

getNanoseconds :: TimeDuration -> Number
getNanoseconds (TimeDuration d) = D.getNanoseconds d

-- | Negate the duration.
negate :: TimeDuration -> TimeDuration
negate (TimeDuration d) = TimeDuration (D.negated d)

-- | Absolute value.
abs :: TimeDuration -> TimeDuration
abs (TimeDuration d) = TimeDuration (D.abs d)

-- | The zero time duration. `mempty` alias.
zero :: TimeDuration
zero = mempty

-- Fixed reference point for Semigroup addition. Any valid date works since
-- we only use time-only durations.
epoch :: PD.PlainDate
epoch = unsafePartial $ fromJust $ PD.plainDate 1970 1 1
