-- | Option types used across Temporal operations.
-- |
-- | These ADTs represent the various option enums from the TC39 Temporal
-- | specification. They are converted to JavaScript strings before being passed
-- | to the FFI.
module Temporal.Internal.Options where

import Prelude

-- | Rounding modes matching TC39 Temporal specification.
-- |
-- | - `HalfExpand` (default) — round half toward positive infinity ("normal" rounding)
-- | - `Ceil` — round toward positive infinity
-- | - `Floor` — round toward negative infinity
-- | - `Trunc` — round toward zero
-- | - `Expand` — round away from zero
-- | - `HalfCeil` — round half toward positive infinity
-- | - `HalfFloor` — round half toward negative infinity
-- | - `HalfTrunc` — round half toward zero
-- | - `HalfEven` — round half to even (banker's rounding)
data RoundingMode
  = Ceil
  | Floor
  | Expand
  | Trunc
  | HalfCeil
  | HalfFloor
  | HalfExpand
  | HalfTrunc
  | HalfEven

derive instance eqRoundingMode :: Eq RoundingMode
derive instance ordRoundingMode :: Ord RoundingMode

instance showRoundingMode :: Show RoundingMode where
  show Ceil = "Ceil"
  show Floor = "Floor"
  show Expand = "Expand"
  show Trunc = "Trunc"
  show HalfCeil = "HalfCeil"
  show HalfFloor = "HalfFloor"
  show HalfExpand = "HalfExpand"
  show HalfTrunc = "HalfTrunc"
  show HalfEven = "HalfEven"

roundingModeToString :: RoundingMode -> String
roundingModeToString Ceil = "ceil"
roundingModeToString Floor = "floor"
roundingModeToString Expand = "expand"
roundingModeToString Trunc = "trunc"
roundingModeToString HalfCeil = "halfCeil"
roundingModeToString HalfFloor = "halfFloor"
roundingModeToString HalfExpand = "halfExpand"
roundingModeToString HalfTrunc = "halfTrunc"
roundingModeToString HalfEven = "halfEven"

-- | How to handle out-of-range field values (e.g. day 31 in a 30-day month).
-- |
-- | - `Constrain` — clamp to the nearest valid value (e.g. day 31 → day 30)
-- | - `Reject` — fail (return `Nothing`)
-- |
-- | Most Temporal constructors default to `Constrain`.
data Overflow = Constrain | Reject

derive instance eqOverflow :: Eq Overflow
derive instance ordOverflow :: Ord Overflow

instance showOverflow :: Show Overflow where
  show Constrain = "Constrain"
  show Reject = "Reject"

overflowToString :: Overflow -> String
overflowToString Constrain = "constrain"
overflowToString Reject = "reject"

-- | How to resolve wall-clock times that are ambiguous or non-existent due to
-- | DST transitions.
-- |
-- | - `Compatible` (default) — acts like `Later` for gaps (spring forward) and
-- |   `Earlier` for ambiguities (fall back). This matches the behavior of
-- |   legacy `Date`.
-- | - `Earlier` — pick the earlier of two possible instants
-- | - `Later` — pick the later of two possible instants
-- | - `RejectDisambiguation` — fail for ambiguous or non-existent times
data Disambiguation = Compatible | Earlier | Later | RejectDisambiguation

derive instance eqDisambiguation :: Eq Disambiguation
derive instance ordDisambiguation :: Ord Disambiguation

instance showDisambiguation :: Show Disambiguation where
  show Compatible = "Compatible"
  show Earlier = "Earlier"
  show Later = "Later"
  show RejectDisambiguation = "RejectDisambiguation"

disambiguationToString :: Disambiguation -> String
disambiguationToString Compatible = "compatible"
disambiguationToString Earlier = "earlier"
disambiguationToString Later = "later"
disambiguationToString RejectDisambiguation = "reject"

-- | How to resolve conflicts between an explicit UTC offset in a string and
-- | the IANA timezone's rules for that instant.
-- |
-- | - `Use` — trust the offset, ignore timezone rules
-- | - `Ignore` — trust the timezone, recompute the offset
-- | - `Prefer` — use the offset if it's valid for the timezone, otherwise
-- |   recompute (good for `with` operations)
-- | - `RejectOffset` — fail if offset and timezone disagree (good for parsing
-- |   untrusted input)
data OffsetDisambiguation = Use | Ignore | Prefer | RejectOffset

derive instance eqOffsetDisambiguation :: Eq OffsetDisambiguation
derive instance ordOffsetDisambiguation :: Ord OffsetDisambiguation

instance showOffsetDisambiguation :: Show OffsetDisambiguation where
  show Use = "Use"
  show Ignore = "Ignore"
  show Prefer = "Prefer"
  show RejectOffset = "RejectOffset"

offsetDisambiguationToString :: OffsetDisambiguation -> String
offsetDisambiguationToString Use = "use"
offsetDisambiguationToString Ignore = "ignore"
offsetDisambiguationToString Prefer = "prefer"
offsetDisambiguationToString RejectOffset = "reject"

-- | Direction for `getTimeZoneTransition` queries.
data TransitionDirection = Next | Previous

derive instance eqTransitionDirection :: Eq TransitionDirection
derive instance ordTransitionDirection :: Ord TransitionDirection

instance showTransitionDirection :: Show TransitionDirection where
  show Next = "Next"
  show Previous = "Previous"

transitionDirectionToString :: TransitionDirection -> String
transitionDirectionToString Next = "next"
transitionDirectionToString Previous = "previous"

-- | Units that apply only to date components.
data DateUnit = Years | Months | Weeks | Days

derive instance eqDateUnit :: Eq DateUnit
derive instance ordDateUnit :: Ord DateUnit

instance showDateUnit :: Show DateUnit where
  show Years = "Years"
  show Months = "Months"
  show Weeks = "Weeks"
  show Days = "Days"

dateUnitToString :: DateUnit -> String
dateUnitToString Years = "years"
dateUnitToString Months = "months"
dateUnitToString Weeks = "weeks"
dateUnitToString Days = "days"

-- | Units that apply only to time components.
data TimeUnit = Hours | Minutes | Seconds | Milliseconds | Microseconds | Nanoseconds

derive instance eqTimeUnit :: Eq TimeUnit
derive instance ordTimeUnit :: Ord TimeUnit

instance showTimeUnit :: Show TimeUnit where
  show Hours = "Hours"
  show Minutes = "Minutes"
  show Seconds = "Seconds"
  show Milliseconds = "Milliseconds"
  show Microseconds = "Microseconds"
  show Nanoseconds = "Nanoseconds"

timeUnitToString :: TimeUnit -> String
timeUnitToString Hours = "hours"
timeUnitToString Minutes = "minutes"
timeUnitToString Seconds = "seconds"
timeUnitToString Milliseconds = "milliseconds"
timeUnitToString Microseconds = "microseconds"
timeUnitToString Nanoseconds = "nanoseconds"

-- | Combined date+time units for operations that accept either.
data DateTimeUnit = DateU DateUnit | TimeU TimeUnit

derive instance eqDateTimeUnit :: Eq DateTimeUnit
derive instance ordDateTimeUnit :: Ord DateTimeUnit

instance showDateTimeUnit :: Show DateTimeUnit where
  show (DateU u) = "(DateU " <> show u <> ")"
  show (TimeU u) = "(TimeU " <> show u <> ")"

dateTimeUnitToString :: DateTimeUnit -> String
dateTimeUnitToString (DateU u) = dateUnitToString u
dateTimeUnitToString (TimeU u) = timeUnitToString u
