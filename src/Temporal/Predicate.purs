-- | Readable boolean predicates for Temporal types.
-- |
-- | These are thin wrappers over `Eq`/`Ord` and component getters. They
-- | communicate intent better than raw comparison operators.
module Temporal.Predicate
  ( sameDay
  , sameMonth
  , sameYear
  , isBetweenDates
  , sameDayDateTime
  , isBetweenDateTimes
  , isBetweenInstants
  ) where

import Prelude

import Temporal.Internal.Types (Instant, PlainDate, PlainDateTime)
import Temporal.PlainDate as PD
import Temporal.PlainDateTime as PDT

-- | Whether two dates are the same calendar day.
sameDay :: PlainDate -> PlainDate -> Boolean
sameDay = eq

-- | Whether two dates are in the same month **and** year.
sameMonth :: PlainDate -> PlainDate -> Boolean
sameMonth a b = PD.getYear a == PD.getYear b && PD.getMonth a == PD.getMonth b

-- | Whether two dates are in the same calendar year.
sameYear :: PlainDate -> PlainDate -> Boolean
sameYear a b = PD.getYear a == PD.getYear b

-- | Whether a date falls between two bounds (inclusive on both ends).
-- |
-- | ```purescript
-- | isBetweenDates (plainDate 2024 3 15) (plainDate 2024 3 1) (plainDate 2024 3 31)
-- | ```
isBetweenDates :: PlainDate -> PlainDate -> PlainDate -> Boolean
isBetweenDates x lo hi = lo <= x && x <= hi

-- | Whether two datetimes fall on the same calendar day.
sameDayDateTime :: PlainDateTime -> PlainDateTime -> Boolean
sameDayDateTime a b =
  PDT.getYear a == PDT.getYear b
    && PDT.getMonth a == PDT.getMonth b
    && PDT.getDay a == PDT.getDay b

-- | Whether a datetime falls between two bounds (inclusive on both ends).
isBetweenDateTimes :: PlainDateTime -> PlainDateTime -> PlainDateTime -> Boolean
isBetweenDateTimes x lo hi = lo <= x && x <= hi

-- | Whether an instant falls between two bounds (inclusive on both ends).
isBetweenInstants :: Instant -> Instant -> Instant -> Boolean
isBetweenInstants x lo hi = lo <= x && x <= hi
