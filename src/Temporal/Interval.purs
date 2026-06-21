-- | Half-open intervals `[start, end)` with Allen's interval algebra.
-- |
-- | All intervals use half-open semantics: the start is included, the end is
-- | excluded. This is the standard convention for temporal intervals and
-- | composes cleanly — adjacent intervals share a boundary with no gap or
-- | overlap.
-- |
-- | For date ranges, use `[Jan 1, Feb 1)` to mean "all of January" rather
-- | than the closed `[Jan 1, Jan 31]`.
module Temporal.Interval
  ( Interval(..)
  , interval
  , unsafeInterval
  , start
  , end
  , Relation(..)
  , relate
  , contains
  , overlaps
  , encloses
  , abuts
  , isBefore
  , isAfter
  , intersection
  , union
  , hull
  , gap
  , difference
  ) where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.:))
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))

-- | A half-open interval `[start, end)`. The start is included, the end is
-- | excluded. An interval where `start >= end` is considered empty/invalid
-- | and cannot be constructed via `interval`.
newtype Interval a = Interval { start :: a, end :: a }

-- JSON: an interval round-trips as `{ "start": ..., "end": ... }`.
instance encodeJsonInterval :: EncodeJson a => EncodeJson (Interval a) where
  encodeJson (Interval r) = encodeJson r
-- Decode the fields explicitly (via `.:`) rather than through the generic
-- record decoder, which needs `DecodeJsonField a` and overlaps with the
-- `Maybe` field instance for a rigid type variable.
instance decodeJsonInterval :: DecodeJson a => DecodeJson (Interval a) where
  decodeJson j = do
    obj <- decodeJson j
    s <- obj .: "start"
    e <- obj .: "end"
    pure (Interval { start: s, end: e })

derive newtype instance eqInterval :: Eq a => Eq (Interval a)

instance showInterval :: Show a => Show (Interval a) where
  show (Interval { start: s, end: e }) = "[" <> show s <> ", " <> show e <> ")"

-- | Construct an interval. Returns `Nothing` if `start >= end` (which would
-- | be empty in half-open semantics).
interval :: forall a. Ord a => a -> a -> Maybe (Interval a)
interval s e
  | s < e = Just (Interval { start: s, end: e })
  | otherwise = Nothing

-- | Construct an interval without validation. The caller must ensure
-- | `start < end`.
unsafeInterval :: forall a. a -> a -> Interval a
unsafeInterval s e = Interval { start: s, end: e }

-- | The inclusive start of the interval.
start :: forall a. Interval a -> a
start (Interval r) = r.start

-- | The exclusive end of the interval.
end :: forall a. Interval a -> a
end (Interval r) = r.end

-- | Allen's 13 interval relations. Any two non-empty intervals are in exactly
-- | one of these relations.
-- |
-- | Given intervals X and Y:
-- |
-- | ```
-- | Before       : X entirely before Y (gap between)
-- | Meets        : X.end == Y.start (adjacent, X first)
-- | Overlaps     : X starts first, X.end inside Y
-- | Starts       : same start, X ends before Y
-- | During       : X fully inside Y
-- | Finishes     : X starts after Y.start, same end
-- | Equals       : identical start and end
-- | FinishedBy   : inverse of Finishes
-- | Contains     : inverse of During
-- | StartedBy    : inverse of Starts
-- | OverlappedBy : inverse of Overlaps
-- | MetBy        : inverse of Meets
-- | After        : inverse of Before
-- | ```
data Relation
  = Before
  | Meets
  | Overlaps
  | Starts
  | During
  | Finishes
  | Equals
  | FinishedBy
  | Contains
  | StartedBy
  | OverlappedBy
  | MetBy
  | After

derive instance eqRelation :: Eq Relation
derive instance ordRelation :: Ord Relation
derive instance genericRelation :: Generic Relation _

instance encodeJsonRelation :: EncodeJson Relation where encodeJson = genericEncodeJson
instance decodeJsonRelation :: DecodeJson Relation where decodeJson = genericDecodeJson

instance showRelation :: Show Relation where
  show Before = "Before"
  show Meets = "Meets"
  show Overlaps = "Overlaps"
  show Starts = "Starts"
  show During = "During"
  show Finishes = "Finishes"
  show Equals = "Equals"
  show FinishedBy = "FinishedBy"
  show Contains = "Contains"
  show StartedBy = "StartedBy"
  show OverlappedBy = "OverlappedBy"
  show MetBy = "MetBy"
  show After = "After"

-- | Determine the Allen relation between two intervals.
-- |
-- | ```purescript
-- | relate [1, 3) [5, 7) == Before
-- | relate [1, 5) [3, 7) == Overlaps
-- | relate [1, 7) [3, 5) == Contains
-- | ```
relate :: forall a. Ord a => Interval a -> Interval a -> Relation
relate (Interval x) (Interval y) =
  case compare x.start y.start, compare x.end y.end of
    LT, LT -> if x.end <= y.start then
                 if x.end == y.start then Meets else Before
               else Overlaps
    LT, EQ -> FinishedBy
    LT, GT -> Contains
    EQ, LT -> Starts
    EQ, EQ -> Equals
    EQ, GT -> StartedBy
    GT, LT -> if x.start >= y.end then
                 if x.start == y.end then MetBy else After
               else During
    GT, EQ -> Finishes
    GT, GT -> if x.start >= y.end then
                 if x.start == y.end then MetBy else After
               else OverlappedBy

-- | Whether a point falls within the interval (half-open: `start <= x < end`).
contains :: forall a. Ord a => a -> Interval a -> Boolean
contains x (Interval r) = r.start <= x && x < r.end

-- | Whether two intervals share any time (colloquial "overlaps").
-- | Adjacent intervals (`Meets`/`MetBy`) do NOT overlap in half-open semantics.
overlaps :: forall a. Ord a => Interval a -> Interval a -> Boolean
overlaps a b = case relate a b of
  Before -> false
  Meets -> false
  MetBy -> false
  After -> false
  _ -> true

-- | Whether the first interval fully contains the second.
encloses :: forall a. Ord a => Interval a -> Interval a -> Boolean
encloses a b = case relate a b of
  Contains -> true
  StartedBy -> true
  FinishedBy -> true
  Equals -> true
  _ -> false

-- | Whether two intervals are adjacent with no gap (one's end is the other's
-- | start). Adjacent half-open intervals compose cleanly:
-- | `[a, b)` abuts `[b, c)`.
abuts :: forall a. Ord a => Interval a -> Interval a -> Boolean
abuts a b = case relate a b of
  Meets -> true
  MetBy -> true
  _ -> false

-- | Whether the first interval is entirely before the second (with or without
-- | gap).
isBefore :: forall a. Ord a => Interval a -> Interval a -> Boolean
isBefore a b = case relate a b of
  Before -> true
  Meets -> true
  _ -> false

-- | Whether the first interval is entirely after the second (with or without
-- | gap).
isAfter :: forall a. Ord a => Interval a -> Interval a -> Boolean
isAfter a b = case relate a b of
  After -> true
  MetBy -> true
  _ -> false

-- | The overlapping portion of two intervals, if any.
intersection :: forall a. Ord a => Interval a -> Interval a -> Maybe (Interval a)
intersection a b =
  let s = max (start a) (start b)
      e = min (end a) (end b)
  in if s < e then Just (Interval { start: s, end: e }) else Nothing

-- | Merge two intervals into one. Returns `Nothing` if the intervals neither
-- | overlap nor are adjacent (i.e., there is a gap between them).
union :: forall a. Ord a => Interval a -> Interval a -> Maybe (Interval a)
union a b =
  if overlaps a b || abuts a b
    then Just (Interval { start: min (start a) (start b), end: max (end a) (end b) })
    else Nothing

-- | The smallest interval that encloses both inputs, ignoring any gap.
-- | Always succeeds.
-- |
-- | ```purescript
-- | hull [1, 3) [5, 7) == [1, 7)  -- includes the gap
-- | ```
hull :: forall a. Ord a => Interval a -> Interval a -> Interval a
hull a b = Interval { start: min (start a) (start b), end: max (end a) (end b) }

-- | The gap between two non-overlapping intervals, if any.
-- | Returns `Nothing` if the intervals overlap or are adjacent.
gap :: forall a. Ord a => Interval a -> Interval a -> Maybe (Interval a)
gap a b =
  let s = min (end a) (end b)
      e = max (start a) (start b)
  in if s < e then Just (Interval { start: s, end: e }) else Nothing

-- | The parts of the first interval not in the second. Returns 0, 1, or 2
-- | intervals.
-- |
-- | ```purescript
-- | difference [1, 10) [3, 7) == [ [1, 3), [7, 10) ]  -- hole punched
-- | difference [1, 5) [3, 7)  == [ [1, 3) ]            -- trimmed
-- | difference [5, 7) [1, 10) == []                     -- fully consumed
-- | ```
difference :: forall a. Ord a => Interval a -> Interval a -> Array (Interval a)
difference a b =
  if not (overlaps a b) then [ a ]
  else
    let left = if start a < start b
          then interval (start a) (start b)
          else Nothing
        right = if end b < end a
          then interval (end b) (end a)
          else Nothing
    in case left, right of
      Just l, Just r -> [ l, r ]
      Just l, Nothing -> [ l ]
      Nothing, Just r -> [ r ]
      Nothing, Nothing -> []
