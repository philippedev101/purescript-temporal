-- | Sorted, non-overlapping interval collections.
-- |
-- | An `IntervalSet` maintains the invariant that its intervals are sorted by
-- | start, non-overlapping, non-adjacent, and non-empty. All operations
-- | preserve this invariant.
-- |
-- | Use `IntervalSet` for scheduling, calendar free/busy queries, and timeline
-- | operations.
module Temporal.Interval.Set
  ( IntervalSet
  , empty
  , singleton
  , fromIntervals
  , toArray
  , isEmpty
  , span
  , member
  , findContaining
  , gaps
  , insert
  , remove
  , unionSets
  , intersectSets
  , differenceSets
  , complement
  ) where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Temporal.Interval (Interval, start, end, unsafeInterval)
import Temporal.Interval as I

-- | A sorted collection of non-overlapping, non-adjacent, non-empty
-- | half-open intervals.
newtype IntervalSet a = IntervalSet (Array (Interval a))

-- JSON: round-trips as a JSON array of intervals.
instance encodeJsonIntervalSet :: EncodeJson a => EncodeJson (IntervalSet a) where
  encodeJson (IntervalSet xs) = encodeJson xs
instance decodeJsonIntervalSet :: DecodeJson a => DecodeJson (IntervalSet a) where
  decodeJson j = IntervalSet <$> decodeJson j

derive newtype instance eqIntervalSet :: Eq a => Eq (IntervalSet a)

instance showIntervalSet :: Show a => Show (IntervalSet a) where
  show (IntervalSet arr) = "IntervalSet " <> show arr

-- | The empty interval set.
empty :: forall a. IntervalSet a
empty = IntervalSet []

-- | An interval set containing a single interval.
singleton :: forall a. Interval a -> IntervalSet a
singleton i = IntervalSet [ i ]

-- | Whether the set contains no intervals.
isEmpty :: forall a. IntervalSet a -> Boolean
isEmpty (IntervalSet arr) = A.null arr

-- | Build an interval set from an array of intervals, merging any that
-- | overlap or are adjacent. O(n log n).
fromIntervals :: forall a. Ord a => Array (Interval a) -> IntervalSet a
fromIntervals arr =
  let sorted = A.sortBy (\a b -> compare (start a) (start b)) arr
  in IntervalSet (mergeAdjacent sorted)

-- | Extract the sorted array of non-overlapping intervals.
toArray :: forall a. IntervalSet a -> Array (Interval a)
toArray (IntervalSet arr) = arr

-- | The bounding interval (from the earliest start to the latest end).
-- | Returns `Nothing` for an empty set.
span :: forall a. Ord a => IntervalSet a -> Maybe (Interval a)
span (IntervalSet arr) = case A.head arr, A.last arr of
  Just first, Just lst -> Just (unsafeInterval (start first) (end lst))
  _, _ -> Nothing

-- | Whether a point is contained in any interval in the set. O(log n).
member :: forall a. Ord a => a -> IntervalSet a -> Boolean
member x (IntervalSet arr) =
  case A.index arr (binarySearchLE x arr) of
    Just iv -> I.contains x iv
    Nothing -> false

-- | Find the interval containing the given point, if any. O(log n).
findContaining :: forall a. Ord a => a -> IntervalSet a -> Maybe (Interval a)
findContaining x (IntervalSet arr) =
  case A.index arr (binarySearchLE x arr) of
    Just iv | I.contains x iv -> Just iv
    _ -> Nothing

-- | Insert an interval into the set, merging with any overlapping or adjacent
-- | existing intervals. O(n).
insert :: forall a. Ord a => Interval a -> IntervalSet a -> IntervalSet a
insert i (IntervalSet arr) =
  let { before, merged, after } = go i arr
  in IntervalSet (before <> [ merged ] <> after)
  where
  go :: Interval a -> Array (Interval a) -> { before :: Array (Interval a), merged :: Interval a, after :: Array (Interval a) }
  go cur intervals = case A.uncons intervals of
    Nothing -> { before: [], merged: cur, after: [] }
    Just { head: h, tail } ->
      if I.isBefore cur h && not (I.abuts cur h)
        then { before: [], merged: cur, after: intervals }
      else case I.union cur h of
        Just merged -> go merged tail
        -- cur is After h (the only remaining case): skip h and continue
        Nothing ->
          let rest = go cur tail
          in rest { before = [ h ] <> rest.before }

-- | Remove an interval from the set, punching holes as needed. O(n).
remove :: forall a. Ord a => Interval a -> IntervalSet a -> IntervalSet a
remove i (IntervalSet arr) = IntervalSet (A.concatMap (\x -> I.difference x i) arr)

-- | The gaps (free time) between intervals within the set's span.
-- | Returns an empty set if the set has fewer than 2 intervals.
gaps :: forall a. Ord a => IntervalSet a -> IntervalSet a
gaps (IntervalSet arr) = IntervalSet (A.mapMaybe identity gapPairs)
  where
  gapPairs = A.zipWith I.gap arr (A.drop 1 arr)

-- | Union of two interval sets. O(n + m).
unionSets :: forall a. Ord a => IntervalSet a -> IntervalSet a -> IntervalSet a
unionSets (IntervalSet as) (IntervalSet bs) =
  IntervalSet (mergeAdjacent (mergeSortedArrays as bs))

-- | Intersection of two interval sets — the time present in both. O(n + m).
intersectSets :: forall a. Ord a => IntervalSet a -> IntervalSet a -> IntervalSet a
intersectSets (IntervalSet as) (IntervalSet bs) =
  IntervalSet (sweepIntersect as bs [])

-- | Difference of two interval sets — time in the first but not the
-- | second. O(n + m).
differenceSets :: forall a. Ord a => IntervalSet a -> IntervalSet a -> IntervalSet a
differenceSets (IntervalSet as) (IntervalSet bs) =
  IntervalSet (sweepDifference as bs [])

-- | The complement of an interval set within a bounding interval — the gaps
-- | relative to the bound.
-- |
-- | ```purescript
-- | complement [0, 100) { [10, 20), [50, 60) }
-- | -- { [0, 10), [20, 50), [60, 100) }
-- | ```
complement :: forall a. Ord a => Interval a -> IntervalSet a -> IntervalSet a
complement bound set = differenceSets (singleton bound) set

-- Internal helpers

-- | Merge a sorted array of intervals, combining overlapping and adjacent
-- | entries into single intervals.
mergeAdjacent :: forall a. Ord a => Array (Interval a) -> Array (Interval a)
mergeAdjacent arr = case A.uncons arr of
  Nothing -> []
  Just { head: first, tail } ->
    let { current, result } = A.foldl step { current: first, result: [] } tail
    in A.snoc result current
  where
  step { current, result } next =
    case I.union current next of
      Just merged -> { current: merged, result }
      Nothing -> { current: next, result: A.snoc result current }

-- | Merge two sorted interval arrays into one sorted array. O(n + m).
mergeSortedArrays :: forall a. Ord a => Array (Interval a) -> Array (Interval a) -> Array (Interval a)
mergeSortedArrays xs ys = go xs ys []
  where
  go as' bs' acc = case A.uncons as', A.uncons bs' of
    Just { head: a, tail: at }, Just { head: b, tail: bt } ->
      if start a <= start b
        then go at bs' (A.snoc acc a)
        else go as' bt (A.snoc acc b)
    Just _, Nothing -> acc <> as'
    Nothing, _ -> acc <> bs'

-- | Binary search for the index of the last interval whose start <= x.
-- | Returns -1 if no such interval exists.
binarySearchLE :: forall a. Ord a => a -> Array (Interval a) -> Int
binarySearchLE x arr = go 0 (A.length arr - 1) (-1)
  where
  go lo hi result
    | lo > hi = result
    | otherwise =
        let mid = lo + (hi - lo) / 2
        in case A.index arr mid of
          Nothing -> result
          Just iv
            | start iv <= x -> go (mid + 1) hi mid
            | otherwise -> go lo (mid - 1) result

-- | Sweep-line intersection of two sorted, non-overlapping interval arrays.
-- | O(n + m).
sweepIntersect :: forall a. Ord a => Array (Interval a) -> Array (Interval a) -> Array (Interval a) -> Array (Interval a)
sweepIntersect xs ys acc = case A.uncons xs, A.uncons ys of
  Just { head: x, tail: xs' }, Just { head: y, tail: ys' } ->
    let newAcc = case I.intersection x y of
          Just isect -> A.snoc acc isect
          Nothing -> acc
    in if end x <= end y
         then sweepIntersect xs' ys newAcc
         else sweepIntersect xs ys' newAcc
  _, _ -> acc

-- | Sweep-line difference: intervals in the first array minus the second.
-- | Both arrays must be sorted and non-overlapping. O(n + m).
sweepDifference :: forall a. Ord a => Array (Interval a) -> Array (Interval a) -> Array (Interval a) -> Array (Interval a)
sweepDifference xs ys acc = case A.uncons xs of
  Nothing -> acc
  Just { head: x, tail: xs' } -> case A.uncons ys of
    -- No more removals: keep all remaining
    Nothing -> acc <> xs
    Just { head: y, tail: ys' }
      -- x entirely before y: keep x, advance x
      | end x <= start y -> sweepDifference xs' ys (A.snoc acc x)
      -- y entirely before x: skip y, advance y
      | end y <= start x -> sweepDifference xs ys' acc
      -- y starts at or before x
      | start y <= start x ->
          if end y >= end x
            -- y fully covers x: discard x
            then sweepDifference xs' ys acc
            -- y covers left part of x: trim x, advance y
            else sweepDifference (A.cons (unsafeInterval (end y) (end x)) xs') ys' acc
      -- y starts inside x (start y > start x)
      | otherwise ->
          if end y >= end x
            -- y covers right part of x: keep left piece, advance x
            then sweepDifference xs' ys (A.snoc acc (unsafeInterval (start x) (start y)))
            -- y punches hole in x: keep left piece, continue with right remainder
            else sweepDifference
                   (A.cons (unsafeInterval (end y) (end x)) xs')
                   ys'
                   (A.snoc acc (unsafeInterval (start x) (start y)))
