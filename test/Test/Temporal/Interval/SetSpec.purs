module Test.Temporal.Interval.SetSpec where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (fst, snd)
import Temporal.Interval (Interval, unsafeInterval, start, end, overlaps)
import Temporal.Interval.Set as IS
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck ((===))
import Test.Temporal.Generators (ArbInterval(..), ArbIntervalSet(..))

-- Helper
i :: Int -> Int -> Interval Int
i = unsafeInterval

spec :: Spec Unit
spec = describe "Temporal.Interval.Set" do
  describe "construction" do
    it "empty set has no intervals" do
      IS.toArray (IS.empty :: IS.IntervalSet Int) `shouldEqual` []

    it "singleton set has one interval" do
      IS.toArray (IS.singleton (i 1 5)) `shouldEqual` [ i 1 5 ]

    it "fromIntervals merges overlapping" do
      IS.toArray (IS.fromIntervals [ i 1 5, i 3 7 ]) `shouldEqual` [ i 1 7 ]

    it "fromIntervals merges adjacent" do
      IS.toArray (IS.fromIntervals [ i 1 3, i 3 7 ]) `shouldEqual` [ i 1 7 ]

    it "fromIntervals keeps non-overlapping sorted" do
      IS.toArray (IS.fromIntervals [ i 5 7, i 1 3 ]) `shouldEqual` [ i 1 3, i 5 7 ]

    it "fromIntervals merges multiple overlapping" do
      IS.toArray (IS.fromIntervals [ i 1 4, i 3 6, i 5 9 ]) `shouldEqual` [ i 1 9 ]

    it "fromIntervals handles mixed" do
      IS.toArray (IS.fromIntervals [ i 1 3, i 5 7, i 2 6 ])
        `shouldEqual` [ i 1 7 ]

  describe "isEmpty" do
    it "empty is empty" do
      IS.isEmpty (IS.empty :: IS.IntervalSet Int) `shouldEqual` true

    it "singleton is not empty" do
      IS.isEmpty (IS.singleton (i 1 5)) `shouldEqual` false

  describe "span" do
    it "empty set has no span" do
      IS.span (IS.empty :: IS.IntervalSet Int) `shouldEqual` Nothing

    it "singleton span is itself" do
      IS.span (IS.singleton (i 3 7)) `shouldEqual` Just (i 3 7)

    it "span covers from first start to last end" do
      IS.span (IS.fromIntervals [ i 1 3, i 5 7, i 10 15 ])
        `shouldEqual` Just (i 1 15)

  describe "member" do
    it "point in interval" do
      let set = IS.fromIntervals [ i 1 5, i 10 15 ]
      IS.member 3 set `shouldEqual` true

    it "point outside intervals" do
      let set = IS.fromIntervals [ i 1 5, i 10 15 ]
      IS.member 7 set `shouldEqual` false

    it "point at start (included)" do
      IS.member 1 (IS.singleton (i 1 5)) `shouldEqual` true

    it "point at end (excluded, half-open)" do
      IS.member 5 (IS.singleton (i 1 5)) `shouldEqual` false

    it "point in gap" do
      let set = IS.fromIntervals [ i 1 3, i 5 7 ]
      IS.member 4 set `shouldEqual` false

  describe "findContaining" do
    it "finds the interval containing a point" do
      let set = IS.fromIntervals [ i 1 5, i 10 15 ]
      IS.findContaining 12 set `shouldEqual` Just (i 10 15)

    it "returns Nothing for points outside" do
      let set = IS.fromIntervals [ i 1 5, i 10 15 ]
      IS.findContaining 7 set `shouldEqual` Nothing

  describe "insert" do
    it "insert into empty" do
      IS.toArray (IS.insert (i 3 7) IS.empty) `shouldEqual` [ i 3 7 ]

    it "insert before existing" do
      IS.toArray (IS.insert (i 1 3) (IS.singleton (i 5 7)))
        `shouldEqual` [ i 1 3, i 5 7 ]

    it "insert after existing" do
      IS.toArray (IS.insert (i 5 7) (IS.singleton (i 1 3)))
        `shouldEqual` [ i 1 3, i 5 7 ]

    it "insert overlapping merges" do
      IS.toArray (IS.insert (i 3 7) (IS.singleton (i 1 5)))
        `shouldEqual` [ i 1 7 ]

    it "insert bridging two intervals" do
      let set = IS.fromIntervals [ i 1 3, i 7 9 ]
      IS.toArray (IS.insert (i 2 8) set) `shouldEqual` [ i 1 9 ]

    it "insert adjacent merges" do
      IS.toArray (IS.insert (i 3 5) (IS.singleton (i 5 7)))
        `shouldEqual` [ i 3 7 ]

    it "insert enclosed is no-op" do
      let set = IS.singleton (i 1 10)
      IS.toArray (IS.insert (i 3 7) set) `shouldEqual` [ i 1 10 ]

  describe "remove" do
    it "remove from empty" do
      IS.toArray (IS.remove (i 1 5) (IS.empty :: IS.IntervalSet Int))
        `shouldEqual` []

    it "remove non-overlapping is no-op" do
      let set = IS.singleton (i 1 5)
      IS.toArray (IS.remove (i 7 9) set) `shouldEqual` [ i 1 5 ]

    it "remove punches hole" do
      let set = IS.singleton (i 1 10)
      IS.toArray (IS.remove (i 3 7) set) `shouldEqual` [ i 1 3, i 7 10 ]

    it "remove trims from right" do
      let set = IS.singleton (i 1 5)
      IS.toArray (IS.remove (i 3 7) set) `shouldEqual` [ i 1 3 ]

    it "remove trims from left" do
      let set = IS.singleton (i 3 7)
      IS.toArray (IS.remove (i 1 5) set) `shouldEqual` [ i 5 7 ]

    it "remove fully consuming" do
      let set = IS.singleton (i 3 5)
      IS.toArray (IS.remove (i 1 7) set) `shouldEqual` []

    it "remove across multiple intervals" do
      let set = IS.fromIntervals [ i 1 4, i 6 9, i 11 14 ]
      IS.toArray (IS.remove (i 3 12) set) `shouldEqual` [ i 1 3, i 12 14 ]

  describe "gaps" do
    it "empty set has no gaps" do
      IS.toArray (IS.gaps (IS.empty :: IS.IntervalSet Int)) `shouldEqual` []

    it "singleton has no gaps" do
      IS.toArray (IS.gaps (IS.singleton (i 1 5))) `shouldEqual` []

    it "finds gaps between intervals" do
      let set = IS.fromIntervals [ i 1 3, i 5 7, i 10 12 ]
      IS.toArray (IS.gaps set) `shouldEqual` [ i 3 5, i 7 10 ]

    it "adjacent intervals have no gap" do
      let set = IS.fromIntervals [ i 1 3, i 3 7 ]
      -- Adjacent intervals are merged, so no gaps
      IS.toArray (IS.gaps set) `shouldEqual` []

  describe "unionSets" do
    it "union with empty" do
      let set = IS.fromIntervals [ i 1 5 ]
      IS.toArray (IS.unionSets set IS.empty) `shouldEqual` [ i 1 5 ]
      IS.toArray (IS.unionSets IS.empty set) `shouldEqual` [ i 1 5 ]

    it "union of overlapping sets" do
      let a = IS.fromIntervals [ i 1 5, i 10 15 ]
          b = IS.fromIntervals [ i 3 12 ]
      IS.toArray (IS.unionSets a b) `shouldEqual` [ i 1 15 ]

    it "union of disjoint sets" do
      let a = IS.fromIntervals [ i 1 3 ]
          b = IS.fromIntervals [ i 5 7 ]
      IS.toArray (IS.unionSets a b) `shouldEqual` [ i 1 3, i 5 7 ]

  describe "intersectSets" do
    it "intersection with empty" do
      let set = IS.fromIntervals [ i 1 5 ]
      IS.toArray (IS.intersectSets set IS.empty) `shouldEqual` []

    it "intersection of overlapping" do
      let a = IS.fromIntervals [ i 1 5, i 10 15 ]
          b = IS.fromIntervals [ i 3 12 ]
      IS.toArray (IS.intersectSets a b) `shouldEqual` [ i 3 5, i 10 12 ]

    it "intersection of disjoint" do
      let a = IS.fromIntervals [ i 1 3 ]
          b = IS.fromIntervals [ i 5 7 ]
      IS.toArray (IS.intersectSets a b) `shouldEqual` []

    it "intersection with self" do
      let set = IS.fromIntervals [ i 1 5, i 10 15 ]
      IS.toArray (IS.intersectSets set set) `shouldEqual` [ i 1 5, i 10 15 ]

  describe "differenceSets" do
    it "difference with empty" do
      let set = IS.fromIntervals [ i 1 5 ]
      IS.toArray (IS.differenceSets set IS.empty) `shouldEqual` [ i 1 5 ]

    it "difference removes overlap" do
      let a = IS.fromIntervals [ i 1 10 ]
          b = IS.fromIntervals [ i 3 7 ]
      IS.toArray (IS.differenceSets a b) `shouldEqual` [ i 1 3, i 7 10 ]

    it "difference with self is empty" do
      let set = IS.fromIntervals [ i 1 5, i 10 15 ]
      IS.toArray (IS.differenceSets set set) `shouldEqual` []

    it "difference with superset is empty" do
      let a = IS.fromIntervals [ i 3 7 ]
          b = IS.fromIntervals [ i 1 10 ]
      IS.toArray (IS.differenceSets a b) `shouldEqual` []

  describe "complement" do
    it "complement of empty within bounds" do
      IS.toArray (IS.complement (i 0 100) IS.empty) `shouldEqual` [ i 0 100 ]

    it "complement within bounds" do
      let set = IS.fromIntervals [ i 10 20, i 50 60 ]
      IS.toArray (IS.complement (i 0 100) set)
        `shouldEqual` [ i 0 10, i 20 50, i 60 100 ]

    it "complement of full coverage is empty" do
      let set = IS.singleton (i 0 100)
      IS.toArray (IS.complement (i 0 100) set) `shouldEqual` []

    it "complement gives back the gaps" do
      let set = IS.fromIntervals [ i 0 3, i 5 7, i 10 15 ]
          g = IS.gaps set
          c = IS.complement (i 0 15) set
      -- gaps and complement within span should be equal
      IS.toArray g `shouldEqual` IS.toArray c

  describe "sweep-line edge cases" do
    it "intersectSets: multiple partial overlaps" do
      let a = IS.fromIntervals [ i 0 5, i 10 15, i 20 25 ]
          b = IS.fromIntervals [ i 3 12, i 22 30 ]
      IS.toArray (IS.intersectSets a b)
        `shouldEqual` [ i 3 5, i 10 12, i 22 25 ]

    it "intersectSets: interleaved non-overlapping" do
      let a = IS.fromIntervals [ i 0 2, i 4 6, i 8 10 ]
          b = IS.fromIntervals [ i 2 4, i 6 8 ]
      IS.toArray (IS.intersectSets a b) `shouldEqual` []

    it "intersectSets: one set encloses the other" do
      let a = IS.fromIntervals [ i 0 100 ]
          b = IS.fromIntervals [ i 10 20, i 30 40, i 50 60 ]
      IS.toArray (IS.intersectSets a b)
        `shouldEqual` [ i 10 20, i 30 40, i 50 60 ]

    it "intersectSets: identical sets" do
      let set = IS.fromIntervals [ i 1 5, i 10 15 ]
      IS.toArray (IS.intersectSets set set) `shouldEqual` [ i 1 5, i 10 15 ]

    it "differenceSets: multiple holes punched" do
      let a = IS.fromIntervals [ i 0 100 ]
          b = IS.fromIntervals [ i 10 20, i 30 40, i 50 60 ]
      IS.toArray (IS.differenceSets a b)
        `shouldEqual` [ i 0 10, i 20 30, i 40 50, i 60 100 ]

    it "differenceSets: interleaved sets" do
      let a = IS.fromIntervals [ i 0 5, i 10 15, i 20 25 ]
          b = IS.fromIntervals [ i 3 12, i 18 22 ]
      IS.toArray (IS.differenceSets a b)
        `shouldEqual` [ i 0 3, i 12 15, i 22 25 ]

    it "differenceSets: b extends beyond a on both sides" do
      let a = IS.fromIntervals [ i 5 10 ]
          b = IS.fromIntervals [ i 0 15 ]
      IS.toArray (IS.differenceSets a b) `shouldEqual` []

    it "differenceSets: a minus nothing" do
      let a = IS.fromIntervals [ i 1 5, i 10 15 ]
      IS.toArray (IS.differenceSets a IS.empty)
        `shouldEqual` [ i 1 5, i 10 15 ]

    it "unionSets: merge-sort interleaving" do
      let a = IS.fromIntervals [ i 0 2, i 6 8, i 12 14 ]
          b = IS.fromIntervals [ i 3 5, i 9 11, i 15 17 ]
      IS.toArray (IS.unionSets a b)
        `shouldEqual` [ i 0 2, i 3 5, i 6 8, i 9 11, i 12 14, i 15 17 ]

    it "unionSets: cascading merge" do
      let a = IS.fromIntervals [ i 0 5, i 10 15, i 20 25 ]
          b = IS.fromIntervals [ i 4 11, i 14 21 ]
      IS.toArray (IS.unionSets a b) `shouldEqual` [ i 0 25 ]

    it "unionSets: identical sets" do
      let set = IS.fromIntervals [ i 1 5, i 10 15 ]
      IS.toArray (IS.unionSets set set) `shouldEqual` [ i 1 5, i 10 15 ]

    it "binary search: member at first interval" do
      let set = IS.fromIntervals [ i 10 20, i 30 40, i 50 60 ]
      IS.member 15 set `shouldEqual` true

    it "binary search: member at last interval" do
      let set = IS.fromIntervals [ i 10 20, i 30 40, i 50 60 ]
      IS.member 55 set `shouldEqual` true

    it "binary search: not member before all" do
      let set = IS.fromIntervals [ i 10 20, i 30 40, i 50 60 ]
      IS.member 5 set `shouldEqual` false

    it "binary search: not member after all" do
      let set = IS.fromIntervals [ i 10 20, i 30 40, i 50 60 ]
      IS.member 65 set `shouldEqual` false

    it "binary search: not member in gap" do
      let set = IS.fromIntervals [ i 10 20, i 30 40, i 50 60 ]
      IS.member 25 set `shouldEqual` false

    it "findContaining with binary search" do
      let set = IS.fromIntervals [ i 10 20, i 30 40, i 50 60 ]
      IS.findContaining 35 set `shouldEqual` Just (i 30 40)
      IS.findContaining 25 set `shouldEqual` Nothing

  describe "additional unit tests" do
    it "fromIntervals: empty input" do
      IS.toArray (IS.fromIntervals ([] :: Array (Interval Int))) `shouldEqual` []

    it "fromIntervals: single input" do
      IS.toArray (IS.fromIntervals [ i 3 7 ]) `shouldEqual` [ i 3 7 ]

    it "fromIntervals: all identical intervals" do
      IS.toArray (IS.fromIntervals [ i 3 7, i 3 7, i 3 7 ]) `shouldEqual` [ i 3 7 ]

    it "insert identical interval is no-op" do
      let set = IS.singleton (i 5 10)
      IS.toArray (IS.insert (i 5 10) set) `shouldEqual` [ i 5 10 ]

  describe "quickcheck properties" do
    it "invariant: intervals are sorted by start" $
      quickCheck \(ArbIntervalSet set) ->
        let arr = IS.toArray set
            starts = map start arr
            sorted = A.sort starts
        in starts === sorted

    it "invariant: intervals are non-overlapping" $
      quickCheck \(ArbIntervalSet set) ->
        let arr = IS.toArray set
            pairs = A.zip arr (A.drop 1 arr)
        in A.all (\p -> not (overlaps (fst p) (snd p))) pairs === true

    it "invariant: intervals are non-adjacent" $
      quickCheck \(ArbIntervalSet set) ->
        let arr = IS.toArray set
            pairs = A.zip arr (A.drop 1 arr)
        in A.all (\p -> end (fst p) < start (snd p)) pairs === true

    it "unionSets is commutative" $
      quickCheck \(ArbIntervalSet a) (ArbIntervalSet b) ->
        IS.toArray (IS.unionSets a b) === IS.toArray (IS.unionSets b a)

    it "intersectSets is commutative" $
      quickCheck \(ArbIntervalSet a) (ArbIntervalSet b) ->
        IS.toArray (IS.intersectSets a b) === IS.toArray (IS.intersectSets b a)

    it "intersectSets a a == a" $
      quickCheck \(ArbIntervalSet a) ->
        IS.toArray (IS.intersectSets a a) === IS.toArray a

    it "differenceSets a a == empty" $
      quickCheck \(ArbIntervalSet a) ->
        IS.toArray (IS.differenceSets a a) === ([] :: Array (Interval Int))

    it "unionSets a empty == a" $
      quickCheck \(ArbIntervalSet a) ->
        IS.toArray (IS.unionSets a IS.empty) === IS.toArray a

    it "intersectSets a empty == empty" $
      quickCheck \(ArbIntervalSet a) ->
        IS.toArray (IS.intersectSets a IS.empty) === ([] :: Array (Interval Int))

    it "differenceSets a empty == a" $
      quickCheck \(ArbIntervalSet a) ->
        IS.toArray (IS.differenceSets a IS.empty) === IS.toArray a

    it "member x == isJust (findContaining x)" $
      quickCheck \(ArbIntervalSet set) (ArbInterval iv) ->
        let x = start iv
        in IS.member x set === isJust (IS.findContaining x set)

    it "insert then member is true (at start)" $
      quickCheck \(ArbIntervalSet set) (ArbInterval iv) ->
        IS.member (start iv) (IS.insert iv set) === true

    it "union = A + (B - A)" $
      quickCheck \(ArbIntervalSet a) (ArbIntervalSet b) ->
        let u = IS.unionSets a b
            bMinusA = IS.differenceSets b a
            aPlusBMinusA = IS.unionSets a bMinusA
        in IS.toArray u === IS.toArray aPlusBMinusA

    it "A = (A intersect B) union (A minus B)" $
      quickCheck \(ArbIntervalSet a) (ArbIntervalSet b) ->
        let aAndB = IS.intersectSets a b
            aMinusB = IS.differenceSets a b
            recomposed = IS.unionSets aAndB aMinusB
        in IS.toArray recomposed === IS.toArray a
