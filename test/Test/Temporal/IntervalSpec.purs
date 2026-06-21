module Test.Temporal.IntervalSpec where

import Prelude

import Data.Maybe (Maybe(..), isNothing, isJust)
import Temporal.Interval (Interval, Relation(..), interval, unsafeInterval, start, end, relate, contains, overlaps, encloses, abuts, isBefore, isAfter, intersection, union, hull, gap, difference)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck ((===))
import Test.Temporal.Generators (ArbInterval(..), ArbIntervalPair(..))

-- Helper: create intervals from Int pairs for readable tests
i :: Int -> Int -> Interval Int
i = unsafeInterval

-- | Allen's inverse mapping
inverseRelation :: Relation -> Relation
inverseRelation Before = After
inverseRelation Meets = MetBy
inverseRelation Overlaps = OverlappedBy
inverseRelation Starts = StartedBy
inverseRelation During = Contains
inverseRelation Finishes = FinishedBy
inverseRelation Equals = Equals
inverseRelation FinishedBy = Finishes
inverseRelation Contains = During
inverseRelation StartedBy = Starts
inverseRelation OverlappedBy = Overlaps
inverseRelation MetBy = Meets
inverseRelation After = Before

spec :: Spec Unit
spec = describe "Temporal.Interval" do
  describe "construction" do
    it "creates valid interval" do
      interval 1 5 `shouldSatisfy` isJust

    it "rejects empty interval (start == end)" do
      interval 3 3 `shouldSatisfy` (isNothing :: Maybe (Interval Int) -> Boolean)

    it "rejects reversed interval (start > end)" do
      interval 5 1 `shouldSatisfy` (isNothing :: Maybe (Interval Int) -> Boolean)

    it "accessors work" do
      start (i 1 5) `shouldEqual` 1
      end (i 1 5) `shouldEqual` 5

  describe "Eq" do
    it "equal intervals are equal" do
      i 1 5 `shouldEqual` i 1 5

  describe "Show" do
    it "displays half-open notation" do
      show (i 1 5) `shouldEqual` "[1, 5)"

  describe "Allen's interval algebra" do
    it "Before: X entirely before Y with gap" do
      relate (i 1 3) (i 5 7) `shouldEqual` Before

    it "Meets: X.end == Y.start" do
      relate (i 1 3) (i 3 7) `shouldEqual` Meets

    it "Overlaps: X starts first, X.end inside Y" do
      relate (i 1 5) (i 3 7) `shouldEqual` Overlaps

    it "Starts: same start, X ends before Y" do
      relate (i 1 5) (i 1 7) `shouldEqual` Starts

    it "During: X fully inside Y" do
      relate (i 3 5) (i 1 7) `shouldEqual` During

    it "Finishes: X starts after Y.start, same end" do
      relate (i 3 7) (i 1 7) `shouldEqual` Finishes

    it "Equals: identical" do
      relate (i 1 7) (i 1 7) `shouldEqual` Equals

    it "FinishedBy: inverse of Finishes" do
      relate (i 1 7) (i 3 7) `shouldEqual` FinishedBy

    it "Contains: inverse of During" do
      relate (i 1 7) (i 3 5) `shouldEqual` Contains

    it "StartedBy: inverse of Starts" do
      relate (i 1 7) (i 1 5) `shouldEqual` StartedBy

    it "OverlappedBy: inverse of Overlaps" do
      relate (i 3 7) (i 1 5) `shouldEqual` OverlappedBy

    it "MetBy: inverse of Meets" do
      relate (i 3 7) (i 1 3) `shouldEqual` MetBy

    it "After: inverse of Before" do
      relate (i 5 7) (i 1 3) `shouldEqual` After

    it "all 13 relations are distinct" do
      let rels =
            [ relate (i 1 3) (i 5 7)
            , relate (i 1 3) (i 3 7)
            , relate (i 1 5) (i 3 7)
            , relate (i 1 5) (i 1 7)
            , relate (i 3 5) (i 1 7)
            , relate (i 3 7) (i 1 7)
            , relate (i 1 7) (i 1 7)
            , relate (i 1 7) (i 3 7)
            , relate (i 1 7) (i 3 5)
            , relate (i 1 7) (i 1 5)
            , relate (i 3 7) (i 1 5)
            , relate (i 3 7) (i 1 3)
            , relate (i 5 7) (i 1 3)
            ]
      rels `shouldEqual`
        [ Before, Meets, Overlaps, Starts, During, Finishes, Equals
        , FinishedBy, Contains, StartedBy, OverlappedBy, MetBy, After
        ]

  describe "contains (point)" do
    it "start is included" do
      contains 1 (i 1 5) `shouldEqual` true

    it "end is excluded (half-open)" do
      contains 5 (i 1 5) `shouldEqual` false

    it "middle is included" do
      contains 3 (i 1 5) `shouldEqual` true

    it "outside is excluded" do
      contains 0 (i 1 5) `shouldEqual` false
      contains 6 (i 1 5) `shouldEqual` false

  describe "overlaps" do
    it "overlapping intervals" do
      overlaps (i 1 5) (i 3 7) `shouldEqual` true

    it "non-overlapping intervals" do
      overlaps (i 1 3) (i 5 7) `shouldEqual` false

    it "adjacent intervals do NOT overlap (half-open)" do
      overlaps (i 1 3) (i 3 7) `shouldEqual` false

    it "enclosed intervals overlap" do
      overlaps (i 1 7) (i 3 5) `shouldEqual` true

    it "identical intervals overlap" do
      overlaps (i 1 5) (i 1 5) `shouldEqual` true

  describe "encloses" do
    it "larger encloses smaller" do
      encloses (i 1 7) (i 3 5) `shouldEqual` true

    it "same start" do
      encloses (i 1 7) (i 1 5) `shouldEqual` true

    it "same end" do
      encloses (i 1 7) (i 3 7) `shouldEqual` true

    it "equal intervals enclose each other" do
      encloses (i 1 7) (i 1 7) `shouldEqual` true

    it "smaller does not enclose larger" do
      encloses (i 3 5) (i 1 7) `shouldEqual` false

    it "overlapping but not enclosing" do
      encloses (i 1 5) (i 3 7) `shouldEqual` false

  describe "abuts" do
    it "adjacent intervals abut" do
      abuts (i 1 3) (i 3 7) `shouldEqual` true
      abuts (i 3 7) (i 1 3) `shouldEqual` true

    it "non-adjacent do not abut" do
      abuts (i 1 3) (i 5 7) `shouldEqual` false

    it "overlapping do not abut" do
      abuts (i 1 5) (i 3 7) `shouldEqual` false

  describe "isBefore / isAfter" do
    it "before with gap" do
      isBefore (i 1 3) (i 5 7) `shouldEqual` true

    it "before when adjacent" do
      isBefore (i 1 3) (i 3 7) `shouldEqual` true

    it "not before when overlapping" do
      isBefore (i 1 5) (i 3 7) `shouldEqual` false

    it "after is inverse of before" do
      isAfter (i 5 7) (i 1 3) `shouldEqual` true
      isAfter (i 1 3) (i 5 7) `shouldEqual` false

  describe "intersection" do
    it "overlapping intervals" do
      intersection (i 1 5) (i 3 7) `shouldEqual` Just (i 3 5)

    it "one contains the other" do
      intersection (i 1 7) (i 3 5) `shouldEqual` Just (i 3 5)

    it "identical intervals" do
      intersection (i 1 5) (i 1 5) `shouldEqual` Just (i 1 5)

    it "non-overlapping returns Nothing" do
      intersection (i 1 3) (i 5 7) `shouldEqual` Nothing

    it "adjacent returns Nothing (half-open)" do
      intersection (i 1 3) (i 3 7) `shouldEqual` Nothing

    it "is commutative" do
      intersection (i 3 7) (i 1 5) `shouldEqual` intersection (i 1 5) (i 3 7)

    it "same start, different end" do
      intersection (i 1 5) (i 1 10) `shouldEqual` Just (i 1 5)

    it "different start, same end" do
      intersection (i 3 10) (i 1 10) `shouldEqual` Just (i 3 10)

  describe "union" do
    it "overlapping intervals" do
      union (i 1 5) (i 3 7) `shouldEqual` Just (i 1 7)

    it "adjacent intervals merge" do
      union (i 1 3) (i 3 7) `shouldEqual` Just (i 1 7)

    it "non-overlapping fails" do
      union (i 1 3) (i 5 7) `shouldEqual` Nothing

    it "one contains the other" do
      union (i 1 7) (i 3 5) `shouldEqual` Just (i 1 7)

    it "is commutative" do
      union (i 3 7) (i 1 5) `shouldEqual` union (i 1 5) (i 3 7)

  describe "hull" do
    it "overlapping intervals" do
      hull (i 1 5) (i 3 7) `shouldEqual` i 1 7

    it "non-overlapping spans the gap" do
      hull (i 1 3) (i 5 7) `shouldEqual` i 1 7

    it "is commutative" do
      hull (i 5 7) (i 1 3) `shouldEqual` hull (i 1 3) (i 5 7)

  describe "gap" do
    it "non-overlapping intervals" do
      gap (i 1 3) (i 5 7) `shouldEqual` Just (i 3 5)

    it "overlapping returns Nothing" do
      gap (i 1 5) (i 3 7) `shouldEqual` Nothing

    it "adjacent returns Nothing" do
      gap (i 1 3) (i 3 7) `shouldEqual` Nothing

    it "is commutative" do
      gap (i 5 7) (i 1 3) `shouldEqual` gap (i 1 3) (i 5 7)

  describe "difference" do
    it "hole punched" do
      difference (i 1 10) (i 3 7) `shouldEqual` [ i 1 3, i 7 10 ]

    it "trimmed from right" do
      difference (i 1 5) (i 3 7) `shouldEqual` [ i 1 3 ]

    it "trimmed from left" do
      difference (i 3 7) (i 1 5) `shouldEqual` [ i 5 7 ]

    it "fully consumed" do
      difference (i 3 5) (i 1 7) `shouldEqual` []

    it "no overlap returns self" do
      difference (i 1 3) (i 5 7) `shouldEqual` [ i 1 3 ]

    it "identical returns empty" do
      difference (i 1 5) (i 1 5) `shouldEqual` []

    it "adjacent returns self (half-open)" do
      difference (i 1 3) (i 3 7) `shouldEqual` [ i 1 3 ]

    it "same start, b shorter" do
      difference (i 1 10) (i 1 5) `shouldEqual` [ i 5 10 ]

    it "same end, b starts earlier" do
      difference (i 3 10) (i 1 10) `shouldEqual` []

    it "same end, b starts later" do
      difference (i 1 10) (i 5 10) `shouldEqual` [ i 1 5 ]

  describe "quickcheck properties" do
    it "relate x y == inverse (relate y x)" $
      quickCheck \(ArbIntervalPair { a, b }) ->
        relate a b === inverseRelation (relate b a)

    it "relate a a == Equals" $
      quickCheck \(ArbInterval a) ->
        relate a a === Equals

    it "overlaps is symmetric" $
      quickCheck \(ArbIntervalPair { a, b }) ->
        overlaps a b === overlaps b a

    it "overlaps a b iff intersection a b is Just" $
      quickCheck \(ArbIntervalPair { a, b }) ->
        overlaps a b === isJust (intersection a b)

    it "encloses a a is true" $
      quickCheck \(ArbInterval a) ->
        encloses a a === true

    it "intersection is commutative" $
      quickCheck \(ArbIntervalPair { a, b }) ->
        intersection a b === intersection b a

    it "intersection a a == Just a" $
      quickCheck \(ArbInterval a) ->
        intersection a a === Just a

    it "union is commutative" $
      quickCheck \(ArbIntervalPair { a, b }) ->
        union a b === union b a

    it "union a a == Just a" $
      quickCheck \(ArbInterval a) ->
        union a a === Just a

    it "hull is commutative" $
      quickCheck \(ArbIntervalPair { a, b }) ->
        hull a b === hull b a

    it "hull a a == a" $
      quickCheck \(ArbInterval a) ->
        hull a a === a

    it "gap is commutative" $
      quickCheck \(ArbIntervalPair { a, b }) ->
        gap a b === gap b a

    it "difference a a == []" $
      quickCheck \(ArbInterval a) ->
        difference a a === ([] :: Array (Interval Int))

    it "isBefore a b == isAfter b a" $
      quickCheck \(ArbIntervalPair { a, b }) ->
        isBefore a b === isAfter b a

    it "abuts is symmetric" $
      quickCheck \(ArbIntervalPair { a, b }) ->
        abuts a b === abuts b a

    it "contains start is true" $
      quickCheck \(ArbInterval a) ->
        contains (start a) a === true

    it "contains end is false (half-open)" $
      quickCheck \(ArbInterval a) ->
        contains (end a) a === false
