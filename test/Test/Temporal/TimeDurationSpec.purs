module Test.Temporal.TimeDurationSpec where

import Prelude

import Data.Array (replicate)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), isNothing, isJust)
import Temporal.Duration as D
import Temporal.TimeDuration as TD
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)
import Test.Temporal.Generators (ArbTimeDuration(..))

spec :: Spec Unit
spec = describe "Temporal.TimeDuration" do
  describe "construction" do
    it "fromHours" do
      TD.getHours (TD.fromHours 2.0) `shouldEqual` 2.0

    it "fromMinutes" do
      TD.getMinutes (TD.fromMinutes 30.0) `shouldEqual` 30.0

    it "fromSeconds" do
      TD.getSeconds (TD.fromSeconds 45.0) `shouldEqual` 45.0

    it "timeDuration with valid fields" do
      TD.timeDuration { hours: 1.0, minutes: 30.0, seconds: 0.0, milliseconds: 0.0, microseconds: 0.0, nanoseconds: 0.0 }
        `shouldSatisfy` isJust

    it "timeDuration rejects mixed signs" do
      TD.timeDuration { hours: 1.0, minutes: -30.0, seconds: 0.0, milliseconds: 0.0, microseconds: 0.0, nanoseconds: 0.0 }
        `shouldSatisfy` isNothing

  describe "fromDuration / toDuration" do
    it "accepts time-only duration" do
      TD.fromDuration (D.hours 5.0) `shouldSatisfy` isJust

    it "rejects duration with days" do
      TD.fromDuration (D.days 1.0) `shouldSatisfy` isNothing

    it "rejects duration with months" do
      TD.fromDuration (D.months 1.0) `shouldSatisfy` isNothing

    it "toDuration round-trips" do
      let td = TD.fromHours 3.0
      D.getHours (TD.toDuration td) `shouldEqual` 3.0

  describe "Semigroup" do
    it "append adds durations" do
      let a = TD.fromHours 1.0
          b = TD.fromMinutes 30.0
          result = a <> b
      TD.getHours result `shouldEqual` 1.0
      TD.getMinutes result `shouldEqual` 30.0

    it "append with negative" do
      let a = TD.fromHours 2.0
          b = TD.negate (TD.fromMinutes 30.0)
          result = a <> b
      TD.getHours result `shouldEqual` 1.0
      TD.getMinutes result `shouldEqual` 30.0

    it "mixed-sign append doesn't silently return zero" do
      let a = TD.fromHours 1.0
          b = TD.negate (TD.fromMinutes 30.0)
          result = a <> b
      (result /= TD.zero) `shouldEqual` true
      TD.getMinutes result `shouldEqual` 30.0

  describe "Monoid" do
    it "mempty is zero" do
      TD.getHours (mempty :: TD.TimeDuration) `shouldEqual` 0.0
      TD.getMinutes (mempty :: TD.TimeDuration) `shouldEqual` 0.0

    it "mempty <> x == x" do
      let x = TD.fromHours 5.0
      (mempty <> x) `shouldEqual` x

    it "x <> mempty == x" do
      let x = TD.fromHours 5.0
      (x <> mempty) `shouldEqual` x

    it "fold works" do
      let durations = replicate 5 (TD.fromMinutes 12.0)
          total = fold durations
      TD.getMinutes total `shouldEqual` 60.0

  describe "Ord" do
    it "compares by total time" do
      (TD.fromHours 1.0 < TD.fromHours 2.0) `shouldEqual` true
      (TD.fromHours 2.0 > TD.fromHours 1.0) `shouldEqual` true

    it "60 minutes compares equal to 1 hour" do
      compare (TD.fromMinutes 60.0) (TD.fromHours 1.0) `shouldEqual` EQ

    it "Eq agrees with Ord for equivalent durations" do
      -- Duration.Eq uses Duration.compare, so 60min == 1hr
      (TD.fromMinutes 60.0 == TD.fromHours 1.0) `shouldEqual` true

    it "negative < positive" do
      (TD.negate (TD.fromHours 1.0) < TD.fromHours 1.0) `shouldEqual` true

  describe "negate / abs" do
    it "negate flips sign" do
      TD.getHours (TD.negate (TD.fromHours 5.0)) `shouldEqual` (-5.0)

    it "abs makes positive" do
      TD.getHours (TD.abs (TD.negate (TD.fromHours 5.0))) `shouldEqual` 5.0

    it "zero alias" do
      TD.zero `shouldEqual` (mempty :: TD.TimeDuration)

  describe "quickcheck" do
    it "mempty is identity (left)" $
      quickCheck \(ArbTimeDuration d) ->
        case TD.fromDuration d of
          Nothing -> true
          Just td -> (mempty <> td) == td

    it "mempty is identity (right)" $
      quickCheck \(ArbTimeDuration d) ->
        case TD.fromDuration d of
          Nothing -> true
          Just td -> (td <> mempty) == td

    it "associativity" $
      quickCheck \(ArbTimeDuration d1) (ArbTimeDuration d2) (ArbTimeDuration d3) ->
        case TD.fromDuration d1, TD.fromDuration d2, TD.fromDuration d3 of
          Just a, Just b, Just c -> ((a <> b) <> c) == (a <> (b <> c))
          _, _, _ -> true

    it "negate (negate x) == x" $
      quickCheck \(ArbTimeDuration d) ->
        case TD.fromDuration d of
          Nothing -> true
          Just td -> TD.negate (TD.negate td) == td

    it "x <> negate x == zero" $
      quickCheck \(ArbTimeDuration d) ->
        case TD.fromDuration d of
          Nothing -> true
          Just td -> (td <> TD.negate td) == TD.zero
