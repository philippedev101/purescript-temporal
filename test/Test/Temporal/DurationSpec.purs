module Test.Temporal.DurationSpec where

import Prelude

import Data.Maybe (Maybe(..), isNothing, isJust)
import Temporal.Duration as D
import Temporal.Internal.Options (DateTimeUnit(..), TimeUnit(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck ((===))
import Test.Temporal.Generators (ArbDuration(..), ArbTimeDuration(..))

spec :: Spec Unit
spec = describe "Temporal.Duration" do
  describe "construction" do
    it "creates duration from default fields" do
      let d = D.duration D.defaultDurationFields
      d `shouldSatisfy` isJust

    it "creates duration from string" do
      D.fromString "PT1H30M" `shouldSatisfy` isJust
      D.fromString "P1Y2M3D" `shouldSatisfy` isJust

    it "rejects invalid strings" do
      D.fromString "not a duration" `shouldSatisfy` isNothing

  describe "smart constructors" do
    it "creates single-field durations" do
      D.getHours (D.hours 5.0) `shouldEqual` 5.0
      D.getDays (D.days 10.0) `shouldEqual` 10.0
      D.getMinutes (D.minutes 30.0) `shouldEqual` 30.0

  describe "properties" do
    it "reads all fields" do
      case D.fromString "P1Y2M3W4DT5H6M7.008009010S" of
        Nothing -> pure unit
        Just d -> do
          D.getYears d `shouldEqual` 1.0
          D.getMonths d `shouldEqual` 2.0
          D.getWeeks d `shouldEqual` 3.0
          D.getDays d `shouldEqual` 4.0
          D.getHours d `shouldEqual` 5.0
          D.getMinutes d `shouldEqual` 6.0
          D.getSeconds d `shouldEqual` 7.0
          D.getMilliseconds d `shouldEqual` 8.0
          D.getMicroseconds d `shouldEqual` 9.0
          D.getNanoseconds d `shouldEqual` 10.0

    it "reports sign correctly" do
      D.sign (D.hours 1.0) `shouldEqual` 1
      D.sign (D.hours (-1.0)) `shouldEqual` (-1)
      case D.duration D.defaultDurationFields of
        Nothing -> pure unit
        Just d -> D.sign d `shouldEqual` 0

    it "reports blank correctly" do
      case D.duration D.defaultDurationFields of
        Nothing -> pure unit
        Just d -> D.blank d `shouldEqual` true
      D.blank (D.hours 1.0) `shouldEqual` false

  describe "arithmetic" do
    it "adds durations" do
      let result = D.add (D.hours 1.0) (D.hours 2.0)
      case result of
        Nothing -> pure unit
        Just d -> D.getHours d `shouldEqual` 3.0

    it "subtracts durations" do
      let result = D.subtract (D.hours 3.0) (D.hours 1.0)
      case result of
        Nothing -> pure unit
        Just d -> D.getHours d `shouldEqual` 2.0

    it "negates duration" do
      D.getHours (D.negated (D.hours 5.0)) `shouldEqual` (-5.0)

    it "takes absolute value" do
      D.getHours (D.abs (D.hours (-5.0))) `shouldEqual` 5.0

  describe "equality" do
    it "equal durations are equal" do
      D.hours 1.0 `shouldEqual` D.hours 1.0

  describe "round" do
    it "rounds to largest unit" do
      let opts = D.defaultDurationRoundOptions
            { largestUnit = TimeU Hours
            , smallestUnit = TimeU Hours
            }
      case D.round opts (D.minutes 90.0) of
        Nothing -> pure unit
        Just d -> D.getHours d `shouldEqual` 2.0

  describe "total" do
    it "computes total in given unit" do
      case D.total (TimeU Minutes) (D.hours 1.0) of
        Nothing -> pure unit
        Just n -> n `shouldEqual` 60.0

  describe "properties" do
    it "negated is an involution" $
      quickCheck \(ArbDuration d) ->
        D.negated (D.negated d) === d

    it "abs d == abs (negated d)" $
      quickCheck \(ArbDuration d) ->
        D.abs d === D.abs (D.negated d)

    it "sign (negated d) == negate (sign d)" $
      quickCheck \(ArbDuration d) ->
        D.sign (D.negated d) === negate (D.sign d)

    it "add d (negated d) is blank (time-only)" $
      quickCheck \(ArbTimeDuration d) ->
        case D.add d (D.negated d) of
          Nothing -> false
          Just result -> D.blank result

    it "toString / fromString round-trip" $
      quickCheck \(ArbDuration d) ->
        D.fromString (D.toString d) === Just d
