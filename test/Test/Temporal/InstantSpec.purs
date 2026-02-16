module Test.Temporal.InstantSpec where

import Prelude

import Data.Maybe (Maybe(..), isJust, isNothing)
import JS.BigInt as BigInt
import Temporal.Duration as D
import Temporal.Instant as I
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck ((===))
import Test.Temporal.Generators (ArbInstant(..), ArbTimeDuration(..))

spec :: Spec Unit
spec = describe "Temporal.Instant" do
  describe "construction" do
    it "creates from epoch nanoseconds" do
      I.fromEpochNanoseconds (BigInt.fromInt 0) `shouldSatisfy` isJust

    it "creates from epoch milliseconds" do
      I.fromEpochMilliseconds 0.0 `shouldSatisfy` isJust

    it "creates from string" do
      I.fromString "1970-01-01T00:00:00Z" `shouldSatisfy` isJust

    it "rejects invalid strings" do
      I.fromString "not an instant" `shouldSatisfy` isNothing

  describe "properties" do
    it "reads epoch milliseconds" do
      case I.fromEpochMilliseconds 1000.0 of
        Nothing -> pure unit
        Just i -> I.getEpochMilliseconds i `shouldEqual` 1000.0

    it "reads epoch nanoseconds" do
      case I.fromEpochNanoseconds (BigInt.fromInt 1000000000) of
        Nothing -> pure unit
        Just i -> I.getEpochNanoseconds i `shouldEqual` BigInt.fromInt 1000000000

  describe "arithmetic" do
    it "adds time duration" do
      case I.fromEpochMilliseconds 0.0 of
        Nothing -> pure unit
        Just i -> do
          case I.add (D.hours 1.0) i of
            Nothing -> pure unit
            Just i2 -> I.getEpochMilliseconds i2 `shouldEqual` 3600000.0

    it "rejects date-unit durations" do
      case I.fromEpochMilliseconds 0.0 of
        Nothing -> pure unit
        Just i -> I.add (D.months 1.0) i `shouldSatisfy` isNothing

  describe "comparison" do
    it "compares instants correctly" do
      case I.fromEpochMilliseconds 1000.0, I.fromEpochMilliseconds 2000.0 of
        Just i1, Just i2 -> do
          (i1 < i2) `shouldEqual` true
          (i1 == i2) `shouldEqual` false
        _, _ -> pure unit

  describe "diff" do
    it "calculates difference in hours" do
      case I.fromEpochMilliseconds 0.0, I.fromEpochMilliseconds 7200000.0 of
        Just i1, Just i2 -> do
          let dur = I.until' i1 i2
          D.getHours dur `shouldEqual` 2.0
        _, _ -> pure unit

  describe "conversion" do
    it "converts to ZonedDateTime" do
      case I.fromEpochMilliseconds 0.0 of
        Nothing -> pure unit
        Just i -> do
          _ <- pure $ I.toZonedDateTimeISO "UTC" i
          pure unit

  describe "properties" do
    it "add then subtract is identity (time-only)" $
      quickCheck \(ArbTimeDuration d) (ArbInstant i) ->
        case I.add d i of
          Nothing -> false
          Just i2 -> I.subtract d i2 == Just i

    it "fromEpochMilliseconds round-trip" $
      quickCheck \(ArbInstant i) ->
        I.fromEpochMilliseconds (I.getEpochMilliseconds i) === Just i

    it "toString / fromString round-trip" $
      quickCheck \(ArbInstant i) ->
        I.fromString (I.toString i) === Just i

    it "compare a a == EQ" $
      quickCheck \(ArbInstant i) ->
        compare i i === EQ
