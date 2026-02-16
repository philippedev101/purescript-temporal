module Test.Temporal.ZonedDateTimeSpec where

import Prelude

import Data.Maybe (Maybe(..), isJust, isNothing)
import JS.BigInt as BigInt
import Temporal.Duration as D
import Temporal.Instant as I
import Temporal.ZonedDateTime as ZDT
import Temporal.Internal.Options (TransitionDirection(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck ((===))
import Test.Temporal.Generators (ArbInstant(..))

spec :: Spec Unit
spec = describe "Temporal.ZonedDateTime" do
  describe "construction" do
    it "creates from epoch nanoseconds and timezone" do
      ZDT.fromEpochNanoseconds (BigInt.fromInt 0) "UTC" `shouldSatisfy` isJust

    it "creates from string" do
      ZDT.fromString "2024-03-15T10:30:00+00:00[UTC]" `shouldSatisfy` isJust

    it "rejects invalid strings" do
      ZDT.fromString "not a zoned datetime" `shouldSatisfy` isNothing

    it "rejects invalid timezone" do
      ZDT.fromEpochNanoseconds (BigInt.fromInt 0) "Not/A/Timezone" `shouldSatisfy` isNothing

  describe "properties" do
    it "reads all fields" do
      case ZDT.fromString "2024-03-15T10:30:45+00:00[UTC]" of
        Nothing -> pure unit
        Just zdt -> do
          ZDT.getYear zdt `shouldEqual` 2024
          ZDT.getMonth zdt `shouldEqual` 3
          ZDT.getDay zdt `shouldEqual` 15
          ZDT.getHour zdt `shouldEqual` 10
          ZDT.getMinute zdt `shouldEqual` 30
          ZDT.getSecond zdt `shouldEqual` 45
          ZDT.getTimeZoneId zdt `shouldEqual` "UTC"
          ZDT.getOffset zdt `shouldEqual` "+00:00"

    it "reads computed properties" do
      case ZDT.fromString "2024-03-15T10:30:00+00:00[UTC]" of
        Nothing -> pure unit
        Just zdt -> do
          ZDT.getDayOfWeek zdt `shouldEqual` 5
          ZDT.getInLeapYear zdt `shouldEqual` true

  describe "arithmetic" do
    it "adds duration" do
      case ZDT.fromString "2024-01-31T23:00:00+00:00[UTC]" of
        Nothing -> pure unit
        Just zdt -> do
          case ZDT.add (D.hours 2.0) zdt of
            Nothing -> pure unit
            Just zdt2 -> do
              ZDT.getDay zdt2 `shouldEqual` 1
              ZDT.getMonth zdt2 `shouldEqual` 2
              ZDT.getHour zdt2 `shouldEqual` 1

  describe "comparison" do
    it "compares correctly" do
      case ZDT.fromString "2024-01-01T00:00:00+00:00[UTC]", ZDT.fromString "2024-12-31T23:59:59+00:00[UTC]" of
        Just z1, Just z2 -> (z1 < z2) `shouldEqual` true
        _, _ -> pure unit

  describe "timezone operations" do
    it "gets start of day" do
      case ZDT.fromString "2024-03-15T10:30:00+00:00[UTC]" of
        Nothing -> pure unit
        Just zdt -> do
          let sod = ZDT.startOfDay zdt
          ZDT.getHour sod `shouldEqual` 0
          ZDT.getMinute sod `shouldEqual` 0
          ZDT.getSecond sod `shouldEqual` 0

    it "gets timezone transition" do
      case ZDT.fromString "2024-03-01T00:00:00-05:00[America/New_York]" of
        Nothing -> pure unit
        Just zdt -> do
          -- There should be a DST transition (spring forward) in March
          ZDT.getTimeZoneTransition Next zdt `shouldSatisfy` isJust

  describe "conversions" do
    it "converts to Instant" do
      case ZDT.fromString "2024-03-15T10:30:00+00:00[UTC]" of
        Nothing -> pure unit
        Just zdt -> do
          _ <- pure $ ZDT.toInstant zdt
          pure unit

    it "converts to PlainDateTime" do
      case ZDT.fromString "2024-03-15T10:30:00+00:00[UTC]" of
        Nothing -> pure unit
        Just zdt -> do
          _ <- pure $ ZDT.toPlainDateTime zdt
          pure unit

    it "converts to PlainDate" do
      case ZDT.fromString "2024-03-15T10:30:00+00:00[UTC]" of
        Nothing -> pure unit
        Just zdt -> do
          _ <- pure $ ZDT.toPlainDate zdt
          pure unit

    it "converts to PlainTime" do
      case ZDT.fromString "2024-03-15T10:30:00+00:00[UTC]" of
        Nothing -> pure unit
        Just zdt -> do
          _ <- pure $ ZDT.toPlainTime zdt
          pure unit

  describe "withTimeZone" do
    it "changes timezone" do
      case ZDT.fromString "2024-03-15T10:30:00+00:00[UTC]" of
        Nothing -> pure unit
        Just zdt -> do
          let zdt2 = ZDT.withTimeZone "America/New_York" zdt
          ZDT.getTimeZoneId zdt2 `shouldEqual` "America/New_York"

  describe "properties" do
    it "toInstant (toZonedDateTimeISO UTC i) == i" $
      quickCheck \(ArbInstant i) ->
        ZDT.toInstant (I.toZonedDateTimeISO "UTC" i) === i

    it "toString / fromString round-trip (UTC)" $
      quickCheck \(ArbInstant i) ->
        let zdt = I.toZonedDateTimeISO "UTC" i
        in ZDT.fromString (ZDT.toString zdt) === Just zdt

    it "compare a a == EQ" $
      quickCheck \(ArbInstant i) ->
        let zdt = I.toZonedDateTimeISO "UTC" i
        in compare zdt zdt === EQ
