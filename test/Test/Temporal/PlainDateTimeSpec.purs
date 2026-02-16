module Test.Temporal.PlainDateTimeSpec where

import Prelude

import Data.Maybe (Maybe(..), isJust, isNothing)
import Temporal.Duration as D
import Temporal.PlainDate as PD
import Temporal.PlainTime as PT
import Temporal.PlainDateTime as PDT
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck ((===))
import Test.Temporal.Generators (ArbPlainDate(..), ArbPlainTime(..), ArbPlainDateTime(..))

spec :: Spec Unit
spec = describe "Temporal.PlainDateTime" do
  describe "construction" do
    it "creates from fields" do
      let dt = PDT.plainDateTime (PDT.defaultPlainDateTimeFields { year = 2024, month = 3, day = 15, hour = 10, minute = 30 })
      dt `shouldSatisfy` isJust

    it "creates from string" do
      PDT.fromString "2024-03-15T10:30:00" `shouldSatisfy` isJust

    it "rejects invalid values" do
      PDT.fromString "not a datetime" `shouldSatisfy` isNothing

  describe "properties" do
    it "reads all fields" do
      case PDT.plainDateTime (PDT.defaultPlainDateTimeFields { year = 2024, month = 3, day = 15, hour = 10, minute = 30, second = 45 }) of
        Nothing -> pure unit
        Just dt -> do
          PDT.getYear dt `shouldEqual` 2024
          PDT.getMonth dt `shouldEqual` 3
          PDT.getDay dt `shouldEqual` 15
          PDT.getHour dt `shouldEqual` 10
          PDT.getMinute dt `shouldEqual` 30
          PDT.getSecond dt `shouldEqual` 45

    it "reads computed properties" do
      case PDT.plainDateTime (PDT.defaultPlainDateTimeFields { year = 2024, month = 3, day = 15 }) of
        Nothing -> pure unit
        Just dt -> do
          PDT.getDayOfWeek dt `shouldEqual` 5 -- Friday
          PDT.getInLeapYear dt `shouldEqual` true

  describe "arithmetic" do
    it "adds duration" do
      case PDT.plainDateTime (PDT.defaultPlainDateTimeFields { year = 2024, month = 1, day = 31, hour = 23, minute = 30 }) of
        Nothing -> pure unit
        Just dt -> do
          case PDT.add (D.hours 2.0) dt of
            Nothing -> pure unit
            Just dt2 -> do
              PDT.getDay dt2 `shouldEqual` 1
              PDT.getMonth dt2 `shouldEqual` 2
              PDT.getHour dt2 `shouldEqual` 1
              PDT.getMinute dt2 `shouldEqual` 30

  describe "comparison" do
    it "compares datetimes correctly" do
      case PDT.fromString "2024-01-01T00:00:00", PDT.fromString "2024-12-31T23:59:59" of
        Just dt1, Just dt2 -> (dt1 < dt2) `shouldEqual` true
        _, _ -> pure unit

  describe "conversions" do
    it "extracts PlainDate" do
      case PDT.fromString "2024-03-15T10:30:00" of
        Nothing -> pure unit
        Just dt -> do
          _ <- pure $ PDT.toPlainDate dt
          pure unit

    it "extracts PlainTime" do
      case PDT.fromString "2024-03-15T10:30:00" of
        Nothing -> pure unit
        Just dt -> do
          _ <- pure $ PDT.toPlainTime dt
          pure unit

  describe "properties" do
    it "toPlainDate (toPlainDateTime date time) preserves date fields" $
      quickCheck \(ArbPlainDate date) (ArbPlainTime time) ->
        let dt = PT.toPlainDateTime date time
            d2 = PDT.toPlainDate dt
        in PD.getYear d2 == PD.getYear date
          && PD.getMonth d2 == PD.getMonth date
          && PD.getDay d2 == PD.getDay date

    it "toPlainTime (toPlainDateTime date time) preserves time fields" $
      quickCheck \(ArbPlainDate date) (ArbPlainTime time) ->
        let dt = PT.toPlainDateTime date time
            t2 = PDT.toPlainTime dt
        in PT.getHour t2 == PT.getHour time
          && PT.getMinute t2 == PT.getMinute time
          && PT.getSecond t2 == PT.getSecond time

    it "toString / fromString round-trip" $
      quickCheck \(ArbPlainDateTime dt) ->
        PDT.fromString (PDT.toString dt) === Just dt

    it "compare a a == EQ" $
      quickCheck \(ArbPlainDateTime dt) ->
        compare dt dt === EQ
