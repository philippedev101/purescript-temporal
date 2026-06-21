module Test.Temporal.Interval.DurationSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Temporal.Duration as D
import Temporal.Instant as I
import Temporal.Interval (unsafeInterval)
import Temporal.Interval.Duration (duration, durationBetween)
import Temporal.PlainDate as PD
import Temporal.PlainDateTime as PDT
import Temporal.PlainTime as PT
import Temporal.ZonedDateTime as ZDT
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = describe "Temporal.Interval.Duration" do
  describe "durationBetween" do
    it "PlainDate: days between two dates" do
      case PD.plainDate 2024 1 1, PD.plainDate 2024 1 15 of
        Just d1, Just d2 -> do
          let dur = durationBetween d1 d2
          D.getDays dur `shouldEqual` 14.0
        _, _ -> pure unit

    it "PlainTime: hours between two times" do
      case PT.plainTime { hour: 9, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 }
         , PT.plainTime { hour: 17, minute: 30, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 } of
        Just t1, Just t2 -> do
          let dur = durationBetween t1 t2
          D.getHours dur `shouldEqual` 8.0
          D.getMinutes dur `shouldEqual` 30.0
        _, _ -> pure unit

    it "PlainDateTime: mixed date+time duration" do
      case PDT.plainDateTime PDT.defaultPlainDateTimeFields { year = 2024, month = 1, day = 1, hour = 9 }
         , PDT.plainDateTime PDT.defaultPlainDateTimeFields { year = 2024, month = 1, day = 2, hour = 17 } of
        Just dt1, Just dt2 -> do
          let dur = durationBetween dt1 dt2
          D.getDays dur `shouldEqual` 1.0
          D.getHours dur `shouldEqual` 8.0
        _, _ -> pure unit

    it "Instant: millisecond-level duration" do
      case I.fromEpochMilliseconds 0.0, I.fromEpochMilliseconds 3600000.0 of
        Just i1, Just i2 -> do
          let dur = durationBetween i1 i2
          D.getHours dur `shouldEqual` 1.0
        _, _ -> pure unit

    it "ZonedDateTime: timezone-aware duration" do
      case ZDT.fromString "2024-06-15T09:00:00-04:00[America/New_York]"
         , ZDT.fromString "2024-06-15T17:00:00-04:00[America/New_York]" of
        Just z1, Just z2 -> do
          let dur = durationBetween z1 z2
          D.getHours dur `shouldEqual` 8.0
        _, _ -> pure unit

  describe "duration (on Interval)" do
    it "PlainDate interval" do
      case PD.plainDate 2024 3 1, PD.plainDate 2024 4 1 of
        Just d1, Just d2 -> do
          let dur = duration (unsafeInterval d1 d2)
          -- until' defaults to largestUnit=Years, so March 1 to April 1 = P1M
          D.getMonths dur `shouldEqual` 1.0
        _, _ -> pure unit

    it "PlainDateTime interval" do
      case PDT.plainDateTime PDT.defaultPlainDateTimeFields { year = 2024, month = 1, day = 1 }
         , PDT.plainDateTime PDT.defaultPlainDateTimeFields { year = 2024, month = 1, day = 1, hour = 8, minute = 30 } of
        Just dt1, Just dt2 -> do
          let dur = duration (unsafeInterval dt1 dt2)
          D.getHours dur `shouldEqual` 8.0
          D.getMinutes dur `shouldEqual` 30.0
        _, _ -> pure unit

    it "ZonedDateTime interval across DST" do
      -- Spring forward: March 10, 2024 in America/New_York
      case ZDT.fromString "2024-03-10T00:00:00-05:00[America/New_York]"
         , ZDT.fromString "2024-03-11T00:00:00-04:00[America/New_York]" of
        Just z1, Just z2 -> do
          let dur = duration (unsafeInterval z1 z2)
          -- until' defaults to largestUnit=Years, so midnight to midnight = P1D
          D.getDays dur `shouldEqual` 1.0
        _, _ -> pure unit

    it "Instant interval" do
      case I.fromEpochMilliseconds 0.0, I.fromEpochMilliseconds 7200000.0 of
        Just i1, Just i2 -> do
          let dur = duration (unsafeInterval i1 i2)
          D.getHours dur `shouldEqual` 2.0
        _, _ -> pure unit

    it "duration is positive for valid intervals" do
      case PD.plainDate 2024 1 1, PD.plainDate 2024 12 31 of
        Just d1, Just d2 -> do
          let dur = duration (unsafeInterval d1 d2)
          D.sign dur `shouldEqual` 1
          dur `shouldSatisfy` \d -> D.getDays d > 0.0
        _, _ -> pure unit
