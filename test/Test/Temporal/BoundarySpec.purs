module Test.Temporal.BoundarySpec where

import Prelude

import Data.Maybe (Maybe(..))
import Temporal.PlainDate as PD
import Temporal.PlainDateTime as PDT
import Temporal.PlainDate.Boundary as PDB
import Temporal.ZonedDateTime as ZDT
import Temporal.ZonedDateTime.Boundary as ZDTB
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck ((===))
import Test.Temporal.Generators (ArbPlainDate(..))

-- Helper
pd :: Int -> Int -> Int -> PD.PlainDate
pd y m d = case PD.plainDate y m d of
  Just x -> x
  Nothing -> pd 2024 1 1 -- fallback, should never happen in tests

spec :: Spec Unit
spec = describe "Boundary functions" do
  describe "PlainDate.Boundary" do
    describe "startOfWeek" do
      it "Wednesday -> Monday" do
        -- 2024-03-20 is a Wednesday
        PDB.startOfWeek (pd 2024 3 20) `shouldEqual` pd 2024 3 18

      it "Monday -> Monday (identity)" do
        PDB.startOfWeek (pd 2024 3 18) `shouldEqual` pd 2024 3 18

      it "Sunday -> Monday of same week" do
        PDB.startOfWeek (pd 2024 3 24) `shouldEqual` pd 2024 3 18

      it "crosses month boundary" do
        -- 2024-03-01 is a Friday, Monday is Feb 26
        PDB.startOfWeek (pd 2024 3 1) `shouldEqual` pd 2024 2 26

      it "crosses year boundary (stays in same year)" do
        -- 2024-01-03 is a Wednesday, Monday is Jan 1
        PDB.startOfWeek (pd 2024 1 3) `shouldEqual` pd 2024 1 1

      it "crosses year boundary backward" do
        -- 2025-01-01 is a Wednesday, Monday is Dec 30 2024
        PDB.startOfWeek (pd 2025 1 1) `shouldEqual` pd 2024 12 30

    describe "endOfWeek" do
      it "Wednesday -> Sunday" do
        PDB.endOfWeek (pd 2024 3 20) `shouldEqual` pd 2024 3 24

      it "Sunday -> Sunday (identity)" do
        PDB.endOfWeek (pd 2024 3 24) `shouldEqual` pd 2024 3 24

      it "Monday -> Sunday" do
        PDB.endOfWeek (pd 2024 3 18) `shouldEqual` pd 2024 3 24

      it "crosses month boundary" do
        -- 2024-03-28 is Thursday, Sunday is March 31
        PDB.endOfWeek (pd 2024 3 28) `shouldEqual` pd 2024 3 31

      it "crosses month boundary forward" do
        -- 2025-01-30 is Thursday, Sunday is Feb 2
        PDB.endOfWeek (pd 2025 1 30) `shouldEqual` pd 2025 2 2

      it "crosses year boundary forward" do
        -- 2024-12-30 is Monday, Sunday is Jan 5 2025
        PDB.endOfWeek (pd 2024 12 30) `shouldEqual` pd 2025 1 5

    describe "startOfMonth" do
      it "middle of month" do
        PDB.startOfMonth (pd 2024 3 15) `shouldEqual` pd 2024 3 1

      it "first of month (identity)" do
        PDB.startOfMonth (pd 2024 3 1) `shouldEqual` pd 2024 3 1

      it "last of month" do
        PDB.startOfMonth (pd 2024 3 31) `shouldEqual` pd 2024 3 1

    describe "endOfMonth" do
      it "31-day month" do
        PDB.endOfMonth (pd 2024 3 15) `shouldEqual` pd 2024 3 31

      it "30-day month" do
        PDB.endOfMonth (pd 2024 4 10) `shouldEqual` pd 2024 4 30

      it "February leap year" do
        PDB.endOfMonth (pd 2024 2 1) `shouldEqual` pd 2024 2 29

      it "February non-leap year" do
        PDB.endOfMonth (pd 2023 2 1) `shouldEqual` pd 2023 2 28

      it "last day (identity)" do
        PDB.endOfMonth (pd 2024 1 31) `shouldEqual` pd 2024 1 31

    describe "startOfYear" do
      it "returns January 1" do
        PDB.startOfYear (pd 2024 6 15) `shouldEqual` pd 2024 1 1

      it "January 1 (identity)" do
        PDB.startOfYear (pd 2024 1 1) `shouldEqual` pd 2024 1 1

    describe "endOfYear" do
      it "returns December 31" do
        PDB.endOfYear (pd 2024 6 15) `shouldEqual` pd 2024 12 31

      it "December 31 (identity)" do
        PDB.endOfYear (pd 2024 12 31) `shouldEqual` pd 2024 12 31

    describe "startOfDay" do
      it "produces midnight" do
        let dt = PDB.startOfDay (pd 2024 3 15)
        PDT.getHour dt `shouldEqual` 0
        PDT.getMinute dt `shouldEqual` 0
        PDT.getSecond dt `shouldEqual` 0
        PDT.getMillisecond dt `shouldEqual` 0
        PDT.getMicrosecond dt `shouldEqual` 0
        PDT.getNanosecond dt `shouldEqual` 0

      it "preserves date" do
        let dt = PDB.startOfDay (pd 2024 3 15)
        PDT.getYear dt `shouldEqual` 2024
        PDT.getMonth dt `shouldEqual` 3
        PDT.getDay dt `shouldEqual` 15

    describe "endOfDay" do
      it "produces 23:59:59.999999999" do
        let dt = PDB.endOfDay (pd 2024 3 15)
        PDT.getHour dt `shouldEqual` 23
        PDT.getMinute dt `shouldEqual` 59
        PDT.getSecond dt `shouldEqual` 59
        PDT.getMillisecond dt `shouldEqual` 999
        PDT.getMicrosecond dt `shouldEqual` 999
        PDT.getNanosecond dt `shouldEqual` 999

      it "preserves date" do
        let dt = PDB.endOfDay (pd 2024 3 15)
        PDT.getYear dt `shouldEqual` 2024
        PDT.getMonth dt `shouldEqual` 3
        PDT.getDay dt `shouldEqual` 15

    describe "quickcheck" do
      it "startOfWeek is always Monday" $
        quickCheck \(ArbPlainDate d) ->
          PD.getDayOfWeek (PDB.startOfWeek d) === 1

      it "endOfWeek is always Sunday" $
        quickCheck \(ArbPlainDate d) ->
          PD.getDayOfWeek (PDB.endOfWeek d) === 7

      it "startOfMonth always has day 1" $
        quickCheck \(ArbPlainDate d) ->
          PD.getDay (PDB.startOfMonth d) === 1

      it "endOfMonth day == daysInMonth" $
        quickCheck \(ArbPlainDate d) ->
          PD.getDay (PDB.endOfMonth d) === PD.getDaysInMonth d

      it "startOfMonth preserves year and month" $
        quickCheck \(ArbPlainDate d) ->
          (PD.getYear (PDB.startOfMonth d) == PD.getYear d)
            && (PD.getMonth (PDB.startOfMonth d) == PD.getMonth d)

      it "endOfMonth preserves year and month" $
        quickCheck \(ArbPlainDate d) ->
          (PD.getYear (PDB.endOfMonth d) == PD.getYear d)
            && (PD.getMonth (PDB.endOfMonth d) == PD.getMonth d)

      it "startOfYear is January 1" $
        quickCheck \(ArbPlainDate d) ->
          (PD.getMonth (PDB.startOfYear d) == 1)
            && (PD.getDay (PDB.startOfYear d) == 1)

      it "endOfYear is December 31" $
        quickCheck \(ArbPlainDate d) ->
          (PD.getMonth (PDB.endOfYear d) == 12)
            && (PD.getDay (PDB.endOfYear d) == 31)

      it "startOfWeek <= d <= endOfWeek" $
        quickCheck \(ArbPlainDate d) ->
          PDB.startOfWeek d <= d && d <= PDB.endOfWeek d

      it "startOfMonth <= d <= endOfMonth" $
        quickCheck \(ArbPlainDate d) ->
          PDB.startOfMonth d <= d && d <= PDB.endOfMonth d

      it "startOfYear <= d <= endOfYear" $
        quickCheck \(ArbPlainDate d) ->
          PDB.startOfYear d <= d && d <= PDB.endOfYear d

      it "startOfWeek is idempotent" $
        quickCheck \(ArbPlainDate d) ->
          PDB.startOfWeek (PDB.startOfWeek d) === PDB.startOfWeek d

      it "endOfWeek is idempotent" $
        quickCheck \(ArbPlainDate d) ->
          PDB.endOfWeek (PDB.endOfWeek d) === PDB.endOfWeek d

      it "startOfMonth is idempotent" $
        quickCheck \(ArbPlainDate d) ->
          PDB.startOfMonth (PDB.startOfMonth d) === PDB.startOfMonth d

      it "endOfMonth is idempotent" $
        quickCheck \(ArbPlainDate d) ->
          PDB.endOfMonth (PDB.endOfMonth d) === PDB.endOfMonth d

      it "startOfYear is idempotent" $
        quickCheck \(ArbPlainDate d) ->
          PDB.startOfYear (PDB.startOfYear d) === PDB.startOfYear d

      it "endOfYear is idempotent" $
        quickCheck \(ArbPlainDate d) ->
          PDB.endOfYear (PDB.endOfYear d) === PDB.endOfYear d

      it "week span is always 6 days" $
        quickCheck \(ArbPlainDate d) ->
          PD.getDayOfYear (PDB.endOfWeek d) - PD.getDayOfYear (PDB.startOfWeek d)
            == 6
            || -- handle year boundary: endOfWeek and startOfWeek in different years
               PD.getYear (PDB.endOfWeek d) /= PD.getYear (PDB.startOfWeek d)

      it "startOfDay < endOfDay" $
        quickCheck \(ArbPlainDate d) ->
          PDB.startOfDay d < PDB.endOfDay d

      it "startOfDay preserves date" $
        quickCheck \(ArbPlainDate d) ->
          let dt = PDB.startOfDay d
          in (PDT.getYear dt == PD.getYear d)
            && (PDT.getMonth dt == PD.getMonth d)
            && (PDT.getDay dt == PD.getDay d)

      it "endOfDay preserves date" $
        quickCheck \(ArbPlainDate d) ->
          let dt = PDB.endOfDay d
          in (PDT.getYear dt == PD.getYear d)
            && (PDT.getMonth dt == PD.getMonth d)
            && (PDT.getDay dt == PD.getDay d)

      it "startOfYear preserves year" $
        quickCheck \(ArbPlainDate d) ->
          PD.getYear (PDB.startOfYear d) === PD.getYear d

      it "endOfYear preserves year" $
        quickCheck \(ArbPlainDate d) ->
          PD.getYear (PDB.endOfYear d) === PD.getYear d

  describe "ZonedDateTime.Boundary" do
    describe "startOfWeek" do
      it "navigates to Monday start" do
        -- 2024-06-20 is a Thursday
        case ZDT.fromString "2024-06-20T14:30:00-04:00[America/New_York]" of
          Nothing -> pure unit
          Just zdt -> do
            let result = ZDTB.startOfWeek zdt
            ZDT.getYear result `shouldEqual` 2024
            ZDT.getMonth result `shouldEqual` 6
            ZDT.getDay result `shouldEqual` 17
            ZDT.getHour result `shouldEqual` 0

    describe "endOfWeek" do
      it "navigates to Sunday end with full precision" do
        case ZDT.fromString "2024-06-20T14:30:00-04:00[America/New_York]" of
          Nothing -> pure unit
          Just zdt -> do
            let result = ZDTB.endOfWeek zdt
            ZDT.getYear result `shouldEqual` 2024
            ZDT.getMonth result `shouldEqual` 6
            ZDT.getDay result `shouldEqual` 23
            ZDT.getHour result `shouldEqual` 23
            ZDT.getMinute result `shouldEqual` 59
            ZDT.getSecond result `shouldEqual` 59
            ZDT.getMillisecond result `shouldEqual` 999
            ZDT.getMicrosecond result `shouldEqual` 999
            ZDT.getNanosecond result `shouldEqual` 999

    describe "startOfMonth" do
      it "navigates to first of month" do
        case ZDT.fromString "2024-06-15T14:30:00-04:00[America/New_York]" of
          Nothing -> pure unit
          Just zdt -> do
            let result = ZDTB.startOfMonth zdt
            ZDT.getMonth result `shouldEqual` 6
            ZDT.getDay result `shouldEqual` 1
            ZDT.getHour result `shouldEqual` 0

    describe "endOfMonth" do
      it "navigates to last of month" do
        case ZDT.fromString "2024-06-15T14:30:00-04:00[America/New_York]" of
          Nothing -> pure unit
          Just zdt -> do
            let result = ZDTB.endOfMonth zdt
            ZDT.getMonth result `shouldEqual` 6
            ZDT.getDay result `shouldEqual` 30
            ZDT.getHour result `shouldEqual` 23

    describe "startOfYear" do
      it "navigates to January 1" do
        case ZDT.fromString "2024-06-15T14:30:00-04:00[America/New_York]" of
          Nothing -> pure unit
          Just zdt -> do
            let result = ZDTB.startOfYear zdt
            ZDT.getYear result `shouldEqual` 2024
            ZDT.getMonth result `shouldEqual` 1
            ZDT.getDay result `shouldEqual` 1

    describe "endOfYear" do
      it "navigates to December 31" do
        case ZDT.fromString "2024-06-15T14:30:00-04:00[America/New_York]" of
          Nothing -> pure unit
          Just zdt -> do
            let result = ZDTB.endOfYear zdt
            ZDT.getMonth result `shouldEqual` 12
            ZDT.getDay result `shouldEqual` 31

    describe "timezone preservation" do
      it "preserves timezone across boundaries" do
        case ZDT.fromString "2024-06-15T14:30:00+09:00[Asia/Tokyo]" of
          Nothing -> pure unit
          Just zdt -> do
            ZDT.getTimeZoneId (ZDTB.startOfWeek zdt) `shouldEqual` "Asia/Tokyo"
            ZDT.getTimeZoneId (ZDTB.endOfWeek zdt) `shouldEqual` "Asia/Tokyo"
            ZDT.getTimeZoneId (ZDTB.startOfMonth zdt) `shouldEqual` "Asia/Tokyo"
            ZDT.getTimeZoneId (ZDTB.endOfMonth zdt) `shouldEqual` "Asia/Tokyo"
            ZDT.getTimeZoneId (ZDTB.startOfYear zdt) `shouldEqual` "Asia/Tokyo"
            ZDT.getTimeZoneId (ZDTB.endOfYear zdt) `shouldEqual` "Asia/Tokyo"

    describe "DST-aware" do
      it "startOfMonth near spring-forward" do
        -- March 2024 in America/New_York: DST spring forward on March 10
        case ZDT.fromString "2024-03-15T12:00:00-04:00[America/New_York]" of
          Nothing -> pure unit
          Just zdt -> do
            let result = ZDTB.startOfMonth zdt
            ZDT.getMonth result `shouldEqual` 3
            ZDT.getDay result `shouldEqual` 1
            -- March 1 is before DST, so offset should be -05:00
            ZDT.getOffset result `shouldEqual` "-05:00"

      it "startOfMonth after spring-forward preserves correct offset" do
        case ZDT.fromString "2024-04-15T12:00:00-04:00[America/New_York]" of
          Nothing -> pure unit
          Just zdt -> do
            let result = ZDTB.startOfMonth zdt
            ZDT.getMonth result `shouldEqual` 4
            ZDT.getDay result `shouldEqual` 1
            -- April 1 is after DST, offset should be -04:00
            ZDT.getOffset result `shouldEqual` "-04:00"

      it "endOfMonth across DST boundary" do
        -- March 2024: DST spring-forward on March 10
        -- endOfMonth from March 15 should give March 31 23:59:59.999999999
        case ZDT.fromString "2024-03-15T12:00:00-04:00[America/New_York]" of
          Nothing -> pure unit
          Just zdt -> do
            let result = ZDTB.endOfMonth zdt
            ZDT.getMonth result `shouldEqual` 3
            ZDT.getDay result `shouldEqual` 31
            ZDT.getHour result `shouldEqual` 23
            ZDT.getSecond result `shouldEqual` 59
            ZDT.getNanosecond result `shouldEqual` 999
            -- March 31 is after DST, offset -04:00
            ZDT.getOffset result `shouldEqual` "-04:00"

      it "endOfYear full precision" do
        case ZDT.fromString "2024-06-15T14:30:00-04:00[America/New_York]" of
          Nothing -> pure unit
          Just zdt -> do
            let result = ZDTB.endOfYear zdt
            ZDT.getMonth result `shouldEqual` 12
            ZDT.getDay result `shouldEqual` 31
            ZDT.getHour result `shouldEqual` 23
            ZDT.getMinute result `shouldEqual` 59
            ZDT.getSecond result `shouldEqual` 59
            ZDT.getMillisecond result `shouldEqual` 999
            ZDT.getMicrosecond result `shouldEqual` 999
            ZDT.getNanosecond result `shouldEqual` 999

      it "fall-back DST: startOfMonth November" do
        -- November 3, 2024 is fall-back in America/New_York
        case ZDT.fromString "2024-11-15T12:00:00-05:00[America/New_York]" of
          Nothing -> pure unit
          Just zdt -> do
            let result = ZDTB.startOfMonth zdt
            ZDT.getMonth result `shouldEqual` 11
            ZDT.getDay result `shouldEqual` 1
            -- Nov 1 is still in EDT (-04:00), fall-back is Nov 3
            ZDT.getOffset result `shouldEqual` "-04:00"

      it "fall-back DST: endOfMonth November" do
        case ZDT.fromString "2024-11-01T12:00:00-04:00[America/New_York]" of
          Nothing -> pure unit
          Just zdt -> do
            let result = ZDTB.endOfMonth zdt
            ZDT.getMonth result `shouldEqual` 11
            ZDT.getDay result `shouldEqual` 30
            -- Nov 30 is in EST (-05:00), after fall-back
            ZDT.getOffset result `shouldEqual` "-05:00"

    describe "boundary crossing" do
      it "startOfWeek crosses month boundary" do
        -- 2024-07-01 is Monday, startOfWeek should be July 1 itself
        case ZDT.fromString "2024-07-03T12:00:00-04:00[America/New_York]" of
          Nothing -> pure unit
          Just zdt -> do
            let result = ZDTB.startOfWeek zdt
            ZDT.getMonth result `shouldEqual` 7
            ZDT.getDay result `shouldEqual` 1

      it "endOfWeek crosses month boundary" do
        -- 2024-06-27 is Thursday, endOfWeek (Sunday) is June 30
        case ZDT.fromString "2024-06-27T12:00:00-04:00[America/New_York]" of
          Nothing -> pure unit
          Just zdt -> do
            let result = ZDTB.endOfWeek zdt
            ZDT.getMonth result `shouldEqual` 6
            ZDT.getDay result `shouldEqual` 30

      it "startOfYear crosses DST boundary" do
        -- From June (EDT) to January (EST)
        case ZDT.fromString "2024-06-15T12:00:00-04:00[America/New_York]" of
          Nothing -> pure unit
          Just zdt -> do
            let result = ZDTB.startOfYear zdt
            ZDT.getMonth result `shouldEqual` 1
            ZDT.getDay result `shouldEqual` 1
            -- January is EST
            ZDT.getOffset result `shouldEqual` "-05:00"
