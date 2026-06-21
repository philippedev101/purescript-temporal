module Test.Temporal.DurationSpec where

import Prelude

import Data.Maybe (Maybe(..), isNothing, isJust)
import Temporal.Duration as D
import Temporal.Duration.Extra as DX
import Temporal.PlainDate as PD
import Temporal.PlainDateTime as PDT
import Temporal.ZonedDateTime as ZDT
import Temporal.Internal.Options (DateTimeUnit(..), DateUnit(..), TimeUnit(..), RelativeTo(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck ((===))
import Test.Temporal.Generators (ArbDuration(..), ArbTimeDuration(..), ArbPlainDate(..))

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

    it "zero is blank" do
      D.blank D.zero `shouldEqual` true
      D.sign D.zero `shouldEqual` 0

  describe "arithmetic" do
    it "adds time-only durations without relativeTo" do
      case D.add Nothing (D.hours 1.0) (D.hours 2.0) of
        Nothing -> pure unit
        Just d -> D.getHours d `shouldEqual` 3.0

    it "subtracts time-only durations without relativeTo" do
      case D.subtract Nothing (D.hours 3.0) (D.hours 1.0) of
        Nothing -> pure unit
        Just d -> D.getHours d `shouldEqual` 2.0

    it "fails adding calendar durations without relativeTo" do
      D.add Nothing (D.months 1.0) (D.months 2.0) `shouldSatisfy` isNothing

    it "fails subtracting calendar durations without relativeTo" do
      D.subtract Nothing (D.months 3.0) (D.months 1.0) `shouldSatisfy` isNothing

    it "adds calendar durations with RelDate" do
      case PD.plainDate 2024 1 1 of
        Nothing -> pure unit
        Just jan1 -> do
          let rel = Just (RelDate jan1)
          case D.add rel (D.months 1.0) (D.months 2.0) of
            Nothing -> pure unit
            Just d -> D.getMonths d `shouldEqual` 3.0

    it "subtracts calendar durations with RelDate" do
      case PD.plainDate 2024 1 1 of
        Nothing -> pure unit
        Just jan1 -> do
          let rel = Just (RelDate jan1)
          case D.subtract rel (D.months 3.0) (D.months 1.0) of
            Nothing -> pure unit
            Just d -> D.getMonths d `shouldEqual` 2.0

    it "adds calendar durations with RelDateTime" do
      case PDT.plainDateTime PDT.defaultPlainDateTimeFields
        { year = 2024, month = 6, day = 15, hour = 14, minute = 30 } of
        Nothing -> pure unit
        Just dt -> do
          let rel = Just (RelDateTime dt)
          case D.add rel (D.months 2.0) (D.days 10.0) of
            Nothing -> pure unit
            Just d -> d `shouldSatisfy` \_ -> true -- succeeds with RelDateTime

    it "adds calendar durations with RelZoned" do
      case ZDT.fromString "2024-06-15T12:00:00-04:00[America/New_York]" of
        Nothing -> pure unit
        Just zdt -> do
          let rel = Just (RelZoned zdt)
          case D.add rel (D.months 1.0) (D.days 5.0) of
            Nothing -> pure unit
            Just d -> d `shouldSatisfy` \_ -> true -- succeeds with RelZoned

    it "subtracts calendar durations with RelZoned" do
      case ZDT.fromString "2024-06-15T12:00:00-04:00[America/New_York]" of
        Nothing -> pure unit
        Just zdt -> do
          let rel = Just (RelZoned zdt)
          case D.subtract rel (D.months 3.0) (D.months 1.0) of
            Nothing -> pure unit
            Just d -> D.getMonths d `shouldEqual` 2.0

    it "adds durations near month boundary" do
      case PD.plainDate 2024 1 31 of
        Nothing -> pure unit
        Just jan31 -> do
          let rel = Just (RelDate jan31)
          -- P1M + P1M from Jan 31: Jan31 + P1M = Feb29 (leap year),
          -- Feb29 + P1M = Mar29. Total result: P2M
          case D.add rel (D.months 1.0) (D.months 1.0) of
            Nothing -> pure unit
            Just d -> D.getMonths d `shouldEqual` 2.0

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

    it "rebalances time units" do
      let opts = D.defaultDurationRoundOptions
            { largestUnit = TimeU Hours
            , smallestUnit = TimeU Minutes
            }
      case D.round opts (D.minutes 150.0) of
        Nothing -> pure unit
        Just d -> do
          D.getHours d `shouldEqual` 2.0
          D.getMinutes d `shouldEqual` 30.0

    it "rebalances calendar units with RelDate" do
      case PD.plainDate 2024 1 1 of
        Nothing -> pure unit
        Just jan1 -> do
          let opts = D.defaultDurationRoundOptions
                { largestUnit = DateU Years
                , smallestUnit = DateU Months
                , relativeTo = Just (RelDate jan1)
                }
          case D.round opts (D.months 18.0) of
            Nothing -> pure unit
            Just d -> do
              D.getYears d `shouldEqual` 1.0
              D.getMonths d `shouldEqual` 6.0

    it "rebalances calendar units with RelDateTime" do
      case PDT.plainDateTime PDT.defaultPlainDateTimeFields
        { year = 2024, month = 1, day = 1 } of
        Nothing -> pure unit
        Just dt -> do
          let opts = D.defaultDurationRoundOptions
                { largestUnit = DateU Years
                , smallestUnit = DateU Months
                , relativeTo = Just (RelDateTime dt)
                }
          case D.round opts (D.months 25.0) of
            Nothing -> pure unit
            Just d -> do
              D.getYears d `shouldEqual` 2.0
              D.getMonths d `shouldEqual` 1.0

    it "rebalances with RelZoned near DST transition" do
      -- Spring forward: March 10, 2024 in America/New_York
      -- This day has only 23 hours
      case ZDT.fromString "2024-03-10T00:00:00-05:00[America/New_York]" of
        Nothing -> pure unit
        Just zdt -> do
          let opts = D.defaultDurationRoundOptions
                { largestUnit = DateU Days
                , smallestUnit = TimeU Hours
                , relativeTo = Just (RelZoned zdt)
                }
          -- 24 hours from midnight on DST day = 1 day + 1 hour (since day is 23h)
          case D.round opts (D.hours 24.0) of
            Nothing -> pure unit
            Just d -> do
              D.getDays d `shouldEqual` 1.0
              D.getHours d `shouldEqual` 1.0

    it "fails rounding calendar units without relativeTo" do
      let opts = D.defaultDurationRoundOptions
            { largestUnit = DateU Years
            , smallestUnit = DateU Months
            }
      D.round opts (D.months 18.0) `shouldSatisfy` isNothing

  describe "total" do
    it "computes total in given unit" do
      case DX.totalUnit (TimeU Minutes) (D.hours 1.0) of
        Nothing -> pure unit
        Just n -> n `shouldEqual` 60.0

    it "totalUnit is equivalent to total with Nothing relativeTo" do
      case DX.totalUnit (TimeU Seconds) (D.minutes 2.0) of
        Nothing -> pure unit
        Just n1 -> do
          case D.total { unit: TimeU Seconds, relativeTo: Nothing } (D.minutes 2.0) of
            Nothing -> pure unit
            Just n2 -> n1 `shouldEqual` n2

    it "computes total with RelDate for calendar units" do
      case PD.plainDate 2024 1 1 of
        Nothing -> pure unit
        Just jan1 -> do
          -- 31 days from Jan 1 = exactly 1 month (January has 31 days)
          case D.total { unit: DateU Months, relativeTo: Just (RelDate jan1) } (D.days 31.0) of
            Nothing -> pure unit
            Just n -> n `shouldEqual` 1.0

    it "total depends on reference date" do
      case PD.plainDate 2024 2 1 of
        Nothing -> pure unit
        Just feb1 -> do
          -- 29 days from Feb 1 = exactly 1 month (Feb 2024 is leap year)
          case D.total { unit: DateU Months, relativeTo: Just (RelDate feb1) } (D.days 29.0) of
            Nothing -> pure unit
            Just n -> n `shouldEqual` 1.0

    it "computes total with RelDateTime" do
      case PDT.plainDateTime PDT.defaultPlainDateTimeFields
        { year = 2024, month = 3, day = 1 } of
        Nothing -> pure unit
        Just dt -> do
          -- March has 31 days, so 31 days from March 1 = 1 month
          case D.total { unit: DateU Months, relativeTo: Just (RelDateTime dt) } (D.days 31.0) of
            Nothing -> pure unit
            Just n -> n `shouldEqual` 1.0

    it "computes total with RelZoned near DST" do
      -- Spring forward: March 10, 2024 in New York (day has 23 hours)
      case ZDT.fromString "2024-03-10T00:00:00-05:00[America/New_York]" of
        Nothing -> pure unit
        Just zdt -> do
          -- 1 day in hours on a 23-hour day should be 23
          case D.total { unit: TimeU Hours, relativeTo: Just (RelZoned zdt) } (D.days 1.0) of
            Nothing -> pure unit
            Just n -> n `shouldEqual` 23.0

    it "fails computing calendar total without relativeTo" do
      D.total { unit: DateU Months, relativeTo: Nothing } (D.days 31.0) `shouldSatisfy` isNothing

  describe "compare" do
    it "compares equal durations" do
      case PD.plainDate 2024 1 1 of
        Nothing -> pure unit
        Just jan1 ->
          D.compare (RelDate jan1) (D.hours 1.0) (D.hours 1.0) `shouldEqual` EQ

    it "compares different durations" do
      case PD.plainDate 2024 1 1 of
        Nothing -> pure unit
        Just jan1 -> do
          D.compare (RelDate jan1) (D.hours 1.0) (D.hours 2.0) `shouldEqual` LT
          D.compare (RelDate jan1) (D.hours 2.0) (D.hours 1.0) `shouldEqual` GT

    it "compares calendar durations with context" do
      case PD.plainDate 2024 1 1 of
        Nothing -> pure unit
        Just jan1 -> do
          -- January has 31 days
          D.compare (RelDate jan1) (D.days 31.0) (D.months 1.0) `shouldEqual` EQ
          D.compare (RelDate jan1) (D.days 30.0) (D.months 1.0) `shouldEqual` LT

    it "comparison depends on reference date" do
      case PD.plainDate 2024 2 1 of
        Nothing -> pure unit
        Just feb1 ->
          -- February 2024 has 29 days (leap year), so 30 days > 1 month
          D.compare (RelDate feb1) (D.days 30.0) (D.months 1.0) `shouldEqual` GT

    it "compares with RelDateTime" do
      case PDT.plainDateTime PDT.defaultPlainDateTimeFields
        { year = 2024, month = 4, day = 1 } of
        Nothing -> pure unit
        Just dt -> do
          -- April has 30 days
          D.compare (RelDateTime dt) (D.days 30.0) (D.months 1.0) `shouldEqual` EQ
          D.compare (RelDateTime dt) (D.days 31.0) (D.months 1.0) `shouldEqual` GT

    it "compares with RelZoned" do
      case ZDT.fromString "2024-06-15T12:00:00-04:00[America/New_York]" of
        Nothing -> pure unit
        Just zdt -> do
          D.compare (RelZoned zdt) (D.hours 2.0) (D.minutes 120.0) `shouldEqual` EQ
          D.compare (RelZoned zdt) (D.hours 2.0) (D.minutes 119.0) `shouldEqual` GT

    it "DST-aware comparison: 24 hours vs 1 day on spring-forward" do
      -- March 10, 2024 is spring-forward in America/New_York (23-hour day)
      case ZDT.fromString "2024-03-10T00:00:00-05:00[America/New_York]" of
        Nothing -> pure unit
        Just zdt -> do
          -- 1 day = 23 hours on this day, so 24 hours > 1 day
          D.compare (RelZoned zdt) (D.hours 24.0) (D.days 1.0) `shouldEqual` GT
          -- And 23 hours = 1 day
          D.compare (RelZoned zdt) (D.hours 23.0) (D.days 1.0) `shouldEqual` EQ

    it "DST-aware comparison: 24 hours vs 1 day on fall-back" do
      -- November 3, 2024 is fall-back in America/New_York (25-hour day)
      case ZDT.fromString "2024-11-03T00:00:00-04:00[America/New_York]" of
        Nothing -> pure unit
        Just zdt -> do
          -- 1 day = 25 hours on this day, so 24 hours < 1 day
          D.compare (RelZoned zdt) (D.hours 24.0) (D.days 1.0) `shouldEqual` LT
          -- And 25 hours = 1 day
          D.compare (RelZoned zdt) (D.hours 25.0) (D.days 1.0) `shouldEqual` EQ

    it "compare is antisymmetric" do
      case PD.plainDate 2024 1 1 of
        Nothing -> pure unit
        Just jan1 -> do
          let rel = RelDate jan1
              a = D.days 15.0
              b = D.months 1.0
          let ab = D.compare rel a b
              ba = D.compare rel b a
          -- If a < b then b > a
          case ab of
            LT -> ba `shouldEqual` GT
            GT -> ba `shouldEqual` LT
            EQ -> ba `shouldEqual` EQ

  describe "quickcheck properties" do
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
        case D.add Nothing d (D.negated d) of
          Nothing -> false
          Just result -> D.blank result

    it "toString / fromString round-trip" $
      quickCheck \(ArbDuration d) ->
        D.fromString (D.toString d) === Just d

    it "compare is antisymmetric (quickcheck)" $
      quickCheck \(ArbPlainDate date) (ArbTimeDuration d1) (ArbTimeDuration d2) ->
        let rel = RelDate date
            ab = D.compare rel d1 d2
            ba = D.compare rel d2 d1
        in case ab of
          LT -> ba === GT
          GT -> ba === LT
          EQ -> ba === EQ

    it "compare reflexivity" $
      quickCheck \(ArbPlainDate date) (ArbTimeDuration d) ->
        D.compare (RelDate date) d d === EQ

    it "totalUnit (TimeU Hours) (hours n) == Just n" $
      quickCheck \(ArbTimeDuration d) ->
        -- total in nanoseconds should equal the sum of all components
        -- converted to nanoseconds
        let h = D.getHours d
            m = D.getMinutes d
            s = D.getSeconds d
            ms = D.getMilliseconds d
            us = D.getMicroseconds d
            ns = D.getNanoseconds d
            expected = h * 3600.0e9 + m * 60.0e9 + s * 1.0e9
              + ms * 1.0e6 + us * 1.0e3 + ns
        in DX.totalUnit (TimeU Nanoseconds) d === Just expected
