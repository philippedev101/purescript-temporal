module Test.Temporal.Format.HtmlSpec where

import Prelude

import Data.Maybe (Maybe(..), isNothing)
import Temporal.Format.Html (toHtmlDate, fromHtmlDate, toHtmlTime, fromHtmlTime, toHtmlDateTime, fromHtmlDateTime, toHtmlMonth, fromHtmlMonth)
import Temporal.PlainDate as PD
import Temporal.PlainDateTime as PDT
import Temporal.PlainTime as PT
import Temporal.PlainYearMonth as PYM
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck ((===))
import Test.Temporal.Generators (ArbPlainDate(..), ArbPlainDateTime(..), ArbPlainTime(..), ArbPlainYearMonth(..))

spec :: Spec Unit
spec = describe "Temporal.Format.Html" do
  describe "date" do
    it "formats YYYY-MM-DD" do
      case PD.plainDate 2024 3 15 of
        Nothing -> pure unit
        Just d -> toHtmlDate d `shouldEqual` "2024-03-15"

    it "pads single-digit month and day" do
      case PD.plainDate 2024 1 5 of
        Nothing -> pure unit
        Just d -> toHtmlDate d `shouldEqual` "2024-01-05"

    it "parses valid date" do
      case PD.plainDate 2024 12 25 of
        Nothing -> pure unit
        Just expected -> fromHtmlDate "2024-12-25" `shouldEqual` Just expected

    it "rejects invalid date" do
      fromHtmlDate "not-a-date" `shouldSatisfy` isNothing
      fromHtmlDate "" `shouldSatisfy` isNothing

    it "round-trips" $
      quickCheck \(ArbPlainDate d) ->
        fromHtmlDate (toHtmlDate d) === Just d

  describe "time" do
    it "formats HH:MM" do
      case PT.plainTime { hour: 14, minute: 30, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 } of
        Nothing -> pure unit
        Just t -> toHtmlTime t `shouldEqual` "14:30"

    it "pads single-digit hour and minute" do
      case PT.plainTime { hour: 9, minute: 5, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 } of
        Nothing -> pure unit
        Just t -> toHtmlTime t `shouldEqual` "09:05"

    it "truncates seconds" do
      case PT.plainTime { hour: 14, minute: 30, second: 45, millisecond: 123, microsecond: 456, nanosecond: 789 } of
        Nothing -> pure unit
        Just t -> toHtmlTime t `shouldEqual` "14:30"

    it "formats midnight" do
      case PT.plainTime { hour: 0, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 } of
        Nothing -> pure unit
        Just t -> toHtmlTime t `shouldEqual` "00:00"

    it "parses HH:MM" do
      fromHtmlTime "14:30" `shouldSatisfy` \r -> case r of
        Just t -> PT.getHour t == 14 && PT.getMinute t == 30
        Nothing -> false

    it "parses midnight 00:00" do
      fromHtmlTime "00:00" `shouldSatisfy` \r -> case r of
        Just t -> PT.getHour t == 0 && PT.getMinute t == 0
        Nothing -> false

    it "parses HH:MM:SS (browser with step)" do
      fromHtmlTime "14:30:45" `shouldSatisfy` \r -> case r of
        Just t -> PT.getHour t == 14 && PT.getMinute t == 30 && PT.getSecond t == 45
        Nothing -> false

    it "rejects invalid time" do
      fromHtmlTime "25:00" `shouldSatisfy` isNothing
      fromHtmlTime "" `shouldSatisfy` isNothing

    it "round-trips (truncating seconds)" $
      quickCheck \(ArbPlainTime t) ->
        case fromHtmlTime (toHtmlTime t) of
          Nothing -> false
          Just parsed -> PT.getHour parsed == PT.getHour t
            && PT.getMinute parsed == PT.getMinute t

  describe "datetime-local" do
    it "formats YYYY-MM-DDTHH:MM" do
      case PDT.plainDateTime PDT.defaultPlainDateTimeFields
        { year = 2024, month = 3, day = 15, hour = 14, minute = 30 } of
        Nothing -> pure unit
        Just dt -> toHtmlDateTime dt `shouldEqual` "2024-03-15T14:30"

    it "pads all components" do
      case PDT.plainDateTime PDT.defaultPlainDateTimeFields
        { year = 2024, month = 1, day = 5, hour = 9, minute = 5 } of
        Nothing -> pure unit
        Just dt -> toHtmlDateTime dt `shouldEqual` "2024-01-05T09:05"

    it "truncates seconds" do
      case PDT.plainDateTime PDT.defaultPlainDateTimeFields
        { year = 2024, month = 3, day = 15, hour = 14, minute = 30
        , second = 45, millisecond = 123 } of
        Nothing -> pure unit
        Just dt -> toHtmlDateTime dt `shouldEqual` "2024-03-15T14:30"

    it "parses YYYY-MM-DDTHH:MM" do
      case fromHtmlDateTime "2024-03-15T14:30" of
        Nothing -> pure unit
        Just dt -> do
          PDT.getYear dt `shouldEqual` 2024
          PDT.getMonth dt `shouldEqual` 3
          PDT.getDay dt `shouldEqual` 15
          PDT.getHour dt `shouldEqual` 14
          PDT.getMinute dt `shouldEqual` 30

    it "formats midnight datetime" do
      case PDT.plainDateTime PDT.defaultPlainDateTimeFields
        { year = 2024, month = 1, day = 1 } of
        Nothing -> pure unit
        Just dt -> toHtmlDateTime dt `shouldEqual` "2024-01-01T00:00"

    it "formats small year" do
      case PDT.plainDateTime PDT.defaultPlainDateTimeFields
        { year = 100, month = 6, day = 15, hour = 12, minute = 0 } of
        Nothing -> pure unit
        Just dt -> toHtmlDateTime dt `shouldEqual` "0100-06-15T12:00"

    it "rejects invalid datetime" do
      fromHtmlDateTime "not-a-datetime" `shouldSatisfy` isNothing
      fromHtmlDateTime "" `shouldSatisfy` isNothing

    it "round-trips (truncating seconds)" $
      quickCheck \(ArbPlainDateTime dt) ->
        case fromHtmlDateTime (toHtmlDateTime dt) of
          Nothing -> false
          Just parsed -> PDT.getYear parsed == PDT.getYear dt
            && PDT.getMonth parsed == PDT.getMonth dt
            && PDT.getDay parsed == PDT.getDay dt
            && PDT.getHour parsed == PDT.getHour dt
            && PDT.getMinute parsed == PDT.getMinute dt

  describe "month" do
    it "formats YYYY-MM" do
      case PYM.plainYearMonth 2024 3 of
        Nothing -> pure unit
        Just ym -> toHtmlMonth ym `shouldEqual` "2024-03"

    it "pads single-digit month" do
      case PYM.plainYearMonth 2024 1 of
        Nothing -> pure unit
        Just ym -> toHtmlMonth ym `shouldEqual` "2024-01"

    it "parses valid month" do
      case PYM.plainYearMonth 2024 12 of
        Nothing -> pure unit
        Just expected -> fromHtmlMonth "2024-12" `shouldEqual` Just expected

    it "rejects invalid month" do
      fromHtmlMonth "not-a-month" `shouldSatisfy` isNothing
      fromHtmlMonth "" `shouldSatisfy` isNothing

    it "round-trips" do
      case PYM.plainYearMonth 2024 6 of
        Nothing -> pure unit
        Just ym -> fromHtmlMonth (toHtmlMonth ym) `shouldEqual` Just ym

    it "round-trips (quickcheck)" $
      quickCheck \(ArbPlainYearMonth ym) ->
        fromHtmlMonth (toHtmlMonth ym) === Just ym
