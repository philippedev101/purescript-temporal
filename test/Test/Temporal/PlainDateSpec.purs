module Test.Temporal.PlainDateSpec where

import Prelude

import Data.Maybe (Maybe(..), isJust, isNothing)
import Temporal.Duration as D
import Temporal.PlainDate as PD
import Temporal.PlainDate.Extra as PDX
import Temporal.PlainYearMonth as PYM
import Temporal.PlainMonthDay as PMD
import Temporal.Internal.Options (Overflow(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck ((===))
import Test.Temporal.Generators (ArbPlainDate(..))

spec :: Spec Unit
spec = describe "Temporal.PlainDate" do
  describe "construction" do
    it "creates from year/month/day" do
      PD.plainDate 2024 3 15 `shouldSatisfy` isJust

    it "creates from string" do
      PD.fromString "2024-03-15" `shouldSatisfy` isJust

    it "rejects invalid dates" do
      PD.plainDate 2024 13 1 `shouldSatisfy` isNothing
      PD.fromString "not a date" `shouldSatisfy` isNothing

    it "constrains overflow" do
      case PD.plainDateWith Constrain 2024 2 31 of
        Nothing -> pure unit
        Just d -> PD.getDay d `shouldEqual` 29 -- 2024 is leap year

    it "rejects overflow" do
      PD.plainDateWith Reject 2024 2 30 `shouldSatisfy` isNothing

  describe "properties" do
    it "reads basic fields" do
      case PD.plainDate 2024 3 15 of
        Nothing -> pure unit
        Just d -> do
          PD.getYear d `shouldEqual` 2024
          PD.getMonth d `shouldEqual` 3
          PD.getDay d `shouldEqual` 15

    it "reads computed properties" do
      case PD.plainDate 2024 3 15 of
        Nothing -> pure unit
        Just d -> do
          PD.getDayOfWeek d `shouldEqual` 5 -- Friday
          PD.getDaysInMonth d `shouldEqual` 31
          PD.getInLeapYear d `shouldEqual` true

    it "non-leap year" do
      case PD.plainDate 2023 2 1 of
        Nothing -> pure unit
        Just d -> do
          PD.getInLeapYear d `shouldEqual` false
          PD.getDaysInMonth d `shouldEqual` 28

  describe "arithmetic" do
    it "adds duration" do
      case PD.plainDate 2024 1 31 of
        Nothing -> pure unit
        Just d -> do
          case PD.add (D.months 1.0) d of
            Nothing -> pure unit
            Just d2 -> do
              PD.getMonth d2 `shouldEqual` 2
              PD.getDay d2 `shouldEqual` 29 -- 2024 is leap year

    it "subtracts duration" do
      case PD.plainDate 2024 3 1 of
        Nothing -> pure unit
        Just d -> do
          case PD.subtract (D.days 1.0) d of
            Nothing -> pure unit
            Just d2 -> do
              PD.getMonth d2 `shouldEqual` 2
              PD.getDay d2 `shouldEqual` 29

  describe "comparison" do
    it "compares dates correctly" do
      case PD.plainDate 2024 1 1, PD.plainDate 2024 12 31 of
        Just d1, Just d2 -> do
          (d1 < d2) `shouldEqual` true
          (d1 == d2) `shouldEqual` false
        _, _ -> pure unit

  describe "diff" do
    it "calculates difference" do
      case PD.plainDate 2024 1 1, PD.plainDate 2024 3 15 of
        Just d1, Just d2 -> do
          let dur = PDX.until' d1 d2
          D.getMonths dur `shouldEqual` 2.0
          D.getDays dur `shouldEqual` 14.0
        _, _ -> pure unit

  describe "conversions" do
    it "converts to PlainYearMonth" do
      case PD.plainDate 2024 3 15 of
        Nothing -> pure unit
        Just d -> do
          _ <- pure $ PD.toPlainYearMonth d
          pure unit

    it "converts to PlainMonthDay" do
      case PD.plainDate 2024 3 15 of
        Nothing -> pure unit
        Just d -> do
          _ <- pure $ PD.toPlainMonthDay d
          pure unit

  describe "properties" do
    it "field round-trip: getYear/getMonth/getDay -> plainDate -> same fields" $
      quickCheck \(ArbPlainDate d) ->
        PD.plainDate (PD.getYear d) (PD.getMonth d) (PD.getDay d) == Just d

    it "toString / fromString round-trip" $
      quickCheck \(ArbPlainDate d) ->
        PD.fromString (PD.toString d) === Just d

    it "compare a a == EQ" $
      quickCheck \(ArbPlainDate d) ->
        compare d d === EQ

    it "toPlainYearMonth preserves year and month" $
      quickCheck \(ArbPlainDate d) ->
        let ym = PD.toPlainYearMonth d
        in PYM.getYear ym == PD.getYear d
          && PYM.getMonth ym == PD.getMonth d

    it "toPlainMonthDay preserves day" $
      quickCheck \(ArbPlainDate d) ->
        let md = PD.toPlainMonthDay d
        in PMD.getDay md == PD.getDay d
