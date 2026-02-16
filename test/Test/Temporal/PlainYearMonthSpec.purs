module Test.Temporal.PlainYearMonthSpec where

import Prelude

import Data.Maybe (Maybe(..), isJust, isNothing)
import Temporal.Duration as D
import Temporal.PlainYearMonth as PYM
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = describe "Temporal.PlainYearMonth" do
  describe "construction" do
    it "creates from year/month" do
      PYM.plainYearMonth 2024 3 `shouldSatisfy` isJust

    it "creates from string" do
      PYM.fromString "2024-03" `shouldSatisfy` isJust

    it "rejects invalid" do
      PYM.plainYearMonth 2024 13 `shouldSatisfy` isNothing
      PYM.fromString "not valid" `shouldSatisfy` isNothing

  describe "properties" do
    it "reads fields" do
      case PYM.plainYearMonth 2024 3 of
        Nothing -> pure unit
        Just ym -> do
          PYM.getYear ym `shouldEqual` 2024
          PYM.getMonth ym `shouldEqual` 3
          PYM.getDaysInMonth ym `shouldEqual` 31
          PYM.getInLeapYear ym `shouldEqual` true

    it "February in leap year" do
      case PYM.plainYearMonth 2024 2 of
        Nothing -> pure unit
        Just ym -> PYM.getDaysInMonth ym `shouldEqual` 29

    it "February in non-leap year" do
      case PYM.plainYearMonth 2023 2 of
        Nothing -> pure unit
        Just ym -> PYM.getDaysInMonth ym `shouldEqual` 28

  describe "arithmetic" do
    it "adds months" do
      case PYM.plainYearMonth 2024 11 of
        Nothing -> pure unit
        Just ym -> do
          case PYM.add (D.months 3.0) ym of
            Nothing -> pure unit
            Just ym2 -> do
              PYM.getYear ym2 `shouldEqual` 2025
              PYM.getMonth ym2 `shouldEqual` 2

  describe "comparison" do
    it "compares correctly" do
      case PYM.plainYearMonth 2024 1, PYM.plainYearMonth 2024 12 of
        Just ym1, Just ym2 -> (ym1 < ym2) `shouldEqual` true
        _, _ -> pure unit

  describe "diff" do
    it "calculates difference" do
      case PYM.plainYearMonth 2024 1, PYM.plainYearMonth 2024 6 of
        Just ym1, Just ym2 -> do
          let dur = PYM.until' ym1 ym2
          D.getMonths dur `shouldEqual` 5.0
        _, _ -> pure unit

  describe "conversion" do
    it "converts to PlainDate" do
      case PYM.plainYearMonth 2024 3 of
        Nothing -> pure unit
        Just ym -> PYM.toPlainDate 15 ym `shouldSatisfy` isJust

    it "constrains out-of-range day" do
      case PYM.plainYearMonth 2024 2 of
        Nothing -> pure unit
        Just ym -> PYM.toPlainDate 30 ym `shouldSatisfy` isJust
