module Test.Temporal.PlainMonthDaySpec where

import Prelude

import Data.Maybe (Maybe(..), isJust, isNothing)
import Temporal.PlainMonthDay as PMD
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = describe "Temporal.PlainMonthDay" do
  describe "construction" do
    it "creates from month/day" do
      PMD.plainMonthDay 3 15 `shouldSatisfy` isJust

    it "creates from string" do
      PMD.fromString "03-15" `shouldSatisfy` isJust

    it "rejects invalid" do
      PMD.plainMonthDay 13 1 `shouldSatisfy` isNothing
      PMD.fromString "not valid" `shouldSatisfy` isNothing

  describe "properties" do
    it "reads fields" do
      case PMD.plainMonthDay 3 15 of
        Nothing -> pure unit
        Just md -> PMD.getDay md `shouldEqual` 15

  describe "equality" do
    it "equal month-days are equal" do
      case PMD.plainMonthDay 3 15, PMD.plainMonthDay 3 15 of
        Just md1, Just md2 -> (md1 == md2) `shouldEqual` true
        _, _ -> pure unit

    it "different month-days are not equal" do
      case PMD.plainMonthDay 3 15, PMD.plainMonthDay 3 16 of
        Just md1, Just md2 -> (md1 == md2) `shouldEqual` false
        _, _ -> pure unit

  describe "conversion" do
    it "converts to PlainDate with year" do
      case PMD.plainMonthDay 2 29 of
        Nothing -> pure unit
        Just md -> do
          PMD.toPlainDate 2024 md `shouldSatisfy` isJust -- leap year
          -- Non-leap year constrains Feb 29 -> Feb 28
          PMD.toPlainDate 2023 md `shouldSatisfy` isJust
