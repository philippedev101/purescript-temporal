module Test.Temporal.PredicateSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Temporal.Instant as I
import Temporal.PlainDate as PD
import Temporal.PlainDateTime as PDT
import Temporal.Predicate (sameDay, sameMonth, sameYear, isBetweenDates, sameDayDateTime, isBetweenDateTimes, isBetweenInstants)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Temporal.Generators (ArbPlainDate(..), ArbPlainDateTime(..), ArbInstant(..))

pd :: Int -> Int -> Int -> PD.PlainDate
pd y m d = case PD.plainDate y m d of
  Just x -> x
  Nothing -> pd 2024 1 1

dt :: Int -> Int -> Int -> Int -> Int -> PDT.PlainDateTime
dt y mo d h mi = case PDT.plainDateTime PDT.defaultPlainDateTimeFields
  { year = y, month = mo, day = d, hour = h, minute = mi } of
  Just x -> x
  Nothing -> dt 2024 1 1 0 0

spec :: Spec Unit
spec = describe "Temporal.Predicate" do
  describe "sameDay" do
    it "same date" do
      sameDay (pd 2024 3 15) (pd 2024 3 15) `shouldEqual` true

    it "different day" do
      sameDay (pd 2024 3 15) (pd 2024 3 16) `shouldEqual` false

  describe "sameMonth" do
    it "same month and year" do
      sameMonth (pd 2024 3 1) (pd 2024 3 31) `shouldEqual` true

    it "same month different year" do
      sameMonth (pd 2024 3 15) (pd 2025 3 15) `shouldEqual` false

    it "different month" do
      sameMonth (pd 2024 3 15) (pd 2024 4 15) `shouldEqual` false

  describe "sameYear" do
    it "same year" do
      sameYear (pd 2024 1 1) (pd 2024 12 31) `shouldEqual` true

    it "different year" do
      sameYear (pd 2024 1 1) (pd 2025 1 1) `shouldEqual` false

  describe "isBetweenDates" do
    it "inside range" do
      isBetweenDates (pd 2024 3 15) (pd 2024 3 1) (pd 2024 3 31) `shouldEqual` true

    it "at lower bound (inclusive)" do
      isBetweenDates (pd 2024 3 1) (pd 2024 3 1) (pd 2024 3 31) `shouldEqual` true

    it "at upper bound (inclusive)" do
      isBetweenDates (pd 2024 3 31) (pd 2024 3 1) (pd 2024 3 31) `shouldEqual` true

    it "before range" do
      isBetweenDates (pd 2024 2 28) (pd 2024 3 1) (pd 2024 3 31) `shouldEqual` false

    it "after range" do
      isBetweenDates (pd 2024 4 1) (pd 2024 3 1) (pd 2024 3 31) `shouldEqual` false

  describe "sameDayDateTime" do
    it "same day different times" do
      sameDayDateTime (dt 2024 3 15 9 0) (dt 2024 3 15 17 30) `shouldEqual` true

    it "different days" do
      sameDayDateTime (dt 2024 3 15 23 59) (dt 2024 3 16 0 0) `shouldEqual` false

  describe "isBetweenDateTimes" do
    it "inside range" do
      isBetweenDateTimes (dt 2024 3 15 12 0) (dt 2024 3 15 9 0) (dt 2024 3 15 17 0) `shouldEqual` true

    it "outside range" do
      isBetweenDateTimes (dt 2024 3 15 8 0) (dt 2024 3 15 9 0) (dt 2024 3 15 17 0) `shouldEqual` false

  describe "isBetweenInstants" do
    it "inside range" do
      case I.fromEpochMilliseconds 100.0, I.fromEpochMilliseconds 0.0, I.fromEpochMilliseconds 200.0 of
        Just x, Just lo, Just hi -> isBetweenInstants x lo hi `shouldEqual` true
        _, _, _ -> pure unit

    it "outside range" do
      case I.fromEpochMilliseconds 300.0, I.fromEpochMilliseconds 0.0, I.fromEpochMilliseconds 200.0 of
        Just x, Just lo, Just hi -> isBetweenInstants x lo hi `shouldEqual` false
        _, _, _ -> pure unit

  describe "quickcheck" do
    it "sameDay is reflexive" $
      quickCheck \(ArbPlainDate d) -> sameDay d d

    it "sameMonth is reflexive" $
      quickCheck \(ArbPlainDate d) -> sameMonth d d

    it "sameYear is reflexive" $
      quickCheck \(ArbPlainDate d) -> sameYear d d

    it "sameDay implies sameMonth" $
      quickCheck \(ArbPlainDate a) (ArbPlainDate b) ->
        not (sameDay a b) || sameMonth a b

    it "sameMonth implies sameYear" $
      quickCheck \(ArbPlainDate a) (ArbPlainDate b) ->
        not (sameMonth a b) || sameYear a b

    it "isBetweenDates d d d is true" $
      quickCheck \(ArbPlainDate d) -> isBetweenDates d d d

    it "sameDayDateTime is reflexive" $
      quickCheck \(ArbPlainDateTime dt') -> sameDayDateTime dt' dt'
