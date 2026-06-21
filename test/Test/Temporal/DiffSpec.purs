module Test.Temporal.DiffSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Temporal.Diff (diffDays, diffWeeks, diffMonths, diffYears, diffHours, diffMinutes, diffSeconds, diffMilliseconds)
import Temporal.Instant as I
import Temporal.PlainDate as PD
import Temporal.PlainDateTime as PDT
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Temporal.Generators (ArbPlainDate(..))

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
spec = describe "Temporal.Diff" do
  describe "diffDays" do
    it "same date = 0" do
      diffDays (pd 2024 3 15) (pd 2024 3 15) `shouldEqual` 0

    it "forward" do
      diffDays (pd 2024 1 1) (pd 2024 1 15) `shouldEqual` 14

    it "backward (negative)" do
      diffDays (pd 2024 1 15) (pd 2024 1 1) `shouldEqual` (-14)

    it "across months" do
      diffDays (pd 2024 1 15) (pd 2024 3 1) `shouldEqual` 46

    it "across years" do
      diffDays (pd 2023 12 31) (pd 2024 1 1) `shouldEqual` 1

  describe "diffWeeks" do
    it "exactly 2 weeks" do
      diffWeeks (pd 2024 3 4) (pd 2024 3 18) `shouldEqual` 2

    it "rounds toward zero (not a full week)" do
      diffWeeks (pd 2024 3 4) (pd 2024 3 10) `shouldEqual` 0

    it "exactly 1 week" do
      diffWeeks (pd 2024 3 4) (pd 2024 3 11) `shouldEqual` 1

  describe "diffMonths" do
    it "same month = 0" do
      diffMonths (pd 2024 3 1) (pd 2024 3 31) `shouldEqual` 0

    it "one month" do
      diffMonths (pd 2024 1 1) (pd 2024 2 1) `shouldEqual` 1

    it "across year boundary" do
      diffMonths (pd 2023 11 1) (pd 2024 2 1) `shouldEqual` 3

    it "backward" do
      diffMonths (pd 2024 6 1) (pd 2024 3 1) `shouldEqual` (-3)

  describe "diffYears" do
    it "same year = 0" do
      diffYears (pd 2024 1 1) (pd 2024 12 31) `shouldEqual` 0

    it "one year" do
      diffYears (pd 2024 1 1) (pd 2025 1 1) `shouldEqual` 1

    it "multiple years" do
      diffYears (pd 2020 6 15) (pd 2024 6 15) `shouldEqual` 4

    it "backward" do
      diffYears (pd 2024 1 1) (pd 2020 1 1) `shouldEqual` (-4)

  describe "diffHours" do
    it "same time = 0" do
      diffHours (dt 2024 1 1 12 0) (dt 2024 1 1 12 0) `shouldEqual` 0.0

    it "forward" do
      diffHours (dt 2024 1 1 9 0) (dt 2024 1 1 17 0) `shouldEqual` 8.0

    it "across midnight" do
      diffHours (dt 2024 1 1 22 0) (dt 2024 1 2 6 0) `shouldEqual` 8.0

    it "backward" do
      diffHours (dt 2024 1 1 17 0) (dt 2024 1 1 9 0) `shouldEqual` (-8.0)

  describe "diffMinutes" do
    it "90 minutes" do
      diffMinutes (dt 2024 1 1 9 0) (dt 2024 1 1 10 30) `shouldEqual` 90.0

  describe "diffSeconds" do
    it "3600 seconds = 1 hour" do
      diffSeconds (dt 2024 1 1 9 0) (dt 2024 1 1 10 0) `shouldEqual` 3600.0

  describe "diffMilliseconds" do
    it "1 hour in milliseconds" do
      case I.fromEpochMilliseconds 0.0, I.fromEpochMilliseconds 3600000.0 of
        Just a, Just b -> diffMilliseconds a b `shouldEqual` 3600000.0
        _, _ -> pure unit

    it "backward" do
      case I.fromEpochMilliseconds 3600000.0, I.fromEpochMilliseconds 0.0 of
        Just a, Just b -> diffMilliseconds a b `shouldEqual` (-3600000.0)
        _, _ -> pure unit

  describe "quickcheck" do
    it "diffDays a a == 0" $
      quickCheck \(ArbPlainDate d) ->
        diffDays d d == 0

    it "diffDays a b == negate (diffDays b a)" $
      quickCheck \(ArbPlainDate a) (ArbPlainDate b) ->
        diffDays a b == negate (diffDays b a)

    it "diffMonths a b == negate (diffMonths b a)" $
      quickCheck \(ArbPlainDate a) (ArbPlainDate b) ->
        diffMonths a b == negate (diffMonths b a)

    it "diffYears a b == negate (diffYears b a)" $
      quickCheck \(ArbPlainDate a) (ArbPlainDate b) ->
        diffYears a b == negate (diffYears b a)
