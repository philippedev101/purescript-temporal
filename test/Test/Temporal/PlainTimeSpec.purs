module Test.Temporal.PlainTimeSpec where

import Prelude

import Data.Maybe (Maybe(..), isJust, isNothing)
import Temporal.Duration as D
import Temporal.PlainTime as PT
import Temporal.PlainTime.Extra as PTX
import Temporal.Internal.Options (TimeUnit(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck ((===))
import Test.Temporal.Generators (ArbPlainTime(..), ArbTimeDuration(..))

spec :: Spec Unit
spec = describe "Temporal.PlainTime" do
  describe "construction" do
    it "creates from fields" do
      let t = PT.plainTime (PT.defaultPlainTimeFields { hour = 10, minute = 30 })
      t `shouldSatisfy` isJust

    it "creates from string" do
      PT.fromString "10:30:00" `shouldSatisfy` isJust

    it "rejects invalid times" do
      PT.plainTime (PT.defaultPlainTimeFields { hour = 25 }) `shouldSatisfy` isNothing
      PT.fromString "not a time" `shouldSatisfy` isNothing

  describe "properties" do
    it "reads all fields" do
      case PT.plainTime { hour: 10, minute: 30, second: 45, millisecond: 100, microsecond: 200, nanosecond: 300 } of
        Nothing -> pure unit
        Just t -> do
          PT.getHour t `shouldEqual` 10
          PT.getMinute t `shouldEqual` 30
          PT.getSecond t `shouldEqual` 45
          PT.getMillisecond t `shouldEqual` 100
          PT.getMicrosecond t `shouldEqual` 200
          PT.getNanosecond t `shouldEqual` 300

  describe "arithmetic" do
    it "adds duration (wraps at midnight)" do
      case PT.plainTime (PT.defaultPlainTimeFields { hour = 23, minute = 30 }) of
        Nothing -> pure unit
        Just t -> do
          let result = PT.add (D.hours 2.0) t
          PT.getHour result `shouldEqual` 1
          PT.getMinute result `shouldEqual` 30

    it "subtracts duration" do
      case PT.plainTime (PT.defaultPlainTimeFields { hour = 1, minute = 30 }) of
        Nothing -> pure unit
        Just t -> do
          let result = PT.subtract (D.hours 3.0) t
          PT.getHour result `shouldEqual` 22
          PT.getMinute result `shouldEqual` 30

  describe "diff" do
    it "calculates difference" do
      case PT.plainTime (PT.defaultPlainTimeFields { hour = 10 }), PT.plainTime (PT.defaultPlainTimeFields { hour = 12, minute = 30 }) of
        Just t1, Just t2 -> do
          let dur = PTX.until' t1 t2
          D.getHours dur `shouldEqual` 2.0
          D.getMinutes dur `shouldEqual` 30.0
        _, _ -> pure unit

  describe "comparison" do
    it "compares times correctly" do
      case PT.plainTime (PT.defaultPlainTimeFields { hour = 10 }), PT.plainTime (PT.defaultPlainTimeFields { hour = 12 }) of
        Just t1, Just t2 -> do
          (t1 < t2) `shouldEqual` true
          (t1 == t2) `shouldEqual` false
        _, _ -> pure unit

  describe "with" do
    it "creates modified copy" do
      case PT.plainTime (PT.defaultPlainTimeFields { hour = 10, minute = 30 }) of
        Nothing -> pure unit
        Just t -> do
          case PT.with (PT.defaultPlainTimeFields { hour = 10, minute = 45 }) t of
            Nothing -> pure unit
            Just t2 -> do
              PT.getHour t2 `shouldEqual` 10
              PT.getMinute t2 `shouldEqual` 45

  describe "round" do
    it "rounds to nearest hour" do
      case PT.plainTime (PT.defaultPlainTimeFields { hour = 10, minute = 45 }) of
        Nothing -> pure unit
        Just t -> do
          let rounded = PT.round (PT.defaultPlainTimeRoundOptions { smallestUnit = Hours }) t
          PT.getHour rounded `shouldEqual` 11
          PT.getMinute rounded `shouldEqual` 0

  describe "bounded" do
    it "has correct bounds" do
      PT.getHour (bottom :: PT.PlainTime) `shouldEqual` 0
      PT.getHour (top :: PT.PlainTime) `shouldEqual` 23
      PT.getMinute (top :: PT.PlainTime) `shouldEqual` 59

  describe "constants" do
    it "midnight is 00:00:00" do
      PT.getHour PTX.midnight `shouldEqual` 0
      PT.getMinute PTX.midnight `shouldEqual` 0
      PT.getSecond PTX.midnight `shouldEqual` 0

    it "midnight == bottom" do
      PTX.midnight `shouldEqual` (bottom :: PT.PlainTime)

    it "noon is 12:00:00" do
      PT.getHour PTX.noon `shouldEqual` 12
      PT.getMinute PTX.noon `shouldEqual` 0
      PT.getSecond PTX.noon `shouldEqual` 0

    it "midnight < noon" do
      (PTX.midnight < PTX.noon) `shouldEqual` true

  describe "properties" do
    it "add then subtract is identity" $
      quickCheck \(ArbTimeDuration d) (ArbPlainTime t) ->
        PT.subtract d (PT.add d t) === t

    it "until' a b == negated (since' a b)" $
      quickCheck \(ArbPlainTime a) (ArbPlainTime b) ->
        PTX.until' a b === D.negated (PTX.since' a b)

    it "add (until' a b) a == b" $
      quickCheck \(ArbPlainTime a) (ArbPlainTime b) ->
        PT.add (PTX.until' a b) a === b

    it "toString / fromString round-trip" $
      quickCheck \(ArbPlainTime t) ->
        PT.fromString (PT.toString t) === Just t

    it "compare a a == EQ" $
      quickCheck \(ArbPlainTime t) ->
        compare t t === EQ

    it "reflexivity: a == a" $
      quickCheck \(ArbPlainTime t) ->
        (t == t) === true
