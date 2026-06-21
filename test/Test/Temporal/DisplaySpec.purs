module Test.Temporal.DisplaySpec where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Temporal.Display (toLocal, toLocalDate, toLocalTime, todayIn, nowIn)
import Temporal.Instant as I
import Temporal.PlainDateTime as PDT
import Temporal.PlainDate as PD
import Temporal.PlainTime as PT
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = describe "Temporal.Display" do
  describe "toLocal" do
    it "converts instant to local datetime" do
      -- 2024-03-15T18:30:00Z in UTC = 2024-03-15T14:30:00 in New York (EDT, -4)
      case I.fromString "2024-03-15T18:30:00Z" of
        Nothing -> pure unit
        Just inst -> do
          let local = toLocal "America/New_York" inst
          PDT.getYear local `shouldEqual` 2024
          PDT.getMonth local `shouldEqual` 3
          PDT.getDay local `shouldEqual` 15
          PDT.getHour local `shouldEqual` 14
          PDT.getMinute local `shouldEqual` 30

    it "converts to different timezone" do
      -- 2024-03-15T18:30:00Z in UTC = 2024-03-16T03:30:00 in Tokyo (+9)
      case I.fromString "2024-03-15T18:30:00Z" of
        Nothing -> pure unit
        Just inst -> do
          let local = toLocal "Asia/Tokyo" inst
          PDT.getYear local `shouldEqual` 2024
          PDT.getMonth local `shouldEqual` 3
          PDT.getDay local `shouldEqual` 16
          PDT.getHour local `shouldEqual` 3
          PDT.getMinute local `shouldEqual` 30

    it "UTC stays the same" do
      case I.fromString "2024-03-15T12:00:00Z" of
        Nothing -> pure unit
        Just inst -> do
          let local = toLocal "UTC" inst
          PDT.getHour local `shouldEqual` 12

  describe "toLocalDate" do
    it "extracts date in timezone" do
      -- Late UTC = next day in Tokyo
      case I.fromString "2024-03-15T18:30:00Z" of
        Nothing -> pure unit
        Just inst -> do
          let d = toLocalDate "Asia/Tokyo" inst
          PD.getDay d `shouldEqual` 16

  describe "toLocalTime" do
    it "extracts time in timezone" do
      case I.fromString "2024-03-15T18:30:00Z" of
        Nothing -> pure unit
        Just inst -> do
          let t = toLocalTime "America/New_York" inst
          PT.getHour t `shouldEqual` 14
          PT.getMinute t `shouldEqual` 30

  describe "todayIn / nowIn" do
    it "todayIn returns a date" do
      d <- liftEffect $ todayIn "UTC"
      PD.getYear d `shouldSatisfy` \y -> y >= 2024

    it "nowIn returns a datetime" do
      dt <- liftEffect $ nowIn "UTC"
      PDT.getYear dt `shouldSatisfy` \y -> y >= 2024
