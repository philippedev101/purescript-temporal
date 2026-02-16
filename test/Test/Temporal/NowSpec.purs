module Test.Temporal.NowSpec where

import Prelude

import Effect.Class (liftEffect)
import Temporal.Now as Now
import Temporal.Instant as I
import Temporal.ZonedDateTime as ZDT
import Temporal.PlainDateTime as PDT
import Temporal.PlainDate as PD
import Temporal.PlainTime as PT
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)

spec :: Spec Unit
spec = describe "Temporal.Now" do
  it "returns current instant" do
    inst <- liftEffect Now.instant
    I.getEpochMilliseconds inst `shouldSatisfy` (_ > 0.0)

  it "returns current zoned datetime" do
    zdt <- liftEffect Now.zonedDateTimeISO
    ZDT.getYear zdt `shouldSatisfy` (_ >= 2025)

  it "returns zoned datetime in specific timezone" do
    zdt <- liftEffect (Now.zonedDateTimeISOIn "UTC")
    ZDT.getTimeZoneId zdt `shouldSatisfy` (_ == "UTC")

  it "returns current plain datetime" do
    pdt <- liftEffect Now.plainDateTimeISO
    PDT.getYear pdt `shouldSatisfy` (_ >= 2025)

  it "returns current plain date" do
    pd <- liftEffect Now.plainDateISO
    PD.getYear pd `shouldSatisfy` (_ >= 2025)

  it "returns current plain time" do
    pt <- liftEffect Now.plainTimeISO
    PT.getHour pt `shouldSatisfy` (_ >= 0)

  it "returns timezone id" do
    tz <- liftEffect Now.timeZoneId
    tz `shouldSatisfy` (_ /= "")
