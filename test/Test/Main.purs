module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Temporal.DurationSpec as DurationSpec
import Test.Temporal.PlainTimeSpec as PlainTimeSpec
import Test.Temporal.PlainDateSpec as PlainDateSpec
import Test.Temporal.InstantSpec as InstantSpec
import Test.Temporal.PlainDateTimeSpec as PlainDateTimeSpec
import Test.Temporal.PlainYearMonthSpec as PlainYearMonthSpec
import Test.Temporal.PlainMonthDaySpec as PlainMonthDaySpec
import Test.Temporal.ZonedDateTimeSpec as ZonedDateTimeSpec
import Test.Temporal.NowSpec as NowSpec

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  DurationSpec.spec
  PlainTimeSpec.spec
  PlainDateSpec.spec
  InstantSpec.spec
  PlainDateTimeSpec.spec
  PlainYearMonthSpec.spec
  PlainMonthDaySpec.spec
  ZonedDateTimeSpec.spec
  NowSpec.spec
