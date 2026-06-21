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
import Test.Temporal.IntervalSpec as IntervalSpec
import Test.Temporal.Interval.DurationSpec as IntervalDurationSpec
import Test.Temporal.Interval.SetSpec as IntervalSetSpec
import Test.Temporal.BoundarySpec as BoundarySpec
import Test.Temporal.WeekdaySpec as WeekdaySpec
import Test.Temporal.Format.HtmlSpec as HtmlSpec
import Test.Temporal.DiffSpec as DiffSpec
import Test.Temporal.TimeDurationSpec as TimeDurationSpec
import Test.Temporal.PredicateSpec as PredicateSpec
import Test.Temporal.DisplaySpec as DisplaySpec

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
  IntervalSpec.spec
  IntervalDurationSpec.spec
  IntervalSetSpec.spec
  BoundarySpec.spec
  WeekdaySpec.spec
  HtmlSpec.spec
  DiffSpec.spec
  TimeDurationSpec.spec
  PredicateSpec.spec
  DisplaySpec.spec
