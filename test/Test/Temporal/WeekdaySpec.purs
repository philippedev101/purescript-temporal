module Test.Temporal.WeekdaySpec where

import Prelude

import Data.Enum (fromEnum, toEnum, succ, pred, enumFromTo)
import Data.Maybe (Maybe(..), isNothing)
import Temporal.PlainDate as PD
import Temporal.PlainDate.Weekday (Weekday(..), getWeekday, isWeekday, isWeekend, addBusinessDays, nextWeekday, nextOrSameWeekday, previousWeekday, previousOrSameWeekday, dayOfWeekInSameWeek, nthWeekdayOfMonth, lastWeekdayOfMonth)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck ((===))
import Test.Temporal.Generators (ArbPlainDate(..))

-- Helper
pd :: Int -> Int -> Int -> PD.PlainDate
pd y m d = case PD.plainDate y m d of
  Just x -> x
  Nothing -> pd 2024 1 1

spec :: Spec Unit
spec = describe "Temporal.PlainDate.Weekday" do
  describe "Weekday type" do
    it "Show" do
      show Monday `shouldEqual` "Monday"
      show Sunday `shouldEqual` "Sunday"

    it "Eq" do
      Monday `shouldEqual` Monday
      (Monday == Tuesday) `shouldEqual` false

    it "Ord" do
      (Monday < Tuesday) `shouldEqual` true
      (Sunday > Saturday) `shouldEqual` true
      (Monday < Sunday) `shouldEqual` true

    it "Bounded" do
      (bottom :: Weekday) `shouldEqual` Monday
      (top :: Weekday) `shouldEqual` Sunday

    it "Enum succ/pred" do
      succ Monday `shouldEqual` Just Tuesday
      succ Sunday `shouldEqual` Nothing
      pred Sunday `shouldEqual` Just Saturday
      pred Monday `shouldEqual` Nothing

    it "BoundedEnum fromEnum" do
      fromEnum Monday `shouldEqual` 1
      fromEnum Sunday `shouldEqual` 7

    it "BoundedEnum toEnum" do
      toEnum 1 `shouldEqual` (Just Monday :: Maybe Weekday)
      toEnum 7 `shouldEqual` (Just Sunday :: Maybe Weekday)
      toEnum 0 `shouldEqual` (Nothing :: Maybe Weekday)
      toEnum 8 `shouldEqual` (Nothing :: Maybe Weekday)

    it "enumFromTo" do
      enumFromTo Monday Friday `shouldEqual` [ Monday, Tuesday, Wednesday, Thursday, Friday ]
      enumFromTo Saturday Sunday `shouldEqual` [ Saturday, Sunday ]
      enumFromTo Monday Sunday `shouldEqual` [ Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday ]

  describe "getWeekday" do
    it "identifies days correctly" do
      -- 2024-03-18 is Monday
      getWeekday (pd 2024 3 18) `shouldEqual` Monday
      getWeekday (pd 2024 3 19) `shouldEqual` Tuesday
      getWeekday (pd 2024 3 20) `shouldEqual` Wednesday
      getWeekday (pd 2024 3 21) `shouldEqual` Thursday
      getWeekday (pd 2024 3 22) `shouldEqual` Friday
      getWeekday (pd 2024 3 23) `shouldEqual` Saturday
      getWeekday (pd 2024 3 24) `shouldEqual` Sunday

  describe "isWeekday / isWeekend" do
    it "weekdays are Mon-Fri" do
      isWeekday (pd 2024 3 18) `shouldEqual` true  -- Mon
      isWeekday (pd 2024 3 22) `shouldEqual` true  -- Fri
      isWeekday (pd 2024 3 23) `shouldEqual` false -- Sat
      isWeekday (pd 2024 3 24) `shouldEqual` false -- Sun

    it "weekends are Sat-Sun" do
      isWeekend (pd 2024 3 23) `shouldEqual` true  -- Sat
      isWeekend (pd 2024 3 24) `shouldEqual` true  -- Sun
      isWeekend (pd 2024 3 22) `shouldEqual` false -- Fri

    it "isWeekday and isWeekend are complementary" $
      quickCheck \(ArbPlainDate d) ->
        isWeekday d /= isWeekend d

  describe "addBusinessDays" do
    it "zero is identity" do
      addBusinessDays 0 (pd 2024 3 20) `shouldEqual` pd 2024 3 20

    it "zero on weekend is identity" do
      addBusinessDays 0 (pd 2024 3 23) `shouldEqual` pd 2024 3 23

    it "forward 1 from Friday = Monday" do
      addBusinessDays 1 (pd 2024 3 22) `shouldEqual` pd 2024 3 25

    it "forward 1 from Thursday = Friday" do
      addBusinessDays 1 (pd 2024 3 21) `shouldEqual` pd 2024 3 22

    it "forward 5 from Monday = Monday (next week)" do
      addBusinessDays 5 (pd 2024 3 18) `shouldEqual` pd 2024 3 25

    it "forward 6 from Monday = Tuesday (next week)" do
      addBusinessDays 6 (pd 2024 3 18) `shouldEqual` pd 2024 3 26

    it "forward 10 from Monday = Monday (2 weeks later)" do
      addBusinessDays 10 (pd 2024 3 18) `shouldEqual` pd 2024 4 1

    it "forward 1 from Saturday = Monday" do
      addBusinessDays 1 (pd 2024 3 23) `shouldEqual` pd 2024 3 25

    it "forward 1 from Sunday = Monday" do
      addBusinessDays 1 (pd 2024 3 24) `shouldEqual` pd 2024 3 25

    it "backward 1 from Monday = Friday" do
      addBusinessDays (-1) (pd 2024 3 25) `shouldEqual` pd 2024 3 22

    it "backward 1 from Tuesday = Monday" do
      addBusinessDays (-1) (pd 2024 3 26) `shouldEqual` pd 2024 3 25

    it "backward 5 from Friday = Friday (previous week)" do
      addBusinessDays (-5) (pd 2024 3 22) `shouldEqual` pd 2024 3 15

    it "backward 1 from Saturday = Friday" do
      addBusinessDays (-1) (pd 2024 3 23) `shouldEqual` pd 2024 3 22

    it "backward 1 from Sunday = Friday" do
      addBusinessDays (-1) (pd 2024 3 24) `shouldEqual` pd 2024 3 22

    it "forward 4 from Wednesday crosses weekend" do
      -- Wed + 4 biz days = Wed->Thu->Fri->[skip]->Mon->Tue
      addBusinessDays 4 (pd 2024 3 20) `shouldEqual` pd 2024 3 26

    it "forward 2 from Saturday" do
      addBusinessDays 2 (pd 2024 3 23) `shouldEqual` pd 2024 3 26

    it "backward 2 from Sunday" do
      addBusinessDays (-2) (pd 2024 3 24) `shouldEqual` pd 2024 3 21

    it "crosses month boundary" do
      addBusinessDays 3 (pd 2024 3 28) `shouldEqual` pd 2024 4 2

    it "backward crosses month boundary" do
      addBusinessDays (-3) (pd 2024 4 3) `shouldEqual` pd 2024 3 29

    it "large n: 252 business days from Jan 1 2024" do
      -- 252 business days = 50 weeks + 2 days = 350 + 2 = 352 calendar days
      -- Jan 1 2024 is Monday, +252 biz days should land on Dec 18 2024 (Wednesday)
      addBusinessDays 252 (pd 2024 1 1) `shouldEqual` pd 2024 12 18

    it "result is always a weekday (forward)" $
      quickCheck \(ArbPlainDate d) ->
        isWeekday (addBusinessDays 1 d)

    it "result is always a weekday (backward)" $
      quickCheck \(ArbPlainDate d) ->
        isWeekday (addBusinessDays (-1) d)

  describe "nextWeekday" do
    it "advances past same day" do
      -- 2024-03-18 is Monday
      nextWeekday Monday (pd 2024 3 18) `shouldEqual` pd 2024 3 25

    it "finds next occurrence" do
      nextWeekday Wednesday (pd 2024 3 18) `shouldEqual` pd 2024 3 20

    it "Friday from Monday = same week" do
      nextWeekday Friday (pd 2024 3 18) `shouldEqual` pd 2024 3 22

    it "Monday from Friday = next week" do
      nextWeekday Monday (pd 2024 3 22) `shouldEqual` pd 2024 3 25

    it "Monday from Saturday" do
      nextWeekday Monday (pd 2024 3 23) `shouldEqual` pd 2024 3 25

    it "Friday from Sunday" do
      nextWeekday Friday (pd 2024 3 24) `shouldEqual` pd 2024 3 29

  describe "nextOrSameWeekday" do
    it "returns same day if matching" do
      nextOrSameWeekday Monday (pd 2024 3 18) `shouldEqual` pd 2024 3 18

    it "advances if not matching" do
      nextOrSameWeekday Wednesday (pd 2024 3 18) `shouldEqual` pd 2024 3 20

  describe "previousWeekday" do
    it "retreats past same day" do
      previousWeekday Monday (pd 2024 3 18) `shouldEqual` pd 2024 3 11

    it "finds previous occurrence" do
      previousWeekday Friday (pd 2024 3 18) `shouldEqual` pd 2024 3 15

    it "Friday from Saturday" do
      previousWeekday Friday (pd 2024 3 23) `shouldEqual` pd 2024 3 22

    it "Monday from Sunday" do
      previousWeekday Monday (pd 2024 3 24) `shouldEqual` pd 2024 3 18

  describe "previousOrSameWeekday" do
    it "returns same day if matching" do
      previousOrSameWeekday Monday (pd 2024 3 18) `shouldEqual` pd 2024 3 18

    it "retreats if not matching" do
      previousOrSameWeekday Friday (pd 2024 3 18) `shouldEqual` pd 2024 3 15

  describe "dayOfWeekInSameWeek" do
    it "same day is identity" do
      dayOfWeekInSameWeek Monday (pd 2024 3 18) `shouldEqual` pd 2024 3 18

    it "forward in same week" do
      dayOfWeekInSameWeek Friday (pd 2024 3 18) `shouldEqual` pd 2024 3 22

    it "backward in same week" do
      dayOfWeekInSameWeek Monday (pd 2024 3 22) `shouldEqual` pd 2024 3 18

    it "Sunday from Monday" do
      dayOfWeekInSameWeek Sunday (pd 2024 3 18) `shouldEqual` pd 2024 3 24

    it "Monday from Sunday" do
      dayOfWeekInSameWeek Monday (pd 2024 3 24) `shouldEqual` pd 2024 3 18

  describe "nthWeekdayOfMonth" do
    it "1st Monday of March 2024" do
      nthWeekdayOfMonth 1 Monday (pd 2024 3 15) `shouldEqual` Just (pd 2024 3 4)

    it "2nd Tuesday of March 2024" do
      nthWeekdayOfMonth 2 Tuesday (pd 2024 3 1) `shouldEqual` Just (pd 2024 3 12)

    it "2nd Wednesday of January 2024" do
      nthWeekdayOfMonth 2 Wednesday (pd 2024 1 20) `shouldEqual` Just (pd 2024 1 10)

    it "4th Thursday of November 2024 (Thanksgiving)" do
      nthWeekdayOfMonth 4 Thursday (pd 2024 11 1) `shouldEqual` Just (pd 2024 11 28)

    it "3rd Friday of March 2024" do
      nthWeekdayOfMonth 3 Friday (pd 2024 3 1) `shouldEqual` Just (pd 2024 3 15)

    it "1st Sunday of March 2024" do
      nthWeekdayOfMonth 1 Sunday (pd 2024 3 15) `shouldEqual` Just (pd 2024 3 3)

    it "5th Monday of March 2024 does not exist" do
      -- March 2024: Mondays are 4, 11, 18, 25 — only 4, no 5th
      nthWeekdayOfMonth 5 Monday (pd 2024 3 1) `shouldSatisfy` isNothing

    it "5th Saturday of March 2024 exists" do
      -- March 2024: Saturdays are 2, 9, 16, 23, 30 — 5th exists
      nthWeekdayOfMonth 5 Saturday (pd 2024 3 1) `shouldEqual` Just (pd 2024 3 30)

    it "rejects n < 1" do
      nthWeekdayOfMonth 0 Monday (pd 2024 3 1) `shouldSatisfy` isNothing
      nthWeekdayOfMonth (-1) Monday (pd 2024 3 1) `shouldSatisfy` isNothing

    it "1st day of month that IS the target weekday" do
      -- 2024-07-01 is a Monday
      nthWeekdayOfMonth 1 Monday (pd 2024 7 15) `shouldEqual` Just (pd 2024 7 1)

  describe "lastWeekdayOfMonth" do
    it "last Friday of March 2024" do
      lastWeekdayOfMonth Friday (pd 2024 3 15) `shouldEqual` pd 2024 3 29

    it "last Monday of February 2024 (leap year)" do
      lastWeekdayOfMonth Monday (pd 2024 2 1) `shouldEqual` pd 2024 2 26

    it "last Sunday of March 2024" do
      lastWeekdayOfMonth Sunday (pd 2024 3 1) `shouldEqual` pd 2024 3 31

    it "last day of month IS the target weekday" do
      -- 2024-03-31 is a Sunday
      lastWeekdayOfMonth Sunday (pd 2024 3 15) `shouldEqual` pd 2024 3 31

  describe "quickcheck properties" do
    it "getWeekday matches getDayOfWeek" $
      quickCheck \(ArbPlainDate d) ->
        fromEnum (getWeekday d) === PD.getDayOfWeek d

    it "nextWeekday always advances 1-7 days" $
      quickCheck \(ArbPlainDate d) ->
        let target = getWeekday d
            next = nextWeekday target d
        in next > d && (PD.getDayOfWeek next == PD.getDayOfWeek d)

    it "nextOrSameWeekday result has correct weekday" $
      quickCheck \(ArbPlainDate d) ->
        getWeekday (nextOrSameWeekday Friday d) === Friday

    it "previousOrSameWeekday result has correct weekday" $
      quickCheck \(ArbPlainDate d) ->
        getWeekday (previousOrSameWeekday Monday d) === Monday

    it "dayOfWeekInSameWeek result has correct weekday" $
      quickCheck \(ArbPlainDate d) ->
        getWeekday (dayOfWeekInSameWeek Wednesday d) === Wednesday

    it "dayOfWeekInSameWeek is within 6 days" $
      quickCheck \(ArbPlainDate d) ->
        let result = dayOfWeekInSameWeek Monday d
            diff = PD.getDayOfYear d - PD.getDayOfYear result
        in diff >= (-6) && diff <= 6
           || PD.getYear d /= PD.getYear result -- year boundary

    it "nthWeekdayOfMonth 1 always succeeds" $
      quickCheck \(ArbPlainDate d) ->
        case nthWeekdayOfMonth 1 Monday d of
          Just result -> PD.getMonth result == PD.getMonth d
          Nothing -> false

    it "lastWeekdayOfMonth is always in the same month" $
      quickCheck \(ArbPlainDate d) ->
        let result = lastWeekdayOfMonth Friday d
        in (PD.getMonth result == PD.getMonth d)
          && (PD.getYear result == PD.getYear d)

    it "lastWeekdayOfMonth result has correct weekday" $
      quickCheck \(ArbPlainDate d) ->
        getWeekday (lastWeekdayOfMonth Wednesday d) === Wednesday

    it "addBusinessDays result is weekday for arbitrary n" $
      quickCheck \(ArbPlainDate d) ->
        isWeekday (addBusinessDays 17 d)
          && isWeekday (addBusinessDays (-13) d)

    it "addBusinessDays round-trip n=1 from weekday" $
      quickCheck \(ArbPlainDate d) ->
        not (isWeekday d) || addBusinessDays (-1) (addBusinessDays 1 d) == d

    it "addBusinessDays round-trip n=5 from weekday" $
      quickCheck \(ArbPlainDate d) ->
        not (isWeekday d) || addBusinessDays (-5) (addBusinessDays 5 d) == d

    it "addBusinessDays composability from weekday: add (n+m) == add m . add n" $
      quickCheck \(ArbPlainDate d) ->
        not (isWeekday d)
          || addBusinessDays 3 (addBusinessDays 4 d) == addBusinessDays 7 d

    it "nextWeekday always strictly advances" $
      quickCheck \(ArbPlainDate d) ->
        nextWeekday Monday d > d
          && nextWeekday Friday d > d

    it "previousWeekday always strictly retreats" $
      quickCheck \(ArbPlainDate d) ->
        previousWeekday Monday d < d
          && previousWeekday Friday d < d

    it "nextOrSameWeekday >= d" $
      quickCheck \(ArbPlainDate d) ->
        nextOrSameWeekday Tuesday d >= d
          && nextOrSameWeekday Saturday d >= d

    it "previousOrSameWeekday <= d" $
      quickCheck \(ArbPlainDate d) ->
        previousOrSameWeekday Tuesday d <= d
          && previousOrSameWeekday Saturday d <= d

    it "nextOrSameWeekday then previousOrSameWeekday is identity for same target" $
      quickCheck \(ArbPlainDate d) ->
        previousOrSameWeekday (getWeekday d) (nextOrSameWeekday (getWeekday d) d) === d

    it "lastWeekdayOfMonth >= nthWeekdayOfMonth 4 (when 4th exists)" $
      quickCheck \(ArbPlainDate d) ->
        case nthWeekdayOfMonth 4 Friday d of
          Just fourth -> lastWeekdayOfMonth Friday d >= fourth
          Nothing -> false -- 4th always exists

    it "nthWeekdayOfMonth result has correct weekday" $
      quickCheck \(ArbPlainDate d) ->
        case nthWeekdayOfMonth 2 Thursday d of
          Just result -> getWeekday result == Thursday
          Nothing -> false -- 2nd always exists
