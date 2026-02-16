module Test.Temporal.Generators where

import Prelude

import Data.Int as Int
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Temporal.Duration as D
import Temporal.Instant as I
import Temporal.PlainDate as PD
import Temporal.PlainDateTime as PDT
import Temporal.PlainTime as PT
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt)

-- | Duration with arbitrary fields (small values, same sign).
newtype ArbDuration = ArbDuration D.Duration

instance Arbitrary ArbDuration where
  arbitrary = ArbDuration <$> genDuration

-- | Duration with only time fields — safe for Instant arithmetic.
newtype ArbTimeDuration = ArbTimeDuration D.Duration

instance Arbitrary ArbTimeDuration where
  arbitrary = ArbTimeDuration <$> genTimeDuration

newtype ArbPlainTime = ArbPlainTime PT.PlainTime

instance Arbitrary ArbPlainTime where
  arbitrary = ArbPlainTime <$> genPlainTime

newtype ArbPlainDate = ArbPlainDate PD.PlainDate

instance Arbitrary ArbPlainDate where
  arbitrary = ArbPlainDate <$> genPlainDate

newtype ArbPlainDateTime = ArbPlainDateTime PDT.PlainDateTime

instance Arbitrary ArbPlainDateTime where
  arbitrary = ArbPlainDateTime <$> genPlainDateTime

newtype ArbInstant = ArbInstant I.Instant

instance Arbitrary ArbInstant where
  arbitrary = ArbInstant <$> genInstant

-- Generators

genDuration :: Gen D.Duration
genDuration = do
  y <- chooseInt 0 10
  mo <- chooseInt 0 10
  w <- chooseInt 0 10
  d <- chooseInt 0 10
  h <- chooseInt 0 10
  mi <- chooseInt 0 10
  s <- chooseInt 0 10
  ms <- chooseInt 0 999
  us <- chooseInt 0 999
  ns <- chooseInt 0 999
  let fields = D.defaultDurationFields
        { years = Int.toNumber y
        , months = Int.toNumber mo
        , weeks = Int.toNumber w
        , days = Int.toNumber d
        , hours = Int.toNumber h
        , minutes = Int.toNumber mi
        , seconds = Int.toNumber s
        , milliseconds = Int.toNumber ms
        , microseconds = Int.toNumber us
        , nanoseconds = Int.toNumber ns
        }
  pure $ unsafePartial $ fromJust $ D.duration fields

genTimeDuration :: Gen D.Duration
genTimeDuration = do
  h <- chooseInt 0 23
  mi <- chooseInt 0 59
  s <- chooseInt 0 59
  ms <- chooseInt 0 999
  us <- chooseInt 0 999
  ns <- chooseInt 0 999
  let fields = D.defaultDurationFields
        { hours = Int.toNumber h
        , minutes = Int.toNumber mi
        , seconds = Int.toNumber s
        , milliseconds = Int.toNumber ms
        , microseconds = Int.toNumber us
        , nanoseconds = Int.toNumber ns
        }
  pure $ unsafePartial $ fromJust $ D.duration fields

genPlainTime :: Gen PT.PlainTime
genPlainTime = do
  h <- chooseInt 0 23
  mi <- chooseInt 0 59
  s <- chooseInt 0 59
  ms <- chooseInt 0 999
  us <- chooseInt 0 999
  ns <- chooseInt 0 999
  pure $ unsafePartial $ fromJust $ PT.plainTime
    { hour: h, minute: mi, second: s
    , millisecond: ms, microsecond: us, nanosecond: ns
    }

genPlainDate :: Gen PD.PlainDate
genPlainDate = do
  y <- chooseInt 1970 2100
  m <- chooseInt 1 12
  d <- chooseInt 1 28
  pure $ unsafePartial $ fromJust $ PD.plainDate y m d

genPlainDateTime :: Gen PDT.PlainDateTime
genPlainDateTime = do
  y <- chooseInt 1970 2100
  m <- chooseInt 1 12
  d <- chooseInt 1 28
  h <- chooseInt 0 23
  mi <- chooseInt 0 59
  s <- chooseInt 0 59
  ms <- chooseInt 0 999
  us <- chooseInt 0 999
  ns <- chooseInt 0 999
  pure $ unsafePartial $ fromJust $ PDT.plainDateTime
    { year: y, month: m, day: d
    , hour: h, minute: mi, second: s
    , millisecond: ms, microsecond: us, nanosecond: ns
    }

genInstant :: Gen I.Instant
genInstant = do
  -- Generate integer milliseconds by combining days + ms-within-day
  -- Days from epoch: 0 to 47482 (covers 1970-2100)
  day <- chooseInt 0 47482
  -- Milliseconds within the day: 0 to 86399999
  msInDay <- chooseInt 0 86399999
  let ms = Int.toNumber day * 86400000.0 + Int.toNumber msInDay
  pure $ unsafePartial $ fromJust $ I.fromEpochMilliseconds ms
