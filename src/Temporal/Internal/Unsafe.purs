-- | Internal unsafe helpers shared across modules. Not exported from the
-- | public API.
module Temporal.Internal.Unsafe
  ( unsafePlainDate
  , unsafeAddDays
  , unsafeSubtractDays
  ) where

import Prelude

import Data.Int as Int
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Temporal.Duration as D
import Temporal.Internal.Types (PlainDate)
import Temporal.PlainDate as PD

unsafePlainDate :: Int -> Int -> Int -> PlainDate
unsafePlainDate y m d = unsafePartial $ fromJust $ PD.plainDate y m d

unsafeAddDays :: Int -> PlainDate -> PlainDate
unsafeAddDays 0 d = d
unsafeAddDays n d = unsafePartial $ fromJust $ PD.add (D.days (Int.toNumber n)) d

unsafeSubtractDays :: Int -> PlainDate -> PlainDate
unsafeSubtractDays 0 d = d
unsafeSubtractDays n d = unsafePartial $ fromJust $ PD.subtract (D.days (Int.toNumber n)) d
