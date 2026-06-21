-- | Formatting and parsing for HTML5 date/time input elements.
-- |
-- | HTML5 `<input>` elements use specific string formats that differ from
-- | full ISO 8601. This module handles the conversion.
-- |
-- | | Input type         | Format            | Example              |
-- | |--------------------|-------------------|----------------------|
-- | | `date`             | `YYYY-MM-DD`      | `"2024-03-15"`       |
-- | | `time`             | `HH:MM`           | `"14:30"`            |
-- | | `datetime-local`   | `YYYY-MM-DDTHH:MM`| `"2024-03-15T14:30"` |
-- | | `month`            | `YYYY-MM`         | `"2024-03"`          |
module Temporal.Format.Html
  ( toHtmlDate
  , fromHtmlDate
  , toHtmlTime
  , fromHtmlTime
  , toHtmlDateTime
  , fromHtmlDateTime
  , toHtmlMonth
  , fromHtmlMonth
  ) where

import Prelude

import Data.Maybe (Maybe)
import Temporal.Internal.Types (PlainDate, PlainDateTime, PlainTime, PlainYearMonth)
import Temporal.PlainDate as PD
import Temporal.PlainDateTime as PDT
import Temporal.PlainTime as PT
import Temporal.PlainYearMonth as PYM

-- | Format a `PlainDate` for `<input type="date">`.
-- |
-- | Produces `"YYYY-MM-DD"` (e.g. `"2024-03-15"`).
toHtmlDate :: PlainDate -> String
toHtmlDate = PD.toString

-- | Parse a value from `<input type="date">`.
-- |
-- | Accepts `"YYYY-MM-DD"` format.
fromHtmlDate :: String -> Maybe PlainDate
fromHtmlDate = PD.fromString

-- | Format a `PlainTime` for `<input type="time">`.
-- |
-- | Produces `"HH:MM"` (e.g. `"14:30"`), truncating seconds and sub-seconds.
toHtmlTime :: PlainTime -> String
toHtmlTime t = padTwo (PT.getHour t) <> ":" <> padTwo (PT.getMinute t)

-- | Parse a value from `<input type="time">`.
-- |
-- | Accepts `"HH:MM"` and `"HH:MM:SS"` formats (browsers may include seconds
-- | when the `step` attribute is set).
fromHtmlTime :: String -> Maybe PlainTime
fromHtmlTime = PT.fromString

-- | Format a `PlainDateTime` for `<input type="datetime-local">`.
-- |
-- | Produces `"YYYY-MM-DDTHH:MM"` (e.g. `"2024-03-15T14:30"`), truncating
-- | seconds and sub-seconds.
toHtmlDateTime :: PlainDateTime -> String
toHtmlDateTime dt =
  padFour (PDT.getYear dt) <> "-"
    <> padTwo (PDT.getMonth dt) <> "-"
    <> padTwo (PDT.getDay dt) <> "T"
    <> padTwo (PDT.getHour dt) <> ":"
    <> padTwo (PDT.getMinute dt)

-- | Parse a value from `<input type="datetime-local">`.
-- |
-- | Accepts `"YYYY-MM-DDTHH:MM"` and `"YYYY-MM-DDTHH:MM:SS"` formats.
fromHtmlDateTime :: String -> Maybe PlainDateTime
fromHtmlDateTime = PDT.fromString

-- | Format a `PlainYearMonth` for `<input type="month">`.
-- |
-- | Produces `"YYYY-MM"` (e.g. `"2024-03"`).
toHtmlMonth :: PlainYearMonth -> String
toHtmlMonth = PYM.toString

-- | Parse a value from `<input type="month">`.
-- |
-- | Accepts `"YYYY-MM"` format.
fromHtmlMonth :: String -> Maybe PlainYearMonth
fromHtmlMonth = PYM.fromString

-- Internal helpers

padTwo :: Int -> String
padTwo n
  | n < 10 = "0" <> show n
  | otherwise = show n

padFour :: Int -> String
padFour n
  | n < 10 = "000" <> show n
  | n < 100 = "00" <> show n
  | n < 1000 = "0" <> show n
  | otherwise = show n
