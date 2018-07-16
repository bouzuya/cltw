module DateTimeFormat
  ( format
  , iso8601DateTimeFormatWithoutMilliseconds
  , parse
  , twitterDateTimeFormat
  ) where

import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Formatter.DateTime as Formatter
import Data.List as List

format :: Formatter.Formatter -> DateTime -> String
format = Formatter.format

-- ISO 8601 date time format (without milliseconds)
-- e.g. 2018-01-02T03:04:05Z
iso8601DateTimeFormatWithoutMilliseconds :: Formatter.Formatter
iso8601DateTimeFormatWithoutMilliseconds =
  List.fromFoldable
  [ Formatter.YearFull
  , Formatter.Placeholder "-"
  , Formatter.MonthTwoDigits
  , Formatter.Placeholder "-"
  , Formatter.DayOfMonthTwoDigits
  , Formatter.Placeholder "T"
  , Formatter.Hours24
  , Formatter.Placeholder ":"
  , Formatter.MinutesTwoDigits
  , Formatter.Placeholder ":"
  , Formatter.SecondsTwoDigits
  , Formatter.Placeholder "Z"
  ]

parse :: Formatter.Formatter -> String -> Either String DateTime
parse = Formatter.unformat

-- Twitter date time format
-- e.g. Sun Jan 02 03:04:05 +0000 2018
-- NOTE: ANSI C's asctime() format (`Sun Jan 01 00:00:00 2018`)
-- https://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.3
twitterDateTimeFormat :: Formatter.Formatter
twitterDateTimeFormat =
  List.fromFoldable
  [ Formatter.DayOfWeekNameShort
  , Formatter.Placeholder " "
  , Formatter.MonthShort
  , Formatter.Placeholder " "
  , Formatter.DayOfMonthTwoDigits
  , Formatter.Placeholder " "
  , Formatter.Hours24
  , Formatter.Placeholder ":"
  , Formatter.MinutesTwoDigits
  , Formatter.Placeholder ":"
  , Formatter.SecondsTwoDigits
  , Formatter.Placeholder " +0000 "
  , Formatter.YearFull
  ]
