module Main
  (main) where

import Data.Date (Date)
import Data.DateTime (DateTime(..))
import Data.DateTime as DateTime
import Data.Maybe (fromJust)
import Data.Time (Time(..))
import Data.Time.Duration (Minutes(..))
import DateTimeFormat (format, iso8601DateTimeFormatWithoutMilliseconds)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Effect.Now (nowDate)
import GitHub (getCommitCount)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, bottom, map, negate, (*))
import Twitter (getTweetCount)

beginningOfDay :: Time
beginningOfDay = Time bottom bottom bottom bottom

toDateString :: Date -> String
toDateString date =
  let
    time = beginningOfDay
    localDateTime = DateTime date time
    jstOffset = Minutes (negate (60.0 * 9.0))
    utcDateTimeMaybe = DateTime.adjust jstOffset localDateTime
    utcDateTime = unsafePartial (fromJust utcDateTimeMaybe)
  in
    format iso8601DateTimeFormatWithoutMilliseconds utcDateTime

getDateTimeString :: Effect String
getDateTimeString = map toDateString nowDate

main :: Effect Unit
main = launchAff_ do
  dateTimeString <- liftEffect getDateTimeString
  commitCount <- getCommitCount dateTimeString
  _ <- liftEffect (logShow commitCount)
  tweetCount <- getTweetCount dateTimeString
  liftEffect (logShow tweetCount)
