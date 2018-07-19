module Main
  (main) where

import Data.DateTime (DateTime(..))
import Data.DateTime as DateTime
import Data.Foldable (intercalate)
import Data.Maybe (fromJust)
import Data.Time (Time(..))
import Data.Time.Duration (Minutes(..))
import DateTimeFormat (format, iso8601DateTimeFormatWithoutMilliseconds)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (nowDate)
import GitHub (getCommitCount)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, bottom, negate, pure, show, (*), (<>))
import Twitter (getTweetCount)

beginningOfDay :: Time
beginningOfDay = Time bottom bottom bottom bottom

getToday :: Effect DateTime
getToday = do
  date <- nowDate
  let
    time = beginningOfDay
    localDateTime = DateTime date time
    jstOffset = Minutes (negate (60.0 * 9.0))
    utcDateTimeMaybe = DateTime.adjust jstOffset localDateTime
    utcDateTime = unsafePartial (fromJust utcDateTimeMaybe)
  pure utcDateTime

main :: Effect Unit
main = launchAff_ do
  today <- liftEffect getToday
  let dateTimeString = format iso8601DateTimeFormatWithoutMilliseconds today
  commitCount <- getCommitCount dateTimeString
  tweetCount <- getTweetCount today
  let
    message =
      intercalate "\n"
        [ "Commit Count: " <> (show commitCount)
        , "Tweet Count : " <> (show tweetCount)
        ]
  liftEffect (log message)
