module Main
  (main) where

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Argonaut.Parser (jsonParser)
import Data.Array (foldl)
import Data.Array as Array
import Data.Date (Date)
import Data.DateTime (DateTime(..))
import Data.DateTime as DateTime
import Data.Either (Either, either)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Options ((:=))
import Data.Ord (lessThan)
import Data.Time (Time(..))
import Data.Time.Duration (Minutes(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import DateTimeFormat (format, iso8601DateTimeFormatWithoutMilliseconds)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Effect.Exception (throw)
import Effect.Now (nowDate)
import Fetch (fetch)
import Fetch.Options (body, defaults, headers, method, url)
import Foreign.Object (Object)
import Foreign.Object as Object
import GitHub (getCommitCount)
import Node.Process as Process
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, add, bind, bottom, compose, const, identity, join, map, negate, pure, (&&), (*), (<>), (==), (>))

foreign import encodeBase64 :: String -> String

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

base64header :: String -> String -> Tuple String String
base64header userName password =
  Tuple
    "Authorization"
    ("Basic " <> encodeBase64 (userName <> ":" <> password))

fetchTwitterToken :: TwitterCredentials -> Aff (Maybe String)
fetchTwitterToken { consumerKey, consumerSecret } = do
  response <- fetch
    ( defaults
    <> body := "grant_type=client_credentials"
    <> headers := (Object.fromFoldable [base64header consumerKey consumerSecret])
    <> method := "POST"
    <> url := "https://api.twitter.com/oauth2/token"
    )
  pure response.body

fetchTweets :: TwitterToken -> Aff (Maybe String)
fetchTweets { accessToken } = do
  response <- fetch
    ( defaults
    <> headers := (Object.fromFoldable
        [ Tuple "Authorization" ("Bearer " <> accessToken) ])
    <> method := "GET"
    <> url := "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=bouzuya&count=200&trim_user=true"
    )
  pure response.body

type TwitterCredentials = { consumerKey :: String, consumerSecret :: String }
type TwitterToken = { tokenType :: String, accessToken :: String }

parseTwitterToken :: String -> Maybe TwitterToken
parseTwitterToken responseBody =
  let
    maybeFromEither :: forall a b. Either a b -> Maybe b
    maybeFromEither = either (const Nothing) Just
    toJson :: String -> Maybe Json
    toJson = compose maybeFromEither jsonParser
    toRecord :: Json -> Maybe TwitterToken
    toRecord json = do
      o <- Json.toObject json
      accessToken <- bind (Object.lookup "access_token" o) Json.toString
      tokenType <- bind (Object.lookup "token_type" o) Json.toString
      pure { accessToken, tokenType }
  in
    bind (toJson responseBody) toRecord

loadCredentials :: Effect (Maybe TwitterCredentials)
loadCredentials = runMaybeT do
  consumerKey <- MaybeT (Process.lookupEnv "CLTW_TWITTER_CONSUMER_KEY")
  consumerSecret <- MaybeT (Process.lookupEnv "CLTW_TWITTER_CONSUMER_SECRET")
  pure { consumerKey, consumerSecret }

-- https://developer.twitter.com/en/docs/basics/authentication/overview/application-only
getTweetCount :: String -> Aff Int
getTweetCount dateTimeString = do
  credentialsMaybe <- liftEffect loadCredentials
  credentials <- liftEffect (maybe (throw "no env") pure credentialsMaybe)
  tokenMaybe <-
    (map
      (compose join (map parseTwitterToken))
      (fetchTwitterToken credentials))
  token <- liftEffect (maybe (throw "no token") pure tokenMaybe)
  tweets <- fetchTweets token
  _ <- liftEffect (logShow tweets)
  pure 0

main :: Effect Unit
main = launchAff_ do
  dateTimeString <- liftEffect getDateTimeString
  commitCount <- getCommitCount dateTimeString
  _ <- liftEffect (logShow commitCount)
  tweetCount <- getTweetCount dateTimeString
  liftEffect (logShow tweetCount)
