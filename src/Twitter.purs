module Twitter
  (getTweetCount) where

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Either (hush)
import Data.Maybe (Maybe, maybe)
import Data.Options ((:=))
import Data.Ord (lessThan)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import DateTimeFormat as DateTimeFormat
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Fetch (fetch)
import Fetch.Options (body, defaults, headers, method, url)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Process as Process
import Prelude (bind, compose, const, join, map, pure, (<>))

type Tweet = { createdAt :: DateTime }
type TwitterCredentials = { consumerKey :: String, consumerSecret :: String }
type TwitterToken = { tokenType :: String, accessToken :: String }

foreign import encodeBase64 :: String -> String

base64header :: String -> String -> Tuple String String
base64header userName password =
  Tuple
    "Authorization"
    ("Basic " <> encodeBase64 (userName <> ":" <> password))

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

-- https://developer.twitter.com/en/docs/basics/authentication/overview/application-only
getTweetCount :: DateTime -> Aff Int
getTweetCount dateTime = do
  credentialsMaybe <- liftEffect loadCredentials
  credentials <- liftEffect (maybe (throw "no env") pure credentialsMaybe)
  tokenMaybe <-
    (map
      (compose join (map parseTwitterToken))
      (fetchTwitterToken credentials))
  token <- liftEffect (maybe (throw "no token") pure tokenMaybe)
  tweetsMaybe <- map (compose join (map parseTweets)) (fetchTweets token)
  tweets <- maybe (pure []) pure tweetsMaybe
  pure (Array.length (Array.filter (compose (lessThan dateTime) _.createdAt) tweets))

loadCredentials :: Effect (Maybe TwitterCredentials)
loadCredentials = runMaybeT do
  consumerKey <- MaybeT (Process.lookupEnv "CLTW_TWITTER_CONSUMER_KEY")
  consumerSecret <- MaybeT (Process.lookupEnv "CLTW_TWITTER_CONSUMER_SECRET")
  pure { consumerKey, consumerSecret }

parseTweets :: String -> Maybe (Array Tweet)
parseTweets responseBody =
  let
    toJson :: String -> Maybe Json
    toJson = compose hush jsonParser
    toRecords :: Json -> Maybe (Array Tweet)
    toRecords json = do
      array <- Json.toArray json
      objects <- traverse Json.toObject array
      pure (Array.catMaybes (map toRecord objects))
    toRecord :: Object Json -> Maybe Tweet
    toRecord o = do
      createdAtString <- bind (Object.lookup "created_at" o) Json.toString
      createdAt <-
        hush
          (DateTimeFormat.parse
            DateTimeFormat.twitterDateTimeFormat
            createdAtString)
      pure { createdAt }
  in
    bind (toJson responseBody) toRecords

parseTwitterToken :: String -> Maybe TwitterToken
parseTwitterToken responseBody =
  let
    toJson :: String -> Maybe Json
    toJson = compose hush jsonParser
    toRecord :: Json -> Maybe TwitterToken
    toRecord json = do
      o <- Json.toObject json
      accessToken <- bind (Object.lookup "access_token" o) Json.toString
      tokenType <- bind (Object.lookup "token_type" o) Json.toString
      pure { accessToken, tokenType }
  in
    bind (toJson responseBody) toRecord

