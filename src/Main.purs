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
import Node.Process as Process
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, add, bind, bottom, compose, const, identity, join, map, negate, pure, (&&), (*), (<>), (==), (>))

foreign import encodeBase64 :: String -> String

type Repo =
  { fullName :: String
  , language :: String
  , pushedAt :: String
  }
type RepoWithCount =
  { count :: Int
  , fullName :: String
  , language :: String
  , pushedAt :: String
  }

fetchRepos :: Aff (Maybe String)
fetchRepos = do
  response <- fetch
    ( defaults
    <> method := "GET"
    <> url := "https://api.github.com/users/bouzuya/repos?type=owner&sort=pushed&direction=desc&per_page=100"
    )
  pure response.body

parseRepos :: String -> Maybe (Array Repo)
parseRepos responseBody =
  let
    maybeFromEither :: forall a b. Either a b -> Maybe b
    maybeFromEither = either (const Nothing) Just
    toJson :: String -> Maybe Json
    toJson = compose maybeFromEither jsonParser
    toRecords :: Json -> Maybe (Array Repo)
    toRecords json = do
      array <- Json.toArray json
      objects <- traverse Json.toObject array
      pure (Array.catMaybes (map toRecord objects))
    toRecord :: Object Json -> Maybe Repo
    toRecord o = do
      fullName <- bind (Object.lookup "full_name" o) Json.toString
      language <- bind (Object.lookup "language" o) Json.toString
      pushedAt <- bind (Object.lookup "pushed_at" o) Json.toString
      pure { fullName, language, pushedAt }
  in
    bind (toJson responseBody) toRecords

fetchCommits :: String -> Aff (Maybe String)
fetchCommits fullName = do
  response <-
    fetch
      ( defaults
      <> method := "GET"
      <> url := ("https://api.github.com/repos/" <> fullName <> "/commits?per_page=100")
      )
  pure response.body

parseCommits :: String -> Maybe (Array String)
parseCommits responseBody =
  let
    maybeFromEither :: forall a b. Either a b -> Maybe b
    maybeFromEither = either (const Nothing) Just
    toJson :: String -> Maybe Json
    toJson = compose maybeFromEither jsonParser
    toStrings :: Json -> Maybe (Array String)
    toStrings json = do
      array <- Json.toArray json
      objects <- traverse Json.toObject array
      pure (Array.catMaybes (map toString objects))
    toString :: Object Json -> Maybe String
    toString o = do
      commit <- bind (Object.lookup "commit" o) Json.toObject
      author <- bind (Object.lookup "author" commit) Json.toObject
      bind (Object.lookup "date" author) Json.toString
  in
    bind (toJson responseBody) toStrings

fetchFilteredRepos :: String -> Aff (Array Repo)
fetchFilteredRepos date = do
  reposMaybe <- (map (compose join (map parseRepos)) fetchRepos)
  repos <- liftEffect (maybe (throw "no repos") pure reposMaybe)
  let filter { language, pushedAt } = pushedAt > date && language == "PureScript"
  pure (Array.filter filter repos)

fetchFilteredCount :: String -> Repo -> Aff RepoWithCount
fetchFilteredCount date repo = do
  commitsMaybe <- (map (compose join (map parseCommits))) (fetchCommits repo.fullName)
  commits <- liftEffect (maybe (throw "no commits") pure commitsMaybe)
  pure
    { count: Array.length (Array.filter (lessThan date) commits)
    , fullName: repo.fullName
    , language: repo.language
    , pushedAt: repo.pushedAt
    }

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

getCommitCount :: String -> Aff Int
getCommitCount dateTimeString = do
  filteredRepos <- fetchFilteredRepos dateTimeString
  filteredCounts <- traverse (fetchFilteredCount dateTimeString) filteredRepos
  pure (foldl add 0 (map _.count filteredCounts))

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
