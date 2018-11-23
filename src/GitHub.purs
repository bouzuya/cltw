module GitHub
  (getCommitCount) where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Argonaut.Parser (jsonParser)
import Data.Array (foldl)
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Either (hush)
import Data.Maybe (Maybe, maybe)
import Data.Options ((:=))
import Data.Ord (lessThan, (>))
import Data.Traversable (traverse)
import DateTimeFormat as DateTimeFormat
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Fetch (fetch)
import Fetch.Options (defaults, method, url)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude (add, bind, compose, join, map, pure, (&&), (<>), (==))

type Repo =
  { fullName :: String
  , language :: String
  , pushedAt :: DateTime
  }
type RepoWithCount =
  { count :: Int
  , fullName :: String
  , language :: String
  , pushedAt :: DateTime
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
    toJson :: String -> Maybe Json
    toJson = compose hush jsonParser
    toRecords :: Json -> Maybe (Array Repo)
    toRecords json = do
      array <- Json.toArray json
      objects <- traverse Json.toObject array
      pure (Array.catMaybes (map toRecord objects))
    toRecord :: Object Json -> Maybe Repo
    toRecord o = do
      fullName <- bind (Object.lookup "full_name" o) Json.toString
      language <- bind (Object.lookup "language" o) Json.toString
      pushedAtString <- bind (Object.lookup "pushed_at" o) Json.toString
      pushedAt <-
        hush
          (DateTimeFormat.parse
            DateTimeFormat.iso8601DateTimeFormatWithoutMilliseconds
            pushedAtString)
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

fetchFilteredCount :: DateTime -> Repo -> Aff RepoWithCount
fetchFilteredCount date repo = do
  commitsMaybe <- (map (compose join (map parseCommits))) (fetchCommits repo.fullName)
  commits <- liftEffect (maybe (throw "no commits") pure commitsMaybe)
  pure
    { count: Array.length (Array.filter (lessThan date) commits)
    , fullName: repo.fullName
    , language: repo.language
    , pushedAt: repo.pushedAt
    }

fetchFilteredRepos :: DateTime -> Aff (Array Repo)
fetchFilteredRepos date = do
  reposMaybe <- (map (compose join (map parseRepos)) fetchRepos)
  repos <- liftEffect (maybe (throw "no repos") pure reposMaybe)
  let filter { language, pushedAt } = pushedAt > date && language == "PureScript"
  pure (Array.filter filter repos)

getCommitCount :: DateTime -> Aff Int
getCommitCount dateTime = do
  filteredRepos <- fetchFilteredRepos dateTime
  filteredCounts <- traverse (fetchFilteredCount dateTime) filteredRepos
  pure (foldl add 0 (map _.count filteredCounts))

parseCommits :: String -> Maybe (Array DateTime)
parseCommits responseBody =
  let
    toJson :: String -> Maybe Json
    toJson = compose hush jsonParser
    toStrings :: Json -> Maybe (Array DateTime)
    toStrings json = do
      array <- Json.toArray json
      objects <- traverse Json.toObject array
      pure (Array.catMaybes (map toString objects))
    toString :: Object Json -> Maybe DateTime
    toString o = do
      commit <- bind (Object.lookup "commit" o) Json.toObject
      author <- bind (Object.lookup "author" commit) Json.toObject
      dateString <- bind (Object.lookup "date" author) Json.toString
      hush
        (DateTimeFormat.parse
          DateTimeFormat.iso8601DateTimeFormatWithoutMilliseconds
          dateString)
  in
    bind (toJson responseBody) toStrings
