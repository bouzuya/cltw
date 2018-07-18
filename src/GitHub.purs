module GitHub
  (getCommitCount) where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Argonaut.Parser (jsonParser)
import Data.Array (foldl)
import Data.Array as Array
import Data.Either (Either, either)
import Data.Maybe (Maybe(..), maybe)
import Data.Options ((:=))
import Data.Ord (lessThan, (>))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Fetch (fetch)
import Fetch.Options (defaults, method, url)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude (add, bind, compose, const, join, map, pure, (&&), (<>), (==))

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

fetchFilteredRepos :: String -> Aff (Array Repo)
fetchFilteredRepos date = do
  reposMaybe <- (map (compose join (map parseRepos)) fetchRepos)
  repos <- liftEffect (maybe (throw "no repos") pure reposMaybe)
  let filter { language, pushedAt } = pushedAt > date && language == "PureScript"
  pure (Array.filter filter repos)

getCommitCount :: String -> Aff Int
getCommitCount dateTimeString = do
  filteredRepos <- fetchFilteredRepos dateTimeString
  filteredCounts <- traverse (fetchFilteredCount dateTimeString) filteredRepos
  pure (foldl add 0 (map _.count filteredCounts))

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
