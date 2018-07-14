module Main
  (main) where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either, either)
import Data.Maybe (Maybe(..), maybe)
import Data.Options ((:=))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Effect.Exception (throw)
import Fetch (fetch)
import Fetch.Options (defaults, method, url)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude (Unit, bind, compose, const, join, map, pure, (<>), (>))

fetchRepos :: Aff (Maybe String)
fetchRepos = do
  response <- fetch
    ( defaults
    <> method := "GET"
    <> url := "https://api.github.com/users/bouzuya/repos?type=owner&sort=pushed&direction=desc&per_page=100"
    )
  pure response.body

parseRepos :: String -> Maybe (Array { fullName :: String, pushedAt :: String })
parseRepos responseBody =
  let
    maybeFromEither :: forall a b. Either a b -> Maybe b
    maybeFromEither = either (const Nothing) Just
    toJson :: String -> Maybe Json
    toJson = compose maybeFromEither jsonParser
    toRecords :: Json -> Maybe (Array { fullName :: String, pushedAt :: String })
    toRecords json = do
      array <- Json.toArray json
      objects <- traverse Json.toObject array
      pure (Array.catMaybes (map toRecord objects))
    toRecord :: Object Json -> Maybe { fullName :: String, pushedAt :: String }
    toRecord o = do
      fullName <- bind (Object.lookup "full_name" o) Json.toString
      pushedAt <- bind (Object.lookup "pushed_at" o) Json.toString
      pure { fullName, pushedAt }
  in
    bind (toJson responseBody) toRecords

fetchCommits :: String -> Aff (Maybe (Array Json))
fetchCommits fullName = do
  response <-
    fetch
      ( defaults
      <> method := "GET"
      <> url := ("https://api.github.com/repos/" <> fullName <> "/commits")
      )
  let
    toJson :: String -> Maybe Json
    toJson = compose (either (const Nothing) Just) jsonParser
    json :: Maybe Json
    json = join (map toJson response.body)
    commitJsons :: Maybe (Array Json)
    commitJsons = join (map Json.toArray json)
  pure commitJsons

main :: Effect Unit
main = launchAff_ do
  reposMaybe <- (map (compose join (map parseRepos)) fetchRepos)
  repos <- liftEffect (maybe (throw "no repos") pure reposMaybe)
  let date = "2018-06-01T00:00:00Z" -- TODO
  let filteredRepos = Array.filter (\{ pushedAt } -> pushedAt > date) repos
  _ <- liftEffect (traverse logShow filteredRepos)
  repo <- liftEffect (maybe (throw "no repo") pure (Array.head filteredRepos))
  commitsMaybe <- fetchCommits repo.fullName
  commits <- liftEffect (maybe (throw "no commits") pure commitsMaybe)
  _ <- liftEffect (logShow (Array.length commits))
  liftEffect (log "OK")
