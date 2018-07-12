module Main
  (main) where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Fetch (fetch)
import Fetch.Options (defaults, method, url)
import Prelude (Unit, bind, compose, const, join, map, pure, (<>))

fetchRepos :: Aff (Maybe Json)
fetchRepos = do
  response <-
    fetch
      ( defaults
      <> method := "GET"
      <> url := "https://api.github.com/users/bouzuya/repos?type=owner&sort=pushed&direction=desc&per_page=100"
      )
  let
    toJson :: String -> Maybe Json
    toJson = compose (either (const Nothing) Just) jsonParser
    json :: Maybe Json
    json = join (map toJson response.body)
  pure json

main :: Effect Unit
main = launchAff_ do
  _ <- fetchRepos -- TODO
  liftEffect (log "OK")
