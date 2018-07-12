module Main
  (main) where

import Data.Options ((:=))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Fetch (fetch)
import Fetch.Options (defaults, method, url)
import Prelude (Unit, bind, (<>))

main :: Effect Unit
main = launchAff_ do
  response <-
    fetch
      ( defaults
      <> method := "GET"
      <> url := "https://api.github.com/users/bouzuya/repos?type=owner&sort=pushed&direction=desc&per_page=100"
      )
  _ <- liftEffect (logShow response.status)
  _ <- liftEffect (logShow response.body)
  liftEffect (log "OK")
