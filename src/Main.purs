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
main = do
  _ <- log "Hello"
  launchAff_ do
    response <-
      fetch
        ( defaults
        <> method := "GET"
        <> url := "https://bouzuya.net"
        )
    liftEffect (logShow response.status)
