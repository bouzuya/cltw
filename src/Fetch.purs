module Fetch
  ( fetch
  ) where

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Fetch.Options (FetchOptions, defaults)
import Control.Promise (Promise)
import Control.Promise as Promise
import Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Data.Options (Options, options)
import Prelude (append, bind, eq, pure)

data FetchResponse

foreign import fetchImpl :: Foreign -> Effect (Promise FetchResponse)

foreign import textImpl :: FetchResponse -> String

foreign import statusImpl :: FetchResponse -> Int

fetch' :: Options FetchOptions -> Aff FetchResponse
fetch' opts = do
  promise <- liftEffect (fetchImpl (options (append defaults opts)))
  Promise.toAff promise

fetch :: Options FetchOptions -> Aff { body :: Maybe String, status :: Int }
fetch opts = do
  response <- fetch' opts
  let status = statusImpl response
  pure
    { body:
        if eq status 204
          then Nothing
          else Just (textImpl response)
    , status
    }
