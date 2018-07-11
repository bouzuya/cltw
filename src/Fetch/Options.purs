module Fetch.Options
  ( FetchOptions
  , body
  , defaults
  , headers
  , method
  , url
  ) where

-- TODO
-- import Bouzuya.HTTP.Method (Method)
-- import Bouzuya.HTTP.Method as Method
-- import Data.Functor.Contravariant (cmap)
import Data.Options (Option, Options, defaultToOptions, opt)
import Foreign.Object (Object)
-- import Prelude (show)

data FetchOptions

body :: Option FetchOptions String
body = opt "body"

defaults :: Options FetchOptions
defaults = defaultToOptions "method" "GET" -- Method.GET

headers :: Option FetchOptions (Object String)
headers = opt "headers"

method :: Option FetchOptions String -- Method
-- method = cmap show (opt "method")
method = opt "method"

url :: Option FetchOptions String
url = opt "url"
