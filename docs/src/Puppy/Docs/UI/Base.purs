module Puppy.Docs.UI.Base (basePath) where

import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), stripSuffix)

foreign import baseUrl :: String

-- | The deploy base without its trailing slash: `""` when the site is served
-- | from the root, `"/puppy"` for a project page. In that form it prefixes a
-- | route path directly, and since `Routing.Duplex.path ""` is exactly `root`,
-- | the root case needs no special handling.
basePath :: String
basePath = fromMaybe baseUrl (stripSuffix (Pattern "/") baseUrl)
