module Puppy.Docs.UI.Base (basePath, asset) where

import Prelude

import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), stripSuffix)

foreign import baseUrl :: String

-- | The deploy base without its trailing slash: `""` when the site is served
-- | from the root, `"/puppy"` for a project page. In that form it prefixes a
-- | route path directly, and since `Routing.Duplex.path ""` is exactly `root`,
-- | the root case needs no special handling.
basePath :: String
basePath = fromMaybe baseUrl (stripSuffix (Pattern "/") baseUrl)

-- | Where a file under `public/` is served from, e.g. `asset "img/law-ico.svg"`.
-- |
-- | It has to be an absolute URL. Vite copies `public/` to the site root and
-- | never sees these strings, so nothing rewrites them; and a relative one is
-- | read against the current route's directory, which changes with the page and
-- | is not where the file is.
asset :: String -> String
asset path = basePath <> "/" <> path
