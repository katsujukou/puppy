module Puppy.Docs.UI.Route where

import Prelude hiding ((/))

import Data.Array (filter)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), joinWith, split)
import Puppy.Docs.UI.Base (basePath)
import Routing.Duplex as RD
import Routing.Duplex (rest, string)
import Routing.Duplex.Generic as RDG
import Routing.Duplex.Generic.Syntax ((?))

-- Docs pages are data driven (see Manifest), so instead of one constructor per
-- page the router carries the path segments in `Doc`. Home and Search stay
-- typed; `Doc` is the catch-all matched last.
data Route
  = Home
  | Search { q :: String }
  | Doc (Array String)

derive instance Eq Route
derive instance Ord Route
derive instance Generic Route _

instance Show Route where
  show = genericShow

-- Routes as served under a given deploy base: "" for the root, "/puppy" for a
-- project page. The base is a parameter rather than baked in so that it can be
-- exercised at both.
routesUnder :: String -> RD.RouteDuplex' Route
routesUnder base = RD.path base $ RDG.sum
  { "Home": RDG.noArgs
  , "Search": "search" ? { q: string }
  , "Doc": rest
  }

-- The routes as this build is served. Everything the app parses or prints goes
-- through here, so the base reaches pushState and back without any caller
-- having to know about it.
route :: RD.RouteDuplex' Route
route = routesUnder basePath

-- Build the `Doc` route for a manifest path like "/dev/optimizations.md".
docRoute :: String -> Route
docRoute = Doc <<< filter (_ /= "") <<< split (Pattern "/")

-- The path a `Doc` route points at, e.g. Doc ["dev"] -> "/dev". This is a
-- manifest key, not a URL, so it never carries the deploy base. For non-Doc
-- routes it is "/", which never matches a manifest page.
routePath :: Route -> String
routePath = case _ of
  Doc segs -> "/" <> joinWith "/" segs
  _ -> "/"
