module Puppy.Docs.UI.Manifest where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array (find)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)

type PageInfo =
  { path :: String
  , title :: String
  , nav :: String
  , html :: String
  }

pageInfo :: CA.JsonCodec PageInfo
pageInfo = CA.object "PageInfo" $
  CAR.record
    { path: CA.string
    , title: CA.string
    , nav: CA.string
    , html: CA.string
    }

-- | The rendered docs, as `scripts/render-docs.mjs` left them.
-- |
-- | `pages` is the contents list, in reading order. The landing page is kept
-- | apart from it: it is what the site opens on, not an entry in its own
-- | contents.
type Manifest =
  { landing :: PageInfo
  , pages :: Array PageInfo
  }

manifestCodec :: CA.JsonCodec Manifest
manifestCodec = CA.object "Manifest" $
  CAR.record
    { landing: pageInfo
    , pages: CA.array pageInfo
    }

-- | What the app holds before the real manifest has been decoded.
empty :: Manifest
empty =
  { landing: { path: "/", title: "", nav: "", html: "" }
  , pages: []
  }

lookupByPath :: Manifest -> String -> Maybe PageInfo
lookupByPath { pages } p = find (\pg -> pg.path == p) pages

foreign import manifestJson :: Json
