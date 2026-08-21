module Puppy.Docs.UI.Search
  ( SearchResult
  , search
  ) where

type SearchResult =
  { path :: String
  , anchor :: String
  , docTitle :: String
  , heading :: String
  , headingHtml :: String
  , snippetHtml :: String
  }

foreign import searchImpl :: String -> Array SearchResult

-- | Full-text search over the build-time index, ranked by relevance.
-- | Returns at most a handful of dozen heading-level sections.
search :: String -> Array SearchResult
search = searchImpl
