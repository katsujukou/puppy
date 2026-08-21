module Puppy.Docs.UI.Views.Search where

import Prelude

import Data.Array as Array
import Data.String (trim)
import Halogen (ClassName(..), PropName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Puppy.Docs.UI.Search (search)

type Input = { query :: String }

make :: forall q o m. H.Component q Input o m
make = Hooks.component \_ { query } -> Hooks.do
  Hooks.pure (render query)
  where
  render query =
    let
      q = trim query
      results = if q == "" then [] else search q
    in
      HH.div []
        [ HH.h1 [ HP.class_ $ ClassName "text-2xl font-bold tracking-tight text-brand mb-1" ]
            [ HH.text "Search" ]
        , HH.p [ HP.class_ $ ClassName "text-sm text-fg-muted mb-6" ]
            [ HH.text (summary q results) ]
        , if q == "" then HH.text ""
          else if Array.null results then emptyState q
          else HH.div [ HP.class_ $ ClassName "flex flex-col gap-7" ]
            (renderGroup results <$> docOrder results)
        ]

  summary q results
    | q == "" = "Type a query in the search box above."
    | otherwise = show (Array.length results) <> " result(s) for \"" <> q <> "\""

  emptyState q =
    HH.div [ HP.class_ $ ClassName "text-fg-muted" ]
      [ HH.text $ "No results for \"" <> q <> "\"." ]

  docOrder results = Array.nub (_.docTitle <$> results)

  renderGroup results docTitle =
    HH.section []
      [ HH.h2 [ HP.class_ $ ClassName "text-xs font-semibold uppercase tracking-wider text-fg-muted border-b border-border pb-1 mb-2" ]
          [ HH.text docTitle ]
      , HH.div [ HP.class_ $ ClassName "flex flex-col" ]
          (renderResult <$> Array.filter (\r -> r.docTitle == docTitle) results)
      ]

  renderResult r =
    HH.a
      [ HP.class_ $ ClassName "block py-2 px-3 -mx-3 rounded-md hover:bg-accent-soft transition-colors"
      , HP.href ""
      ]
      [ HH.div
          [ HP.class_ $ ClassName "font-medium text-accent"
          , HP.prop (PropName "innerHTML") r.headingHtml
          ]
          []
      , HH.p
          [ HP.class_ $ ClassName "mt-0.5 text-sm text-fg-muted leading-snug"
          , HP.prop (PropName "innerHTML") r.snippetHtml
          ]
          []
      ]
