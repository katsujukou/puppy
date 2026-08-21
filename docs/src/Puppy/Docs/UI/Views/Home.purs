module Puppy.Docs.UI.Views.Home where

import Prelude

import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Puppy.Docs.UI.Hooks.UseApp (Theme(..), useApp)
import Puppy.Docs.UI.Views.MarkdownView as MarkdownView
import Type.Proxy (Proxy(..))

-- | The rendered landing page: `docs/md/README.md`, as the manifest carries it.
type Input = { html :: String }

-- | The landing page is the README, so that what a reader meets here and what
-- | they meet in the repository are the same words. The links out to GitHub and
-- | npm follow it, since the README opens with its own introduction.
make :: forall q o m. MonadEffect m => H.Component q Input o m
make = Hooks.component \_ { html } -> Hooks.do
  appApi <- useApp

  Hooks.pure (render { theme: appApi.theme, html })
  where
  render ctx = do
    HH.div []
      [ HH.div [ HP.class_ $ ClassName "my-6"
        ]
        [ HH.span [ HP.class_ $ ClassName "text-5xl font-mono font-bold text-brand "]
          [ HH.text "🐶 Puppy"]
        ]
      , HH.p [ HP.class_ $ ClassName "mt-6 mb-8" ]
        [ HH.text "Puppy is a LR(1) parser generator for PureScript, inspired by Menhir and Happy." ]
      , HH.div [ HP.class_ $ ClassName "flex gap-5 items-center mb-10" ]
          [ HH.a
              [ HP.class_ $ ClassName "flex gap-3 p-3 rounded-sm items-center bg-github text-[#ffffff]"
              , HP.href "https://github.com/katsujukou/puppy"
              , HP.target "_blank"
              , HP.rel "noopener noreferrer"
              ]
              [ HH.img
                  [ HP.src $
                      case ctx.theme of
                        Dark -> "img/github-mark.svg"
                        Light -> "img/github-mark-white.svg"
                  , HP.class_ $ ClassName "w-8 h-8"
                  ]
              , HH.span
                  [ HP.class_ $ ClassName "font-bold text-xl dark:text-[#24292e]" ]
                  [ HH.text "GitHub" ]
              ]
          , HH.a
              [ HP.class_ $ ClassName "flex gap-1 p-3 rounded-sm items-center bg-[#cb3837] text-[#ffffff]"
              , HP.href "https://www.npmjs.com/package/purs-puppy"
              , HP.target "_blank"
              , HP.rel "noopener noreferrer"
              ]
              [ HH.img
                  [ HP.src "img/npmjs-icon.svg"
                  , HP.class_ $ ClassName "w-8 h-8"
                  ]
              , HH.span
                  [ HP.class_ $ ClassName "font-bold text-xl " ]
                  [ HH.text "npm" ]
              ]
          ]
      , HH.hr [ HP.class_ $ ClassName "my-8 border-0 border-t border-border" ]
      , HH.slot_ (Proxy :: _ "landing") unit MarkdownView.make { html: ctx.html }
      ]
