module Puppy.Docs.UI.App where

import Prelude

import Data.Array (fold)
import Data.Codec as CA
import Data.Codec.Argonaut (printJsonDecodeError)
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Fmt as Fmt
import Halogen (AttrName(..), ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (useLifecycleEffect, useState)
import Halogen.Hooks as Hooks
import Puppy.Docs.UI.Base (asset)
import Puppy.Docs.UI.Hooks.UseApp (Theme(..), useApp)
import Puppy.Docs.UI.Hooks.UseNavigate (useNavigate)
import Puppy.Docs.UI.Manifest (lookupByPath, manifestCodec, manifestJson)
import Puppy.Docs.UI.Manifest as Manifest
import Puppy.Docs.UI.Route (Route(..), docRoute, route, routePath)
import Puppy.Docs.UI.Scroll as Scroll
import Puppy.Docs.UI.SideMenuItem as SideMenuItem
import Puppy.Docs.UI.Version (puppyVersion)
import Puppy.Docs.UI.Views.Home as Home
import Puppy.Docs.UI.Views.MarkdownView as MarkdownView
import Puppy.Docs.UI.Views.Search as SearchView
import Type.Proxy (Proxy(..))
import Web.UIEvent.KeyboardEvent as KE

make :: forall q i o m. MonadAff m => H.Component q i o m
make = Hooks.component \_ _ -> Hooks.do
  navigator <- useNavigate route
  appApi <- useApp
  query /\ queryId <- Hooks.useState ""
  menuOpen /\ menuOpenId <- Hooks.useState false
  searchFocused /\ searchFocusedId <- Hooks.useState false
  manifest /\ manifestId <- useState Manifest.empty

  useLifecycleEffect do
    navigator.initialize
    CA.decode manifestCodec manifestJson
      # either
          (printJsonDecodeError >>> Console.error)
          (Hooks.put manifestId)
    pure Nothing

  -- A new page starts at its top. The reader's position belongs to the page
  -- they were on, not the one they asked for, and the content column keeps its
  -- scroll offset across a route change because nothing about it is replaced.
  Hooks.captures { route: navigator.currentRoute } Hooks.useTickEffect do
    liftEffect Scroll.reset
    pure Nothing

  Hooks.pure $ render
    { currentPage: navigator.currentRoute
    , query
    , queryId
    , navigateTo: \r -> do
        Hooks.put menuOpenId false
        navigator.navigateTo r
    , dark: appApi.theme == Dark
    , setTheme: appApi.setTheme
    , toggleTheme: appApi.toggleTheme
    , menuOpen
    , menuOpenId
    , searchFocused
    , searchFocusedId
    , manifest
    }
  where
  render ctx =
    HH.div
      [ HP.class_ $ ClassName "flex flex-col h-screen bg-base text-fg" ]
      [ HH.header [ HP.class_ $ ClassName "relative z-50 shrink-0 bg-header text-brand border-b border-border" ]
          [ HH.div [ HP.class_ $ ClassName "flex items-center justify-between h-14 px-4 sm:px-9 gap-3" ]
              [ HH.div [ HP.class_ $ ClassName "flex items-center gap-3 min-w-0" ]
                  [ HH.button
                      [ HP.class_ $ ClassName "md:hidden p-1 -ml-1 flex cursor-pointer text-brand"
                      , HP.type_ HP.ButtonButton
                      , HP.attr (AttrName "aria-label") "Toggle navigation menu"
                      , HE.onClick \_ -> Hooks.put ctx.menuOpenId (not ctx.menuOpen)
                      ]
                      [ HH.span
                          [ HP.class_ $ ClassName "mask-icon w-6 h-6"
                          , HP.style (maskImage (asset "img/menu-icon.svg"))
                          ]
                          []
                      ]
                  , HH.div
                      [ HP.class_ $ ClassName "hidden md:flex items-center gap-3 font-mono font-semibold tracking-tight text-[20px] whitespace-nowrap cursor-pointer"
                      , HE.onClick \_ -> ctx.navigateTo Home
                      ]
                      [ HH.span
                          [ HP.class_ $ ClassName "mask-icon w-6 h-6"
                          , HP.style (maskImage (asset "img/book-icon.svg"))
                          ]
                          []
                      , HH.span [] [ HH.text "Puppy Documentation" ]
                      ]
                  ]
              , HH.div [ HP.class_ $ ClassName "flex items-center gap-4 sm:gap-5" ]
                  [ themeToggle ctx
                  , HH.div
                      [ HP.class_ $ ClassName $
                          if ctx.searchFocused then "absolute left-4 right-4 top-3 z-20 w-auto md:relative md:inset-auto md:w-56"
                          else "relative w-9 md:w-56 mr-3"
                      ]
                      [ HH.span
                          [ HP.class_ $ ClassName "mask-icon text-fg-muted w-4 h-4 absolute left-3 top-1/2 -translate-y-1/2 pointer-events-none"
                          , HP.style (maskImage (asset "img/search-icon.svg"))
                          ]
                          []
                      , HH.input
                          [ HP.class_ $ ClassName $
                              "w-full pl-9 pr-3 py-1.5 text-sm border border-border bg-base text-fg placeholder:text-fg-muted focus:outline-none focus:border-accent "
                                <> (if ctx.searchFocused then "rounded-md" else "rounded-full md:rounded-md")
                          , HP.type_ HP.InputText
                          , HP.placeholder "Search docs…"
                          , HP.value ctx.query
                          , HE.onFocus \_ -> Hooks.put ctx.searchFocusedId true
                          , HE.onBlur \_ -> Hooks.put ctx.searchFocusedId false
                          , HE.onValueInput \v -> Hooks.put ctx.queryId v
                          , HE.onKeyDown \ev ->
                              when (KE.key ev == "Enter" && ctx.query /= "")
                                (ctx.navigateTo (Search { q: ctx.query }))
                          ]
                      ]
                  , HH.a
                      [ HP.class_ $ ClassName "opacity-70 hover:opacity-100 transition-opacity"
                      , HP.href "https://github.com/katsujukou/puppy"
                      , HP.target "_blank"
                      , HP.rel "noopener noreferrer"
                      ]
                      [ HH.div
                          [ HP.class_ $ ClassName "flex gap-3 items-center" ]
                          [ HH.img [ HP.src (asset "img/github-mark.svg"), HP.alt "GitHub", HP.class_ $ ClassName "w-6 h-6 block dark:invert" ]
                          , HH.span [ HP.class_ $ ClassName "font-mono hover:underline" ] [ HH.text $ Fmt.fmt @"v{version}" { version: puppyVersion } ]
                          ]
                      ]
                  ]
              ]
          ]
      , HH.main [ HP.class_ $ ClassName "relative flex-1 min-h-0 flex" ]
          [ if ctx.menuOpen then
              HH.div
                [ HP.class_ $ ClassName "fixed inset-0 top-14 z-30 bg-black/40 md:hidden"
                , HE.onClick \_ -> Hooks.put ctx.menuOpenId false
                ]
                []
            else HH.text ""
          , HH.aside
              [ HP.class_ $ ClassName $
                  "fixed md:static top-14 bottom-0 left-0 z-40 w-64 shrink-0 min-h-0 overflow-y-auto border-r border-border bg-surface py-6 transition-transform md:translate-x-0 "
                    <> (if ctx.menuOpen then "translate-x-0" else "-translate-x-full")
              , HE.onClick \_ -> Hooks.put ctx.menuOpenId false
              ]
              [ HH.nav [ HP.class_ $ ClassName "flex flex-col gap-0.5 px-3" ] $ fold
                  [ [ contentsHeading ]
                  , map (\p -> sideMenuItem p.path p.nav) ctx.manifest.pages
                  , [ spacer
                    , HH.div
                        [ HP.class_ $ ClassName "px-3 pt-3 text-xs leading-relaxed text-fg-muted" ]
                        [ HH.span
                            [ HP.class_ $ ClassName "flex items-center gap-1" ]
                            -- A mask rather than an `img`: the icon is two-tone
                            -- maroon and pink, and the maroon all but vanishes
                            -- against a dark surface. As a mask it is a
                            -- silhouette in `currentColor`, so it matches the
                            -- text beside it in either theme.
                            [ HH.span
                                [ HP.class_ $ ClassName "mask-icon w-4 h-4 shrink-0"
                                , HP.style (maskImage (asset "img/law-ico.svg"))
                                ]
                                []
                            , HH.text "MIT licensed"
                            ]
                        , HH.text "© 2026 Katsujukou Kineya"
                        ]
                    ]
                  ]
              ]
          , HH.div
              [ HP.class_ $ ClassName "flex-1 min-h-0 overflow-y-auto"
              , HP.id Scroll.contentId
              ]
              [ HH.div [ HP.class_ $ ClassName "mx-auto max-w-3xl px-4 sm:px-8 py-8 sm:py-10" ]
                  [ renderRouterView ctx ]
              ]
          ]
      ]

  themeToggle ctx =
    HH.div [ HP.class_ $ ClassName "flex items-center gap-2" ]
      [ HH.span [ HP.class_ $ ClassName "hidden sm:flex" ]
          [ themeIcon "Use light theme" (asset "img/sun-icon.svg") (not ctx.dark) (ctx.setTheme Light) ]
      , HH.button
          [ HP.class_ $ ClassName "relative w-10 h-5 rounded-full bg-border transition-colors cursor-pointer"
          , HP.type_ HP.ButtonButton
          , HP.attr (AttrName "aria-label") "Toggle dark mode"
          , HE.onClick \_ -> ctx.toggleTheme
          ]
          [ HH.span
              [ HP.class_ $ ClassName ("absolute top-1 left-1 w-3 h-3 rounded-full bg-brand shadow-sm transition-transform" <> if ctx.dark then " translate-x-5" else "") ]
              []
          ]
      , HH.span [ HP.class_ $ ClassName "hidden sm:flex" ]
          [ themeIcon "Use dark theme" (asset "img/moon-icon.svg") ctx.dark (ctx.setTheme Dark) ]
      ]

  themeIcon label src active action =
    HH.button
      [ HP.class_ $ ClassName "p-1 -m-1 flex cursor-pointer"
      , HP.type_ HP.ButtonButton
      , HP.attr (AttrName "aria-label") label
      , HE.onClick \_ -> action
      ]
      [ HH.span
          [ HP.class_ $ ClassName ("theme-ico" <> if active then "" else " inactive")
          , HP.style (maskImage src)
          ]
          []
      ]

  maskImage url = "-webkit-mask-image:url(" <> url <> ");mask-image:url(" <> url <> ")"

  -- Horizontal divider between sections.
  spacer = HH.hr [ HP.class_ $ ClassName "my-3 border-0 border-t border-border" ]

  -- Names the list below it. Static: the pages under it come from the manifest,
  -- but what they are is not something the manifest says.
  contentsHeading =
    HH.div
      [ HP.class_ $ ClassName "px-3 pb-2 text-xs font-semibold uppercase tracking-wider text-fg-muted" ]
      [ HH.text "Contents" ]

  sideMenuItem path label =
    HH.slot_ (Proxy :: _ "side-menu-item") (docRoute path) SideMenuItem.make
      { routes: route, label, to: docRoute path }

  renderRouterView ctx = case ctx.currentPage of
    Nothing -> HH.text "Page not found"
    Just rt -> case rt of
      Home -> HH.slot_ (Proxy :: _ "home") unit Home.make { html: ctx.manifest.landing.html }
      Search { q } -> HH.slot_ (Proxy :: _ "search") unit SearchView.make { query: q }
      Doc _ ->
        maybe (HH.text "Page not found")
          (\pg -> renderMarkdownView rt pg.html)
          (lookupByPath ctx.manifest (routePath rt))

  renderMarkdownView r html = HH.slot_ (Proxy :: _ "router-view") r MarkdownView.make { html }
