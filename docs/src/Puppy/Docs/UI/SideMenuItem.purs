module Puppy.Docs.UI.SideMenuItem where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Puppy.Docs.UI.Hooks.UseNavigate (useNavigate)
import Routing.Duplex (RouteDuplex')

type Input r = { label :: String, routes :: RouteDuplex' r, to :: r }

make :: forall q o m r. Eq r => MonadEffect m => H.Component q (Input r) o m
make = Hooks.component \_ inps -> Hooks.do
  navigator <- useNavigate inps.routes
  let
    ctx =
      { label: inps.label
      , navigate: navigator.navigateTo inps.to
      , active: navigator.currentRoute == Just inps.to
      }
  Hooks.pure (render ctx)
  where
  render ctx = do
    HH.button
      [ HP.class_ $ ClassName $ baseClass <> " " <>
          if ctx.active then activeClass else inactiveClass
      , HE.onClick \_ -> ctx.navigate
      ]
      [ HH.text ctx.label ]

  baseClass = "w-full text-left px-3 py-1.5 rounded-md text-sm transition-colors cursor-pointer"
  activeClass = "bg-accent-soft text-accent font-medium"
  inactiveClass = "text-fg-muted hover:bg-black/5 dark:hover:bg-white/5 hover:text-fg"
