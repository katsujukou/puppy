module Puppy.Docs.UI.Hooks.UseApp
  ( Theme(..)
  , parseTheme
  , toggle
  , useApp
  , UseApp
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Helix (HelixMiddleware', UseHelix, UseHelixHook, makeStore, (<|))
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookType, UseEffect, useLifecycleEffect)
import Halogen.Hooks as Hooks
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.Element (classList)
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLHtmlElement as HTMLhtmlElement
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage

data Theme = Dark | Light

derive instance Eq Theme

instance Show Theme where
  show = case _ of
    Dark -> "Dark"
    Light -> "Light"

darkClass :: String
darkClass = "dark"

toggle :: Theme -> Theme
toggle = case _ of
  Dark -> Light
  Light -> Dark

parseTheme :: String -> Maybe Theme
parseTheme = case _ of
  "Dark" -> Just Dark
  "Light" -> Just Light
  _ -> Nothing

type State =
  { theme :: Theme
  }

data Action = SetTheme Theme

reducer :: State -> Action -> State
reducer st = case _ of
  SetTheme theme -> st { theme = theme }

useAppStore :: forall m a. MonadEffect m => Eq a => UseHelixHook State Action a m
useAppStore = makeStore "app" reducer { theme: Light } mw
  where
  mw :: HelixMiddleware' State Action m
  mw = persistTheme <| applyTheme

  applyTheme _ act next = case act of
    SetTheme theme -> do
      liftEffect do
        htmlDoc <- HTML.window >>= Window.document >>= HTMLDocument.documentElement
        case htmlDoc of
          Nothing -> pure unit
          Just htmlElem
            | el <- HTMLhtmlElement.toElement htmlElem -> do
                domTokens <- classList el
                case theme of
                  Dark -> domTokens `DOMTokenList.add` darkClass
                  _ -> domTokens `DOMTokenList.remove` darkClass
      next act

  persistTheme _ act next = case act of
    SetTheme thm -> do
      liftEffect do
        ls <- HTML.window >>= Window.localStorage
        Storage.setItem "theme" (show thm) ls
      next act

foreign import data UseApp :: (Type -> Type) -> HookType

type UseApp' m = UseHelix State m <> UseEffect <> Hooks.Pure

type AppAPI m =
  { theme :: Theme
  , isDark :: Hooks.HookM m Boolean
  , setTheme :: Theme -> Hooks.HookM m Unit
  , toggleTheme :: Hooks.HookM m Unit
  }

instance HookNewtype (UseApp m) (UseApp' m)

useApp :: forall m. MonadEffect m => Hook m (UseApp m) (AppAPI m)
useApp = Hooks.wrap h
  where
  h :: Hook _ (UseApp' m) _
  h = Hooks.do
    st /\ ctx <- useAppStore identity

    let
      setTheme = ctx.dispatch <<< SetTheme

      toggleTheme = do
        { theme } <- ctx.getState
        ctx.dispatch $ SetTheme (toggle theme)

    useLifecycleEffect do
      setTheme =<< liftEffect do
        htmlDoc <- HTML.window >>= Window.document >>= HTMLDocument.documentElement
        case htmlDoc of
          Nothing -> pure Light
          Just htmlElem
            | el <- HTMLhtmlElement.toElement htmlElem -> do
                domTokens <- classList el
                isDark <- domTokens `DOMTokenList.contains` darkClass
                pure $ if isDark then Dark else Light
      pure Nothing

    Hooks.pure
      { theme: st.theme
      , isDark: ctx.getState <#> (_.theme >>> (_ == Dark))
      , setTheme
      , toggleTheme
      }