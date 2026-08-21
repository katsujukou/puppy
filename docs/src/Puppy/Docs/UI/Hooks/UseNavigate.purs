module Puppy.Docs.UI.Hooks.UseNavigate where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Foldable (minimum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Halogen.Helix (HelixMiddleware, UseHelix, UseHelixHook, makeStore)
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookType, UseEffect, useLifecycleEffect)
import Halogen.Hooks as Hooks
import Halogen.Subscription as HS
import Puppy.Docs.UI.Hooks.UseSingleton (SingletonId, UseSingleton, mkSingleton, useSingleton)
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as RD
import Routing.PushState as PushState
import Unsafe.Coerce (unsafeCoerce)

type State route =
  { rawPath :: String
  , currentRoute :: Maybe route
  , initialized :: Boolean
  }

data Action route = Initialized | SetRoute { rawPath :: String, route :: Maybe route }

reducer :: forall route. State route -> Action route -> State route
reducer s0 = case _ of
  Initialized -> s0 { initialized = true }
  SetRoute { rawPath, route } -> s0 { rawPath = rawPath, currentRoute = route }

middlewareStack :: forall m route. MonadEffect m => HelixMiddleware (State route) (Action route) m
middlewareStack _ act next = do
  case act of
    SetRoute { rawPath } -> do
      liftEffect do
        { pushState, locationState } <- PushState.makeInterface
        loc <- locationState
        when (loc.pathname /= rawPath) do
          pushState (unsafeCoerce {}) rawPath
      next act
    _ -> next act

initialState :: forall route. State route
initialState =
  { rawPath: ""
  , currentRoute: Nothing
  , initialized: false
  }

useNavigatorStore :: forall m a route. Eq a => MonadEffect m => UseHelixHook (State route) (Action route) a m
useNavigatorStore = makeStore "navigate" reducer initialState middlewareStack

foreign import data UseNavigate :: (Type -> Type) -> Type -> HookType

type UseNavigate' m route = UseSingleton (Array (NavigationGuard m route))
  <> UseHelix (State route) m
  <> UseEffect
  <> Hooks.Pure

instance HookNewtype (UseNavigate m route) (UseNavigate' m route)

type NavigationGuard m route = { from :: Either String route, to :: route } -> Hooks.HookM m (Maybe route)

type UseNaviagteInterface route m =
  { currentRoute :: Maybe route
  , getCurrentRoute :: Hooks.HookM m (Maybe route)
  , navigateTo :: route -> Hooks.HookM m Unit
  , addGuard :: NavigationGuard m route -> Hooks.HookM m Unit
  , initialize :: Hooks.HookM m Unit
  }

evalGuards :: forall m route. Eq route => Either String route -> route -> Maybe route -> Array (NavigationGuard m route) -> Hooks.HookM m (Maybe route)
evalGuards from to' prev = Array.uncons >>> case _ of
  Nothing -> pure (Just to')
  Just { head: g, tail: rest } -> do
    guardResult <- g { from, to: to' }
    pure guardResult >>= case _ of
      Nothing -> pure Nothing
      Just rewritedTo
        | Just rewritedTo == prev -> pure (Just rewritedTo)
        | otherwise -> evalGuards from rewritedTo (Just to') rest

navguardsId :: forall m route. SingletonId (Array (NavigationGuard m route))
navguardsId = mkSingleton "navguards" []

-- A trailing slash names the same page as the path without one, but the parser
-- reads it as one more, empty, segment and so stops matching. Drop it before
-- parsing, leaving any query and hash where they are. "/" itself stays: there
-- the slash is the whole path, not a suffix on one.
dropTrailingSlash :: String -> String
dropTrailingSlash raw = case String.stripSuffix (Pattern "/") path of
  Just "" -> raw
  Just trimmed -> trimmed <> query
  Nothing -> raw
  where
  cut = fromMaybe (String.length raw)
    $ minimum
    $ Array.catMaybes [ String.indexOf (Pattern "?") raw, String.indexOf (Pattern "#") raw ]
  path = String.take cut raw
  query = String.drop cut raw

parseRoute :: forall r. RouteDuplex' r -> String -> Either String r
parseRoute route rawPath = case RD.parse route (rawPath) of
  Left _ -> Left rawPath
  Right r -> Right r

useNavigate
  :: forall m route
   . MonadEffect m
  => Eq route
  => RouteDuplex' route
  -> Hook m (UseNavigate m route) (UseNaviagteInterface route m)
useNavigate route = Hooks.wrap hook
  where
  hook :: _ _ (UseNavigate' _ _) _
  hook = Hooks.do
    getNavGuards /\ modifyNavGuards <- useSingleton navguardsId
    { currentRoute } /\ storeApi <- useNavigatorStore identity

    let
      navigateTo :: route -> Hooks.HookM m Unit
      navigateTo to = navigateToRaw (Right to)

      addGuard g = modifyNavGuards (_ `Array.snoc` g)

      navigateToRaw :: Either String route -> Hooks.HookM m Unit
      navigateToRaw (Left rawPath) = do
        Console.log "navigateToRaw Left"
        storeApi.dispatch (SetRoute { rawPath, route: Nothing })
      navigateToRaw (Right to) = do
        from <- storeApi.getState <#> \{ currentRoute: cr, rawPath } -> note rawPath cr
        guards <- getNavGuards
        evalGuards from to Nothing guards >>= case _ of
          Just to' -> do
            storeApi.dispatch $
              SetRoute { rawPath: RD.print route to', route: Just to' }
          Nothing -> do
            Console.log "navigation is cancelled by navguard"
            pure unit

      initialize = do
        -- 初期化直後のパス情報をStore反映させる
        inst <- liftEffect PushState.makeInterface
        liftEffect inst.locationState >>= _.path >>> dropTrailingSlash >>> parseRoute route >>> navigateToRaw
        storeApi.dispatch Initialized

    useLifecycleEffect $ do
      initialized <- storeApi.getState <#> _.initialized
      if initialized then pure Nothing
      else do
        inst <- liftEffect PushState.makeInterface
        let
          updateRoute :: PushState.LocationState -> Hooks.HookM m Unit
          updateRoute lc =
            let
              path = dropTrailingSlash lc.path
            in
              navigateToRaw (lmap (const path) $ RD.parse route path)

        { unlistenLoc, emitter } <- liftEffect do
          { emitter, listener } <- HS.create
          unlistenLoc <- inst.listen (HS.notify listener)
          pure { unlistenLoc, emitter: emitter <#> updateRoute }

        subscriptionId <- Hooks.subscribe emitter

        pure $ Just do
          liftEffect unlistenLoc
          Hooks.unsubscribe subscriptionId

    Hooks.pure $
      { currentRoute
      , getCurrentRoute: storeApi.getState <#> _.currentRoute
      , navigateTo
      , addGuard
      , initialize
      }
