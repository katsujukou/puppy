module Puppy.Docs.UI.Hooks.UseSingleton
  ( SingletonId
  , UseSingleton
  , getSingleton
  , mkSingleton
  , putSingleton
  , useSingleton
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Halogen.Hooks (class HookNewtype, HookType)
import Halogen.Hooks as Hooks

foreign import data SingletonIdRep :: Type -> Type

newtype SingletonId a = SingletonId (Effect (SingletonIdRep a))

foreign import _mkSingleton :: forall a. Fn2 String a (Effect (SingletonIdRep a))

mkSingleton :: forall a. String -> a -> SingletonId a
mkSingleton id = runFn2 _mkSingleton id >>> SingletonId

foreign import _getSingleton :: forall a. EffectFn1 (SingletonIdRep a) a

getSingleton :: forall a. SingletonId a -> Effect a
getSingleton (SingletonId id) = id >>= runEffectFn1 _getSingleton

foreign import _modifySingleton :: forall a. EffectFn2 (a -> a) (SingletonIdRep a) a

modifySingleton :: forall a. (a -> a) -> SingletonId a -> Effect a
modifySingleton f (SingletonId id) = id >>= runEffectFn2 _modifySingleton f

putSingleton :: forall a. a -> SingletonId a -> Effect Unit
putSingleton a = modifySingleton (const a) >>> void

foreign import data UseSingleton :: Type -> HookType

type UseSingleton' :: Type -> HookType
type UseSingleton' a = Hooks.Pure

instance HookNewtype (UseSingleton a) (UseSingleton' a)

useSingleton :: forall m a. MonadEffect m => SingletonId a -> Hooks.Hook m (UseSingleton a) (Hooks.HookM m a /\ ((a -> a) -> Hooks.HookM m Unit))
useSingleton id = Hooks.wrap hook
  where
  hook :: Hooks.Hook m (UseSingleton' a) _
  hook = Hooks.do
    let
      modify = \f -> liftEffect do
        -- id <- Ref.read ref
        void $ modifySingleton f id

      get = liftEffect (getSingleton id)

    Hooks.pure (get /\ modify)