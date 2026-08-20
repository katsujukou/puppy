-- | The box a generated parser keeps its semantic values in.
-- |
-- | A parser stack holds values of every type the grammar produces, so the
-- | driver cannot be told what is on it. `Value` is what it is told instead:
-- | an opaque type it only ever moves around.
-- |
-- | The coercions are unsound on their own, and that is the point of this
-- | module -- there are exactly two of them, they sit here, and generated code
-- | never mentions `Unsafe.Coerce` itself. What makes them safe in practice is
-- | that a value is only ever unboxed at the position it was boxed at: the
-- | grammar fixes which production puts a value on the stack and which one
-- | takes it off, and the generator emits both halves from the same place.
module Puppy.Runtime.Value
  ( Value
  , box
  , unbox
  , slot
  , internalError
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Value :: Type

box :: forall a. a -> Value
box = unsafeCoerce

unbox :: forall a. Value -> a
unbox = unsafeCoerce

-- | One of the values a production is reducing over.
-- |
-- | The driver hands a semantic action exactly as many values as the production
-- | has symbols, so an index the generator emitted can only be out of range if
-- | the tables and the actions disagree -- which is a bug in the generator, not
-- | something a grammar can cause.
slot :: Int -> Array Value -> Value
slot i values = case Array.index values i of
  Just value -> value
  Nothing -> internalError
    ( "semantic action asked for value " <> show i <> " of "
        <> show (Array.length values)
    )

-- | Something the generator promised would not happen.
-- |
-- | Reserved for broken tables. Nothing a grammar or an input can do should
-- | reach one of these; a parse error is an `Either`, not a crash.
internalError :: forall a. String -> a
internalError message = unsafeCrashWith ("puppy: " <> message)
