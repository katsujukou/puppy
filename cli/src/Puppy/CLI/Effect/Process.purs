-- | Running another program and reading what it said.
-- |
-- | One operation, because there is one thing to ask: `spago`, where its
-- | packages are. Failure comes back as `Left` rather than an exception,
-- | because "spago is not installed" is something to explain, not to crash on.
module Puppy.CLI.Effect.Process
  ( ProcessF(..)
  , PROC
  , _proc
  , interpret
  , capture
  ) where

import Prelude

import Data.Either (Either)
import Run (Run)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data ProcessF a = Capture String (Array String) (Either String String -> a)

derive instance Functor ProcessF

type PROC r = (proc :: ProcessF | r)

_proc :: Proxy "proc"
_proc = Proxy

interpret :: forall r a. (ProcessF ~> Run r) -> Run (PROC + r) a -> Run r a
interpret handler = Run.interpret (Run.on _proc handler Run.send)

capture :: forall r. String -> Array String -> Run (PROC + r) (Either String String)
capture command args = Run.lift _proc (Capture command args identity)
