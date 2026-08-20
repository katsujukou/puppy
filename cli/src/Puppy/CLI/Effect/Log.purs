-- | Saying things, as an effect.
-- |
-- | Plain text and nothing else. What this tool has to say is mostly conflict
-- | reports, which are paragraphs, and paths, which are already hard to read
-- | when decorated.
module Puppy.CLI.Effect.Log
  ( LogF(..)
  , LOG
  , Level(..)
  , _log
  , interpret
  , info
  , warn
  , error
  ) where

import Prelude

import Run (Run)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data Level
  = Info
  | Warn
  | Error

derive instance Eq Level

data LogF a = Log Level String a

derive instance Functor LogF

type LOG r = (log :: LogF | r)

_log :: Proxy "log"
_log = Proxy

interpret :: forall r a. (LogF ~> Run r) -> Run (LOG + r) a -> Run r a
interpret handler = Run.interpret (Run.on _log handler Run.send)

say :: forall r. Level -> String -> Run (LOG + r) Unit
say level message = Run.lift _log (Log level message unit)

info :: forall r. String -> Run (LOG + r) Unit
info = say Info

warn :: forall r. String -> Run (LOG + r) Unit
warn = say Warn

error :: forall r. String -> Run (LOG + r) Unit
error = say Error
