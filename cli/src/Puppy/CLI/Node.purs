-- | Discharging the effects against Node, and nothing else.
-- |
-- | Everything that touches the outside world is here. The rest of the CLI is
-- | written against the effects and never mentions `node:fs`, which is what
-- | lets a command be read without knowing how a file gets read.
-- |
-- | The foreign functions take the constructors they should build rather than
-- | throwing, so a failure arrives as a value in the same row as everything
-- | else and cannot slip past the error handling on its way out.
-- |
-- | They are foreign rather than `node-fs` calls because two things are needed
-- | that the wrapper does not give: the error *code*, to tell a missing
-- | directory from an unreadable one, and a write that goes through a rename,
-- | so that a failure leaves the previous module rather than half of a new one.
module Puppy.CLI.Node
  ( runNode
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (try)
import Effect.Exception as Exception
import Puppy.CLI.Effect.Filesystem (FS, FilesystemF(..), IOError)
import Puppy.CLI.Effect.Filesystem as FS
import Puppy.CLI.Effect.Log (LOG, Level(..), LogF(..))
import Puppy.CLI.Effect.Log as Log
import Puppy.CLI.Effect.Process (PROC, ProcessF(..))
import Puppy.CLI.Effect.Process as Proc
import Run (EFFECT, Run, liftEffect, runBaseEffect)
import Run.Except (EXCEPT)
import Run.Except as Except
import Type.Row (type (+))

-- | Run a command with its effect row closed. A `Left` is a message meant for
-- | whoever ran the tool, not a stack trace.
runNode
  :: forall a
   . Run (FS + LOG + PROC + EFFECT + EXCEPT String + ()) a
  -> Effect (Either String a)
runNode program = program
  # FS.interpret filesystem
  # Log.interpret logger
  # Proc.interpret process
  # Except.runExcept
  # runBaseEffect

filesystem :: forall r. FilesystemF ~> Run (EFFECT + r)
filesystem = case _ of
  ReadText path k -> k <$> liftEffect (readTextImpl Left Right path)
  WriteText path contents k ->
    k <$> liftEffect (writeTextImpl Left (Right unit) path contents)
  ReadDir path k ->
    k <$> liftEffect (readDirImpl Left (Right Nothing) (Right <<< Just) path)
  IsDirectory path k -> k <$> liftEffect (isDirectoryImpl Left Right path)
  MkdirP path k -> k <$> liftEffect (mkdirPImpl Left (Right unit) path)
  Remove path k -> k <$> liftEffect (removeImpl Left (Right unit) path)
  SameFile a b k -> k <$> liftEffect (sameFileImpl Left Right a b)

-- | Errors and warnings to stderr, so that a shell can keep them apart from
-- | whatever else is being written.
logger :: forall r. LogF ~> Run (EFFECT + r)
logger = case _ of
  Log level message next -> do
    liftEffect case level of
      Info -> Console.log message
      Warn -> Console.error message
      Error -> Console.error message
    pure next

process :: forall r. ProcessF ~> Run (EFFECT + r)
process = case _ of
  Capture command args k -> k <$> liftEffect do
    result <- try (captureImpl command args)
    pure case result of
      Left e -> Left (Exception.message e)
      Right out -> Right out

foreign import readTextImpl
  :: forall a. (IOError -> a) -> (String -> a) -> String -> Effect a

foreign import writeTextImpl
  :: forall a. (IOError -> a) -> a -> String -> String -> Effect a

foreign import readDirImpl
  :: forall a. (IOError -> a) -> a -> (Array String -> a) -> String -> Effect a

foreign import isDirectoryImpl
  :: forall a. (IOError -> a) -> (Boolean -> a) -> String -> Effect a

foreign import mkdirPImpl :: forall a. (IOError -> a) -> a -> String -> Effect a

foreign import removeImpl :: forall a. (IOError -> a) -> a -> String -> Effect a

foreign import sameFileImpl
  :: forall a. (IOError -> a) -> (Boolean -> a) -> String -> String -> Effect a

-- | Run a program and return its standard output. Its standard error is left
-- | alone, so that a tool with something to say still says it.
foreign import captureImpl :: String -> Array String -> Effect String
