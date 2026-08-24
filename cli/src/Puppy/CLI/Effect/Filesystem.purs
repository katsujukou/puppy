-- | The bits of a filesystem this tool needs, as an effect it can be handed
-- | rather than one it reaches for.
-- |
-- | Every operation that can fail says so with an `IOError` carrying the path
-- | it was working on. Neither of the obvious alternatives will do: an
-- | exception escapes the effect row and lands wherever the interpreter
-- | happens to be, and a bare `Maybe` cannot tell "there is no such directory"
-- | from "that directory could not be read", which are answers a generator has
-- | to treat differently.
module Puppy.CLI.Effect.Filesystem
  ( IOError
  , FilesystemF(..)
  , FS
  , _fs
  , interpret
  , readText
  , readIfPresent
  , writeText
  , readDir
  , isDirectory
  , mkdirP
  , remove
  , sameFile
  , relative
  , describe
  , blame
  ) where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Fmt as Fmt
import Run (Run)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type IOError =
  { path :: String
  , reason :: String
  }

describe :: IOError -> String
describe e = Fmt.fmt @"{path}: {reason}" { path: e.path, reason: e.reason }

data FilesystemF a
  = ReadText String (Either IOError String -> a)
  -- | The same, for a file that need not be there. `Nothing` is an ordinary
  -- | answer -- nothing has been generated here yet -- and is what tells that
  -- | apart from a file that is there and cannot be read, which is a reason to
  -- | stop rather than to carry on and overwrite it.
  | ReadIfPresent String (Either IOError (Maybe String) -> a)
  | WriteText String String (Either IOError Unit -> a)
  -- | `Nothing` for a directory that is not there, which is an ordinary answer
  -- | -- a package need not have a `src`. Anything else is a failure.
  | ReadDir String (Either IOError (Maybe (Array String)) -> a)
  | IsDirectory String (Either IOError Boolean -> a)
  | MkdirP String (Either IOError Unit -> a)
  -- | Removing what is not there is not a failure; the point is that it is
  -- | gone afterwards.
  | Remove String (Either IOError Unit -> a)
  -- | Would writing to the second clobber the first? Two names can reach one
  -- | file, and comparing the names cannot tell.
  | SameFile String String (Either IOError Boolean -> a)
  -- | This path as someone standing here would write it. For saying things,
  -- | not for reaching anything.
  | Relative String (String -> a)

derive instance Functor FilesystemF

type FS r = (fs :: FilesystemF | r)

_fs :: Proxy "fs"
_fs = Proxy

interpret :: forall r a. (FilesystemF ~> Run r) -> Run (FS + r) a -> Run r a
interpret handler = Run.interpret (Run.on _fs handler Run.send)

readText :: forall r. String -> Run (FS + r) (Either IOError String)
readText path = Run.lift _fs (ReadText path identity)

readIfPresent
  :: forall r. String -> Run (FS + r) (Either IOError (Maybe String))
readIfPresent path = Run.lift _fs (ReadIfPresent path identity)

writeText :: forall r. String -> String -> Run (FS + r) (Either IOError Unit)
writeText path contents = Run.lift _fs (WriteText path contents identity)

readDir
  :: forall r. String -> Run (FS + r) (Either IOError (Maybe (Array String)))
readDir path = Run.lift _fs (ReadDir path identity)

isDirectory :: forall r. String -> Run (FS + r) (Either IOError Boolean)
isDirectory path = Run.lift _fs (IsDirectory path identity)

mkdirP :: forall r. String -> Run (FS + r) (Either IOError Unit)
mkdirP path = Run.lift _fs (MkdirP path identity)

remove :: forall r. String -> Run (FS + r) (Either IOError Unit)
remove path = Run.lift _fs (Remove path identity)

sameFile :: forall r. String -> String -> Run (FS + r) (Either IOError Boolean)
sameFile a b = Run.lift _fs (SameFile a b identity)

relative :: forall r. String -> Run (FS + r) String
relative path = Run.lift _fs (Relative path identity)

-- | A filesystem failure, with its path written the way the reader would.
-- |
-- | `describe` is the pure form, for when there is no effect to hand. Anything
-- | reporting to a person should use this one, so that a single run does not
-- | mix relative and absolute paths.
blame :: forall r. IOError -> Run (FS + r) String
blame problem = do
  shown <- relative problem.path
  pure (describe problem { path = shown })
