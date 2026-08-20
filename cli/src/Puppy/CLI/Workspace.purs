-- | Finding the grammars, and where their modules should be called.
-- |
-- | Spago is asked where its packages are rather than told: `spago ls packages
-- | --json` reports every package it knows about, and marks the ones belonging
-- | to the workspace. Reading `spago.yaml` directly would mean writing a YAML
-- | parser and then keeping it in step with a format that is not ours.
-- |
-- | Given a package, the rest follows from the one convention Spago does fix:
-- | sources live under `src`. That makes `src/Foo/Parser.pursy` unambiguously
-- | module `Foo.Parser`, which is the whole reason a package is a better thing
-- | to name than a path.
module Puppy.CLI.Workspace
  ( Package
  , Grammar
  , packages
  , workspaceOnly
  , parseJson
  , packageNamed
  , grammarsIn
  , extension
  ) where

import Prelude

import Data.Argonaut.Core (Json, toObject, toString)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.Common (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Fmt as Fmt
import Foreign.Object as Object
import Node.Path as Path
import Puppy.CLI.Effect.Filesystem (FS)
import Puppy.CLI.Effect.Filesystem as FS
import Puppy.CLI.Effect.Process (PROC)
import Puppy.CLI.Effect.Process as Proc
import Run (Run)
import Run.Except (EXCEPT)
import Run.Except as Except
import Type.Row (type (+))

-- | The extension a grammar file carries. `purs` plus `y`, in the tradition
-- | that names a parser generator's input after the language it produces and
-- | the tool it descends from.
extension :: String
extension = ".pursy"

type Package =
  { name :: String
  , path :: String
  }

type Grammar =
  { source :: String
  -- ^ Where the grammar is.
  , output :: String
  -- ^ Where its module goes: beside it, with the extension changed.
  , moduleName :: String
  }

-- | Every package in the workspace, as Spago sees it.
packages :: forall r. Run (PROC + EXCEPT String + r) (Array Package)
packages = do
  -- `--quiet` because spago narrates its progress on standard error, and a
  -- generator that says nothing on a good run should not borrow someone
  -- else's voice to say it.
  said <- Proc.capture "spago" [ "ls", "packages", "--json", "--quiet" ]
  case said of
    Left message -> Except.throw
      ( Fmt.fmt
          @"could not ask spago where its packages are: {message}\n\
          \Naming a package only means something inside a spago workspace; \
          \give a grammar and a module name instead."
          { message }
      )
    Right json -> parseJson json
      ( \message -> Except.throw
          ( Fmt.fmt @"spago answered with something that is not JSON: {message}"
              { message }
          )
      )
      (pure <<< workspaceOnly)

workspaceOnly :: Json -> Array Package
workspaceOnly json = fromMaybe [] do
  entries <- toObject json
  pure (Array.mapMaybe named (Object.toUnfoldable entries))
  where
  named (Tuple name entry) = do
    fields <- toObject entry
    kind <- toString =<< Object.lookup "type" fields
    if kind /= "workspace" then Nothing
    else do
      value <- toObject =<< Object.lookup "value" fields
      path <- toString =<< Object.lookup "path" value
      pure { name, path }

packageNamed
  :: forall r. String -> Run (PROC + EXCEPT String + r) Package
packageNamed name = do
  known <- packages
  case Array.find (\p -> p.name == name) known of
    Just found -> pure found
    Nothing -> Except.throw
      ( Fmt.fmt @"no package named `{name}` in this workspace; spago knows {known}"
          { name, known: joinWith ", " (map _.name known) }
      )

-- | Every grammar under a package's `src`, with the module name its position
-- | there gives it.
-- |
-- | A package with no `src` has no grammars, which is an answer. A `src` that
-- | cannot be read is not: taking it for an empty one would report success and
-- | leave whatever was generated last time in place.
grammarsIn
  :: forall r. Package -> Run (FS + EXCEPT String + r) (Array Grammar)
grammarsIn package = walk (Path.concat [ package.path, "src" ]) []
  where
  walk dir prefix = do
    entries <- FS.readDir dir
    case entries of
      Left problem -> Except.throw (FS.describe problem)
      Right Nothing -> pure []
      Right (Just names) -> map Array.concat (traverse (visit dir prefix) names)

  visit dir prefix name = do
    let
      here = Path.concat [ dir, name ]
    directory <- FS.isDirectory here
    case directory of
      Left problem -> Except.throw (FS.describe problem)
      Right true -> walk here (Array.snoc prefix name)
      Right false -> pure (grammarAt dir prefix name)

  grammarAt dir prefix name =
    case String.stripSuffix (String.Pattern extension) name of
      Nothing -> []
      Just stem ->
        [ { source: Path.concat [ dir, name ]
          , output: Path.concat [ dir, stem <> ".purs" ]
          , moduleName: joinWith "." (Array.snoc prefix stem)
          }
        ]

-- | `JSON.parse`, which `argonaut-core` has no counterpart for in this package
-- | set. `Json` is the raw value, so there is nothing to convert.
parseJson :: forall a. String -> (String -> a) -> (Json -> a) -> a
parseJson text onError onOk = parseJsonImpl onError onOk text

foreign import parseJsonImpl
  :: forall a. (String -> a) -> (Json -> a) -> String -> a
