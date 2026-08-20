-- | Turning grammars into parser modules.
-- |
-- | Which grammars is the interesting part. Naming a package is the usual way,
-- | and the one that makes module names follow from where a file sits; naming
-- | a file is the escape hatch for anything outside that convention, and then
-- | the module name has to be given, because nothing else says what it is.
module Puppy.CLI.Generate
  ( Options
  , options
  , cmd
  -- Exposed for the tests: which files a run touches is part of what this
  -- command promises, and saying so is easier than watching a disk.
  , explainPath
  , reportConflicts
  , resolve
  ) where

import Prelude

import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as String
import Data.String.Common (joinWith)
import Data.Traversable (traverse)
import Fmt as Fmt
import Node.Path as Path
import Puppy.CLI.Effect.Filesystem (FS)
import Puppy.CLI.Effect.Filesystem as FS
import Puppy.CLI.Effect.Log (LOG)
import Puppy.CLI.Effect.Log as Log
import Puppy.CLI.Effect.Process (PROC)
import Puppy.CLI.Workspace (Grammar)
import Puppy.CLI.Workspace as Workspace
import Puppy.Pipeline (Problem)
import Puppy.Pipeline as Pipeline
import Run (Run)
import Run.Except (EXCEPT)
import Run.Except as Except
import Type.Row (type (+))

type Options =
  { package :: Maybe String
  , grammar :: Maybe String
  , moduleName :: Maybe String
  , output :: Maybe String
  , allowConflicts :: Boolean
  , emitExplain :: Boolean
  }

options :: ArgParser.ArgParser Options
options = ArgParser.fromRecord
  { package:
      ArgParser.argument [ "--package", "-p" ]
        "Generate for one spago workspace package"
        # ArgParser.optional
  , grammar:
      ArgParser.anyNotFlag "GRAMMAR"
        "A single grammar to generate from, by path"
        # ArgParser.optional
  , moduleName:
      ArgParser.argument [ "--module", "-m" ]
        "Module name for the generated parser"
        # ArgParser.optional
  , output:
      ArgParser.argument [ "--output", "-o" ]
        "Where to write the generated module"
        # ArgParser.optional
  , allowConflicts:
      ArgParser.flag [ "--allow-conflicts" ]
        "Generate even where a conflict is unsettled"
        # ArgParser.boolean
  , emitExplain:
      ArgParser.flag [ "--emit-explain" ]
        "Write conflict reports to .puppy-explain files"
        # ArgParser.boolean
  }

cmd :: forall r. Options -> Run (FS + LOG + PROC + EXCEPT String + r) Unit
cmd opts = do
  targets <- resolve opts
  if Array.null targets then
    Log.warn
      (Fmt.fmt @"no {extension} files to generate from" { extension: Workspace.extension })
  else do
    failures <- traverse (generateOne opts) targets
    case Array.catMaybes failures of
      [] -> pure unit
      -- Every grammar is attempted before anything is said about the run, so
      -- that one broken file does not hide the state of the others.
      problems -> Except.throw (joinWith "\n\n" problems)

-- | Which grammars to generate from, and what to call what comes out.
resolve
  :: forall r. Options -> Run (FS + PROC + EXCEPT String + r) (Array Grammar)
resolve opts = case opts.grammar of
  Just source -> do
    when (isJust opts.package) do
      Except.throw "give a grammar or a package, not both"
    case opts.moduleName of
      Nothing -> Except.throw
        "a grammar named by path needs --module; nothing about the path says what the module is called"
      Just moduleName -> do
        let
          output = fromMaybe (besides source) opts.output
        -- The grammar is read before the module is written, so writing over it
        -- would succeed and leave nothing to have generated from. Two names can
        -- reach one file, so the paths are not what gets compared.
        clash <- FS.sameFile source output
        case clash of
          Left problem -> Except.throw (FS.describe problem)
          Right true -> Except.throw
            ( Fmt.fmt
                @"{output} is the grammar being read from; writing the module there would destroy it"
                { output }
            )
          Right false -> pure [ { source, output, moduleName } ]
  Nothing -> do
    when (isJust opts.moduleName || isJust opts.output) do
      Except.throw "--module and --output only mean anything for a grammar named by path"
    case opts.package of
      Just name -> Workspace.grammarsIn =<< Workspace.packageNamed name
      Nothing -> do
        known <- Workspace.packages
        map Array.concat (traverse Workspace.grammarsIn known)
  where
  besides source = stem source <> ".purs"

-- | Generate one module, and say what went wrong rather than stopping.
generateOne
  :: forall r
   . Options
  -> Grammar
  -> Run (FS + LOG + EXCEPT String + r) (Maybe String)
generateOne opts target = do
  read <- FS.readText target.source
  case read of
    Left problem -> pure (Just (FS.describe problem))
    Right source -> case Pipeline.compile { moduleName: target.moduleName } source of
      Left problems -> pure (Just (joinWith "\n" (map (render target.source) problems)))
      Right compiled -> do
        told <- reportConflicts opts target compiled.conflicts
        case told of
          Just problem -> pure (Just problem)
          Nothing
            | Array.null compiled.conflicts || opts.allowConflicts ->
                write target compiled.source
            | otherwise -> pure
                ( Just
                    ( Fmt.fmt
                        @"{path}: {count} unsettled conflict(s); nothing written. Settle them, or pass --allow-conflicts."
                        { path: target.source
                        , count: Array.length compiled.conflicts
                        }
                    )
                )

write
  :: forall r
   . Grammar
  -> String
  -> Run (FS + LOG + r) (Maybe String)
write target source = do
  made <- FS.mkdirP (Path.dirname target.output)
  case made of
    Left problem -> pure (Just (FS.describe problem))
    Right _ -> do
      written <- FS.writeText target.output source
      case written of
        Left problem -> pure (Just (FS.describe problem))
        Right _ -> do
          Log.info
            ( Fmt.fmt @"{path} -> {name}"
                { path: target.source, name: target.moduleName }
            )
          pure Nothing

-- | Where a grammar's conflict report goes, when it is asked for.
explainPath :: Grammar -> String
explainPath target = stem target.source <> ".puppy-explain"

-- | Say what was left unsettled, and make sure what is said is current.
reportConflicts
  :: forall r
   . Options
  -> Grammar
  -> Array String
  -> Run (FS + LOG + r) (Maybe String)
reportConflicts opts target conflicts
  -- A report from a run when there were conflicts describes a grammar that no
  -- longer has them. Leaving it there would be answering an old question.
  | Array.null conflicts && opts.emitExplain =
      map (either (Just <<< FS.describe) (const Nothing))
        (FS.remove (explainPath target))
  | Array.null conflicts = pure Nothing
  | opts.emitExplain = do
      written <- FS.writeText (explainPath target) (joinWith "\n" conflicts)
      case written of
        Left problem -> pure (Just (FS.describe problem))
        Right _ -> do
          Log.warn
            ( Fmt.fmt @"{source}: {count} unsettled conflict(s), explained in {path}"
                { source: target.source
                , count: Array.length conflicts
                , path: explainPath target
                }
            )
          pure Nothing
  | otherwise = do
      Log.warn
        ( Fmt.fmt @"{path}:\n\n{report}"
            { path: target.source, report: joinWith "\n" conflicts }
        )
      pure Nothing

-- | `path:line:column: message`, which is what an editor knows how to follow.
render :: String -> Problem -> String
render path problem = case problem.span of
  Nothing -> Fmt.fmt @"{path}: {message}" { path, message: problem.message }
  Just span -> Fmt.fmt @"{path}:{line}:{column}: {message}"
    { path
    , line: span.start.line
    , column: span.start.column
    , message: problem.message
    }

-- | A grammar's path with its extension taken off.
stem :: String -> String
stem source =
  fromMaybe source (String.stripSuffix (String.Pattern Workspace.extension) source)
