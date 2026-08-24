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
  , write
  ) where

import Prelude

import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Either (Either(..))
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
import Puppy.CLI.Effect.Prompt (PROMPT)
import Puppy.CLI.Effect.Prompt as Prompt
import Puppy.CLI.Workspace (Grammar)
import Puppy.CLI.Workspace as Workspace
import Puppy.Codegen as Codegen
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

cmd
  :: forall r
   . Options
  -> Run (FS + LOG + PROC + PROMPT + EXCEPT String + r) Unit
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
  -> Run (FS + LOG + PROMPT + EXCEPT String + r) (Maybe String)
generateOne opts target = do
  -- Worked out once and used for every path this run says out loud. Paths are
  -- resolved from the workspace root so that the tool works from anywhere, and
  -- read back from where the reader is standing so that they are short.
  shown <- FS.relative target.source
  read <- FS.readText target.source
  case read of
    Left problem -> Just <$> FS.blame problem
    Right source -> case Pipeline.compile { moduleName: target.moduleName } source of
      Left problems -> pure (Just (joinWith "\n" (map (render shown) problems)))
      Right compiled -> do
        told <- reportConflicts opts target shown compiled.conflicts
        case told of
          Just problem -> pure (Just problem)
          Nothing
            | Array.null compiled.conflicts || opts.allowConflicts ->
                write target shown compiled.source
            | otherwise -> pure
                ( Just
                    ( Fmt.fmt
                        @"{path}: {count} unsettled conflict(s); nothing written. Settle them, or pass --allow-conflicts."
                        { path: shown, count: Array.length compiled.conflicts }
                    )
                )

write
  :: forall r
   . Grammar
  -> String
  -> String
  -> Run (FS + LOG + PROMPT + r) (Maybe String)
write target shown source = do
  refusal <- mayWrite target
  case refusal of
    Just reason -> pure (Just reason)
    Nothing -> do
      made <- FS.mkdirP (Path.dirname target.output)
      case made of
        Left problem -> Just <$> FS.blame problem
        Right _ -> do
          written <- FS.writeText target.output source
          case written of
            Left problem -> Just <$> FS.blame problem
            Right _ -> do
              Log.info
                ( Fmt.fmt @"{path} -> {name}"
                    { path: shown, name: target.moduleName }
                )
              pure Nothing

-- | Nothing if the module may be written where it is going; otherwise why not.
-- |
-- | A generated module is Puppy's to replace, and says so on its first line.
-- | Anything else at that path is somebody's own work, and a generator that
-- | quietly writes over it has destroyed something no version of the grammar
-- | can bring back. The usual cause is innocent -- a `Foo.pursy` put beside a
-- | `Foo.purs` that was already there -- which is exactly why it is worth
-- | stopping for.
-- |
-- | The decision belongs to whoever is running the tool, so it is put to them.
-- | Where there is nobody to put it to, nothing is written: a build that would
-- | have destroyed a file should fail rather than guess.
mayWrite :: forall r. Grammar -> Run (FS + PROMPT + r) (Maybe String)
mayWrite target = do
  existing <- FS.readIfPresent target.output
  case existing of
    Left problem -> Just <$> FS.blame problem
    Right Nothing -> pure Nothing
    Right (Just contents)
      | ours contents -> pure Nothing
      | otherwise -> do
          path <- FS.relative target.output
          answer <- Prompt.confirm
            ( Fmt.fmt
                @"{path} was not written by Puppy. Overwrite it? [y/N] "
                { path }
            )
          pure case answer of
            Prompt.Yes -> Nothing
            Prompt.No -> Just
              ( Fmt.fmt
                  @"{path}: left as it was; overwriting it was declined"
                  { path }
              )
            Prompt.NoOneToAsk -> Just
              ( Fmt.fmt
                  @"{path}: exists and was not written by Puppy, and there is nobody to ask -- standard input is not a terminal. Move it aside, or send the module somewhere else."
                  { path }
              )

-- | Where a grammar's conflict report goes, when it is asked for.
explainPath :: Grammar -> String
explainPath target = stem target.source <> ".puppy-explain"

-- | Say what was left unsettled, and make sure what is said is current.
reportConflicts
  :: forall r
   . Options
  -> Grammar
  -> String
  -> Array String
  -> Run (FS + LOG + r) (Maybe String)
reportConflicts opts target shown conflicts
  -- A report from a run when there were conflicts describes a grammar that no
  -- longer has them. Leaving it there would be answering an old question.
  | Array.null conflicts && opts.emitExplain = do
      removed <- FS.remove (explainPath target)
      case removed of
        Left problem -> Just <$> FS.blame problem
        Right _ -> pure Nothing
  | Array.null conflicts = pure Nothing
  | opts.emitExplain = do
      written <- FS.writeText (explainPath target) (joinWith "\n" conflicts)
      case written of
        Left problem -> Just <$> FS.blame problem
        Right _ -> do
          report <- FS.relative (explainPath target)
          Log.warn
            ( Fmt.fmt @"{source}: {count} unsettled conflict(s), explained in {path}"
                { source: shown, count: Array.length conflicts, path: report }
            )
          pure Nothing
  | otherwise = do
      Log.warn
        ( Fmt.fmt @"{path}:\n\n{report}"
            { path: shown, report: joinWith "\n" conflicts }
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

-- | Did Puppy write this?
-- |
-- | The whole of the first line has to be the marker, not merely the start of
-- | it. A file beginning `-- Generated by Puppy. Do not edit.example` begins
-- | with the marker and is nobody's business but its author's, and this is the
-- | check that stands between a regenerated parser and their afternoon: it is
-- | the one place where being approximately right is worse than being useless.
-- |
-- | A trailing carriage return is allowed for. Puppy writes newlines, but a
-- | file it wrote can come back through a checkout that does not.
ours :: String -> Boolean
ours contents = case Array.head (String.split (String.Pattern "\n") contents) of
  Nothing -> false
  Just first -> withoutReturn first == Codegen.marker
  where
  withoutReturn line =
    fromMaybe line (String.stripSuffix (String.Pattern "\r") line)

-- | A grammar's path with its extension taken off.
stem :: String -> String
stem source =
  fromMaybe source (String.stripSuffix (String.Pattern Workspace.extension) source)
