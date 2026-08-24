-- | The parts of the CLI that do not need a filesystem to be interesting.
-- |
-- | Discovery does need one, but only as an effect, so it gets a made-up one:
-- | working out what a grammar's module is called has nothing to do with
-- | whether the file is really there.
module Test.Puppy.CLI.Main where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains, stripPrefix)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Data.Either (Either(..), either, isLeft)
import Puppy.CLI.Effect.Filesystem (FilesystemF(..))
import Puppy.CLI.Effect.Log (LogF(..))
import Puppy.CLI.Effect.Log as Log
import Puppy.CLI.Effect.Process (ProcessF(..))
import Puppy.CLI.Effect.Process as Proc
import Puppy.CLI.Effect.Prompt (Answer(..), PromptF(..))
import Puppy.CLI.Effect.Prompt as Prompt
import Puppy.Codegen as Codegen
import Puppy.CLI.Generate (Options)
import Puppy.CLI.Generate as Generate
import Puppy.CLI.Effect.Filesystem as FS
import Puppy.CLI.Workspace (Grammar, Package)
import Puppy.CLI.Workspace as Workspace
import Run (Run)
import Run as Run
import Run.State (STATE)
import Run.State as State
import Run.Except as Except
import Test.Spec (describe, it)
import Type.Row (type (+))
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | A filesystem made of nothing but a directory listing.
tree :: Map String (Array String)
tree = Map.fromFoldable
  [ Tuple "demo/src" [ "Foo", "Top.pursy", "Top.purs", "notes.txt" ]
  , Tuple "demo/src/Foo" [ "Bar", "Parser.pursy", "Parser.purs" ]
  , Tuple "demo/src/Foo/Bar" [ "Deep.pursy" ]
  ]

-- | A filesystem that answers from the listing above, and refuses to read the
-- | directories it is told to refuse.
fake :: forall r. Array String -> FilesystemF ~> Run r
fake unreadable = case _ of
  ReadDir path k
    | Array.elem path unreadable -> pure (k (Left (refusal path)))
    | otherwise -> pure (k (Right (Map.lookup path tree)))
  IsDirectory path k -> pure (k (Right (Map.member path tree)))
  ReadText path k -> pure (k (Left (refusal path)))
  ReadIfPresent _ k -> pure (k (Right Nothing))
  WriteText _ _ k -> pure (k (Right unit))
  MkdirP _ k -> pure (k (Right unit))
  Remove _ k -> pure (k (Right unit))
  SameFile a b k -> pure (k (Right (a == b)))
  Relative path k -> pure (k path)
  where
  refusal path = { path, reason: "permission denied" }

-- | Where a workspace sits when it is not where the tool was run. Spago reports
-- | package paths rooted at the workspace, so they reach discovery absolute.
elsewhere :: String
elsewhere = "/somewhere/else"

-- | A filesystem under that root, with one of the two operations discovery
-- | performs refusing. `Relative` behaves as the Node interpreter does: it
-- | brings a path back down to where the reader is standing.
rooted :: forall r. { refuseDir :: Boolean } -> FilesystemF ~> Run r
rooted { refuseDir } = case _ of
  ReadDir path k
    | refuseDir -> pure (k (Left (refusal path)))
    | otherwise -> pure (k (Right (Just [ "Foo" ])))
  IsDirectory path k
    | refuseDir -> pure (k (Right false))
    | otherwise -> pure (k (Left (refusal path)))
  Relative path k ->
    pure (k (fromMaybe path (stripPrefix (Pattern (elsewhere <> "/")) path)))
  ReadText path k -> pure (k (Left (refusal path)))
  ReadIfPresent _ k -> pure (k (Right Nothing))
  WriteText _ _ k -> pure (k (Right unit))
  MkdirP _ k -> pure (k (Right unit))
  Remove _ k -> pure (k (Right unit))
  SameFile a b k -> pure (k (Right (a == b)))
  where
  refusal path = { path, reason: "permission denied" }

discoverAt :: { refuseDir :: Boolean } -> Either String (Array Grammar)
discoverAt how = Run.extract
  ( Except.runExcept
      ( FS.interpret (rooted how)
          (Workspace.grammarsIn { name: "demo", path: elsewhere <> "/demo" })
      )
  )

demo :: Package
demo = { name: "demo", path: "demo" }

discover :: Array String -> Either String (Array Grammar)
discover unreadable = Run.extract
  (Except.runExcept (FS.interpret (fake unreadable) (Workspace.grammarsIn demo)))

found :: Array Grammar
found = either (const []) identity (discover [])

-- | What `spago ls packages --json` looks like, cut down to the shape that
-- | matters: workspace entries carry a path, registry ones do not.
spagoSaid :: Json
spagoSaid = Workspace.parseJson
  """
  { "prelude": { "type": "registry", "value": { "version": "6.0.1" } }
  , "demo": { "type": "workspace", "value": { "path": "demo", "package": { "name": "demo" } } }
  , "tools": { "type": "workspace", "value": { "path": "packages/tools", "package": { "name": "tools" } } }
  }
  """
  (const emptyJson)
  identity

foreign import emptyJson :: Json

-- | A filesystem that writes nothing down but remembers being asked.
recording :: forall r. FilesystemF ~> Run (STATE (Array String) + r)
recording = case _ of
  WriteText path _ k -> do
    State.modify (flip Array.snoc ("write " <> path))
    pure (k (Right unit))
  Remove path k -> do
    State.modify (flip Array.snoc ("remove " <> path))
    pure (k (Right unit))
  MkdirP _ k -> pure (k (Right unit))
  ReadText path k -> pure (k (Left { path, reason: "not here" }))
  ReadIfPresent _ k -> pure (k (Right Nothing))
  ReadDir _ k -> pure (k (Right Nothing))
  IsDirectory _ k -> pure (k (Right false))
  SameFile a b k -> pure (k (Right (a == b)))
  Relative path k -> pure (k path)

-- | A filesystem with something already where the module is going, and a note
-- | of everything it was asked to write.
holding
  :: forall r
   . Maybe String
  -> FilesystemF ~> Run (STATE (Array String) + r)
holding contents = case _ of
  ReadIfPresent _ k -> pure (k (Right contents))
  WriteText path _ k -> do
    State.modify (flip Array.snoc ("write " <> path))
    pure (k (Right unit))
  MkdirP _ k -> pure (k (Right unit))
  ReadText path k -> pure (k (Left { path, reason: "not here" }))
  ReadDir _ k -> pure (k (Right Nothing))
  IsDirectory _ k -> pure (k (Right false))
  Remove _ k -> pure (k (Right unit))
  SameFile a b k -> pure (k (Right (a == b)))
  Relative path k -> pure (k path)

-- | Someone who always gives the same answer, and a note of being asked at all.
answering :: forall r. Answer -> PromptF ~> Run (STATE (Array String) + r)
answering answer = case _ of
  Confirm question k -> do
    State.modify (flip Array.snoc ("ask " <> question))
    pure (k answer)

-- | Write a module over whatever is there, with that answer ready.
writing
  :: Maybe String
  -> Answer
  -> { touched :: Array String, refused :: Maybe String }
writing contents answer = case run of
  Tuple touched refused -> { touched, refused }
  where
  run = Run.extract
    ( State.runState []
        ( FS.interpret (holding contents)
            ( Log.interpret quiet
                ( Prompt.interpret (answering answer)
                    (Generate.write grammar grammar.source "module Foo where\n")
                )
            )
        )
    )

quiet :: forall r. LogF ~> Run r
quiet = case _ of
  Log _ _ next -> pure next

grammar :: Grammar
grammar =
  { source: "demo/src/Foo.pursy"
  , output: "demo/src/Foo.purs"
  , moduleName: "Foo"
  }

asked :: Options -> Array String -> Array String
asked opts conflicts = fst
  ( Run.extract
      ( State.runState []
          (FS.interpret recording (Log.interpret quiet (Generate.reportConflicts opts grammar grammar.source conflicts)))
      )
  )

explaining :: Options
explaining =
  { package: Nothing
  , grammar: Nothing
  , moduleName: Nothing
  , output: Nothing
  , allowConflicts: false
  , emitExplain: true
  }

-- | Resolving a grammar named by path, with a filesystem that answers whether
-- | two names reach one file.
resolving :: Options -> Either String (Array Grammar)
resolving opts = Run.extract
  ( Except.runExcept
      (Proc.interpret noProcess (FS.interpret (fake []) (Generate.resolve opts)))
  )

-- | Naming a grammar by path never asks spago anything.
noProcess :: forall r. ProcessF ~> Run r
noProcess = case _ of
  Capture _ _ k -> pure (k (Left "no process here"))

byPath :: Options
byPath = explaining
  { grammar = Just "demo/src/Foo.pursy"
  , moduleName = Just "Foo"
  , emitExplain = false
  }

-- | A spago that answers the two questions Puppy asks it, from a workspace
-- | rooted somewhere other than where Puppy was started.
spagoAt :: forall r. String -> ProcessF ~> Run r
spagoAt root = case _ of
  Capture _ args k
    | Array.elem "paths" args -> pure
        ( k
            ( Right
                ("[[\"Local cache path\", \"" <> root <> "/.spago\"]]")
            )
        )
    | otherwise -> pure (k (Right packagesJson))

packagesJson :: String
packagesJson =
  """
  { "prelude": { "type": "registry", "value": { "version": "6.0.1" } }
  , "demo": { "type": "workspace", "value": { "path": "demo", "package": { "name": "demo" } } }
  , "tools": { "type": "workspace", "value": { "path": "packages/tools", "package": { "name": "tools" } } }
  }
  """

listed :: String -> Either String (Array Package)
listed root = Run.extract
  (Except.runExcept (Proc.interpret (spagoAt root) Workspace.packages))

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Puppy.CLI.Workspace" do
    it "takes the workspace packages and leaves the registry ones" do
      Array.sort (map _.name (Workspace.workspaceOnly spagoSaid))
        `shouldEqual` [ "demo", "tools" ]

    it "keeps the path spago reported" do
      map _.path
        (Array.filter (\p -> p.name == "tools") (Workspace.workspaceOnly spagoSaid))
        `shouldEqual` [ "packages/tools" ]

    -- The position under `src` is the module name, which is the whole reason a
    -- package is a better thing to name than an output path.
    it "names a module after where its grammar sits" do
      Array.sort (map _.moduleName found)
        `shouldEqual` [ "Foo.Bar.Deep", "Foo.Parser", "Top" ]

    it "writes each module beside its grammar" do
      Array.sort (map _.output found) `shouldEqual`
        [ "demo/src/Foo/Bar/Deep.purs"
        , "demo/src/Foo/Parser.purs"
        , "demo/src/Top.purs"
        ]

    it "ignores everything that is not a grammar" do
      Array.length found `shouldEqual` 3

    -- Taking an unreadable directory for an empty one would report success and
    -- leave whatever was generated last time exactly where it was.
    it "fails rather than calling an unreadable directory empty" do
      isLeft (discover [ "demo/src/Foo" ]) `shouldEqual` true

    it "says which directory it could not read" do
      discover [ "demo/src/Foo" ]
        `shouldEqual` Left "demo/src/Foo: permission denied"

    -- Package paths are rooted at the workspace so that the tool works from
    -- anywhere; a failure while looking through one must still be reported the
    -- way every other path this run mentions is.
    it "says which directory it could not read, from where the reader is" do
      discoverAt { refuseDir: true }
        `shouldEqual` Left "demo/src: permission denied"

    it "says which entry it could not stat, from where the reader is" do
      discoverAt { refuseDir: false }
        `shouldEqual` Left "demo/src/Foo: permission denied"

  describe "Puppy.CLI.Generate" do
    it "writes a report beside a grammar that has conflicts" do
      asked explaining [ "a conflict" ]
        `shouldEqual` [ "write " <> Generate.explainPath grammar ]

    -- A report from a run when there were conflicts describes a grammar that
    -- no longer has them.
    it "takes the report away once the conflicts are settled" do
      asked explaining []
        `shouldEqual` [ "remove " <> Generate.explainPath grammar ]

    it "leaves the disk alone when no report was asked for" do
      asked (explaining { emitExplain = false }) [ "a conflict" ]
        `shouldEqual` []

    -- The grammar is read before the module is written, so writing over it
    -- would succeed and leave nothing to have generated from.
    it "refuses to write the module over the grammar" do
      resolving (byPath { output = Just "demo/src/Foo.pursy" })
        `shouldEqual` Left
          "demo/src/Foo.pursy is the grammar being read from; writing the module there would destroy it"

    it "writes beside the grammar when told nothing else" do
      map (map _.output) (resolving byPath)
        `shouldEqual` Right [ "demo/src/Foo.purs" ]

    it "takes an output path that is somewhere else" do
      map (map _.output) (resolving (byPath { output = Just "build/Foo.purs" }))
        `shouldEqual` Right [ "build/Foo.purs" ]

    -- Spago reports package paths relative to the workspace root, and Puppy is
    -- not obliged to have been started there. Resolving them where Puppy
    -- happens to stand finds nothing and says so cheerfully.
    it "roots package paths at the workspace, not at the working directory" do
      map (map _.path) (listed "/somewhere/else")
        `shouldEqual` Right [ "/somewhere/else/demo", "/somewhere/else/packages/tools" ]

    it "keeps looking for the same packages wherever it was run" do
      map (map _.name) (listed "/elsewhere/again")
        `shouldEqual` Right [ "demo", "tools" ]

    -- A grammar put beside a module somebody wrote by hand is an easy mistake
    -- to make and an expensive one to make quietly.
    describe "writing over what is already there" do
      it "writes where there is nothing, without asking" do
        let out = writing Nothing No
        out.touched `shouldEqual` [ "write demo/src/Foo.purs" ]
        out.refused `shouldEqual` Nothing

      it "replaces a module it wrote before, without asking" do
        let
          out = writing
            (Just (Codegen.marker <> "\nmodule Foo where\n"))
            No
        out.touched `shouldEqual` [ "write demo/src/Foo.purs" ]
        out.refused `shouldEqual` Nothing

      -- The marker is the whole of the first line, not the start of it. A file
      -- that merely begins with those characters is somebody's own, and this
      -- is the check standing between it and a generator.
      it "asks about a file whose first line only begins like ours" do
        let
          out = writing
            (Just (Codegen.marker <> "example\nmodule Foo where\n"))
            No
        Array.elem "write demo/src/Foo.purs" out.touched `shouldEqual` false
        map (contains (Pattern "declined")) out.refused `shouldEqual` Just true

      -- Puppy writes newlines; a checkout can hand them back with a carriage
      -- return in front, and that is still a file Puppy wrote.
      it "knows one of ours that came back with CRLF line endings" do
        let
          out = writing
            (Just (Codegen.marker <> "\r\nmodule Foo where\r\n"))
            No
        out.touched `shouldEqual` [ "write demo/src/Foo.purs" ]
        out.refused `shouldEqual` Nothing

      it "asks before writing over anything else" do
        let out = writing (Just "module Foo where\n") Yes
        Array.length (Array.filter (contains (Pattern "ask ")) out.touched)
          `shouldEqual` 1
        Array.elem "write demo/src/Foo.purs" out.touched `shouldEqual` true
        out.refused `shouldEqual` Nothing

      it "leaves it alone when told to" do
        let out = writing (Just "module Foo where\n") No
        Array.elem "write demo/src/Foo.purs" out.touched `shouldEqual` false
        map (contains (Pattern "declined")) out.refused `shouldEqual` Just true

      -- A build script or a CI job. Waiting for an answer there waits for ever,
      -- so nothing is asked and nothing is written.
      it "leaves it alone when there is nobody to ask" do
        let out = writing (Just "module Foo where\n") NoOneToAsk
        Array.elem "write demo/src/Foo.purs" out.touched `shouldEqual` false
        map (contains (Pattern "not a terminal")) out.refused
          `shouldEqual` Just true
