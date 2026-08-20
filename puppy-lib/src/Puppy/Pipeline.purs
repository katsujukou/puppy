-- | Every pass, in order, behind one entry point.
-- |
-- | The passes each have their own error type and their own idea of what a
-- | position is, which is right for them and wrong for anyone calling all of
-- | them in a row. This flattens that: one function, one list of problems, each
-- | with a span where there is one to give.
-- |
-- | Nothing here does any I/O. What a caller does with an unresolved conflict
-- | -- refuse to write the file, or write it and complain -- is the caller's
-- | business, so both the module and the conflicts come back together.
module Puppy.Pipeline
  ( Problem
  , Compiled
  , compile
  ) where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Puppy.Codegen (generate)
import Puppy.Codegen.Emit (SourceMapping)
import Puppy.Expand (expand)
import Puppy.LR.Analysis (analyse)
import Puppy.LR.Automaton (Policy(..), build)
import Puppy.LR.Explain (report)
import Puppy.LR.Grammar (number)
import Puppy.LR.Table (tabulate, unresolved)
import Puppy.Syntax (Span)
import Puppy.Syntax.Parser as Parser

-- | Something wrong with the grammar. A span when the pass that found it knew
-- | where to point; the code generator's complaints are about the whole file.
type Problem =
  { message :: String
  , span :: Maybe Span
  }

type Compiled =
  { source :: String
  , mappings :: Array SourceMapping
  , conflicts :: Array String
  -- ^ The conflicts nothing in the grammar settled, each already explained.
  -- An empty array is the usual case and the one worth aiming for.
  }

located :: forall r. { message :: String, span :: Span | r } -> Problem
located e = { message: e.message, span: Just e.span }

compile :: { moduleName :: String } -> String -> Either (Array Problem) Compiled
compile options source = do
  syntax <- lmap (Array.singleton <<< located) (Parser.parse source)
  core <- lmap (map located) (expand syntax)
  grammar <- lmap (map located) (number core)
  -- Pager: merged where merging cannot invent a conflict, kept apart where it
  -- could. Not a choice a caller gets to make; canonical and LALR exist to be
  -- measured against, not to generate from.
  automaton <- lmap (map located) (build Pager grammar (analyse grammar))
  let
    table = tabulate grammar automaton
  written <- lmap (Array.singleton <<< unlocated)
    ( generate
        { moduleName: options.moduleName
        , core
        , grammar
        , automaton
        , table
        }
    )
  pure
    { source: written.source
    , mappings: written.mappings
    , conflicts:
        if Array.null (unresolved table) then []
        else report grammar automaton table
    }
  where
  unlocated message = { message, span: Nothing }
