-- | An output buffer that knows where it is.
-- |
-- | Generated code contains stretches of the grammar author's own PureScript,
-- | and when the compiler complains about one of them the complaint has to be
-- | traceable back to the `.puppy` file. That means knowing the line and column
-- | each stretch was written at, which means counting as the output is built:
-- | formatting the result afterwards would move everything and leave the
-- | positions pointing at nothing.
module Puppy.Codegen.Emit
  ( Position
  , SourceMapping
  , Emitter
  , empty
  , write
  , writeCode
  , writeAll
  , render
  , mappings
  , position
  , trimCode
  ) where

import Prelude

import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Data.String.Common (joinWith, split)
import Data.String.Pattern (Pattern(..))
import Puppy.Syntax (Code, Span)

type Position = { line :: Int, column :: Int }

-- | Where a stretch of the author's own code ended up, and where it came from.
-- |
-- | The generated side is a range rather than a point, because a compiler
-- | complaining about the third line of a semantic action needs to be told
-- | whether that line is still inside the fragment. `indent` is what was added
-- | to every line after the first to keep the layout valid, and is what a
-- | column has to be shifted back by to be read against the original.
type SourceMapping =
  { generated :: { start :: Position, end :: Position }
  , source :: Span
  , indent :: Int
  }

newtype Emitter = Emitter
  { chunks :: List String
  , at :: Position
  , found :: List SourceMapping
  }

empty :: Emitter
empty = Emitter
  { chunks: Nil
  , at: { line: 1, column: 1 }
  , found: Nil
  }

write :: String -> Emitter -> Emitter
write text (Emitter e) = Emitter e
  { chunks = text : e.chunks
  , at = advance text e.at
  }

writeAll :: Array String -> Emitter -> Emitter
writeAll texts e = Array.foldl (flip write) e texts

-- | Write a fragment the grammar author wrote, at the indentation it now sits
-- | at, remembering where it landed.
-- |
-- | Lines after the first are moved along by `indent`, which keeps whatever
-- | they had relative to each other -- all the layout rule cares about -- while
-- | putting the whole fragment under the code it is nested in.
writeCode :: Int -> Code -> Emitter -> Emitter
writeCode indent code (Emitter e) =
  Emitter e
    { chunks = shifted : e.chunks
    , at = after
    , found =
        { generated: { start: e.at, end: after }
        , source: trimmed.span
        , indent
        } : e.found
    }
  where
  trimmed = trimCode code

  shifted = joinWith ("\n" <> padding) (split (Pattern "\n") trimmed.text)

  padding = joinWith "" (Array.replicate indent " ")

  after = advance shifted e.at

render :: Emitter -> String
render (Emitter e) = joinWith "" (Array.fromFoldable (List.reverse e.chunks))

mappings :: Emitter -> Array SourceMapping
mappings (Emitter e) = Array.fromFoldable (List.reverse e.found)

position :: Emitter -> Position
position (Emitter e) = e.at

-- | Columns are counted in code units, which is what the PureScript compiler
-- | reports.
advance :: String -> Position -> Position
advance text at = case Array.unsnoc (split (Pattern "\n") text) of
  Nothing -> at
  Just { init, last }
    | Array.null init -> at { column = at.column + SCU.length last }
    | otherwise ->
        { line: at.line + Array.length init
        , column: 1 + SCU.length last
        }

-- | Drop the whitespace a `{ ... }` leaves around a fragment, and move the span
-- | to match.
-- |
-- | Without this the surrounding spaces come through into the generated code,
-- | which reads badly in a type annotation and worse in a constructor argument.
-- | Moving the span is the whole reason this is not just a string trim: the
-- | position has to keep pointing at the character it names.
trimCode :: Code -> Code
trimCode code =
  { text: SCU.fromCharArray kept
  , span: { start, end: Array.foldl step start kept }
  }
  where
  -- The end is walked forwards from the new start rather than backwards from
  -- the old end: stepping back over a newline would need the length of the
  -- line before it, which is not to hand.
  start = Array.foldl step code.span.start dropped

  chars = SCU.toCharArray code.text

  dropped = Array.takeWhile isSpace chars

  kept = Array.reverse
    (Array.dropWhile isSpace (Array.reverse (Array.drop (Array.length dropped) chars)))

  isSpace c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

  step at c =
    if c == '\n' then { offset: at.offset + 1, line: at.line + 1, column: 1 }
    else { offset: at.offset + 1, line: at.line, column: at.column + 1 }
