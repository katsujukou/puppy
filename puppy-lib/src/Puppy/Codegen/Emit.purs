-- | An output buffer that knows where it is.
-- |
-- | Generated code contains stretches of the grammar author's own PureScript,
-- | and when the compiler complains about one of them the complaint has to be
-- | traceable back to the `.pursy` file. That means knowing the line and column
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
  , writePattern
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
import Puppy.Syntax (Code, Pos, Span, TokenPattern)

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
  , verbatim :: Boolean
  -- ^ Whether the generated text *is* the source text.
  --
  -- False for what Puppy wrote in place of a `$$`. The two sides then have
  -- nothing to say to each other character by character -- `$$` is two
  -- characters and `puppyPayload` is twelve -- so a position anywhere inside
  -- the generated range means the placeholder as a whole.
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
writeCode indent = writeExact indent <<< trimCode

-- | The same, for a fragment already known to be exactly what should come out.
-- |
-- | `writeCode` trims because a `{ ... }` leaves whitespace around what it
-- | holds. A stretch between two holes has no such whitespace to spare: the
-- | space before a `$$` separates it from what came before, and trimming it
-- | would run two words together.
writeExact :: Int -> Code -> Emitter -> Emitter
writeExact indent code (Emitter e) =
  Emitter e
    { chunks = shifted : e.chunks
    , at = after
    , found =
        { generated: { start: e.at, end: after }
        , source: code.span
        , indent
        , verbatim: true
        } : e.found
    }
  where
  shifted = joinWith ("\n" <> padding) (split (Pattern "\n") code.text)

  padding = joinWith "" (Array.replicate indent " ")

  after = advance shifted e.at

-- | Write a token pattern, putting something of the generator's own where each
-- | `$$` was.
-- |
-- | The stretches between the holes are the author's, and each is recorded on
-- | its own. One mapping for the whole pattern would be wrong the moment a hole
-- | is not as wide as what replaced it -- `$$` is two characters and
-- | `puppyPayload` is twelve, so everything after it on that line has moved,
-- | and a mapping that claims otherwise sends a reader ten columns wide of what
-- | the compiler was talking about.
writePattern :: Int -> String -> TokenPattern -> Emitter -> Emitter
writePattern indent replacement pattern emitter =
  verbatim done.at trimmed.span.end done.out
  where
  trimmed = trimCode pattern.code

  base = trimmed.span.start.offset

  -- Trimming only ever removes whitespace and a `$$` is not whitespace, so
  -- this drops nothing in practice. The arithmetic below is indexed from the
  -- trimmed start, and would be wrong rather than empty-handed without it.
  holes = Array.filter inside pattern.holes

  inside h =
    h.start.offset >= base && h.end.offset <= trimmed.span.end.offset

  done = Array.foldl piece { at: trimmed.span.start, out: emitter } holes

  piece acc hole =
    { at: hole.end
    , out: standIn hole (verbatim acc.at hole.start acc.out)
    }

  -- The substitution is recorded too. Without it a compiler complaining about
  -- `puppyPayload` has nothing pointing anywhere: the stretches either side of
  -- a hole cover the author's text and not the hole itself.
  standIn hole (Emitter out) = Emitter out
    { chunks = replacement : out.chunks
    , at = advance replacement out.at
    , found =
        { generated: { start: out.at, end: advance replacement out.at }
        , source: hole
        , indent
        , verbatim: false
        } : out.found
    }

  verbatim :: Pos -> Pos -> Emitter -> Emitter
  verbatim from to out
    | to.offset <= from.offset = out
    | otherwise = writeExact indent
        { text: SCU.take (to.offset - from.offset)
            (SCU.drop (from.offset - base) trimmed.text)
        , span: { start: from, end: to }
        }
        out

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
