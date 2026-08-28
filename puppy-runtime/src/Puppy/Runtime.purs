-- | The runtime support library that generated parsers depend on.
-- |
-- | This is the whole of it. A generated module imports this and nothing else
-- | of Puppy's, and so should anyone writing against one: `ParseError` is the
-- | only name here that a person is likely to need, and it is the type the
-- | entry points return.
-- |
-- | The parser itself is also here, and it is worth knowing which shape of it
-- | you are looking at. `parse` and `parseM` are the two runners a generated
-- | module calls; `start`, `resume` and `unexpectedEnd` are the machine
-- | underneath them, for a caller whose tokens arrive by some route neither
-- | runner covers -- pushed from a callback, say, rather than pulled.
-- |
-- | What is behind it is split in two, for one reason.
-- | [`Puppy.Runtime.Value`](Puppy.Runtime.Value.html) holds the box a parser
-- | keeps its semantic values in, and with it the only two coercions in the
-- | system; keeping them in a module of their own is what makes "exactly two,
-- | and here they are" a thing you can check rather than a thing to believe.
-- | [`Puppy.Runtime.Driver`](Puppy.Runtime.Driver.html) is the parser itself,
-- | and has no unsoundness in it at all.
-- |
-- | One module of the package is deliberately not re-exported here.
-- | [`Puppy.Runtime.Source`](Puppy.Runtime.Source.html) is for putting a pass
-- | between a lexer and a parser -- dropping comments, applying an offside
-- | rule, anything that does not hand over exactly one token for each one it
-- | is given. Nothing generated calls it and most callers never need it, so it
-- | is imported qualified when it is wanted rather than added to the names
-- | every generated module already brings along.
module Puppy.Runtime
  ( module Puppy.Runtime.Driver
  , module Puppy.Runtime.Value
  ) where

import Puppy.Runtime.Driver (Action, ParseError, ProductionInfo, Recovery, RecoveryResult(..), Resume, Step(..), Table, acceptAction, canConsume, errorAction, parse, parseM, parseMRecovering, parseRecoveringAt, reduce, resume, shift, start, unexpectedEnd)
import Puppy.Runtime.Value (Value, box, internalError, slot, unbox)
