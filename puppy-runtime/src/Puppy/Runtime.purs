-- | The runtime support library that generated parsers depend on.
-- |
-- | This is the whole of it. A generated module imports this and nothing else
-- | of Puppy's, and so should anyone writing against one: `ParseError` is the
-- | only name here that a person is likely to need, and it is the type the
-- | entry points return.
-- |
-- | What is behind it is split in two, for one reason.
-- | [`Puppy.Runtime.Value`](Puppy.Runtime.Value.html) holds the box a parser
-- | keeps its semantic values in, and with it the only two coercions in the
-- | system; keeping them in a module of their own is what makes "exactly two,
-- | and here they are" a thing you can check rather than a thing to believe.
-- | [`Puppy.Runtime.Driver`](Puppy.Runtime.Driver.html) is the parser itself,
-- | and has no unsoundness in it at all.
module Puppy.Runtime
  ( module Puppy.Runtime.Driver
  , module Puppy.Runtime.Value
  ) where

import Puppy.Runtime.Driver (Action(..), ParseError, ProductionInfo, Table, parse)
import Puppy.Runtime.Value (Value, box, internalError, slot, unbox)
