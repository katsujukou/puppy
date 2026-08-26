-- | A source that turns one stream of tokens into another.
-- |
-- | The pulling entry point a generated module exposes asks for one token and
-- | is given one. Plenty of useful passes between a lexer and a parser are not
-- | one-for-one: dropping comments produces nothing for some tokens, and an
-- | offside rule produces several for others -- a token that ends three blocks
-- | at once has three closing tokens to hand over before it is itself handed
-- | over. Either way something has to hold what has been produced and not yet
-- | asked for, and that something is state.
-- |
-- | Hence `StateT` rather than a source that hides its own state. There is
-- | nowhere in an arbitrary `m` to keep a queue between two calls: `Effect`
-- | would do it with a `Ref`, but `Either` and `State` have no such place, and
-- | a combinator that only worked for some monads would be the wrong shape for
-- | an entry point that asks for `MonadRec` and nothing else.
-- |
-- | This is not where an offside rule lives, and it does not reach every kind
-- | of one. What can be written as a pass over the token stream is a layout
-- | rule that is settled by the tokens themselves: PureScript's is, which is
-- | why its compiler applies layout before parsing rather than during it.
-- | Haskell's is not, because its rule closes a block when the parser turns
-- | out not to be able to accept a token, and nothing here can ask a parser
-- | that. For the rules this does reach, what is here is the plumbing they
-- | need, so that writing one is writing the rule and not the queue.
module Puppy.Runtime.Source
  ( SourceState
  , initial
  , transduce
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (StateT)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))

-- | What a transducing source carries between one token and the next: the
-- | pass's own state, the tokens it has produced that the parser has not asked
-- | for yet, and whether the underlying source has run out.
-- |
-- | Opaque, like the parser's own `Resume` and for the same reason. Two of
-- | those three are promises rather than data -- that the queue is handed over
-- | in the order it was produced, and that a source which has ended is not
-- | asked again -- and both are promises a caller holding the record could
-- | quietly break. Keeping the shape private also means the queue can stop
-- | being a `List` without that being a change anyone has to hear about.
newtype SourceState s tok = SourceState
  { transducer :: s
  , pending :: List tok
  , ended :: Boolean
  }

-- | A source that has produced nothing yet, wrapping the pass's initial state.
initial :: forall s tok. s -> SourceState s tok
initial transducer = SourceState { transducer, pending: Nil, ended: false }

-- | Put a pass between a source of tokens and the parser.
-- |
-- | The step function is given the pass's state and one token from the source,
-- | and answers with the state to carry on with and however many tokens that
-- | one produced -- none, one, or several. What comes back is a source in the
-- | shape a generated entry point wants, so:
-- |
-- | ```purescript
-- | State.evalStateT
-- |   (Grammar.moduleFrom (transduce insertLayout nextToken))
-- |   (initial emptyStack)
-- | ```
-- |
-- | End of input is passed on rather than swallowed: when the source answers
-- | `Nothing` the step function is called with `Nothing`, exactly once, and
-- | may produce a last batch of tokens. A pass with blocks still open closes
-- | them there. Only when that batch is exhausted does this answer `Nothing`
-- | in turn, which is the end of input the parser is waiting for.
-- |
-- | The loop is `tailRecM` because a step is allowed to produce nothing at
-- | all. A pass that drops what it is given -- comments, whitespace -- may do
-- | that as many times in a row as the input has comments in a row, and that
-- | is a loop as long as the input rather than a bounded one.
transduce
  :: forall m s raw tok
   . MonadRec m
  => (s -> Maybe raw -> Tuple s (Array tok))
  -> m (Maybe raw)
  -> StateT (SourceState s tok) m (Maybe tok)
transduce step next = tailRecM go unit
  where
  go _ = do
    SourceState source <- State.get
    case List.uncons source.pending of
      Just queued -> do
        State.put (SourceState source { pending = queued.tail })
        pure (Done (Just queued.head))
      Nothing
        | source.ended -> pure (Done Nothing)
        | otherwise -> do
            raw <- lift next
            case step source.transducer raw of
              Tuple carried produced -> do
                State.put
                  ( SourceState
                      { transducer: carried
                      , pending: List.fromFoldable produced
                      , ended: isNothing raw
                      }
                  )
                pure (Loop unit)
