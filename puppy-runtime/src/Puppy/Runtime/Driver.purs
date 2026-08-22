-- | The table-driven parser every generated module calls.
-- |
-- | A generated module builds a `Table` out of its emitted arrays and hands it
-- | to one of the runners below. Nothing here knows anything about a particular
-- | grammar, and nothing here depends on the generator, so a project that
-- | merely *uses* a generated parser only needs this package.
-- |
-- | `Puppy.Runtime` is the front door and re-exports all of this. Import that
-- | one; this module is where the parser happens to live.
-- |
-- | There is one LR loop here, and it is a machine that stops. `start` returns
-- | it already waiting for a token, `resume` gives it one and gets back either
-- | the next wait or an answer, and everything else in this module is an
-- | adapter over those two: `parse` feeds it from an array, `parseM` from an
-- | action in whatever monad the caller lexes in.
-- |
-- | Stopping between tokens is not a contrivance. An LR parser consumes the
-- | lookahead only when it shifts -- a run of reductions all inspect the same
-- | one -- so "waiting for a token" is a state the automaton already passes
-- | through, and the machine here simply hands it back instead of reaching for
-- | the next element of an array. Nothing is buffered and nothing is read
-- | ahead: exactly one token is outstanding at a time.
module Puppy.Runtime.Driver
  ( Action(..)
  , ProductionInfo
  , Table
  , ParseError
  , Step(..)
  , Resume
  , start
  , resume
  , unexpectedEnd
  , parse
  , parseM
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.Rec.Class as Rec
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))

-- | One entry of the LR action table, indexed by (state, terminal).
data Action
  = Shift Int
  -- ^ Consume the lookahead and move to the given state.
  | Reduce Int
  -- ^ Apply the production with the given index.
  | Accept
  | Error

derive instance Eq Action

instance Show Action where
  show = case _ of
    Shift s -> "Shift " <> show s
    Reduce p -> "Reduce " <> show p
    Accept -> "Accept"
    Error -> "Error"

-- | What the driver needs to know about a production in order to reduce by it.
type ProductionInfo =
  { lhs :: Int
  , arity :: Int
  , name :: String
  }

-- | Everything a generated module supplies. `val` is the type the generated
-- | code boxes every semantic value into; the driver only ever moves those
-- | values around, so it never needs to look inside one.
type Table tok val =
  { action :: Int -> Int -> Action
  , goto :: Int -> Int -> Int
  , production :: Int -> ProductionInfo
  , semanticAction :: Int -> Array val -> val
  , terminalIndex :: tok -> Int
  , terminalValue :: tok -> val
  , terminalName :: Int -> String
  , terminalCount :: Int
  , startState :: Int
  }

type ParseError tok =
  { position :: Int
  , found :: Maybe tok
  , state :: Int
  , expected :: Array String
  }

-- | Where the parser has got to.
-- |
-- | The stacks grow at the head, so a push is a cons and a pop of `k` is a drop
-- | of `k`; both cost the same no matter how deep the stack is. Holding them as
-- | `Array` would copy the entire stack on every shift and every reduce, which
-- | makes parsing quadratic for any grammar whose stack depth tracks the input
-- | length -- right recursion being the obvious case.
-- |
-- | Invariant: `length states == length values + 1`. The bottom state has no
-- | semantic value beneath it, which is why the two stacks are kept apart
-- | instead of being one list of pairs.
type Machine tok val =
  { table :: Table tok val
  , states :: List Int
  , values :: List val
  , position :: Int
  }

-- | A parser stopped for want of a token.
-- |
-- | Opaque on purpose. What is inside is the two stacks and the invariant
-- | between them, and a caller who could take it apart could put it back
-- | together wrong; `resume` and `unexpectedEnd` are the whole of what may be
-- | done with one.
newtype Resume tok val = Resume (Machine tok val)

-- | What the parser did with the token it was given.
data Step tok val
  = Await (Resume tok val)
  -- ^ It shifted, and now wants the next one.
  | Done val
  | Failed (ParseError tok)

-- | A parser that has read nothing yet, waiting for the first token.
start :: forall tok val. Table tok val -> Step tok val
start table = Await
  ( Resume
      { table
      , states: List.singleton table.startState
      , values: Nil
      , position: 0
      }
  )

-- | Give the parser the token it asked for.
-- |
-- | This runs until the parser needs another one, which means every reduction
-- | the token calls for happens here: a lookahead can finish any number of
-- | productions before it is finally shifted, and none of that asks the caller
-- | for anything.
-- |
-- | The two stacks are carried as arguments rather than as the record they came
-- | out of. Only a shift changes anything else -- the table never changes, and
-- | the position changes exactly once, at the shift that ends the run -- so
-- | threading the whole `Machine` through the loop would allocate one on every
-- | reduction to hold two fields that did not move. A grammar reducing its way
-- | through a long input does that a great many times.
-- |
-- | End of input is a token like any other as far as this is concerned. The
-- | table decides what it means, which is how the driver stays ignorant of
-- | which terminal a particular grammar ends with.
resume :: forall tok val. Resume tok val -> tok -> Step tok val
resume (Resume machine) tok = go machine.states machine.values
  where
  table = machine.table

  go states values =
    case states of
      Nil -> Failed (errorAt machine 0 (Just tok))
      Cons state _ ->
        case table.action state (table.terminalIndex tok) of
          Shift next ->
            Await
              ( Resume
                  { table
                  , states: Cons next states
                  , values: Cons (table.terminalValue tok) values
                  , position: machine.position + 1
                  }
              )

          Reduce p ->
            let
              info = table.production p
              -- The stack keeps the rightmost symbol at the head, so the
              -- arguments have to be flipped back into production order.
              args =
                Array.reverse
                  (Array.fromFoldable (List.take info.arity values))
              values' = List.drop info.arity values
              states' = List.drop info.arity states
            in
              case states' of
                Nil -> Failed (errorAt machine state (Just tok))
                Cons under _ ->
                  go
                    (Cons (table.goto under info.lhs) states')
                    (Cons (table.semanticAction p args) values')

          Accept ->
            case values of
              Nil -> Failed (errorAt machine state (Just tok))
              Cons v _ -> Done v

          Error -> Failed (errorAt machine state (Just tok))

-- | The error a parser is owed when its supply of tokens runs out.
-- |
-- | This is not how a parse is finished. A grammar ends on a terminal of its
-- | own -- `Nothing`, for a generated parser -- and a caller who has one to
-- | give should call `resume` with it. This is for a caller who has not: the
-- | array ran off the end, the file was truncated, the socket closed.
-- |
-- | It is a function rather than a field of `Await` because `expected` costs a
-- | pass over every terminal in the grammar. Computing it each time the parser
-- | stopped for a token would make parsing cost the length of the input times
-- | the size of the alphabet, to answer a question almost no caller asks.
unexpectedEnd :: forall tok val. Resume tok val -> ParseError tok
unexpectedEnd (Resume m) = errorAt m (topOf m.states) Nothing
  where
  topOf = case _ of
    Nil -> 0
    Cons state _ -> state

errorAt :: forall tok val. Machine tok val -> Int -> Maybe tok -> ParseError tok
errorAt m state found =
  { position: m.position
  , found
  , state
  , expected: expectedAt m.table state
  }

-- | The terminals that would not immediately fail in this state. This is the
-- | cheap approximation every LR generator reports; it can name a token that a
-- | later reduce would reject, but it never omits a genuinely viable one.
expectedAt :: forall tok val. Table tok val -> Int -> Array String
expectedAt table state =
  map table.terminalName
    $ Array.filter (\t -> table.action state t /= Error)
    $ Array.range 0 (table.terminalCount - 1)

-- | Run the LR automaton over an array of tokens.
-- |
-- | The array must be terminated by whatever token the grammar declared as its
-- | end marker; this does not append one. Running off the end is therefore
-- | reported as an error rather than treated as end of input -- see
-- | `unexpectedEnd`.
parse :: forall tok val. Table tok val -> Array tok -> Either (ParseError tok) val
parse table input = go 0 (start table)
  where
  go index step =
    case step of
      Done value -> Right value
      Failed err -> Left err
      Await waiting ->
        case Array.index input index of
          Nothing -> Left (unexpectedEnd waiting)
          Just tok -> go (index + 1) (resume waiting tok)

-- | Run the LR automaton over tokens pulled one at a time.
-- |
-- | The action is run exactly when the parser wants a token, and not once
-- | after it has an answer: a parse that accepts or fails on the token it is
-- | holding does not ask for another. There is no need for the source to know
-- | when to stop, only what to say at the end, which for a generated parser is
-- | `Nothing`.
-- |
-- | Whatever else `m` can do, the source may do while lexing. Failing is the
-- | useful case -- a lexer that meets a character it cannot read wants to say
-- | so, and in `ExceptT` or `Aff` saying so abandons the parse without the
-- | driver needing an opinion about it.
-- |
-- | `MonadRec` rather than `Monad` because the loop is as long as the input.
parseM
  :: forall m tok val
   . MonadRec m
  => Table tok val
  -> m tok
  -> m (Either (ParseError tok) val)
parseM table next = tailRecM go (start table)
  where
  go = case _ of
    Done value -> pure (Rec.Done (Right value))
    Failed err -> pure (Rec.Done (Left err))
    Await waiting -> map (Rec.Loop <<< resume waiting) next
