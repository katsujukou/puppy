-- | The table-driven parser every generated module calls.
-- |
-- | A generated module builds a `Table` out of its emitted arrays and hands it
-- | to `parse`. Nothing here knows anything about a particular grammar, and
-- | nothing here depends on the generator, so a project that merely *uses* a
-- | generated parser only needs this package.
-- |
-- | `Puppy.Runtime` is the front door and re-exports all of this. Import that
-- | one; this module is where the parser happens to live.
module Puppy.Runtime.Driver
  ( Action(..)
  , ProductionInfo
  , Table
  , ParseError
  , parse
  ) where

import Prelude

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

-- | Run the LR automaton over a token stream.
-- |
-- | The stream must be terminated by whatever token the grammar declared as its
-- | end marker; the driver does not append one. Running off the end of the
-- | array is therefore reported as an error rather than treated as EOF.
parse :: forall tok val. Table tok val -> Array tok -> Either (ParseError tok) val
parse table input = go (List.singleton table.startState) Nil 0
  where
  -- The stacks grow at the head, so a push is a cons and a pop of `k` is a drop
  -- of `k`; both cost the same no matter how deep the stack is. Holding them as
  -- `Array` would copy the entire stack on every shift and every reduce, which
  -- makes parsing quadratic for any grammar whose stack depth tracks the input
  -- length -- right recursion being the obvious case.
  --
  -- Invariant: `length states == length values + 1`. The bottom state has no
  -- semantic value beneath it, which is why the two stacks are kept apart
  -- instead of being one list of pairs.
  go :: List Int -> List val -> Int -> Either (ParseError tok) val
  go states values pos =
    case states of
      Nil -> Left (errorAt 0 pos)
      Cons state _ ->
        case Array.index input pos of
          Nothing -> Left (errorAt state pos)
          Just tok ->
            case table.action state (table.terminalIndex tok) of
              Shift next ->
                go
                  (Cons next states)
                  (Cons (table.terminalValue tok) values)
                  (pos + 1)

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
                    Nil -> Left (errorAt state pos)
                    Cons under _ ->
                      go
                        (Cons (table.goto under info.lhs) states')
                        (Cons (table.semanticAction p args) values')
                        pos

              Accept ->
                case values of
                  Nil -> Left (errorAt state pos)
                  Cons v _ -> Right v

              Error -> Left (errorAt state pos)

  errorAt :: Int -> Int -> ParseError tok
  errorAt state pos =
    { position: pos
    , found: Array.index input pos
    , state
    , expected: expectedAt state
    }

  -- The terminals that would not immediately fail in this state. This is the
  -- cheap approximation every LR generator reports; it can name a token that a
  -- later reduce would reject, but it never omits a genuinely viable one.
  expectedAt :: Int -> Array String
  expectedAt state =
    map table.terminalName
      $ Array.filter (\t -> table.action state t /= Error)
      $ Array.range 0 (table.terminalCount - 1)
