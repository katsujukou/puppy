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
  ( Action
  , errorAction
  , acceptAction
  , shift
  , reduce
  , ProductionInfo
  , Recovery
  , Table
  , ParseError
  , Step(..)
  , Resume
  , start
  , resume
  , canConsume
  , RecoveryResult(..)
  , parseMRecovering
  , parseRecoveringAt
  , unexpectedEnd
  , parse
  , parseM
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.Rec.Class as Rec
import Control.Monad.State as State
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))

-- | One entry of the LR action table, indexed by (state, terminal).
-- | One cell of the action table, as an `Int`.
-- |
-- | ```
-- |   0        the token is an error here
-- |   1        accept
-- |   n >= 2   shift, and go to state n - 2
-- |   n < 0    reduce by production -n - 1
-- | ```
-- |
-- | A number rather than a constructor, because of how many of these there
-- | are. A table with a cell for every (state, terminal) pair is read by index
-- | instead of searched, which is the whole point of an LR parser being a
-- | table -- and a cell for every pair is affordable as a number and is not
-- | affordable as an object. For a real grammar the difference is a table half
-- | the size that answers in one read instead of a scan.
-- |
-- | Build them with the four below rather than by arithmetic.
type Action = Int

-- | The token cannot appear here.
errorAction :: Action
errorAction = 0

-- | The input is complete.
acceptAction :: Action
acceptAction = 1

-- | Consume the lookahead and move to the given state.
shift :: Int -> Action
shift state = state + 2

-- | Apply the production with the given index.
reduce :: Int -> Action
reduce production = negate production - 1

-- | What a parse needs in order to carry on past an error.
-- |
-- | One thing rather than two, because a terminal number with no way to make a
-- | value for it, or a value with no terminal to attach it to, is not half of
-- | anything. `terminal` is one past `endTerminal`: recovery is the only thing
-- | that names it, `terminalIndex` never returns it, and nothing lists it
-- | among the tokens that were expected.
type Recovery tok val =
  { terminal :: Int
  , value :: ParseError tok -> val
  }

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
  , endTerminal :: Int
  -- ^ The number of the end-of-input terminal, and so the last one a token can
  -- be. The user's terminals are `0 .. endTerminal - 1`.
  , recovery :: Maybe (Recovery tok val)
  -- ^ Where a grammar said a parse may carry on past an error, if it said so
  -- anywhere. `Nothing` is a grammar with no recovery rules in it, and the
  -- only thing a parse can do with an error is stop.
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

-- | A parser that has read nothing, as the machine rather than as a step.
-- |
-- | Only for the runners here, which need the thing itself rather than the
-- | answer that it is waiting: taking it back out of a `Step` would mean a
-- | case with an arm that cannot happen, and an arm that cannot happen is a
-- | crash somebody has to be told to ignore.
starting :: forall tok val. Table tok val -> Resume tok val
starting table = Resume
  { table
  , states: List.singleton table.startState
  , values: Nil
  , position: 0
  }

-- | A parser that has read nothing yet, waiting for the first token.
start :: forall tok val. Table tok val -> Step tok val
start = Await <<< starting

-- | What the LR loop did with a token.
-- |
-- | The one difference from `Step`, and the reason there are two of these, is
-- | the last case. A parse that stops has no more use for its stacks and
-- | `Step` does not carry them; a parse that means to carry on needs exactly
-- | those stacks -- the ones left after every reduction the token called for,
-- | which is where a place to pick up again has to be looked for.
data Outcome tok val
  = Advanced (Resume tok val)
  | Accepted val
  | Rejected
      { error :: ParseError tok
      , states :: List Int
      , values :: List val
      }

-- | The LR loop. One of them, with two ways of reading what it answered.
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
offer :: forall tok val. Resume tok val -> tok -> Outcome tok val
offer (Resume machine) tok = go machine.states machine.values
  where
  table = machine.table

  -- Which terminal the token is, worked out once.
  --
  -- The loop below runs a reduction and comes round again on the same
  -- lookahead, and a grammar with levels in it -- an expression grammar, say,
  -- where an atom climbs half a dozen rules before anything shifts -- comes
  -- round many times per token. Asking again each time would be asking a
  -- question that cannot have changed its answer, and where the token type is
  -- the author's the asking is a walk down every pattern they wrote.
  terminal = table.terminalIndex tok

  stuck state states values =
    Rejected { error: errorAt machine state (Just tok), states, values }

  go states values =
    case states of
      Nil -> stuck 0 states values
      Cons state _ ->
        let
          code = table.action state terminal
        in
          if code >= 2 then
            Advanced
              ( Resume
                  { table
                  , states: Cons (code - 2) states
                  , values: Cons (table.terminalValue tok) values
                  , position: machine.position + 1
                  }
              )
          else if code < 0 then
            let
              production = negate code - 1

              info = table.production production
            in
              -- The stack keeps the rightmost symbol at the head, so the
              -- arguments have to be turned back into production order.
              --
              -- The two short cases are worth writing out. Most of the
              -- productions of a real grammar take one symbol or none -- the
              -- chains of `a -> b` that give an expression grammar its
              -- precedence levels are all arity one -- and for those the
              -- argument is already at the head of the stack.
              case fastest info.arity states values of
                Nothing -> stuck state states values
                Just taken -> case taken.states of
                  Nil -> stuck state states values
                  Cons under _ ->
                    -- `go` calls itself here and nowhere else. Reaching it
                    -- through a helper would make the two mutually recursive,
                    -- which is not a tail call the compiler turns into a loop,
                    -- and a grammar that reduces its way through a long input
                    -- would run out of stack for it.
                    go
                      (Cons (table.goto under info.lhs) taken.states)
                      (Cons (table.semanticAction production taken.args) taken.values)
          else if code == 1 then
            case values of
              Nil -> stuck state states values
              Cons v _ -> Accepted v
          else stuck state states values

-- | Give the parser the token it asked for.
-- |
-- | `offer` above is the loop; this is the answer a caller who is not going to
-- | carry on past an error wants to hear, which is the same one with the
-- | stacks left out of the failure.
resume :: forall tok val. Resume tok val -> tok -> Step tok val
resume waiting tok = case offer waiting tok of
  Advanced next -> Await next
  Accepted value -> Done value
  Rejected stopped -> Failed stopped.error

-- | Take one production's worth off both stacks, in one walk down each.
-- |
-- | The arguments come off backwards -- the stack keeps the rightmost symbol
-- | at the head -- so consing each one onto a list as it comes off puts them
-- | back in the order the production was written, and that list becomes the
-- | array in one go. Doing it this way walks each stack once rather than three
-- | times, and builds one array rather than two.
-- |
-- | `Nothing` when a stack ran out, which is a broken table rather than
-- | anything an input can do.
-- | Calls `popped`, after answering the two short arities without walking
-- | anything: nothing is taken off for a production of none, and a production
-- | of one is handed what is already at the head of the stack.
fastest
  :: forall val
   . Int
  -> List Int
  -> List val
  -> Maybe { states :: List Int, values :: List val, args :: Array val }
fastest arity states values = case arity, states, values of
  0, _, _ -> Just { states, values, args: [] }
  1, Cons _ states', Cons value values' ->
    Just { states: states', values: values', args: [ value ] }
  _, _, _ -> popped arity states values Nil

popped
  :: forall val
   . Int
  -> List Int
  -> List val
  -> List val
  -> Maybe { states :: List Int, values :: List val, args :: Array val }
popped left states values args
  | left <= 0 = Just { states, values, args: Array.fromFoldable args }
  | otherwise = case states, values of
      Cons _ states', Cons value values' ->
        popped (left - 1) states' values' (Cons value args)
      _, _ -> Nothing

-- | Whether the parser could take this token, without taking it.
-- |
-- | A question about the tables and the state stack, and about nothing else. A
-- | reduction on the way to the answer is followed on the states alone: no
-- | semantic value is unboxed, `terminalValue` is never asked what the token
-- | carries and `semanticAction` is never run. Answering has no cost beyond
-- | the reductions it walks and leaves nothing behind, so the same `Resume`
-- | can be asked about as many tokens as a caller likes.
-- |
-- | `true` includes accepting. A token that ends the parse is one the parser
-- | can take, which is what a caller looking for somewhere to carry on from
-- | wants to hear -- the alternative is a caller that walks past the end of a
-- | complete parse looking for a better place.
-- |
-- | `expected` is not a substitute for this. That is the cheap approximation
-- | every LR parser reports: it asks the top state alone and so can name a
-- | token that the reduction underneath would have rejected. This follows the
-- | reductions, and so answers exactly.
canConsume :: forall tok val. Resume tok val -> tok -> Boolean
canConsume (Resume machine) tok = go machine.states
  where
  table = machine.table

  terminal = table.terminalIndex tok

  go states = case states of
    Nil -> false
    Cons state _ ->
      let
        code = table.action state terminal
      in
        -- Shift and accept are both the parser taking the token. Only a
        -- reduction leaves the question open, and then it is the same question
        -- one state further down.
        if code >= acceptAction then true
        else if code == errorAction then false
        else
          let
            info = table.production (negate code - 1)

            states' = List.drop info.arity states
          in
            case states' of
              Nil -> false
              Cons under _ -> go (Cons (table.goto under info.lhs) states')

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
    $ Array.filter (\t -> table.action state t /= errorAction)
    $ Array.range 0 table.endTerminal

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

--------------------------------------------------------------------------------
-- Carrying on past an error
--------------------------------------------------------------------------------

-- | What came of a parse that was allowed to carry on past an error.
-- |
-- | Three answers rather than two, because "it parsed" and "it parsed, and
-- | here is what was wrong with it" are different things to be told and a
-- | caller acts differently on each. The errors are in the order they were
-- | found.
data RecoveryResult tok val
  = ParseSucceeded val
  | ParseRecovered (Array (ParseError tok)) val
  | ParseFailed (Array (ParseError tok))

derive instance (Eq tok, Eq val) => Eq (RecoveryResult tok val)

instance (Show tok, Show val) => Show (RecoveryResult tok val) where
  show = case _ of
    ParseSucceeded value -> "(ParseSucceeded " <> show value <> ")"
    ParseRecovered errors value ->
      "(ParseRecovered " <> show errors <> " " <> show value <> ")"
    ParseFailed errors -> "(ParseFailed " <> show errors <> ")"

-- | Where the recovery runner is in its work: reading tokens the ordinary way,
-- | or throwing away the ones it cannot use yet.
-- |
-- | Carried by the loop rather than by a recursive call, so that a run of
-- | thrown-away tokens is a loop and not a stack.
data Mode tok val
  = Parsing (Resume tok val)
  | Discarding (Resume tok val)

-- | A parser standing where an `ERROR` has just been shifted, if the grammar
-- | said there was anywhere to shift one.
-- |
-- | Where to shift it is found by walking down the stacks the failure left --
-- | which are the ones after every reduction the token called for, not the
-- | ones the parser was holding when the token arrived. A state accepts an
-- | `ERROR` when its `ERROR` cell is a shift and not a reduce: reducing on it
-- | would be treating a token nobody read as a lookahead, and the whole point
-- | of it is that it is not one.
-- |
-- | `position` does not move. An `ERROR` is not a token read from the input,
-- | and the next thing reported has to be able to say where it really was.
recoverAt
  :: forall tok val
   . Table tok val
  -> Int
  -> ParseError tok
  -> List Int
  -> List val
  -> Maybe (Resume tok val)
recoverAt table position error states values = do
  handler <- table.recovery
  go handler states values
  where
  go handler left leftValues = case left of
    Nil -> Nothing
    Cons state under ->
      let
        code = table.action state handler.terminal
      in
        if code >= 2 then Just
          ( Resume
              { table
              , states: Cons (code - 2) left
              , values: Cons (handler.value error) leftValues
              , position
              }
          )
        else case under, leftValues of
          Cons _ _, Cons _ deeper -> go handler under deeper
          _, _ -> Nothing

-- | Parse from tokens pulled one at a time, carrying on past what it can.
-- |
-- | The same LR loop as `parseM`, read one case further: where that one stops
-- | at a failure, this one looks for somewhere the grammar said a parse may be
-- | picked up again, puts an `ERROR` there, and then throws tokens away until
-- | it finds one the parser can take.
-- |
-- | `canConsume` is what "can take" means, and it has to be that rather than
-- | the cheaper question. A token the top state would reduce on and the state
-- | underneath would reject is not one to carry on from; taking it would fail
-- | again immediately and the parser would throw the next one away for the
-- | same reason, all the way to the end.
-- |
-- | End of input is where this stops. A parser that cannot take it has nowhere
-- | left to go, and asking the source for another token after it has said
-- | there are none is asking a question that has already been answered.
parseMRecovering
  :: forall m tok val
   . MonadRec m
  => Table tok val
  -> m tok
  -> m (RecoveryResult tok val)
parseMRecovering table next = do
  first <- next
  tailRecM step { mode: Parsing (starting table), tok: first, errors: Nil }
  where
  -- One step asks the source for at most one token and comes straight back to
  -- `tailRecM`, which is what keeps a long run of thrown-away tokens a loop.
  -- Throwing them away inside the bind, as one recursive call after another,
  -- is not a tail call the compiler can see, and a file with a great many
  -- unreadable tokens in a row is exactly the file this is for.
  step state = case state.mode of
    Parsing waiting -> case offer waiting state.tok of
      Accepted value -> pure (Rec.Done (answered state.errors value))
      Advanced onwards -> do
        tok <- next
        pure (Rec.Loop { mode: Parsing onwards, tok, errors: state.errors })
      Rejected stopped ->
        let
          errors = Cons stopped.error state.errors
        in
          case
            recoverAt table (positionOf waiting) stopped.error stopped.states
              stopped.values
            of
            Nothing -> pure (Rec.Done (ParseFailed (collected errors)))
            Just resumed -> pure
              (Rec.Loop { mode: Discarding resumed, tok: state.tok, errors })

    -- The token that failed is tried first: an `ERROR` on the stack may be
    -- exactly what it was waiting for, and throwing it away unread would lose
    -- a token that has just become perfectly good.
    Discarding waiting
      | canConsume waiting state.tok ->
          pure (Rec.Loop { mode: Parsing waiting, tok: state.tok, errors: state.errors })
      -- End of input is answered once. Asking again after being told there are
      -- no more tokens is asking a question that has already been answered.
      | table.terminalIndex state.tok == table.endTerminal ->
          pure (Rec.Done (ParseFailed (collected state.errors)))
      | otherwise -> do
          tok <- next
          pure
            ( Rec.Loop
                { mode: Discarding (moved waiting), tok, errors: state.errors }
            )

  -- A token that was read and thrown away still moves the count on, or the
  -- next thing reported would say it was somewhere it was not.
  moved (Resume machine) = Resume machine { position = machine.position + 1 }

  positionOf (Resume machine) = machine.position

  -- Errors are collected by consing, so that a file with a great many of them
  -- does not copy the list it has so far for every one it finds.
  collected errors = Array.reverse (Array.fromFoldable errors)

  answered errors value = case collected errors of
    [] -> ParseSucceeded value
    found -> ParseRecovered found value

-- | Parse from something a caller can look up by position, recovering where it
-- | can.
-- |
-- | `at` has to be total, and out past the end of the input it has to keep
-- | answering with the terminal the grammar ends on. A generated module gets
-- | that for nothing: its driver token is a `Maybe`, and looking past the end
-- | of an array is `Nothing`, which is the end of input as far as the tables
-- | are concerned.
-- |
-- | There is no second loop here. Reading by position is a token source like
-- | any other once it is given somewhere to keep the position, so this is
-- | `parseMRecovering` with that somewhere -- and no copy of the input is made
-- | on the way.
parseRecoveringAt
  :: forall tok val
   . Table tok val
  -> (Int -> tok)
  -> RecoveryResult tok val
parseRecoveringAt table at = State.evalState (parseMRecovering table next) 0
  where
  next = do
    index <- State.get
    State.put (index + 1)
    pure (at index)
