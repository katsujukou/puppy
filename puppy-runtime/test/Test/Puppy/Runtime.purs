module Test.Puppy.Runtime where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Rec.Class as Rec
import Control.Monad.State (State)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.String.Common (joinWith)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Partial.Unsafe (unsafeCrashWith)
import Puppy.Runtime (ParseError, Table, acceptAction, errorAction, parse, parseM, reduce, shift)
import Puppy.Runtime.Source (initial, transduce)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- Hand-built tables, so that the driver can be exercised before anything is
-- able to generate one.

--------------------------------------------------------------------------------
-- E -> E + T | T
-- T -> n | ( E )
--
-- The SLR table. Terminals n=0, +=1, (=2, )=3, eof=4; nonterminals E=0, T=1.
-- The semantic value of an expression is the sum of its numbers.
--
-- The parenthesis rule earns its keep: because FOLLOW(T) and FOLLOW(E) both
-- contain `)`, a stray `)` at the top level is reduced through twice before any
-- state rejects it, which is the only way to reach the error path that runs
-- after several reduces.
--------------------------------------------------------------------------------

data Tok = TNum Int | TPlus | TLParen | TRParen | TEof

derive instance Eq Tok

instance Show Tok where
  show = case _ of
    TNum n -> "TNum " <> show n
    TPlus -> "TPlus"
    TLParen -> "TLParen"
    TRParen -> "TRParen"
    TEof -> "TEof"

exprTable :: Table Tok Int
exprTable =
  { action
  , goto
  , production
  , semanticAction
  , terminalIndex
  , terminalValue
  , terminalName
  , terminalCount: 5
  , startState: 0
  }
  where
  action = case _, _ of
    0, 0 -> shift 3
    0, 2 -> shift 4
    1, 1 -> shift 5
    1, 4 -> acceptAction
    2, 1 -> reduce 1
    2, 3 -> reduce 1
    2, 4 -> reduce 1
    3, 1 -> reduce 2
    3, 3 -> reduce 2
    3, 4 -> reduce 2
    4, 0 -> shift 3
    4, 2 -> shift 4
    5, 0 -> shift 3
    5, 2 -> shift 4
    6, 1 -> shift 5
    6, 3 -> shift 8
    7, 1 -> reduce 0
    7, 3 -> reduce 0
    7, 4 -> reduce 0
    8, 1 -> reduce 3
    8, 3 -> reduce 3
    8, 4 -> reduce 3
    _, _ -> errorAction

  goto = case _, _ of
    0, 0 -> 1
    0, 1 -> 2
    4, 0 -> 6
    4, 1 -> 2
    5, 1 -> 7
    _, _ -> 0

  production = case _ of
    0 -> { lhs: 0, arity: 3, name: "E -> E + T" }
    1 -> { lhs: 0, arity: 1, name: "E -> T" }
    2 -> { lhs: 1, arity: 1, name: "T -> n" }
    _ -> { lhs: 1, arity: 3, name: "T -> ( E )" }

  semanticAction = case _, _ of
    0, [ a, _, b ] -> a + b
    1, [ t ] -> t
    2, [ n ] -> n
    3, [ _, e, _ ] -> e
    _, _ -> 0

  terminalIndex = case _ of
    TNum _ -> 0
    TPlus -> 1
    TLParen -> 2
    TRParen -> 3
    TEof -> 4

  terminalValue = case _ of
    TNum n -> n
    _ -> 0

  terminalName = case _ of
    0 -> "n"
    1 -> "+"
    2 -> "("
    3 -> ")"
    _ -> "eof"

--------------------------------------------------------------------------------
-- S -> a S | <empty>
--
-- Right recursive, so the stack depth tracks the input length -- this is the
-- shape that turns quadratic if the stacks are copied on push and pop. It also
-- covers reducing by an empty production, including at the very start symbol.
--
-- Terminals a=0, eof=1; nonterminal S=0. The semantic value counts the a's.
--------------------------------------------------------------------------------

data RTok = RA | REof

derive instance Eq RTok

instance Show RTok where
  show = case _ of
    RA -> "RA"
    REof -> "REof"

rightRecTable :: Table RTok Int
rightRecTable =
  { action
  , goto
  , production
  , semanticAction
  , terminalIndex
  , terminalValue
  , terminalName
  , terminalCount: 2
  , startState: 0
  }
  where
  action = case _, _ of
    0, 0 -> shift 2
    0, 1 -> reduce 1
    1, 1 -> acceptAction
    2, 0 -> shift 2
    2, 1 -> reduce 1
    3, 1 -> reduce 0
    _, _ -> errorAction

  goto = case _, _ of
    0, 0 -> 1
    2, 0 -> 3
    _, _ -> 0

  production = case _ of
    0 -> { lhs: 0, arity: 2, name: "S -> a S" }
    _ -> { lhs: 0, arity: 0, name: "S -> <empty>" }

  semanticAction = case _, _ of
    0, [ _, s ] -> 1 + s
    1, [] -> 0
    _, _ -> 0

  terminalIndex = case _ of
    RA -> 0
    REof -> 1

  terminalValue = const 0

  terminalName = case _ of
    0 -> "a"
    _ -> "eof"

-- Deep enough that the copy-the-stack version took seconds (~4.5s at this
-- size), while constant-time stack operations finish in milliseconds.
deepSize :: Int
deepSize = 20000

-- Wide enough to absorb a slow machine, narrow enough that a return to
-- copy-the-whole-stack cannot slip through.
deepBudgetMs :: Number
deepBudgetMs = 1000.0

-- Deep enough that a runner which recursed through the monad rather than
-- looping would have run out of stack long before the end.
deepPullSize :: Int
deepPullSize = 100000

-- | The same table, unable to name a terminal.
mute :: forall tok val. Table tok val -> Table tok val
mute table = table
  { terminalName = \_ ->
      unsafeCrashWith "the expected set was worked out for a parse that did not fail"
  }

--------------------------------------------------------------------------------
-- Pulling tokens one at a time.
--------------------------------------------------------------------------------

-- | How far a source has got, and how often it was asked.
-- |
-- | `pulls` is what tells a shift from a reduction here. A reduction runs on
-- | the token the parser is already holding, so it must not reach the source;
-- | counting is the only way to see that difference from outside.
type Source = { index :: Int, pulls :: Int }

-- | Hand out an array, then end of input -- once.
-- |
-- | Being asked for anything after the end marker is not something a parser is
-- | allowed to do, so this says so rather than quietly answering again and
-- | letting the test pass.
pulling :: forall tok. tok -> Array tok -> State Source tok
pulling eof input = do
  source <- State.get
  -- `if` rather than `when`: the arguments of a call are evaluated whether or
  -- not the call uses them, and a crash is not a value that survives being
  -- built and thrown away.
  if source.index > Array.length input then
    unsafeCrashWith "asked for a token after end of input"
  else do
    State.put { index: source.index + 1, pulls: source.pulls + 1 }
    pure (fromMaybe eof (Array.index input source.index))

pulled
  :: forall tok val
   . Table tok val
  -> tok
  -> Array tok
  -> { result :: Either (ParseError tok) val, pulls :: Int }
pulled table eof input =
  case State.runState (parseM table (pulling eof input)) { index: 0, pulls: 0 } of
    Tuple result source -> { result, pulls: source.pulls }

-- | A lexer that gives up part way through, in a monad that can say so.
lexing :: Int -> Array Tok -> ExceptT String (State Source) Tok
lexing at input = do
  source <- lift State.get
  when (source.index == at) do
    throwError ("no idea what to make of token " <> show at)
  lift (pulling TEof input)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Puppy.Runtime" do
    describe "accepting" do
      it "reduces a single terminal" do
        parse exprTable [ TNum 7, TEof ] `shouldEqual` Right 7

      it "folds a left-recursive chain" do
        parse exprTable [ TNum 1, TPlus, TNum 2, TPlus, TNum 3, TEof ]
          `shouldEqual` Right 6

      it "descends into a nested group" do
        parse exprTable
          [ TLParen, TNum 1, TPlus, TNum 2, TRParen, TPlus, TNum 3, TEof ]
          `shouldEqual` Right 6

    describe "empty productions" do
      it "accepts an empty start symbol" do
        parse rightRecTable [ REof ] `shouldEqual` Right 0

      it "reduces an empty production mid-parse" do
        parse rightRecTable [ RA, RA, REof ] `shouldEqual` Right 2

    describe "errors" do
      it "reports an error on the very first token" do
        parse exprTable [ TPlus, TEof ] `shouldEqual` Left
          { position: 0
          , found: Just TPlus
          , state: 0
          , expected: [ "n", "(" ]
          }

      it "reports the position and expected set at a syntax error" do
        parse exprTable [ TNum 1, TPlus, TEof ] `shouldEqual` Left
          { position: 2
          , found: Just TEof
          , state: 5
          , expected: [ "n", "(" ]
          }

      -- `)` is in FOLLOW of both T and E, so the driver reduces by `T -> n` and
      -- then by `E -> T` before arriving at a state that has no action for it.
      it "reports an error reached only after several reduces" do
        parse exprTable [ TNum 1, TRParen ] `shouldEqual` Left
          { position: 1
          , found: Just TRParen
          , state: 1
          , expected: [ "+", "eof" ]
          }

      it "fails when the stream is not terminated" do
        parse exprTable [ TNum 1 ] `shouldEqual` Left
          { position: 1
          , found: Nothing
          , state: 3
          , expected: [ "+", ")", "eof" ]
          }

    describe "stack cost" do
      -- Timed by hand rather than left to the spec timeout: `parse` is a pure,
      -- synchronous computation, so it blocks the event loop and the Aff timer
      -- cannot fire while it runs.
      it "parses deep right recursion without quadratic blowup" do
        let input = Array.snoc (Array.replicate deepSize RA) REof
        Milliseconds start <- liftEffect (unInstant <$> now)
        let result = parse rightRecTable input
        Milliseconds end <- liftEffect (unInstant <$> now)
        result `shouldEqual` Right deepSize
        let elapsed = end - start
        when (elapsed >= deepBudgetMs) do
          fail $ "parsing " <> show deepSize <> " tokens took " <> show elapsed
            <> "ms, over the "
            <> show deepBudgetMs
            <> "ms budget -- the parser stacks are probably being copied again"

    describe "pulling tokens" do
      it "agrees with the array runner where the parse succeeds" do
        let input = [ TNum 1, TPlus, TNum 2, TEof ]
        (pulled exprTable TEof input).result `shouldEqual` parse exprTable input

      it "agrees with the array runner where the parse fails" do
        let input = [ TNum 1, TPlus, TRParen, TEof ]
        (pulled exprTable TEof input).result `shouldEqual` parse exprTable input

      it "asks once to begin with, and once after every shift" do
        let out = pulled exprTable TEof [ TNum 1, TPlus, TNum 2, TEof ]
        out.result `shouldEqual` Right 3
        out.pulls `shouldEqual` 4

      it "asks for nothing further once it has accepted" do
        let out = pulled exprTable TEof [ TNum 7, TEof ]
        out.result `shouldEqual` Right 7
        out.pulls `shouldEqual` 2

      -- Three pulls for two shifts: the token that fails the parse was asked
      -- for like any other, because nothing can tell a token is wrong without
      -- first having it. What does not happen is a fourth.
      it "asks for nothing further once the parse has failed" do
        let out = pulled exprTable TEof [ TNum 1, TPlus, TRParen, TEof ]
        out.result `shouldEqual` Left
          { position: 2
          , found: Just TRParen
          , state: 5
          , expected: [ "n", "(" ]
          }
        out.pulls `shouldEqual` 3

      it "runs a whole cascade of reductions on one token" do
        -- Right recursion: nothing can be reduced until the end marker
        -- arrives, and then everything is, one production after another. The
        -- count is what says the source was left alone while that happened.
        let
          input = Array.snoc (Array.replicate deepSize RA) REof
          out = pulled rightRecTable REof input
        out.result `shouldEqual` Right deepSize
        out.pulls `shouldEqual` (deepSize + 1)

      it "pulls a long input without running out of stack" do
        let
          input = Array.snoc (Array.replicate deepPullSize RA) REof
          out = pulled rightRecTable REof input
        out.result `shouldEqual` Right deepPullSize

      it "lets the source fail the parse" do
        State.evalState
          ( runExceptT
              (parseM exprTable (lexing 2 [ TNum 1, TPlus, TNum 2, TEof ]))
          )
          { index: 0, pulls: 0 }
          `shouldEqual` Left "no idea what to make of token 2"

    describe "the expected set" do
      -- Naming a terminal is the last step of working out `expected`, so a
      -- table that cannot name one turns any attempt into a crash. Paying for
      -- that at every token would cost the length of the input times the size
      -- of the alphabet, to answer a question a successful parse never asks.
      it "is not worked out for an array parse that succeeds" do
        parse (mute exprTable) [ TNum 1, TPlus, TNum 2, TEof ]
          `shouldEqual` Right 3

      it "is not worked out for a pulled parse that succeeds" do
        let input = Array.snoc (Array.replicate 100 RA) REof
        (pulled (mute rightRecTable) REof input).result `shouldEqual` Right 100

    describe "reducing by a wide production" do
      -- Five symbols, spelled back out. A reversed answer, or one that walked
      -- the value stack from the wrong end, reads as "edcba".
      it "hands the semantic action its arguments in production order" do
        parse wideTable [ WA, WB, WC, WD, WE, WEof ]
          `shouldEqual` Right "abcde"

      -- The pop walks both stacks with an accumulator, which the compiler
      -- turns into a loop -- and has to, because nothing bounds how many
      -- symbols a production may have.
      it "reduces by a production of thousands of symbols" do
        parse (hugeTable deepArity)
          (Array.snoc (Array.replicate deepArity WA) WEof)
          `shouldEqual` Right deepArity

    describe "transducing sources" do
      it "hands over what a step produced, in order" do
        (drain expand 0 [ One 1, Skip, Burst 2, One 3 ]).tokens
          `shouldEqual` [ 1, 2, 2, 2, 3, -4 ]

      it "offers the end of the input to the step, once" do
        -- Five pulls for four tokens: the fifth is the `Nothing` that lets the
        -- pass flush, and nothing asks a sixth time.
        (drain expand 0 [ One 1, Skip, Burst 2, One 3 ]).pulls
          `shouldEqual` 5

      it "asks again when a step produced nothing" do
        (drain expand 0 [ Skip, Skip, One 1 ]).tokens
          `shouldEqual` [ 1, -3 ]

      it "skips a long run of empty steps without running out of stack" do
        (drain expand 0 (Array.replicate deepPullSize Skip)).tokens
          `shouldEqual` [ negate deepPullSize ]

      it "is a source a pulling entry point can parse from" do
        -- `parseM` is the shape a generated `xFrom` has, and the pass here
        -- produces two tokens for most of the ones it is given. A generated
        -- entry point takes the `Maybe` as it comes, end of input being
        -- `Nothing` to its tables; this hand-built one names its own end
        -- terminal, so the two have to be lined up here.
        State.evalState
          ( State.evalStateT
              (parseM exprTable (fromMaybe TEof <$> transduce infixing counted))
              (initial false)
          )
          { index: 0, pulls: 0, input: [ 1, 2, 3 ] }
          `shouldEqual` Right 6

--------------------------------------------------------------------------------
-- Transducing sources
--------------------------------------------------------------------------------

-- | The three shapes a step can have, in one pass: a token that produces
-- | nothing, one that produces a token, and one that produces several.
data Raw = Skip | One Int | Burst Int

-- | Counts what it was given, and says so at the end of the input.
expand :: Int -> Maybe Raw -> Tuple Int (Array Int)
expand seen = case _ of
  Nothing -> Tuple seen [ negate seen ]
  Just Skip -> Tuple (seen + 1) []
  Just (One n) -> Tuple (seen + 1) [ n ]
  Just (Burst n) -> Tuple (seen + 1) [ n, n, n ]

-- | Puts a `+` between the numbers it is given, and ends the input properly.
infixing :: Boolean -> Maybe Int -> Tuple Boolean (Array Tok)
infixing started = case _ of
  Nothing -> Tuple started [ TEof ]
  Just n
    | started -> Tuple true [ TPlus, TNum n ]
    | otherwise -> Tuple true [ TNum n ]

type Reading a = { index :: Int, pulls :: Int, input :: Array a }

-- | The underlying source: one element of the array per pull, counting them.
counted :: forall a. State (Reading a) (Maybe a)
counted = do
  reading <- State.get
  State.put reading { index = reading.index + 1, pulls = reading.pulls + 1 }
  pure (Array.index reading.input reading.index)

-- | Everything a transducing source produces, and how often it pulled.
drain
  :: forall s raw tok
   . (s -> Maybe raw -> Tuple s (Array tok))
  -> s
  -> Array raw
  -> { tokens :: Array tok, pulls :: Int }
drain step transducer input =
  case State.runState (State.evalStateT (Rec.tailRecM go Nil) (initial transducer)) start' of
    Tuple collected reading ->
      { tokens: Array.reverse (Array.fromFoldable collected)
      , pulls: reading.pulls
      }
  where
  start' = { index: 0, pulls: 0, input }

  go collected = transduce step counted <#> case _ of
    Nothing -> Rec.Done collected
    Just token -> Rec.Loop (Cons token collected)

--------------------------------------------------------------------------------
-- Reducing by a production of more than two symbols
--
-- The stack keeps the rightmost symbol at the head, so a reduction has to turn
-- what it takes off back into the order the production was written. Two
-- symbols cannot tell a correct answer from a reversed one, and the tables
-- above reduce by `E -> E + T` into a sum, which cannot tell either. This one
-- can: it spells out what it was handed, in the order it was handed it.
--
--   wide -> A B C D E
--------------------------------------------------------------------------------

data WTok = WA | WB | WC | WD | WE | WEof

derive instance Eq WTok

instance Show WTok where
  show = case _ of
    WA -> "WA"
    WB -> "WB"
    WC -> "WC"
    WD -> "WD"
    WE -> "WE"
    WEof -> "WEof"

wideTable :: Table WTok String
wideTable =
  { action
  , goto
  , production
  , semanticAction
  , terminalIndex
  , terminalValue
  , terminalName
  , terminalCount: 6
  , startState: 0
  }
  where
  -- Shift the five symbols in turn, then reduce, then accept.
  action = case _, _ of
    0, 0 -> shift 1
    1, 1 -> shift 2
    2, 2 -> shift 3
    3, 3 -> shift 4
    4, 4 -> shift 5
    5, 5 -> reduce 0
    6, 5 -> acceptAction
    _, _ -> errorAction

  goto = case _, _ of
    0, 0 -> 6
    _, _ -> 0

  production _ = { lhs: 0, arity: 5, name: "wide -> A B C D E" }

  semanticAction _ values = joinWith "" values

  terminalIndex = case _ of
    WA -> 0
    WB -> 1
    WC -> 2
    WD -> 3
    WE -> 4
    WEof -> 5

  terminalValue = case _ of
    WA -> "a"
    WB -> "b"
    WC -> "c"
    WD -> "d"
    WE -> "e"
    WEof -> ""

  terminalName = case _ of
    0 -> "A"
    1 -> "B"
    2 -> "C"
    3 -> "D"
    4 -> "E"
    _ -> "end of input"

deepArity :: Int
deepArity = 50000

-- | One production that takes `n` symbols, for showing that taking them off
-- | the stack is a loop and not a stack of its own.
hugeTable :: Int -> Table WTok Int
hugeTable n =
  { action
  , goto
  , production
  , semanticAction
  , terminalIndex
  , terminalValue
  , terminalName
  , terminalCount: 6
  , startState: 0
  }
  where
  -- State 0 shifts every `A` and stays put; the end marker reduces, and the
  -- state the goto lands in accepts.
  action = case _, _ of
    0, 0 -> shift 0
    0, 5 -> reduce 0
    1, 5 -> acceptAction
    _, _ -> errorAction

  goto _ _ = 1

  production _ = { lhs: 0, arity: n, name: "huge" }

  semanticAction _ values = Array.length values

  terminalIndex = case _ of
    WA -> 0
    _ -> 5

  terminalValue _ = 0

  terminalName _ = "A"
