module Test.Puppy.Runtime where

import Prelude

import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Puppy.Runtime (Action(..), Table, parse)
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
    0, 0 -> Shift 3
    0, 2 -> Shift 4
    1, 1 -> Shift 5
    1, 4 -> Accept
    2, 1 -> Reduce 1
    2, 3 -> Reduce 1
    2, 4 -> Reduce 1
    3, 1 -> Reduce 2
    3, 3 -> Reduce 2
    3, 4 -> Reduce 2
    4, 0 -> Shift 3
    4, 2 -> Shift 4
    5, 0 -> Shift 3
    5, 2 -> Shift 4
    6, 1 -> Shift 5
    6, 3 -> Shift 8
    7, 1 -> Reduce 0
    7, 3 -> Reduce 0
    7, 4 -> Reduce 0
    8, 1 -> Reduce 3
    8, 3 -> Reduce 3
    8, 4 -> Reduce 3
    _, _ -> Error

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
    0, 0 -> Shift 2
    0, 1 -> Reduce 1
    1, 1 -> Accept
    2, 0 -> Shift 2
    2, 1 -> Reduce 1
    3, 1 -> Reduce 0
    _, _ -> Error

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
