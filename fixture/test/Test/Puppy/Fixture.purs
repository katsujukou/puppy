-- | The generated parsers, compiled by the PureScript compiler and then run.
-- |
-- | A golden test on the generated text says the generator produced what it
-- | produced last time. It cannot say the result is a parser. These do: the
-- | modules under `src` are the generator's own output, this package builds
-- | them for real, and the assertions below feed them tokens.
module Test.Puppy.Fixture where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Puppy.Fixture.Calculator as Calculator
import Puppy.Fixture.Awkward as Awkward
import Puppy.Fixture.Lists as Lists
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | Needs a signature of its own: without one it would be fixed to whichever
-- | parser used it first.
leftOf :: forall e a. Either e a -> Maybe e
leftOf = case _ of
  Left err -> Just err
  Right _ -> Nothing

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "a generated calculator" do
    it "parses a single number" do
      Calculator.expression [ Calculator.INT 42 ] `shouldEqual` Right 42

    it "runs the semantic actions the grammar wrote" do
      Calculator.expression
        [ Calculator.INT 2
        , Calculator.PLUS
        , Calculator.INT 3
        ]
        `shouldEqual` Right 5

    -- `%left TIMES` after `%left PLUS`, so multiplication binds tighter: this
    -- is 2 + (3 * 4), not (2 + 3) * 4.
    it "obeys the precedence declarations" do
      Calculator.expression
        [ Calculator.INT 2
        , Calculator.PLUS
        , Calculator.INT 3
        , Calculator.TIMES
        , Calculator.INT 4
        ]
        `shouldEqual` Right 14

    it "obeys left associativity" do
      Calculator.expression
        [ Calculator.INT 10
        , Calculator.PLUS
        , Calculator.INT 3
        , Calculator.PLUS
        , Calculator.INT 1
        ]
        `shouldEqual` Right 14

    it "reads a parenthesised group" do
      Calculator.expression
        [ Calculator.LPAREN
        , Calculator.INT 2
        , Calculator.PLUS
        , Calculator.INT 3
        , Calculator.RPAREN
        , Calculator.TIMES
        , Calculator.INT 4
        ]
        `shouldEqual` Right 20

    it "reports where the input went wrong, and what it wanted" do
      Calculator.expression [ Calculator.INT 1, Calculator.PLUS ]
        `shouldEqual` Left
          { position: 2
          , found: Nothing
          , state: 5
          , expected: [ "LPAREN", "INT" ]
          }

    -- The end marker is not a `Token`, so an error that lands on it has no
    -- token to report. An error anywhere else does.
    it "names the token when the input goes wrong before the end" do
      map _.found (leftOf (Calculator.expression [ Calculator.PLUS ]))
        `shouldEqual` Just (Just Calculator.PLUS)

  describe "a generated list parser" do
    it "accepts nothing at all" do
      Lists.items [] `shouldEqual` Right []

    it "reduces by an empty production in the middle of a parse" do
      Lists.items [ Lists.A "x" ] `shouldEqual` Right [ "[x!]" ]

    it "reads a separated list" do
      Lists.items [ Lists.A "x", Lists.COMMA, Lists.B "y" ]
        `shouldEqual` Right [ "[x!]", "[y?]" ]

    -- `item` binds `x` to the result of `wrapped`, and `wrapped` binds `x` to
    -- its own token. Each piece of the grammar's code has to see the `x` it was
    -- written against: `[x!]` means the inner one produced `x!` and the outer
    -- one wrapped it, which is only right if the two never met.
    it "keeps a binder inside an inlined rule apart from one outside it" do
      Lists.single [ Lists.B "q" ] `shouldEqual` Right "[q?]"

    -- Both entry points share one set of tables and differ only in where they
    -- start.
    it "gives each start symbol its own entry point" do
      Lists.single [ Lists.A "z" ] `shouldEqual` Right "[z!]"
      Lists.items [ Lists.A "z" ] `shouldEqual` Right [ "[z!]" ]

    it "will not accept what only the other start symbol would" do
      map _.found (leftOf (Lists.single []))
        `shouldEqual` Just Nothing

  -- Every start symbol and binder in this grammar is a name the generated code
  -- once used for something of its own. That it compiles at all is the test;
  -- these run it to be sure it compiled into the right thing.
  describe "a grammar whose names the generator once used" do
    it "gives each start symbol its own working entry point" do
      Awkward.input [ Awkward.VALUE 7 ] `shouldEqual` Right 7
      Awkward.state [ Awkward.VALUE 9 ] `shouldEqual` Right 9

    it "runs actions whose binders are named after generated locals" do
      Awkward.input [ Awkward.STATE ] `shouldEqual` Right 0
      Awkward.input [ Awkward.TAB ] `shouldEqual` Right 1

    -- The display names carry a tab and a control character, and the one after
    -- the control character is a hex digit.
    it "reports display names that needed escaping" do
      map _.expected (leftOf (Awkward.input []))
        -- Written the same way the generator has to write it: `\x` is greedy,
        -- so the `A` would otherwise be read as part of the escape.
        `shouldEqual` Just [ "STATE", "VALUE", "a\tb", "x\x01" <> "Ay" ]
