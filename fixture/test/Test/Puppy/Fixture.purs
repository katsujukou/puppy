-- | The generated parsers, compiled by the PureScript compiler and then run.
-- |
-- | A golden test on the generated text says the generator produced what it
-- | produced last time. It cannot say the result is a parser. These do: the
-- | modules under `src` are the generator's own output, this package builds
-- | them for real, and the assertions below feed them tokens.
module Test.Puppy.Fixture where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.State (State)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Puppy.Fixture.Calculator (Token(..))
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

--------------------------------------------------------------------------------
-- Handing the parser one token at a time.
--
-- The point of this lexer is what it does not do. There is no `Array Token`
-- anywhere in it, and at no moment has the whole input been turned into
-- tokens: what is left of the string is the whole of the state, and each token
-- is built when the parser asks for it and dropped when it has been shifted.
--
-- `ExceptT` over `State` because a lexer has two things to say -- here is the
-- next token, and I cannot read this -- and the monad the parser pulls in is
-- where the second one goes. The parser needs no opinion about it.
--------------------------------------------------------------------------------

type Lexing = ExceptT String (State String)

nextToken :: Lexing (Maybe Token)
nextToken = do
  rest <- lift (State.gets (SCU.dropWhile (_ == ' ')))
  lift (State.put rest)
  case SCU.uncons rest of
    Nothing -> pure Nothing
    Just { head, tail }
      | head == '+' -> emit tail PLUS
      | head == '*' -> emit tail TIMES
      | head == '(' -> emit tail LPAREN
      | head == ')' -> emit tail RPAREN
      | isDigit head ->
          let
            digits = SCU.takeWhile isDigit rest
          in
            case Int.fromString digits of
              Just n -> emit (SCU.drop (SCU.length digits) rest) (INT n)
              Nothing -> throwError ("not a number: " <> digits)
      | otherwise -> throwError ("unexpected character " <> show head)
  where
  emit rest token = do
    lift (State.put rest)
    pure (Just token)

  isDigit c = c >= '0' && c <= '9'

-- | Lex and parse together, in one pass over the text.
calculate :: String -> Either String Int
calculate input =
  case State.evalState (runExceptT (Calculator.expressionFrom nextToken)) input of
    Left lexError -> Left lexError
    Right (Left parseError) -> Left ("parse error at " <> show parseError.position)
    Right (Right value) -> Right value

-- | The other kind of source: something already in hand, handed over a piece at
-- | a time. Enough to exercise a second start symbol without a second lexer.
oneAtATime :: forall tok. State (Array tok) (Maybe tok)
oneAtATime = do
  rest <- State.get
  case Array.uncons rest of
    Nothing -> pure Nothing
    Just { head, tail } -> do
      State.put tail
      pure (Just head)

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

  describe "a generated parser fed one token at a time" do
    it "reaches the same answer as the array entry point" do
      calculate "1 + 2 * 3" `shouldEqual` Right 7
      Calculator.expression
        [ Calculator.INT 1
        , Calculator.PLUS
        , Calculator.INT 2
        , Calculator.TIMES
        , Calculator.INT 3
        ]
        `shouldEqual` Right 7

    it "obeys the same precedence, lexing as it goes" do
      calculate "(1 + 2) * 3" `shouldEqual` Right 9

    it "reports a syntax error at the token it counted" do
      calculate "1 +" `shouldEqual` Left "parse error at 2"

    -- The lexer's failure is not the parser's, and it does not have to be
    -- turned into one to get out: it is already in the monad the parser is
    -- running in, which abandons the parse on its own.
    it "lets the lexer abandon the parse" do
      calculate "1 @ 2" `shouldEqual` Left "unexpected character '@'"

    it "gives every start symbol one of these too" do
      State.evalState (Lists.itemsFrom oneAtATime)
        [ Lists.A "x", Lists.COMMA, Lists.B "y" ]
        `shouldEqual` Right [ "[x!]", "[y?]" ]
      State.evalState (Lists.singleFrom oneAtATime) [ Lists.B "q" ]
        `shouldEqual` Right "[q?]"
