-- | An offside rule, and a generated parser reading its output.
-- |
-- | This is the arrangement Puppy expects for a layout-sensitive language, and
-- | the point of the fixture is that the parser is not in on it. A lexer makes
-- | tokens and knows what column each one starts at; a pass over that stream
-- | turns columns into `BlockStart`, `BlockSep` and `BlockEnd`; the generated
-- | parser is handed the result and sees an ordinary bracketed language.
-- | Nothing feeds back the other way. The parser is never asked whether a
-- | token would fit, and the pass never looks at a parse state.
-- |
-- | That is enough for PureScript's own layout rule, which its compiler
-- | likewise applies before parsing rather than during it -- see
-- | `Language.PureScript.CST.Layout`. It is not enough for every offside
-- | language: Haskell's rule ends a block when the parser turns out not to be
-- | able to accept a token, and no pass with only the token stream to go on
-- | can tell that. PureScript's rule was written not to need it.
-- |
-- | The rule below is a scaled-down one for a language with a single kind of
-- | block, but the shape is the same, `in` included: closing a block early
-- | because a particular keyword arrived is a thing the pass decides from the
-- | keyword, not something it learns by watching a parser fail.
-- |
-- | `Puppy.Runtime.Source.transduce` is what joins the two. A pass produces
-- | none, one or several tokens for each one it is given -- a line that ends
-- | two blocks at once produces two closes before the token itself -- and the
-- | entry point wants exactly one at a time, so something has to hold the rest
-- | in between.
module Test.Puppy.Fixture.Offside (spec) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.State (State, StateT)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import Puppy.Fixture.Offside.Block as Block
import Puppy.Fixture.Offside.Token as T
import Puppy.Runtime (ParseError)
import Puppy.Runtime.Source (SourceState)
import Puppy.Runtime.Source as Source
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

--------------------------------------------------------------------------------
-- The lexer
--
-- Ordinary, except that it counts columns. Nothing here knows what a block is.
--------------------------------------------------------------------------------

type Cursor = { rest :: String, column :: Int }

type Lexing = ExceptT String (State Cursor)

nextToken :: Lexing (Maybe T.Token)
nextToken = do
  cursor <- lift (State.gets skipSpace)
  lift (State.put cursor)
  case SCU.uncons cursor.rest of
    Nothing -> pure Nothing
    Just { head }
      | head == '=' -> emit cursor 1 T.Equals
      | head == '+' -> emit cursor 1 T.Plus
      | isDigit head ->
          let
            digits = SCU.takeWhile isDigit cursor.rest
          in
            case Int.fromString digits of
              Just n -> emit cursor (SCU.length digits) (T.Number n)
              Nothing -> throwError ("not a number: " <> digits)
      | isLetter head ->
          let
            word = SCU.takeWhile isLetter cursor.rest
          in
            emit cursor (SCU.length word) case word of
              "let" -> T.Let
              "in" -> T.In
              _ -> T.Name word
      | otherwise -> throwError ("unexpected character " <> show head)
  where
  emit cursor width lexeme = do
    lift
      ( State.put
          { rest: SCU.drop width cursor.rest
          , column: cursor.column + width
          }
      )
    pure (Just (T.At cursor.column lexeme))

  isDigit c = c >= '0' && c <= '9'

  isLetter c = c >= 'a' && c <= 'z'

-- | Whitespace, and what it does to the column. A newline is the only thing
-- | here that matters to anything else: it is what puts a token at a column
-- | the layout rule can compare against.
skipSpace :: Cursor -> Cursor
skipSpace cursor = case SCU.uncons cursor.rest of
  Just { head: ' ', tail } -> skipSpace { rest: tail, column: cursor.column + 1 }
  Just { head: '\n', tail } -> skipSpace { rest: tail, column: 1 }
  _ -> cursor

--------------------------------------------------------------------------------
-- The layout pass
--------------------------------------------------------------------------------

-- | The columns of the blocks that are open, innermost first, and whether the
-- | token about to arrive is the one that opens a new block.
-- |
-- | `opening` is how a block learns its column without anything reading ahead.
-- | `let` does not say where its block is -- the token after it does -- so
-- | `let` records that the next token, wherever it turns out to be, begins one.
type Layout = { columns :: Array Int, opening :: Boolean }

emptyLayout :: Layout
emptyLayout = { columns: [], opening: false }

-- | One token in, however many out.
-- |
-- | Three things can happen to a token that is not opening a block. It may be
-- | further left than blocks that are open, which ends them. It may be at the
-- | column of the innermost one, which ends a declaration and starts the next.
-- | And it may be `in`, which ends the block it belongs to wherever it is
-- | written -- the case that makes `let x = 1 in x` work on one line, and the
-- | one a rule built only on columns would miss.
insertLayout :: Layout -> Maybe T.Token -> Tuple Layout (Array T.Token)
insertLayout layout = case _ of
  -- The input ran out with blocks still open. Closing them here is what lets
  -- the parser fail for the reason it should -- a missing `in` -- rather than
  -- for want of a `VCLOSE` nobody was ever going to write.
  Nothing ->
    Tuple emptyLayout (map (\_ -> T.At 0 T.BlockEnd) layout.columns)

  Just token@(T.At column lexeme)
    | layout.opening ->
        Tuple
          { columns: Array.cons column layout.columns
          , opening: lexeme == T.Let
          }
          [ T.At column T.BlockStart, token ]
    | otherwise ->
        let
          -- Every block whose column this token is to the left of.
          dedented = Array.span (column < _) layout.columns
          closes = map (\_ -> T.At column T.BlockEnd) dedented.init
        in
          case lexeme, Array.uncons dedented.rest of
            -- `in` closes the block it ends, unless dedenting already did.
            T.In, Just open | Array.null dedented.init ->
              Tuple
                { columns: open.tail, opening: false }
                [ T.At column T.BlockEnd, token ]
            T.In, _ ->
              Tuple
                { columns: dedented.rest, opening: false }
                (closes <> [ token ])
            _, Just open | open.head == column ->
              Tuple
                { columns: dedented.rest, opening: lexeme == T.Let }
                (closes <> [ T.At column T.BlockSep, token ])
            _, _ ->
              Tuple
                { columns: dedented.rest, opening: lexeme == T.Let }
                (closes <> [ token ])

--------------------------------------------------------------------------------
-- The two, joined
--------------------------------------------------------------------------------

-- | Lexer, layout pass and parser, in one pass over the text.
-- |
-- | The state of the layout pass sits between the parser and the lexer, which
-- | is what `StateT` over the lexer's own monad says. No token array is built
-- | at either step: the lexer holds the text it has not read, the pass holds
-- | the tokens it has produced and the parser has not asked for, and that is
-- | the whole of what is in flight.
run :: String -> Either String String
run input =
  case State.evalState (runExceptT (State.evalStateT parsing sourced)) cursor of
    Left lexError -> Left ("lex error: " <> lexError)
    Right (Left parseError) -> Left (describeError parseError)
    Right (Right rendered) -> Right rendered
  where
  cursor = { rest: input, column: 1 }

  sourced = Source.initial emptyLayout

  parsing
    :: StateT (SourceState Layout T.Token) Lexing
         (Either (ParseError T.Token) String)
  parsing = Block.expressionFrom (Source.transduce insertLayout nextToken)

-- | The token index in an error is counted over what the parser was given, and
-- | what the parser was given is the layout pass's output. Reporting it as a
-- | position in the text would be wrong twice over: the virtual tokens are not
-- | in the text at all, and the ones that are do not know where they were. A
-- | column comes from the token, which is why the token type carries one.
describeError :: ParseError T.Token -> String
describeError parseError =
  "parse error at token " <> show parseError.position
    <> found
    <> ", expected "
    <> joinWith " or " parseError.expected
  where
  found = case parseError.found of
    Just (T.At column _) -> " (column " <> show column <> ")"
    Nothing -> " (end of input)"

spec :: Spec Unit
spec = describe "a generated parser behind an offside rule" do
  it "reads a block written on one line" do
    run "let x = 1 in x" `shouldEqual` Right "(let x=1 in x)"

  it "reads a block written over several lines" do
    run "let\n  x = 1\n  y = 2\nin x + y"
      `shouldEqual` Right "(let x=1 y=2 in (x+y))"

  -- The declarations line up under one another, so the pass puts a separator
  -- between them and the grammar's `decls` rule has something to match.
  it "separates declarations by their column, not by punctuation" do
    run "let\n  x = 1\n  y = 2\n  z = 3\nin z"
      `shouldEqual` Right "(let x=1 y=2 z=3 in z)"

  -- A continuation line is indented past the column of the block, so nothing
  -- is inserted and the declaration simply carries on.
  it "lets a declaration run over a line without ending it" do
    run "let\n  x = 1\n    + 2\nin x"
      `shouldEqual` Right "(let x=(1+2) in x)"

  it "nests one block inside another" do
    run "let\n  x = let y = 2 in y + 1\nin x"
      `shouldEqual` Right "(let x=(let y=2 in (y+1)) in x)"

  -- `in` at the column of the block would otherwise be read as the start of
  -- another declaration: the pass closes the block instead of separating it.
  it "closes a block for an `in` written at its own column" do
    run "let\n  x = 1\n  in x" `shouldEqual` Right "(let x=1 in x)"

  it "closes every open block at the end of the input" do
    -- Two blocks are open and neither has its `in`. The pass closes both, so
    -- what the parser reports is the missing `in` rather than a block that
    -- never ended -- and it reports it against the second of the two closing
    -- tokens, which is a token the pass made up and the text does not contain.
    -- Column 0 is what such a token is given: it was written nowhere.
    run "let\n  x = let y = 2\n"
      `shouldEqual` Left "parse error at token 10 (column 0), expected in"

  -- What `position` counts. The `+` is the ninth token of the text, and the
  -- twelfth the parser was given: a VOPEN, a VSEP and a VCLOSE went in ahead
  -- of it. An index into the text is not something this number can be, and
  -- pointing at the mistake means asking the token, which is why the token
  -- type carries a column.
  it "counts an error's position over the tokens the parser was given" do
    run "let\n  x = 1\n  y = 2\nin + 1"
      `shouldEqual`
        Left
          ( "parse error at token 11 (column 4), expected let or a name"
              <> " or a number"
          )

  it "still reports what the lexer could not read" do
    run "let x = ? in x" `shouldEqual` Left "lex error: unexpected character '?'"
