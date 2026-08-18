module Test.Puppy.Syntax.Lexer (spec) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Puppy.Syntax.Lexer (LexToken(..), lex)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- | The text of every `{ ... }` block in a fragment, or the error that stopped
-- | the lexer. Most of these cases distinguish "the scanner understood this"
-- | from "the scanner ran to the end of the file looking for a closing brace",
-- | so the error message matters as much as the text.
braced :: String -> Either String (Array String)
braced source = case lex source of
  Left err -> Left err.message
  Right toks -> Right (Array.mapMaybe pick toks)
  where
  pick t = case t.token of
    TBraced code -> Just code.text
    _ -> Nothing

spec :: Spec Unit
spec = describe "Puppy.Syntax.Lexer" do
  describe "semantic actions" do
    it "takes the text between the braces, unchanged" do
      braced "{ Add a b }" `shouldEqual` Right [ " Add a b " ]

    it "counts nested braces" do
      braced "{ { a: 1, b: { c: 2 } } }"
        `shouldEqual` Right [ " { a: 1, b: { c: 2 } } " ]

    it "ignores a brace inside a string" do
      braced """{ "}" }""" `shouldEqual` Right [ """ "}" """ ]

    it "ignores a brace after an escaped quote" do
      braced "{ \"\\\"}\" }" `shouldEqual` Right [ " \"\\\"}\" " ]

    it "ignores a brace inside a character literal" do
      braced "{ '}' }" `shouldEqual` Right [ " '}' " ]

    -- Without the prime rule the quote would open a character literal and
    -- swallow the closing brace.
    it "treats a quote after an identifier as a prime" do
      braced "{ f xs' }" `shouldEqual` Right [ " f xs' " ]

    it "ignores a brace inside a line comment" do
      braced "{ a -- }\n }" `shouldEqual` Right [ " a -- }\n " ]

    -- `-->` is an operator, not a comment opener, so the rest of the line is
    -- still code and the brace on it still closes the block.
    it "does not mistake an operator for a line comment" do
      braced "{ a --> b }" `shouldEqual` Right [ " a --> b " ]

    it "ignores braces inside nested block comments" do
      braced "{ a {- x {- y -} } -} b }"
        `shouldEqual` Right [ " a {- x {- y -} } -} b " ]

    it "ignores a brace inside a triple-quoted string" do
      braced "{ \"\"\" } \"\"\" }" `shouldEqual` Right [ " \"\"\" } \"\"\" " ]

    it "reports an unterminated block rather than guessing" do
      braced "{ Add a b" `shouldEqual` Left "unterminated `{ ... }` block"

    it "reads several blocks in one file" do
      braced "a: | X { 1 } | Y { 2 }" `shouldEqual` Right [ " 1 ", " 2 " ]

  -- Types are delimited the same way actions are. `}` is reserved punctuation
  -- in PureScript and can never be part of an operator, so brace counting cuts
  -- an arbitrary type in exactly the right place -- which is the whole reason
  -- these are not written `<...>`.
  describe "type annotations" do
    it "takes the text between the braces" do
      braced "%token { Int } INT" `shouldEqual` Right [ " Int " ]

    it "keeps a function type whole" do
      braced "%type { Int -> Int } f" `shouldEqual` Right [ " Int -> Int " ]

    it "keeps a constrained type whole" do
      braced "%type { Show a => a } f" `shouldEqual` Right [ " Show a => a " ]

    -- The case that defeats angle brackets: `~>` is an ordinary type operator
    -- and `>` is a symbol character, so no rule about `->` and `=>` saves it.
    it "keeps a natural transformation whole" do
      braced "%type { f ~> g } main" `shouldEqual` Right [ " f ~> g " ]

    it "keeps an operator section whole" do
      braced "%type { (>) a b } main" `shouldEqual` Right [ " (>) a b " ]

    it "keeps a record type whole" do
      braced "%type { { x :: Int, y :: Int } } point"
        `shouldEqual` Right [ " { x :: Int, y :: Int } " ]

    it "no longer accepts the angle-bracketed spelling" do
      braced "%token <Int> INT" `shouldEqual` Left "unexpected character '<'"

  describe "grammar syntax" do
    it "skips line and nested block comments between declarations" do
      braced "-- one\n{- two {- three -} -}\n%token { Int } INT"
        `shouldEqual` Right [ " Int " ]

    it "reports an unterminated header" do
      braced "%{ import Foo" `shouldEqual` Left "unterminated `%{` header"
