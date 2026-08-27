module Test.Puppy.LR.Grammar (spec, numbered) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (Pattern(..), contains)
import Data.String.Common (joinWith)
import Effect.Aff (Aff)
import Puppy.Expand (expand)
import Puppy.LR.Analysis (Analysis, analyse, firstOf)
import Puppy.LR.Grammar (LRGrammar, number, renderProd)
import Puppy.Syntax (ConflictDirective(..))
import Puppy.Syntax.Parser as Parser
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

numbered :: String -> Either (Array String) LRGrammar
numbered source = case Parser.parse source of
  Left err -> Left [ "parse: " <> err.message ]
  Right syn -> case expand syn of
    Left errs -> Left (map _.message errs)
    Right core -> case number core of
      Left errs -> Left (map _.message errs)
      Right g -> Right g

withGrammar :: String -> (LRGrammar -> Aff Unit) -> Aff Unit
withGrammar source k = case numbered source of
  Left messages -> fail ("failed to number: " <> joinWith "; " messages)
  Right g -> k g

shouldFailWith :: String -> String -> Aff Unit
shouldFailWith needle source = case numbered source of
  Right _ -> fail ("expected a failure mentioning " <> show needle)
  Left messages ->
    when (not (Array.any (contains (Pattern needle)) messages)) do
      fail ("expected " <> show needle <> ", got: " <> joinWith "; " messages)

nullableNames :: LRGrammar -> Analysis -> Array String
nullableNames g a = Array.sort
  ( Array.mapMaybe (Array.index g.nonterminals)
      (Set.toUnfoldable a.nullable :: Array Int)
  )

firstNames :: LRGrammar -> Analysis -> String -> Array String
firstNames g a name = case Array.findIndex (_ == name) g.nonterminals of
  Nothing -> [ "<no such nonterminal>" ]
  Just n -> Array.sort
    ( Array.mapMaybe
        (map _.name <<< Array.index g.terminals)
        (Set.toUnfoldable (firstOf a n) :: Array Int)
    )

unproductiveSource :: String
unproductiveSource =
  "%token A\n%start { X } main\n%%\nmain: | x = expr { x }\nexpr: | a = expr A { a }\n"

arithmetic :: String
arithmetic =
  """
%token PLUS TIMES INT
%start { E } main
%%
main: | e = expr { e }
expr:
  | i = INT                              { Lit i }
  | a = expr PLUS b = expr               { Add a b }
  | a = expr TIMES b = expr %prec TIMES  { Mul a b }
"""

-- | The same tokens as `arithmetic`, with one rule that names `ERROR`, so
-- | that the two can be compared terminal for terminal.
recovering :: String
recovering =
  """
%token PLUS TIMES INT
%start { E } main
%%
main: | e = expr { e }
expr:
  | i = INT                              { Lit i }
  | a = expr PLUS b = expr               { Add a b }
  | a = expr TIMES b = expr %prec TIMES  { Mul a b }
  | e = ERROR                            { Broken e }
"""

spec :: Spec Unit
spec = do
  describe "Puppy.LR.Grammar" do
    it "numbers the declared tokens in order and puts end of input last" do
      withGrammar arithmetic \g ->
        map _.name g.terminals `shouldEqual` [ "PLUS", "TIMES", "INT", "EOF" ]

    it "numbers nonterminals in the order they first appear" do
      withGrammar arithmetic \g ->
        g.nonterminals `shouldEqual` [ "main", "expr", "<start main>" ]

    -- Written so that no grammar can name it: an identifier cannot contain
    -- angle brackets.
    it "adds a production to reduce the start symbol by" do
      withGrammar arithmetic \g ->
        map (renderProd g) g.productions `shouldEqual`
          [ "main -> expr"
          , "expr -> INT"
          , "expr -> expr PLUS expr"
          , "expr -> expr TIMES expr"
          , "<start main> -> main"
          ]

    it "points each start symbol at its own production" do
      withGrammar arithmetic \g ->
        g.starts `shouldEqual`
          [ { name: "main", symbol: 0, production: 4 } ]

    it "numbers the end of input after the declared tokens" do
      withGrammar arithmetic \g -> do
        map _.name g.terminals `shouldEqual` [ "PLUS", "TIMES", "INT", "EOF" ]
        g.eof `shouldEqual` 3
        g.errorTerminal `shouldEqual` Nothing

    -- `ERROR` goes on the end, after the end of input, and only where a rule
    -- named it. Putting it there first would make `eof` the number of whatever
    -- was appended last.
    it "numbers ERROR past the end of input, where a rule names one" do
      withGrammar recovering \g -> do
        map _.name g.terminals
          `shouldEqual` [ "PLUS", "TIMES", "INT", "EOF", "ERROR" ]
        g.eof `shouldEqual` 3
        g.errorTerminal `shouldEqual` Just 4

    it "leaves the numbers alone in a grammar that names no ERROR" do
      withGrammar arithmetic \plain ->
        withGrammar recovering \recovered ->
          Array.take 4 (map _.name recovered.terminals)
            `shouldEqual` map _.name plain.terminals

    it "resolves %prec to a terminal number" do
      withGrammar arithmetic \g ->
        map _.directive g.productions `shouldEqual`
          [ Inferred, Inferred, Inferred, Prec 1, Inferred ]

    it "keeps a link back to the production a semantic action came from" do
      withGrammar arithmetic \g ->
        map _.source g.productions `shouldEqual`
          [ Just 0, Just 1, Just 2, Just 3, Nothing ]

    -- Well formed by every earlier check, and yet no input can match it.
    it "rejects a nonterminal that cannot derive any tokens" do
      shouldFailWith "cannot derive any sequence of tokens" unproductiveSource

    -- The added start production is unproductive too, but it is named so that
    -- no grammar could have written it, so pointing at it would point at
    -- something the reader cannot find.
    it "does not name its own internal nonterminals in a diagnostic" do
      case numbered unproductiveSource of
        Right _ -> fail "expected a failure"
        Left messages ->
          Array.any (contains (Pattern "<start")) messages `shouldEqual` false

  describe "Puppy.LR.Analysis" do
    it "finds the nonterminals that can vanish" do
      withGrammar
        "%token A B\n%start { X } main\n%%\nmain: | x = opt B { x }\nopt: | { N } | a = A { J a }\n"
        \g -> nullableNames g (analyse g) `shouldEqual` [ "opt" ]

    it "leaves a nonterminal that always needs a token out of the nullable set" do
      withGrammar arithmetic \g ->
        nullableNames g (analyse g) `shouldEqual` []

    it "reads through a nullable prefix when collecting first tokens" do
      withGrammar
        "%token A B C\n%start { X } main\n%%\nmain: | p q C { M }\np: | { N } | A { N }\nq: | { N } | B { N }\n"
        \g -> do
          let a = analyse g
          nullableNames g a `shouldEqual` [ "p", "q" ]
          firstNames g a "main" `shouldEqual` [ "A", "B", "C" ]

    it "collects first tokens through a chain of nonterminals" do
      withGrammar arithmetic \g -> do
        let a = analyse g
        firstNames g a "expr" `shouldEqual` [ "INT" ]
        firstNames g a "main" `shouldEqual` [ "INT" ]
        firstNames g a "<start main>" `shouldEqual` [ "INT" ]
