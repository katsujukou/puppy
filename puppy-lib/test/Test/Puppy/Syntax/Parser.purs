module Test.Puppy.Syntax.Parser (spec) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Effect.Aff (Aff)
import Data.String.Common (joinWith)
import Puppy.Syntax
  ( Associativity(..)
  , ConflictDirective(..)
  , Grammar
  , Production
  , Rule
  , SymbolRef(..)
  , TokenDecl
  , TokenSource(..)
  , TokenValue(..)
  , declaredTokens
  )
import Puppy.Syntax.Lexer (LexToken(..), Located)
import Puppy.Syntax.Lexer as Lexer
import Puppy.Syntax.Parser (parse, parseGrammar)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

manyRules :: Int -> String
manyRules n = "%token P\n%start { X } main\n%%\nmain: |"
  <> joinWith "" (map (\i -> " t" <> show i) (Array.range 1 n))
  <> " { R }\n"
  <> joinWith "" (map (\i -> "t" <> show i <> ": | P { R }\n") (Array.range 1 n))

manyTokens :: Int -> String
manyTokens n = "%token"
  <> joinWith "" (map (\i -> " T" <> show i) (Array.range 1 n))
  <> "\n%start { X } main\n%%\nmain: | T1 { R }\n"

manyAlternatives :: Int -> String
manyAlternatives n = "%token P\n%start { X } main\n%%\nmain:\n"
  <> joinWith "" (map (\i -> "  | P { " <> show i <> " }\n") (Array.range 1 n))

sample :: String
sample =
  """
%{
import Ast (Expr(..))
%}

%token PLUS "+" TIMES "*"
%token LPAREN RPAREN MINUS UMINUS
%token { Int } INT

%start { Expr } main
%type { Expr } expr

%left PLUS
%left TIMES

%%

main:
  | e = expr   { e }

expr:
  | i = INT                      { Lit i }
  | a = expr PLUS b = expr       { Add a b }
  | LPAREN e = expr RPAREN       { e }
  | MINUS e = expr %prec UMINUS  { Neg e }
  ;

%inline pair(A, B):
  | a = A b = B   { Tuple a b }

items:
  | xs = separated_list(COMMA, expr)   { xs }
"""

withGrammar :: (Grammar -> Aff Unit) -> Aff Unit
withGrammar = withSource sample

withSource :: String -> (Grammar -> Aff Unit) -> Aff Unit
withSource source k = case parse source of
  Left err -> fail $ "failed to parse: " <> err.message
    <> " (line "
    <> show err.span.start.line
    <> ", column "
    <> show err.span.start.column
    <> ")"
  Right g -> k g

tokenDecls :: Grammar -> Array TokenDecl
tokenDecls g = declaredTokens g.tokens

ruleNamed :: String -> Grammar -> Maybe Rule
ruleNamed name g = Array.find (\r -> r.name == name) g.rules

productionAt :: Int -> Rule -> Maybe Production
productionAt = flip (\r -> Array.index r.productions)

-- | A token at a position no test cares about, for hand-built streams.
located :: LexToken -> Located
located token = { token, span: { start: nowhere, end: nowhere } }
  where
  nowhere = { offset: 0, line: 1, column: 1 }

-- | Assert that a parse fails and that its message mentions `needle`, which is
-- | what tells apart "rejected for the right reason" from "rejected at all".
shouldRejectWith :: String -> String -> Aff Unit
shouldRejectWith needle source = case parse source of
  Right _ -> fail $ "expected a parse error mentioning " <> show needle
  Left err ->
    when (not (contains (Pattern needle) err.message)) do
      fail $ "expected an error mentioning " <> show needle
        <> ", got: "
        <> err.message

-- | A rejection that has to point at a particular place, not just say a
-- | particular thing.
rejectionAt :: String -> { line :: Int, column :: Int } -> String -> Aff Unit
rejectionAt needle at source = case parse source of
  Right _ -> fail ("expected a parse error mentioning " <> show needle)
  Left err -> do
    when (not (contains (Pattern needle) err.message)) do
      fail ("expected an error mentioning " <> show needle <> ", got: " <> err.message)
    { line: err.span.start.line, column: err.span.start.column }
      `shouldEqual` at

externalWithWhole :: String
externalWithWhole =
  """
%tokentype { T.Token }
%token MINUS "-" { T.At _ T.Minus } PLUS "+" @ { T.At _ T.Plus } TIMES "*" { T.At _ T.Times }
%start { X } m
%%
m: | PLUS { 1 } | MINUS { 2 } | TIMES { 3 }
"""

isWhole :: TokenValue -> Boolean
isWhole = case _ of
  FromToken _ -> true
  FromPattern -> false

spec :: Spec Unit
spec = describe "Puppy.Syntax.Parser" do
  describe "declarations" do
    it "keeps the header verbatim" do
      withGrammar \g ->
        map _.text g.header `shouldEqual` Just "\nimport Ast (Expr(..))\n"

    it "reads every token declaration in source order" do
      withGrammar \g ->
        map _.name (tokenDecls g) `shouldEqual`
          [ "PLUS", "TIMES", "LPAREN", "RPAREN", "MINUS", "UMINUS", "INT" ]

    it "defaults the constructor and display name to the token name" do
      withGrammar \g ->
        map _.constructor (tokenDecls g) `shouldEqual`
          [ "PLUS", "TIMES", "LPAREN", "RPAREN", "MINUS", "UMINUS", "INT" ]

    it "takes a quoted display name when one is given" do
      withGrammar \g ->
        map _.display (tokenDecls g) `shouldEqual`
          [ "+", "*", "LPAREN", "RPAREN", "MINUS", "UMINUS", "INT" ]

    it "attaches a payload type only to the tokens that declared one" do
      withGrammar \g ->
        map (\d -> map _.text d.payload) (tokenDecls g) `shouldEqual`
          [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just " Int " ]

    -- `@` is what a terminal writes when its value is the whole token rather
    -- than a part of it, which a `$$` could not have reached.
    --
    -- One declaration, three names, and the mark on the middle one: `@` is per
    -- token and not per `%token`, which is what lets a grammar mix the two
    -- kinds without splitting every terminal into a declaration of its own.
    it "records which tokens are marked `@`, one by one" do
      case parse externalWithWhole of
        Left err -> fail ("failed to parse: " <> err.message)
        Right g -> case g.tokens of
          GeneratedTokens _ -> fail "expected an external token type"
          ExternalTokens external ->
            map (isWhole <<< _.value) external.tokens
              `shouldEqual` [ false, true, false ]

    -- The column matters as much as the message. The mark that cannot be
    -- there is the one to point at, and for a `$$` beside a `@` that is the
    -- `$$`, which is the later of the two.
    it "rejects `@` on a token that also declares a payload type" do
      rejectionAt "has nothing left to say" { line: 2, column: 18 }
        "%tokentype { T }\n%token { Int } A @ { PA }\n%start { X } m\n%%\nm: | A { 1 }\n"

    it "rejects `@` on a pattern that also has a `$$`" do
      rejectionAt "no part left for" { line: 2, column: 17 }
        "%tokentype { T }\n%token A @ { PA $$ }\n%start { X } m\n%%\nm: | A { 1 }\n"

    it "rejects `@` where Puppy writes the token type" do
      rejectionAt "no `%tokentype`" { line: 1, column: 10 }
        "%token A @ { PA }\n%start { X } m\n%%\nm: | A { 1 }\n"

    it "reads start symbols with their result type" do
      withGrammar \g ->
        map (\s -> { symbol: s.symbol, ty: s.resultType.text }) g.starts
          `shouldEqual` [ { symbol: "main", ty: " Expr " } ]

    it "reads nonterminal type declarations" do
      withGrammar \g ->
        map (\t -> { symbol: t.symbol, ty: t.resultType.text }) g.types
          `shouldEqual` [ { symbol: "expr", ty: " Expr " } ]

    -- Priority is positional, so the order these come back in is the whole
    -- meaning of the declarations.
    it "keeps precedence declarations in source order" do
      withGrammar \g ->
        map (\p -> { assoc: p.associativity, tokens: p.tokens }) g.precedences
          `shouldEqual`
            [ { assoc: AssocLeft, tokens: [ "PLUS" ] }
            , { assoc: AssocLeft, tokens: [ "TIMES" ] }
            ]

  describe "rules" do
    it "reads every rule in source order" do
      withGrammar \g ->
        map _.name g.rules `shouldEqual` [ "main", "expr", "pair", "items" ]

    it "records parameters and the inline flag" do
      withGrammar \g -> case ruleNamed "pair" g of
        Nothing -> fail "expected a rule named `pair`"
        Just r -> do
          r.parameters `shouldEqual` [ "A", "B" ]
          r.inline `shouldEqual` true

    it "leaves an ordinary rule uninlined and unparameterised" do
      withGrammar \g -> case ruleNamed "expr" g of
        Nothing -> fail "expected a rule named `expr`"
        Just r -> do
          r.parameters `shouldEqual` []
          r.inline `shouldEqual` false
          Array.length r.productions `shouldEqual` 4

    it "records which elements are bound and which are not" do
      withGrammar \g -> case ruleNamed "expr" g >>= productionAt 1 of
        Nothing -> fail "expected a second production of `expr`"
        Just p -> map _.binder p.elements
          `shouldEqual` [ Just "a", Nothing, Just "b" ]

    it "reads the symbols of a production" do
      withGrammar \g -> case ruleNamed "expr" g >>= productionAt 1 of
        Nothing -> fail "expected a second production of `expr`"
        Just p -> map _.symbol p.elements `shouldEqual`
          [ SymbolRef "expr" [], SymbolRef "PLUS" [], SymbolRef "expr" [] ]

    it "reads a nested symbol application" do
      withGrammar \g -> case ruleNamed "items" g >>= productionAt 0 of
        Nothing -> fail "expected a production of `items`"
        Just p -> map _.symbol p.elements `shouldEqual`
          [ SymbolRef "separated_list"
              [ SymbolRef "COMMA" [], SymbolRef "expr" [] ]
          ]

    it "records a %prec override on the production that carries it" do
      withGrammar \g -> case ruleNamed "expr" g of
        Nothing -> fail "expected a rule named `expr`"
        Just r -> map _.directive r.productions
          `shouldEqual` [ Inferred, Inferred, Inferred, Prec "UMINUS" ]

    it "records a `%shift` the same way" do
      case parse "%token A T\n%start { X } main\n%%\nmain: | A %shift { 1 } | T { 2 }\n" of
        Left err -> fail ("failed to parse: " <> err.message)
        Right g -> case ruleNamed "main" g of
          Nothing -> fail "expected a rule named `main`"
          Just r -> map _.directive r.productions
            `shouldEqual` [ PreferShift, Inferred ]

    -- One asks for a precedence and the other asks for the production to have
    -- none, so a production carrying both has not said what it wants.
    --
    -- The column is checked as well as the message: the complaint belongs on
    -- the second of the two, which is the one that cannot be there. Pointing
    -- at the first would send whoever reads it to a directive that is fine.
    it "rejects a production carrying both `%prec` and `%shift`" do
      rejectionAt "not both" { line: 5, column: 19 }
        "%token A P\n%start { X } main\n%left P\n%%\nmain: | A %prec P %shift { 1 }\n"

    it "rejects them the other way round too" do
      rejectionAt "not both" { line: 5, column: 18 }
        "%token A P\n%start { X } main\n%left P\n%%\nmain: | A %shift %prec P { 1 }\n"

    it "keeps each semantic action verbatim" do
      withGrammar \g -> case ruleNamed "expr" g of
        Nothing -> fail "expected a rule named `expr`"
        Just r -> map (_.text <<< _.action) r.productions `shouldEqual`
          [ " Lit i ", " Add a b ", " e ", " Neg e " ]

  -- A declaration naming a single symbol cannot tell a correct list from a
  -- reversed one, which is how the ordering bugs these cover went unnoticed.
  describe "declarations naming several symbols" do
    let
      source =
        "%token A B C\n%start { R } one two\n%type { R } p q\n%left A B\n%%\none: | A { 1 }\n"

    it "keeps token names in order" do
      withSource source \g ->
        map _.name (tokenDecls g) `shouldEqual` [ "A", "B", "C" ]

    it "keeps start symbols in order" do
      withSource source \g ->
        map _.symbol g.starts `shouldEqual` [ "one", "two" ]

    it "keeps type declarations in order" do
      withSource source \g ->
        map _.symbol g.types `shouldEqual` [ "p", "q" ]

    it "keeps the tokens of a precedence declaration in order" do
      withSource source \g ->
        map _.tokens g.precedences `shouldEqual` [ [ "A", "B" ] ]

  -- Every position where a grammar can name a symbol, since reserving the name
  -- in only some of them reserves nothing.
  describe "the reserved end-of-input name" do
    it "cannot be declared as a token" do
      shouldRejectWith "reserved" "%token EOF\n%%\nmain: | X { 1 }\n"

    it "cannot be given a precedence" do
      shouldRejectWith "reserved" "%token X\n%left EOF\n%%\nmain: | X { 1 }\n"

    it "cannot be declared as a start symbol" do
      shouldRejectWith "reserved"
        "%token X\n%start { Int } EOF\n%%\nmain: | X { 1 }\n"

    it "cannot be given a type" do
      shouldRejectWith "reserved"
        "%token X\n%type { Int } EOF\n%%\nmain: | X { 1 }\n"

    it "cannot be named by %prec" do
      shouldRejectWith "reserved"
        "%token X\n%%\nmain: | X %prec EOF { 1 }\n"

    it "cannot be used as a rule parameter" do
      shouldRejectWith "reserved" "%token X\n%%\npair(EOF): | X { 1 }\n"

    it "cannot be mentioned in a production" do
      shouldRejectWith "reserved" "%token X\n%%\nmain: | X EOF { 1 }\n"

    it "cannot be used as a rule name" do
      shouldRejectWith "reserved" "%token X\n%%\nEOF: | X { 1 }\n"

  describe "malformed input" do
    it "requires a semantic action on every production" do
      shouldRejectWith "semantic action" "%token X\n%%\nmain: | X\n"

    it "requires the %% separator" do
      shouldRejectWith "%%" "%token X\n"

    it "rejects an unknown directive" do
      shouldRejectWith "unexpected" "%nonsense X\n%%\nmain: | X { 1 }\n"

    -- Streams the lexer could not have produced. Without the first check the
    -- declaration loop would read the trailing identifier forever; without the
    -- second the parse would stop at the interior end marker and throw away
    -- everything after it.
    it "rejects a token stream that never reaches the end marker" do
      case parseGrammar [ located (TIdent "a") ] of
        Right _ -> fail "expected a rejection"
        Left err ->
          when (not (contains (Pattern "end-of-file") err.message)) do
            fail $ "unexpected message: " <> err.message

    it "rejects tokens left over after an interior end marker" do
      case Lexer.lex "%token X\n%%\nmain: | X { 1 }\n" of
        Left err -> fail $ "failed to lex: " <> err.message
        Right toks -> do
          let
            extended = toks <> [ located (TIdent "ignored"), located TEnd ]
          case parseGrammar extended of
            Right _ -> fail "expected a rejection"
            Left err ->
              when (not (contains (Pattern "after the end") err.message)) do
                fail $ "unexpected message: " <> err.message

  -- Every repetition in this parser accumulates through an explicit loop. The
  -- obvious spelling puts the recursive call under a bind, where it stops
  -- being a tail call and costs a stack frame per rule.
  describe "large inputs" do
    it "reads thousands of rules without a frame for each" do
      case parse (manyRules 8000) of
        Left err -> fail ("failed to parse: " <> err.message)
        Right g -> Array.length g.rules `shouldEqual` 8001

    it "reads thousands of names in one declaration" do
      case parse (manyTokens 8000) of
        Left err -> fail ("failed to parse: " <> err.message)
        Right g -> Array.length (declaredTokens g.tokens) `shouldEqual` 8000

    it "reads thousands of alternatives in one rule" do
      case parse (manyAlternatives 8000) of
        Left err -> fail ("failed to parse: " <> err.message)
        Right g -> case Array.head g.rules of
          Nothing -> fail "expected a rule"
          Just r -> Array.length r.productions `shouldEqual` 8000
