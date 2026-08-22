module Test.Puppy.Expand (spec) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.String.Common (joinWith)
import Effect.Aff (Aff)
import Puppy.Expand (expand)
import Puppy.Grammar (Action(..), Bound(..), Grammar, renderProduction)
import Puppy.Syntax.Parser as Parser
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

expanded :: String -> Either (Array String) Grammar
expanded source = case Parser.parse source of
  Left err -> Left [ "parse: " <> err.message ]
  Right syn -> case expand syn of
    Left errs -> Left (map _.message errs)
    Right g -> Right g

productionsOf :: String -> Either (Array String) (Array String)
productionsOf = map (map renderProduction <<< _.productions) <<< expanded

-- | An action as `{binder=source; ...|code}`, where a binder's source is either
-- | a right-hand side position or, nested in braces, the action of a rule that
-- | was inlined into that position.
renderAction :: Action -> String
renderAction (Action a) =
  "{" <> joinWith "; " (map binding a.bindings) <> "|" <> a.code.text <> "}"
  where
  binding b = b.name <> "=" <> case b.value of
    FromStack i -> show i
    FromAction inner -> renderAction inner

actionsOf :: String -> Either (Array String) (Array String)
actionsOf = map (map (renderAction <<< _.action) <<< _.productions) <<< expanded

-- | A grammar whose one production inlines a single-alternative rule `n` times.
-- | Nothing multiplies: the result is always one production.
chain :: Int -> String
chain n = "%token A\n%start { X } main\n%%\nmain: |"
  <> joinWith "" (map (\i -> " x" <> show i <> " = one") (Array.range 1 n))
  <> " { 1 }\n%inline one: | A { O }\n"

-- | A grammar with `n` rules, all of them reached. Instantiation walks a
-- | worklist, so this should cost no stack at all.
manyRules :: Int -> String
manyRules n = "%token P\n%start { X } main\n%%\nmain: |"
  <> joinWith "" (map (\i -> " t" <> show i) (Array.range 1 n))
  <> " { R }\n"
  <> joinWith "" (map (\i -> "t" <> show i <> ": | P { R }\n") (Array.range 1 n))

shouldFailWith :: String -> String -> Aff Unit
shouldFailWith needle source = case expanded source of
  Right _ -> fail ("expected a failure mentioning " <> show needle)
  Left messages ->
    when (not (Array.any (contains (Pattern needle)) messages)) do
      fail
        ( "expected a failure mentioning " <> show needle <> ", got: "
            <> joinWith "; " messages
        )

-- | Every needle has to be matched by some message. This is what tells apart
-- | collecting independent errors from stopping at the first one.
shouldFailWithAll :: Array String -> String -> Aff Unit
shouldFailWithAll needles source = case expanded source of
  Right _ -> fail "expected a failure"
  Left messages ->
    case Array.filter (\n -> not (Array.any (contains (Pattern n)) messages)) needles of
      [] -> pure unit
      missing ->
        fail
          ( "nothing matched " <> show missing <> "; got: "
              <> joinWith "; " messages
          )

spec :: Spec Unit
spec = describe "Puppy.Expand" do
  describe "parameterised rules" do
    it "generates one instance per distinct argument list" do
      productionsOf
        """
%token A B
%start { X } main
%%
main:
  | xs = list(A) ys = list(B)   { Pair xs ys }

list(X):
  |                      { [] }
  | x = X xs = list(X)   { x : xs }
"""
        `shouldEqual` Right
          [ "main -> list(A) list(B)"
          , "list(A) -> <empty>"
          , "list(A) -> A list(A)"
          , "list(B) -> <empty>"
          , "list(B) -> B list(B)"
          ]

    it "instantiates a rule applied to several arguments" do
      productionsOf
        """
%token COMMA A
%start { X } main
%%
main: | xs = pair(COMMA, A) { xs }
pair(sep, X): | x = X sep y = X { Two x y }
"""
        `shouldEqual` Right
          [ "main -> pair(COMMA,A)"
          , "pair(COMMA,A) -> A COMMA A"
          ]

    -- A parameter can stand for a rule that itself takes arguments, so the
    -- arguments an instance was created with and the ones written at the use
    -- site have to compose.
    it "instantiates a rule passed as a parameter" do
      productionsOf
        """
%token A
%start { X } main
%%
main: | x = apply(twice, A) { x }
apply(F, X): | y = F(X) { y }
twice(X): | a = X b = X { Two a b }
"""
        `shouldEqual` Right
          [ "main -> apply(twice,A)"
          , "apply(twice,A) -> twice(A)"
          , "twice(A) -> A A"
          ]

    it "does not generate rules nothing reaches" do
      productionsOf
        """
%token A B
%start { X } main
%%
main: | A { 1 }
unused: | B { 2 }
"""
        `shouldEqual` Right [ "main -> A" ]

  describe "%inline" do
    it "replaces the use site with each alternative" do
      productionsOf
        """
%token A B C
%start { X } main
%%
main: | x = op y = C { Use x y }
%inline op:
  | A   { OpA }
  | B   { OpB }
"""
        `shouldEqual` Right [ "main -> A C", "main -> B C" ]

    it "multiplies out when a production inlines more than one rule" do
      productionsOf
        """
%token A B
%start { X } main
%%
main: | p = ab q = ab { P p q }
%inline ab:
  | A { X }
  | B { Y }
"""
        `shouldEqual` Right
          [ "main -> A A", "main -> A B", "main -> B A", "main -> B B" ]

    -- The inlined rule is wider than the symbol it replaces, so every binder to
    -- its right has to move along by the difference.
    it "moves later positions along by the width of what was spliced in" do
      productionsOf
        """
%token A B C
%start { X } main
%%
main: | z = C x = pair { Use z x }
%inline pair: | a = A b = B { Both a b }
"""
        `shouldEqual` Right [ "main -> C A B" ]

    -- An alternative with no symbols is narrower than what it replaces, so the
    -- positions to its right move backwards. Nothing else exercises a negative
    -- shift.
    it "moves later positions back when an empty alternative is spliced in" do
      productionsOf
        """
%token A C
%start { X } main
%%
main: | x = opt y = C { Use x y }
%inline opt:
  |        { Nothing }
  | a = A  { Just a }
"""
        `shouldEqual` Right [ "main -> C", "main -> A C" ]

    it "rebinds correctly around an empty alternative" do
      actionsOf
        """
%token A C
%start { X } main
%%
main: | x = opt y = C { Use x y }
%inline opt:
  |        { Nothing }
  | a = A  { Just a }
"""
        `shouldEqual` Right
          [ "{x={| Nothing }; y=0| Use x y }"
          , "{x={a=0| Just a }; y=1| Use x y }"
          ]

    it "nests the inlined rule's action inside the binding it feeds" do
      actionsOf
        """
%token A B C
%start { X } main
%%
main: | z = C x = pair { Use z x }
%inline pair: | a = A b = B { Both a b }
"""
        `shouldEqual` Right [ "{z=0; x={a=1; b=2| Both a b }| Use z x }" ]

    it "folds a chain of inline rules" do
      actionsOf
        """
%token A
%start { X } main
%%
main: | x = outer { x }
%inline outer: | y = inner { Wrap y }
%inline inner: | a = A { Leaf a }
"""
        `shouldEqual` Right [ "{x={y={a=0| Leaf a }| Wrap y }| x }" ]

    it "carries a %prec from the inlined rule to the production it lands in" do
      map (map _.precedence <<< _.productions)
        ( expanded
            """
%token A P
%start { X } main
%left P
%%
main: | x = inl { x }
%inline inl: | A %prec P { 1 }
"""
        )
        `shouldEqual` Right [ Just "P" ]

    it "rejects an inline rule that reaches itself" do
      shouldFailWith "reaches itself"
        """
%token A
%start { X } main
%%
main: | x = loop { x }
%inline loop: | A x = loop { x }
"""

    it "rejects an inline start symbol" do
      shouldFailWith "cannot be `%inline`"
        """
%token A
%start { X } main
%%
%inline main: | A { 1 }
"""

  describe "rejected grammars" do
    it "rejects an unknown symbol" do
      shouldFailWith "unknown symbol `nope`"
        "%token A\n%start { X } main\n%%\nmain: | x = nope { x }\n"

    it "rejects the wrong number of arguments" do
      shouldFailWith "takes 1 parameter but was given 2 arguments"
        "%token A B\n%start { X } main\n%%\nmain: | x = list(A, B) { x }\nlist(X): | { [] }\n"

    it "rejects arguments applied to a token" do
      shouldFailWith "cannot take arguments"
        "%token A B\n%start { X } main\n%%\nmain: | x = A(B) { x }\n"

    it "rejects a repeated binder in one production" do
      shouldFailWith "binder `x` is declared more than once"
        "%token A B\n%start { X } main\n%%\nmain: | x = A x = B { 1 }\n"

    it "rejects a token declared twice" do
      shouldFailWith "token `A` is declared more than once"
        "%token A A\n%start { X } main\n%%\nmain: | A { 1 }\n"

    it "rejects a rule declared twice" do
      shouldFailWith "rule `main` is declared more than once"
        "%token A\n%start { X } main\n%%\nmain: | A { 1 }\nmain: | A { 2 }\n"

    it "rejects a name used as both a token and a rule" do
      shouldFailWith "declared as a token and as a rule"
        "%token main A\n%start { X } main\n%%\nmain: | A { 1 }\n"

    it "rejects a precedence on something that is not a token" do
      shouldFailWith "is given a precedence but is not a declared token"
        "%token A\n%start { X } main\n%left NOPE\n%%\nmain: | A { 1 }\n"

    it "rejects a type on something with no rule" do
      shouldFailWith "is given a type but has no rule"
        "%token A\n%start { X } main\n%type { T } nope\n%%\nmain: | A { 1 }\n"

    it "rejects %prec on something that is not a token" do
      shouldFailWith "does not name a declared token"
        "%token A\n%start { X } main\n%%\nmain: | A %prec NOPE { 1 }\n"

    it "rejects a grammar with no start symbol" do
      shouldFailWith "no `%start` symbol"
        "%token A\n%%\nmain: | A { 1 }\n"

    it "rejects a start symbol with no rule" do
      shouldFailWith "has no rule"
        "%token A\n%start { X } nope\n%%\nmain: | A { 1 }\n"

  -- "Not generated" and "not checked" are different things: a rule no start
  -- symbol reaches is still part of the grammar and still has to be well
  -- formed.
  describe "rules nothing reaches" do
    it "checks a rule the start symbol cannot reach" do
      shouldFailWith "unknown symbol `nope`"
        "%token A\n%start { X } main\n%%\nmain: | A { 1 }\nunused: | nope { 1 }\n"

    it "checks binders in a rule the start symbol cannot reach" do
      shouldFailWith "binder `x` is declared more than once"
        "%token A B\n%start { X } main\n%%\nmain: | A { 1 }\nunused: | x = A x = B { 1 }\n"

    it "checks %prec in a rule the start symbol cannot reach" do
      shouldFailWith "does not name a declared token"
        "%token A\n%start { X } main\n%%\nmain: | A { 1 }\nunused: | A %prec NOPE { 1 }\n"

    it "still keeps unreachable rules out of the output" do
      productionsOf
        "%token A B\n%start { X } main\n%%\nmain: | A { 1 }\nunused: | B { 2 }\n"
        `shouldEqual` Right [ "main -> A" ]

  describe "collecting several errors" do
    it "reports independent problems together" do
      shouldFailWithAll
        [ "unknown symbol `nope`"
        , "binder `x` is declared more than once"
        , "is given a precedence but is not a declared token"
        ]
        "%token A B\n%start { X } main\n%left MISSING\n%%\nmain: | x = A x = B { 1 }\none: | nope { 2 }\n"

    it "rejects a repeated start symbol" do
      shouldFailWith "start symbol `main` is declared more than once"
        "%token A\n%start { X } main\n%start { Y } main\n%%\nmain: | A { 1 }\n"

    it "rejects a repeated type declaration" do
      shouldFailWith "is given a type more than once"
        "%token A\n%start { X } main\n%type { T } main\n%type { U } main\n%%\nmain: | A { 1 }\n"

    it "rejects a token given a precedence twice" do
      shouldFailWith "is given a precedence more than once"
        "%token A\n%start { X } main\n%left A\n%right A\n%%\nmain: | A { 1 }\n"

  describe "bounded instantiation" do
    it "rejects a rule parameter declared twice" do
      shouldFailWith "parameter `X` is declared more than once"
        "%token A B\n%start { X } main\n%%\nmain: | x = dup(A, B) { x }\ndup(X, X): | x = X { x }\n"

    -- Each round asks for an instance with a name of its own, so remembering
    -- which names have been generated never catches up with it.
    it "stops a parameterised rule that grows its own argument" do
      shouldFailWith "nested more than"
        "%token A\n%start { X } main\n%%\nmain: | x = grow(A) { x }\ngrow(X): | y = grow(grow(X)) { y }\n"

  describe "bounded inlining" do
    -- Two rules and one start symbol, so the instance limit never sees this
    -- coming: what multiplies is the number of productions, not the number of
    -- rules.
    it "stops a production that multiplies out through repeated inlining" do
      shouldFailWith "inlining produced more than"
        ( "%token A B\n%start { X } main\n%%\nmain: |"
            <> joinWith "" (map (\i -> " x" <> show i <> " = ab") (Array.range 1 24))
            <> " { 1 }\n%inline ab: | A { X } | B { Y }\n"
        )

    it "still accepts an amount of inlining a grammar might really use" do
      map Array.length
        ( productionsOf
            ( "%token A B\n%start { X } main\n%%\nmain: |"
                <> joinWith ""
                  (map (\i -> " x" <> show i <> " = ab") (Array.range 1 8))
                <> " { 1 }\n%inline ab: | A { X } | B { Y }\n"
            )
        )
        `shouldEqual` Right 256

  -- The cycle check after instantiation only sees rules a start symbol
  -- reaches, so these have to be caught from the templates.
  describe "inline cycles nothing reaches" do
    it "rejects an unreachable rule that inlines itself" do
      shouldFailWith "reaches itself"
        "%token A\n%start { X } main\n%%\nmain: | A { 1 }\n%inline unused: | x = unused { x }\n"

    it "rejects an unreachable pair of mutually inlined rules" do
      shouldFailWith "reaches itself"
        "%token A\n%start { X } main\n%%\nmain: | A { 1 }\n%inline one: | x = two { x }\n%inline two: | x = one { x }\n"

    it "does not mistake a rule passed as an argument for a use" do
      productionsOf
        "%token A\n%start { X } main\n%%\nmain: | x = use(leaf) { x }\nuse(F): | y = F { y }\n%inline leaf: | A { L }\n"
        `shouldEqual` Right [ "main -> use(leaf)", "use(leaf) -> A" ]

  -- A single-alternative rule multiplies nothing, so the ceiling on finished
  -- productions never sees these coming. What grows is the rewriting: every
  -- splice reads and rebuilds the whole right-hand side.
  describe "bounded inline rewriting" do
    it "folds a long chain without recursing through it" do
      map Array.length (productionsOf (chain 2000)) `shouldEqual` Right 1

    it "stops a chain long enough for the rewriting to run away" do
      shouldFailWith "units of work" (chain 3000)

    -- A worklist is only worth having if the loop around it stays a tail call.
    -- Threading the result through `Either` with a `do` block quietly turns it
    -- back into recursion, one frame per rule instance.
    it "instantiates thousands of rules without a frame for each" do
      map Array.length (productionsOf (manyRules 3000)) `shouldEqual` Right 3001

  -- A grammar's names do not all end up in the same place, and each place has
  -- its own rules. Accepting one that breaks them means reporting the problem
  -- later, against generated code nobody wrote.
  describe "names that have to survive into PureScript" do
    it "rejects a token name that cannot be a constructor" do
      shouldFailWith "begin with a capital letter"
        "%token plus\n%start { X } main\n%%\nmain: | plus { 1 }\n"

    it "rejects a start symbol that cannot be a value" do
      shouldFailWith "begin with a lower case letter"
        "%token A\n%start { X } Main\n%%\nMain: | A { 1 }\n"

    it "rejects a start symbol the generated parser already uses" do
      shouldFailWith "already uses"
        "%token A\n%start { X } tableFor\n%%\ntableFor: | A { 1 }\n"

    -- Each start symbol produces two entry points, so two start symbols can
    -- want the same generated name without either being declared twice.
    it "rejects two start symbols whose entry points would collide" do
      shouldFailWith "both produce an entry point"
        "%token A\n%start { X } expr\n%start { X } exprFrom\n%%\nexpr: | A { 1 }\nexprFrom: | A { 2 }\n"

    it "says which other start symbol it collided with, and where" do
      shouldFailWith "`expr` (line 2, column 14)"
        "%token A\n%start { X } expr\n%start { X } exprFrom\n%%\nexpr: | A { 1 }\nexprFrom: | A { 2 }\n"

    -- The collision is between the names, not the order they were written in.
    it "rejects the pair the other way round too" do
      shouldFailWith "both produce an entry point"
        "%token A\n%start { X } exprFrom\n%start { X } expr\n%%\nexpr: | A { 1 }\nexprFrom: | A { 2 }\n"

    -- A start symbol ending in `From` is only a problem when something else
    -- claims the same name.
    it "accepts a start symbol ending in From on its own" do
      productionsOf "%token A\n%start { X } exprFrom\n%%\nexprFrom: | A { 1 }\n"
        `shouldEqual` Right [ "exprFrom -> A" ]

    it "rejects a binder that is a reserved word" do
      shouldFailWith "cannot be a PureScript value"
        "%token A\n%start { X } main\n%%\nmain: | let = A { 1 }\n"

    it "rejects a class it does not know how to derive" do
      shouldFailWith "knows how to derive"
        "%token A\n%derive Functor\n%start { X } main\n%%\nmain: | A { 1 }\n"

    it "rejects the same class derived twice" do
      shouldFailWith "derived more than once"
        "%token A\n%derive Eq Eq\n%start { X } main\n%%\nmain: | A { 1 }\n"

    -- `_` is a wildcard, not a name: an export list containing one does not
    -- parse.
    it "rejects a bare underscore as a start symbol" do
      shouldFailWith "cannot be a PureScript value"
        "%token A\n%start { X } _\n%%\n_: | A { 1 }\n"

    it "rejects a bare underscore as a binder" do
      shouldFailWith "cannot be a PureScript value"
        "%token A\n%start { X } main\n%%\nmain: | _ = A { 1 }\n"

    -- Names the generator uses for its own locals are no longer a problem: it
    -- keeps them under a prefix of its own.
    it "accepts a start symbol named after a generated local" do
      productionsOf "%token A\n%start { X } input\n%%\ninput: | A { 1 }\n"
        `shouldEqual` Right [ "input -> A" ]
