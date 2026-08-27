module Test.Puppy.LR.Table (spec, explainSpec) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains)
import Data.String.Common (joinWith, replaceAll)
import Data.String.Pattern (Replacement(..))
import Data.DateTime.Instant (unInstant)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Puppy.LR.Analysis (analyse)
import Puppy.LR.Automaton (Automaton, Policy(..), build)
import Puppy.LR.Explain (report)
import Puppy.LR.Grammar (LRGrammar)
import Puppy.LR.Table (Action(..), Resolution(..), Table, conflictTerminal, tabulate, unresolved)
import Test.Puppy.LR.Grammar (numbered)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

type Built =
  { g :: LRGrammar
  , automaton :: Automaton
  , table :: Table
  }

withTable :: Policy -> String -> (Built -> Aff Unit) -> Aff Unit
withTable policy source k = case numbered source of
  Left messages -> fail ("failed to number: " <> joinWith "; " messages)
  Right g -> case build policy g (analyse g) of
    Left errs -> fail ("failed to build: " <> joinWith "; " (map _.message errs))
    Right automaton -> k { g, automaton, table: tabulate g automaton }

-- | How a conflict was settled, in a shape a test can read at a glance.
classify :: Resolution -> String
classify = case _ of
  ByPrecedence (Shift _) -> "precedence: shift"
  ByPrecedence (Reduce _) -> "precedence: reduce"
  ByPrecedence Accept -> "precedence: accept"
  ByShift -> "shift: shift"
  ByNonassoc -> "nonassoc: error"
  ByDefault (Shift _) -> "default: shift"
  ByDefault (Reduce _) -> "default: reduce"
  ByDefault Accept -> "default: accept"

terminalNamed :: LRGrammar -> String -> Int
terminalNamed g name =
  fromMaybe (-1) (Array.findIndex (\t -> t.name == name) g.terminals)

settlements :: Built -> Array String
settlements built = map (classify <<< _.resolution) built.table.conflicts

-- | The one grammar the associativity tests use, with whatever declaration is
-- | put in front of it. Only `PLUS` can ever be in conflict, so each of these
-- | produces exactly one.
plusGrammar :: String -> String
plusGrammar declaration =
  "%token PLUS INT\n%start { E } main\n" <> declaration
    <> "%%\nmain: | e = expr { e }\n"
    <> "expr: | i = INT { L } | a = expr PLUS b = expr { A }\n"

levels :: String
levels =
  """
%token PLUS TIMES INT
%start { E } main
%left PLUS
%left TIMES
%%
main: | e = expr { e }
expr:
  | i = INT                 { L }
  | a = expr PLUS b = expr  { A }
  | a = expr TIMES b = expr { M }
"""

-- The same grammar, except that the `TIMES` production borrows the precedence
-- of `PLUS`. That drops it below a pending `TIMES`, which turns one settlement
-- from a reduce into a shift.
borrowed :: String
borrowed =
  """
%token PLUS TIMES INT
%start { E } main
%left PLUS
%left TIMES
%%
main: | e = expr { e }
expr:
  | i = INT                            { L }
  | a = expr PLUS b = expr             { A }
  | a = expr TIMES b = expr %prec PLUS { M }
"""

-- One cell wanted by a shift and two reduces at once. Settling them a pair at
-- a time makes the answer depend on the order they arrive in.
duplicated :: String
duplicated =
  """
%token PLUS INT
%start { E } main
%nonassoc PLUS
%%
main: | expr { 0 }
expr:
  | INT            { 0 }
  | expr PLUS expr { 1 }
  | expr PLUS expr { 2 }
"""

-- A start symbol that derives itself: once the input is complete, finishing
-- and reducing once more look the same.
recursiveStart :: String
recursiveStart = "%token A\n%start { S } s\n%%\ns: | x = s { R } | A { R }\n"

-- The same shape as `duplicated`, but with a declaration that lets the shift
-- beat every one of the reduces.
duplicatedRight :: String
duplicatedRight =
  """
%token PLUS INT
%start { E } main
%right PLUS
%%
main: | expr { 0 }
expr:
  | INT            { 0 }
  | expr PLUS expr { 1 }
  | expr PLUS expr { 2 }
"""

-- Two productions whose `%prec` declarations point in opposite directions: one
-- sits below `PLUS` and so loses to the shift, the other above it and so wins.
-- Nothing is missing here; the declarations simply disagree.
disputed :: String
disputed =
  """
%token LOW PLUS HIGH INT
%start { E } main
%left LOW
%left PLUS
%left HIGH
%%
main: | expr { 0 }
expr:
  | INT                       { 0 }
  | expr PLUS expr %prec LOW  { 1 }
  | expr PLUS expr %prec HIGH { 2 }
"""

-- Independent ambiguous rules, so that the number of conflicts and the size of
-- the automaton both grow with `n`.
ambiguous :: Int -> String
ambiguous n =
  "%token PLUS INT\n%start { E } main\n%%\nmain:\n"
    <> joinWith "" (map (\i -> "  | e" <> show i <> " { R }\n") (Array.range 1 n))
    <> joinWith ""
      ( map
          ( \i -> "e" <> show i <> ": | INT { R } | e" <> show i <> " PLUS e"
              <> show i
              <> " { R }\n"
          )
          (Array.range 1 n)
      )

crossed :: String
crossed =
  """
%token A B C D E
%start { S } s
%%
s: | A x = e C { P } | A x = f D { P } | B x = f C { P } | B x = e D { P }
e: | E { Ee }
f: | E { Ff }
"""

-- | The same grammar with `%shift` on the production that is in conflict.
markShift :: String -> String
markShift = replaceAll (Pattern "b = expr { A }") (Replacement "b = expr %shift { A }")

-- | `%shift` on everything, for showing where it has no effect.
markEvery :: String -> String
markEvery = replaceAll (Pattern "{ Ee }") (Replacement "%shift { Ee }")
  <<< replaceAll (Pattern "{ Ff }") (Replacement "%shift { Ff }")

-- | One cell wanted by a shift and by two reductions, one of which asked to
-- | lose and the other of which said nothing.
-- |
-- | After reading `A` the parser is holding three items at once: `main -> A .
-- | T`, `one -> A .` and `two -> A .`, and `T` is a lookahead of both
-- | reductions. So the cell on `T` has all three wanting it.
sharedCell :: String
sharedCell =
  """
%token A T
%start { X } main
%%
main:
  | p = one T { p }
  | q = two T { q }
  | A T       { 0 }
one: | A %shift { 1 }
two: | A { 2 }
"""

-- | The same cell again, with the reduction that said nothing given a
-- | precedence that wants it to win. `A` outranks `T`, so `two -> A` beats a
-- | pending `T`, while `one -> A` has asked to lose to it.
disputedShift :: String
disputedShift =
  """
%token A T
%start { X } main
%left T
%left A
%%
main:
  | p = one T { p }
  | q = two T { q }
  | A T       { 0 }
one: | A %shift { 1 }
two: | A { 2 }
"""

-- | A grammar where `ERROR` is not on the path at all: it is inside the
-- | shortest input that matches the nonterminals the path goes through.
hiddenError :: String
hiddenError =
  """
%token A B
%start { X } p
%%
p:
  | x = q B { x }
  | x = r B { x }
q: | x = s { x }
r: | x = s { x }
s: | ERROR { 1 }
"""

-- | A grammar whose conflicting states are reached through two recoveries.
twiceRecovered :: String
twiceRecovered =
  """
%token A B
%start { X } p
%%
p:
  | x = q { x }
  | x = r { x }
q: | ERROR A ERROR B { 1 }
r: | ERROR A ERROR B { 2 }
"""

spec :: Spec Unit
spec = describe "Puppy.LR.Table" do
  it "accepts once the start symbol has been read and nothing is left" do
    withTable Pager (plusGrammar "") \built ->
      Array.any (_ == Accept) (Array.fromFoldable (Map.values built.table.action))
        `shouldEqual` true

  describe "settling a shift against a reduce" do
    it "prefers the shift when nothing says otherwise" do
      withTable Pager (plusGrammar "") \built ->
        settlements built `shouldEqual` [ "default: shift" ]

    it "reduces on a left-associative token" do
      withTable Pager (plusGrammar "%left PLUS\n") \built ->
        settlements built `shouldEqual` [ "precedence: reduce" ]

    it "shifts on a right-associative token" do
      withTable Pager (plusGrammar "%right PLUS\n") \built ->
        settlements built `shouldEqual` [ "precedence: shift" ]

    it "makes the token an error where it is non-associative" do
      withTable Pager (plusGrammar "%nonassoc PLUS\n") \built -> do
        settlements built `shouldEqual` [ "nonassoc: error" ]
        -- Not merely resolved: there is no action left at all.
        map (actionAt built) built.table.conflicts `shouldEqual` [ Nothing ]

    -- `TIMES` is declared after `PLUS`, so it binds more tightly: a pending
    -- `TIMES` outranks a production whose precedence came from `PLUS`, and a
    -- production whose precedence came from `TIMES` outranks a pending `PLUS`.
    it "lets a later declaration outrank an earlier one" do
      withTable Pager levels \built ->
        Array.sort (settlements built) `shouldEqual`
          [ "precedence: reduce"
          , "precedence: reduce"
          , "precedence: reduce"
          , "precedence: shift"
          ]

    it "lets %prec override the precedence a production would have had" do
      withTable Pager borrowed \built ->
        Array.sort (settlements built) `shouldEqual`
          [ "precedence: reduce"
          , "precedence: reduce"
          , "precedence: shift"
          , "precedence: shift"
          ]

    it "leaves nothing unresolved once precedence covers the grammar" do
      withTable Pager levels \built ->
        Array.length (unresolved built.table) `shouldEqual` 0

    -- When a shift faces several reduces, the reduces may never get to compete
    -- with each other: if the token belongs to none of them, no input reaches
    -- the point where the choice between them would matter.
    it "settles a cell wanted by a shift and more than one reduce" do
      withTable Pager duplicated \built ->
        Array.sort (settlements built) `shouldEqual`
          [ "default: reduce", "nonassoc: error", "nonassoc: error" ]

    it "leaves no action where %nonassoc settled a many-sided cell" do
      withTable Pager duplicated \built ->
        map (actionAt built) (on built "PLUS") `shouldEqual` [ Nothing, Nothing ]

    it "does not call a deliberately settled cell unresolved" do
      withTable Pager duplicated \built ->
        Array.length (on built "PLUS" # Array.filter (isUnresolved built))
          `shouldEqual` 0

    -- The one place the two productions really do compete: nothing is shifted
    -- at the end of the input, so there the choice between them is genuine.
    it "still reports the reduce/reduce where the reduces do compete" do
      withTable Pager duplicated \built ->
        map (classify <<< _.resolution) (unresolved built.table)
          `shouldEqual` [ "default: reduce" ]

    it "does not claim a reduce the table does not have" do
      withTable Pager duplicated \built ->
        Array.any (contains (Pattern "on `PLUS`"))
          (report built.g built.automaton built.table) `shouldEqual` false

    -- Every production here has a `%prec`. What is unsettled is that they
    -- point different ways, which is a different complaint from "nothing
    -- says which to prefer".
    it "treats a cell whose precedences disagree as one conflict" do
      withTable Pager disputed \built ->
        map (classify <<< _.resolution) (on built "PLUS")
          `shouldEqual` [ "default: shift" ]

    it "falls back to the shift when the precedences disagree" do
      withTable Pager disputed \built ->
        Array.all (isShift <<< actionAt built) (on built "PLUS")
          `shouldEqual` true

    it "lets the shift take a many-sided cell when it beats every reduce" do
      withTable Pager duplicatedRight \built -> do
        Array.sort (settlements built) `shouldEqual`
          [ "default: reduce", "precedence: shift", "precedence: shift" ]
        Array.all (isShift <<< actionAt built) (on built "PLUS")
          `shouldEqual` true
        Array.length (on built "PLUS" # Array.filter (isUnresolved built))
          `shouldEqual` 0

  describe "settling a shift against a reduce marked `%shift`" do
    it "settles it, and settles it as the shift" do
      withTable Pager (plusGrammar "" # markShift) \built -> do
        settlements built `shouldEqual` [ "shift: shift" ]
        map (isShift <<< actionAt built) built.table.conflicts
          `shouldEqual` [ true ]

    -- The whole point: the conflict is still there and still reported, it is
    -- just no longer something the grammar failed to say.
    it "keeps the conflict, and takes it out of `unresolved`" do
      withTable Pager (plusGrammar "" # markShift) \built -> do
        Array.length built.table.conflicts `shouldEqual` 1
        unresolved built.table `shouldEqual` []

    it "changes nothing about what the parser does" do
      withTable Pager (plusGrammar "") \plain ->
        withTable Pager (plusGrammar "" # markShift) \marked ->
          marked.table.action `shouldEqual` plain.table.action

    -- `%left PLUS` would give this production the precedence of its last
    -- terminal and settle the cell as a reduce. `%shift` says the production
    -- has no precedence at all, so it loses -- which is the whole of what
    -- asking to lose means.
    it "gives up the precedence the production would have inherited" do
      withTable Pager (plusGrammar "%left PLUS\n") \plain ->
        withTable Pager (plusGrammar "%left PLUS\n" # markShift) \marked -> do
          settlements plain `shouldEqual` [ "precedence: reduce" ]
          settlements marked `shouldEqual` [ "shift: shift" ]

    -- Two reductions in one cell, one asking to lose and one whose precedence
    -- says it should win. That is a disagreement, and it is reported as one.
    it "disagrees with a neighbour whose precedence wants the reduce" do
      withTable Pager disputedShift \built -> do
        settlements built `shouldEqual` [ "default: shift" ]
        Array.length (unresolved built.table) `shouldEqual` 1

    it "does not cover a neighbour that said nothing" do
      withTable Pager sharedCell \built -> do
        Array.sort (settlements built) `shouldEqual`
          [ "default: shift", "shift: shift" ]
        Array.length (unresolved built.table) `shouldEqual` 1

    it "has nothing to say about a reduce/reduce conflict" do
      withTable LALR (markEvery crossed) \built ->
        settlements built `shouldEqual` [ "default: reduce", "default: reduce" ]

  describe "settling a reduce against a reduce" do
    -- The same grammar that told canonical from LALR in the automaton tests,
    -- now seen from the other end: the merge LALR makes is exactly a pair of
    -- reductions landing in one cell.
    it "is what merging too eagerly costs" do
      withTable LALR crossed \built ->
        settlements built `shouldEqual` [ "default: reduce", "default: reduce" ]

    it "does not arise when merging is done carefully" do
      withTable Pager crossed \built ->
        settlements built `shouldEqual` []
  where
  on built name = Array.filter
    (\c -> conflictTerminal c == terminalNamed built.g name)
    built.table.conflicts

  actionAt built c = Map.lookup
    { state: c.state, terminal: conflictTerminal c }
    built.table.action

  isUnresolved built c = Array.elem c (unresolved built.table)

  isShift = case _ of
    Just (Shift _) -> true
    _ -> false

explainSpec :: Spec Unit
explainSpec = describe "Puppy.LR.Explain" do
  -- `ERROR` is not a token anybody typed, so a path that reaches a state
  -- through one is not an input and is not written out as one.
  describe "a path that goes through a recovery" do
    it "finds an ERROR inside the shortest derivation of a nonterminal" do
      -- Nothing on the path is `ERROR`. It is inside the shortest input that
      -- matches `q` and `r`, which is what the path is spelled out through.
      withTable Pager hiddenError \built ->
        case Array.head (report built.g built.automaton built.table) of
          Nothing -> fail "expected a conflict to explain"
          Just text -> do
            mentions text "then recovering from a syntax error"
            notMentions text "a syntax error B"

    it "splits at every ERROR, not only the first" do
      withTable Pager twiceRecovered \built ->
        case Array.head (report built.g built.automaton built.table) of
          Nothing -> fail "expected a conflict to explain"
          Just text -> do
            mentions text "then recovering from a syntax error and reading   A"
            mentions text "then recovering from a syntax error and reading   B"

  it "says where the parser is, in tokens rather than state numbers" do
    withTable Pager (plusGrammar "") \built ->
      case Array.head (report built.g built.automaton built.table) of
        Nothing -> fail "expected a conflict to explain"
        Just text -> do
          mentions text "shift/reduce conflict"
          mentions text "after reading   INT PLUS INT"
          mentions text "and seeing      PLUS"
          mentions text "expr -> expr . PLUS expr"
          mentions text "expr -> expr PLUS expr"
          mentions text "the parser will shift the token"

  it "names the two rules a reduce/reduce conflict is torn between" do
    withTable LALR crossed \built ->
      case Array.head (report built.g built.automaton built.table) of
        Nothing -> fail "expected a conflict to explain"
        Just text -> do
          mentions text "reduce/reduce conflict"
          mentions text "after reading   A E"
          mentions text "e -> E"
          mentions text "f -> E"
          mentions text "Precedence cannot settle"

  it "has nothing to say about a grammar whose precedence covers it" do
    withTable Pager levels \built ->
      report built.g built.automaton built.table `shouldEqual` []

  it "explains an accept/reduce conflict in its own terms" do
    withTable Pager recursiveStart \built ->
      case Array.head (report built.g built.automaton built.table) of
        Nothing -> fail "expected a conflict to explain"
        Just text -> do
          mentions text "accept/reduce conflict"
          mentions text "start symbol from itself"
          when (contains (Pattern "reduce/reduce conflict") text) do
            fail ("an accept/reduce conflict explained as reduce/reduce:\n" <> text)

  -- Timed by hand rather than left to the spec timeout: this is a pure,
  -- synchronous computation, so it blocks the event loop and the Aff timer
  -- cannot fire while it runs.
  it "explains a thousand conflicts without redoing the shared work" do
    withTable Pager (ambiguous 400) \built -> do
      Milliseconds start <- liftEffect (unInstant <$> now)
      let texts = report built.g built.automaton built.table
      Milliseconds end <- liftEffect (unInstant <$> now)
      (Array.length texts > 1000) `shouldEqual` true
      let elapsed = end - start
      when (elapsed >= 1000.0) do
        fail
          ( "explaining " <> show (Array.length texts) <> " conflicts took "
              <> show elapsed
              <> "ms; the shortest derivations and the path to each state are"
              <> " probably being recomputed for every one of them"
          )

  -- The complaint here is not that something is missing. Both productions say
  -- what they want; they want different things.
  it "explains disagreeing precedences as a disagreement" do
    withTable Pager disputed \built ->
      case
        Array.find (contains (Pattern "on `PLUS`"))
          (report built.g built.automaton built.table)
        of
        Nothing -> fail "expected a conflict on PLUS to explain"
        Just text -> do
          mentions text "%prec LOW"
          mentions text "%prec HIGH"
          mentions text "its precedence prefers the shift"
          mentions text "its precedence prefers this reduction"
          mentions text "do not"
          when (contains (Pattern "Nothing in the grammar says") text) do
            fail ("disagreeing precedences reported as absent ones:\n" <> text)
  where
  mentions text needle =
    when (not (contains (Pattern needle) text)) do
      fail ("expected the report to mention " <> show needle <> ", got:\n" <> text)

-- | Reading a conflict report, which is text and is meant to be.
mentions :: String -> String -> Aff Unit
mentions text needle =
  when (not (contains (Pattern needle) text)) do
    fail ("expected the report to mention " <> show needle <> ", got:\n" <> text)

notMentions :: String -> String -> Aff Unit
notMentions text needle =
  when (contains (Pattern needle) text) do
    fail ("expected the report not to mention " <> show needle <> ", got:\n" <> text)
