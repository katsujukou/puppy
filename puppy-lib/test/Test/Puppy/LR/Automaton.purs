module Test.Puppy.LR.Automaton (spec, mergeSpec, invariantSpec) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.DateTime.Instant (unInstant)
import Data.Foldable (traverse_)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Now (now)
import Data.Traversable (traverse)
import Data.String.Common (joinWith)
import Effect.Aff (Aff)
import Puppy.LR.Analysis (analyse)
import Puppy.LR.Automaton (Automaton, Policy(..), build)
import Puppy.LR.Item (renderItems)
import Puppy.LR.Grammar (LRGrammar, Sym(..))
import Test.Puppy.LR.Grammar (numbered)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

withAutomaton :: String -> (LRGrammar -> Automaton -> Aff Unit) -> Aff Unit
withAutomaton source k = case numbered source of
  Left messages -> fail ("failed to number: " <> joinWith "; " messages)
  Right g -> case build Canonical g (analyse g) of
    Left errs -> fail ("failed to build: " <> joinWith "; " (map _.message errs))
    Right automaton -> k g automaton

-- The textbook grammar for telling canonical LR(1) from LALR: `S -> C C`,
-- `C -> c C | d`. Canonical keeps ten states; merging states that differ only
-- in their lookaheads leaves seven.
twoCs :: String
twoCs =
  """
%token C D
%start { S } s
%%
s: | x = cs y = cs { Pair x y }
cs:
  | C x = cs { More x }
  | D        { End }
"""

arithmetic :: String
arithmetic =
  """
%token PLUS INT
%start { E } main
%%
main: | e = expr { e }
expr:
  | i = INT                 { Lit i }
  | a = expr PLUS b = expr  { Add a b }
"""

spec :: Spec Unit
spec = describe "Puppy.LR.Automaton" do
  it "starts from an item that is about to read the start symbol" do
    withAutomaton twoCs \g automaton -> case Array.head automaton.initial of
      Nothing -> fail "expected an entry state"
      Just entry -> case Array.index automaton.states entry of
        Nothing -> fail "entry state is not in the automaton"
        Just state -> renderItems g state.kernel
          `shouldEqual` [ "<start s> -> . s  [EOF]" ]

  it "closes the entry state over everything reachable without reading input" do
    withAutomaton twoCs \g automaton -> case Array.head automaton.initial of
      Nothing -> fail "expected an entry state"
      Just entry -> case Array.index automaton.states entry of
        Nothing -> fail "entry state is not in the automaton"
        Just state -> renderItems g state.items `shouldEqual`
          [ "s -> . cs cs  [EOF]"
          , "cs -> . C cs  [C D]"
          , "cs -> . D  [C D]"
          , "<start s> -> . s  [EOF]"
          ]

  -- Merging on cores alone would give seven. Ten is the number that says no
  -- state was merged with another.
  it "keeps states that differ only in their lookaheads apart" do
    withAutomaton twoCs \_ automaton ->
      Array.length automaton.states `shouldEqual` 10

  it "reaches an accepting item by way of the start symbol" do
    withAutomaton twoCs \g automaton ->
      case Array.head g.starts, Array.head automaton.initial of
        Just start, Just entry -> case Array.index automaton.states entry of
          Nothing -> fail "entry state is not in the automaton"
          Just state -> case Map.lookup (N start.symbol) state.transitions of
            Nothing -> fail "no transition on the start symbol"
            Just target -> case Array.index automaton.states target of
              Nothing -> fail "the transition leads nowhere"
              Just accepting -> renderItems g accepting.kernel
                `shouldEqual` [ "<start s> -> s .  [EOF]" ]
        _, _ -> fail "expected a start symbol and an entry state"

  it "builds a state for every viable prefix of a small grammar" do
    withAutomaton arithmetic \_ automaton ->
      Array.length automaton.states `shouldEqual` 6

-- The textbook grammar that LALR gets wrong while LR(1) does not. After `A E`
-- the lookahead decides between `e -> E` and `f -> E`; after `B E` it decides
-- the other way round. Merging those two states puts both reductions on both
-- tokens, which is a conflict neither state had.
crossed :: String
crossed =
  """
%token A B C D E
%start { S } s
%%
s:
  | A x = e C { P x }
  | A x = f D { P x }
  | B x = f C { P x }
  | B x = e D { P x }
e: | E { Ee }
f: | E { Ff }
"""

sizes :: String -> (Sizes -> Aff Unit) -> Aff Unit
sizes source k = case numbered source of
  Left messages -> fail ("failed to number: " <> joinWith "; " messages)
  Right g -> case traverse (\p -> build p g (analyse g)) [ Canonical, Pager, LALR ] of
    Left errs -> fail ("failed to build: " <> joinWith "; " (map _.message errs))
    Right built -> case built of
      [ c, p, l ] -> k
        { canonical: Array.length c.states
        , pager: Array.length p.states
        , lalr: Array.length l.states
        }
      _ -> fail "expected three automata"

type Sizes = { canonical :: Int, pager :: Int, lalr :: Int }

mergeSpec :: Spec Unit
mergeSpec = describe "Puppy.LR.Automaton merging" do
  -- Every state here that could be merged has a single-item kernel, so there
  -- is no pair of positions to cross: merging is safe and Pager takes it.
  it "merges when merging cannot invent a conflict" do
    sizes twoCs \s -> s `shouldEqual` { canonical: 10, pager: 7, lalr: 7 }

  it "refuses to merge when merging would invent a conflict" do
    sizes crossed \s -> s `shouldEqual` { canonical: 14, pager: 14, lalr: 13 }

-- | Every state the automaton returns, reached by following transitions from
-- | the entry states.
reached :: Automaton -> Int
reached aut = Set.size
  (walk (List.fromFoldable aut.initial) (Set.fromFoldable aut.initial))
  where
  walk pending seen = case pending of
    Nil -> seen
    Cons s rest -> case Array.index aut.states s of
      Nothing -> walk rest seen
      Just state ->
        let
          fresh = Array.filter (\t -> not (Set.member t seen))
            (Array.fromFoldable (Map.values state.transitions))
        in
          walk (List.fromFoldable fresh <> rest)
            (Array.foldl (flip Set.insert) seen fresh)

-- | A state that nothing can reach is a state that was built and then stranded.
-- | Merging is where that can happen: if a state that has gained lookaheads
-- | went looking for a new home for its successors, the old ones would be left
-- | with no edge pointing at them.
allReachable :: String -> Aff Unit
allReachable source = case numbered source of
  Left messages -> fail ("failed to number: " <> joinWith "; " messages)
  Right g ->
    case traverse (\p -> build p g (analyse g)) [ Canonical, Pager, LALR ] of
      Left errs -> fail ("failed to build: " <> joinWith "; " (map _.message errs))
      Right built -> traverse_
        (\aut -> reached aut `shouldEqual` Array.length aut.states)
        built

-- | A grammar whose alternatives share a prefix, leaving one state holding `n`
-- | positions. Weak compatibility looks at every pair of them.
wide :: Int -> String
wide n =
  "%token A P C D\n%start { X } main\n%%\n"
    <> "main: | C x = u { R } | D x = u { R }\nu:\n"
    <> joinWith "" (map (\i -> "  | A t" <> show i <> " { R }\n") (Array.range 1 n))
    <> joinWith "" (map (\i -> "t" <> show i <> ": | P { R }\n") (Array.range 1 n))

wideSize :: Int
wideSize = 1500

-- Generous next to the ~700ms this takes, tight next to the seconds a
-- quadratic number of allocated pairs took.
wideBudgetMs :: Number
wideBudgetMs = 4000.0

-- Reduced from a search over generated grammars. Under Pager, one state used
-- to be left with no edge pointing at it: a state gained lookaheads, was
-- reprocessed, and its successor was sent to a different home than the one the
-- edge already led to.
stranded :: String
stranded =
  """
%token A B C D
%start { X } n0
%%
n0:
  | B             { 0 }
  | B n4 n0 n3    { 0 }
n1:
  | C             { 0 }
  | D n2          { 0 }
n2:
  | n0            { 0 }
  |               { 0 }
n3:
  | C             { 0 }
  | n1 n0         { 0 }
n4:
  |               { 0 }
  | n3 A          { 0 }
  | n1 n1         { 0 }
"""

invariantSpec :: Spec Unit
invariantSpec = describe "Puppy.LR.Automaton invariants" do
  it "leaves no state stranded, under any policy" do
    allReachable twoCs
    allReachable crossed
    allReachable arithmetic
    allReachable stranded

  -- Timed by hand rather than left to the spec timeout: building is a pure,
  -- synchronous computation, so it blocks the event loop and the Aff timer
  -- cannot fire while it runs.
  it "tests compatibility over a wide state without materialising the pairs" do
    case numbered (wide wideSize) of
      Left messages -> fail ("failed to number: " <> joinWith "; " messages)
      Right g -> do
        Milliseconds start <- liftEffect (unInstant <$> now)
        let built = build Pager g (analyse g)
        Milliseconds end <- liftEffect (unInstant <$> now)
        case built of
          Left errs -> fail ("failed to build: " <> joinWith "; " (map _.message errs))
          Right aut -> do
            Array.length aut.states `shouldEqual` (wideSize + 8)
            let elapsed = end - start
            when (elapsed >= wideBudgetMs) do
              fail
                ( "building over a state with " <> show wideSize
                    <> " positions took "
                    <> show elapsed
                    <> "ms, over the "
                    <> show wideBudgetMs
                    <> "ms budget"
                )
