-- | Saying what a conflict is, in terms of the grammar rather than the
-- | automaton.
-- |
-- | A state number tells whoever wrote the grammar nothing. What tells them
-- | something is an input that reaches the disagreement: a short concrete
-- | sequence of tokens that gets the parser into the state, the token that then
-- | arrives, and the two things it could do with it.
-- |
-- | "Short" is worth being exact about. The path taken is one with the fewest
-- | transitions, and each nonterminal on it is then replaced by the shortest
-- | input that matches it. The result is not necessarily the shortest input
-- | reaching the state -- a longer path through cheaper symbols could beat it
-- | -- but it is short, and it is the same one every time.
-- |
-- | What this does not do is show two complete derivations of one sentence, the
-- | way Menhir's `--explain` can. It shows where the parser is and what it is
-- | torn between, which is enough to act on in most cases.
module Puppy.LR.Explain
  ( Trail
  , Context
  , context
  , explain
  , explainWith
  , report
  , pathTo
  , shortestDerivations
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String.Common (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Puppy.LR.Automaton (Automaton)
import Puppy.LR.Grammar (LRGrammar, Sym(..))
import Puppy.LR.Item (Item)
import Puppy.LR.Table
  ( Action(..)
  , Conflict
  , ConflictKind(..)
  , Preference(..)
  , Resolution(..)
  , Table
  , unresolved
  )

--------------------------------------------------------------------------------
-- Everything that is the same for every conflict
--------------------------------------------------------------------------------

-- | The step that first reached a state, and where it came from.
type Trail = { from :: Int, via :: Sym }

-- | Work that does not depend on which conflict is being explained.
-- |
-- | Both halves cost a pass over the whole grammar or the whole automaton, and
-- | a grammar with a thousand conflicts is exactly the kind that has a large
-- | automaton. Doing them once is the difference between a report and a wait.
type Context =
  { derivations :: Map Int (Array Int)
  , trail :: Map Int Trail
  , entries :: Set Int
  }

context :: LRGrammar -> Automaton -> Context
context g automaton =
  { derivations: shortestDerivations g
  , trail: trails automaton
  , entries: Set.fromFoldable automaton.initial
  }

-- | One breadth-first sweep, recording for every state the step that reached
-- | it first. Because the sweep is breadth first, that step lies on a path with
-- | the fewest transitions.
trails :: Automaton -> Map Int Trail
trails automaton =
  walk automaton.initial (Set.fromFoldable automaton.initial) Map.empty
  where
  walk frontier seen tree
    | Array.null frontier = tree
    | otherwise =
        let
          fresh = keepNew seen (Array.concatMap step frontier)
        in
          walk (map _.state fresh)
            (Array.foldl (\s e -> Set.insert e.state s) seen fresh)
            ( Array.foldl
                (\t e -> Map.insert e.state { from: e.from, via: e.via } t)
                tree
                fresh
            )

  step state = case Array.index automaton.states state of
    Nothing -> []
    Just st -> map
      (\(Tuple via to) -> { state: to, from: state, via })
      (Map.toUnfoldable st.transitions :: Array (Tuple Sym Int))

  keepNew seen = go seen []
    where
    go known acc items = case Array.uncons items of
      Nothing -> acc
      Just { head, tail }
        | Set.member head.state known -> go known acc tail
        | otherwise -> go (Set.insert head.state known) (Array.snoc acc head) tail

-- | Walk back up the trail to an entry state.
pathTo :: Context -> Int -> Maybe (Array Sym)
pathTo ctx = go []
  where
  go acc state
    | Set.member state ctx.entries = Just acc
    | otherwise = case Map.lookup state ctx.trail of
        Nothing -> Nothing
        Just step -> go (Array.cons step.via acc) step.from

-- | For each nonterminal, the shortest sequence of tokens it derives.
-- |
-- | A nonterminal with no entry derives nothing at all, which
-- | `Puppy.LR.Grammar` rejects before it gets this far.
shortestDerivations :: LRGrammar -> Map Int (Array Int)
shortestDerivations g = fixpoint Map.empty
  where
  fixpoint known =
    let
      grown = Array.foldl step known g.productions
    in
      if grown == known then known else fixpoint grown

  step known p = case expand known p.rhs of
    Nothing -> known
    Just tokens -> case Map.lookup p.lhs known of
      Just existing | Array.length existing <= Array.length tokens -> known
      _ -> Map.insert p.lhs tokens known

  expand known rhs = map Array.concat (traverse one rhs)
    where
    one = case _ of
      T t -> Just [ t ]
      N n -> Map.lookup n known

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

terminalName :: LRGrammar -> Int -> String
terminalName g t =
  fromMaybe ("token " <> show t) (map _.display (Array.index g.terminals t))

symbolName :: LRGrammar -> Sym -> String
symbolName g = case _ of
  T t -> fromMaybe ("token " <> show t) (map _.name (Array.index g.terminals t))
  N n -> fromMaybe ("nonterminal " <> show n) (Array.index g.nonterminals n)

-- | A production with a dot marking how far the parser has got through it.
dotted :: LRGrammar -> Int -> Int -> String
dotted g production dot = case Array.index g.productions production of
  Nothing -> "?"
  Just p ->
    fromMaybe "?" (Array.index g.nonterminals p.lhs) <> " -> "
      <> joinWith " "
        ( map (symbolName g) (Array.take dot p.rhs)
            <> [ "." ]
            <> map (symbolName g) (Array.drop dot p.rhs)
        )

whole :: LRGrammar -> Int -> String
whole g production = case Array.index g.productions production of
  Nothing -> "?"
  Just p ->
    fromMaybe "?" (Array.index g.nonterminals p.lhs) <> " -> "
      <>
        if Array.null p.rhs then "<empty>"
        else joinWith " " (map (symbolName g) p.rhs)

-- | A path written as the tokens a parser would actually have read: every
-- | nonterminal on the way replaced by the shortest input that matches it.
spellOut :: LRGrammar -> Context -> Array Sym -> String
spellOut g ctx path =
  if Array.null tokens then "nothing"
  else joinWith " " (map (terminalName g) tokens)
  where
  tokens = Array.concatMap expand path

  expand = case _ of
    T t -> [ t ]
    N n -> fromMaybe [] (Map.lookup n ctx.derivations)

-- | The item in this state that is waiting to shift the given terminal.
shiftingItem :: LRGrammar -> Automaton -> Int -> Int -> Maybe Item
shiftingItem g automaton state terminal = do
  st <- Array.index automaton.states state
  Array.find waitingFor
    ( map (\(Tuple item _) -> item)
        (Map.toUnfoldable st.items :: Array (Tuple Item (Set Int)))
    )
  where
  waitingFor item = case Array.index g.productions item.production of
    Nothing -> false
    Just p -> Array.index p.rhs item.dot == Just (T terminal)

-- | Explain one conflict on its own. Use `explainWith` when there are several.
explain :: LRGrammar -> Automaton -> Conflict -> String
explain g automaton = explainWith g automaton (context g automaton)

explainWith :: LRGrammar -> Automaton -> Context -> Conflict -> String
explainWith g automaton ctx conflict = case conflict.kind of
  ShiftReduce c ->
    heading "shift/reduce" c.terminal
      <> situation c.terminal
      <> "\n  the parser can shift it, continuing\n      "
      <> shifted c.terminal
      <> "\n  or reduce, having read the whole of\n      "
      <> withPrec c.production
      <> "\n\n  "
      <> outcome
  Disputed c ->
    heading "shift/reduce" c.terminal
      <> situation c.terminal
      <> "\n  the parser can shift it, continuing\n      "
      <> shifted c.terminal
      <> "\n  or reduce by one of these, whose precedence declarations do not\n  agree with one another:\n"
      <> joinWith "\n" (map verdict c.verdicts)
      <> "\n\n  Because they disagree, nothing settles the cell, and the parser will\n  shift the token. Precedences that agree -- or the same `%prec` on each --\n  would settle it."
  ReduceReduce c ->
    heading "reduce/reduce" c.terminal
      <> situation c.terminal
      <> "\n  the parser could reduce either\n      "
      <> withPrec c.kept
      <> "\n  or\n      "
      <> withPrec c.dropped
      <> "\n\n  "
      <> outcome
  AcceptReduce c ->
    heading "accept/reduce" c.terminal
      <> situation c.terminal
      <> "\n  the parser could accept the input, or reduce\n      "
      <> withPrec c.production
      <> "\n\n  "
      <> outcome
  where
  heading kind terminal =
    kind <> " conflict in state " <> show conflict.state <> ", on `"
      <> terminalName g terminal
      <> "`.\n\n"

  situation terminal =
    "  after reading   "
      <> fromMaybe "<no path to this state>"
        (map (spellOut g ctx) (pathTo ctx conflict.state))
      <> "\n  and seeing      "
      <> terminalName g terminal
      <> "\n"

  verdict v = "      " <> withPrec v.production <> "\n          " <> case v.prefers of
    PrefersShift -> "its precedence prefers the shift"
    PrefersReduce -> "its precedence prefers this reduction"
    PrefersError -> "its precedence makes the token an error here"
    NoPreference -> "it has no precedence"

  -- The `%prec` is what tells two otherwise identical productions apart, which
  -- is exactly the situation a disputed cell tends to be.
  withPrec production = whole g production <> case declared of
    Nothing -> ""
    Just t -> " %prec " <> symbolName g (T t)
    where
    declared = Array.index g.productions production >>= _.precedence

  shifted terminal = case shiftingItem g automaton conflict.state terminal of
    Just item -> dotted g item.production item.dot
    Nothing -> "<no item is waiting for it>"

  outcome = case conflict.resolution of
    ByPrecedence action ->
      "Settled by precedence: the parser will " <> describe action <> "."
    ByNonassoc ->
      "The token is declared `%nonassoc`, so it is a parse error here."
    ByDefault action ->
      "Nothing in the grammar says which to prefer, so the parser will "
        <> describe action
        <> ".\n  "
        <> advice

  describe = case _ of
    Shift _ -> "shift the token"
    Reduce p -> "reduce by `" <> whole g p <> "`"
    Accept -> "accept the input"

  advice = case conflict.kind of
    -- A disputed cell writes its own ending, just above; this is here for
    -- totality and says the same thing.
    Disputed _ ->
      "Precedences that agree -- or the same `%prec` on each -- would settle it."
    ShiftReduce _ ->
      "Declaring `%left`, `%right` or `%nonassoc` for the token, or `%prec` on\n  the production, would settle it deliberately."
    ReduceReduce _ ->
      "Precedence cannot settle a reduce/reduce conflict. Two rules matching the\n  same input usually means the grammar says something other than what was\n  meant."
    AcceptReduce _ ->
      "The start symbol can still be reduced once the input is complete, so a\n  finished parse and one more reduction look alike. A rule that derives the\n  start symbol from itself is the usual cause."

report :: LRGrammar -> Automaton -> Table -> Array String
report g automaton table = case unresolved table of
  [] -> []
  conflicts ->
    let
      ctx = context g automaton
    in
      map (explainWith g automaton ctx) conflicts
