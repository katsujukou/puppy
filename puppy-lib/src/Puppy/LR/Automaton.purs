-- | Building the LR automaton.
-- |
-- | A state is a set of items, and it is identified by its kernel: the items
-- | whose dot has moved, which is exactly what the transition into the state
-- | produced. Everything else in a state follows from the kernel by closure.
-- |
-- | How aggressively states are merged is the one knob. The construction is
-- | otherwise the same, which is the point: `Canonical` is the answer to
-- | measure the others against, and it differs from `Pager` only in which
-- | existing state a new kernel is allowed to land in.
module Puppy.LR.Automaton
  ( Policy(..)
  , State
  , Automaton
  , maxStates
  , build
  , closure
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Puppy.LR.Analysis (Analysis, firstOfSequence)
import Puppy.LR.Grammar (LRGrammar, LRError, Sym(..))
import Puppy.LR.Item (Item, ItemSet, core)
import Puppy.LR.Pager (comparisons, weaklyCompatible)
import Puppy.Syntax (Span)

data Policy
  = Canonical
  -- ^ Never merge. The largest automaton, and the one with the sharpest
  -- lookaheads; what the others are judged against.
  | Pager
  -- ^ Merge states with the same core unless the merge would invent a conflict.
  | LALR

-- ^ Merge every pair of states with the same core, conflicts and all. Here to
-- make the difference measurable, not because a generator should use it.

derive instance Eq Policy

instance Show Policy where
  show = case _ of
    Canonical -> "Canonical"
    Pager -> "Pager"
    LALR -> "LALR"

mergeable :: Policy -> ItemSet -> ItemSet -> Boolean
mergeable = case _ of
  Canonical -> eq
  Pager -> weaklyCompatible
  LALR -> \_ _ -> true

type State =
  { kernel :: ItemSet
  , items :: ItemSet
  -- ^ The kernel's closure: what the state can actually do.
  , transitions :: Map Sym Int
  }

type Automaton =
  { states :: Array State
  , initial :: Array Int
  -- ^ One entry state per `%start` symbol, in the same order.
  }

-- | Canonical LR(1) is the construction that blows up; a grammar that asks for
-- | more states than this is past the point where a table would be usable.
maxStates :: Int
maxStates = 50000

-- | How much comparing of lookaheads the whole construction may do.
-- |
-- | Separate from `maxStates` because it bounds something else: one state with
-- | a few thousand positions costs millions of comparisons on its own, so a
-- | grammar with a handful of states can still take minutes. The unit is one
-- | pair of positions looked at.
maxCompatibilityWork :: Int
maxCompatibilityWork = 20000000

productionsByLhs :: LRGrammar -> Map Int (Array Int)
productionsByLhs g = Array.foldl step Map.empty
  (Array.mapWithIndex Tuple g.productions)
  where
  step acc (Tuple i p) =
    Map.alter (Just <<< maybe [ i ] (_ <> [ i ])) p.lhs acc

rhsOf :: LRGrammar -> Int -> Array Sym
rhsOf g i = maybe [] _.rhs (Array.index g.productions i)

-- | The symbol the dot sits before, if the item is not complete.
afterDot :: LRGrammar -> Item -> Maybe Sym
afterDot g item = Array.index (rhsOf g item.production) item.dot

-- | Add every item reachable without consuming input.
-- |
-- | Only the lookaheads an item has newly gained go back on the worklist. That
-- | is sound because what an item contributes is a union over its lookaheads
-- | taken one at a time, so nothing is lost by not replaying the ones already
-- | handled -- and replaying them would turn a fixpoint into a lot of repeated
-- | work.
closure :: LRGrammar -> Analysis -> Map Int (Array Int) -> ItemSet -> ItemSet
closure g an byLhs kernel = go (Map.toUnfoldable kernel) kernel
  where
  go pending acc = case pending of
    Nil -> acc
    Cons (Tuple item lookaheads) rest -> case afterDot g item of
      Just (N b) ->
        let
          rest' = Array.drop (item.dot + 1) (rhsOf g item.production)
          inherited = firstOfSequence an rest' lookaheads
          grown = Array.foldl (add inherited) { acc, pending: rest }
            (fromMaybe [] (Map.lookup b byLhs))
        in
          go grown.pending grown.acc
      _ -> go rest acc

  add lookaheads st production =
    let
      item = { production, dot: 0 }
      existing = fromMaybe Set.empty (Map.lookup item st.acc)
      delta = Set.difference lookaheads existing
    in
      if Set.isEmpty delta then st
      else st
        { acc = Map.insert item (Set.union existing delta) st.acc
        , pending = Tuple item delta : st.pending
        }

-- | The kernel each outgoing transition leads to.
successors :: LRGrammar -> ItemSet -> Map Sym ItemSet
successors g items = Array.foldl step Map.empty
  (Map.toUnfoldable items :: Array (Tuple Item (Set Int)))
  where
  step acc (Tuple item lookaheads) = case afterDot g item of
    Nothing -> acc
    Just symbol ->
      let
        moved = { production: item.production, dot: item.dot + 1 }
      in
        Map.alter (Just <<< place moved lookaheads) symbol acc

  place item lookaheads = case _ of
    Nothing -> Map.singleton item lookaheads
    Just set -> Map.insertWith Set.union item lookaheads set

type Build =
  { byCore :: Map (Set Item) (Array Int)
  , kernels :: Map Int ItemSet
  , items :: Map Int ItemSet
  , transitions :: Map Int (Map Sym Int)
  , next :: Int
  , budget :: Int
  }

-- | What one compatibility test costs, in units of "one pair of positions
-- | looked at". Only Pager pays a quadratic price; the others are here so that
-- | every policy is charged on the same scale.
comparisonCost :: Policy -> Int -> Int
comparisonCost policy size = case policy of
  Canonical -> max 1 size
  Pager -> max 1 (comparisons size)
  LALR -> 1

nowhere :: Span
nowhere = { start: origin, end: origin }
  where
  origin = { offset: 0, line: 1, column: 1 }

build :: Policy -> LRGrammar -> Analysis -> Either (Array LRError) Automaton
build policy g an = case construct policy g an of
  Left err -> Left [ err ]
  Right automaton -> Right automaton

construct :: Policy -> LRGrammar -> Analysis -> Either LRError Automaton
construct policy g an = do
  seeded <- Array.foldl seed (Right { st: fresh, entries: [], work: Nil }) g.starts
  st <- go seeded.work seeded.st
  pure
    { states: Array.mapMaybe (assemble st) (indices st)
    , initial: seeded.entries
    }
  where
  byLhs = productionsByLhs g

  fresh =
    { byCore: Map.empty
    , kernels: Map.empty
    , items: Map.empty
    , transitions: Map.empty
    , next: 0
    , budget: maxCompatibilityWork
    }

  indices st = if st.next <= 0 then [] else Array.range 0 (st.next - 1)

  seed acc start = acc >>= \a -> do
    let
      kernel = Map.singleton
        { production: start.production, dot: 0 }
        (Set.singleton g.eof)
    landed <- intern kernel a.st
    pure
      { st: landed.st
      , entries: Array.snoc a.entries landed.state
      , work: if landed.dirty then landed.state : a.work else a.work
      }

  -- | Add lookaheads to a state that already exists.
  absorb target kernel st =
    let
      existing = fromMaybe Map.empty (Map.lookup target st.kernels)
      merged = Map.unionWith Set.union existing kernel
    in
      if merged == existing then { st, dirty: false }
      else
        { st: st { kernels = Map.insert target merged st.kernels }
        , dirty: true
        }

  -- | Walk the states that share this core, looking for one the kernel may
  -- | join. Each test is charged for, because Pager's is quadratic in the size
  -- | of the core and a grammar can make cores very large.
  chooseHome kernel st =
    let
      shape = core kernel
      cost = comparisonCost policy (Set.size shape)

      scan candidates acc = case Array.uncons candidates of
        Nothing -> Right { st: acc, found: Nothing, shape }
        Just { head, tail }
          | acc.budget < cost -> Left
              { message: "deciding which LR states to merge took more than "
                  <> show maxCompatibilityWork
                  <> " comparisons of lookaheads; testing a state with "
                  <> show (Set.size shape)
                  <> " positions costs a quadratic number of them"
              , span: nowhere
              }
          | otherwise ->
              let
                acc' = acc { budget = acc.budget - cost }
                fits = maybe false (mergeable policy kernel)
                  (Map.lookup head acc'.kernels)
              in
                if fits then Right { st: acc', found: Just head, shape }
                else scan tail acc'
    in
      scan (fromMaybe [] (Map.lookup shape st.byCore)) st

  intern kernel st = do
    home <- chooseHome kernel st
    case home.found of
      Just target ->
        let
          absorbed = absorb target kernel home.st
        in
          pure { st: absorbed.st, state: target, dirty: absorbed.dirty }
      Nothing ->
        let
          fresh' = home.st
        in
          pure
            { st: fresh'
                { byCore = Map.alter
                    (Just <<< maybe [ fresh'.next ] (_ <> [ fresh'.next ]))
                    home.shape
                    fresh'.byCore
                , kernels = Map.insert fresh'.next kernel fresh'.kernels
                , next = fresh'.next + 1
                }
            , state: fresh'.next
            , dirty: true
            }

  go pending st = case pending of
    Nil -> Right st
    Cons state rest
      | st.next > maxStates -> Left
          { message: "this grammar needs more than " <> show maxStates
              <> " LR states"
          , span: nowhere
          }
      | otherwise ->
          let
            kernel = fromMaybe Map.empty (Map.lookup state st.kernels)
            closed = closure g an byLhs kernel
            previous = fromMaybe Map.empty (Map.lookup state st.transitions)
            stepped = Array.foldl (link previous)
              (Right { st, transitions: Map.empty, work: rest })
              (Map.toUnfoldable (successors g closed) :: Array (Tuple Sym ItemSet))
          in
            case stepped of
              Left err -> Left err
              Right s -> go s.work s.st
                { items = Map.insert state closed s.st.items
                , transitions = Map.insert state s.transitions s.st.transitions
                }

  -- | Follow an edge, or make one.
  -- |
  -- | Where the edge already exists it is kept. A state that has gained
  -- | lookaheads pushes them along the edges it already has; asking again which
  -- | state its successor should be would re-point the edge whenever the
  -- | grown kernel no longer suits the old target, stranding that target with
  -- | lookaheads nothing accounts for. Compatibility decides where a kernel
  -- | first lands, not where it goes on landing.
  link previous acc (Tuple symbol kernel) = acc >>= \a ->
    case Map.lookup symbol previous of
      Just target ->
        let
          absorbed = absorb target kernel a.st
        in
          pure
            { st: absorbed.st
            , transitions: Map.insert symbol target a.transitions
            , work: if absorbed.dirty then target : a.work else a.work
            }
      Nothing -> do
        landed <- intern kernel a.st
        pure
          { st: landed.st
          , transitions: Map.insert symbol landed.state a.transitions
          , work: if landed.dirty then landed.state : a.work else a.work
          }

  assemble st i = do
    kernel <- Map.lookup i st.kernels
    pure
      { kernel
      , items: fromMaybe Map.empty (Map.lookup i st.items)
      , transitions: fromMaybe Map.empty (Map.lookup i st.transitions)
      }
