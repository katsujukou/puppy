-- | LR(1) items, and the sets of them that make up a state.
-- |
-- | A position in a production is one thing and the tokens that may follow it
-- | are another, so they are kept apart: an item set maps each position to the
-- | set of lookaheads it has been reached with. Merging two states is then a
-- | union of lookaheads, and asking whether two states have the same shape is a
-- | question about the keys alone.
module Puppy.LR.Item
  ( Item
  , ItemSet
  , core
  , lookaheadsOf
  , renderItem
  , renderItems
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import Puppy.LR.Grammar (LRGrammar, Prod, Sym(..), renderSym)

type Item = { production :: Int, dot :: Int }

type ItemSet = Map Item (Set Int)

-- | The item set stripped of its lookaheads: the LR(0) state underneath it.
-- | Two LR(1) states can only ever be merged if these agree.
core :: ItemSet -> Set Item
core = Set.fromFoldable <<< Map.keys

lookaheadsOf :: ItemSet -> Item -> Set Int
lookaheadsOf items item = fromMaybe Set.empty (Map.lookup item items)

-- | `expr -> expr . PLUS expr  [EOF PLUS]`
renderItem :: LRGrammar -> Item -> Set Int -> String
renderItem g item lookaheads =
  lhsName <> " -> " <> joinWith " " (before <> [ "." ] <> after)
    <> "  ["
    <> joinWith " " names
    <> "]"
  where
  production :: Maybe Prod
  production = Array.index g.productions item.production

  rhs = maybe [] _.rhs production

  lhsName = maybe "?" (\p -> fromMaybe "?" (Array.index g.nonterminals p.lhs))
    production

  before = map (renderSym g) (Array.take item.dot rhs)

  after = map (renderSym g) (Array.drop item.dot rhs)

  names = map (renderSym g <<< T) (Set.toUnfoldable lookaheads :: Array Int)

renderItems :: LRGrammar -> ItemSet -> Array String
renderItems g items = map
  (\(Tuple item lookaheads) -> renderItem g item lookaheads)
  (Map.toUnfoldable items)
