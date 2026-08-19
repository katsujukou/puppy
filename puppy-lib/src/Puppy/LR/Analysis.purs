-- | Which nonterminals can vanish, and which tokens each can begin with.
-- |
-- | Both are least fixed points: start from nothing and keep adding until a
-- | pass adds nothing. The closure that builds LR(1) item sets asks for the
-- | first tokens of "the rest of this production, and then whatever could
-- | follow it", which is what `firstOfSequence` answers.
module Puppy.LR.Analysis
  ( Analysis
  , analyse
  , firstOf
  , firstOfSequence
  , isNullable
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Puppy.LR.Grammar (LRGrammar, Sym(..))

type Analysis =
  { nullable :: Set Int
  , first :: Map Int (Set Int)
  }

isNullable :: Analysis -> Int -> Boolean
isNullable a n = Set.member n a.nullable

firstOf :: Analysis -> Int -> Set Int
firstOf a n = fromMaybe Set.empty (Map.lookup n a.first)

-- | The tokens a sequence of symbols can begin with. `andThen` is what may
-- | follow the sequence, and is reached only if every symbol in it can vanish.
firstOfSequence :: Analysis -> Array Sym -> Set Int -> Set Int
firstOfSequence a syms andThen = go 0
  where
  go i = case Array.index syms i of
    Nothing -> andThen
    Just (T t) -> Set.singleton t
    Just (N n)
      | isNullable a n -> Set.union (firstOf a n) (go (i + 1))
      | otherwise -> firstOf a n

analyse :: LRGrammar -> Analysis
analyse g = { nullable, first: firstFixpoint Map.empty }
  where
  nullable = nullableFixpoint Set.empty

  nullableFixpoint known =
    let
      grown = Array.foldl nullableStep known g.productions
    in
      if Set.size grown == Set.size known then known else nullableFixpoint grown

  nullableStep known p
    | Set.member p.lhs known = known
    | Array.all (vanishes known) p.rhs = Set.insert p.lhs known
    | otherwise = known

  vanishes known = case _ of
    T _ -> false
    N n -> Set.member n known

  -- Sets only ever grow, so the total number of members is a fine measure of
  -- whether a pass changed anything.
  firstFixpoint acc =
    let
      grown = Array.foldl firstStep acc g.productions
    in
      if totalSize grown == totalSize acc then grown else firstFixpoint grown

  firstStep acc p =
    let
      -- An empty tail is right here: this is what the production alone can
      -- begin with, not what may follow the nonterminal.
      added = firstOfSequence { nullable, first: acc } p.rhs Set.empty
    in
      Map.insert p.lhs (Set.union (fromMaybe Set.empty (Map.lookup p.lhs acc)) added) acc

  totalSize m = foldl (\n s -> n + Set.size s) 0 (Map.values m)
