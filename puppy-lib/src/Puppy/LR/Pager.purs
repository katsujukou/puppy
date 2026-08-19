-- | Pager's weak compatibility test.
-- |
-- | Canonical LR(1) never merges states, and pays for it in size. LALR merges
-- | every pair with the same core, and pays for it in conflicts that the
-- | grammar did not actually have. Pager's rule is the useful middle: merge two
-- | states with the same core unless doing so would invent a conflict that
-- | neither of them had.
module Puppy.LR.Pager
  ( weaklyCompatible
  , comparisons
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Puppy.LR.Item (ItemSet, core, lookaheadsOf)

-- | How many pairs of positions a core of this size makes the test look at.
-- |
-- | Quadratic, which is why the caller has to budget for it -- and why the
-- | arithmetic has to be careful. `n * (n - 1)` leaves the range of `Int` a
-- | little past forty thousand, and the wrapped result can come out negative,
-- | which would make the most expensive core in the grammar look like the
-- | cheapest thing in the world.
-- |
-- | So one factor is halved before the multiplication, and sizes past what even
-- | that can hold saturate. Saturating loses nothing: the only question ever
-- | asked here is whether the cost exceeds a budget, and past this size the
-- | answer cannot be no.
-- | The largest core size whose pair count still fits in an `Int`:
-- | `32768 * 65535` is just under the top of the range, and one more is over.
widest :: Int
widest = 65536

comparisons :: Int -> Int
comparisons n
  | n < 2 = 0
  | n > widest = top
  | mod n 2 == 0 = (n / 2) * (n - 1)
  | otherwise = n * ((n - 1) / 2)

-- | May these two states, which must already share a core, be merged?
-- |
-- | A merge can only cause harm through a pair of distinct positions `p` and
-- | `q`. After merging, both carry the union of their lookaheads, so a token in
-- | `p` from one state now sits alongside the same token in `q` from the other
-- | -- and where they meet, the merged state has to choose between two
-- | reductions. The pair is safe when any of these holds:
-- |
-- |   * the lookaheads do not cross: nothing `p` has in one state is something
-- |     `q` has in the other, in either direction, so the union creates no
-- |     overlap that was not there before;
-- |   * `p` and `q` already overlap within the first state, or within the
-- |     second -- the conflict exists already and merging is not what caused
-- |     it.
-- |
-- | Note what this does *not* claim: that the merged state is conflict-free.
-- | Only that it has no conflict the grammar did not already have.
-- |
-- | The two loops walk indices and stop at the first unsafe pair. Materialising
-- | the pairs first would allocate a quadratic number of them before looking at
-- | any, which a state with a few thousand positions turns into seconds.
weaklyCompatible :: ItemSet -> ItemSet -> Boolean
weaklyCompatible left right = outer 0
  where
  items = Set.toUnfoldable (core left) :: Array _

  -- Looked at O(n^2) times between them, so they are read once here rather
  -- than from the maps on every pair.
  lookaheads = map
    (\item -> { l: lookaheadsOf left item, r: lookaheadsOf right item })
    items

  n = Array.length lookaheads

  outer i
    | i >= n - 1 = true
    | otherwise = if inner i (i + 1) then outer (i + 1) else false

  inner i j
    | j >= n = true
    | otherwise = case Array.index lookaheads i, Array.index lookaheads j of
        Just p, Just q -> if safe p q then inner i (j + 1) else false
        _, _ -> true

  safe p q =
    (disjoint p.l q.r && disjoint q.l p.r)
      || overlaps p.l q.l
      || overlaps p.r q.r

  disjoint a b = Set.isEmpty (Set.intersection a b)

  overlaps a b = not (disjoint a b)
