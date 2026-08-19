module Test.Puppy.LR.Pager (spec) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Puppy.LR.Item (ItemSet)
import Puppy.LR.Pager (comparisons, weaklyCompatible)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- Two positions that a merge could put in conflict with each other. Which
-- productions they belong to does not matter here; only that they are distinct.
p :: { production :: Int, dot :: Int }
p = { production: 0, dot: 1 }

q :: { production :: Int, dot :: Int }
q = { production: 1, dot: 1 }

-- A state with those two positions, carrying the given lookaheads.
state :: Array Int -> Array Int -> ItemSet
state forP forQ = Map.fromFoldable
  [ Tuple p (Set.fromFoldable forP)
  , Tuple q (Set.fromFoldable forQ)
  ]

spec :: Spec Unit
spec = describe "Puppy.LR.Pager" do
  it "merges a state with itself" do
    weaklyCompatible (state [ 1 ] [ 2 ]) (state [ 1 ] [ 2 ]) `shouldEqual` true

  it "merges when no lookahead of one position meets the other's" do
    weaklyCompatible (state [ 1 ] [ 2 ]) (state [ 3 ] [ 4 ]) `shouldEqual` true

  -- The case the whole test exists for: `p` has 1 here and `q` has 1 there, and
  -- the other way round for 2. Neither state can act on 1 two ways, but the
  -- merged one could.
  it "refuses when the lookaheads cross" do
    weaklyCompatible (state [ 1 ] [ 2 ]) (state [ 2 ] [ 1 ]) `shouldEqual` false

  -- Crossed, but one state already has both positions on 1, so the merge is
  -- not what causes the conflict.
  it "merges when one state already has the conflict" do
    weaklyCompatible (state [ 1, 2 ] [ 1 ]) (state [ 2 ] [ 1 ])
      `shouldEqual` true

  it "merges when a state has a single position, whatever the lookaheads" do
    weaklyCompatible
      (Map.singleton p (Set.fromFoldable [ 1 ]))
      (Map.singleton p (Set.fromFoldable [ 2 ]))
      `shouldEqual` true

  -- Crossing in one direction is enough to do the damage: the merged state
  -- would have both positions on 2, and neither state had that.
  it "refuses when the lookaheads cross in only one direction" do
    weaklyCompatible (state [ 1 ] [ 2 ]) (state [ 2 ] [ 3 ])
      `shouldEqual` false

  -- The cost is only ever compared against a budget, but it has to be a number
  -- for that to mean anything. Multiplying before dividing leaves the range of
  -- `Int` a little past forty thousand, and a negative cost would make the most
  -- expensive core in a grammar look like the cheapest.
  describe "counting the pairs" do
    it "counts nothing below two positions" do
      map comparisons [ 0, 1, 2, 3 ] `shouldEqual` [ 0, 0, 1, 3 ]

    it "stays exact where the arithmetic still fits" do
      map comparisons [ 46341, 50000, 65536 ]
        `shouldEqual` [ 1073720970, 1249975000, 2147450880 ]

    it "saturates rather than wrapping past that" do
      map comparisons [ 65537, 100000, 1000000 ]
        `shouldEqual` [ top, top, top ]

    it "never returns a negative cost" do
      Array.all (_ >= 0) (map comparisons sizes) `shouldEqual` true
  where
  sizes = [ 0, 2, 1000, 46340, 46341, 46342, 50000, 65535, 65536, 65537, 200000, 2000000 ]
