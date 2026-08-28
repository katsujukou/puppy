-- | That what the generator claims about `puppy-runtime` is true.
-- |
-- | `Puppy.runtimeVersions` is a claim, not a measurement: it says which
-- | runtimes can run what this generator writes, and nothing about writing it
-- | down makes it so. While the runtime is still in this repository there is
-- | one thing that can be checked, which is that the runtime sitting next to
-- | the generator is one of the versions the generator says it supports. That
-- | is what this is.
-- |
-- | The runtime's manifest says it publishes to a repository of its own, so
-- | this check has a shelf life. When the runtime moves out, `packages` stops
-- | having a `puppy-runtime` field and this module stops compiling -- which is
-- | the right way for it to end, because at that point the claim needs a
-- | different kind of evidence and someone should be made to think about what.
module Test.Puppy.Runtimes (spec) where

import Prelude

import Data.Array as Array
import Puppy as Puppy
import Spago.Generated.BuildInfo as BuildInfo
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Puppy" do
  it "supports the runtime it is built beside" do
    let beside = BuildInfo.packages."puppy-runtime"
    Array.elem beside Puppy.runtimeVersions `shouldEqual` true
