-- | The standard-library names a generated parser uses, in one place.
-- |
-- | Not for you: this is here so that a generated module can be written against
-- | `Puppy.Runtime.*` alone. Without it, every package holding a generated
-- | parser has to depend on `arrays`, `either`, `maybe` and `tailrec` on that
-- | file's behalf -- for a file its author did not write and is told not to
-- | read.
-- |
-- | These are the same names your own code gets from `Data.Maybe`,
-- | `Data.Either` and the rest; a re-export renames nothing. Import those
-- | directly.
module Puppy.Runtime.Deps
  ( module Control.Monad.Rec.Class
  , module Data.Array
  , module Data.Either
  , module Data.Maybe
  ) where

import Control.Monad.Rec.Class (class MonadRec)
import Data.Array (find, index)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
