-- | An external token type whose constructors are exactly the grammar's
-- | terminals.
-- |
-- | This is the ordinary case -- a token type and a grammar written for one
-- | another -- and it is the one that catches a fallback the compiler can prove
-- | unreachable. Between them the patterns cover `Maybe Word` entirely, so an
-- | arm for "a token this grammar does not declare" would be a dead arm, and a
-- | dead arm is an error under `--strict` in a file nobody wrote by hand.
module Puppy.Fixture.External.Word where

import Prelude

data Word
  = Yes
  | No
  | Count Int

derive instance Eq Word

instance Show Word where
  show = case _ of
    Yes -> "Yes"
    No -> "No"
    Count n -> "(Count " <> show n <> ")"
