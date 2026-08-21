-- | The version this build calls itself.
-- |
-- | Foreign so that `npm version` can write it: the package manifest is where
-- | the number actually lives, and a copy in PureScript would be a copy to
-- | forget to change.
module Puppy.CLI.Version (version) where

foreign import version :: String
