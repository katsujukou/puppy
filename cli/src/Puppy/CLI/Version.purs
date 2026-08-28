module Puppy.CLI.Version (version, versionInfo) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Fmt as Fmt
import Puppy as Puppy
import Spago.Generated.BuildInfo as BuildInfo

foreign import version :: String

versionInfo :: String
versionInfo = Fmt.fmt
  @"Puppy {version} (built with purs {pursVersion})\n\
  \Supported runtime: {runtimeVersions}"
  { version
  , pursVersion: BuildInfo.pursVersion
  , runtimeVersions: anyOf Puppy.runtimeVersions
  }

-- | `a`, `a or b`, `a, b or c` -- the versions as something to read.
-- |
-- | Empty is not a case the list is ever in, and saying so out loud beats
-- | printing a sentence with a hole where the version should be.
anyOf :: Array String -> String
anyOf versions = case Array.unsnoc versions of
  Nothing -> "no version this build knows of"
  Just { init: [], last } -> last
  Just { init, last } -> joinWith ", " init <> " or " <> last
