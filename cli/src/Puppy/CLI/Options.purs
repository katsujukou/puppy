-- | What the command line means.
-- |
-- | One command for now, so it is the whole of the parser rather than a name
-- | to type before the flags. `Command` is a sum all the same: adding a second
-- | one should be a matter of a `choose`, not a rewrite.
module Puppy.CLI.Options
  ( Command(..)
  , parse
  ) where

import Prelude

import ArgParse.Basic as ArgParser
import Data.Either (Either)
import Puppy.CLI.Generate as Generate
import Puppy.CLI.Version (version)

data Command = Generate Generate.Options

command :: ArgParser.ArgParser Command
command = (Generate <$> Generate.options)
  <* ArgParser.flagHelp
  <* ArgParser.flagInfo [ "--version", "-v" ] "Show the current version" version

parse :: Array String -> Either ArgParser.ArgError Command
parse = ArgParser.parseArgs
  "puppy"
  "A parser generator for PureScript"
  command
