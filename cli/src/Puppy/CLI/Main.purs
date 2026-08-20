module Puppy.CLI.Main where

import Prelude

import ArgParse.Basic (ArgError(..), ArgErrorMsg(..))
import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console as Console
import Fmt as Fmt
import Node.Process as Process
import Puppy.CLI.Generate as Generate
import Puppy.CLI.Node as Node
import Puppy.CLI.Options as Options

main :: Effect Unit
main = do
  args <- Array.drop 2 <$> Process.argv
  case Options.parse args of
    -- Asking for help is not a failure, and a shell that checks the exit
    -- status should not be told it was one.
    Left err@(ArgError _ ShowHelp) -> asked err
    Left err@(ArgError _ (ShowInfo _)) -> asked err
    Left err -> do
      Console.error (ArgParser.printArgError err)
      Process.exit' 1
    Right cmd -> run case cmd of
      Options.Generate opts -> Generate.cmd opts
  where
  asked err = Console.log (ArgParser.printArgError err)

  run program = do
    result <- Node.runNode program
    case result of
      Right _ -> pure unit
      Left message -> do
        Console.error (Fmt.fmt @"puppy: {message}" { message })
        Process.exit' 1
