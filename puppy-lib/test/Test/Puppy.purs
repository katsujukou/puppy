module Test.Puppy where

import Prelude

import Effect (Effect)
import Test.Puppy.Expand as Expand
import Test.Puppy.Syntax.Lexer as Lexer
import Test.Puppy.Syntax.Parser as Parser
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Lexer.spec
  Parser.spec
  Expand.spec
