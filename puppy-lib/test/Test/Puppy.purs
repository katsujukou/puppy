module Test.Puppy where

import Prelude

import Effect (Effect)
import Test.Puppy.Expand as Expand
import Test.Puppy.LR.Automaton as LRAutomaton
import Test.Puppy.LR.Grammar as LRGrammar
import Test.Puppy.LR.Pager as LRPager
import Test.Puppy.LR.Table as LRTable
import Test.Puppy.Syntax.Lexer as Lexer
import Test.Puppy.Syntax.Parser as Parser
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Lexer.spec
  Parser.spec
  Expand.spec
  LRGrammar.spec
  LRAutomaton.spec
  LRAutomaton.mergeSpec
  LRAutomaton.invariantSpec
  LRPager.spec
  LRTable.spec
  LRTable.explainSpec
