-- | Asking whoever ran the tool a yes-or-no question.
-- |
-- | One question, because there is one thing worth stopping for: a file that
-- | is about to be overwritten and was not written by Puppy.
-- |
-- | The answer has three shapes rather than two. "No" and "there was nobody to
-- | ask" both mean the file stays as it is, but they are different situations
-- | and want different things said: one is a decision, and the other is a
-- | build script that needs changing.
module Puppy.CLI.Effect.Prompt
  ( Answer(..)
  , PromptF(..)
  , PROMPT
  , _prompt
  , interpret
  , confirm
  ) where

import Prelude

import Run (Run)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

-- | `NoOneToAsk` is standard input not being a terminal: a build script, a CI
-- | job, a git hook. Waiting for an answer there is waiting for ever, so
-- | nothing is asked.
data Answer
  = Yes
  | No
  | NoOneToAsk

derive instance Eq Answer

instance Show Answer where
  show = case _ of
    Yes -> "Yes"
    No -> "No"
    NoOneToAsk -> "NoOneToAsk"

data PromptF a = Confirm String (Answer -> a)

derive instance Functor PromptF

type PROMPT r = (prompt :: PromptF | r)

_prompt :: Proxy "prompt"
_prompt = Proxy

interpret :: forall r a. (PromptF ~> Run r) -> Run (PROMPT + r) a -> Run r a
interpret handler = Run.interpret (Run.on _prompt handler Run.send)

-- | Put a question, and wait for an answer if there is anyone to give one.
confirm :: forall r. String -> Run (PROMPT + r) Answer
confirm question = Run.lift _prompt (Confirm question identity)
