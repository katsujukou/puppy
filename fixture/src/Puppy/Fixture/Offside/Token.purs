-- | Tokens for a language whose blocks are written with indentation.
-- |
-- | The column each token starts at is here because the layout pass needs it,
-- | not because the parser does. By the time the grammar sees a token the
-- | blocks have already been marked out, and every rule matches `T.At _`.
-- |
-- | The last three constructors are the ones the parser cares about most and
-- | no lexer ever produces. They are what the layout pass inserts: where a
-- | block begins, where one declaration ends and the next starts, and where
-- | the block ends. Written out, they are the `{`, `;` and `}` the grammar
-- | would have had if the language made you type them.
module Puppy.Fixture.Offside.Token where

import Prelude

data Token = At Int Lexeme

data Lexeme
  = Let
  | In
  | Equals
  | Plus
  | Name String
  | Number Int
  | BlockStart
  | BlockSep
  | BlockEnd

derive instance Eq Token
derive instance Eq Lexeme

instance Show Token where
  show (At column lexeme) = "(At " <> show column <> " " <> show lexeme <> ")"

instance Show Lexeme where
  show = case _ of
    Let -> "Let"
    In -> "In"
    Equals -> "Equals"
    Plus -> "Plus"
    Name name -> "(Name " <> show name <> ")"
    Number n -> "(Number " <> show n <> ")"
    BlockStart -> "BlockStart"
    BlockSep -> "BlockSep"
    BlockEnd -> "BlockEnd"
