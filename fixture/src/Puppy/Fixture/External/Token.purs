-- | A token type Puppy did not write.
-- |
-- | Everything here is deliberately awkward for a generator: every token
-- | carries a source position, so the interesting part of one is nested inside
-- | another constructor, and two of the lexemes are things a parser has no
-- | business seeing at all.
module Puppy.Fixture.External.Token where

import Prelude

data Token = At Int Lexeme

data Lexeme
  = Plus
  | Times
  | Number Int
  | Comment String
  | Space

derive instance Eq Token
derive instance Eq Lexeme

instance Show Token where
  show (At at lexeme) = "(At " <> show at <> " " <> show lexeme <> ")"

instance Show Lexeme where
  show = case _ of
    Plus -> "Plus"
    Times -> "Times"
    Number n -> "(Number " <> show n <> ")"
    Comment text -> "(Comment " <> show text <> ")"
    Space -> "Space"
