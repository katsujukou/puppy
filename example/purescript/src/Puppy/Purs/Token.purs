-- | The tokens the PureScript grammar is written over.
-- |
-- | This is a token type Puppy did not write, named by `%tokentype`, and its
-- | shape is decided by one rule of Puppy's: a `%token` pattern gets a single
-- | `$$`, so everything a rule wants from a token has to be reachable through
-- | one hole. That is why the position lives inside each payload instead of in
-- | an enclosing `Token pos lexeme` constructor, the way the compiler's own
-- | `SourceToken` has it -- a rule matching `Token _ (TokLower ...)` could
-- | capture the name or the position, and not both.
-- |
-- | It is also why qualified and unqualified names are separate constructors.
-- | Upstream writes `TokLowerName [] _` and `TokLowerName _ _` for the two, and
-- | tells them apart by pattern; here the pattern that pins the qualifier down
-- | would be the pattern that could no longer capture the record.
module Puppy.Purs.Token where

import Prelude

import Data.Array as Array
import Data.String.Common (joinWith)

type Pos = { line :: Int, column :: Int }

-- | A name the lexer read without a qualifier, and where it starts.
type Ident = { pos :: Pos, name :: String }

-- | A name with one, `Data.Map` in `Data.Map.insert`.
type Qual = { pos :: Pos, qualifier :: Array String, name :: String }

-- | A literal, with the text it was written as. The text is kept because
-- | `0x10` and `16` are the same `Int` and not the same source.
type Lit a = { pos :: Pos, raw :: String, value :: a }

data Token
  = TokLeftParen Pos
  | TokRightParen Pos
  | TokLeftBrace Pos
  | TokRightBrace Pos
  | TokLeftSquare Pos
  | TokRightSquare Pos
  | TokLayoutStart Pos
  | TokLayoutEnd Pos
  | TokLayoutSep Pos
  | TokLeftArrow Pos
  | TokRightArrow Pos
  | TokLeftFatArrow Pos
  | TokRightFatArrow Pos
  | TokDoubleColon Pos
  | TokEquals Pos
  | TokPipe Pos
  | TokTick Pos
  | TokDot Pos
  | TokComma Pos
  | TokUnderscore Pos
  | TokBackslash Pos
  | TokForall Pos
  | TokSymbolArrow Pos
  | TokLower Ident
  | TokQualLower Qual
  | TokUpper Ident
  | TokQualUpper Qual
  | TokSymbol Ident
  | TokQualSymbol Qual
  | TokOperator Ident
  | TokQualOperator Qual
  | TokHole Ident
  | TokChar (Lit Char)
  | TokString (Lit String)
  | TokRawString (Lit String)
  | TokInt (Lit Int)
  | TokNumber (Lit Number)

derive instance Eq Token

instance Show Token where
  show = case _ of
    TokLeftParen _ -> "("
    TokRightParen _ -> ")"
    TokLeftBrace _ -> "{"
    TokRightBrace _ -> "}"
    TokLeftSquare _ -> "["
    TokRightSquare _ -> "]"
    TokLayoutStart _ -> "{-start-}"
    TokLayoutEnd _ -> "{-end-}"
    TokLayoutSep _ -> "{-sep-}"
    TokLeftArrow _ -> "<-"
    TokRightArrow _ -> "->"
    TokLeftFatArrow _ -> "<="
    TokRightFatArrow _ -> "=>"
    TokDoubleColon _ -> "::"
    TokEquals _ -> "="
    TokPipe _ -> "|"
    TokTick _ -> "`"
    TokDot _ -> "."
    TokComma _ -> ","
    TokUnderscore _ -> "_"
    TokBackslash _ -> "\\"
    TokForall _ -> "forall"
    TokSymbolArrow _ -> "(->)"
    TokLower t -> t.name
    TokQualLower t -> qualifiedText t
    TokUpper t -> t.name
    TokQualUpper t -> qualifiedText t
    TokSymbol t -> "(" <> t.name <> ")"
    TokQualSymbol t -> "(" <> qualifiedText t <> ")"
    TokOperator t -> t.name
    TokQualOperator t -> qualifiedText t
    TokHole t -> "?" <> t.name
    TokChar t -> t.raw
    TokString t -> t.raw
    TokRawString t -> t.raw
    TokInt t -> t.raw
    TokNumber t -> t.raw

qualifiedText :: Qual -> String
qualifiedText q
  | Array.null q.qualifier = q.name
  | otherwise = joinWith "." q.qualifier <> "." <> q.name

-- | The position of whatever a token is.
positionOf :: Token -> Pos
positionOf = case _ of
  TokLeftParen p -> p
  TokRightParen p -> p
  TokLeftBrace p -> p
  TokRightBrace p -> p
  TokLeftSquare p -> p
  TokRightSquare p -> p
  TokLayoutStart p -> p
  TokLayoutEnd p -> p
  TokLayoutSep p -> p
  TokLeftArrow p -> p
  TokRightArrow p -> p
  TokLeftFatArrow p -> p
  TokRightFatArrow p -> p
  TokDoubleColon p -> p
  TokEquals p -> p
  TokPipe p -> p
  TokTick p -> p
  TokDot p -> p
  TokComma p -> p
  TokUnderscore p -> p
  TokBackslash p -> p
  TokForall p -> p
  TokSymbolArrow p -> p
  TokLower t -> t.pos
  TokQualLower t -> t.pos
  TokUpper t -> t.pos
  TokQualUpper t -> t.pos
  TokSymbol t -> t.pos
  TokQualSymbol t -> t.pos
  TokOperator t -> t.pos
  TokQualOperator t -> t.pos
  TokHole t -> t.pos
  TokChar t -> t.pos
  TokString t -> t.pos
  TokRawString t -> t.pos
  TokInt t -> t.pos
  TokNumber t -> t.pos
