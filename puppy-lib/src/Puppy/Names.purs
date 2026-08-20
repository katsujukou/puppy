-- | Which names can survive the trip into PureScript.
-- |
-- | A grammar's names are identifiers as far as the lexer is concerned, but
-- | they do not all end up in the same place. A token name becomes a
-- | constructor, a start symbol becomes a top-level function, a binder becomes
-- | a `let`. Each of those has rules, and a name that breaks them produces a
-- | module that will not compile -- reported, by then, against generated code
-- | nobody wrote.
module Puppy.Names
  ( isConstructor
  , isValue
  , isModuleName
  , isReserved
  , takenByGeneratedCode
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))

isUpper :: Char -> Boolean
isUpper c = c >= 'A' && c <= 'Z'

isLower :: Char -> Boolean
isLower c = c >= 'a' && c <= 'z'

isIdentChar :: Char -> Boolean
isIdentChar c =
  isUpper c || isLower c || (c >= '0' && c <= '9') || c == '_' || c == '\''

shaped :: (Char -> Boolean) -> String -> Boolean
shaped start name = case Array.uncons (SCU.toCharArray name) of
  Nothing -> false
  Just { head, tail } -> start head && Array.all isIdentChar tail

-- | A data constructor: upper case to begin with.
isConstructor :: String -> Boolean
isConstructor = shaped isUpper

-- | A value: lower case, or an underscore. Not a bare underscore, which is a
-- | wildcard and cannot stand where a name is expected.
isValue :: String -> Boolean
isValue name =
  name /= "_"
    && shaped (\c -> isLower c || c == '_') name
    && not (isReserved name)

isModuleName :: String -> Boolean
isModuleName name = case split (Pattern ".") name of
  [] -> false
  parts -> Array.all isConstructor parts && not (Array.null parts)

isReserved :: String -> Boolean
isReserved name = Array.elem name
  [ "ado"
  , "case"
  , "class"
  , "data"
  , "derive"
  , "do"
  , "else"
  , "false"
  , "forall"
  , "foreign"
  , "if"
  , "import"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "module"
  , "newtype"
  , "of"
  , "then"
  , "true"
  , "type"
  , "where"
  ]

-- | Names the generated module already defines at the top level, or uses
-- | without qualifying.
-- |
-- | A start symbol becomes a top-level function beside these, so one that
-- | shares a name with `tableFor` redefines it, and one called `map` makes
-- | every use of `map` in the generated code ambiguous.
takenByGeneratedCode :: String -> Boolean
takenByGeneratedCode name =
  Array.elem name
    [ "actionAt"
    , "actionRows"
    , "feed"
    , "gotoAt"
    , "gotoRows"
    , "map"
    , "productionAt"
    , "productionTable"
    , "semanticActionAt"
    , "semanticActionTable"
    , "show"
    , "tableFor"
    , "terminalIndex"
    , "terminalName"
    , "terminalNames"
    , "terminalValue"
    , "toParseError"
    , "unit"
    ]
    || startsWith "puppy" name
  where
  startsWith prefix text = SCU.take (SCU.length prefix) text == prefix
