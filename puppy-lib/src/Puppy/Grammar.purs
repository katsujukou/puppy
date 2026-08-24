-- | A grammar with every parameterised rule instantiated and every `%inline`
-- | rule folded away: the form an automaton can be built from.
-- |
-- | Symbols are still names rather than numbers. Numbering them is the
-- | automaton's business, and keeping names here means a conflict report can
-- | say `separated_list(COMMA,expr)` instead of `nonterminal 17`.
module Puppy.Grammar
  ( Symbol(..)
  , Bound(..)
  , Binding
  , Action(..)
  , Production
  , Grammar
  , symbolName
  , renderProduction
  ) where

import Prelude

-- `Symbol` is also the name of a kind in `Prim`, which is imported implicitly.
-- Hiding it keeps the domain name without the shadowing warning.
import Prim hiding (Symbol)

import Data.Array as Array
import Data.Maybe (Maybe)
import Data.String.Common (joinWith)
import Puppy.Syntax
  ( Code
  , DeriveDecl
  , PrecedenceDecl
  , Span
  , StartDecl
  , TokenSource
  , TypeDecl
  )

data Symbol
  = Terminal String
  | Nonterminal String

derive instance Eq Symbol
derive instance Ord Symbol

instance Show Symbol where
  show = case _ of
    Terminal name -> "(Terminal " <> show name <> ")"
    Nonterminal name -> "(Nonterminal " <> show name <> ")"

symbolName :: Symbol -> String
symbolName = case _ of
  Terminal name -> name
  Nonterminal name -> name

-- | How a production computes its semantic value.
-- |
-- | The user's code is still the untouched text the parser read. What inlining
-- | changes is the *scope* it runs in, never the characters: folding a rule
-- | into its use site nests that rule's action inside the binding it feeds.
-- |
-- | Nesting rather than renaming is what makes this safe. If an inlined rule
-- | and its use site both bind `x`, each `x` stays in its own scope and each
-- | piece of code sees the one it was written against -- which no amount of
-- | textual substitution could promise, since it cannot tell a real reference
-- | from the same three characters inside a string literal.
newtype Action = Action
  { bindings :: Array Binding
  , code :: Code
  }

-- | A name in scope for one piece of user code.
type Binding = { name :: String, value :: Bound }

data Bound
  = FromStack Int
  -- ^ The semantic value of the right-hand side symbol at this position.
  | FromAction Action

-- ^ The value an inlined rule would have computed.

derive instance Eq Action
derive instance Eq Bound

instance Show Action where
  show (Action a) =
    "(Action { bindings: " <> show a.bindings <> ", code: " <> show a.code <> " })"

instance Show Bound where
  show = case _ of
    FromStack i -> "(FromStack " <> show i <> ")"
    FromAction a -> "(FromAction " <> show a <> ")"

type Production =
  { lhs :: String
  , rhs :: Array Symbol
  , precedence :: Maybe String
  -- ^ The token named by `%prec`, if the production carried one.
  , action :: Action
  , span :: Span
  -- ^ Where the production this came from was written, which survives both
  -- instantiation and inlining so that diagnostics can point at source.
  }

type Grammar =
  { header :: Maybe Code
  , tokens :: TokenSource
  -- ^ Whether the token type is generated or the author's own, and what is
  -- known about each terminal either way. `Syntax.declaredTokens` is what
  -- anything that only wants the terminals should reach for.
  , precedences :: Array PrecedenceDecl
  , types :: Array TypeDecl
  , derives :: Array DeriveDecl
  , starts :: Array StartDecl
  , productions :: Array Production
  }

-- | `expr -> expr PLUS expr`, for diagnostics.
renderProduction :: Production -> String
renderProduction p =
  p.lhs <> " -> " <>
    if Array.null p.rhs then "<empty>"
    else joinWith " " (map symbolName p.rhs)
