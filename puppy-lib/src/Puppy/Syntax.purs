-- | The shape of a `.puppy` grammar file, as written.
-- |
-- | This is a faithful record of the source: nothing is resolved or desugared,
-- | and every fragment of PureScript is kept verbatim with the span it came
-- | from. Turning this into something an automaton can be built from is a later
-- | pass.
-- |
-- | Declarations are split by kind, and each kind keeps its own source order --
-- | which is all any later pass needs, since precedence levels only rank
-- | against one another and terminal numbering only follows `%token`. Order
-- | across kinds is not stored, but neither is it lost: every declaration
-- | carries its own span, so sorting the four arrays together on `span.start`
-- | recovers the order they were written in. Order is all that recovers: this
-- | is an abstract syntax tree, and comments and whitespace are not kept.
module Puppy.Syntax
  ( Pos
  , Span
  , Code
  , Associativity(..)
  , TokenSource(..)
  , TokenDecl
  , StartDecl
  , TypeDecl
  , DeriveDecl
  , PrecedenceDecl
  , SymbolRef(..)
  , Element
  , Production
  , Rule
  , Grammar
  , eofToken
  , symbolRefName
  ) where

import Prelude

import Data.Maybe (Maybe)

-- | A point in the source, counted in code units. `line` and `column` are
-- | 1-based, which is what editors and compilers report.
type Pos = { offset :: Int, line :: Int, column :: Int }

-- | A half-open source range: `start` inclusive, `end` exclusive.
type Span = { start :: Pos, end :: Pos }

-- | A fragment of PureScript lifted verbatim out of a grammar file.
-- |
-- | The text is stored exactly as it was written and is never rewritten --
-- | not even to substitute semantic value references, which is why bindings
-- | are named on the right-hand side rather than positional. A `$1` scheme
-- | would mean textual substitution into this string, which cannot tell a
-- | genuine reference from the same characters inside a string literal or a
-- | comment. Instead the generator will emit these characters into a scope
-- | where the binders are already bound.
-- |
-- | Keeping the span is what lets an error the PureScript compiler reports in
-- | the generated file be pointed back at a position in the `.puppy` source.
type Code = { text :: String, span :: Span }

data Associativity
  = AssocLeft
  | AssocRight
  | AssocNone

derive instance Eq Associativity

instance Show Associativity where
  show = case _ of
    AssocLeft -> "AssocLeft"
    AssocRight -> "AssocRight"
    AssocNone -> "AssocNone"

-- | Where the parser's token type comes from.
-- |
-- | Only `GeneratedTokens` exists today: `%token` declarations are the single
-- | source of truth and Puppy emits a closed ADT from them. It is a sum rather
-- | than a bare array of declarations so that a future `%tokentype` -- a token
-- | type the user defines and Puppy merely refers to -- can be added beside it
-- | without disturbing anything that already matches on this.
data TokenSource = GeneratedTokens (Array TokenDecl)

derive instance Eq TokenSource

instance Show TokenSource where
  show (GeneratedTokens decls) = "(GeneratedTokens " <> show decls <> ")"

-- | One `%token` declaration.
-- |
-- | The three names are kept apart on purpose. They coincide by default, but
-- | each answers to a different constraint: `name` is whatever reads well in a
-- | grammar rule, `constructor` has to be a legal PureScript constructor, and
-- | `display` is prose shown to whoever is looking at a parse error.
type TokenDecl =
  { name :: String
  , constructor :: String
  , display :: String
  , payload :: Maybe Code
  -- ^ The `{ Int }` of `%token { Int } INT`; `Nothing` for a token carrying
  -- no value.
  , span :: Span
  }

-- | `%start { Expr } main` -- an entry point, and the type its parser returns.
type StartDecl =
  { symbol :: String
  , resultType :: Code
  , span :: Span
  }

-- | `%type { Expr } expr` -- the semantic value type of a nonterminal.
type TypeDecl =
  { symbol :: String
  , resultType :: Code
  , span :: Span
  }

-- | `%derive Eq Show` -- instances to put on the generated token type.
-- |
-- | Opt-in because they are not free: `Eq` and a `Show` that prints payloads
-- | both demand the same of every payload type, and a token can perfectly well
-- | carry something that has neither.
type DeriveDecl =
  { name :: String
  , span :: Span
  }

-- | `%left`, `%right` or `%nonassoc` over a set of tokens. Priority is
-- | positional: later declarations bind more tightly, so it is the index of
-- | this declaration within the grammar that matters, not anything stored here.
type PrecedenceDecl =
  { associativity :: Associativity
  , tokens :: Array String
  , span :: Span
  }

-- | A reference to a symbol, possibly applied to arguments.
-- |
-- | Arguments are themselves references, so `separated_list(COMMA, expr)`
-- | nests. A plain terminal or nonterminal is just an empty argument list; only
-- | the expansion pass cares about the difference.
data SymbolRef = SymbolRef String (Array SymbolRef)

derive instance Eq SymbolRef

instance Show SymbolRef where
  show (SymbolRef name args) =
    "(SymbolRef " <> show name <> " " <> show args <> ")"

-- | The head of a symbol reference, ignoring its arguments.
symbolRefName :: SymbolRef -> String
symbolRefName (SymbolRef name _) = name

-- | One symbol on the right-hand side of a production, with the name its
-- | semantic value is bound to, if any.
type Element =
  { binder :: Maybe String
  , symbol :: SymbolRef
  , span :: Span
  }

type Production =
  { elements :: Array Element
  , precedence :: Maybe String
  -- ^ The token named by `%prec`, overriding the production's own precedence.
  , action :: Code
  , span :: Span
  }

type Rule =
  { name :: String
  , parameters :: Array String
  , inline :: Boolean
  , productions :: Array Production
  , span :: Span
  }

type Grammar =
  { header :: Maybe Code
  , tokens :: TokenSource
  , starts :: Array StartDecl
  , types :: Array TypeDecl
  , precedences :: Array PrecedenceDecl
  , derives :: Array DeriveDecl
  , rules :: Array Rule
  }

-- | The end-of-input terminal.
-- |
-- | It is never declared and never written in a rule: Puppy reserves the name,
-- | emits a constructor for it alongside the declared tokens, and the generated
-- | wrapper appends it to the token stream before handing it to the driver.
-- | Declaring it or mentioning it in a production is a static error, so that
-- | every grammar ends the same way instead of each inventing its own marker.
eofToken :: { name :: String, constructor :: String, display :: String }
eofToken =
  { name: "EOF"
  , constructor: "EOF"
  , display: "end of input"
  }
