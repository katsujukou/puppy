-- | The shape of a `.pursy` grammar file, as written.
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
  , ConflictDirective(..)
  , Span
  , Code
  , Associativity(..)
  , TokenSource(..)
  , TokenPattern
  , TokenValue(..)
  , ExternalToken
  , TokenDecl
  , declaredTokens
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
-- | the generated file be pointed back at a position in the `.pursy` source.
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

-- | A `{ ... }` fragment that may contain `$$`.
-- |
-- | The text is the author's, verbatim, exactly as `Code` promises. `holes` is
-- | where the `$$` placeholders were, as spans into the same source -- so the
-- | generator can write the text out with something in their place without
-- | rewriting a single character of the rest.
-- |
-- | Finding them is the lexer's job and not a search over this text, because a
-- | `$$` inside a string literal or a comment is not a placeholder and no
-- | amount of looking at the finished string can tell the difference.
type TokenPattern = { code :: Code, holes :: Array Span }

-- | Where the parser's token type comes from.
-- |
-- | `GeneratedTokens` is the original arrangement: `%token` declarations are the
-- | single source of truth and Puppy emits a closed ADT from them.
-- |
-- | `ExternalTokens` is a type the author already has. Puppy never names its
-- | constructors; it is handed a pattern per terminal and matches with those,
-- | which is what lets a token type carry things this grammar has no opinion
-- | about -- comments, whitespace, a source span wrapped around everything.
-- |
-- | Which mode a grammar is in is settled once, when the declarations are
-- | assembled, and the two carry different things because they need different
-- | things: a pattern is meaningless without an external type to match against,
-- | and a generated constructor is meaningless with one.
data TokenSource
  = GeneratedTokens (Array TokenDecl)
  | ExternalTokens
      { tokenType :: Code
      , tokens :: Array ExternalToken
      }

-- | Where a terminal's semantic value comes from, in external mode.
-- |
-- | The two are exclusive, and the parser is what makes them so: a pattern
-- | either says which part of the token the value is or the grammar says the
-- | value is the token, and writing both marks is refused where both are still
-- | in front of it.
data TokenValue
  = FromPattern
  -- ^ The value is whatever `$$` stands for, or nothing at all if the pattern
  -- has no `$$`. Which of those it is depends on the declared payload type, and
  -- checking that the two agree is a later pass's job.
  | FromToken Span

-- ^ `@` before the pattern: the value is the whole token, whose type is the one
-- `%tokentype` named. The span is the `@` itself, so a diagnostic can point at
-- the mark rather than at the pattern it applies to.

derive instance Eq TokenValue

instance Show TokenValue where
  show = case _ of
    FromPattern -> "FromPattern"
    FromToken span -> "(FromToken " <> show span <> ")"

-- | A `%token` in external mode: the declaration, the pattern that picks it out
-- | of the author's own type, and what of the token the value is.
type ExternalToken =
  { decl :: TokenDecl
  , pattern :: TokenPattern
  , value :: TokenValue
  }

derive instance Eq TokenSource

instance Show TokenSource where
  show = case _ of
    GeneratedTokens decls -> "(GeneratedTokens " <> show decls <> ")"
    ExternalTokens spec -> "(ExternalTokens " <> show spec <> ")"

-- | The declarations, whichever mode they came from.
-- |
-- | Almost everything downstream wants a terminal's name, display name and
-- | payload type and nothing else; only the generator cares how the token is
-- | recognised.
declaredTokens :: TokenSource -> Array TokenDecl
declaredTokens = case _ of
  GeneratedTokens decls -> decls
  ExternalTokens spec -> map _.decl spec.tokens

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

-- | What a production says about the conflicts it takes part in.
-- |
-- | The three are exclusive by construction rather than by convention. A
-- | production annotated both `%prec` and `%shift` would be asking for a
-- | precedence and for its precedence to be ignored, so the parser refuses it
-- | and nothing downstream has to decide which of the two it meant.
data ConflictDirective a
  = Inferred
  -- ^ Nothing was written. A production takes the precedence of its last
  -- terminal, and settles a conflict only if that terminal has one.
  | Prec a
  -- ^ `%prec TOKEN`: take that token's precedence instead.
  | PreferShift

-- ^ `%shift`: lose every shift/reduce conflict this production is in, so that
-- the shift wins deliberately rather than by the rule of thumb. Says nothing
-- about reduce/reduce, where there is no shift to prefer.

derive instance Eq a => Eq (ConflictDirective a)

derive instance Functor ConflictDirective

instance Show a => Show (ConflictDirective a) where
  show = case _ of
    Inferred -> "Inferred"
    Prec a -> "(Prec " <> show a <> ")"
    PreferShift -> "PreferShift"

type Production =
  { elements :: Array Element
  , directive :: ConflictDirective String
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
