# The generated module

Puppy writes one module per grammar. It is ordinary PureScript, and it is not
meant to be read — but it is meant to be *used*, and this is its surface.

## What it exports

```purescript
module Puppy.Fixture.Calculator
  ( Token(..)
  , expression
  ) where
```

A `Token` type, and one function for each `%start` symbol.

## The token type

```purescript
data Token
  = PLUS
  | TIMES
  | LPAREN
  | RPAREN
  | INT (Int)
```

One constructor per `%token`, in declaration order, carrying whatever payload
type it was declared with. Building these is your job — see
[lexing](#lexing), below.

`Eq` is there if [`%derive`](grammar.md#derive) asked for it, and not
otherwise. `Show` is always there: without `%derive Show` it names the
constructor and ignores the payload, which asks nothing of the payload types;
with it, payloads are printed too.

## Entry points

```purescript
expression :: Array Token -> Either (ParseError Token) Int
```

The argument type of the array is the `Token` above; the result is what
`%start { Int } expression` declared. Several `%start` symbols share one set of
tables and differ only in where they begin.

## End of input

There is no `EOF` constructor, and this is deliberate. End of input is not a
token: to the parser it is `Nothing` in the `Maybe Token` the entry point
appends for you, and to a caller it does not exist at all.

The alternative — a constructor you could write — means a caller can put one in
the middle of the input, at which point the parser stops there and the rest is
silently ignored. Puppy makes that unrepresentable rather than merely
discouraged.

One consequence is visible: a `ParseError` that landed on the end of input
reports `found: Nothing`, because there is no token there to name.

## Parse errors

```purescript
import Puppy.Runtime (ParseError)
```

`Puppy.Runtime` is the whole of the runtime package's surface, and this is the
one name in it you are likely to want: the generated module exports its entry
points and its `Token`, but the error type belongs to the runtime.

```purescript
type ParseError tok =
  { position :: Int
  , found :: Maybe tok
  , state :: Int
  , expected :: Array String
  }
```

`position` indexes the array you passed. `found` is the token there, or
`Nothing` at the end of the input. `expected` names the tokens that would not
have failed immediately, using the display names from `%token`. `state` is the
automaton state, which is of no use except when comparing notes with a conflict
report.

`expected` is the approximation every LR parser reports: it can name a token
that a later reduction would have rejected, but it never leaves out one that
would have worked.

## What your package must depend on

```yaml
dependencies:
  - prelude
  - puppy-runtime
```

`puppy-runtime` holds the table-driven parser the generated module calls, the
box its semantic values travel in, and — in `Puppy.Runtime.Deps` — the handful
of standard-library names the generated code itself uses. That last one is why
this list is two lines: a package holding a generated parser should not have to
depend on `arrays` on behalf of a file its author did not write.

It is a small package that does not depend on the generator: building and
shipping a parser does not need Puppy installed.

> **Note** `puppy-runtime` is not in the PureScript registry yet. Until it is,
> name it as an extra package pointing at the subdirectory it lives in — see
> [getting started](getting-started.md#generating).

Anything your own code uses is on top of this. Pattern matching the `Either` a
parser returns means depending on `either` — because your code says `Left`, not
because the generated module does.

## Lexing

Puppy does not produce a lexer. A parser takes `Array Token`, and how you
build one is up to you — by hand for a small language, with a library for a
larger one.

```purescript
tokenise :: String -> Either String (Array Token)
tokenise = ...
```

The split is worth the small inconvenience: lexing is a separate problem with
good separate answers, and tying the two together would mean Puppy having
opinions about regular expressions as well as grammars.

Keep the lexer in its own module. The generated module has no idea the lexer
exists, and a lexer that imports the generated module for its `Token` type is
the right way round.

## Do not edit it

The header says so, and it means it: the file is rewritten every time the
grammar is. Anything you want in there goes in the grammar's header block, and
anything that does not belong in the grammar belongs in a module beside it.
