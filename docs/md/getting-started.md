# Getting started

A calculator, from grammar to working parser. The grammar below is the one in
Puppy's own test fixture, so it is built and run on every commit.

## The grammar

Put this in `src/Calculator/Parser.pursy`:

```plain
%token PLUS TIMES LPAREN RPAREN
%token { Int } INT

%derive Eq Show

%start { Int } expression

%type { Int } expr

%left PLUS
%left TIMES

%%

expression:
  | e = expr { e }

expr:
  | i = INT                 { i }
  | a = expr PLUS b = expr  { a + b }
  | a = expr TIMES b = expr { a * b }
  | LPAREN e = expr RPAREN  { e }
```

Reading it from the top:

`%token` names the tokens. `INT` carries an `Int`; the rest carry nothing.

`%derive Eq Show` puts those instances on the generated `Token` type. They are
not free — every payload type needs them too — so they are asked for rather
than assumed.

`%start { Int } expression` makes `expression` an exported function returning
`Int`. `%type { Int } expr` says what `expr` means, which lets Puppy annotate
the generated code so that a mistake in a semantic action is reported here
rather than somewhere further along.

`%left PLUS` then `%left TIMES` gives both tokens a rank and makes them
left-associative. **Later declarations bind more tightly**, so `1 + 2 * 3` is
`1 + (2 * 3)`. Without these two lines the grammar is ambiguous and Puppy says
so — see [conflicts](conflicts.md).

Then `%%`, and the rules. `i = INT` binds that token's payload for the action
that follows in braces. The action is PureScript, reproduced exactly, evaluated
with the binders in scope.

## Generating

```sh
puppy -p my-package
```

```plain
src/Calculator/Parser.pursy -> Calculator.Parser
```

The module name comes from where the file sits under `src`, and
`src/Calculator/Parser.purs` appears beside it. Commit that file: it is what
gets compiled.

Your package needs `puppy-runtime` and the handful of packages the generated
module imports:

```yaml
dependencies:
  - arrays
  - either
  - maybe
  - prelude
  - puppy-runtime
```

That is what the *generated* module needs. Everything else your package uses is
on top of it — the lexer below wants `integers` and `strings` as well.

> **Note** `puppy-runtime` is not in the PureScript registry yet. Until it is,
> name it as an extra package pointing at the subdirectory it lives in:
>
> ```yaml
> workspace:
>   extraPackages:
>     puppy-runtime:
>       git: https://github.com/katsujukou/puppy.git
>       ref: v0.1.0
>       subdir: puppy-runtime
> ```
>
> Pin `ref` to a tag rather than a branch, so that a build stays what it was.
> When `puppy-runtime` reaches the registry this block comes out and nothing
> else changes.

## A lexer

Puppy does not write one. The parser takes an `Array Token`, and turning text
into tokens is yours:

```purescript
module Calculator.Lexer (tokenise) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Calculator.Parser (Token(..))

tokenise :: String -> Either String (Array Token)
tokenise input = go [] (SCU.toCharArray input)
  where
  go acc chars = case Array.uncons chars of
    Nothing -> Right acc
    Just { head, tail }
      | head == ' ' -> go acc tail
      | head == '+' -> go (Array.snoc acc PLUS) tail
      | head == '*' -> go (Array.snoc acc TIMES) tail
      | head == '(' -> go (Array.snoc acc LPAREN) tail
      | head == ')' -> go (Array.snoc acc RPAREN) tail
      | isDigit head ->
          let
            digits = SCU.fromCharArray (Array.takeWhile isDigit chars)
          in
            case Int.fromString digits of
              Just n -> go (Array.snoc acc (INT n)) (Array.dropWhile isDigit chars)
              Nothing -> Left ("not a number: " <> digits)
      | otherwise -> Left ("unexpected character " <> show head)

  isDigit c = c >= '0' && c <= '9'
```

Note which way the import goes: the lexer imports the generated module for its
`Token` type, and the generated module has no idea the lexer exists.

## Running it

```purescript
> tokenise "1 + 2 * 3" >>= expression
Right 7

> tokenise "(1 + 2) * 3" >>= expression
Right 9
```

`1 + 2 * 3` is 7 and not 9, which is `%left TIMES` coming after `%left PLUS`
doing its job.

## When the input is wrong

```purescript
> tokenise "1 +" >>= expression
Left { position: 2, found: Nothing, state: 5, expected: ["LPAREN","INT"] }
```

`position` indexes the token array. `found: Nothing` means the input ran out —
end of input is not a token, so there is nothing there to name. `expected` uses
the display names from `%token`, which is why they read as `LPAREN` and `INT`
here; give them better ones and the errors improve:

```plain
%token LPAREN "(" RPAREN ")"
%token { Int } INT "a number"
```

## Where next

- [The grammar file](grammar.md) for everything a `.pursy` may contain,
  including parameterised rules and `%inline`.
- [The generated module](generated.md) for the exact shape of what comes out.
- [Conflicts](conflicts.md) for when the grammar says two things at once.
