# The generated module

Puppy writes one module per grammar. It is ordinary PureScript, and it is not
meant to be read — but it is meant to be *used*, and this is its surface.

## What it exports

```purescript
module Puppy.Fixture.Calculator
  ( Token(..)
  , expression
  , expressionFrom
  ) where
```

A `Token` type, and two functions for each `%start` symbol.

Where the grammar declared [`%tokentype`](grammar.md#tokentype) the token type
is already yours, and the module exports the entry points alone:

```purescript
module Language.Parser
  ( expression
  , expressionFrom
  ) where
```

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

None of this happens under [`%tokentype`](grammar.md#tokentype). There is no
generated type and no generated instances; the type is the one you named, and
everything below reads with your type in place of `Token`.

## Entry points

Each `%start` symbol produces two functions. They share one set of tables and
differ only in where the next token comes from.

```purescript
expression :: Array Token -> Either (ParseError Token) Int

expressionFrom
  :: forall m
   . MonadRec m
  => m (Maybe Token)
  -> m (Either (ParseError Token) Int)
```

The result type is what `%start { Int } expression` declared, and the token
type is the `Token` above. Several `%start` symbols share one set of tables and
differ only in where they begin.

The first takes the tokens all at once. The second asks for them one at a time:
`m (Maybe Token)` is an action that produces the next token, or `Nothing` when
there are no more.

Which one to use is a question about your lexer rather than about the grammar.
A lexer that already has an array should hand it over. A lexer reading a file,
a socket, or a long string has nothing to gain by turning all of it into tokens
first, and this is what the second one is for: the parser holds one token at a
time, and every token it has shifted is the lexer's to forget.

### What the action is asked, and when

Once to begin with, and once after every shift. Nothing else runs it: a run of
reductions works on the token the parser is already holding and does not reach
the source at all, so nothing is read ahead and nothing is buffered.

That is not the same as once per token the parser keeps. The last token a
failing parse asks for is the one it rejects — nothing can tell that a token is
wrong before having it — so a parse that shifted two tokens and then failed
asked three times. A parse that ran to the end asked for the `Nothing` in the
same way.

Once there is an answer — accepted, or failed on the token in hand — the action
is not run again. So `Nothing` has to be produced once, at the end, and the
source never has to remember having ended.

### What `m` is for

Whatever your lexer needs to do while lexing, it does in `m`. Failing is the
useful case: the parser has no opinion about a character it was never shown, so
a lexer that meets one it cannot read says so in its own monad, and the parse is
abandoned there rather than being turned into a parse error it is not.

`State` for a lexer walking a string it holds, `ExceptT` over it to be able to
give up, `Effect` or `Aff` to read from somewhere else. `MonadRec` is the single
demand made of `m` — the loop is as long as the input, and it has to be a loop —
and every monad named here has it. There is a whole one
[below](#one-token-at-a-time).

### What this saves, and what it does not

The array goes away: neither the token array nor the copy of it the parser
needs is built. What does not go away is everything else. Your lexer's own
input is still whatever it was — a string in memory is still a string in memory
— and the parser's stacks still grow the way the grammar makes them grow. A
right-recursive rule reads its whole input before it can reduce any of it, so
its stack tracks the input length whichever entry point you use; a
left-recursive one folds as it goes.

What is promised is therefore narrow and worth stating exactly: the hand-off
between lexer and parser is one token wide, and no array of tokens is built.
Total memory is what your lexer holds, plus what the grammar's LR stack needs.

## End of input

There is no `EOF` constructor, and this is deliberate. End of input is not a
token: to the parser it is `Nothing` in a `Maybe Token`, and to a caller it does
not exist at all. An array entry point reaches it by running off the end of the
array; a pulling one by asking for a token and being told there are none.

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

`position` counts the tokens the parser read before it stopped, which for an
array entry point is an index into the array you passed. `found` is the token
there, or `Nothing` at the end of the input. `expected` names the tokens that would not
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
depend on `arrays` or `tailrec` on behalf of a file its author did not write.

It is a small package that does not depend on the generator: building and
shipping a parser does not need Puppy installed.

> **Note** `puppy-runtime` is not in the PureScript registry yet. Until it is,
> name it as an extra package pointing at the subdirectory it lives in — see
> [getting started](getting-started.md#generating).

Anything your own code uses is on top of this. Pattern matching the `Either` a
parser returns means depending on `either` — because your code says `Left`, not
because the generated module does.

The same goes for a token type of your own. Under
[`%tokentype`](grammar.md#tokentype) the generated module imports it, but the
module is in your package and whatever it needs is already on your list; these
two lines are still the whole of what the generated module adds.

## Lexing

Puppy does not produce a lexer. A parser wants tokens and how you build them is
up to you — by hand for a small language, with a library for a larger one. The
two entry points take them two ways:

```purescript
tokenise :: String -> Either String (Array Token)

nextToken :: Lexing (Maybe Token)
```

The first is the whole stream at once, for `expression`. The second is one
token at a time, for `expressionFrom`, and is the one to write when the input
is large enough that holding every token of it at once is not something you
want to do.

The split between lexing and parsing is worth the small inconvenience: lexing
is a separate problem with good separate answers, and tying the two together
would mean Puppy having opinions about regular expressions as well as grammars.

Keep the lexer in its own module. The generated module has no idea the lexer
exists, and a lexer that imports the generated module for its `Token` type is
the right way round.

### One token at a time

A lexer written to be asked never holds more than the text it has not read yet.
Here that text *is* the state, and a token is built when the parser wants one:

```purescript
module Calculator.Lexer (Lexing, nextToken) where

import Prelude

import Calculator.Parser (Token(..))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.State (State)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU

type Lexing = ExceptT String (State String)

nextToken :: Lexing (Maybe Token)
nextToken = do
  rest <- lift (State.gets (SCU.dropWhile (_ == ' ')))
  lift (State.put rest)
  case SCU.uncons rest of
    Nothing -> pure Nothing
    Just { head, tail }
      | head == '+' -> emit tail PLUS
      | head == '*' -> emit tail TIMES
      | head == '(' -> emit tail LPAREN
      | head == ')' -> emit tail RPAREN
      | isDigit head ->
          let
            digits = SCU.takeWhile isDigit rest
          in
            case Int.fromString digits of
              Just n -> emit (SCU.drop (SCU.length digits) rest) (INT n)
              Nothing -> throwError ("not a number: " <> digits)
      | otherwise -> throwError ("unexpected character " <> show head)
  where
  emit rest token = do
    lift (State.put rest)
    pure (Just token)

  isDigit c = c >= '0' && c <= '9'
```

That one wants `transformers`, on top of what the lexing itself uses. Running
it against the parser is a single pass over the text:

```purescript
calculate :: String -> Either String Int
calculate input =
  case State.evalState (runExceptT (expressionFrom nextToken)) input of
    Left lexError -> Left lexError
    Right (Left parseError) -> Left ("parse error at " <> show parseError.position)
    Right (Right value) -> Right value
```

```purescript
> calculate "1 + 2 * 3"
(Right 7)

> calculate "1 +"
(Left "parse error at 2")

> calculate "1 @ 2"
(Left "unexpected character '@'")
```

Notice where the lexer's complaint went. It never became a parse error, because
it is not one — the parser has no opinion about a character it was never shown
— and `ExceptT` carried it out on its own.

## Do not edit it

The header says so, and it means it: the file is rewritten every time the
grammar is. Anything you want in there goes in the grammar's header block, and
anything that does not belong in the grammar belongs in a module beside it.
