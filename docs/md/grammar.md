# The grammar file

A grammar lives in a file ending `.pursy` and has two halves separated by `%%`:
declarations first, then rules.

```plain
%token PLUS
%token { Int } INT

%start { Int } total

%%

total:
  | i = INT                { i }
  | a = total PLUS b = INT { a + b }
```

Comments are PureScript's: `--` to the end of the line, and `{- ... -}`, which
nests.

## Declarations

Declarations may appear in any order before `%%`, with one exception: the
header, if there is one, comes first. Their order otherwise matters only within
a kind — `%token` fixes the order tokens are numbered in, and the precedence
declarations rank against one another by position.

### The header

PureScript copied verbatim into the generated module, after the imports Puppy
writes for itself and before everything else. This is where the imports your
semantic actions need go.

It must be the first thing in the grammar file: the parser looks for it once,
before reading any declarations.

```plain
%{
import Data.List (List(..), (:))
import Ast (Expr(..))
%}
```

Imports and top-level declarations, within two limits.

The module declaration is not yours to write — Puppy has already written one by
the time the header is inserted — and so is `import Prelude`, so repeating that
one is a duplicate import (a warning, and an error under `--strict`).

What Puppy needs for itself it imports under a `Puppy.` prefix:

```purescript
import Puppy.Runtime as Puppy.Runtime
import Puppy.Runtime.Deps as Puppy.Deps
```

Those names are prefixed so that yours are free: a header may import
`Data.Array as Array` and use `Array.snoc` in an action without colliding with
anything Puppy wrote.

The header ends at the first literal `%}`, wherever that falls. One inside a
string or a comment ends it just the same, so a header cannot contain that
sequence at all.

### `%token`

```plain
%token PLUS TIMES
%token { Int } INT
```

A string after a name is what parse errors call it:

```plain
%token PLUS "+" TIMES "*"
```

Each name becomes a constructor of the generated `Token` type, so it must begin
with a capital letter.

A `{ ... }` before the names gives them a payload type: `%token { Int } INT`
produces `INT Int`, and a rule that mentions `INT` has an `Int` to bind. Tokens
declared together share a payload type; declare them separately to give them
different ones.

Without one a token is called by its own name, which is usually right for `INT`
and rarely right for `PLUS`.

`EOF` is not a token and cannot be declared. See
[end of input](generated.md#end-of-input).

### `%start`

```plain
%start { Expr } main
%start { Array Stmt } program statements
```

Each name becomes an exported function in the generated module, so it must
begin with a lower case letter and must not be a name the generated code
already uses. The `{ ... }` is the type that entry point returns.

A start symbol must be a rule in the same file, must take no parameters, and
must not be `%inline` — there would be nothing left to start from.

### `%type`

```plain
%type { Expr } expr
%type { Array Stmt } stmts block
```

Optional, and worth writing. Where Puppy knows a nonterminal's type it
annotates the bindings and results in the generated code, so a mistake in a
semantic action is reported against the rule that has it rather than somewhere
further along.

### `%left`, `%right`, `%nonassoc`

```plain
%left PLUS MINUS
%left TIMES DIVIDE
%right POWER
%nonassoc EQUALS
```

Each declaration names one or more tokens and gives them a rank. **Later
declarations bind more tightly**: with the above, `TIMES` outranks `PLUS`, and
`POWER` outranks both.

Within a rank, the keyword decides: `%left` reduces (so `a - b - c` is
`(a - b) - c`), `%right` shifts (so `a ^ b ^ c` is `a ^ (b ^ c)`), and
`%nonassoc` makes the token an error there, so `a == b == c` does not parse.

A token may be given a precedence once.

### `%derive`

```plain
%derive Eq Show
```

Puppy knows how to derive `Eq` and `Show`, and they work differently.

`Eq` is only there if asked for. A token can carry anything, including
something with no `Eq` at all, so assuming it would make such a grammar
impossible to generate from.

`Show` is always there. Without `%derive Show` it names the constructor and
ignores the payload, which asks nothing of the payload types; with it, payloads
are printed too, and they need `Show` of their own.

An instance has to live in the module its type does, so neither of these can be
added by a caller afterwards. That is why `%derive` exists rather than being
left to whoever needs it.

## Rules

A rule is a name, a colon, and one or more alternatives. The leading `|` is
optional on the first; a trailing `;` is allowed.

```plain
expr:
  | i = INT                 { i }
  | a = expr PLUS b = expr  { a + b }
  | LPAREN e = expr RPAREN  { e }
```

Each alternative is a sequence of symbols followed by a semantic action.

### Binders

`name = symbol` binds that symbol's value for the action. Symbols without a
binder have no name and no value in scope — which is what you want for
punctuation.

There is no `$1`. Positional references would mean substituting text into your
action, and nothing that substitutes text can tell a real reference from the
same characters inside a string literal or a comment. Named binders are put in
scope around your code instead, and your code is never rewritten.

A binder must be a legal PureScript value name.

### Semantic actions

The `{ ... }` after the symbols is PureScript, reproduced exactly. It is an
expression, evaluated with the alternative's binders in scope.

Puppy finds the end of an action by counting braces, skipping the ones inside
string literals, character literals, `--` comments and `{- -}` comments. Braces
are reserved punctuation in PureScript and can never be part of an operator, so
this finds the end exactly rather than approximately.

One consequence: an action cannot begin with `-` immediately after the brace,
because `{-` opens a comment. Write `{ (-1) }` or `{ -1 }`, not `{-1}`.

### `%prec`

```plain
expr:
  | MINUS e = expr %prec UMINUS { negate e }
```

An alternative's precedence is normally that of its last terminal. `%prec`
names a different token to take it from, which is how a unary minus is made to
bind more tightly than the binary one it shares a token with.

The named token must be declared.

### `%inline`

```plain
%inline op:
  | PLUS  { (+) }
  | TIMES { (*) }

expr:
  | a = expr f = op b = expr { f a b }
```

An `%inline` rule is not a rule of the finished grammar. Every place it is used
is replaced by each of its alternatives, which multiplies that use site by the
number of alternatives. Its semantic action is folded in too, so `f` above is
bound to whichever operator was matched.

Its main use is removing conflicts. A rule used where the parser must decide
something before it can be reduced is a common source of them, and folding it
away moves the decision to a point where the parser has seen enough to make it.

An `%inline` rule may not reach itself, directly or through another, and may
not be a start symbol.

Bindings survive inlining without being renamed: the inlined rule's action runs
in a scope holding exactly its own bindings, so an `x` it bound and an `x` at
the use site never meet.

### Parameterised rules

A rule may take parameters, which stand for symbols:

```plain
separated(sep, X):
  |                                   { [] }
  | x = X rest = tail(sep, X)         { [ x ] <> rest }

tail(sep, X):
  |                                   { [] }
  | sep x = X rest = tail(sep, X)     { [ x ] <> rest }

args:
  | xs = separated(COMMA, expr) { xs }
```

Each distinct argument list makes a separate nonterminal: `separated(COMMA,
expr)` and `separated(SEMI, stmt)` are two rules, generated on demand from the
start symbols. A parameterised rule nothing reaches is checked but not
generated.

A parameter may itself be applied, so a rule can be passed to another:

```plain
apply(F, X):
  | y = F(X) { y }
```

Puppy has no standard library of these. `list`, `option` and their relatives
are five lines each and yours to name.

## Names

A grammar's names end up in different places, and each place has its own rules.

| Where it came from | What it becomes | Must be |
| --- | --- | --- |
| `%token` name | a data constructor | upper case first |
| `%start` symbol | an exported function | lower case first, not a name the generated code uses |
| binder | a `let` binding | lower case first, not a reserved word |

Puppy checks these against the grammar rather than letting the PureScript
compiler complain about generated code nobody wrote.

## Limits

Some grammars ask for more than can be given. Rather than run until memory is
gone, Puppy stops with an explanation. These are not settings; if you meet one,
something is usually wrong with the grammar.

| Limit | Value | What it guards |
| --- | ---: | --- |
| Argument nesting | 50 | A parameterised rule that uses itself with a *larger* argument has no finite set of instances |
| Rule instances | 10,000 | The same, by another route |
| Productions after inlining | 50,000 | Each `%inline` symbol multiplies a production by that rule's alternatives |
| Inlining work | 10,000,000 | A long chain of single-alternative inlining is rewritten end to end at every step |
| LR states | 50,000 | |
| Merge comparisons | 20,000,000 | Deciding whether two large states may merge is quadratic in their size |

## Next Step

Now that we understand the grammar DSL, [let's explore the generated parser module in more depth as our next step](./generated.md).
