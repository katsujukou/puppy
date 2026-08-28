# PureScript, in Puppy

A port of the PureScript compiler's own grammar — `src/Language/PureScript/CST/Parser.y`,
a Happy grammar — to a Puppy `.pursy` file. It is here to find out what Puppy
does with a real grammar rather than a made-up one.

It is an example, not something that ships. It is built and run by a `stress`
job of its own in CI, separately from the packages that do: a grammar this size
is a regression test worth keeping green, and a change to it should not hold up
anything else.

```
example/purescript/
  src/Puppy/Purs/Parser.pursy  the grammar
  src/Puppy/Purs/Parser.purs   what Puppy made of it
  src/Puppy/Purs/CST.purs      the tree it builds, hand written
  src/Puppy/Purs/Run.purs      lexer in, tree out
  test/Test/Puppy/Purs.purs    source text in, trees out
  test/Test/Puppy/Purs/Bench.purs   the parsers, same file
```

Regenerating:

```sh
spago run -p puppy-cli -- example/purescript/src/Puppy/Purs/Parser.pursy \
  -m Puppy.Purs.Parser -o example/purescript/src/Puppy/Purs/Parser.purs
```

## How it went

| | |
| --- | ---: |
| `Parser.y` | 811 lines |
| `Parser.pursy` | 836 lines |
| Generated module | 487 KB |
| LR states | 650 |
| Terminals / productions | 76 / 402 |
| Time to generate | ~2s |
| Conflicts | 0 unsettled; 70 settled by `%shift` |
| Warnings from the PureScript compiler on the generated module | 0 |

The grammar is all there: types, expressions, binders, declarations, classes,
instances, imports, exports, fixity and roles.

It reads real source. The tokens come from
[`purescript-language-cst-parser`](https://github.com/natefaubion/purescript-language-cst-parser)
— its lexer, its offside rule, its `SourceToken` — and `Puppy.Purs.Run` is the
twenty lines that join the two. Nothing here lexes and nothing here works out
where a block begins; Puppy does neither, and this is what borrowing them looks
like.

```purescript
parseModule :: String -> Parsed C.Module
parseModule = runWith lexModule P.parseModuleFrom
```

## Against the hand-written parser

Both sides lex with the same lexer, so the cost of that is in both numbers and
is measured on its own as well.

| file | lex | puppy | puppy, recovering | language-cst-parser |
| --- | ---: | ---: | ---: | ---: |
| `Codegen.purs` (43 KB) | 14.0ms | 29.3ms | 29.9ms | 28.6ms |
| `Expand.purs` (39 KB) | 14.1ms | 29.5ms | 29.0ms | 27.6ms |
| `Driver.purs` (26 KB) | 5.9ms | 12.3ms | 12.3ms | 10.9ms |

Read the columns against each other and not against another machine. These are
milliseconds on the one this was run on, and an earlier run of the same three
files on the same machine put every column at about six tenths of what is here
— `language-cst-parser` included, which is the arm nothing in this repository
has touched. What holds across both runs is the ratio, and that is the figure
the rest of this section is about.

Best of ten samples. A sample is however many repetitions come to at least
200ms, divided back down: `Date.now` counts whole milliseconds, and a single
parse of a small file is few enough of them that the difference between two
figures would otherwise be mostly rounding. How many repetitions that is comes
from measuring rather than estimating -- twice, so that the count is settled on
warm code rather than on the cold run -- and the benchmark prints the shortest
sample it took, so that the floor can be seen to have held.

End to end Puppy is between two and thirteen per cent longer, and widest on the
smallest of the three. Taking the lexer out -- which is approximate, since the
two hold the tokens differently -- the parsing itself is roughly 15.3ms against
14.0ms on the larger two, under a tenth longer.

It was half as long again until recently, and what closed most of the gap was
not the algorithm. `resume` worked out which terminal the lookahead was on
every pass of its reduction loop, and a grammar with levels in it goes round
that loop half a dozen times per token; asking once instead took a fifth off
the parsing. Which is worth saying because it is the kind of thing that is left
in a young implementation and not the kind of thing that is inherent to LR.

Two things to hold against that, in opposite directions. The tree here is
smaller: `PureScript.CST` keeps every token it read, comments and all, because
a formatter has to put the source back, and this one keeps names, shapes and
positions. So Puppy is doing less work for that fifth. On the other hand a
generated table-driven parser landing in the same range as a hand-written
combinator parser that has been tuned for years is closer than the shape of the
two would suggest.

## What recovery costs

Nothing that shows. The fourth column is `parseModuleRecovering` over the same
three files, every one of which parses cleanly — so what is priced there is
carrying a recovery path, not using one. It lands within two per cent of the
entry point that has none, in both directions across the three files, which is
inside the scatter of the `language-cst-parser` column beside it.

The table it reads from grew, and that was measured on its own. Generating the
grammar with the three `ERROR` alternatives and without them gives 650 states
against 647 and a row of 76 against 75 — 1.8% more table — and timing the two
generated parsers against each other separates them by no more than 2.5%, in no
consistent direction, on a benchmark whose untouched arm moves by 4% between
runs. Three recovery points cost three states and one column, and no time.

Which is what declaring the points rather than searching for them is for. A
recovering parse is the same loop until something goes wrong; the extra work is
at the point where a parse would otherwise have stopped, and a file with
nothing wrong with it never arrives there.

Both sides are timed on clean input only. `language-cst-parser` can carry on
past a failure as well, and timing one side's recovery against the other's is a
different measurement than this one.

## Where the upstream grammar is not LR(1)

Three parts of `Parser.y` are not an LR grammar at all, and say so. They are
parsed by invoking `%partial` parsers by hand and backtracking with `tryPrefix`:

- **`do` and `ado` statements.** `Foo a b c` is either a constructor pattern or
  three applications, and nothing says which until a `<-` or a layout separator
  arrives. Upstream's comment: *"require unbounded lookahead due to many
  conflicts between `binder` and `expr` syntax"*.
- **Pattern guards.** The same ambiguity, in `| x <- e`.
- **Class heads.** `class (a :: B) <= C` — `(a :: B)` is a row inside a
  constraint if `B` is a type, and a kinded type variable if `B` is a kind. Only
  a later `<=`, `where` or layout token decides.

Puppy has no backtracking and no `%partial`, so all three are rewritten here.
The rewrite is the same in each case, and it is the one upstream's own comment
describes and declines:

> One way to resolve this would be to parse a `binder` as an `expr` and then
> reassociate it after the fact. However this means we can't use the `binder`
> productions to parse it, so we'd have to maintain an ad-hoc handwritten parser
> which is very difficult to audit.

So `doStatement` is `expr | expr '<-' expr`, and `C.toBinder` turns the left
side into a binder afterwards. `classHeadPrefix` parses one shape and
`C.typeToVarBinding` reads the type atoms as type variables. Both are LR(1) and
neither needs a token of lookahead beyond one.

The objection stands: `toBinder` is a hand-written reassociation, and it is
where a bug in this port would hide. That is the price of not having
backtracking, and it is worth knowing that the price is real.

## What Puppy is missing

Found by doing this, in the order they cost the most.

### 1. ~~No way to prefer a shift~~ — added

This is what the port was for. All 70 conflicts wanted the same thing, shift,
at the same sites Happy handles with `%shift`:

| Reduce that loses | Times | Upstream |
| --- | ---: | --- |
| `expr3 -> expr4` (application continues) | 33 | `expr3 : expr4 %shift` |
| `type4 -> type5` (application continues) | 18 | `type4 : type5 %shift` |
| `type2 -> type3` | 7 | `type2 : type3 %shift` |
| `expr -> expr1` | 7 | `expr : expr1 %shift` |
| five others | 5 | mostly `%shift` too |

Puppy already shifted — that is its default when nothing settles a conflict —
so the generated parser was already correct. What it would not do was generate
without `--allow-conflicts`, which turns a grammar with 70 deliberate decisions
into one with 70 unexplained ones and buries any real conflict that turns up
later.

`%prec` could express it in principle, since the resolution rule is the usual
one. It could not be used here: the reduce that loses to an application is in
conflict with *every token that can start an operand* — 33 of them for `expr3`
— so preferring the shift would have meant giving 33 tokens a precedence level
and changing what they mean everywhere else.

[`%shift`](../../docs/md/grammar.md#shift) was added for this. Eight marks in
the grammar settle 69 of the 70; the last one is a genuine ambiguity of this
port's own — `(Foo a)` after `class` is both a parenthesised constraint and a
one-element list of constraints — and is settled by a ninth. The action table
is byte-for-byte what it was, and the tests the port had at the time were
unchanged.

### 2. ~~One `$$` per token~~ — `@` added

A `%token` pattern gets one hole, so a terminal could capture one value out of
its token and no more. That is fine for `T.TokInt _ $$` and useless for a
pattern that has already spent itself saying which token it is: `TokLowerName
Nothing "as"` pins the name down, and pinning it down is what leaves nowhere
for a `$$` to stand.

The port paid for it twice. The token type had to keep the source position
*inside* each payload rather than in an enclosing constructor, and qualified
and unqualified names had to be separate constructors — because in both cases
the pattern that pins one field down is the pattern that can no longer capture
the record it is in. And a keyword the grammar also accepts as an identifier —
`as`, `role`, `hiding` — reached the tree with no position at all, rebuilt from
a constant.

[`@`](../../docs/md/grammar.md#-for-the-whole-token) closed all of it. A
terminal marked `@` has the whole token for its value, so:

```plain
%token AS    "as"     @ { { value: T.TokLowerName Nothing "as" } }
%token LOWER "a name" @ { { value: T.TokLowerName Nothing _ } }
%token QUAL_LOWER "a qualified name" @ { { value: T.TokLowerName _ _ } }
```

Every terminal in this grammar is now marked `@`, the hand-written token type
is gone entirely, and the tree is built out of
`purescript-language-cst-parser`'s own `SourceToken`. The name rules collapsed
to one line each:

```plain
ident:
  | t = LOWER { C.name t }
  | t = AS    { C.name t }
```

Nothing is rebuilt from a constant and nothing loses its position.

### 3. `%type` cannot be given to a parameterised rule

`%type` takes nonterminal names, and `many(a)` has no single type to name. The
binders in its generated action come out unannotated, which typechecks by
inference but gives up the checking `%type` exists for.

The way round is a typed wrapper per instantiation, and this grammar has
seventeen of them:

```plain
%type { Array C.Type } typeAtoms

typeAtoms:
  | ts = manyOrEmpty(typeAtom) { ts }
```

Which works, and reads fine, but it is seventeen rules that exist only to carry
an annotation.

### 4. The entry point is named after the start symbol

Upstream writes `%name parseType type`: the rule is `type`, the function is
`parseType`. Puppy names the function after the rule, and `type` is a
PureScript keyword, so each entry point here is a rule of its own that does
nothing but wrap another one. Cheap, but four rules of nothing.

### 5. Semantic actions cannot fail

83 of upstream's actions are monadic and several of them fail: a name that is
not allowed, a `=` where a record literal wanted `:`, a wildcard in a type
signature. Puppy's actions are ordinary expressions.

Everything survives, but it moves. Checks that can be made later are recorded
in the tree (`C.ExprError`, `C.BinderError`); checks that were really
reinterpretations become pure functions (`C.recordApplyOrUpdate`,
`C.binderConstructor`, `C.letBinding`). Arguably this is the better
arrangement — the grammar stays a grammar — but it is a rewrite, not a port,
and a language wanting to *stop* at the first bad name cannot.

### 6. No `%partial`

Upstream exposes thirteen partial parsers, which its incremental module
parsing and its own backtracking are built on. Nothing here needs them because
nothing here backtracks, but a tool that wanted to parse a module header alone
would.

## What went better than expected

- **Pager over LALR earns its keep.** `type3 : type3 qualOp type4` is written
  left-recursive here, exactly as upstream has it, and produces no conflict.
  Several of upstream's `%shift` annotations are for conflicts that Puppy's
  automaton does not have.
- **The conflict reports are readable on a real grammar.** Every one names a
  concrete input that reaches the state — `after reading class ( a constructor
  / and seeing )` — rather than a state number. On 70 conflicts across 650
  states that is the difference between a report and a wall.
- **Parameterised rules carried the weight.** `many`, `manySep`, `delimited`
  and `layout` are the whole of the boilerplate, the same five-line helpers
  `Parser.y` opens with.
- **The generated module compiles clean.** 4,859 lines, no warnings, under
  `--strict`-shaped settings.
- **It is fast.** About two seconds from grammar to module for 650 states, run
  straight through `node`.

## What is not here

- **A lexer, and the offside rule that follows it.** Puppy writes neither, and
  this borrows both from `purescript-language-cst-parser`. That is the intended
  arrangement rather than a gap — but it does mean the comparison above is a
  comparison of parsers and not of front ends.
- **The same tree.** `PureScript.CST.Types` keeps every token, comment and
  range, because a formatter needs them; this keeps names, shapes and the
  position of every name. Building the first from this grammar is what a real
  compatibility claim would need, and it is a bigger piece of work than the
  grammar was.
- **Recovery anywhere but a declaration, a `let` binding or a `do` statement.**
  Those are the three the grammar declares, and they are the three the layout
  pass has already marked the ends of. `PureScript.CST` carries on in more
  places than that, and a parse that goes wrong inside a type or an expression
  gives up here where it would not there.
- **A shebang anywhere but a module.** `parseModule` lexes with `lexModule`,
  which allows for one; the entry points that take a fragment use `lex`, which
  does not, because a fragment cannot have one.
