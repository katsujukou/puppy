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
  src/Puppy/Purs/Token.purs    the token type, hand written
  src/Puppy/Purs/CST.purs      the tree, hand written
  src/Puppy/Purs/Parser.pursy  the grammar
  src/Puppy/Purs/Parser.purs   what Puppy made of it
  test/Test/Puppy/Purs.purs    token streams in, trees out
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
| `Parser.pursy` | 815 lines |
| Generated module | 4,859 lines |
| LR states | 641 |
| Terminals / productions | 74 / 396 |
| Time to generate | ~2s |
| Conflicts | 0 unsettled; 68 settled by `%shift` |
| Warnings from the PureScript compiler on the generated module | 0 |

The grammar is all there: types, expressions, binders, declarations, classes,
instances, imports, exports, fixity and roles. Twenty-two tests feed it token
streams and check the trees, including `do` blocks, record updates, class heads
with superclasses and whole modules.

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

This is what the port was for. All 68 conflicts wanted the same thing, shift,
at the same sites Happy handles with `%shift`:

| Reduce that loses | Times | Upstream |
| --- | ---: | --- |
| `expr3 -> expr4` (application continues) | 33 | `expr3 : expr4 %shift` |
| `type4 -> type5` (application continues) | 18 | `type4 : type5 %shift` |
| `type2 -> type3` | 6 | `type2 : type3 %shift` |
| `expr -> expr1` | 6 | `expr : expr1 %shift` |
| six others | 5 | mostly `%shift` too |

Puppy already shifted — that is its default when nothing settles a conflict —
so the generated parser was already correct. What it would not do was generate
without `--allow-conflicts`, which turns a grammar with 68 deliberate decisions
into one with 68 unexplained ones and buries any real conflict that turns up
later.

`%prec` could express it in principle, since the resolution rule is the usual
one. It could not be used here: the reduce that loses to an application is in
conflict with *every token that can start an operand* — 33 of them for `expr3`
— so preferring the shift would have meant giving 33 tokens a precedence level
and changing what they mean everywhere else.

[`%shift`](../../docs/md/grammar.md#shift) was added for this. Seven marks in
the grammar settle 67 of the 68; the last one was a genuine ambiguity of this
port's own — `(Foo a)` after `class` is both a parenthesised constraint and a
one-element list of constraints — and is settled by an eighth. The action table
is byte-for-byte what it was, and the 22 tests are unchanged.

### 2. One `$$` per token

A `%token` pattern gets one hole, so a terminal can capture one value out of
its token. Upstream binds the whole `SourceToken` and destructures it in
Haskell, so its rules reach the position and the payload both.

Two things follow, and both shaped `Token.purs`:

- The source position has to live *inside* each payload rather than in an
  enclosing `Token pos lexeme`, because a pattern `Token _ (TokLower $$)` can
  have the name or the position and not both.
- Qualified and unqualified names have to be separate constructors. Upstream
  writes `TokLowerName [] _` and `TokLowerName _ _`; here the pattern that pins
  the qualifier down is the pattern that can no longer capture the record.

And one thing does not have an answer. A keyword the grammar also accepts as an
identifier — `as`, `role`, `hiding`, `nominal`, `phantom`,
`representational` — is matched by `T.TokLower { name: "as" }`, which pins the
name down and so cannot capture anything. Those names reach the tree without a
position (`C.keyword`), which is the one place in this port where something the
source knew is thrown away.

**A placeholder for "the token itself", alongside `$$` for a part of it, would
close this.**

### 3. `%type` cannot be given to a parameterised rule

`%type` takes nonterminal names, and `many(a)` has no single type to name. The
binders in its generated action come out unannotated, which typechecks by
inference but gives up the checking `%type` exists for.

The way round is a typed wrapper per instantiation, and this grammar has
seventeen of them:

```
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
  / and seeing )` — rather than a state number. On 68 conflicts across 641
  states that is the difference between a report and a wall.
- **Parameterised rules carried the weight.** `many`, `manySep`, `delimited`
  and `layout` are the whole of the boilerplate, the same five-line helpers
  `Parser.y` opens with.
- **The generated module compiles clean.** 4,859 lines, no warnings, under
  `--strict`-shaped settings.
- **It is fast.** About two seconds from grammar to module for 641 states, run
  straight through `node`.

## What is not here

- **A lexer, and the layout pass that follows it.** Puppy does not lex, so the
  tests write layout markers out by hand. Running this on real `.purs` source
  needs a PureScript lexer and a port of `Language.PureScript.CST.Layout` —
  which is the arrangement `Puppy.Runtime.Source.transduce` exists for, and a
  separate piece of work.
- **Source spans on anything but names.** The tree keeps positions where a
  token carried one and nowhere else.
- **Comments.** Upstream threads them through every token so the formatter can
  put them back.
