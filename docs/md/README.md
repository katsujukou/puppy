# Introduction

Puppy turns a grammar into a PureScript parser. You write the grammar and what
each rule means; Puppy works out the parsing table and writes a module.

## Reading order

- **[Getting started](getting-started.md)** — a calculator, from grammar to
  working parser.
- **[The grammar file](grammar.md)** — everything a `.pursy` file may contain.
- **[The generated module](generated.md)** — what comes out, and how to use it.
- **[The command line](cli.md)** — running Puppy over a project.
- **[Conflicts](conflicts.md)** — what Puppy is telling you, and what to do
  about it.

## What Puppy is and is not

Puppy is a *parser* generator. It does not produce a lexer: you supply the
tokens — all at once, or one at a time as the parser asks for them — and it
tells you what they mean. Splitting text into tokens is work you do yourself,
or with a library made for it. This is the same division
Happy and Menhir make, and for the same reason — lexing is a small, separate
problem with good answers of its own.

The parsers Puppy produces are LR(1), built with Pager's algorithm. That is the
useful middle between the two well-known extremes: a canonical LR(1) automaton
is precise but enormous, LALR is small but reports conflicts the grammar does
not really have, and Pager's rule merges states only where merging cannot
invent a conflict. The result is close to LALR in size on most grammars, and as
accurate as canonical LR(1) — it keeps two states apart exactly where merging
them would have cost you a conflict, and pays for those in states.

A generated parser depends on `puppy-runtime` and nothing else of Puppy's. The
generator is not needed to build or run what it produced.
