# The command line

```plain
puppy [-p PACKAGE] [GRAMMAR -m MODULE [-o PATH]] [--allow-conflicts] [--emit-explain]
```

## Over a package

```sh
puppy -p my-package
```

Finds every `.pursy` file under that package's `src` and writes a `.purs`
beside each one. The module name comes from where the file sits:
`src/Foo/Parser.pursy` becomes module `Foo.Parser` in `src/Foo/Parser.purs`.

This is the intended way to use Puppy, and the reason a package is a better
thing to name than a path: `src` is the one place Spago fixes, so the position
of a file under it says what the module is called. Nothing has to be repeated
and nothing can drift.

Puppy asks Spago where its packages are, so this works from anywhere in the
workspace and needs `spago` on the path.

With no `-p`, every package in the workspace is done.

## A single grammar

```sh
puppy grammar.pursy --module Foo.Parser --output build/Foo/Parser.purs
```

For grammars that do not live under a `src`, or projects that do not use Spago.
`--module` is required — nothing about a path says what a module is called —
and `--output` defaults to the grammar's own path with the extension changed.

Puppy refuses to write the module over the grammar it just read, including when
the two are the same file under different names.

## Flags

| Flag | |
| --- | --- |
| `-p`, `--package` | Generate for one workspace package |
| `-m`, `--module` | Module name, for a grammar named by path |
| `-o`, `--output` | Where the module goes, for a grammar named by path |
| `--allow-conflicts` | Write the parser even where a conflict is unsettled |
| `--emit-explain` | Put conflict reports in `.puppy-explain` files rather than on standard error |
| `-v`, `--version` | |
| `-h`, `--help` | |

## Exit status and output

Zero when every grammar was generated. Non-zero if any failed, and the failures
are reported together — one broken grammar does not hide the state of the
others.

Progress goes to standard output, one line per module. Conflicts and failures
go to standard error.

Errors carry a position where there is one to give:

```plain
src/Foo/Parser.pursy:14:9: unknown symbol `expresion`
```

## Fitting it into a build

Spago has no hook for running a generator before a build, so Puppy is a step
you run yourself:

```sh
puppy -p my-package && spago build
```

Generated modules are worth committing. They are the thing that gets compiled,
so having them in the tree means a clone builds without Puppy installed, and a
change to them shows up in review where it can be seen. A CI step that
regenerates and checks nothing moved keeps them honest:

```sh
puppy -p my-package
git diff --exit-code
```
