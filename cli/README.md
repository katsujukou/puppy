# 🐶 purs-puppy

An LR parser generator for PureScript.

Puppy turns a grammar into a PureScript parser. You write the grammar and what
each rule means; Puppy works out the parsing table and writes a module.

```sh
npm install --save-dev purs-puppy
```

Puppy assumes your project is organised with
[spago](https://github.com/purescript/spago). Grammars use the `.pursy`
extension and live under `src`:

```sh
puppy -p my-package
```

That finds every grammar under the package's `src` and writes each one's parser
module beside it. A single grammar can be named directly:

```sh
puppy src/Foo/Parser.pursy -m Foo.Parser
```

A generated parser depends on `puppy-runtime`, so you need to add it to your Spago dependencies:

```sh
spago install puppy-runtime
```

To figure out which version of `puppy-runtime` is compatible with the generated parser, run:

```sh
❯ npx puppy -v
Puppy v0.2.0 (built with purs 0.15.16)
Supported runtime: 0.2.0
```

[Read the documentation](https://katsujukou.github.io/puppy/) for the grammar
language, the shape of what Puppy generates, and how to read a conflict report.
