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

A generated module depends on `puppy-runtime` and nothing else of Puppy's. The
generator is not needed to build or run what it produced.

`puppy-runtime` is not in the PureScript registry yet, so for now name it as an
extra package pointing at the subdirectory it lives in:

```yaml
workspace:
  extraPackages:
    puppy-runtime:
      git: https://github.com/katsujukou/puppy.git
      ref: v0.1.0
      subdir: puppy-runtime
```

[Read the documentation](https://katsujukou.github.io/puppy/) for the grammar
language, the shape of what Puppy generates, and how to read a conflict report.
