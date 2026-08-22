# 🐶 PureScript Puppy

An LR(1) parser generator for PureScript

[![CI](https://github.com/katsujukou/puppy/actions/workflows/ci.yaml/badge.svg)](https://github.com/katsujukou/puppy/actions/workflows/ci.yaml)
[![purs - v0.15.16](https://img.shields.io/badge/purs-v0.15.16-blue?logo=purescript)](https://github.com/purescript/purescript/releases/tag/v0.15.16)

## Overview

Puppy is a parser generator for PureScript based on a minimal LR(1) algorithm.
It compiles LR(1) grammar specifications down to PureScript code.

## Installation

```sh
npm i -D purs-puppy
```

A generated parser depends on `puppy-runtime` and nothing else of Puppy's — the
generator is not needed to build or run what it produced. Currently we haven't published that package to the registry yet, so for now name it as an extra package pointing at
the subdirectory it lives in like this:

```yaml
workspace:
  extraPackages:
    puppy-runtime:
      git: https://github.com/katsujukou/puppy.git
      ref: v0.1.0
      subdir: puppy-runtime
```

Pin `ref` to a tag rather than a branch, so that a build stays what it was. When
`puppy-runtime` reaches the registry this block comes out and nothing else
changes.

## Usage

Puppy assumes that your project is organized using
[spago](https://github.com/purescript/spago).
Unfortunately, spago does not currently support pre-process hooks, so you will
need to run Puppy manually.
Puppy's grammar specifications use the `.pursy` extension and should be placed
within the `src` directory.

By running `puppy -p {YOUR_PACKAGE}`, Puppy automatically detects `.pursy`
grammar specifications inside the specified package's `src` directory and
generates the corresponding `.purs` parser module.

[Read the documentation](docs/md/README.md) for the grammar language, the shape of
what Puppy generates, and how to read a conflict report.
