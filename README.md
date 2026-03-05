# El MegaProbe Language

A programming language for high performance genomics computation

## Structure

Currently all are **WIP**

- [mpl-core](mpl-core) is the core language
- [mplc](mplc) is the interpreter/compiler
- [data](data) contains data file examples

## Building & Running

Ensure to install [Haskell](https://haskell.org). Recommended way is via [ghcup](https://www.haskell.org/ghcup/)

To build:

```sh
cabal build all
```

To run the interpreter/compiler:

```sh
cabal exec mplc # optional file
```
