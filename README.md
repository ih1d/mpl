# El MegaProbe Language

A programming language for high performance genomics computation

## Structure

- mpl-core is the core language
- mpli is the interpreter
- mplc is the compiler **WIP**

## Building & Running

Ensure to install [Haskell](https://haskell.org). Recommended way is via [ghcup]

To build:

```sh
cabal build all
```

To run the interpreter:

```sh
cabal run mpli
```

To run the compiler:

```sh
cabal run mplc
```
