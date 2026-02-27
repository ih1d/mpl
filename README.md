# El MegaProbe Language

A programming language for high performance genomics computation

## Structure

- [mpl-core](mpl-core) is the core language
- [mpli](mpli) is the interpreter
- [mplc](mplc) is the compiler **WIP**

## Building & Running

Ensure to install [Haskell](https://haskell.org). Recommended way is via [ghcup](https://www.haskell.org/ghcup/)

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
