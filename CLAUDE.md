# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

MPL (El MegaProbe Language) is a programming language for high-performance genomics computation. It is written in Haskell and includes both an interpreter (REPL + file execution) and an in-progress CUDA code generator. The language has built-in support for DNA/RNA types and dataframe operations via Apache Arrow (through C FFI).

## Build & Run

```sh
cabal build                    # Build the project
cabal exec mpl                 # Launch the REPL
cabal exec mpl -- file.probe   # Run a .probe file
```

Requires GHC (installed via ghcup) and `pkg-config` with `arrow-glib` (and `arrow-cuda-glib` on Linux).

## Linting & Formatting

CI enforces both on every push/PR to main:

```sh
./lint.sh                      # Run fourmolu (formatting) + hlint
fourmolu --mode inplace src    # Format only
hlint src                      # Lint only
```

- Fourmolu v0.16.2.0, HLint v3.8
- Fourmolu config: `fourmolu.yaml` (4-space indent, leading commas)
- HLint config: `.hlint.yaml` (ignores "Use newtype instead of data" and "Redundant bracket")

## Architecture

The compiler pipeline flows: **Source → Lexer → Parser → TypeChecker → Eval** (interpreter path) or **→ CodeGen** (compiler path, WIP).

Key modules in `src/`:

- **Syntax.hs** — Core AST types (`Expr`, `Value`, `Types`, `Op`, `Error`, `Env`). Central to everything; most modules import this.
- **Lexer.hs** — Tokenizer built on Parsec's `TokenParser`. Defines the language definition (reserved words, operators, comment syntax `(* ... *)`).
- **Parser.hs** — Parsec-based parser. Exports `parser` (full parse) and `parseLine` (REPL line parse). Operator precedence defined in `opTable`.
- **MPLTypes.hs** — Domain-specific genomics types: `DNA`, `RNA`, with operations like `transcribe`, `countNucleotides`, `reverseComplement`.
- **InterpM.hs** — Interpreter monad `InterpM`, a stack of `ExceptT Error (StateT Env IO)`. Provides environment operations (`lookupVar`, `bindVar`, `setBackend`).
- **TypeChecker.hs** — Expression type checker (`tc :: Expr -> InterpM Types`). Runs before evaluation.
- **Eval.hs** — Expression evaluator. `runEval` parses+typechecks+evaluates a string. `initialEnv` sets up built-in bindings.
- **Primitives.hs** — Built-in function implementations (`print`, `read_csv`, `transcribe`, `count_nucleotides`, `reverse_complement`).
- **Dataframe.hs** — FFI bindings to Apache Arrow C library (`cbits/mpl_arrow_glib.c`) for CSV reading and table display.
- **CodeGen.hs** — CUDA code generation (early stage, mostly `undefined`).
- **Main.hs** — Entry point. File mode processes lines sequentially; REPL mode loops with persistent environment.

## Language Features

- Types: `int`, `double`, `bool`, `string`, `DNA`, `RNA`, `dataframe`, tuples, `()`
- `let`/`let in` bindings, `let rec` for recursive functions, `lambda` expressions
- `if/then/else`, arithmetic, comparison, boolean operators, pipe operator `|>`
- `use nvidia|opencl|cpu|auto` to select compute backend
- File extension: `.probe`
