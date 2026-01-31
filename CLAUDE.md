# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Miko is a compiler for the Geisha language (a functional programming language) implemented in Rust. It uses LLVM 14 for code generation and a PEG-based inline parser.

## Build Commands

```bash
# Build (stable Rust 2021 edition)
cargo build

# Run tests
cargo test

# Compile Geisha source to object file
./target/debug/miko -o output.o input.gs

# Link with runtime to create executable
cc -o output base.o output.o

# Emit LLVM IR (for debugging)
./target/debug/miko -e input.gs

# Run REPL mode (experimental, not fully implemented)
./target/debug/miko -r
```

## Architecture

The compiler follows a classic multi-stage pipeline:

```
Source (.gs) → Parse → Type Infer → Core Transform → LLVM Codegen → Object File
```

### Key Modules

- **`syntax/`** - PEG parser (inline `peg::parser!` macro) and AST definitions (`Form`, `Def`, `Expr`)
- **`typeinfer/`** - Hindley-Milner type inference with constraint solving
- **`core/`** - K-conversion: transforms AST to core terms with explicit closure free variables
- **`codegen/`** - LLVM IR emission via `llvm-sys`
- **`types.rs`** - Type system: `Type`, `Scheme` (polymorphic types), `TypeEnv`
- **`utils.rs`** - `Interner` (string interning), `SymTable` (lexical scoping)

### Parser

The PEG grammar is defined inline in `src/syntax/parser/mod.rs` using the `peg::parser!` macro. The parser uses `RefCell<Interner>` to handle mutable state threading.

### Runtime

`base.o` contains the precompiled runtime library (print functions, etc.). Must be linked with compiled output.

## Geisha Language Syntax

```
def name(params) = body          # Function definition
let var = value in expr          # Variable binding
(params) -> expr                 # Lambda/closure
if (cond) expr else expr         # Conditional
```

Built-in operators: `+`, `-`, `*`, `/`, `<`, `>`, `<=`, `>=`, `==`, `||`, `&&`
Built-in functions: `printLn`, `putNumber`

Type annotations: `def foo(x: Int): Int = x + 1`
Polymorphic types: `forall a. a * a -> a`
Constrained polymorphism: `forall a. (Eq a) => a * a -> Bool`

### Algebraic Data Types (ADTs)

```
data TypeName {              # Define ADT
    Variant1,                 # Unit variant
    Variant2(Type1, Type2)    # Variant with fields
}
```

### Pattern Matching

```
match expr {
    Pattern1 -> result1,
    Constructor(x, y) -> result2,
    _ -> default               # Wildcard pattern
}
```

### Concepts (Type Classes)

```
concept Eq a {               # Define concept
    eq: a * a -> Bool
}

instance Eq Int {            # Implement for type
    def eq(x, y) = x == y
}
```

## Dependencies

- Rust 1.93.0 stable (2021 edition)
- LLVM 14 (via `llvm-sys` v140)
- `libllvm/` - local crate wrapping LLVM APIs
- `peg` 0.8 - inline PEG parser generator
- `clap` 4 - command-line argument parsing

## Gotchas

- Example Geisha programs in `test/` (fibonacci.gs, factorial.gs, etc.)
- Formal grammar specification in `CFG` file at project root
- Some deprecated LLVM API warnings exist (opaque pointers) but don't affect functionality
- REPL mode (`-r`) is experimental and not fully implemented
- Complete compilation workflow:
  ```bash
  cargo build && ./target/debug/miko -o out.o test/fibonacci.gs && cc -o fib base.o out.o && ./fib
  ```
