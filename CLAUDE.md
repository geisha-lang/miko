# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Miko is a compiler for the Geisha language (a functional programming language) implemented in Rust. It uses LLVM 14 for code generation and a PEG-based inline parser.

## Build Commands

```bash
# Build compiler (also builds the runtime library)
cargo build

# Run tests
cargo test

# Compile Geisha source to executable (auto-links with runtime)
./target/debug/miko -o output input.gs

# Compile to object file only (no linking)
./target/debug/miko -c -o output.o input.gs

# Emit LLVM IR (for debugging)
./target/debug/miko -e -o /dev/null input.gs

# Use custom runtime library path
./target/debug/miko --runtime /path/to/runtime.a -o output input.gs
# Or via environment variable
GEISHA_RUNTIME=/path/to/runtime.a ./target/debug/miko -o output input.gs

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

The runtime library (`src/lib/`) is built automatically with `cargo build`:
- `src/lib/base.c` - I/O functions (`printLn`, `putNumber`, etc.)
- `src/lib/runtime/gc_bitmap.c` - Mark-and-sweep garbage collector
- `src/lib/include/` - Header files for value representation and GC

The runtime is compiled via the `cc` crate in `build.rs` and automatically linked when producing executables.

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
- `cc` 1.0 - C compiler integration for building runtime (build dependency)

## Development Workflow

After implementing a feature:
1. Run `cargo test` to ensure all unit tests pass
2. Test with example programs in `test/` directory
3. **Update documentation** in `docs/` to reflect changes:
   - `docs/specs/` - Language specification docs
   - `docs/runtime/` - Runtime/codegen implementation docs
4. Update any TODO files if issues are resolved

When encountering issues:
- If a proper fix requires deeper architectural changes, **document the workaround** in `docs/todo/`
- Create a markdown file describing: the issue, the workaround applied, and what a proper fix would look like
- The document for TODO things should name start with `TODO_`. After resolve change it to `COMPLETED_`, and update the content.
- This prevents technical debt from being forgotten

## Gotchas

- Example Geisha programs in `test/` (fibonacci.gs, factorial.gs, etc.)
- Formal grammar specification in `CFG` file at project root
- Some deprecated LLVM API warnings exist (opaque pointers) but don't affect functionality
- REPL mode (`-r`) is experimental and not fully implemented
- Quick test:
  ```bash
  cargo build && ./target/debug/miko -o fib test/fibonacci.gs && ./fib
  ```
