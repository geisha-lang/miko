# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) and AI agents when working with code in this repository.

## Project Overview

Miko is a compiler for the Geisha language (a statically-typed functional programming language) implemented in Rust. It uses **LLVM 20** (opaque pointers, `llvm-sys` v201) for native code generation and a PEG-based inline parser (`peg` 0.8).

## Build & Test Commands

```bash
# Build compiler and C runtime library (on macOS with Homebrew llvm@20):
LLVM_SYS_201_PREFIX=/opt/homebrew/opt/llvm@20 cargo build

# Run all unit and end-to-end integration tests
LLVM_SYS_201_PREFIX=/opt/homebrew/opt/llvm@20 cargo test

# Compile Geisha source to executable (auto-links with runtime)
./target/debug/miko -o output input.gs

# Multi-file project compilation
./target/debug/miko --src-root ./src -o output src/main/mod.gs

# Compile to object file only (no linking)
./target/debug/miko -c -o output.o input.gs

# Emit LLVM IR
./target/debug/miko -e input.gs
```

## Architecture

```
Source (.gs) → [Module Loader / DependencyGraph] → Parse → Type Infer (HM + Concepts) → K-Convert & Worklist Monomorphize → Escape Analysis → LLVM 20 Codegen → Executable
```

### Key Modules

- **`syntax/`** — Inline PEG parser (`src/syntax/parser/mod.rs`) and AST (`src/syntax/form.rs`)
- **`typeinfer/`** — Hindley-Milner inference with occurs check, let-generalization, type aliases, concept constraint checking, and pattern exhaustiveness
- **`core/`** — K-conversion, demand-driven transitive monomorphization, static concept dispatch (`convert.rs`), and fixed-point escape analysis (`escape.rs`)
- **`codegen/`** — LLVM 20 opaque-pointer IR emission (`emit.rs`, `llvm.rs`, `target.rs`)
- **`modules/`** — Multi-file & inline module loader, topological `DependencyGraph`, per-module `use`/`open`/`pub use` resolution, and visibility checking
- **`src/lib/`** — C runtime (`base.c`) and mark-and-sweep bitmap GC (`runtime/gc_bitmap.c`)

## Geisha Language Quick Syntax

```
// Functions & Tuples (function arity is separate from tuple types)
def add(x: Int, y: Int): Int = x + y        // (Int, Int) -> Int
def swap(p: (a, b)): (b, a) = match p { (x, y) -> (y, x) }
def makeAdder(n: Int): Int -> Int = (x) -> n + x  // Right-associative ->

// Blocks (; or newline separated), unary ops, short-circuit && / ||
def demo() = {
    putNumber(-42);
    printLn("done")
}

// ADTs (unit, tuple, and struct variants) & Pattern Matching with Guards
data Shape {
    Origin,
    Point(Int, Int),
    Circle { radius: Float },
}

// Concepts (compiled via static monomorphization)
concept Eq a {
    eq: (a, a) -> Bool
}

instance Eq Int {
    def eq(x, y) = x == y
}

def same(x, y): forall a. (Eq a) => (a, a) -> Bool = eq(x, y)
```

## Development Workflow

1. Update feature spec docs in `docs/specs/` and `docs/runtime/` first before changing language or runtime semantics.
2. Run `cargo test` (both unit tests and end-to-end execution tests in `tests/`).
3. Track unresolved architectural items as `docs/todo/TODO_*.md` and rename to `COMPLETED_*.md` once resolved.
