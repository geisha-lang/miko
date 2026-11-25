# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) and AI agents when working with code in this repository.

## Project Overview

Miko is a compiler for the Geisha language (a statically-typed, pragmatic impure functional ML-family language: immutable bindings and ADTs by default, explicit mutable heap containers `Cell` / `MutArray`, native `while` loops, and direct side effects) implemented in Rust. It uses **LLVM 20** (opaque pointers, `llvm-sys` v201) for native code generation and a PEG-based inline parser (`peg` 0.8).

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
- **`modules/`** — Multi-file & inline module loader (with auto-resolved embedded `std.*` & `sys.*` libraries), topological `DependencyGraph`, per-module `use`/`open`/`pub use` resolution, and visibility checking
- **`stdlib/`** — Embedded Geisha standard library (`std.{option,result,list,bits,char,buf,str,vec,map,sync,task,actor}`) and C-ABI system library (`sys.{io,fs,env,proc}`)
- **`src/lib/`** — C runtime (`base.c`), multicore safepoint mark-and-sweep bitmap GC (`runtime/gc_bitmap.c`), and native `pthread` task/channel/actor runtime (`runtime/gs_conc.c`)

## Geisha Language Quick Syntax

```
// Functions & Tuples (function arity is separate from tuple types)
def add(x: Int, y: Int): Int = x + y        // (Int, Int) -> Int
def swap(p: (a, b)): (b, a) = match p { (x, y) -> (y, x) }
def makeAdder(n: Int): Int -> Int = (x) -> n + x  // Right-associative ->

// Blocks (; or newline separated; non-tail expressions MUST have type Void), while loops, Cell & MutArray
def demo() = {
    let i = cellNew(0);
    while (cellGet(i) < 3) {
        putNumber(cellGet(i));
        cellSet(i, cellGet(i) + 1)
    };
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
