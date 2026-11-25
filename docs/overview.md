# Geisha Language Overview

Geisha is a statically-typed, strict impure functional programming language in the MLton/OCaml tradition that compiles to native code via LLVM 20. Bindings (`def` / `let`), tuples, and algebraic data types are immutable by default, while controlled imperative algorithms are supported cleanly via `while` loops, explicit mutable heap containers (`Cell a`, `MutArray a`), and non-tail `Void` checking in blocks. It combines Hindley-Milner type inference, pattern matching with guards, zero-overhead monomorphized concepts (typeclasses), escape-analyzed stack allocation, and a Rust-inspired module system.

## Quick Examples

### Hello World

```
def main() = printLn("Hello, World!")
```

### Functions, Tuples, and Closures

```
def swap(p: (a, b)): (b, a) = match p {
    (x, y) -> (y, x)
}

def makeAdder(n: Int): Int -> Int = (x) -> n + x

def main() = {
    let (_, a) = (0, 2); // pattern match in match or tuple
    let add5 = makeAdder(5);
    putNumber(add5(37))
}
```

### Algebraic Data Types & Pattern Matching

```
data List a {
    Nil,
    Cons(a, List a),
}

def sumPositives(xs: List Int): Int = match xs {
    Nil -> 0,
    Cons(h, t) if (h > 0) -> h + sumPositives(t),
    Cons(_, t) -> sumPositives(t),
}

def main() = putNumber(sumPositives([10, -3, 32]))
```

### Concepts (Monomorphized Typeclasses)

```
concept Eq a {
    eq: (a, a) -> Bool
}

instance Eq Int {
    def eq(x, y) = x == y
}

def allEqual(x, y, z): forall a. (Eq a) => (a, a, a) -> Bool =
    eq(x, y) && eq(y, z)

def main() = if (allEqual(7, 7, 7)) putNumber(42) else putNumber(0)
```

## Documentation

### Language Specification
- [Syntax Reference](specs/syntax.md)
- [Functions](specs/functions.md)
- [Type System](specs/type-system.md)
- [Type Inference & Monomorphization](specs/type-inference.md)
- [Algebraic Data Types](specs/adt.md)
- [Pattern Matching](specs/pattern-matching.md)
- [Concepts](specs/concepts.md)
- [Operators](specs/operators.md)
- [Modules](specs/modules.md)
- [Concurrency Model](specs/concurrency.md)

### Runtime & Compiler Internals
- [Compilation Pipeline](runtime/compilation.md)
- [Closures](runtime/closures.md)
- [Memory Layout & GC](runtime/memory.md)
- [Built-in Functions](runtime/builtins.md)
