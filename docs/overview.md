# Geisha Language Overview

Geisha is a statically-typed functional programming language that compiles to native code via LLVM. It features Hindley-Milner type inference, algebraic data types, pattern matching, a concept system (typeclasses), and a Rust-inspired module system.

## Features

- **Functional**: First-class functions, closures, higher-order functions
- **Statically typed**: Complete type inference with polymorphism
- **Algebraic data types**: Define custom types with pattern matching
- **Concepts**: Typeclass-style polymorphism with constraints
- **Module system**: File-based modules with visibility control and imports
- **Native compilation**: LLVM backend generates efficient machine code

## Hello World

```
def main() = printLn("Hello, World!")
```

## Building and Running

```bash
# Build the compiler
cargo build

# Compile a single file to executable (auto-links with runtime)
./target/debug/miko -o output input.gs

# Run the program
./output

# Multi-file project compilation
./target/debug/miko --src-root ./src -o output src/main/mod.gs

# Compile to object file only (no linking)
./target/debug/miko -c -o output.o input.gs

# Emit LLVM IR (for debugging)
./target/debug/miko -e -o /dev/null input.gs
```

## Quick Examples

### Functions and Recursion

```
def factorial(n) =
    if (n == 0) 1
    else n * factorial(n - 1)

def main() = putNumber(factorial(10))
```

### Higher-Order Functions

```
def twice(f, x) = f(f(x))

def main() =
    let double = (x) -> x * 2 in
    putNumber(twice(double, 5))  // Prints 20
```

### Algebraic Data Types

```
data List a {
    Nil,
    Cons(a, List a)
}

def length(lst) = match lst {
    Nil -> 0,
    Cons(_, t) -> 1 + length(t)
}

def main() = putNumber(length(Cons(1, Cons(2, Cons(3, Nil)))))
```

### Pattern Matching

```
data Maybe a {
    Nothing,
    Just(a)
}

def getOrDefault(opt, default) = match opt {
    Nothing -> default,
    Just(x) -> x
}

def main() = putNumber(getOrDefault(Just(42), 0))
```

### Concepts (Typeclasses)

```
concept Eq a {
    eq: a * a -> Bool
}

instance Eq Int {
    def eq(x, y) = x == y
}

def allEqual(x, y, z) : forall (Eq a). a * a * a -> Bool =
    if (eq(x, y)) eq(y, z) else false
```

### Modules and Visibility

```
// math.gs
pub def add(x, y) = x + y
pub def mul(x, y) = x * y
def helper(x) = x * x       // private

// mod.gs
mod math
use math.{add, mul}

pub def main() = putNumber(mul(add(2, 3), 4))
```

## Documentation

### Language Specification
- [Syntax Reference](specs/syntax.md) - Complete grammar and lexical elements
- [Functions](specs/functions.md) - Function definitions, lambdas, closures
- [Type System](specs/type-system.md) - Types, polymorphism, annotations
- [Type Inference](specs/type-inference.md) - Hindley-Milner algorithm details
- [Algebraic Data Types](specs/adt.md) - Custom data type definitions
- [Pattern Matching](specs/pattern-matching.md) - Match expressions and patterns
- [Concepts](specs/concepts.md) - Typeclass system
- [Operators](specs/operators.md) - Operators and precedence
- [Modules](specs/modules.md) - Module system, visibility, imports

### Runtime Documentation
- [Compilation Pipeline](runtime/compilation.md) - How code is compiled
- [Closures](runtime/closures.md) - Closure representation
- [Memory Layout](runtime/memory.md) - Memory management
- [Built-in Functions](runtime/builtins.md) - Runtime library
