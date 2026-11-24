# Functions

This document covers function definitions, top-level values, lambdas, closures, and recursion in Geisha.

## Function Definitions

```
def add(x, y) = x + y

def addTyped(x: Int, y: Int): Int = x + y

def greet(): Void = printLn("Hello!")
```

Multi-parameter functions take a parameter list `(T1, T2, ...) -> R` without implicit currying. Zero-argument functions have type `() -> R`.

### Top-Level Value Definitions

Top-level constants and computed values are initialized once at program startup before `main()` runs:

```
def tau = 6.2831853
def sampleList = [1, 2, 3]
```

## Lambda Expressions & Closures

```
() -> 42
(x) -> x + 1
(x: Int, y: Int): Int -> x * y
```

Lambdas capture free variables from enclosing scopes by value:

```
def makeAdder(n: Int): Int -> Int = (x) -> n + x

def main() =
    let add5 = makeAdder(5) in
    putNumber(add5(10))  // 15
```

Non-escaping closures are stack-allocated (`alloca`); closures that escape via return, ADT fields, or outer closures are allocated on the GC heap (`gc_alloc`).

## Higher-Order & Polymorphic Functions

```
def twice(f: a -> a, x: a): a = f(f(x))

def compose(f: b -> c, g: a -> b): a -> c = (x) -> f(g(x))

def flip(f: (a, b) -> c): (b, a) -> c = (y, x) -> f(x, y)
```

## Recursion & Mutual Recursion

Top-level functions and local `let` lambdas support direct recursion, tail-call optimization (via LLVM's `tailcallelim` pass), and mutual recursion:

```
def isEven(n) = if (n == 0) true else isOdd(n - 1)
def isOdd(n) = if (n == 0) false else isEven(n - 1)
```
