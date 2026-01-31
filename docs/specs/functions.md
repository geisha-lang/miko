# Functions

This document covers function definitions, lambda expressions, closures, and recursion in Geisha.

## Function Definitions

### Basic Syntax

```
def functionName(param1, param2, ...) = body
```

The function body is a single expression. The return value is the result of evaluating that expression.

### Examples

```
def add(x, y) = x + y

def square(n) = n * n

def greet(name) = printLn("Hello, " ++ name)
```

### Type Annotations

Parameters and return types can be annotated:

```
def add(x: Int, y: Int): Int = x + y

def identity(x: a): a = x

def first(x: a, y: b): a = x
```

When annotations are omitted, types are inferred.

## Lambda Expressions

Lambdas are anonymous functions created with the arrow syntax:

```
(parameters) -> body
```

### Examples

```
(x) -> x + 1
(x, y) -> x * y
(a, b, c) -> a + b + c
```

### With Type Annotations

```
(x: Int) -> x + 1
(x: Int, y: Int) -> x + y
```

### Lambdas as Values

Lambdas can be bound to variables or passed as arguments:

```
def main() =
    let double = (x) -> x * 2 in
    let triple = (x) -> x * 3 in
    putNumber(double(triple(5)))  // 30
```

## Higher-Order Functions

Functions can take other functions as arguments or return functions:

### Functions as Arguments

```
def apply(f, x) = f(x)

def twice(f, x) = f(f(x))

def main() =
    let inc = (n) -> n + 1 in
    putNumber(twice(inc, 5))  // 7
```

### Functions Returning Functions

```
def makeAdder(n) = (x) -> n + x

def main() =
    let add5 = makeAdder(5) in
    putNumber(add5(10))  // 15
```

### Composition

```
def compose(f, g) = (x) -> f(g(x))

def main() =
    let double = (x) -> x * 2 in
    let square = (x) -> x * x in
    let doubleThenSquare = compose(square, double) in
    putNumber(doubleThenSquare(3))  // 36
```

## Closures

Lambdas capture variables from their enclosing scope, forming closures:

```
def makeCounter(start) =
    let count = start in
    (step) -> count + step

def main() =
    let counter = makeCounter(10) in
    putNumber(counter(1))   // 11
    putNumber(counter(5))   // 15
```

### Free Variables

Variables referenced in a lambda but defined outside it are called free variables:

```
def outer() =
    let x = 5 in           // x is captured
    let y = 10 in          // y is captured
    (z) -> x + y + z       // z is a parameter
```

The closure captures the values of `x` and `y` at the time the lambda is created.

## Recursion

Functions can call themselves recursively:

```
def factorial(n) =
    if (n == 0) 1
    else n * factorial(n - 1)

def fibonacci(n) =
    if (n <= 1) n
    else fibonacci(n - 1) + fibonacci(n - 2)
```

### Tail Recursion

For better performance, use tail-recursive patterns when possible:

```
def factorialTail(n, acc) =
    if (n == 0) acc
    else factorialTail(n - 1, n * acc)

def factorial(n) = factorialTail(n, 1)
```

### Mutual Recursion

Functions can call each other:

```
def isEven(n) =
    if (n == 0) true
    else isOdd(n - 1)

def isOdd(n) =
    if (n == 0) false
    else isEven(n - 1)
```

## Polymorphic Functions

Functions can be polymorphic, working with any type:

```
def identity(x) = x
// Inferred type: forall a. a -> a

def const(x, y) = x
// Inferred type: forall a b. a * b -> a

def flip(f) = (x, y) -> f(y, x)
// Inferred type: forall a b c. (a * b -> c) -> b * a -> c
```

### With Explicit Type Annotations

```
def identity(x: a): a = x

def compose(f: b -> c, g: a -> b): a -> c = (x) -> f(g(x))
```

## Currying

Geisha uses multi-parameter functions rather than automatic currying. To achieve currying, return nested lambdas:

```
def addCurried(x) = (y) -> x + y

def main() =
    let add5 = addCurried(5) in
    putNumber(add5(3))  // 8
```

## Partial Application

Simulate partial application using lambdas:

```
def add(x, y) = x + y

def main() =
    let add5 = (y) -> add(5, y) in
    putNumber(add5(3))  // 8
```

## Examples

### Map Function

```
data List a {
    Nil,
    Cons(a, List a)
}

def map(f, xs) = match xs {
    Nil -> Nil,
    Cons(h, t) -> Cons(f(h), map(f, t))
}

def main() =
    let nums = Cons(1, Cons(2, Cons(3, Nil))) in
    let doubled = map((x) -> x * 2, nums) in
    // doubled = Cons(2, Cons(4, Cons(6, Nil)))
```

### Fold Function

```
def foldl(f, acc, xs) = match xs {
    Nil -> acc,
    Cons(h, t) -> foldl(f, f(acc, h), t)
}

def sum(xs) = foldl((acc, x) -> acc + x, 0, xs)

def product(xs) = foldl((acc, x) -> acc * x, 1, xs)
```

### Filter Function

```
def filter(pred, xs) = match xs {
    Nil -> Nil,
    Cons(h, t) ->
        if (pred(h)) Cons(h, filter(pred, t))
        else filter(pred, t)
}

def evens(xs) = filter((x) -> x % 2 == 0, xs)
```
