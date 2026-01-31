# Type System

Geisha has a powerful static type system based on Hindley-Milner with extensions for algebraic data types and typeclasses (concepts).

## Primitive Types

### Int

32-bit signed integer.

```
def x: Int = 42
def y: Int = -100
```

### Float

64-bit double-precision floating point.

```
def pi: Float = 3.14159
def e: Float = 2.71828
```

### String

UTF-8 encoded string.

```
def greeting: String = "Hello, World!"
```

### Bool

Boolean type with two values.

```
def yes: Bool = true
def no: Bool = false
```

### Void

Unit type, used for functions that perform side effects.

```
def greet(): Void = printLn("Hello")
```

## Function Types

Function types use the arrow `->`:

```
Int -> Int              // Function from Int to Int
Int -> Int -> Int       // Curried function (Int -> (Int -> Int))
(Int * Int) -> Int      // Function taking a pair
```

### Multi-parameter Functions

Functions with multiple parameters use product types:

```
def add(x: Int, y: Int): Int = x + y
// Type: Int * Int -> Int
```

### Higher-Order Functions

Functions can take or return other functions:

```
def twice(f: Int -> Int, x: Int): Int = f(f(x))
// Type: (Int -> Int) * Int -> Int

def makeAdder(n: Int): Int -> Int = (x) -> n + x
// Type: Int -> Int -> Int
```

## Product Types

Product types (tuples) use `*`:

```
Int * String            // Pair of Int and String
Int * Float * Bool      // Triple
a * b * c               // Polymorphic product
```

In function types, `*` binds tighter than `->`:

```
Int * Int -> Bool       // (Int * Int) -> Bool, not Int * (Int -> Bool)
```

## Type Variables

Type variables represent generic types:

```
a                       // Type variable
a -> a                  // Same type in and out
a -> b                  // Potentially different types
```

### Polymorphism

Functions can work with any type:

```
def identity(x) = x
// Inferred: forall a. a -> a

def first(x, y) = x
// Inferred: forall a b. a * b -> a

def compose(f, g, x) = f(g(x))
// Inferred: forall a b c. (b -> c) * (a -> b) * a -> c
```

## Type Schemes

A type scheme is a type with universal quantification:

```
forall a. a -> a
forall a b. a * b -> a
forall a b c. (b -> c) * (a -> b) * a -> c
```

The `forall` binds type variables that can be instantiated to any concrete type.

## Constrained Polymorphism

Type constraints restrict which types a type variable can represent:

```
forall (Eq a). a * a -> Bool
forall (Eq a, Ord a). a * a -> Int
forall (Show a). a -> String
```

Multiple constraints on the same variable:

```
forall (Eq a, Show a). a -> Bool
```

Constraints on different variables:

```
forall (Eq a, Show b). a * b -> String
```

## Type Annotations

### On Parameters

```
def add(x: Int, y: Int) = x + y
```

### On Return Types

```
def add(x, y): Int = x + y
```

### Full Annotations

```
def add(x: Int, y: Int): Int = x + y
```

### On Lambdas

```
(x: Int) -> x + 1
(x: Int, y: Float) -> x
```

### Polymorphic Annotations

```
def identity(x: a): a = x
def const(x: a, y: b): a = x
```

### Constrained Annotations

```
def equal(x, y): forall (Eq a). a * a -> Bool = eq(x, y)
```

## Algebraic Data Types

Custom types are defined with `data`:

```
data Maybe a {
    Nothing,
    Just(a)
}
```

The type `Maybe` is parameterized by `a`. Constructors have these types:

```
Nothing : forall a. Maybe a
Just    : forall a. a -> Maybe a
```

See [ADT](adt.md) for complete documentation.

## Type Inference

Geisha uses Hindley-Milner type inference. Most types can be inferred automatically:

```
def add(x, y) = x + y
// Inferred: forall a. a * a -> a (with numeric constraint)

def identity(x) = x
// Inferred: forall a. a -> a

def apply(f, x) = f(x)
// Inferred: forall a b. (a -> b) * a -> b
```

See [Type Inference](type-inference.md) for algorithm details.

## Type Aliases

Create type synonyms with `type`:

```
type IntList = List Int
type Pair a b = a * b
type Predicate a = a -> Bool
```

## Examples

### Polymorphic List Operations

```
data List a {
    Nil,
    Cons(a, List a)
}

def head(xs: List a): a = match xs {
    Cons(h, _) -> h
}

def tail(xs: List a): List a = match xs {
    Cons(_, t) -> t
}

def map(f: a -> b, xs: List a): List b = match xs {
    Nil -> Nil,
    Cons(h, t) -> Cons(f(h), map(f, t))
}
```

### Maybe Type

```
data Maybe a {
    Nothing,
    Just(a)
}

def fromMaybe(default: a, opt: Maybe a): a = match opt {
    Nothing -> default,
    Just(x) -> x
}

def mapMaybe(f: a -> b, opt: Maybe a): Maybe b = match opt {
    Nothing -> Nothing,
    Just(x) -> Just(f(x))
}
```

### Either Type

```
data Either a b {
    Left(a),
    Right(b)
}

def either(f: a -> c, g: b -> c, e: Either a b): c = match e {
    Left(x) -> f(x),
    Right(y) -> g(y)
}
```
