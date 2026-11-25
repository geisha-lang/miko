# Type System

Geisha has a static Hindley-Milner type system extended with algebraic data types, first-class tuples, explicit multi-argument function arities, and monomorphized concepts (typeclasses).

## Primitive Types

- **`Int`**: 32-bit signed integer (`42`, `-100`)
- **`Float`**: 64-bit IEEE-754 double (`3.14159`, `-2.5e-3`)
- **`Char`**: 8-bit character (`'A'`, `'\n'`)
- **`String`**: Null-terminated UTF-8 string (`"Hello, World!"`)
- **`Bool`**: Boolean (`true`, `false`)
- **`Void`** (also written `()`): Unit type (`()`)

## Explicit Mutable Containers (`Cell a` & `MutArray a`)

Variable bindings (`def` / `let`), tuples, and ADTs are strictly immutable. Local or encapsulated imperative state uses explicit heap-allocated mutable containers:

- **`Cell a`**: Single mutable slot (`cellNew: forall a. a -> Cell a`, `cellGet: forall a. Cell a -> a`, `cellSet: forall a. (Cell a, a) -> Void`)
- **`MutArray a`**: Fixed-length bounds-checked mutable array (`arrayNew: forall a. (Int, a) -> MutArray a`, `arrayLen: forall a. MutArray a -> Int`, `arrayGet: forall a. (MutArray a, Int) -> a`, `arraySet: forall a. (MutArray a, Int, a) -> Void`)

## Function Types vs First-Class Tuple Types

Geisha cleanly separates **function parameter arity** from **first-class tuple values**:

### Function Types

Function arrows `->` are **right-associative** and carry explicit parameter arity:

```
() -> Int                   // 0-parameter function
Int -> Int                  // 1-parameter function (equivalent to (Int) -> Int)
(Int, Int) -> Int           // 2-parameter function
Int -> Int -> Int           // Curried function: Int -> (Int -> Int)
((Int) -> Int, Int) -> Int  // Higher-order 2-parameter function
```

```
def add(x: Int, y: Int): Int = x + y
// Type: (Int, Int) -> Int

def makeAdder(n: Int): Int -> Int = (x) -> n + x
// Type: Int -> (Int -> Int)
```

### Tuple Types

First-class tuples group two or more heterogeneous values:

```
def origin: (Int, Int) = (0, 0)
def record: (String, Int, Bool) = ("Alice", 30, true)

def swap(p: (a, b)): (b, a) = match p {
    (x, y) -> (y, x)
}
// Type: forall a b. (a, b) -> (b, a)
```

Notice the difference:
- `def f(x: Int, y: Int): Int` has 2 parameters: `(Int, Int) -> Int`, called as `f(1, 2)`.
- `def g(p: (Int, Int)): Int` has 1 tuple parameter: `((Int, Int)) -> Int`, called as `g((1, 2))`.

## Polymorphism & Type Schemes

Functions and let-bound values are generalized automatically via Hindley-Milner let-polymorphism:

```
def identity(x) = x
// Inferred: forall a. a -> a

def first(x, y) = x
// Inferred: forall a b. (a, b) -> a

def compose(f, g, x) = f(g(x))
// Inferred: forall a b c. (b -> c, a -> b, a) -> c
```

## Constrained Polymorphism (Concepts)

Concept constraints restrict type variables to types implementing an interface:

```
forall a. (Eq a) => (a, a) -> Bool
forall a. (Eq a, Show a) => a -> String
forall k v. (Eq k) => (k, List (k, v)) -> Maybe v
```

Built-in arithmetic (`+`, `-`, `*`, `/`, unary `-`), equality (`==`, `!=`), and ordering (`<`, `<=`, `>`, `>=`) operators carry built-in numeric/comparison constraints so invalid expressions like `"a" + "b"` or `true + false` are rejected during type checking.

## Type Annotations

Annotations may appear inline on parameters, return types, `let` bindings, or as standalone forward signatures:

```
def add(x: Int, y: Int): Int = x + y

equal: forall a. (Eq a) => (a, a) -> Bool
def equal(x, y) = eq(x, y)
```

## Type Aliases

Type synonyms are expanded transparently during type checking:

```
type Pair a b = (a, b)
type Predicate a = a -> Bool
type IntList = List Int
```

Cyclic type aliases are rejected at compile time.

## Algebraic Data Types

Custom sum/product types are defined with `data`:

```
data Maybe a {
    Nothing,
    Just(a),
}

// Nothing : forall a. Maybe a
// Just    : forall a. a -> Maybe a
```

See [ADT](adt.md) for complete documentation.
