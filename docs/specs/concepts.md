# Concepts (Typeclasses)

Concepts provide a way to define interfaces that types can implement. They enable constrained polymorphism, allowing functions to work with any type that implements certain operations.

## Defining Concepts

### Basic Syntax

```
concept ConceptName a {
    method1: Type1,
    method2: Type2,
    ...
}
```

The type variable `a` represents the type implementing the concept.

### Example: Eq

```
concept Eq a {
    eq: a * a -> Bool
}
```

This defines an `Eq` concept requiring an `eq` method that compares two values of the same type.

### Example: Ord

```
concept Ord a {
    compare: a * a -> Int
}
```

Returns negative for less-than, zero for equal, positive for greater-than.

### Example: Show

```
concept Show a {
    show: a -> String
}
```

Converts a value to its string representation.

## Implementing Concepts

### Instance Syntax

```
instance ConceptName Type {
    def method1(args) = body,
    def method2(args) = body,
    ...
}
```

### Example: Eq for Int

```
instance Eq Int {
    def eq(x, y) = x == y
}
```

### Example: Eq for Bool

```
instance Eq Bool {
    def eq(x, y) = if (x) y else (if (y) false else true)
}
```

### Example: Show for Int

```
instance Show Int {
    def show(n) = intToString(n)
}
```

## Constrained Instances

Instances can require constraints on type parameters:

### Syntax

```
instance (Constraint a) => ConceptName (Type a) {
    def method(args) = body
}
```

### Example: Eq for Maybe

```
instance (Eq a) => Eq (Maybe a) {
    def eq(x, y) = match x {
        Nothing -> match y {
            Nothing -> true,
            _ -> false
        },
        Just(a) -> match y {
            Nothing -> false,
            Just(b) -> eq(a, b)
        }
    }
}
```

### Example: Eq for List

```
instance (Eq a) => Eq (List a) {
    def eq(xs, ys) = match xs {
        Nil -> match ys {
            Nil -> true,
            _ -> false
        },
        Cons(x, xt) -> match ys {
            Nil -> false,
            Cons(y, yt) -> if (eq(x, y)) eq(xt, yt) else false
        }
    }
}
```

### Multiple Constraints

```
instance (Eq a, Eq b) => Eq (Pair a b) {
    def eq(p1, p2) = match p1 {
        Pair(a1, b1) -> match p2 {
            Pair(a2, b2) -> eq(a1, a2) && eq(b1, b2)
        }
    }
}
```

## Using Concepts

### Constrained Functions

Functions can require concept constraints:

```
def equal(x, y): forall (Eq a). a * a -> Bool = eq(x, y)

def notEqual(x, y): forall (Eq a). a * a -> Bool =
    if (eq(x, y)) false else true
```

### Type Inference with Constraints

Constraints are inferred when concept methods are used:

```
def allEqual(x, y, z) = eq(x, y) && eq(y, z)
// Inferred: forall (Eq a). a * a * a -> Bool
```

### Constraint Propagation

Constraints propagate through function calls:

```
def member(x, lst) = match lst {
    Nil -> false,
    Cons(h, t) -> if (eq(x, h)) true else member(x, t)
}
// Inferred: forall (Eq a). a * List a -> Bool
```

## Standard Concepts

### Eq

Equality comparison:

```
concept Eq a {
    eq: a * a -> Bool
}

// Derived operations
def notEq(x, y): forall (Eq a). a * a -> Bool =
    if (eq(x, y)) false else true
```

### Ord

Ordering comparison:

```
concept Ord a {
    compare: a * a -> Int
}

// Derived operations
def lt(x, y): forall (Ord a). a * a -> Bool = compare(x, y) < 0
def gt(x, y): forall (Ord a). a * a -> Bool = compare(x, y) > 0
def le(x, y): forall (Ord a). a * a -> Bool = compare(x, y) <= 0
def ge(x, y): forall (Ord a). a * a -> Bool = compare(x, y) >= 0

def min(x, y): forall (Ord a). a * a -> a =
    if (compare(x, y) <= 0) x else y

def max(x, y): forall (Ord a). a * a -> a =
    if (compare(x, y) >= 0) x else y
```

### Show

String representation:

```
concept Show a {
    show: a -> String
}
```

## Examples

### Sorting with Ord

```
def insert(x, lst): forall (Ord a). a * List a -> List a =
    match lst {
        Nil -> Cons(x, Nil),
        Cons(h, t) ->
            if (compare(x, h) <= 0) Cons(x, lst)
            else Cons(h, insert(x, t))
    }

def insertionSort(lst): forall (Ord a). List a -> List a =
    match lst {
        Nil -> Nil,
        Cons(h, t) -> insert(h, insertionSort(t))
    }
```

### Generic Contains

```
def contains(x, lst): forall (Eq a). a * List a -> Bool =
    match lst {
        Nil -> false,
        Cons(h, t) -> if (eq(x, h)) true else contains(x, t)
    }
```

### Remove Duplicates

```
def nub(lst): forall (Eq a). List a -> List a =
    match lst {
        Nil -> Nil,
        Cons(h, t) -> Cons(h, nub(filter((x) -> notEq(x, h), t)))
    }
```

### Generic Lookup

```
def lookup(key, pairs): forall (Eq k). k * List (Pair k v) -> Maybe v =
    match pairs {
        Nil -> Nothing,
        Cons(p, rest) -> match p {
            Pair(k, v) ->
                if (eq(key, k)) Just(v)
                else lookup(key, rest)
        }
    }
```

## Design Patterns

### Default Implementations

Provide default implementations using other methods:

```
concept Eq a {
    eq: a * a -> Bool
}

// notEq as a regular function using eq
def notEq(x, y): forall (Eq a). a * a -> Bool =
    if (eq(x, y)) false else true
```

### Concept Composition

Build on existing concepts:

```
concept Ord a {
    compare: a * a -> Int
}

// Functions that require Ord can use comparison
def sort(lst): forall (Ord a). List a -> List a = ...
def binarySearch(x, arr): forall (Ord a). a * Array a -> Maybe Int = ...
```

### Instance Chaining

Instances can depend on other instances:

```
instance Eq Int { def eq(x, y) = x == y }

instance (Eq a) => Eq (Maybe a) { ... }

instance (Eq a) => Eq (List a) { ... }

// Now Eq (List (Maybe Int)) is available!
```
