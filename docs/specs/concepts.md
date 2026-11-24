# Concepts (Typeclasses)

Concepts define interfaces that types can implement, enabling constrained polymorphism compiled via zero-overhead static monomorphization.

## Defining Concepts

```
concept Eq a {
    eq: (a, a) -> Bool
}

concept Ord a {
    compare: (a, a) -> Int
}

concept Show a {
    show: a -> String
}
```

Superclass constraints on concepts are also supported:

```
concept Ord a (Eq a) => {
    compare: (a, a) -> Int
}
```

## Implementing Instances

### Concrete Instances

```
instance Eq Int {
    def eq(x, y) = x == y
}

instance Eq Bool {
    def eq(x, y) = x == y
}
```

### Generic & Constrained Instances

Instances for parameterized ADTs can require concept constraints on their type parameters:

```
instance (Eq a) => Eq (Maybe a) {
    def eq(x, y) = match (x, y) {
        (Nothing, Nothing) -> true,
        (Just(a), Just(b)) -> eq(a, b),
        _ -> false,
    }
}

instance (Eq a) => Eq (List a) {
    def eq(xs, ys) = match (xs, ys) {
        (Nil, Nil) -> true,
        (Cons(x, xt), Cons(y, yt)) -> eq(x, y) && eq(xt, yt),
        _ -> false,
    }
}
```

Chained instances compose automatically: with `Eq Int`, `Eq (Maybe a)`, and `Eq (List a)` in scope, `eq` works on `List (Maybe Int)`.

## Writing Constrained Functions

Constraints can be inferred automatically or written explicitly with `forall ... =>`:

```
// Inferred: forall a. (Eq a) => (a, a, a) -> Bool
def allEqual(x, y, z) = eq(x, y) && eq(y, z)

// Explicit constrained signature (including unconstrained type variable `v`)
def lookup(key, pairs): forall k v. (Eq k) => (k, List (k, v)) -> Maybe v =
    match pairs {
        Nil -> Nothing,
        Cons((k, v), rest) ->
            if (eq(key, k)) Just(v)
            else lookup(key, rest),
    }
```

## Static Dispatch via Monomorphization

Unlike Haskell's runtime dictionary passing, Geisha resolves all concept method calls statically during monomorphization:
- Calling `eq(1, 2)` resolves directly to `Eq Int`'s `eq` implementation.
- Calling a constrained generic function `allEqual(1, 1, 1)` specializes `allEqual_Int` and rewrites its internal `eq` calls to `Eq Int`'s `eq`.
- Calling `eq` on a type with no matching instance produces a compile-time `NoInstance` type error.
