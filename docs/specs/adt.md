# Algebraic Data Types

Algebraic Data Types (ADTs) define custom sum and product types with unit, tuple, and named-field struct variants.

## Defining ADTs

```
data Maybe a {
    Nothing,
    Just(a),
}

data Point {
    Origin,
    Point2D(Int, Int),
    Polar { r: Float, theta: Float },
}
```

### Constructor Types

- Unit variant: `Nothing : forall a. Maybe a`
- Single-field tuple variant: `Just : forall a. a -> Maybe a`
- Multi-field tuple variant: `Point2D : (Int, Int) -> Point`
- Struct variant: constructed with named fields `Polar { r: 1.0, theta: 0.5 }` (or positional `Polar(1.0, 0.5)`).

## Recursive & Mutually Recursive ADTs

ADTs may reference themselves directly or through mutually recursive cycles:

```
data List a {
    Nil,
    Cons(a, List a),
}

data Even {
    Zero,
    SuccE(Odd),
}

data Odd {
    SuccO(Even),
}
```

The compiler computes strongly-connected/reachable cycles across the ADT dependency graph so both self-recursive and mutually recursive ADTs are always heap-allocated safely via `gc_alloc`.
