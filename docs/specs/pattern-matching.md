# Pattern Matching

Pattern matching is a powerful feature for deconstructing values and branching based on their structure.

## Match Expressions

### Basic Syntax

```
match expression {
    pattern1 -> result1,
    pattern2 -> result2,
    ...
    patternN -> resultN
}
```

The expression is matched against each pattern in order. The first matching pattern's result is returned.

### Example

```
def describe(n) = match n {
    0 -> "zero",
    1 -> "one",
    _ -> "many"
}
```

## Pattern Types

### Wildcard Pattern

The `_` pattern matches anything and binds nothing:

```
match value {
    _ -> "matches anything"
}
```

Use wildcards for values you don't need:

```
def length(lst) = match lst {
    Nil -> 0,
    Cons(_, t) -> 1 + length(t)  // Ignore head
}
```

### Variable Pattern

A lowercase identifier binds the matched value:

```
match value {
    x -> x + 1  // x is bound to value
}
```

Multiple variables in a pattern:

```
match pair {
    Pair(x, y) -> x + y  // x and y are bound
}
```

### Literal Patterns

Match exact values:

```
match n {
    0 -> "zero",
    1 -> "one",
    42 -> "the answer",
    _ -> "other"
}

match flag {
    true -> "yes",
    false -> "no"
}

match s {
    "" -> "empty",
    _ -> "non-empty"
}
```

### Constructor Patterns

Match ADT variants and bind their fields:

```
data Maybe a { Nothing, Just(a) }

def fromMaybe(default, opt) = match opt {
    Nothing -> default,
    Just(x) -> x
}
```

```
data List a { Nil, Cons(a, List a) }

def head(lst) = match lst {
    Nil -> Nothing,
    Cons(h, _) -> Just(h)
}
```

### Nested Patterns

Patterns can be nested arbitrarily:

```
match lst {
    Nil -> 0,
    Cons(_, Nil) -> 1,
    Cons(_, Cons(_, Nil)) -> 2,
    Cons(_, Cons(_, Cons(_, _))) -> 3
}
```

```
match opt {
    Nothing -> Nothing,
    Just(Nothing) -> Nothing,
    Just(Just(x)) -> Just(x)
}
```

## Pattern Matching on Lists

```
data List a { Nil, Cons(a, List a) }

def sum(lst) = match lst {
    Nil -> 0,
    Cons(h, t) -> h + sum(t)
}

def last(lst) = match lst {
    Nil -> Nothing,
    Cons(x, Nil) -> Just(x),
    Cons(_, t) -> last(t)
}

def take(n, lst) = match lst {
    Nil -> Nil,
    Cons(h, t) ->
        if (n <= 0) Nil
        else Cons(h, take(n - 1, t))
}
```

## Pattern Matching on Maybe

```
data Maybe a { Nothing, Just(a) }

def mapMaybe(f, opt) = match opt {
    Nothing -> Nothing,
    Just(x) -> Just(f(x))
}

def flatMap(f, opt) = match opt {
    Nothing -> Nothing,
    Just(x) -> f(x)
}

def orElse(opt, default) = match opt {
    Nothing -> default,
    Just(_) -> opt
}
```

## Pattern Matching on Either

```
data Either a b { Left(a), Right(b) }

def mapRight(f, e) = match e {
    Left(x) -> Left(x),
    Right(y) -> Right(f(y))
}

def either(f, g, e) = match e {
    Left(x) -> f(x),
    Right(y) -> g(y)
}
```

## Pattern Matching on Trees

```
data Tree a { Leaf, Node(a, Tree a, Tree a) }

def size(t) = match t {
    Leaf -> 0,
    Node(_, l, r) -> 1 + size(l) + size(r)
}

def depth(t) = match t {
    Leaf -> 0,
    Node(_, l, r) ->
        let dl = depth(l) in
        let dr = depth(r) in
        1 + (if (dl > dr) dl else dr)
}

def find(x, t) = match t {
    Leaf -> false,
    Node(v, l, r) ->
        if (x == v) true
        else if (find(x, l)) true
        else find(x, r)
}
```

## Exhaustiveness

Patterns should cover all possible cases. A wildcard `_` can be used as a catch-all:

```
data Color { Red, Green, Blue }

def toHex(c) = match c {
    Red -> "#FF0000",
    Green -> "#00FF00",
    Blue -> "#0000FF"
}
```

For types with many variants or when you only care about specific cases:

```
def isRed(c) = match c {
    Red -> true,
    _ -> false
}
```

## Match Expression Value

Match expressions return a value:

```
def factorial(n) =
    if (n == 0) 1
    else n * factorial(n - 1)

// Alternative with match:
def factorial(n) = match n {
    0 -> 1,
    _ -> n * factorial(n - 1)
}
```

All branches must return the same type:

```
// Valid: all branches return Int
match opt {
    Nothing -> 0,
    Just(x) -> x
}

// Invalid: branches return different types
match opt {
    Nothing -> "none",    // String
    Just(x) -> x          // a (type error if a != String)
}
```

## Examples

### Safe List Operations

```
def safeHead(lst) = match lst {
    Nil -> Nothing,
    Cons(h, _) -> Just(h)
}

def safeTail(lst) = match lst {
    Nil -> Nothing,
    Cons(_, t) -> Just(t)
}

def safeIndex(lst, n) = match lst {
    Nil -> Nothing,
    Cons(h, t) -> if (n == 0) Just(h) else safeIndex(t, n - 1)
}
```

### Expression Interpreter

```
data Expr {
    Num(Int),
    Add(Expr, Expr),
    Mul(Expr, Expr),
    Div(Expr, Expr)
}

def interpret(e) = match e {
    Num(n) -> Just(n),
    Add(l, r) -> match interpret(l) {
        Nothing -> Nothing,
        Just(lv) -> match interpret(r) {
            Nothing -> Nothing,
            Just(rv) -> Just(lv + rv)
        }
    },
    Mul(l, r) -> match interpret(l) {
        Nothing -> Nothing,
        Just(lv) -> match interpret(r) {
            Nothing -> Nothing,
            Just(rv) -> Just(lv * rv)
        }
    },
    Div(l, r) -> match interpret(r) {
        Nothing -> Nothing,
        Just(0) -> Nothing,
        Just(rv) -> match interpret(l) {
            Nothing -> Nothing,
            Just(lv) -> Just(lv / rv)
        }
    }
}
```

### State Machine

```
data State { Initial, Running, Paused, Stopped }
data Event { Start, Pause, Resume, Stop }

def transition(state, event) = match state {
    Initial -> match event {
        Start -> Running,
        _ -> Initial
    },
    Running -> match event {
        Pause -> Paused,
        Stop -> Stopped,
        _ -> Running
    },
    Paused -> match event {
        Resume -> Running,
        Stop -> Stopped,
        _ -> Paused
    },
    Stopped -> Stopped
}
```
