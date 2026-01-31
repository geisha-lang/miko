# Algebraic Data Types

Algebraic Data Types (ADTs) allow defining custom types with multiple variants. Each variant can hold different data.

## Defining ADTs

### Basic Syntax

```
data TypeName {
    Variant1,
    Variant2(Type1, Type2),
    Variant3 { field1: Type1, field2: Type2 }
}
```

### Parameterized Types

ADTs can have type parameters:

```
data TypeName a b {
    Variant1,
    Variant2(a),
    Variant3(a, b)
}
```

## Variant Types

### Unit Variants

Variants with no data:

```
data Bool {
    False,
    True
}

data Ordering {
    LT,
    EQ,
    GT
}
```

Unit variant constructors have type equal to the ADT itself:

```
False : Bool
True : Bool
LT : Ordering
```

### Tuple Variants

Variants with positional fields:

```
data Maybe a {
    Nothing,
    Just(a)
}

data Either a b {
    Left(a),
    Right(b)
}

data Point {
    Point2D(Int, Int),
    Point3D(Int, Int, Int)
}
```

Tuple variant constructors are functions:

```
Nothing : forall a. Maybe a
Just    : forall a. a -> Maybe a
Left    : forall a b. a -> Either a b
Right   : forall a b. b -> Either a b
Point2D : Int * Int -> Point
Point3D : Int * Int * Int -> Point
```

### Struct Variants

Variants with named fields:

```
data Shape {
    Circle { radius: Float },
    Rectangle { width: Float, height: Float }
}
```

## Common ADT Patterns

### Option/Maybe Type

Represents optional values:

```
data Maybe a {
    Nothing,
    Just(a)
}

def safeDivide(x, y) =
    if (y == 0) Nothing
    else Just(x / y)
```

### Either Type

Represents a value of one of two types:

```
data Either a b {
    Left(a),
    Right(b)
}

def parseNumber(s) =
    // Returns Left with error message or Right with parsed value
    ...
```

### List Type

Recursive linked list:

```
data List a {
    Nil,
    Cons(a, List a)
}

def list123 = Cons(1, Cons(2, Cons(3, Nil)))
```

### Binary Tree

Recursive tree structure:

```
data Tree a {
    Leaf,
    Node(a, Tree a, Tree a)
}

def sampleTree =
    Node(1,
        Node(2, Leaf, Leaf),
        Node(3, Leaf, Leaf))
```

## Using ADTs

### Construction

Create values using constructors:

```
def nothing = Nothing
def justFive = Just(5)
def list = Cons(1, Cons(2, Nil))
def point = Point2D(10, 20)
```

### Pattern Matching

Deconstruct values with `match`:

```
def fromMaybe(default, opt) = match opt {
    Nothing -> default,
    Just(x) -> x
}

def length(lst) = match lst {
    Nil -> 0,
    Cons(_, t) -> 1 + length(t)
}
```

See [Pattern Matching](pattern-matching.md) for complete documentation.

## Recursive Types

ADTs can be recursive, referring to themselves:

```
data List a {
    Nil,
    Cons(a, List a)      // Refers to List a
}

data Tree a {
    Leaf,
    Node(a, Tree a, Tree a)  // Refers to Tree a twice
}
```

### Mutually Recursive Types

Types can refer to each other:

```
data Expr {
    Num(Int),
    Add(Expr, Expr),
    IfExpr(BoolExpr, Expr, Expr)
}

data BoolExpr {
    BTrue,
    BFalse,
    LessThan(Expr, Expr)
}
```

## Polymorphic ADTs

### Type Parameters

ADTs can be parameterized by type variables:

```
data Pair a b {
    Pair(a, b)
}

data Triple a b c {
    Triple(a, b, c)
}
```

### Using Polymorphic ADTs

```
def pair1: Pair Int String = Pair(42, "hello")
def pair2: Pair Bool Float = Pair(true, 3.14)

def fst(p) = match p {
    Pair(x, _) -> x
}

def snd(p) = match p {
    Pair(_, y) -> y
}
```

## Examples

### Natural Numbers

```
data Nat {
    Zero,
    Succ(Nat)
}

def toInt(n) = match n {
    Zero -> 0,
    Succ(m) -> 1 + toInt(m)
}

def add(m, n) = match m {
    Zero -> n,
    Succ(m') -> Succ(add(m', n))
}
```

### Expression Tree

```
data Expr {
    Lit(Int),
    Add(Expr, Expr),
    Mul(Expr, Expr),
    Neg(Expr)
}

def evaluate(e) = match e {
    Lit(n) -> n,
    Add(l, r) -> evaluate(l) + evaluate(r),
    Mul(l, r) -> evaluate(l) * evaluate(r),
    Neg(x) -> 0 - evaluate(x)
}

def expr = Add(Lit(1), Mul(Lit(2), Lit(3)))
def result = evaluate(expr)  // 7
```

### JSON Value

```
data Json {
    JNull,
    JBool(Bool),
    JNumber(Float),
    JString(String),
    JArray(List Json),
    JObject(List (Pair String Json))
}
```

### Result Type for Error Handling

```
data Result a e {
    Ok(a),
    Err(e)
}

def divide(x, y) =
    if (y == 0) Err("division by zero")
    else Ok(x / y)

def andThen(result, f) = match result {
    Err(e) -> Err(e),
    Ok(x) -> f(x)
}
```
