# Syntax Reference

This document describes the complete syntax of the Geisha programming language.

## Lexical Elements

### Identifiers

**Lowercase identifiers** (variables, functions):
```
[a-z_][a-zA-Z0-9_]*
```

Examples: `x`, `myFunction`, `list_length`, `_unused`

**Uppercase identifiers** (type constructors, data constructors):
```
[A-Z][a-zA-Z0-9_]*
```

Examples: `Int`, `Maybe`, `Cons`, `Nothing`

### Literals

**Integers** (32-bit signed):
```
42
0
-123
```

**Floats** (64-bit double precision):
```
3.14
2.0
-1.5e-2
0.001
```

**Strings** (UTF-8 with escape sequences):
```
"hello"
"world\n"
"tab\there"
"quote\"inside"
```

Escape sequences: `\n` (newline), `\r` (carriage return), `\t` (tab), `\"` (quote), `\\` (backslash)

**Booleans**:
```
true
false
```

### Keywords

```
def      - Function/value definition
let      - Local binding
in       - Part of let expression
if       - Conditional start
else     - Conditional alternative
match    - Pattern matching
data     - ADT definition
type     - Type alias
concept  - Typeclass definition
instance - Typeclass implementation
forall   - Universal quantification
```

### Comments

```
// Single-line comment

/* Multi-line
   comment */
```

## Program Structure

A Geisha program consists of top-level definitions:

```
definition1;
definition2;
definition3
```

Definitions are separated by semicolons or newlines. Every program must define a `main` function as the entry point.

## Definition Forms

### Value Definition

```
def name = expression
```

Example:
```
def pi = 3.14159
def greeting = "Hello"
```

### Function Definition

```
def name(param1, param2, ...) = body
```

Examples:
```
def add(x, y) = x + y
def factorial(n) = if (n == 0) 1 else n * factorial(n - 1)
```

With type annotations:
```
def add(x: Int, y: Int): Int = x + y
```

### Type Alias

```
type TypeName a b = TypeExpression
```

Example:
```
type IntList = List Int
type Pair a b = a * b
```

### Algebraic Data Type

```
data TypeName a b {
    Variant1,
    Variant2(Type1, Type2),
    Variant3 { field1: Type1, field2: Type2 }
}
```

Examples:
```
data Maybe a {
    Nothing,
    Just(a)
}

data List a {
    Nil,
    Cons(a, List a)
}

data Point {
    Point2D(Int, Int),
    Point3D(Int, Int, Int)
}
```

### Concept Definition

```
concept ConceptName a {
    method1: Type1,
    method2: Type2
}
```

Example:
```
concept Eq a {
    eq: a * a -> Bool
}

concept Ord a {
    compare: a * a -> Int
}
```

### Instance Definition

```
instance ConceptName Type {
    def method1(args) = body,
    def method2(args) = body
}
```

With constraints:
```
instance (Constraint a) => ConceptName (Type a) {
    def method1(args) = body
}
```

## Expression Forms

### Literals

```
42                  // Integer
3.14                // Float
"hello"             // String
true                // Boolean
```

### Variables

```
x
myFunction
_unused
```

### Function Application

```
f(x)
f(x, y, z)
add(1, 2)
```

### Lambda Expressions

```
(x) -> x + 1
(x, y) -> x * y
(a, b, c) -> a + b + c
```

With type annotations:
```
(x: Int) -> x + 1
(x: Int, y: Int) -> x + y
```

### Let Expressions

```
let variable = value in body
```

Examples:
```
let x = 5 in x + 1
let f = (y) -> y * 2 in f(10)
let a = 1 in let b = 2 in a + b
```

### Conditional Expressions

```
if (condition) thenExpr else elseExpr
```

Examples:
```
if (x > 0) x else -x
if (n == 0) 1 else n * factorial(n - 1)
```

### Match Expressions

```
match expr {
    pattern1 -> result1,
    pattern2 -> result2,
    _ -> default
}
```

Examples:
```
match opt {
    Nothing -> 0,
    Just(x) -> x
}

match lst {
    Nil -> 0,
    Cons(h, t) -> 1 + length(t)
}
```

### List Literals

```
[1, 2, 3, 4]
[x, y, z]
[]
```

### Block Expressions

```
{
    expr1,
    expr2,
    expr3
}
```

The value of a block is the value of the last expression.

### Binary Operators

See [Operators](operators.md) for the complete list.

```
x + y
a * b
n == 0
p && q
```

## Type Syntax

### Primitive Types

```
Int
Float
String
Bool
Void
```

### Function Types

```
Int -> Int
Int * Int -> Bool
a -> b -> c
```

### Product Types

```
Int * String
a * b * c
```

### Type Application

```
List Int
Maybe String
Either Int String
```

### Polymorphic Types

```
forall a. a -> a
forall a b. a * b -> a
```

### Constrained Types

```
forall (Eq a). a * a -> Bool
forall (Eq a, Ord b). a * b -> Bool
```

## Pattern Syntax

### Wildcard Pattern

```
_
```

Matches anything, binds nothing.

### Variable Pattern

```
x
myVar
```

Matches anything, binds the value to the variable.

### Literal Pattern

```
0
42
true
"hello"
```

Matches the exact literal value.

### Constructor Pattern

```
Nothing
Just(x)
Cons(head, tail)
Point2D(x, y)
```

Matches a data constructor with its fields.

### Nested Patterns

```
Cons(h, Cons(next, rest))
Just(Just(x))
```
