# Type Inference

Geisha uses the Hindley-Milner (HM) type inference algorithm to automatically determine the types of expressions without requiring explicit annotations.

## Overview

The type inference process consists of three main phases:

1. **Constraint Generation**: Traverse the AST and generate type constraints
2. **Unification**: Solve constraints using Robinson's unification algorithm
3. **Generalization**: Generalize inferred types to polymorphic schemes

## How It Works

### Basic Inference

Given an unannotated function:

```
def identity(x) = x
```

The inferencer:

1. Assigns a fresh type variable `a` to parameter `x`
2. The body `x` has type `a`
3. The function type is `a -> a`
4. Generalizes to `forall a. a -> a`

### Constraint Generation

For each expression type, constraints are generated:

**Variable reference**: Look up type in environment
```
x          // Uses the type bound to x
```

**Lambda**: Create fresh type variables for parameters
```
(x) -> x + 1
// x : a (fresh)
// x + 1 : Int (from + operator)
// Constraint: a = Int
// Result: Int -> Int
```

**Application**: Function return type must match argument
```
f(x)
// f : a -> b
// x : a
// Result: b
```

**Let binding**: Infer value type, generalize, add to environment
```
let id = (x) -> x in id(42)
// id : forall a. a -> a
// id(42) instantiates to Int -> Int
// Result: Int
```

## Unification

The unification algorithm solves type constraints:

### Rules

1. **Same constructor**: Recursively unify components
   ```
   unify(Int, Int) = success
   unify(a -> b, c -> d) = unify(a, c) && unify(b, d)
   ```

2. **Type variable**: Bind variable to type (with occurs check)
   ```
   unify(a, Int) = { a := Int }
   unify(a, a -> b) = error (occurs check fails)
   ```

3. **Different constructors**: Fail
   ```
   unify(Int, String) = error
   unify(Int, a -> b) = error
   ```

### Substitution

Unification produces a substitution mapping type variables to types:

```
{ a := Int, b := String -> Bool }
```

The substitution is applied to all constraints and the final type.

## Generalization

After inferring a function's type, free type variables are generalized:

```
def first(x, y) = x
// Inferred: a * b -> a
// Free variables: a, b
// Generalized: forall a b. a * b -> a
```

### Value Restriction

Only functions are generalized, not arbitrary values:

```
def id = (x) -> x
// Generalized: forall a. a -> a

let x = [] in ...
// x : List a (monomorphic, not generalized)
```

## Instantiation

When a polymorphic function is used, its type is instantiated with fresh variables:

```
def id(x) = x  // forall a. a -> a

id(42)         // Instantiate with a := Int, get Int -> Int
id("hi")       // Instantiate with a := String, get String -> String
```

## Constrained Types

For functions with concept constraints:

```
def equal(x, y) = eq(x, y)
// Constraint: Eq a
// Type: forall (Eq a). a * a -> Bool
```

Constraints are propagated through the inference:

```
def allEqual(x, y, z) = eq(x, y) && eq(y, z)
// Requires Eq a
// Type: forall (Eq a). a * a * a -> Bool
```

## Inference Examples

### Example 1: compose

```
def compose(f, g, x) = f(g(x))
```

Inference:
1. `f : a` (fresh)
2. `g : b` (fresh)
3. `x : c` (fresh)
4. `g(x)` requires `g : c -> d`, so `b = c -> d`
5. `f(g(x))` requires `f : d -> e`, so `a = d -> e`
6. Result type is `e`
7. Function type: `(d -> e) * (c -> d) * c -> e`
8. Generalize: `forall a b c. (b -> c) * (a -> b) * a -> c`

### Example 2: map

```
def map(f, xs) = match xs {
    Nil -> Nil,
    Cons(h, t) -> Cons(f(h), map(f, t))
}
```

Inference:
1. `f : a` (fresh)
2. `xs : List b` (from pattern Nil, Cons)
3. Pattern `Cons(h, t)`: `h : b`, `t : List b`
4. `f(h)` requires `f : b -> c`
5. `map(f, t) : List c` (recursive)
6. `Cons(f(h), map(f, t)) : List c`
7. Function type: `(b -> c) * List b -> List c`
8. Generalize: `forall a b. (a -> b) * List a -> List b`

### Example 3: Constrained function

```
def member(x, xs) = match xs {
    Nil -> false,
    Cons(h, t) -> if (eq(x, h)) true else member(x, t)
}
```

Inference:
1. `eq(x, h)` requires `Eq a` constraint
2. Type: `forall (Eq a). a * List a -> Bool`

## Error Messages

### NotInScope

Variable not found in environment:

```
def f(x) = y  // Error: y is not in scope
```

### MisMatch

Types cannot be unified:

```
def f(x) = x + "hello"  // Error: Int vs String
```

### Occurs Check

Infinite type detected:

```
def f(x) = x(x)  // Error: a = a -> b is infinite
```

### Pattern Error

Pattern type mismatch:

```
def f(x) = match x {
    Just(y) -> y,
    0 -> 0           // Error: Maybe a vs Int
}
```

## Monomorphization

Geisha uses monomorphization to compile polymorphic functions. When a polymorphic function is called with concrete type arguments, a specialized version is generated.

### How It Works

1. **Instantiation Recording**: During type inference, when a polymorphic function is called, the concrete type arguments are recorded.

2. **Specialization**: After type inference, specialized versions of polymorphic functions are generated for each unique set of type arguments.

3. **Call Site Rewriting**: Calls to polymorphic functions are rewritten to call the specialized versions.

### Example

```
def length(lst) = match lst {
    Nil -> 0,
    Cons(_, t) -> 1 + length(t)
}

def main() = putNumber(length(Cons(1, Cons(2, Nil))))
```

The compiler:

1. Infers `length : forall a. List a -> Int`
2. Records that `length` is called with `a = Int`
3. Generates `length_Int : List Int -> Int` with specialized types
4. Rewrites the call to use `length_Int`
5. Skips emitting code for the generic `length` (since it's never called directly)

### Recursive Functions

Recursive calls within specialized functions are also renamed:

```
def length_Int(lst) = match lst {
    Nil -> 0,
    Cons(_, t) -> 1 + length_Int(t)  // Calls specialized version
}
```

### Name Mangling

Specialized functions use name mangling to encode type arguments:

| Original | Type Args | Mangled Name |
|----------|-----------|--------------|
| `length` | `[Int]` | `length_Int` |
| `fst` | `[Int, Bool]` | `fst_Int_Bool` |
| `map` | `[Int, String]` | `map_Int_String` |

For complex types, the mangling uses descriptive suffixes:
- `Fn<A>To<B>` for function types
- `Pair<A>And<B>` for product types
- `<Base>Of<Arg>` for parameterized types

## Algorithm Summary

```
infer(env, expr):
    case Var(x):
        return instantiate(env.lookup(x))

    case Lambda(params, body):
        freshVars = [freshTypeVar() for p in params]
        env' = env.extend(params, freshVars)
        bodyType = infer(env', body)
        return productType(freshVars) -> bodyType

    case App(fn, args):
        fnType = infer(env, fn)
        argTypes = [infer(env, arg) for arg in args]
        resultVar = freshTypeVar()
        unify(fnType, productType(argTypes) -> resultVar)
        return resultVar

    case Let(x, value, body):
        valueType = infer(env, value)
        scheme = generalize(env, valueType)
        return infer(env.extend(x, scheme), body)

    case If(cond, then, else):
        unify(infer(env, cond), Bool)
        thenType = infer(env, then)
        elseType = infer(env, else)
        unify(thenType, elseType)
        return thenType
```
