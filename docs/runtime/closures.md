# Closure Representation

This document describes how closures are represented and called at runtime.

## What is a Closure?

A closure is a function bundled with its environment - the values of free variables captured from the enclosing scope.

```
def makeAdder(n) = (x) -> n + x
```

The lambda `(x) -> n + x` captures the variable `n` from its enclosing scope. When `makeAdder(5)` is called, it returns a closure that "remembers" that `n = 5`.

## Closure Structure

At runtime, a closure consists of:

1. **Entry Function**: A global function that takes the closure's parameters plus an environment pointer
2. **Environment**: A struct containing the captured free variables

### Conceptual Representation

```
Closure = {
    entry: pointer to entry function,
    env: pointer to environment struct
}
```

### LLVM Representation

```llvm
; Environment struct for captured variables
%env_type = type { i32, i8* }  ; Example: captures an Int and a String

; Closure struct
%closure = type {
    i8* (%args..., %env_type*)*,  ; Function pointer
    %env_type*                      ; Environment pointer
}
```

## Closure Creation

When a lambda is encountered during K-conversion:

### Source Code

```
def outer(x) =
    let y = x + 1 in
    (z) -> x + y + z
```

### After K-Conversion

```
// Entry function (global)
def _lambda_0(z, env) =
    let x = env.0 in    // Extract x from environment
    let y = env.1 in    // Extract y from environment
    x + y + z

// Original function creates closure
def outer(x) =
    let y = x + 1 in
    MakeCls(closure, { entry: _lambda_0, fv: [x, y] })
```

### Generated LLVM

```llvm
; Entry function
define i32 @_lambda_0(i32 %z, { i32, i32 }* %env) {
    %x = extractvalue { i32, i32 }* %env, 0
    %y = extractvalue { i32, i32 }* %env, 1
    %sum1 = add i32 %x, %y
    %result = add i32 %sum1, %z
    ret i32 %result
}

; outer function
define { i32 (i32, { i32, i32 }*)*, { i32, i32 }* }* @outer(i32 %x, i8* %_fv) {
    %y = add i32 %x, 1

    ; Allocate environment
    %env = alloca { i32, i32 }
    %env.x = getelementptr { i32, i32 }, %env, 0, 0
    store i32 %x, i32* %env.x
    %env.y = getelementptr { i32, i32 }, %env, 0, 1
    store i32 %y, i32* %env.y

    ; Create closure struct
    %closure = alloca { ..., ... }
    ; ... store entry pointer and env pointer ...

    ret %closure
}
```

## Closure Application

When calling a closure:

### K-Conversion

```
ApplyCls(closure, [arg1, arg2])
```

### Generated Code

```llvm
; Extract entry function pointer from closure
%entry_ptr = extractvalue %closure, 0

; Extract environment pointer from closure
%env_ptr = extractvalue %closure, 1

; Call entry function with args and environment
%result = call i32 %entry_ptr(i32 %arg1, i32 %arg2, %env_type* %env_ptr)
```

## Direct Calls vs Closure Calls

The compiler distinguishes between:

### Direct Calls (ApplyDir)

For known functions with no free variables:

```
def add(x, y) = x + y
add(1, 2)  // Direct call
```

Generated:
```llvm
%result = call i32 @add(i32 1, i32 2, i8* null)
```

No closure overhead - just a direct function call with null environment.

### Closure Calls (ApplyCls)

For closures or unknown functions:

```
def apply(f, x) = f(x)
```

The function `f` could be any closure, so it's called through the closure mechanism.

## Free Variable Analysis

The K-conversion phase analyzes each lambda to determine:

1. **Free variables**: Variables used but not defined in the lambda
2. **Bound variables**: Parameters and let-bound variables

### Example Analysis

```
def example(a) =
    let b = a + 1 in
    (c) ->                  // c is bound (parameter)
        let d = c * 2 in    // d is bound (let)
        a + b + c + d       // a, b are free
```

Free variables for the lambda: `{a, b}`
Bound variables: `{c, d}`

## Nested Closures

Closures can capture other closures:

```
def outer(x) =
    let f = (y) -> x + y in
    (z) -> f(z) * 2
```

The inner lambda captures `f`, which is itself a closure. The environment contains a pointer to `f`'s closure structure.

## Closure Optimization

### Known Function Optimization

When the compiler can determine statically which function a variable refers to:

```
def twice(f, x) = f(f(x))

def main() =
    let inc = (n) -> n + 1 in
    twice(inc, 5)
```

If inlining is performed, the closure for `inc` may be eliminated.

### Environment Trimming

Only actually-used free variables are captured:

```
def example(a, b, c) =
    (x) -> a + x    // Only 'a' is captured, not b or c
```

## Memory Management

### Current Implementation

Closure environments are stack-allocated when possible:

```llvm
%env = alloca { i32, i32 }  ; Stack allocation
```

### Limitations

- Closures that escape their defining scope may cause issues
- No garbage collection for dynamically allocated closures

## Example: Counter

```
def makeCounter(start) =
    let count = start in
    (step) -> count + step

def main() =
    let counter = makeCounter(10) in
    putNumber(counter(1))    // 11
    putNumber(counter(5))    // 15
```

### Compilation

1. `makeCounter` returns a closure capturing `count`
2. Each call to `counter` uses the captured value
3. Note: This is a pure functional counter - `count` is immutable

## Calling Convention

All functions (both regular and closure entry) follow this convention:

```
return_type function_name(param1, param2, ..., paramN, env_ptr)
```

- Regular parameters come first
- Environment pointer is always the last parameter
- For non-closure functions, `env_ptr` is null
