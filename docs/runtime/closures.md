# Closure Representation

At runtime, closures use a two-pointer header `{ ptr entry_fn, env_struct }` (passed via opaque `ptr` in LLVM 20):

- **Direct Calls (`ApplyDir`)**: Known top-level or non-capturing local functions are called directly as `@fn(arg0, arg1, ..., ptr null)` with zero closure allocation overhead.
- **Closure Calls (`ApplyCls`)**: First-class functions load the entry function pointer at field `0`, compute the environment pointer at field `1`, and invoke `%entry_fn(arg0, ..., ptr %env)`.
- **Monomorphization of Nested Closures**: When a generic outer function is specialized for concrete type arguments, any lifted closure entry functions inside it are specialized alongside it so captured environment layouts and parameter types remain concrete.
- **Allocation**: Non-escaping closures use LLVM `alloca`; escaping closures allocate via `@gc_alloc`.
