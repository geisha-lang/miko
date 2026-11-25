# Memory Layout & Garbage Collection

## Value Representation (LLVM 20 Opaque Pointers)

| Geisha Type | LLVM Type | Notes |
|-------------|-----------|-------|
| `Int` | `i32` | 32-bit signed integer |
| `Int64` | `i64` | 64-bit signed integer |
| `Float` | `double` | 64-bit IEEE-754 float |
| `Char` | `i8` | 8-bit character |
| `Bool` | `i1` | Boolean |
| `String` | `ptr` | Global or GC-allocated (`NOSCAN`) null-terminated UTF-8 C string |
| `Ptr` | `ptr` | Opaque C/FFI pointer (not scanned by GC; `!Send`) |
| `Void` / `()` | `void` / `i8 0` | Unit value |
| `(T1, T2, ...)` | `{ T1_llvm, T2_llvm, ... }` | Unpacked first-class tuple value |
| Function `(...) -> R` | `ptr` | Pointer to `{ ptr fn_entry, env_struct }` closure |
| ADT `Data ...` | `ptr` | Pointer to `{ i32 tag, [N x i64] }` ABI-sized inline union |
| `Cell T` | `ptr` | Pointer to GC-allocated cell holding `T_llvm` (`!Send`) |
| `MutArray T` | `ptr` | Pointer to GC-allocated `{ i64 len, [len x T_llvm] }` buffer (`NOSCAN` for scalar `T`; `!Send`) |
| `AtomicInt` | `ptr` | Pointer to GC-allocated (`NOSCAN`) 64-bit aligned atomic `i32` slot (`Send`) |
| `Task T` / `Chan T` / `MVar T` / `Actor S` | `ptr` | Pointer to GC-tracked concurrent runtime handle (`src/lib/runtime/gs_conc.c`) |

## ABI-Sized ADT Union Layout

For each instantiated ADT, the compiler computes the **maximum ABI byte size and alignment** across all variant payload structs (`LLVMABISizeOfType` / `LLVMABIAlignmentOfType`) and allocates `{ i32 tag, payload_storage }` large enough and properly aligned for every variant—preventing truncation when a single-field `Float` variant is larger than a multi-field `Bool` variant.

## Fixed-Point Escape Analysis

`src/core/escape.rs` determines stack (`alloca`) vs heap (`gc_alloc`) allocation:
1. Self-recursive and mutually recursive ADTs are detected via graph reachability over `AdtRegistry` and always heap-allocated.
2. Interprocedural parameter escape analysis computes `NoEscape` / `EscapesReturn` / `EscapesCapture` / `EscapesStore` to a fixed point. All concurrency intrinsics conservatively treat arguments as `EscapesStore` so values and closures shared across threads are guaranteed to reside on the heap.
3. Intraprocedural fixed-point propagation ensures that if an outer ADT or closure escapes, **every inner allocation stored inside its fields or captured in its environment also escapes to the heap**.

## Multicore Mark-and-Sweep Bitmap GC (`src/lib/runtime/gc_bitmap.c`)

- **Object Header & `NOSCAN` Leaf Bit**: Each heap block carries a 1-word header `(payload_words << 1) | noscan_bit`. Leaf allocations without interior pointers (`String`, scalar `MutArray`, `AtomicInt`) allocate via `gc_alloc_atomic` with bit `0` set (`HEADER_NOSCAN_BIT`), allowing `bitmap_mark_obj` to mark them in $O(1)$ without scanning numeric/byte payloads or risking false conservative retention.
- **Thread Registration & Cooperative Safepoints**: Every OS thread running Geisha code registers a `gc_thread_t` stack anchor. Generated LLVM IR checks a relaxed atomic `@__gs_gc_stw_requested` flag on loop backedges and function prologues. Threads entering blocking waits (`Chan`, `MVar`, `Task.await`, `sleepMs`, or `extern` C FFI calls) spill callee-saved registers via `_setjmp` and mark themselves safe (`gc_enter_blocking` / `gc_leave_blocking`) so blocking calls never delay garbage collection on other cores.
- **Stop-the-World Root Scanning & Finalization**: When an allocation triggers collection, active threads park at safepoints, and the collector scans all registered thread stacks `[safepoint_sp, root_start]`, registered top-level globals, and in-flight values inside active or reachable `Task`, `Chan`, `MVar`, and `Actor` runtime structures (`gs_conc_mark_roots`) to a fixed point before sweeping unreachable objects and reclaiming dead synchronization handles.
