# Concurrency Model

Geisha provides an uncolored, data-race-free shared-memory concurrency model built on three orthogonal layers:

1. **Compile-time `Send` Concept**: Statically prevents unsynchronized mutable containers (`Cell a`, `MutArray a`) and raw FFI pointers (`Ptr`) from crossing thread boundaries—requiring no borrow checker, lifetime annotations, or function coloring.
2. **Multicore Safepoint GC & Native OS Threads**: Executes tasks and actors on native POSIX threads (`pthread`) with cooperative stop-the-world safepoints, blocking-section handshakes (`_setjmp` + `gc_enter_blocking`), and `NOSCAN` leaf allocations, preserving zero-overhead C/GPU FFI compatibility and $O(1)$ zero-copy sharing of immutable values.
3. **Standard Concurrency Modules**:
   - `std.sync` — `AtomicInt`, `MVar a`, and bounded/unbounded `Chan a`
   - `std.task` — `Task a` futures, `spawn`, `await`, `waitAll`, `waitAny`, `par`, `parMap`, `parFor`
   - `std.actor` — Sequential `Actor s` encapsulating thread-pinned `!Send` state (`Cell`, `MutArray`, `Vec`, `StrMap`, `Ptr`)

---

## Static Data-Race Safety: The `Send` Concept

`Send a` is a built-in, compiler-derived marker concept with no runtime dictionary or overhead:

| Category | `Send` Status | Notes |
|----------|---------------|-------|
| `Int`, `Int64`, `Float`, `Bool`, `Char`, `String`, `Void` | `Send` | Immutable scalar and string values |
| Tuple `(T1, T2, ...)` | `Send` iff all `Ti: Send` | Structural derivation |
| Algebraic Data Type `Data ...` | `Send` iff all variant fields are `Send` | Coinductive check supports recursive ADTs (`List a`, trees) |
| `AtomicInt` | `Send` | Lock-free atomic integer |
| `Task a`, `Chan a`, `MVar a` | `Send` iff `a: Send` | Concurrent synchronization & future handles |
| `Actor s` | **Always `Send`** | Opaque mailbox handle; state `s` remains pinned to the actor's dedicated OS thread |
| `Cell a`, `MutArray a` (and `Vec a`, `StrMap v`, etc.) | **`!Send`** | Unsynchronized local mutable state; rejected across threads |
| `Ptr` | **`!Send`** | Raw C/GPU FFI pointer; must be encapsulated inside an `Actor` or single thread |
| Arbitrary closure `a -> b` | **`!Send` as value** | Closures passed to `spawn`, `par`, `parMap`, `parFor`, `actorSpawn`, `actorCall`, or `actorSend` have their captured free variables statically verified to satisfy `Send` |

### Constrained Generic Functions

User functions may constrain type parameters with `Send`:

```
use std.task.{Task, spawn, await}

def backgroundDup(x: a): forall a. (Send a) => Task (a, a) =
    spawn(() -> (x, x))
```

Attempting to capture or return a `Cell`, `MutArray`, `Vec`, or `Ptr` across `spawn` produces a compile-time `NoInstance(Send, ...)` type error.

---

## Tasks & Data Parallelism (`std.task`)

Functions in Geisha are **uncolored**: any ordinary function can block on channels, perform I/O, or `await` tasks without changing its type signature.

```
use std.task.{Task, spawn, await, par, parMap, waitAll, waitAny}

def demoTasks() = {
    // Fork-join pair
    let (left, right) = par(() -> heavy(1), () -> heavy(2));

    // Data-parallel map over immutable lists
    let squares = parMap([1, 2, 3, 4], (x) -> x * x);

    // Dynamic task spawning and asynchronous readiness (`waitAny`)
    let tasks = [spawn(() -> step(1)), spawn(() -> step(2)), spawn(() -> step(3))];
    let (ready, pending) = waitAny(tasks, 1, -1);
    let firstBatch = waitAll(ready);
    let restBatch  = waitAll(pending);
    left + right
}
```

### API Summary (`std.task`)

- `spawn: forall a. (Send a) => (() -> a) -> Task a` — Schedules a task onto the native work-helping worker pool.
- `await: forall a. (Send a) => Task a -> a` — Waits for completion and returns the memoized result (performs inline work-helping when called from worker threads so nested fork-join tasks do not deadlock).
- `isReady: forall a. Task a -> Bool` — Non-blocking completion check.
- `waitAll: forall a. (Send a) => List (Task a) -> List a` — Awaits every task in order.
- `waitAny: forall a. (Send a) => (List (Task a), Int, Int) -> (List (Task a), List (Task a))` — Blocks until at least `count` tasks complete (or `timeoutMs >= 0` elapses; `< 0` waits indefinitely), returning `(ready, pending)`.
- `par: forall a b. (Send a, Send b) => (() -> a, () -> b) -> (a, b)` — Runs two computations in parallel.
- `parMap: forall a b. (Send a, Send b) => (List a, a -> b) -> List b` — Parallel map preserving list order.
- `parFor: (Int, Int, Int -> Void) -> Void` — Parallel index range `[start, end)` across CPU worker threads.
- `numCpu: () -> Int` — Available logical CPU cores.
- `sleepMs: Int -> Void` — Cooperative GC-safe sleep in milliseconds.

---

## Channels & Synchronization (`std.sync`)

```
use std.option.{Some, None}
use std.sync.{chanNew, chanSend, chanRecv, chanClose, atomicNew, atomicAdd}
use std.task.{spawn, await}

def demoSync(): Int = {
    let ch = chanNew(4);
    let counter = atomicNew(0);
    let producer = spawn(() -> {
        let _ = chanSend(ch, 10);
        let _ = chanSend(ch, 32);
        chanClose(ch)
    });
    let sum = match chanRecv(ch) {
        Some(a) -> match chanRecv(ch) {
            Some(b) -> a + b,
            None -> a,
        },
        None -> 0,
    };
    let _ = atomicAdd(counter, sum);
    await(producer);
    sum
}
```

### API Summary (`std.sync`)

- **Atomics**: `atomicNew(init)`, `atomicLoad(a)`, `atomicStore(a, v)`, `atomicAdd(a, delta)` (returns previous value), `atomicCas(a, expected, desired)`.
- **`MVar a`** (single-slot synchronized box): `mvarNew(init)`, `mvarEmpty()`, `mvarTake(m)`, `mvarPut(m, v)`, `mvarRead(m)`, `mvarTryTake(m)`.
- **`Chan a`** (MPMC channel): `chanNew(cap)` (`cap <= 0` for unbounded, `cap > 0` for bounded backpressure), `chanUnbounded()`, `chanSend(ch, v) -> Bool`, `chanRecv(ch) -> Option a`, `chanTryRecv(ch) -> Option a`, `chanClose(ch)`, `chanIsClosed(ch)`.

---

## Stateful Pinned Actors (`std.actor`)

Because `Cell a`, `MutArray a`, `Vec a`, `StrMap v`, and FFI `Ptr` handles are `!Send`, concurrent shared access to stateful components or thread-bound native handles (such as GPU contexts or C simulators) is expressed with `Actor s`.

Each `Actor s` runs a sequential mailbox loop on a **dedicated OS thread**:
1. Its initializer `init: () -> s` executes **on the actor's own thread** (so `s` may freely construct and hold `Cell`, `MutArray`, `Vec`, `StrMap`, or `Ptr`, while `init` itself cannot capture `!Send` values from the spawning thread).
2. `actorCall(actor, (s) -> ...)` enqueues a handler and immediately returns a **`Task r`**, composing directly with `await`, `waitAll`, and `waitAny`.

```
use std.actor.{Actor, actorSpawn, actorCall, actorSend, actorStop}
use std.task.{await, waitAll}
use std.vec.{Vec, vecNew, vecPush, vecLen}

def demoActor(): Int = {
    // `Vec Int` is `!Send`, safely owned inside the actor's pinned OS thread
    let bufActor: Actor (Vec Int) = actorSpawn(() -> vecNew());

    actorSend(bufActor, (v) -> vecPush(v, 10));
    actorSend(bufActor, (v) -> vecPush(v, 20));

    let lenTask = actorCall(bufActor, (v) -> vecLen(v));
    let total = await(lenTask);
    actorStop(bufActor);
    total
}
```
