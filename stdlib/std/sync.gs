// Shared-memory synchronization primitives (`std.sync`):
// - `AtomicInt`: lock-free sequentially consistent 32-bit atomic integer
// - `MVar a`: single-slot synchronized box (empty or full)
// - `Chan a`: bounded & unbounded MPMC typed channel

use std.option.{Option, Some, None}

// ============================================================================
// AtomicInt
// ============================================================================

pub def atomicNew(init: Int): AtomicInt = __concAtomicNew(init)

pub def atomicLoad(a: AtomicInt): Int = __concAtomicLoad(a)

pub def atomicStore(a: AtomicInt, val: Int): Void = __concAtomicStore(a, val)

pub def atomicAdd(a: AtomicInt, delta: Int): Int = __concAtomicAdd(a, delta)

pub def atomicCas(a: AtomicInt, expected: Int, desired: Int): Bool =
    __concAtomicCas(a, expected, desired)

// ============================================================================
// MVar a
// ============================================================================

pub def mvarNew(init: a): MVar a = __concMVarNew(init)

pub def mvarEmpty(): MVar a = __concMVarEmpty()

pub def mvarTake(m: MVar a): a = __concMVarTake(m)

pub def mvarPut(m: MVar a, val: a): Void = __concMVarPut(m, val)

pub def mvarRead(m: MVar a): a = __concMVarRead(m)

pub def mvarTryTake(m: MVar a): Option a = match __concMVarTryTake(m) {
    (ok, val) -> if (ok) Some(val) else None,
}

// ============================================================================
// Chan a
// ============================================================================

pub def chanNew(capacity: Int): Chan a = __concChanNew(capacity)

pub def chanUnbounded(): Chan a = __concChanNew(0)

pub def chanSend(ch: Chan a, val: a): Bool = __concChanSend(ch, val)

pub def chanRecv(ch: Chan a): Option a = match __concChanRecv(ch) {
    (ok, val) -> if (ok) Some(val) else None,
}

pub def chanTryRecv(ch: Chan a): Option a = match __concChanTryRecv(ch) {
    (ok, val) -> if (ok) Some(val) else None,
}

pub def chanClose(ch: Chan a): Void = __concChanClose(ch)

pub def chanIsClosed(ch: Chan a): Bool = __concChanIsClosed(ch)
