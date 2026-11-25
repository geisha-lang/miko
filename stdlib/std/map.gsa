// Resizable hash maps (`StrMap v`, `IntMap v`) and compiler symbol `Interner` (`std.map`)

use std.option.{Option, Some, None}
use std.str.strHash
use std.vec.{Vec, vecNew, vecLen, vecPush, vecGetOr}

pub data StrMap v {
    MkStrMap(Cell (MutArray (List (String, v))), Cell Int),
}

pub def strMapNew(): StrMap v =
    MkStrMap(cellNew(arrayNew(16, Nil)), cellNew(0))

pub def strMapLen(m: StrMap v): Int = match m {
    MkStrMap(_, sz) -> cellGet(sz),
}

def bucketLookupStr(chain: List (String, v), key: String): Option v = match chain {
    Nil -> None,
    Cons((k, val), rest) -> if (k == key) Some(val) else bucketLookupStr(rest, key),
}

def bucketUpsertStr(chain: List (String, v), key: String, val: v): (List (String, v), Bool) = match chain {
    Nil -> (Cons((key, val), Nil), true),
    Cons((k, old), rest) ->
        if (k == key) (Cons((key, val), rest), false)
        else {
            let (updatedRest, added) = bucketUpsertStr(rest, key, val);
            (Cons((k, old), updatedRest), added)
        },
}

def bucketRemoveStr(chain: List (String, v), key: String): (List (String, v), Bool) = match chain {
    Nil -> (Nil, false),
    Cons((k, old), rest) ->
        if (k == key) (rest, true)
        else {
            let (updatedRest, removed) = bucketRemoveStr(rest, key);
            (Cons((k, old), updatedRest), removed)
        },
}

def rehashStrMap(m: StrMap v): Void = match m {
    MkStrMap(bucketsCell, _) -> {
        let oldBuckets = cellGet(bucketsCell);
        let oldCap = arrayLen(oldBuckets);
        let newCap = oldCap * 2;
        let newBuckets = arrayNew(newCap, Nil);
        let i = cellNew(0);
        while (cellGet(i) < oldCap) {
            let bidx = cellGet(i);
            let cur = cellNew(arrayGet(oldBuckets, bidx));
            let looping = cellNew(true);
            while (cellGet(looping)) {
                match cellGet(cur) {
                    Nil -> cellSet(looping, false),
                    Cons((k, val), rest) -> {
                        let target = strHash(k) % newCap;
                        arraySet(newBuckets, target, Cons((k, val), arrayGet(newBuckets, target)));
                        cellSet(cur, rest)
                    },
                }
            };
            cellSet(i, bidx + 1)
        };
        cellSet(bucketsCell, newBuckets)
    },
}

pub def strMapGet(m: StrMap v, key: String): Option v = match m {
    MkStrMap(bucketsCell, _) -> {
        let buckets = cellGet(bucketsCell);
        let idx = strHash(key) % arrayLen(buckets);
        bucketLookupStr(arrayGet(buckets, idx), key)
    },
}

pub def strMapContains(m: StrMap v, key: String): Bool = match strMapGet(m, key) {
    Some(_) -> true,
    None -> false,
}

pub def strMapInsert(m: StrMap v, key: String, val: v): Void = match m {
    MkStrMap(bucketsCell, szCell) -> {
        if ((cellGet(szCell) + 1) * 4 > arrayLen(cellGet(bucketsCell)) * 3) {
            rehashStrMap(m)
        } else ();
        let buckets = cellGet(bucketsCell);
        let idx = strHash(key) % arrayLen(buckets);
        let (newChain, added) = bucketUpsertStr(arrayGet(buckets, idx), key, val);
        arraySet(buckets, idx, newChain);
        if (added) cellSet(szCell, cellGet(szCell) + 1) else ()
    },
}

pub def strMapRemove(m: StrMap v, key: String): Bool = match m {
    MkStrMap(bucketsCell, szCell) -> {
        let buckets = cellGet(bucketsCell);
        let idx = strHash(key) % arrayLen(buckets);
        let (newChain, removed) = bucketRemoveStr(arrayGet(buckets, idx), key);
        arraySet(buckets, idx, newChain);
        if (removed) cellSet(szCell, cellGet(szCell) - 1) else ();
        removed
    },
}

// ============================================================================
// Integer-keyed Hash Map (`IntMap v`)
// ============================================================================

pub data IntMap v {
    MkIntMap(Cell (MutArray (List (Int, v))), Cell Int),
}

def hashIntKey(k: Int): Int =
    ((k * 265443576) ^ (k >>> 16)) & 2147483647

pub def intMapNew(): IntMap v =
    MkIntMap(cellNew(arrayNew(16, Nil)), cellNew(0))

pub def intMapLen(m: IntMap v): Int = match m {
    MkIntMap(_, sz) -> cellGet(sz),
}

def bucketLookupInt(chain: List (Int, v), key: Int): Option v = match chain {
    Nil -> None,
    Cons((k, val), rest) -> if (k == key) Some(val) else bucketLookupInt(rest, key),
}

def bucketUpsertInt(chain: List (Int, v), key: Int, val: v): (List (Int, v), Bool) = match chain {
    Nil -> (Cons((key, val), Nil), true),
    Cons((k, old), rest) ->
        if (k == key) (Cons((key, val), rest), false)
        else {
            let (updatedRest, added) = bucketUpsertInt(rest, key, val);
            (Cons((k, old), updatedRest), added)
        },
}

pub def intMapGet(m: IntMap v, key: Int): Option v = match m {
    MkIntMap(bucketsCell, _) -> {
        let buckets = cellGet(bucketsCell);
        let idx = hashIntKey(key) % arrayLen(buckets);
        bucketLookupInt(arrayGet(buckets, idx), key)
    },
}

pub def intMapInsert(m: IntMap v, key: Int, val: v): Void = match m {
    MkIntMap(bucketsCell, szCell) -> {
        let buckets = cellGet(bucketsCell);
        let idx = hashIntKey(key) % arrayLen(buckets);
        let (newChain, added) = bucketUpsertInt(arrayGet(buckets, idx), key, val);
        arraySet(buckets, idx, newChain);
        if (added) cellSet(szCell, cellGet(szCell) + 1) else ()
    },
}

// ============================================================================
// Bidirectional String Symbol Interner (`Interner`) for Compiler ASTs
// ============================================================================

pub data Interner {
    MkInterner(StrMap Int, Vec String),
}

pub def internerNew(): Interner =
    MkInterner(strMapNew(), vecNew())

pub def intern(pool: Interner, s: String): Int = match pool {
    MkInterner(toId, toStr) -> match strMapGet(toId, s) {
        Some(existing) -> existing,
        None -> {
            let nextId = vecLen(toStr);
            vecPush(toStr, s);
            strMapInsert(toId, s, nextId);
            nextId
        },
    },
}

pub def internerResolve(pool: Interner, id: Int): String = match pool {
    MkInterner(_, toStr) -> vecGetOr(toStr, id, ""),
}

pub def internerCount(pool: Interner): Int = match pool {
    MkInterner(_, toStr) -> vecLen(toStr),
}
